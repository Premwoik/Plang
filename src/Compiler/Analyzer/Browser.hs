{-# LANGUAGE FlexibleContexts #-}

module Compiler.Analyzer.Browser where

import           AST
import           Compiler.Analyzer.Error
import           Compiler.Analyzer.Type
import           Compiler.Analyzer.Universal
import           Control.Monad               (filterM, mapM, zipWithM)
import           Control.Monad.Except        (throwError)
import           Control.Monad.State         (get, gets)
import           Data.List                   (group)
import qualified Data.List                   as L
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromJust, isJust, listToMaybe,
                                              maybeToList)
import           Debug.Trace

find' :: [String] -> Analyzer' [ScopeField]
find' name = do
  storage <- get
  let scopes' = scopes storage
  let globalScopes = filterGlobal scopes'
  globalScope <- getGlobalScope
  case name of
    [n]             -> searchNameInScopes n globalScopes
    ["", n]         -> searchNameInScopes n globalScopes
    ["this", n]     -> searchInThisScope n scopes'
    ["g", n]        -> searchInScope n globalScope
    ["global", n]   -> searchInScope n globalScope
    ["", "this", n] -> searchInThisScope n scopes'
    ["nthis", n]    -> searchWithoutThis n globalScopes
    [sName, n]      -> searchInScopeWithName n sName scopes'
    _               -> return []

fixNativeClassType :: VarType -> Analyzer' VarType
fixNativeClassType it@(VClass (VName n) gen) = do
  cl <- listToMaybe <$> find' [n]
  return $
    case cl of
      Just (SClass _ _ (Just path) _ _ _) -> VClass (VNameNative n path) gen
      _                                   -> it
fixNativeClassType x = return x

fixArgsNativeClassType :: [FunArg] -> Analyzer' [FunArg]
fixArgsNativeClassType = mapM fixer
  where
    fixer (FunArg t n) = do
      nt <- fixNativeClassType t
      return $ FunArg nt n

-- | 'findInClass' function searching only in class scope
findInClass :: VarName -> String -> Analyzer' [ScopeField]
findInClass cName name = do
  c <- listToMaybe . filter isClass <$> find' ["nthis", unwrapVarNameForce cName]
  case c of
    Just (SClass o _ _ gen parents s) -> searchDeeper parents =<< searchInScope name s
    _ -> return []
  where
    searchDeeper :: [VarType] -> [ScopeField] -> Analyzer' [ScopeField]
    searchDeeper [] res = return res
    searchDeeper p res = L.nub . concat . (:) res <$> mapM (\x -> filter isPublic <$> findInClass (unwrapClassName x) name) p

getFileName :: String -> Analyzer' String
getFileName alias = do
  s <- gets scopes
  return $
    case L.find cond s of
      Just (FScope _ n _) -> n
      Nothing             -> alias
  where
    cond (FScope a _ _) = a == alias
    cond _              = False

searchInThisScope :: String -> Scopes -> Analyzer' [ScopeField]
searchInThisScope name = searchInScopeWithName name "this"

searchWithoutThis :: String -> Scopes -> Analyzer' [ScopeField]
searchWithoutThis name scopes = do
  let sc = filter notThis scopes
  searchNameInScopes name sc
  where
    notThis (Scope "this" _ ) = False
    notThis _ = True

searchInScopeWithName :: String -> String -> Scopes -> Analyzer' [ScopeField]
searchInScopeWithName name scopeName scopes =
  case getScope scopeName scopes of
    (Just s) -> searchInScope name s
    Nothing -> do
      o <- getOffset
      makeError o $ ScopeNoExist scopeName (getScopesNames scopes)

getScopesNames :: Scopes -> [String]
getScopesNames = map unwrapName

isInsideLoop :: Analyzer' Bool
isInsideLoop = do
  s <- gets scopes
  return . isJust . listToMaybe . filter (\x -> x == "for" || x == "while") . getScopesNames $ s

isVarInLambda :: String -> Analyzer' Bool
isVarInLambda name = do
  sc <- gets scopes
  let s = take (fromJust (L.findIndex isLambdaScope sc) + 2) sc
  not . null <$> searchNameInScopes name s
  where
    isLambdaScope (Scope n _) = n == "lambda"
    isLambdaScope _           = False

-- | 'compareTypes' should be used to compare types with polymorphism check
--  compareTypes lowerClass upperClass
--  @lowerClass@ -
--  @upperClass@ -
compareTypes :: Offset -> VarType -> VarType -> Analyzer' Bool
compareTypes o (VRef t) (VRef t2) = compareTypes o t t2
compareTypes o (VPointer t _) (VPointer t2 _) = compareTypes o t t2
-- | passing list object to native c array e.g. list<int> -> int*
compareTypes o (VPointer t _) (VClass (VName "ArrayList") [t2]) = compareTypes o t t2
-- | comparing classes
compareTypes o c1@(VClass n g1) c2@(VClass n2 g2)
  | unwrapVarNameForce n == unwrapVarNameForce n2 && g1 == g2 = return True
  | otherwise = do
    p <- getClassInheritance c2
    case L.find (\x -> unwrapClassName x == n) p of
      Just rc@(VClass rn rg) ->
        if rg == g1
          then return True
          else makeError o $ AssignTypesMismatch c1 rc
      Nothing -> return False
-- | default type comparator defined in eq
compareTypes _ a b = return $ a == b

-- | 'getClassInheritance' -
getClassInheritance :: VarType -> Analyzer' [VarType]
getClassInheritance (VClass n g) = do
  cl <- listToMaybe <$> find' ["", unwrapVarName n]
  case cl of
    Just (SClass o n p gen parents s) -> do
      let typedGen = addTypeToGens g gen
      let typedParents = map (replaceGenWithType typedGen) parents
      res <- L.nub . concat <$> mapM getClassInheritance typedParents
      return $ VClass (VName n) typedGen : res
    Nothing -> makeError 0 $ CustomError $ "Can't find class with name - " ++ show n

searchNameInScopes :: String -> Scopes -> Analyzer' [ScopeField]
searchNameInScopes name scopes = concat <$> mapM (searchInScope name) scopes

searchInScope :: String -> Scope -> Analyzer' [ScopeField]
searchInScope name (Scope "this" s) = do
  className <- gets cName
  parentsResult <- findInClass (VName className) name
  thisResult <- searchInScope name (Scope "" s)
  return $ thisResult ++ parentsResult
searchInScope name scope = return $ filter cond (unwrapFields scope)
  where
    cond f = getName f == name


unwrapFields (Scope _ s)    = s
unwrapFields (FScope _ _ s) = s

unwrapName (Scope n _)    = n
unwrapName (FScope n _ _) = n

filterGlobal :: Scopes -> Scopes
filterGlobal = filter cond
  where
    cond Scope {} = True
    cond _        = False

-- | PRIVATE
getScope :: String -> Scopes -> Maybe Scope
getScope name = L.find (\s -> name == unwrapName s)

----    MATCH
maybeArgsMatch :: Maybe [AExpr] -> [VarType] -> ScopeField -> Analyzer' Bool
maybeArgsMatch (Just args) gen s = do
  args' <- prepareArgs
  let gen' = prepareFunctionGens gen s args'
  argsMatch args' gen' s
  where
    prepareArgs =
      case s of
        SFunction {} -> zipWithM aExprExtractType (sFunctionArgs s) args
        _            -> mapM (aExprExtractType (FunArg VAuto "")) args
maybeArgsMatch Nothing _ _ = return True

-- | 'preparedFunctionGens' is used for deducting generics from SFunction
prepareFunctionGens :: [VarType] -> ScopeField -> [VarType] -> [VarType]
prepareFunctionGens [] f@SFunction {} args = genMap
  where
    fargs = sFunctionArgs f
    genMap = map (uncurry VGenPair) . Map.toList $ foldl check initAcc (zip fargs args)
    initAcc = Map.empty
    check acc (FunArg (VGen x) _, arg) =
      case Map.lookup x acc of
        Just y ->
          if y /= arg
            then error "You passed different types as same generic"
            else acc
        Nothing -> Map.insert x arg acc
    check acc _ = acc
prepareFunctionGens gens _ _ = gens

argsMatch :: [VarType] -> [VarType] -> ScopeField -> Analyzer' Bool
argsMatch t gen f@SFunction {} = argsMatch' t =<< fixArgs gen (sFunctionArgs f)
argsMatch t gen (SClass _ n _ g _ (Scope _ s)) = or <$> mapM (match (mergeGen g)) s
  where
    match gen (SFunction o n' _ _ args _) =
      (\x -> return ((n' == n) && x)) =<< constructorArgsMatch t =<< fixArgs gen args
    match _ _ = return False
    mergeGen gns = zipWith VGenPair gns gen

argsMatch' :: [VarType] -> [VarType] -> Analyzer' Bool
argsMatch' t args = do
  let lenCheck = length t == length args
  argsCheck <- and <$> mapM match (zip t args)
  return $ lenCheck && argsCheck
  where
    match (VClass (VName "ArrayList") [t], VPointer t2 NativePtr) = compareTypes 0 t t2
    match (w, ot) = compareTypes 0 ot w

constructorArgsMatch :: [VarType] -> [VarType] -> Analyzer' Bool
constructorArgsMatch t args = do
  let lenCheck = length t == length args
  argsCheck <- and <$> mapM match (zip t args)
  return $ lenCheck && argsCheck
  where
    match (w, ot) = compareTypes 0 ot w

filterConstructor :: Offset -> Maybe [AExpr] -> [VarType] -> ScopeField -> Analyzer' ScopeField
filterConstructor o (Just args) gen (SClass _ n _ _ _ (Scope _ s)) = do
  argsT <- mapM (aExprExtractType (FunArg VAuto "")) args
  res <- listToMaybe <$> filterM (match argsT) s
  case res of
    Just c -> return c
    Nothing ->
      makeError o $
      CustomError $ "Cant find constructor that takes such args - " ++ show args ++ "in class with name: " ++ n
  where
    match argsT (SFunction o n' _ _ args _) =
      (\x -> return ((n' == n) && x)) =<< constructorArgsMatch argsT =<< fixArgs gen args
    match _ _ = return False

--
fixArgs :: [VarType] -> [FunArg] -> Analyzer' [VarType]
fixArgs [] args = return . map (\(FunArg t _) -> t) $ args
fixArgs gen args = mapM (\(FunArg t n) -> fixType gen t) args

-- | fixType - replace generic type with exact type
--   e.g
--   [VGenPair T VInt] -> VGen T -> VInt
fixType :: [VarType] -> VarType -> Analyzer' VarType
fixType gen (VGen tName) = makeOutput . filter fGen $ gen
  where
    makeOutput (VGenPair _ t:_) = return t
    makeOutput _ = error $ "Cannont find type for template: " ++ tName ++ " | | " ++ show gen
    fGen (VGenPair tName' _) = tName' == tName
    fGen _                   = False
fixType gen t = return t

aExprExtractType :: FunArg -> AExpr -> Analyzer' VarType
aExprExtractType x (ScopeMark _ _ more)         = aExprExtractType x more
aExprExtractType x (TypedVar _ _ _ (Just more)) = aExprExtractType x more
aExprExtractType _ (TypedVar _ t _ Nothing)     = return $ fixClassGen t
aExprExtractType _ IntConst {}                  = return VInt
aExprExtractType _ FloatConst {}                = return VFloat
aExprExtractType _ StringVal {}                 = return VString
aExprExtractType _ (ABool BoolConst {})         = return VBool
aExprExtractType _ (TypedABinary t _ _ _)       = return t
aExprExtractType a f@LambdaFn {}                = fixFunArgs a f
aExprExtractType _ Null {}                      = return VAuto
aExprExtractType _ x                            = error (show x)

fixFunArgs :: FunArg -> AExpr -> Analyzer' VarType
fixFunArgs (FunArg t' _) f@(LambdaFn o _ t args _) = do
  tmpRet <- gets rType
  setType t'
  res <- injectAnalyzer aExprAnalyzerGetter f
  addPostAExpr $ trd res
  setType tmpRet
  return $ fst3 res

-- remember that pointer is always selected to false as default value
fixClassGen (VClass n gens) = VClass n (map nGen gens)
  where
    nGen (VGenPair _ t) = t
    nGen x              = x
fixClassGen x = x
