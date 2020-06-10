{-# LANGUAGE FlexibleContexts #-}

module Compiler.Analyzer.Browser where

import           AST
import           Compiler.Analyzer.Type
import Control.Monad(mapM, zipWithM)
import           Control.Monad.Except   (throwError)
import           Control.Monad.State    (get, gets)
import           Data.List              (group)
import qualified Data.List              as L
import           Data.Maybe             (fromJust, isJust, listToMaybe, maybeToList)
import           Debug.Trace
import Compiler.Analyzer.Error

find' :: [String] -> Analyzer' [ScopeField]
find' name = do
  storage <- get
  let scopes' = scopes storage
  let globalScopes = filterGlobal scopes'
  globalScope <- getGlobalScope
  case name of
    [n]             -> return $ searchNameInScopes n globalScopes
    ["", n]         -> return $ searchNameInScopes n globalScopes
    ["this", n]     -> searchInThisScope n scopes'
    ["g", n]        -> return $ searchInScope n globalScope
    ["global", n]   -> return $ searchInScope n globalScope
    ["", "this", n] -> searchInThisScope n scopes'
    [sName, n]      -> searchInScopeWithName n sName scopes'
    _               -> return []


fixNativeClassType :: VarType -> Analyzer' VarType
fixNativeClassType it@(VClass (VName n) gen) = do
  cl <- listToMaybe <$> find' [n]
  return $ case cl of
    Just (SClass _ _ (Just path) _ _) -> VClass (VNameNative n path) gen
    _ -> it
fixNativeClassType x = return x

findInClass :: VarName -> String -> Analyzer' [ScopeField]
findInClass cName name = do
  c <- listToMaybe <$> find' [unwrapVarNameForce cName]
  return $
    case c of
      Just (SClass o _ _ gen s) -> searchInScope name s
      Nothing                   -> []

getFileName :: String -> Analyzer' String
getFileName alias = do
  s <- gets scopes
  return $ case L.find cond s of
    Just(FScope _ n _) -> n
    Nothing -> alias
  where
    cond (FScope a _ _) = a == alias
    cond _ = False
    

searchInThisScope :: String -> Scopes -> Analyzer' [ScopeField]
searchInThisScope name = searchInScopeWithName name "this"

searchInScopeWithName :: String -> String -> Scopes -> Analyzer' [ScopeField]
searchInScopeWithName name scopeName scopes =
  case getScope scopeName scopes of
    (Just s) -> return $ searchInScope name s
    Nothing -> do
      o <- getOffset
      makeError o $ ScopeNoExist scopeName (getScopesNames scopes)

getScopesNames :: Scopes -> [String]
getScopesNames = map unwrapName

isInsideLoop :: Analyzer' Bool
isInsideLoop = do
  s <- gets scopes
  return . isJust . listToMaybe . filter (\x -> x == "for" || x == "while") . getScopesNames $ s

searchNameInScopes :: String -> Scopes -> [ScopeField]
searchNameInScopes name = concatMap (searchInScope name)

searchInScope :: String -> Scope -> [ScopeField]
searchInScope name scope = filter cond (unwrapFields scope)
  where
    cond (SFunction o n _ t a_) = n == name
    cond (SVar o n _ t __)      = n == name
    cond (SClass o n _ g s_)    = n == name
    cond _                     = False

unwrapFields (Scope _ s) = s
unwrapFields (FScope _ _ s) = s

unwrapName (Scope n _ )  = n
unwrapName (FScope n _ _ )  = n

filterGlobal :: Scopes -> Scopes
filterGlobal = filter cond
  where
    cond Scope {} = True
    cond _ = False

-- | PRIVATE
getScope :: String -> Scopes -> Maybe Scope
getScope name = L.find (\s -> name == unwrapName s)

----    MATCH

checkFunctionUniqueness o name t args = do
  fns <- find' name
  noVarWithSameName fns
  allReturnTheSame fns
  notSameArguments fns
  where
    noVarWithSameName fns =
      if all isFunction fns
        then return ()
        else makeError o $ VariableWithSameName (show name)
    allReturnTheSame fns =
      if all (\(SFunction o n p t' a) -> t == t') fns
        then return ()
        else makeError o $ FunctionDifferentReturnType (show name) fns t
    notSameArguments fns =
      if all (\g -> length g == 1) .
         group .
         map (\(SFunction _ _ _ _ a) -> map (\(FunArg t _) -> t) a) . filter (\(SFunction i _ _ _ _) -> fOffset i <= o) $
         fns
        then return ()
        else makeError o $ FunctionRepetition fns

maybeArgsMatch :: Maybe [AExpr] -> [VarType] -> AExprAnalyzer -> ScopeField -> Analyzer' Bool
maybeArgsMatch (Just args) gen analyzer s = do
  args' <- prepareArgs
  return $ argsMatch args' gen s
  where
    prepareArgs = case s of
      (SFunction _ _ _ _ fargs) -> zipWithM (aExprExtractType analyzer) fargs args
      _ -> mapM (aExprExtractType analyzer (FunArg VAuto "")) args
maybeArgsMatch Nothing _ _ _ = return True

--
argsMatch :: [VarType] -> [VarType] -> ScopeField -> Bool
argsMatch t gen (SFunction _ _ _ _ args) = argsMatch' t (fixArgs gen args)
argsMatch t gen (SClass _ n _ g (Scope _ s)) = any (match (mergeGen g)) s
  where
    match gen (SFunction o n' _ _ args) = n' == n && constructorArgsMatch t (fixArgs gen args)
    match _ _ = False
    mergeGen gns = zipWith VGenPair gns gen

argsMatch' :: [VarType] -> [FunArg] -> Bool
argsMatch' t args = (length t == length args) && all match (zip t args)
  where
    match (VClass (VName "ArrayList") [t], FunArg (VPointer t2 NativePtr) _) = t == t2
    match (w, FunArg ot _)      = w == ot
    
constructorArgsMatch :: [VarType] -> [FunArg] -> Bool
constructorArgsMatch t args = (length t == length args) && all match (zip t args)
  where
--    match (w, FunArg VGen {} _) = False
    match (w, FunArg ot _)      = w == ot

filterConstructor :: Maybe [AExpr] -> [VarType] -> ScopeField -> Analyzer' ScopeField
filterConstructor (Just args) gen (SClass _ n _ _ (Scope _ s)) = do
  argsT <- mapM (aExprExtractType mockAExprAnalyzer (FunArg VAuto "")) args
  return . head . filter (match argsT) $ s
  where
    match argsT (SFunction o n' _ _ args) = n' == n && constructorArgsMatch argsT (fixArgs gen args)
    match _ _ = False

--
fixArgs :: [VarType] -> [FunArg] -> [FunArg]
fixArgs [] args  = args
fixArgs gen args = map (\(FunArg t n) -> FunArg (fixType gen t) n) args

-- | fixType - replace generic type with exact type
--   e.g
--   [VGenPair T VInt] -> VGen T -> VInt
fixType :: [VarType] -> VarType -> VarType
fixType gen (VGen tName) = makeOutput . filter fGen $ gen
  where
    makeOutput (VGenPair _ t:_) = t
    makeOutput _ = error $ "Cannont find type for template: " ++ tName ++ " | | " ++ show gen
    fGen (VGenPair tName' _) = tName' == tName
    fGen _                   = False
fixType gen t = t

aExprExtractType :: AExprAnalyzer -> FunArg -> AExpr -> Analyzer' VarType
aExprExtractType a x (ScopeMark _ _ more) = aExprExtractType a x more 
aExprExtractType a x (TypedVar _ _ _ (Just more)) = aExprExtractType a x more 
aExprExtractType _ _ (TypedVar _ t _ Nothing)    = return $ fixClassGen t
aExprExtractType _ _ IntConst {}                  = return VInt
aExprExtractType _ _ FloatConst {}                = return VFloat
aExprExtractType _ _ StringVal {}                 = return VString
aExprExtractType _ _ (ABool BoolConst {})         = return VBool
aExprExtractType _ _ (TypedABinary t _ _ _)         = return t
aExprExtractType analyzer a f@ LambdaFn {} = fixFunArgs a f analyzer
aExprExtractType _ _ x                             = error (show x)

fixFunArgs :: FunArg -> AExpr -> AExprAnalyzer -> Analyzer' VarType
fixFunArgs (FunArg t' _) f@(LambdaFn o t args _) analyzer = do
  tmpRet <- gets rType
  setType t' 
  res <- analyzer f
  addPostAExpr $ trd res
  setType tmpRet
  return $ fst3 res
  

-- remember that pointer is always selected to false as default value
fixClassGen (VClass n gens) = VClass n (map nGen gens)
  where
    nGen (VGenPair _ t) = t
    nGen x              = x
fixClassGen x = x
