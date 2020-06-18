module Compiler.Analyzer.Type where

import           AST
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Identity         (Identity)
import           Control.Monad.State            (StateT, get, gets, modify, put)
import           Control.Monad.Writer           (WriterT)
import           Control.Monad.Reader (ReaderT, asks)
import           Data.List
import           Data.Map                       (Map)
import           Data.Maybe                     (fromJust, fromMaybe, listToMaybe)
import           Debug.Trace

type Analyzer' a = ReaderT Dependencies (WriterT [String] (StateT Storage (Except AnalyzerException))) a

type Analyzer = Analyzer' [String]

injectAnalyzer getter arg = asks getter >>= (\x -> x arg)

data Dependencies = Dependencies
  { aExprAnalyzerGetter :: AExpr -> Analyzer' AExprRes
  , bExprAnalyzerGetter :: BExpr -> Analyzer' BExpr
  , stmtAnalyzerGetter :: Stmt -> Analyzer' Stmt
  , functionStmtAnalyzerGetter :: FunctionStmt -> Analyzer' [FunctionStmt]
  , classStmtAnalyzerGetter :: ClassStmt -> Analyzer' ClassStmt
  }

data AnalyzerException
--  = NotAClass String
  = CustomAException FileInfo String
  | AException Int String
  deriving (Show)

data Storage =
  Storage
    { imports    :: [FileScopes]
    , scopes     :: Scopes
    , rType      :: VarType
    , cName      :: String
    , fName      :: String
    , moduleInfo :: FileInfo
    , postAExpr  :: [AExpr]
    , offsets    :: [Int]
    , useCapture :: Bool
    , varId      :: Int
    }
  deriving (Show)

type Scopes = [Scope]

data Scope
  -- | Scope alias fields
  = Scope String [ScopeField]
  -- | FScope alias fullName fields
  | FScope String String [ScopeField]
  deriving (Show)

data FileScopes =
  FileScope String Scopes
  deriving (Show)

data FileInfo =
  FileInfo
    { fOffset  :: Int
    , fileName :: String
    , filePath :: String
    }
  deriving (Show, Eq)

instance Ord FileInfo where
  compare (FileInfo o1 _ _) (FileInfo o2 _ _) = compare o1 o2

data ScopeField
  -- | SFunction ord name path type args
  = SFunction FileInfo String (Maybe String) VarType [FunArg]
  -- | SVar ord name path type scope
  | SVar FileInfo String (Maybe String) VarType String
  -- | SClass ord name path gen scope
  | SClass FileInfo String (Maybe String) [String] Scope
  -- | SConstructor function class
  -- | SGen type name
  | SGen VarType String
  deriving (Show)
  
getModInfo :: Offset -> Analyzer' FileInfo
getModInfo offset = do
  fileInfo <- gets moduleInfo
  return $ fileInfo {fOffset = offset}

emptyStorage = Storage [] [] VBlank "" "" (FileInfo (-1) "" "") [] [] False 0

setModuleInfo :: Int -> String -> String -> Analyzer' ()
setModuleInfo offset name path = modify (\s -> s {moduleInfo = FileInfo offset name path})

addOffset :: Int -> Analyzer' ()
addOffset o = modify (\s -> s {offsets = o : offsets s})

getOffset :: Analyzer' Int
getOffset = head <$> gets offsets

removeOffset :: Analyzer' Int
removeOffset = do
  o <- head <$> gets offsets
  modify (\s -> s {offsets = tail (offsets s)})
  return o

takeId :: Analyzer' Int
takeId = do
  id <- (+ 1) <$> gets varId
  modify (\s -> s {varId = id})
  return id

takeVarName :: Analyzer' String
takeVarName = do
  id <- takeId
  return $ "var" ++ show id

addArgsScope :: Int -> [FunArg] -> Analyzer' ()
addArgsScope o args = do
  mod <- getModInfo o
  modify (\s -> s {scopes = Scope "args" (argsToFields mod) : scopes s})
  where
    argsToFields mod = map (\(FunArg t n) -> SVar mod n Nothing t "args") args

addScope :: String -> Analyzer' ()
addScope sName = modify (\s -> s {scopes = Scope sName [] : scopes s})


scaleNameWithScope' :: [String] -> String
scaleNameWithScope' = concat . scaleNameWithScope

scaleNameWithScope :: [String] -> [String]
-- | TODO do something with this fucking shit that I created to test scopes
--scaleNameWithScope ("this": xn) = "this->" : xn
scaleNameWithScope ("":xn)       = xn
scaleNameWithScope s@("::":xn)   = s
scaleNameWithScope s@(_:"::":xn) = s
scaleNameWithScope (x:"":xn)     = scaleNameWithScope $ x : xn
scaleNameWithScope (s:n:xn)      = (s ++ "___" ++ n) : xn
scaleNameWithScope [n]           = [n]

removeScope :: Analyzer' Scope
removeScope = do
  s <- gets scopes
  remove s
  where
    remove :: Scopes -> Analyzer' Scope
    remove s
      | null s = error "Storage doesn't contain any scope"
      | otherwise = do
        modify (\storage -> storage {scopes = tail s})
        return $ head s

getScopeName :: Analyzer' String
getScopeName =
  unwrap . listToMaybe <$> gets scopes
  where
    unwrap (Just (Scope n _)) = n
    unwrap Nothing = ""

addField :: ScopeField -> Analyzer' ()
addField field = do
  s <- gets scopes
  modify (\storage -> storage {scopes = add (head s) : tail s})
  where
    add s@(Scope n f)
      | isInScope field s = updateField field s
      | otherwise = Scope n (field : f)

addFunction :: Int -> String -> Maybe String -> VarType -> [FunArg] -> Analyzer' ()
addFunction offset name path type' args = do
  mod <- getModInfo offset
  addField $ SFunction mod name path type' args

addVar :: Int -> String -> Maybe String -> VarType -> String -> Analyzer' ()
addVar offset name path type' scopeName = do
  mod <- getModInfo offset
  addField $ SVar mod name path type' scopeName

addClass :: Int -> String -> Maybe String -> [String] -> Scope -> Analyzer' ()
addClass offset name path gen scope = do
  mod <- getModInfo offset
  addField $ SClass mod name path gen scope

addPostAExpr :: AExpr -> Analyzer' ()
addPostAExpr aExpr = modify (\s -> s {postAExpr = aExpr : postAExpr s})

takePostAExpr :: Analyzer' AExpr
takePostAExpr = do
  h <- head <$> gets postAExpr
  modify (\s -> s {postAExpr = tail $ postAExpr s})
  return h

saveFile :: String -> Analyzer' ()
saveFile n = do
  g <- getGlobalScope
  modify (\s -> s {imports = FileScope n [g] : imports s})

loadFiles :: [(String, String)] -> Analyzer' ([ScopeField], Scopes)
loadFiles names = do
  let global = map fst . filter isGlobal $ names
  let alias = filter (not . isGlobal) names
  i <- gets imports
  let resGlobal = concatMap unwrapFields . filter (\(FileScope n _) -> n `elem` global) $ i
  let resAlias = map (\(n, a) -> FScope a n (unwrapFields (fromJust (find (\(FileScope n' _) -> n == n') i)))) alias
  return (resGlobal, resAlias)
  where
    isGlobal (n, "") = True
    isGlobal _       = False
    unwrapFields (FileScope _ [Scope _ x]) = x

setType :: VarType -> Analyzer' ()
setType t = modify (\s -> s {rType = t})

cleanType :: Analyzer' ()
cleanType = modify (\s -> s {rType = VBlank})

setClassName :: String -> Analyzer' ()
setClassName n = modify (\s -> s {cName = n})

setFunName :: String -> Analyzer' ()
setFunName n = modify (\s -> s {fName = n})

getClassScope :: Analyzer' (Maybe Scope)
getClassScope =
  listToMaybe . reverse . filter cond <$> gets scopes
  where
    cond (Scope "this" _) = True
    cond (Scope "fun" _) = True
    cond _           = False

updateScope :: Scope -> Analyzer' ()
updateScope s@(Scope n _) = do
  s' <- gets scopes
  let newScopes = map md s'
  modify (\s -> s {scopes = newScopes})
  return ()
  where
    md s2@(Scope n2 _)
      | n == n2 =  s
      | otherwise = s2

getGlobalScope :: Analyzer' Scope
getGlobalScope = do
  let cl = "global"
  fromJust . find (\(Scope n _) -> n == cl) <$> gets scopes

getClassGens :: Analyzer' [String]
getClassGens = map (\(SGen _ s) -> s) <$> getClassGens'

getClassGens' :: Analyzer' [ScopeField]
getClassGens' = do
  s <- getClassScope
  return . filter genFilter . fromMaybe [] $ s >>= (\(Scope _ f) -> Just f)
  where
    genFilter SGen {} = True
    genFilter _       = False

setCapture :: Bool -> Analyzer' ()
setCapture b =
  modify (\s -> s{useCapture = b})

-- | private
updateField :: ScopeField -> Scope -> Scope
updateField f (Scope n fs) = Scope n $ map (updater f) fs
  where
    updater a@(SFunction o1 _ _ _ _) b@(SFunction o2 _ _ _ _) = match o1 o2 a b
    updater a@(SVar o1 _ _ _ _) b@(SVar o2 _ _ _ _) = match o1 o2 a b
    updater a@(SClass o1 _ _ _ _) b@(SClass o2 _ _ _ _) = match o1 o2 a b
    updater a@(SGen _ n1) b@(SGen _ n2) = match n1 n2 a b
    updater _ v = v
    match o1 o2 a b =
      if o1 == o2
        then a
        else b

-- | private
isInScope :: ScopeField -> Scope -> Bool
isInScope field (Scope _ f) = any (cond field) f
  where
    cond (SFunction o1 _ _ _ _) (SFunction o2 _ _ _ _) = o1 == o2
    cond (SVar o1 _ _ _ _) (SVar o2 _ _ _ _)           = o1 == o2
    cond (SClass o1 _ _ _ _) (SClass o2 _ _ _ _)       = o1 == o2
    cond _ _                                           = False

isFunction :: ScopeField -> Bool
isFunction SFunction {} = True
isFunction _            = False

isVar :: ScopeField -> Bool
isVar SVar {} = True
isVar _       = False

isClass :: ScopeField -> Bool
isClass SClass {} = True
isClass _         = False

instance Exception AnalyzerException

type AExprRes = (VarType, [FunctionStmt], AExpr)

type AExprAnalyzer = AExpr -> Analyzer' AExprRes

type BExprAnalyzer = BExpr -> Analyzer' BExpr

type FnStmtAnalyzer = FunctionStmt -> Analyzer' [FunctionStmt]

type ClassStmtAnalyzer = String -> ClassStmt -> Analyzer' ClassStmt

type RawAssign = (Int, [String], VarType, AExpr)

type RawAssignConst a = (Int -> [String] -> VarType -> AExpr -> a)

type RawWhile b = (BExpr, [b])

type RawWhileConst a b = (BExpr -> [b] -> a)

type RawFunction = (Int, String, [String], VarType, [FunArg], [FunctionStmt])

--type RawFunctionConst a = Int -> String -> [String] -> VarType -> [FunArg] -> [FunctionStmt] -> a

mockAExprAnalyzer :: AExprAnalyzer
mockAExprAnalyzer _ = return (VAuto, [], IntConst 0 0)

trd (_, _, c) = c

fst3 (a, _, _) = a

snd3 (_, b, _) = b
