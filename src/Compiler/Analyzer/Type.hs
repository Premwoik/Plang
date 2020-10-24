module Compiler.Analyzer.Type where

import           AST
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader   (ReaderT, asks)
import           Control.Monad.State    (StateT, get, gets, modify, put)
import           Control.Monad.Writer   (WriterT)
import           Data.List
import           Data.Map               (Map)
import           Data.Maybe             (fromJust, fromMaybe, listToMaybe)
import           Debug.Trace

type Analyzer' a = ReaderT Dependencies (WriterT [String] (StateT Storage (Except AnalyzerException))) a

type Analyzer = Analyzer' [String]

injectAnalyzer getter arg = asks getter >>= (\x -> x arg)

data Dependencies =
  Dependencies
    { aExprAnalyzerGetter        :: AExpr -> Analyzer' AExprRes
    , bExprAnalyzerGetter        :: BExpr -> Analyzer' BExpr
    , stmtAnalyzerGetter         :: Stmt -> Analyzer' Stmt
    , functionStmtAnalyzerGetter :: FunctionStmt -> Analyzer' [FunctionStmt]
    , classStmtAnalyzerGetter    :: ClassStmt -> Analyzer' ClassStmt
    }

data AnalyzerException
  = CustomAException FileInfo String
  | AException Int String
  deriving (Show)

--  = NotAClass String
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
  deriving (Show, Eq)

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

class Field a where
  getInfo :: a -> FileInfo
  getName :: a -> String
  getType :: a -> VarType
  eqConstructor :: a -> a -> Bool
  hasInfo :: a -> Bool

instance Field ScopeField where
  getInfo f@SFunction {} = sFunctionInfo f
  getInfo f@SVar {}      = sVarInfo f
  getInfo f@SClass {}    = sClassInfo f
  getName f@SFunction {} = sFunctionName f
  getName f@SVar {}      = sVarName f
  getName f@SClass {}    = sClassName f
  getName f@SGen {}      = sGenName f
  getType f@SFunction {} = sFunctionType f
  getType f@SVar {} = sVarType f
  getType f@SClass {} = VClass (VName (sClassName f)) (map VGen (sClassGen f))
  getType f@SGen {} = sGenType f
  eqConstructor SVar {} SVar {}           = True
  eqConstructor SFunction {} SFunction {} = True
  eqConstructor SClass {} SClass {}       = True
  eqConstructor SGen {} SGen {}           = True
  eqConstructor _ _                       = False
  hasInfo SGen {} = False
  hasInfo _       = True

data ScopeField
  -- | SFunction ord name path type args
  = SFunction
      { sFunctionInfo    :: FileInfo
      , sFunctionName    :: String
      , sFunctionPath    :: Maybe String
      , sFunctionType    :: VarType
      , sFunctionArgs    :: [FunArg]
      , sFunctionDetails :: MethodDetails
      }
  -- | SVar ord name path type scope
  | SVar
      { sVarInfo      :: FileInfo
      , sVarName      :: String
      , sVarPath      :: Maybe String
      , sVarType      :: VarType
      , sVarOwnerName :: String
      , sVarDetails   :: MethodDetails
      }
  -- | SClass ord name path gen parents(only VClass) scope
  | SClass
      { sClassInfo    :: FileInfo
      , sClassName    :: String
      , sClassPath    :: Maybe String
      , sClassGen     :: [String]
      , sClassParents :: [VarType]
      , sClassBody    :: Scope
      }
  -- | SGen type name
  | SGen
      { sGenType :: VarType
      , sGenName :: String
      }
  deriving (Show, Eq)

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
    argsToFields mod = map (\(FunArg t n) -> SVar mod n Nothing t "args" defaultMethodDetails) args

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

changeScopeName :: String -> Scope -> Scope
changeScopeName n (Scope _ s) = Scope n s
changeScopeName _ s = s

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
getScopeName = unwrap . listToMaybe <$> gets scopes
  where
    unwrap (Just (Scope n _)) = n
    unwrap Nothing            = ""

containsScopeName :: String -> Analyzer' Bool
containsScopeName name = not. null . filter cond <$> gets scopes
  where
    cond (Scope n _) = n == name
    cond (FScope n _ _) = n == name

addField :: ScopeField -> Analyzer' ScopeField
addField field = do
  s <- gets scopes
  modify (\storage -> storage {scopes = add (head s) : tail s})
  return field
  where
    add s@(Scope n f)
      | isInScope field s = updateField field s
      | otherwise = Scope n (field : f)

addFunction :: Int -> String -> Maybe String -> VarType -> [FunArg] -> Analyzer' ScopeField
addFunction offset name path type' args = do
  mod <- getModInfo offset
  addField $ SFunction mod name path type' args defaultMethodDetails

addMethod :: Int -> String -> Maybe String -> VarType -> [FunArg] -> MethodDetails -> Analyzer' ScopeField
addMethod offset name path type' args details = do
  mod <- getModInfo offset
  addField $ SFunction mod name path type' args details 

addVar :: Int -> String -> Maybe String -> VarType -> String -> Analyzer' ScopeField
addVar offset name path type' scopeName = do
  mod <- getModInfo offset
  addField $ SVar mod name path type' scopeName defaultMethodDetails

addClassVar :: Int -> String -> Maybe String -> VarType -> String -> MethodDetails -> Analyzer' ScopeField
addClassVar offset name path type' scopeName details = do
  mod <- getModInfo offset
  addField $ SVar mod name path type' scopeName details 


addClass :: Int -> String -> Maybe String -> [String] -> [VarType] -> Scope -> Analyzer' ScopeField
addClass offset name path gen parents scope = do
  mod <- getModInfo offset
  addField $ SClass mod name path gen parents scope

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

clearClassName :: Analyzer' ()
clearClassName = setClassName ""

setFunName :: String -> Analyzer' ()
setFunName n = modify (\s -> s {fName = n})

getClassScope :: Analyzer' (Maybe Scope)
getClassScope = listToMaybe . reverse . filter cond <$> gets scopes
  where
    cond (Scope "this" _) = True
    cond (Scope "fun" _)  = True
    cond _                = False

updateScope :: Scope -> Analyzer' ()
updateScope s@(Scope n _) = do
  s' <- gets scopes
  let newScopes = map md s'
  modify (\s -> s {scopes = newScopes})
  return ()
  where
    md s2@(Scope n2 _)
      | n == n2 = s
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
setCapture b = modify (\s -> s {useCapture = b})

-- | private
updateField :: ScopeField -> Scope -> Scope
updateField f (Scope n fs) = Scope n $ map (updater f) fs
  where
    updater new old
      | eqConstructor old new && hasInfo old = check getInfo new old
      | eqConstructor old new = check getName new old
      | otherwise = old
    check cond new old =
      if cond new == cond old
        then new
        else old

-- | private
isInScope :: ScopeField -> Scope -> Bool
isInScope field (Scope _ f) = any (cond field) f
  where
    cond new old
      | eqConstructor new old = getInfo new == getInfo old
      | otherwise = False

isFunction :: ScopeField -> Bool
isFunction SFunction {} = True
isFunction _            = False

isVar :: ScopeField -> Bool
isVar SVar {} = True
isVar _       = False

isClass :: ScopeField -> Bool
isClass SClass {} = True
isClass _         = False

isPublic f@SVar {} = "public" == visibilityMD (sVarDetails f)
isPublic f@SFunction {} = "public" == visibilityMD (sFunctionDetails f)
isPublic _ = False

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
