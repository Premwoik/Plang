module Compiler.Analyzer.Type where

import AST
import Control.Exception
import Control.Monad.State (StateT, get, gets, modify, put)
import Control.Monad.Except

--import Compiler.Translator.Type
import Control.Monad.Writer (WriterT)
import Data.List
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Debug.Trace
import Control.Monad.Identity(Identity)

type Analyzer' a = WriterT [String] (StateT Storage (Except AnalyzerException)) a

type Analyzer = Analyzer' [String]

data AnalyzerException
  = IncorrectExprException
  | UnknownMethodName String
  | NotAClass String
  | NotAMethod String
  | VariableNotExist String
  | UnsupportedTypeException String
  | TypesMismatch Int String
  | AException Int String
  deriving (Show)

data Storage =
  Storage
    { imports :: [FileScopes]
    , scopes :: Scopes
    , rType :: VarType
    , cName :: String
    , fName :: String
    , varId :: Int
--    , sName :: [String]
    }
  deriving (Show)

type Scopes = [Scope]

data Scope =
  Scope String [ScopeField]
  deriving (Show)

data FileScopes =
  FileScope String Scopes
  deriving (Show)

data ScopeField
  -- | SFunction ord name path type args
  = SFunction Int String (Maybe String) VarType [FunArg]
  -- | SVar ord name path type scope
  | SVar Int String (Maybe String) VarType String
  -- | SClass ord name path gen scope
  | SClass Int String (Maybe String) [String] Scope
  -- | SGen name
  | SGen String
  deriving (Show)

emptyStorage = Storage [] [] VBlank "" "" 0

takeId :: Analyzer' Int
takeId = do
  id <- (+1) <$> gets varId
  modify(\s -> s {varId = id})
  return id

takeVarName :: Analyzer' String
takeVarName = do
  id <- takeId
  return $ "var" ++ show id

addArgsScope :: [FunArg] -> Analyzer' ()
addArgsScope args = modify (\s -> s {scopes = Scope "args" argsToFields : scopes s})
  where
    argsToFields = map (\(FunArg t n) -> SVar (-1) n Nothing t "args") args

addScope :: String -> Analyzer' ()
addScope sName = modify (\s -> s {scopes = Scope sName [] : scopes s})



scaleNameWithScope' :: [String] -> String
scaleNameWithScope' = concat . scaleNameWithScope

scaleNameWithScope :: [String] -> [String]
-- | TODO do something with this fucking shit that I created to test scopes
--scaleNameWithScope ("this": xn) = "this->" : xn
scaleNameWithScope ("" : xn) = xn
scaleNameWithScope (x :"" : xn) = scaleNameWithScope $ x : xn
scaleNameWithScope (s : n : xn) = (s ++ "___"  ++ n) : xn
scaleNameWithScope [n] =  [n]


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

addField :: ScopeField -> Analyzer' ()
addField field = do
  s <- gets scopes
  modify (\storage -> storage {scopes = add (head s) : tail s})
  where
    add s@(Scope n f)
      | isInScope field s = updateField field s
      | otherwise = Scope n (field : f)

saveFile :: String -> Analyzer' ()
saveFile n = modify (\s -> s {imports = FileScope n (scopes s) : imports s})

loadFiles :: [String] -> Analyzer' [ScopeField]
loadFiles names = do
  i <- gets imports
  concatMap (\(FileScope _ [Scope _ x]) -> x) . filter (\(FileScope n _) -> n `elem` names) <$> gets imports

setType :: VarType -> Analyzer' ()
setType t = modify (\s -> s {rType = t})

setClassName :: String -> Analyzer' ()
setClassName n = modify (\s -> s {cName = n})

setFunName :: String -> Analyzer' ()
setFunName n = modify (\s -> s {fName = n})

getClassScope :: Analyzer' (Maybe Scope)
getClassScope = do
--  cl <- gets cName
  let cl = "this"
  find (\(Scope n _) -> n == cl) <$> gets scopes

getClassGens :: Analyzer' [String]
getClassGens = do
  s <- getClassScope
  return . map (\(SGen s) -> s) . filter genFilter . fromMaybe [] $ s >>= (\(Scope _ f) -> Just f)
  where
    genFilter SGen {} = True
    genFilter _ = False

-- | private
updateField :: ScopeField -> Scope -> Scope
updateField f (Scope n fs) = Scope n $ map (updater f) fs
  where
    updater a@(SFunction o1 _ _ _ _) b@(SFunction o2 _ _ _ _) = match o1 o2 a b
    updater a@(SVar o1 _ _ _ _) b@(SVar o2 _ _ _ _) = match o1 o2 a b
    updater a@(SClass o1 _ _ _ _) b@(SClass o2 _ _ _ _) = match o1 o2 a b
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
    cond (SVar o1 _ _ _ _) (SVar o2 _ _ _ _) = o1 == o2
    cond (SClass o1 _ _ _ _) (SClass o2 _ _ _ _) = o1 == o2
    cond _ _ = False


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

type RawFunction = (Int, String, VarType, [FunArg], [FunctionStmt])

type RawFunctionConst a = Int -> String -> VarType -> [FunArg] -> [FunctionStmt] -> a

trd (_, _, c) = c

