module Compiler.Analyzer.Type where

import           Control.Exception

import           AST
import           Control.Monad.State  (State)

import           Control.Monad.State  (get, gets, modify, put)
--import Compiler.Translator.Type
import           Control.Monad.Writer (WriterT)

type Analyzer' a = WriterT [String] (State Storage) a

type Analyzer = Analyzer' [String]

data AnalyzerException
  = IncorrectExprException
  | UnknownMethodName
  | NotAClass String
  | NotAMethod String
  | VariableNotExist String
  | TypesMismatch String
  | UnsupportedTypeException String
  deriving (Show)

data Storage =
  Storage
    { global :: [Stmt]
    , local  :: LocalStorage
    , cache  :: StorageCache
    }
  deriving (Show)

emptyStorage = Storage [] LocalEmpty EmptyCache

data LocalStorage
  = LocalScope [FunctionStmt]
  | LocalClassScope Stmt (Maybe ClassStmt)
  | LocalStmt [Stmt]
  | LocalFunc Stmt
  | LocalEmpty
  deriving (Show)

getType :: LocalStorage -> VarType
getType (LocalClassScope (ClassExpr _ n _ _) Nothing) = VClass n
getType (LocalClassScope _ (Just (Method _ _ t _ _))) = t
getType (LocalFunc (Function _ _ t _ _)) = t
getType t = throw $ UnsupportedTypeException $ show t

setType :: VarType -> LocalStorage -> LocalStorage
setType t (LocalClassScope c (Just (Method o n _ a b))) = LocalClassScope c (Just (Method o n t a b))
setType t (LocalFunc (Function o n _ a b)) = LocalFunc (Function o n t a b)
setType t l = throw $ UnsupportedTypeException $ show  l

setStmt:: Stmt -> Analyzer' ()
setStmt s =
  case s of
    class'@ClassExpr {} -> modify (\s -> s {local = LocalClassScope class' Nothing})
    func@Function {} -> modify (\s -> s {local = LocalFunc func})
    _ -> throw $ UnsupportedTypeException $ "setStmt | " ++ show s
--    _ -> throw $ NotAClass $ "Function setClass need class as parameter. Indstead of that it got: " ++ show s

setMethod :: ClassStmt -> Analyzer' ()
setMethod s =
  case s of
    method@Method {} -> do
      state <- get
      let (LocalClassScope class' _) = local state
      put $ state {local = LocalClassScope class' (Just method)}
    _ -> throw $ NotAMethod $ "Function setMethod need a method as parameter. Instead of that it got: " ++ show s
    
setScope :: [FunctionStmt] -> Analyzer' ()
setScope scope = modify (\ s -> s {local = LocalScope scope})

getClass :: Analyzer' Stmt
getClass = do
  local' <- gets local
  case local' of
    LocalClassScope c _ -> return c
    _ -> fail "Class can't be get"

data StorageCache
  = EmptyCache
  | ScopeCache [FunctionStmt]
  | ClassScopeCache [ClassStmt]
  | TypeCache [VarType]
  | InjectBefore [FunctionStmt]
  deriving (Show)

--data LocalScope = LSGlobal
instance Exception AnalyzerException

type AExprRes = (VarType, [FunctionStmt], AExpr)

type AExprAnalyzer = AExpr -> Analyzer' AExprRes

type BExprAnalyzer = BExpr -> Analyzer' BExpr

type FnStmtAnalyzer = FunctionStmt -> Analyzer' [FunctionStmt]

type ClassStmtAnalyzer = String -> ClassStmt -> Analyzer' ClassStmt

type RawAssign = (Int, String, VarType, AExpr)

type RawAssignConst a = (Int -> String -> VarType -> AExpr -> a)

type RawWhile b = (BExpr, [b])

type RawWhileConst a b = (BExpr -> [b] -> a)

type RawFunction = (Int, String, VarType, Maybe [FunArg], [FunctionStmt])

type RawFunctionConst a = Int -> String -> VarType -> Maybe [FunArg] -> [FunctionStmt] -> a

trd (_, _, c) = c
