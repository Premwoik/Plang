module Compiler.Analyzer.Type where

import           Control.Exception

import           AST
import           Control.Monad.State  (State)
--import Compiler.Translator.Type
import           Control.Monad.Writer (WriterT)

type Analyzer' a = WriterT [String] (State Storage) a

type Analyzer = Analyzer' [String]

data AnalyzerException
  = IncorrectExprException
  | UnknownMethodName
  | NotAClass
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
  | LocalClassScope [ClassStmt]
  | LocalStmt [Stmt]
  | LocalEmpty
  deriving (Show)

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

type FnStmtAnalyzer = FunctionStmt -> Analyzer' [FunctionStmt]

type ClassStmtAnalyzer = ClassStmt -> Analyzer' ClassStmt

type RawAssign = (String, VarType, AExpr)

type RawAssignConst a = (String -> VarType -> AExpr -> a)

type RawWhile b = (BExpr, [b])

type RawWhileConst a b = (BExpr -> [b] -> a)

type RawFunction = (String, VarType, Maybe [FunArg], [FunctionStmt])

type RawFunctionConst a = String -> VarType -> Maybe [FunArg] -> [FunctionStmt] -> a

trd (_, _, c) = c
