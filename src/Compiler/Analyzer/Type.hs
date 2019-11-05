module Compiler.Analyzer.Type where

import Control.Exception
import Compiler.Translator.Type
import Control.Monad.Writer(WriterT)
import Control.Monad.State(State)
import AST

type Analyzer' a = WriterT [String] (State StorageCache) a

type Analyzer = Analyzer' [String]

data AnalyzerException =
  IncorrectExprException
  deriving (Show)

instance Exception AnalyzerException


type AExprAnalyzer = AExpr -> Analyzer' AExpr
type FnStmtAnalyzer = FunctionStmt -> Analyzer' FunctionStmt
type ClassStmtAnalyzer = ClassStmt -> Analyzer' ClassStmt


type RawAssign =  (String, VarType, AExpr)
type RawAssignConst a = (String -> VarType -> AExpr -> a)

type RawWhile b = (BExpr, [b])
type RawWhileConst a b = (BExpr -> [b] -> a)

