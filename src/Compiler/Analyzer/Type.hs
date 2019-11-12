module Compiler.Analyzer.Type where

import Control.Exception
import Compiler.Translator.Type
import Control.Monad.Writer(WriterT)
import Control.Monad.State(State)
import AST

type Analyzer' a = WriterT [String] (State Storage) a

type Analyzer = Analyzer' [String]

data AnalyzerException =
  IncorrectExprException
  | UnknownMethodName
  | NotAClass
  | VariableNotExist String
  | TypesMismatch String
  deriving (Show)

instance Exception AnalyzerException

type AExprRes = (VarType, [FunctionStmt], AExpr)

type AExprAnalyzer = AExpr -> Analyzer' AExprRes
type FnStmtAnalyzer = FunctionStmt -> Analyzer' [FunctionStmt]
type ClassStmtAnalyzer = ClassStmt -> Analyzer' ClassStmt


type RawAssign =  (String, VarType, AExpr)
type RawAssignConst a = (String -> VarType -> AExpr -> a)

type RawWhile b = (BExpr, [b])
type RawWhileConst a b = (BExpr -> [b] -> a)

type RawFunction = (String, VarType, Maybe [FunArg], [FunctionStmt])
type RawFunctionConst a = String -> VarType -> Maybe [FunArg] -> [FunctionStmt] -> a

trd (_, _, c) = c
