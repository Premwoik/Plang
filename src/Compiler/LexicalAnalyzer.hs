module Compiler.LexicalAnalyzer where

import           AST
import           Compiler.Analyzer.AExpr
import           Compiler.Analyzer.Statement
import           Compiler.Analyzer.Type
import           Compiler.Translator.Type
import           Control.Exception
import           Control.Monad.State         (State, get, gets, put)
import           Control.Monad.Writer        (WriterT, tell)

import Compiler.Analyzer.Pre

analyze :: AST -> Analyzer' AST
analyze (AST stmts) = do
  let indexDefined = findAllDeclaredNames stmts
  return (AST stmts)

statementAnalyzer :: Stmt -> Analyzer' Stmt
statementAnalyzer s =
  case s of
    t@Import {}         -> checkImport t
    t@LinkPath {}       -> checkLinkPath t
    t@Function {}       -> checkFunction t functionStmtAnalyzer
    t@NativeFunction {} -> checkNative t
    Assign a b c         -> checkAssign (a, b, c) Assign
    t@ClassExpr {}      -> checkClass t classStmtAnalyzer
    t@Skip              -> return t

functionStmtAnalyzer :: FunctionStmt -> Analyzer' FunctionStmt
functionStmtAnalyzer s =
  case s of
    AssignFn a b c -> checkAssign (a, b, c) AssignFn
    t@WhileFn {}  -> checkWhile t
    t@ForFn {}    -> checkFor t
    t@ReturnFn {} -> return t
    t@OtherFn {}  -> return t


classStmtAnalyzer :: ClassStmt -> Analyzer' ClassStmt
classStmtAnalyzer s =
  case s of
    t@ClassAssign {} -> return t
    t@Method {}      -> return t

aExprAnalyzer :: AExpr -> Analyzer' AExpr
aExprAnalyzer expr =
  case expr of
    e@IntConst {}   -> return e
    e@FloatConst {} -> return e
    e@StringVal {}  -> return e
    e@Var {}        -> checkVar e
    e@ListVar {}    -> checkListVar e
    e@Range {}      -> checkRange e
    e@Fn {}         -> checkFn e
    e@FnBlock {}    -> checkFnBlock e
    e@Neg {}        -> checkNegBlock e
    e@ABinary {}    -> checkABinary e
    e@If {}         -> checkIfStatement e

bExprAnalyzer :: BExpr -> Analyzer' BExpr
bExprAnalyzer expr =
  case expr of
    e@BoolConst {} -> return e
    e@Not {}       -> return e
    e@BBinary {}   -> return e
    e@RBinary {}   -> return e
