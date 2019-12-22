module Compiler.LexicalAnalyzer where

import           AST
import           Compiler.Analyzer.AExpr
import           Compiler.Analyzer.Statement
import           Compiler.Analyzer.Type
import           Control.Exception
import           Control.Monad.State         (State, get, gets, put)
import           Control.Monad.Writer        (WriterT, tell)

import           Compiler.Analyzer.Pre

analyze :: AST -> Analyzer' AST
analyze (AST stmts) = do
  storage <- get
  let g = findAllDeclaredNames stmts
  put $ storage {global = stmts}
  stmts' <- mapM statementAnalyzer stmts
  return (AST stmts')

statementAnalyzer :: Stmt -> Analyzer' Stmt
statementAnalyzer s =
  case s of
    t@Import {}         -> checkImport t
    t@LinkPath {}       -> checkLinkPath t
    t@Function {}       -> checkFunction t functionStmtAnalyzer
    t@NativeFunction {} -> checkNative t
--    TODO be careful with this head, but probably it always should work great
    Assign a b c        -> checkAssign (a, b, c) Assign aExprAnalyzer
    t@ClassExpr {}      -> checkClass t classStmtAnalyzer
    t@Skip              -> return t

functionStmtAnalyzer :: FunctionStmt -> Analyzer' [FunctionStmt]
functionStmtAnalyzer s =
  case s of
    t@AssignFn {}  -> checkAssignFn t aExprAnalyzer
    t@WhileFn {}   -> checkWhile t
    t@ForFn {}     -> checkFor t
    t@ReturnFn {}  -> checkReturn t aExprAnalyzer
    t@OtherFn {}   -> checkOtherExpr t aExprAnalyzer

classStmtAnalyzer :: String -> ClassStmt -> Analyzer' ClassStmt
classStmtAnalyzer name s =
  case s of
    ClassAssign a b c -> checkAssign (a, b, c) ClassAssign aExprAnalyzer
    t@Method {} -> checkMethod t functionStmtAnalyzer

aExprAnalyzer :: AExpr -> Analyzer' AExprRes
aExprAnalyzer expr =
  case expr of
    e@IntConst {}   -> return (VInt, [], e)
    e@FloatConst {} -> return (VFloat, [], e)
    e@StringVal {}  -> return (VString, [], e)
    e@Var {}        -> checkVar e Nothing
    e@ListVar {}    -> checkListVar e
    e@Range {}      -> checkRange e
    e@Fn {}         -> checkFn e
    e@FnBlock {}    -> checkFnBlock e
    e@Neg {}        -> checkNegBlock e
    e@ABinary {}    -> checkABinary e
    e@If {}         -> checkIfStatement e functionStmtAnalyzer

bExprAnalyzer :: BExpr -> Analyzer' BExpr
bExprAnalyzer expr =
  case expr of
    e@BoolConst {} -> return e
    e@Not {}       -> return e
    e@BBinary {}   -> return e
    e@RBinary {}   -> return e


