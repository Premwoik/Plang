module Compiler.LexicalAnalyzer where

import           AST
import           Compiler.Analyzer.AExpr
import           Compiler.Analyzer.BExpr
import           Compiler.Analyzer.Statement
import           Compiler.Analyzer.Type
import           Control.Exception
import           Control.Monad.State         (State, get, gets, put)
import           Control.Monad.Writer        (WriterT, tell)


analyze :: AST -> Analyzer' AST
analyze (AST stmts) = do
  storage <- get
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
    t@NativeClass {} -> return t
    t@NativeAssignDeclaration {} -> return t
    Assign o a b c      -> checkAssign (o, a, b, c) Assign aExprAnalyzer
    t@ClassExpr {}      -> checkClass t classStmtAnalyzer
    t@Skip {}           -> return t

--    TODO be careful with this head, but probably it always should work great
functionStmtAnalyzer :: FunctionStmt -> Analyzer' [FunctionStmt]
functionStmtAnalyzer s =
  case s of
    t@AssignFn {} -> checkAssignFn t aExprAnalyzer
    t@WhileFn {}  -> checkWhile t functionStmtAnalyzer bExprAnalyzer
    t@ForFn {}    -> checkFor t functionStmtAnalyzer aExprAnalyzer
    t@IfFn {}     -> checkIfFunction t functionStmtAnalyzer bExprAnalyzer
    t@ReturnFn {} -> checkReturn t aExprAnalyzer
    t@OtherFn {}  -> checkOtherExpr t aExprAnalyzer

classStmtAnalyzer :: String -> ClassStmt -> Analyzer' ClassStmt
classStmtAnalyzer name s =
  case s of
    ClassAssign o a b c -> checkAssign (o, a, b, c) ClassAssign aExprAnalyzer
    t@Method {}         -> checkMethod t functionStmtAnalyzer
    t@MethodDeclaration {} -> return t

aExprAnalyzer :: AExpr -> Analyzer' AExprRes
aExprAnalyzer expr =
  case expr of
    e@IntConst {}   -> return (VInt, [], e)
    e@FloatConst {} -> return (VFloat, [], e)
    e@StringVal {}  -> return (VString, [], e)
    e@Var {}        -> checkVar e Nothing aExprAnalyzer
    e@ListVar {}    -> checkListVar e aExprAnalyzer
    e@Range {}      -> checkRange e
    e@Fn {}         -> checkFn e
    e@FnBlock {}    -> checkFnBlock e
    e@Neg {}        -> checkNegBlock e
    e@ABinary {}    -> checkABinary e aExprAnalyzer
    e@If {}         -> checkIfStatement e functionStmtAnalyzer bExprAnalyzer

bExprAnalyzer :: BExpr -> Analyzer' BExpr
bExprAnalyzer expr =
  case expr of
    e@BoolConst {} -> return e
    e@Not {}       -> return e
    e@BBinary {}   -> return e
    e@RBinary {}   -> rBinaryAnalyzer e aExprAnalyzer
