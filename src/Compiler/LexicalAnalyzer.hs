module Compiler.LexicalAnalyzer where

import           AST
import           Compiler.Analyzer.AExpr
import           Compiler.Analyzer.BExpr
import           Compiler.Analyzer.Statement
import           Compiler.Analyzer.Type
import           Control.Exception
import           Control.Monad               (forM)
import           Control.Monad.State         (State, get, gets, modify, put)
import           Control.Monad.Writer        (WriterT, tell)
import           Data.Maybe                  (fromMaybe)

import qualified          Compiler.Importer as Im

analyze' :: [Imported] -> Analyzer' [Imported]
analyze' a =
  forM a $ \(IFile n ast) -> do
    res <- analyze ast
    saveFile n
    return $ IFile n res

analyze :: AST -> Analyzer' AST
analyze (AST stmts) = do
  fields <- loadFiles . map Im.getImportName . Im.filterImport $ AST stmts
  let globalScope = Scope "global" $ catalogueDecl (AST stmts) ++ fields
  modify (\storage -> storage {scopes = [globalScope]})
  stmts' <- mapM statementAnalyzer stmts
  return (AST stmts')

catalogueDecl :: AST -> [ScopeField]
catalogueDecl (AST stmts) = map mapper . filter cond $ stmts
  where
    cond Function {}  = True
    cond ClassExpr {} = True
    cond NativeClass {} = True
    cond NativeFunction {} = True
    cond _            = False
    mapper (Function o n t a _) = SFunction o n Nothing t a
    mapper (NativeFunction o p n t a) = SFunction o n (Just p) t a
    mapper (NativeClass o p n g _) = SClass o n (Just p) g (Scope n [])
    mapper (ClassExpr o n g _)  = SClass o n Nothing g (Scope n [])

statementAnalyzer :: Stmt -> Analyzer' Stmt
statementAnalyzer s =
  case s of
    t@Import {} -> checkImport t
    t@LinkPath {} -> checkLinkPath t
    t@Function {} -> checkFunction t functionStmtAnalyzer
    t@NativeFunction {} -> checkNative t
    t@NativeClass {} -> checkNativeClass t classStmtAnalyzer
    t@NativeAssignDeclaration {} -> checkNativeAssign t aExprAnalyzer
    t@Assign {}-> checkAssign t aExprAnalyzer
    t@ClassExpr {} -> checkClass t classStmtAnalyzer
    t@Skip {} -> return t

functionStmtAnalyzer :: FunctionStmt -> Analyzer' [FunctionStmt]
functionStmtAnalyzer s =
  case s of
    t@AssignFn {} -> checkAssignFn t aExprAnalyzer
    t@WhileFn {}  -> checkWhile t functionStmtAnalyzer bExprAnalyzer
    t@ForFn {}    -> checkFor t functionStmtAnalyzer aExprAnalyzer
    t@IfFn {}     -> checkIfFunction t functionStmtAnalyzer bExprAnalyzer
    t@ReturnFn {} -> checkReturn t aExprAnalyzer
    t@OtherFn {}  -> checkOtherExpr t aExprAnalyzer
    Pass          -> return . return $ Pass

classStmtAnalyzer :: String -> ClassStmt -> Analyzer' ClassStmt
classStmtAnalyzer name s =
  case s of
    t@ClassAssign {} -> checkClassAssign t aExprAnalyzer
    t@Method {}         -> checkMethod t functionStmtAnalyzer
    t@NativeMethod {}   -> checkMethodDeclaration t

aExprAnalyzer :: AExpr -> Analyzer' AExprRes
aExprAnalyzer expr =
  case expr of
    Nop             -> return (VBlank, [], Nop)
    e@ScopeMark {} -> checkScopeMark e aExprAnalyzer
    e@ABracket {}   -> checkBracket e aExprAnalyzer
    e@IntConst {}   -> return (VInt, [], e)
    e@FloatConst {} -> return (VFloat, [], e)
    e@StringVal {}  -> return (VString, [], e)
    e@Var {}        -> checkVar e Nothing "" aExprAnalyzer
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
