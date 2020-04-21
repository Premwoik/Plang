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
import Debug.Trace

import qualified          Compiler.Importer as Im

analyze' :: [Imported] -> Analyzer' [Imported]
analyze' a = do
  trace (show (map (\(IFile n _ _) -> n) a)) $ return ()
  forM a $ \(IFile n p ast) -> do
    res <- analyze n p ast
    saveFile n
    return $ IFile n p res

analyze :: String -> String -> AST -> Analyzer' AST
analyze modName path (AST stmts) = do
  setModuleInfo (-1) modName path
  traceShow modName $ return ()
  fields <- loadFiles . map (\i -> (Im.getImportName i, Im.getImportAlias i)) . Im.filterImport $ AST stmts
  let (globalFields, fileScopes) = fields
  let globalScope = Scope "global" $ catalogueDecl modName path (AST stmts)
  let importScope = Scope "import" globalFields
  modify (\storage -> storage {scopes = globalScope : importScope : fileScopes})
  s <- gets scopes
  trace (show s) $ return ()
  stmts' <- mapM statementAnalyzer stmts
  return (AST stmts')

catalogueDecl :: String -> String -> AST -> [ScopeField]
catalogueDecl modName p (AST stmts) = map mapper . filter cond $ stmts
  where
    cond Function {}  = True
    cond ClassExpr {} = True
    cond NativeClass {} = True
    cond NativeFunction {} = True
    cond _            = False
    mapper (Function o n t a _) = SFunction (FileInfo o modName p) n Nothing t a
    mapper (NativeFunction o p n t a) = SFunction (FileInfo o modName p) n (Just p) t a
    mapper (NativeClass o p n g _) = SClass (FileInfo o modName p) n (Just p) g (Scope n [])
    mapper (ClassExpr o n g _)  = SClass (FileInfo o modName p) n Nothing g (Scope n [])

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
    e@IntConst {}   -> checkIntConst e 
    e@FloatConst {} -> checkFloatConst e
    e@StringVal {}  -> checkStringConst e
    e@Var {}        -> checkVar e Nothing "" aExprAnalyzer
    e@ListVar {}    -> checkListVar e aExprAnalyzer
    e@Range {}      -> checkRange e
    e@Fn {}         -> checkFn e
    e@FnBlock {}    -> checkFnBlock e
    e@Neg {}        -> checkNegBlock e
    e@ABinary {}    -> checkABinary e aExprAnalyzer
    e@If {}         -> checkIfStatement e functionStmtAnalyzer bExprAnalyzer
    e@ABool {}      -> checkABool e bExprAnalyzer

bExprAnalyzer :: BExpr -> Analyzer' BExpr
bExprAnalyzer expr =
  case expr of
    e@BoolConst {} -> return e
    e@Not {}       -> return e
    e@BBinary {}   -> return e
    e@RBinary {}   -> rBinaryAnalyzer e aExprAnalyzer
