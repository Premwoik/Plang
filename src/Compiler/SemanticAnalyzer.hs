module Compiler.SemanticAnalyzer(analyze', getAnalyzers) where

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

getAnalyzers = Dependencies
  { aExprAnalyzerGetter = aExprAnalyzer
  , bExprAnalyzerGetter = bExprAnalyzer
  , stmtAnalyzerGetter = statementAnalyzer
  , functionStmtAnalyzerGetter = functionStmtAnalyzer
  , classStmtAnalyzerGetter = classStmtAnalyzer
}


analyze' :: [Imported] -> Analyzer' [Imported]
analyze' a = 
  forM a $ \(IFile n p ast) -> do
    res <- analyze n p ast
    saveFile n
    return $ IFile n p res

analyze :: String -> String -> AST -> Analyzer' AST
analyze modName path (AST stmts) = do
  setModuleInfo (-1) modName path
  fields <- loadFiles . map (\i -> (Im.getImportName i, Im.getImportAlias i)) . Im.filterImport $ AST stmts
  let (globalFields, fileScopes) = fields
  let globalScope = Scope "global" $ catalogueDecl modName path (AST stmts)
  let importScope = Scope "import" globalFields
  modify (\storage -> storage {scopes = globalScope : importScope : fileScopes})
  s <- gets scopes
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
    mapper (Function o n _ t a _) = SFunction (FileInfo o modName p) n Nothing t a
    mapper (NativeFunction o p n _ t a) = SFunction (FileInfo o modName p) n (Just p) t a
    mapper (NativeClass o p n g _) = SClass (FileInfo o modName p) n (Just p) g (Scope n []) 
    mapper (ClassExpr o n g _)  = SClass (FileInfo o modName p) n Nothing g (Scope n []) 

statementAnalyzer :: Stmt -> Analyzer' Stmt
statementAnalyzer s =
  case s of
    t@Import {} -> checkImport t
    t@LinkPath {} -> checkLinkPath t
    t@Function {} -> checkFunction t 
    t@NativeFunction {} -> checkNative t
    t@NativeClass {} -> checkNativeClass t 
    t@NativeAssignDeclaration {} -> checkNativeAssign t 
    t@Assign {}-> checkAssign t 
    t@ClassExpr {} -> checkClass t 
    t@Skip {} -> return t


functionStmtAnalyzer :: FunctionStmt -> Analyzer' [FunctionStmt]
functionStmtAnalyzer s =
  case s of
    t@AssignFn {} -> checkAssignFn t 
    t@WhileFn {}  -> checkWhile t 
    t@ForFn {}    -> checkFor t 
    t@IfFn {}     -> checkIfFunction t 
    t@ReturnFn {} -> checkReturn t 
    t@OtherFn {}  -> checkOtherExpr t 
    t@Break {} -> checkBreak t
    Pass          -> return . return $ Pass

classStmtAnalyzer :: ClassStmt -> Analyzer' ClassStmt
classStmtAnalyzer s =
  case s of
    t@ClassAssign {} -> checkClassAssign t 
    t@Method {}         -> checkMethod t 
    t@NativeMethod {}   -> checkMethodDeclaration t

aExprAnalyzer :: AExpr -> Analyzer' AExprRes
aExprAnalyzer expr =
  case expr of
    e@ABracketApply {} -> checkBracketApply e 
    e@Optional {} -> checkOptional e 
    e@ScopeMark {} -> checkScopeMark e 
    e@ABracket {}   -> checkBracket e 
    e@IntConst {}   -> checkIntConst e 
    e@FloatConst {} -> checkFloatConst e
    e@StringVal {}  -> checkStringConst e
    e@Var {}        -> checkVar e Nothing "" 
    e@ListVar {}    -> checkListVar e 
    e@Range {}      -> checkRange e 
    e@LambdaFn {}   -> checkLambdaFn False e  
    e@Neg {}        -> checkNegBlock e 
    e@ABinary {}    -> checkABinary e 
    e@If {}         -> checkIfStatement e 
    e@ABool {}      -> checkABool e 
    Nop             -> return (VBlank, [], Nop)

bExprAnalyzer :: BExpr -> Analyzer' BExpr
bExprAnalyzer expr =
  case expr of
    e@BoolConst {} -> return e
    e@BoolVar {} -> boolVarAnalyzer e 
    e@Not {}       -> notAnalyzer e 
    e@BBinary {}   -> bBinaryAnalyzer e 
    e@RBinary {}   -> rBinaryAnalyzer e 
