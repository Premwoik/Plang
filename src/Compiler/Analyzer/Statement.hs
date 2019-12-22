module Compiler.Analyzer.Statement where

import           AST
import           Compiler.Analyzer.Pre
import           Compiler.Analyzer.Type
import           Control.Exception      (throw)
import           Control.Monad.State    (get, gets, modify, put)
import           Control.Monad.Writer   (tell)

checkImport :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkImport t@(Import _) = return t

checkLinkPath :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkLinkPath t@(LinkPath p) = return t


checkFunction :: Stmt -> FnStmtAnalyzer -> Analyzer' Stmt 
checkFunction f@(Function n t a body) bodyAnalyzer = do
  setStmt f
  checkFunction' (n, t, a, body) Function bodyAnalyzer
  

checkMethod :: ClassStmt -> FnStmtAnalyzer -> Analyzer' ClassStmt
checkMethod m@(Method n t a body) bodyAnalyzer = do
  setMethod m
  method <- checkFunction' (n, t, a, body) Method bodyAnalyzer
  class' <- getClass
  return $ if getName class' == n then Constructor n (getBody method) else method
  where
    getBody (Method _ _ _ b) = b
    getName (ClassExpr n _ _) = n
  
  
checkFunction' :: RawFunction -> RawFunctionConst a -> FnStmtAnalyzer -> Analyzer' a
-- TODO check if arguments are good
checkFunction' (name, type', args, body) wrapper bodyAnalyzer = do
  checkedBody' <- concat <$> mapM bodyAnalyzer body
  nType <- getType <$> gets local
  return $ wrapper name nType args checkedBody'


checkReturn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkReturn (ReturnFn aExpr) analyzer = do
  funcType <- getType <$> gets local
  (t, inject, res) <- analyzer aExpr
  checkTypes funcType  t
  modify (\s -> s {cache = TypeCache [t]})
  return $ inject ++ [ReturnFn res]

checkTypes t t' =
  if t == t' || t == VAuto
    then return ()
    else throw $ TypesMismatch ("return missmatch: " ++ show t ++ " =/= " ++ show t')
--checkTypes t _ = throw $ TypesMismatch $ "Local don't contain function struct, so types cant be checked!!! { " ++ show t

checkNative :: Stmt -> Analyzer' Stmt
checkNative t@(NativeFunction name ret args') =
  case ret of
    VAuto -> throw IncorrectExprException
    _     -> NativeFunction name ret <$> checkArgs args'

checkArgs :: Maybe [FunArg] -> Analyzer' (Maybe [FunArg])
checkArgs = return

checkAssignFn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
--TODO
checkAssignFn (AssignFn name ret aExpr) analyzer = do
  (type', inject, res) <- analyzer aExpr
  res' <- checkType ret type' type' res
  return $ inject ++ res'
  where
    checkType a b nt r
      | a == b || a == VAuto = return [AssignFn name nt r]
      | otherwise = throw $ TypesMismatch (show a ++ " =/= " ++ show b)

checkAssign :: RawAssign -> RawAssignConst a -> AExprAnalyzer -> Analyzer' a
--TODO
checkAssign (name, ret, aExpr) wrapper analyzer = do
  (type', inject, res) <- analyzer aExpr
  nType <- checkType ret type'
  return $ wrapper name nType res
  where
    checkType a b
      | a == b || a == VAuto = return b
      | otherwise = throw $ TypesMismatch (show a ++ " =/= " ++ show b)

checkWhile :: FunctionStmt -> Analyzer' [FunctionStmt]
-- TODO
checkWhile t@(WhileFn cond block) = return . return $ t

checkFor :: FunctionStmt -> Analyzer' [FunctionStmt]
-- TODO
checkFor = return . return

checkClass :: Stmt -> ClassStmtAnalyzer -> Analyzer' Stmt
-- TODO
checkClass c@(ClassExpr name cast body) analyzer = do
  setStmt c
  body' <- mapM (analyzer name) body
  return $ ClassExpr name cast body'

--  loadClass name
checkOtherExpr :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkOtherExpr (OtherFn aExpr) analyzer = return . OtherFn . trd <$> analyzer aExpr
