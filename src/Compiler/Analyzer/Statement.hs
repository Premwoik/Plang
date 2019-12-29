module Compiler.Analyzer.Statement where

import           AST
--import           Compiler.Analyzer.Pre
import Compiler.Analyzer.Browser
import           Compiler.Analyzer.Type
import           Control.Exception      (throw)
import           Control.Monad.State    (get, gets, modify, put)
import           Control.Monad.Writer   (tell)

checkImport :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkImport t@(Import _ _) = return t

checkLinkPath :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkLinkPath t@(LinkPath _ p) = return t


checkFunction :: Stmt -> FnStmtAnalyzer -> Analyzer' Stmt 
checkFunction f@(Function o n t a body) bodyAnalyzer = do
  setStmt f
  checkFunction' (o, n, t, a, body) Function bodyAnalyzer
  

checkMethod :: ClassStmt -> FnStmtAnalyzer -> Analyzer' ClassStmt
checkMethod m@(Method o n t a body) bodyAnalyzer = do
  setMethod m
  method <- checkFunction' (o, n, t, a, body) Method bodyAnalyzer
  class' <- getClass
  return $ if getName class' == n then Constructor o n (getBody method) else method
  where
    getBody (Method _ _ _ _ b) = b
    getName (ClassExpr _ n _ _) = n
  
  
checkFunction' :: RawFunction -> RawFunctionConst a -> FnStmtAnalyzer -> Analyzer' a
-- TODO check if arguments are good
checkFunction' (o, name, type', args, body) wrapper bodyAnalyzer = do
  checkedBody' <- concat <$> mapM bodyAnalyzer body
  nType <- getType <$> gets local
--  retType <- retType . unwrap <$> gets cache
  return $ wrapper o name nType args checkedBody'
--  where
--    unwrap (TypeCache l) = l
--    unwrap _ = []
--    retType (t:s) = if all (==t) s then t else error "If statement return' type is not the same in every block"
--    retType _ = error "If statement return' type is not the same in every block"


checkReturn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkReturn (ReturnFn o aExpr) analyzer = do
  local' <- gets local
  let funcType = getType local'
  (t, inject, res) <- analyzer aExpr
  nType <- checkTypes funcType t
  modify (\s -> s {local = setType nType local'})
  return $ inject ++ [ReturnFn o res]
    
checkTypes t t' =
  if t == t' || t == VAuto
    then return t'
    else throw $ TypesMismatch ("return missmatch: " ++ show t ++ " =/= " ++ show t')
--checkTypes t _ = throw $ TypesMismatch $ "Local don't contain function struct, so types cant be checked!!! { " ++ show t

checkNative :: Stmt -> Analyzer' Stmt
checkNative t@(NativeFunction o "" name ret args') = NativeFunction o name name ret <$> checkArgs args'
checkNative t@(NativeFunction o path name ret args') =
 case ret of
--    VAuto -> throw IncorrectExprException
   _     -> NativeFunction o path name ret <$> checkArgs args'


checkArgs :: Maybe [FunArg] -> Analyzer' (Maybe [FunArg])
checkArgs = return

checkAssignFn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
--TODO
checkAssignFn (AssignFn o name ret aExpr) analyzer = do
  s <- get
  (type', inject, res) <- analyzer aExpr
  nType <- case findFirst name (local s) (global s) of
    Just a@Assign {} -> return $ checkType2 a type'
    _ -> return type'
    
  res' <- checkType ret type' nType res
  return $ inject ++ res'
  where
    checkType2 (Assign o' _ t _) b 
      | o' == o = b
      | t == b || t == VAuto = VBlank
      | otherwise = throw $ TypesMismatch (show t ++ " =/= " ++ show b)
    checkType a b nt r
      | a == b || a == VAuto = return [AssignFn o name nt r]
      | otherwise = throw $ TypesMismatch (show a ++ " =/= " ++ show b)
      

checkAssign :: RawAssign -> RawAssignConst a -> AExprAnalyzer -> Analyzer' a
--TODO
checkAssign (o, name, ret, aExpr) wrapper analyzer = do
  (type', inject, res) <- analyzer aExpr
  nType <- checkType ret type'
  return $ wrapper o name nType res
  where
    checkType a b
      | a == b || a == VAuto = return b
      | otherwise = throw $ TypesMismatch (show a ++ " =/= " ++ show b)

checkWhile :: FunctionStmt -> FnStmtAnalyzer -> BExprAnalyzer ->Analyzer' [FunctionStmt]
checkWhile t@(WhileFn o cond block) analyzer bAnalyzer= do
  addFnScope block
  cond' <- bAnalyzer cond 
  block' <- concat <$> mapM analyzer block
  removeFnScope
  return . return $ WhileFn o cond' block'

checkIfFunction :: FunctionStmt -> FnStmtAnalyzer -> BExprAnalyzer -> Analyzer' [FunctionStmt]
checkIfFunction t@(IfFn o ifs) analyzer bExprAnalyzer = do
  newIfs <- mapM makeIf ifs
--  let readyRetType = retType types
--  let newCache = [AssignFn "fuckT12" readyRetType Nop, IfFn newIfs]
--  return (readyRetType, newCache, Var "fuckT12" Nothing Nothing)
  return [IfFn o newIfs]
  where
    makeIf (cond, body) = do
      addFnScope body
      body' <- concat <$> mapM analyzer body
      cond' <- bExprAnalyzer cond
      removeFnScope
      return (cond', body')
--    retType (t:s) = if all (==t) s then t else error "If statement return' type is not the same in every block"
--    retType _ = error "If statement return' type is not the same in every block"


checkFor :: FunctionStmt -> FnStmtAnalyzer -> AExprAnalyzer -> Analyzer' [FunctionStmt]
-- TODO
checkFor (ForFn o (Var n _ _) range body) fnAnalyzer aAnalyzer= do
--  TODO add aExpr analyzing (range and var)
--  (_, _, var') <- aAnalyzer var
  addFnScope body
  (t, _, range') <- aAnalyzer range
  body' <- concat <$> mapM fnAnalyzer body
  removeFnScope
  return [ForFn o (TypedVar n VInt Nothing Nothing) range' body'] 
  

checkClass :: Stmt -> ClassStmtAnalyzer -> Analyzer' Stmt
-- TODO
checkClass c@(ClassExpr o name cast body) analyzer = do
  setStmt c
  body' <- mapM (analyzer name) body
  return $ ClassExpr o name cast body'

--  loadClass name
checkOtherExpr :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkOtherExpr (OtherFn o aExpr) analyzer = return . OtherFn o . trd <$> analyzer aExpr
