module Compiler.Analyzer.Statement where

import           AST
--import           Compiler.Analyzer.Pre
import Compiler.Analyzer.Browser
import           Compiler.Analyzer.Type
import           Control.Exception      (throw)
import           Control.Monad.State    (get, gets, modify, put)
import           Control.Monad.Writer   (tell)
import Data.Maybe (fromMaybe, fromJust)
import Debug.Trace

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
  let (Method o' n' t' a' body') = method
  class' <- fromJust <$> getClass
  let nType = markGen t' (getGen' class')
  let nArgs = markGenInArgs (getGen' class') a'
  return $ if getName class' == n then Constructor o' n' nArgs body' else Method o' n' nType nArgs body'
  where
    getName (ClassExpr _ n _ _) = n
    getGen' (ClassExpr _ _ g _) = g



checkFunction' :: RawFunction -> RawFunctionConst a -> FnStmtAnalyzer -> Analyzer' a
checkFunction' (o, name, type', args, body) wrapper bodyAnalyzer = do
--  TODO fix this maybe
  addFnScope body
  checkedBody' <- concat <$> mapM bodyAnalyzer body
  nType <- getType <$> gets local
  return $ wrapper o name nType args checkedBody'

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

checkArgs :: [FunArg] -> Analyzer' [FunArg]
checkArgs = return

checkMethodDeclaration :: ClassStmt -> Analyzer' ClassStmt
checkMethodDeclaration ttt@(NativeMethod o n t args) = do
  gen <- getGen
  let nType = markGen t gen
  let nArgs = markGenInArgs gen args
  return $ NativeMethod o n nType nArgs


markGen (VClass g []) gen = if g `elem` gen then VGen g else VClass g []
markGen x _ = x

markGenInArgs gen = map checkType
  where
    checkType i@(FunArg (VClass g []) n) =
      if g `elem` gen
        then FunArg (VGen g) n
        else i
    checkType i = i

checkAssignFn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
--TODO
checkAssignFn (AssignFn o name ret aExpr) analyzer = do
  s <- get
  (type', inject, res) <- analyzer aExpr
  nType <-
    case findFirst name (local s) (global s) of
      Just a@Assign {} -> return $ checkType2 a type' name
      _ -> return type'
  res' <- checkType ret type' nType res
  trace (show name ++ " | " ++ show res') $ return ()
  replaceInFunc res'
  return $ inject ++ [res']
  where
    checkType2 (Assign o' n t _) b nn
      | o' == o = b
      | t == b || t == VAuto = VBlank
      | otherwise = throw $ TypesMismatch (show t ++ " =1 "++ show nn ++ "/= " ++ show b)
--      TODO change this shit when VGenClass will be deleted
    checkType a b nt r
      | a == b || a == VAuto = return $ AssignFn o name nt r
      | otherwise = throw $ TypesMismatch (show a ++ " =2/= " ++ show b)

checkAssign :: RawAssign -> RawAssignConst a -> AExprAnalyzer -> Analyzer' a
--TODO
checkAssign (o, name, ret, aExpr) wrapper analyzer = do
  (type', inject, res) <- analyzer aExpr
  gen <- getGen
  nType <- flip markGen gen <$> checkType ret type'
  return $ wrapper o name nType res
  where
    checkType a b
      | b == VBlank = return a
      | a == b || a == VAuto = return b
      | otherwise = throw $ TypesMismatch (show a ++ " =3/= " ++ show b)

--checkClassAssignDecl :: ClassStmt -> Analyzer' ClassStmt
--checkClassAssignDecl (ClassAssignDeclaration o n t) = do
--  class' <- getClass
--  let nType = markGen t (getGen class')
--  return $ ClassAssignDeclaration o n nType 

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
  return [IfFn o newIfs]
  where
    makeIf (cond, body) = do
      addFnScope body
      body' <- concat <$> mapM analyzer body
      cond' <- bExprAnalyzer cond
      removeFnScope
      return (cond', body')


checkFor :: FunctionStmt -> FnStmtAnalyzer -> AExprAnalyzer -> Analyzer' [FunctionStmt]
-- TODO
checkFor (ForFn o (Var n _ _ _) range body) fnAnalyzer aAnalyzer= do
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
  let res = ClassExpr o name cast body'
  updateClassInGlobal res
  return res
  
checkNativeClass :: Stmt -> ClassStmtAnalyzer -> Analyzer' Stmt
-- TODO
checkNativeClass c@(NativeClass o p name cast body) analyzer = do
  setStmt c
  body' <- mapM (analyzer name) body
  let res = NativeClass o p name cast body'
  updateClassInGlobal res
  return res


--  loadClass name
checkOtherExpr :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkOtherExpr (OtherFn o aExpr) analyzer = return . OtherFn o . trd <$> analyzer aExpr
