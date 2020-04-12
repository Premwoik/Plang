{-# LANGUAGE FlexibleContexts #-}
module Compiler.Analyzer.Statement where

import           AST

--import           Compiler.Analyzer.Pre
import           Compiler.Analyzer.Browser
import           Compiler.Analyzer.Type
import           Control.Monad.State       (get, gets, modify, put)
import           Control.Monad.Writer      (tell)
import           Data.Maybe                (fromJust, fromMaybe, listToMaybe)
import           Debug.Trace
import Control.Monad.Except(throwError)

checkImport :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkImport t@(Import _ _ _) = return t

checkLinkPath :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkLinkPath t@(LinkPath _ p) = return t

checkFunction :: Stmt -> FnStmtAnalyzer -> Analyzer' Stmt
checkFunction f@(Function o n t a body) bodyAnalyzer = do
  res <- checkFunction' (o, n, t, a, body) Function bodyAnalyzer
  addField $ SFunction o n Nothing t a
  checkFunctionUniqueness o ["", n] t a
  return res 

checkMethod :: ClassStmt -> FnStmtAnalyzer -> Analyzer' ClassStmt
checkMethod m@(Method o n t a body) bodyAnalyzer = do
  method <- checkFunction' (o, n, t, a, body) Method bodyAnalyzer
  let (Method o' n' t' a' body') = method
  gens <- getClassGens
  let nType = markGen t' gens
  let nArgs = markGenInArgs gens a'
  className <- gets cName
  addField $ SFunction o' n' Nothing nType nArgs
  checkFunctionUniqueness o' ["this", n'] nType nArgs
  return $
    if className == n
      then Constructor o' n' nArgs body'
      else Method o' n' nType nArgs body'

checkFunction' :: RawFunction -> RawFunctionConst a -> FnStmtAnalyzer -> Analyzer' a
checkFunction' (o, name, type', args, body) wrapper bodyAnalyzer = do
  args' <- checkArgs args
  setType type'
  addArgsScope args
  setFunName name
  addScope "fun"
  checkedBody' <- concat <$> mapM bodyAnalyzer body
  nType <- gets rType
  removeScope
  removeScope
  return $ wrapper o name nType args' checkedBody'

--  addFnScope body
checkReturn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkReturn (ReturnFn o aExpr) analyzer = do
  funcType <- gets rType
  (t, inject, res) <- analyzer aExpr
  nType <- checkTypes o funcType t
  setType nType
  return $ inject ++ [ReturnFn o res]

checkTypes o wanted actual
  | wanted == actual || wanted == VAuto = return wanted
  | otherwise =
    throwError $ AException o ("return missmatch: FuncDeclType = " ++ show wanted ++ " =/= FuncRetType " ++ show actual)

--checkTypes t _ = throw $ TypesMismatch $ "Local don't contain function struct, so types cant be checked!!! { " ++ show t
checkNative :: Stmt -> Analyzer' Stmt
--checkNative t@(NativeFunction o "" name ret args') = NativeFunction o name name ret <$> checkArgs args'
checkNative t@(NativeFunction o path name ret args) = do
  args' <- checkArgs args
  addField $ SFunction o name (Just path) ret args'
  return $ NativeFunction o path name ret args'

--    VAuto -> throw IncorrectExprException
checkArgs :: [FunArg] -> Analyzer' [FunArg]
checkArgs = return . map (\(FunArg t n) -> FunArg t (concat (scaleNameWithScope ["args", n])))

checkMethodDeclaration :: ClassStmt -> Analyzer' ClassStmt
checkMethodDeclaration ttt@(NativeMethod o n t args) = do
  gen <- getClassGens
  let nType = markGen t gen
  let nArgs = markGenInArgs gen args
  addField $ SFunction o n Nothing nType nArgs
  return $ NativeMethod o n nType nArgs

markGen (VClass g [] p) gen =
  if g `elem` gen
    then VGen g
    else VClass g [] p
markGen x _ = x

markGenInArgs gen = map checkType
  where
    checkType i@(FunArg (VClass g [] _) n) =
      if g `elem` gen
        then FunArg (VGen g) n
        else i
    checkType i = i



checkAssignFn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkAssignFn a@(AssignFn o name ret aExpr) analyzer = do
  s <- get
  (type', inject, res) <- analyzer aExpr
--  trace ("DIS S " ++ show type') $ return ()
  firstSig <- listToMaybe <$> find' name
  nType <-
    case (firstSig, head name) of
      (_, "args") -> throwError $ AException o ("Function argument can't be reassign in: " ++ show a)
      (Just (SVar o n _ t s), s') ->
        if s == s'
          then check t ret type' VBlank
          else add type'
      (Nothing, "") -> add type'
      x -> error $ show x ++ " Dis to kurwa elo"
--  trace ("ASSIGN to" ++ show name ++ " = type " ++ show nType) $ return ()
  return $ inject ++ [AssignFn o (scaleNameWithScope name) nType res]
  where
    add type' = addField (SVar o (last name) Nothing type' "") >> check type' ret type' type'
    check wantedDecl wanted actual res
      | wantedDecl == actual && (wanted == actual || wanted == VAuto) = return res
      | otherwise = throwError $ AException o ("Types don't match. You tried to assign " ++ show actual  ++ " when should be " ++ show wanted ++ ".\n" ++ show actual ++ " =/= " ++ show wanted)

checkNativeAssign :: Stmt -> AExprAnalyzer -> Analyzer' Stmt
checkNativeAssign s@(NativeAssignDeclaration o p n t) analyzer = do
  addField $ SVar o n (Just p) t ""
  return s



checkAssign :: Stmt -> AExprAnalyzer -> Analyzer' Stmt
checkAssign (Assign o name ret aExpr) analyzer = do
  (type', inject, res) <- analyzer aExpr
  gen <- getClassGens
  nType <- flip markGen gen <$> checkType ret type'
  addField $ SVar o (last name) Nothing nType "g"
  let mergedNameWithScope = (scaleNameWithScope ("g" : name))
  return $ Assign o mergedNameWithScope nType res
  where
    checkType a b
      | b == VBlank = return a
      | a == b || a == VAuto = return b
      | otherwise = throwError $ AException o ("Types don't match. You tried to assign " ++ show b  ++ " when should be " ++ show a ++ ".\n" ++ show a ++ " =/= " ++ show b)


checkWhile :: FunctionStmt -> FnStmtAnalyzer -> BExprAnalyzer -> Analyzer' [FunctionStmt]
checkWhile t@(WhileFn o cond block) analyzer bAnalyzer = do
  addScope "while"
  cond' <- bAnalyzer cond
  block' <- concat <$> mapM analyzer block
  removeScope
  return . return $ WhileFn o cond' block'

checkIfFunction :: FunctionStmt -> FnStmtAnalyzer -> BExprAnalyzer -> Analyzer' [FunctionStmt]
checkIfFunction t@(IfFn o ifs) analyzer bExprAnalyzer = do
  newIfs <- mapM makeIf ifs
  return [IfFn o newIfs]
  where
    makeIf (cond, body) = do
      addScope "if"
      body' <- concat <$> mapM analyzer body
      cond' <- bExprAnalyzer cond
      removeScope
      return (cond', body')

checkFor :: FunctionStmt -> FnStmtAnalyzer -> AExprAnalyzer -> Analyzer' [FunctionStmt]
-- TODO
checkFor (ForFn o (Var _ n _ _ _) range body) fnAnalyzer aAnalyzer = do
  addScope "for"
  (t, _, range') <- aAnalyzer range
  body' <- concat <$> mapM fnAnalyzer body
  removeScope
  return [ForFn o (TypedVar (VName n) VInt Nothing Nothing) range' body']

-- | CLASS
--  TODO add aExpr analyzing (range and var)
--  (_, _, var') <- aAnalyzer var
-- TODO
checkClass :: Stmt -> ClassStmtAnalyzer -> Analyzer' Stmt
checkClass c@(ClassExpr o name cast body) analyzer = do
  setClassName name
  addScope "this"
  mapM_ (addField . SGen) cast
  body' <- mapM (analyzer name) body
  cScope <- removeScope
  addField $ SClass o name Nothing cast cScope
  return $ ClassExpr o name cast body'

-- TODO
checkNativeClass :: Stmt -> ClassStmtAnalyzer -> Analyzer' Stmt
checkNativeClass c@(NativeClass o p name cast body) analyzer = do
  setClassName name
  addScope "this"
  mapM_ (addField . SGen) cast
  body' <- mapM (analyzer name) body
  cScope <- removeScope
  addField (SClass o name (Just p) cast cScope)
  return $ NativeClass o p name cast body'

checkClassAssign :: ClassStmt -> AExprAnalyzer -> Analyzer' ClassStmt
checkClassAssign (ClassAssign o name ret aExpr) analyzer = do
  (type', inject, res) <- analyzer aExpr
  gen <- getClassGens
  nType <- flip markGen gen <$> checkType ret type'
  addField $ SVar o (last name) Nothing ret "this"
  let mergedNameWithScope = (scaleNameWithScope ("this" : name))
  return $ ClassAssign o mergedNameWithScope nType res
  where
    checkType a b
      | b == VBlank = return a
      | a == b || a == VAuto = return b
      | otherwise = throwError $ AException o ("Types don't match. You tried to assign " ++ show b  ++ " when should be " ++ show a ++ ".\n" ++ show a ++ " =/= " ++ show b)


-- | OTHER EXPR
checkOtherExpr :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkOtherExpr (OtherFn o aExpr) analyzer = return . OtherFn o . trd <$> analyzer aExpr

