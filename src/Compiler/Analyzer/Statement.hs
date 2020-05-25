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
import Compiler.Analyzer.UniversalCheckers
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
  addFunction o n Nothing t a
  
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
--  trace ("checkMethod: " ++ show n ++ " " ++ show nType) $ return ()
  addFunction o' n' Nothing nType nArgs
  checkFunctionUniqueness o' ["this", n'] nType nArgs
  return $
    if className == n
      then Constructor o' n' nArgs body'
      else Method o' n' nType nArgs body'

checkFunction' :: RawFunction -> RawFunctionConst a -> FnStmtAnalyzer -> Analyzer' a
checkFunction' (o, name, type', args, body) wrapper bodyAnalyzer = do
  args' <- checkFnArgs args
  setType =<< fixNativeClassType type'
  addArgsScope o args
  setFunName name
  addScope "fun"
  checkedBody' <- concat <$> mapM bodyAnalyzer body
  nType <- gets rType
  removeScope
  removeScope
  return $ wrapper o name nType args' checkedBody'

checkReturn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkReturn (ReturnFn o (Just aExpr)) analyzer = do
  gen <- getClassGens
  funcType <- flip markGen gen <$> gets rType
  (t, inject, res) <- analyzer aExpr
  nType <- compareGens o funcType t
  setType nType
  return $ inject ++ [ReturnFn o (Just res)]
checkReturn (ReturnFn o Nothing) analyzer = do
  gen <- getClassGens
  funcType <- flip markGen gen <$> gets rType
  nType <- compareGens o funcType VVoid
  setType nType
  return [ReturnFn o Nothing]

--checkTypes o wanted actual
--  | wanted == actual || wanted == VAuto = return wanted
--  | otherwise =
--    makeError o ("return missmatch: FuncDeclType = " ++ show wanted ++ " =/= FuncRetType " ++ show actual)

checkNative :: Stmt -> Analyzer' Stmt
checkNative t@(NativeFunction o path name ret args) = do
  ret' <- fixNativeClassType ret
  args' <- checkFnArgs args
  addFunction o name (Just path) ret' args'
  return $ NativeFunction o path name ret' args'


checkMethodDeclaration :: ClassStmt -> Analyzer' ClassStmt
checkMethodDeclaration ttt@(NativeMethod o n t args) = do
  gen <- getClassGens
  ret <- fixNativeClassType t
  let nType = markGen ret gen
  let nArgs = markGenInArgs gen args
  addFunction o n Nothing nType nArgs
  return $ NativeMethod o n nType nArgs

markGen (VClass (VName g) []) gen =
  if g `elem` gen
    then VGen g
    else VClass (VName g) []
markGen x _ = x

markGenInArgs gen = map checkType
  where
    checkType i@(FunArg (VClass (VName g) []) n) =
      if g `elem` gen
        then FunArg (VGen g) n
        else i
    checkType i = i


unwrapAllMethod :: VarType -> VarType
--unwrapAllMethod (VRef x) = x
unwrapAllMethod (VCopy (VPointer x _)) = VCopy x
unwrapAllMethod (VRef (VCopy x)) = VRef x
unwrapAllMethod (VPointer (VCopy x) y) = VPointer x y
unwrapAllMethod (VPointer t NativePtr) = VPointer t SharedPtr
unwrapAllMethod x = x

markNativePtr (VPointer t NativePtr, i, res) = (VPointer t SharedPtr, i, NativePtrRes res)
markNativePtr x = x

checkAssignFn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkAssignFn a@(AssignFn o var@(Var vo vname [] Nothing Nothing) ret aExpr) analyzer = do
  s <- get
  retTmp <- gets rType
  setType ret
  (type', inject, res) <- markNativePtr <$> analyzer aExpr
  setType retTmp
  firstSig <- listToMaybe <$> find' ["", vname]
  nType <-
    case firstSig of
      Just (SVar o' n _ t "") ->
        check o t ret type' VBlank
      Just {} -> add type'
      Nothing -> add type'
      
  return $ inject ++ [AssignFn o (TypedVar (VName vname) VAuto Nothing Nothing) (unwrapAllMethod nType) res]
  where
    add type' = addVar o vname Nothing (unwrapAllMethod type') "" >> check o type' ret type' type'

checkAssignFn a@(AssignFn o nameExpr ret aExpr) analyzer =
  case hasSet nameExpr aExpr of
    Just resExpr -> do
      (nType, nInject, nRes) <- analyzer resExpr
      return $ nInject ++ [OtherFn o nRes]
    Nothing -> do
      (type', inject, res) <- markNativePtr <$> analyzer aExpr
      (nType, nInject, nRes) <- analyzer nameExpr
      nType <- check o nType ret type' VBlank
      let assign = AssignFn o nRes nType res
      return $ nInject ++ inject ++ [assign]
  where
    hasSet :: AExpr -> AExpr -> Maybe AExpr
    hasSet t@(Var o "set" g (Just args) Nothing) rSide = 
      return $ Var o "set" g (Just (rSide : args)) Nothing
    hasSet t@(Var _ _ _ _ Nothing) _ = 
      Nothing
    hasSet (Var a b c d (Just more)) rSide =
      return $ Var a b c d $ hasSet more rSide
    hasSet (ScopeMark o n a) rSide = do
      res <- hasSet a rSide
      return $ ScopeMark o n res
  
  
  
check o wantedDecl wanted actual res
  | wantedDecl == actual && (wanted == actual || wanted == VAuto) = return extendedIntCheck
  | otherwise =
    makeError o $ "Types don't match. You tried to assign " ++ show actual
      ++ " when should be " ++ show wanted ++ ".\n" ++ show actual ++ " =/= " ++ show wanted
  where
    extendedIntCheck = case wanted of 
      VNum {} -> wanted
      _ -> res


checkNativeAssign :: Stmt -> AExprAnalyzer -> Analyzer' Stmt
checkNativeAssign s@(NativeAssignDeclaration o p n t) analyzer = do
  addVar o n (Just p) t ""
  return s


checkAssign :: Stmt -> AExprAnalyzer -> Analyzer' Stmt
checkAssign (Assign o (Var _ name _ _ Nothing) ret aExpr) analyzer = do
  setType ret
  (type', inject, res) <- markNativePtr <$> analyzer aExpr
  firstSig <- listToMaybe <$> find' [name]
  nType <-
    case firstSig of
      Just e@(SVar _ n _ t s) ->
          makeError o $ "Global variables cannot be redefined and reallocated. \n " ++ show e
      Nothing ->
        add type'
  let mergedNameWithScope = concat . scaleNameWithScope $ "g" : [name]
  return $ Assign o (TypedVar (VName mergedNameWithScope) VAuto Nothing Nothing) nType res
  where
    add type' = addVar o name Nothing (unwrapAllMethod type') "g" >> check o type' ret type' type'
--    check wantedDecl wanted actual res
--      | wantedDecl == actual && (wanted == actual || wanted == VAuto) = return res
--      | otherwise =
--          makeError o $
--           "Types don't match. You tried2 to assign " ++
--           show actual ++ " when should be " ++ show wanted ++ ".\n" ++ show actual ++ " =/= " ++ show wanted


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
checkFor (ForFn o (Var vo n _ _ _) range body) fnAnalyzer aAnalyzer = do
  (t, _, range') <- aAnalyzer range
  addVar vo n Nothing (itemType t) ""
  addScope "for"
  body' <- concat <$> mapM fnAnalyzer body
  removeScope
  return [ForFn o (TypedVar (VName n) (itemType t) Nothing Nothing) range' body']
  where
    itemType (VClass (VName "ArrayList") [VGenPair "T" t]) = t
    itemType (VClass (VName "ArrayList") [t]) = t
    itemType t = t

-- | CLASS
checkClass :: Stmt -> ClassStmtAnalyzer -> Analyzer' Stmt
checkClass c@(ClassExpr o name gen body) analyzer = do
  setClassName name
  addScope "this"
  mapM_ (addField . SGen VAuto) gen
  body' <- mapM (analyzer name) body
  cScope <- removeScope
  addClass o name Nothing gen cScope
  return $ ClassExpr o name gen body'

-- TODO
checkNativeClass :: Stmt -> ClassStmtAnalyzer -> Analyzer' Stmt
checkNativeClass c@(NativeClass o p name cast body) analyzer = do
  setClassName name
  addScope "this"
  mapM_ (addField . SGen VAuto) cast
  body' <- mapM (analyzer name) body
  cScope <- removeScope
  addClass o name (Just p) cast cScope
  return $ NativeClass o p name cast body'

checkClassAssign :: ClassStmt -> AExprAnalyzer -> Analyzer' ClassStmt
checkClassAssign aa@(ClassAssign o (Var oV name [] Nothing Nothing) ret aExpr) analyzer = do
  setType ret
  (type', inject, res) <- markNativePtr <$> analyzer aExpr
  gen <- getClassGens
  firstSig <- listToMaybe <$> find' ["this", name]
  nType <- unwrapAllMethod . flip markGen gen <$>
    case firstSig of
      Just e@SVar {} ->
        makeError o $ "Global variables cannot be redefined and reallocated. \n " ++ show e
      Nothing ->
        check ret type'
--  trace ("ClassVar " ++ show name ++ show )
  addVar o name Nothing nType "this"
  
  let name' = concat . scaleNameWithScope $ "this" : [name]
  let newLeft = ScopeMark oV "this" (TypedVar (VName name') VAuto Nothing Nothing)
  return $ ClassAssign o newLeft nType res
  where
    check a b
      | b == VBlank = return a
      | a == b || a == VAuto = return b
      | otherwise =
        makeError o $
          "Types don't match. You tried to assign " ++ show b  ++ " when should be " ++ show a ++ ".\n" ++ show a ++ " =/= " ++ show b

forceNoScopeMarker _ ("":_) = return ()
forceNoScopeMarker o _ = makeError o "Global and class assign can't hava a scope marker."

-- | OTHER EXPR
checkOtherExpr :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkOtherExpr (OtherFn o aExpr) analyzer = do
  retTmp <- gets rType
  setType VAuto
  res <- trd <$> analyzer aExpr
  setType retTmp
  return [OtherFn o res]

checkBreak :: FunctionStmt -> Analyzer' [FunctionStmt]
checkBreak t@(Break o) = do
  cond <- isInsideLoop
  if cond then return [t] else makeError o "Break stmt can be used only inside a loop body."