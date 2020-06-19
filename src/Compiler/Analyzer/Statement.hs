{-# LANGUAGE FlexibleContexts #-}

module Compiler.Analyzer.Statement where

import           AST
import           Compiler.Analyzer.Browser
import           Compiler.Analyzer.Error
import           Compiler.Analyzer.Type
import           Compiler.Analyzer.Universal
import           Compiler.Analyzer.UniversalCheckers (check, checkFnArgs,
                                                      checkFunctionUniqueness,
                                                      compareGens)
import           Control.Monad                       (foldM)
import           Control.Monad.State                 (get, gets, modify, put)
import           Control.Monad.Writer                (tell)
import           Data.Maybe                          (fromJust, fromMaybe,
                                                      listToMaybe)
import qualified Data.Set                            as S
import           Debug.Trace

checkImport :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkImport t@Import {} = return t

checkLinkPath :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkLinkPath t@LinkPath {} = return t

checkFunction :: Stmt -> Analyzer' Stmt
checkFunction f@(Function o n g t a body) = do
  let nType = markGen t g
  let nArgs = markGenInArgs g a
  (_, _, rg, rt, ra, rb) <- checkFunction' (o, n, g, nType, nArgs, body)
  addFunction o n Nothing rt ra
  checkFunctionUniqueness o ["", n] rt ra
  return $ Function o n g rt ra rb

checkMethod :: ClassStmt -> Analyzer' ClassStmt
checkMethod m@(Method o n t a details body) = do
  gens <- getClassGens
  let nType = markGen t gens
  let nArgs = markGenInArgs gens a
  (_, _, _, rt, ra, rb) <- checkFunction' (o, n, [], nType, nArgs, body)
  className <- gets cName
  addFunction o n Nothing rt ra
  checkFunctionUniqueness o ["this", n] rt ra
  return $
    if className == n
      then Constructor o n ra rb
      else Method o n rt ra details rb

checkFunction' :: RawFunction -> Analyzer' RawFunction
checkFunction' (o, name, gen, type', args, body) = do
  args' <- fixArgsNativeClassType =<< checkFnArgs args
  setType =<< fixNativeClassType type'
  addArgsScope o args
  setFunName name
  addScope "fun"
  mapM_ (addField . SGen VAuto) gen
  checkedBody' <- concat <$> mapM (injectAnalyzer functionStmtAnalyzerGetter) body
  nType <- gets rType
  removeScope
  removeScope
  return (o, name, gen, nType, args', checkedBody')

checkReturn :: FunctionStmt -> Analyzer' [FunctionStmt]
checkReturn (ReturnFn o (Just aExpr)) = do
  gen <- getClassGens
  funcType <- flip markGen gen <$> gets rType
  (t, inject, res) <- injectAnalyzer aExprAnalyzerGetter aExpr
  nType <- compareGens o t funcType
  setType nType
  return $ inject ++ [ReturnFn o (Just res)]
checkReturn (ReturnFn o Nothing) = do
  gen <- getClassGens
  funcType <- flip markGen gen <$> gets rType
  nType <- compareGens o funcType VVoid
  setType nType
  return [ReturnFn o Nothing]

checkNative :: Stmt -> Analyzer' Stmt
checkNative t@(NativeFunction o path name gen ret args) = do
  ret' <- fixNativeClassType $ markGen ret gen
  args' <- checkFnArgs $ markGenInArgs gen args
  addFunction o name (Just path) ret' args'
  return $ NativeFunction o path name gen ret' args'

checkMethodDeclaration :: ClassStmt -> Analyzer' ClassStmt
checkMethodDeclaration ttt@(NativeMethod o n t args) = do
  gen <- getClassGens
  ret <- fixNativeClassType t
  let nType = markGen ret gen
  let nArgs = markGenInArgs gen args
  addFunction o n Nothing nType nArgs
  return $ NativeMethod o n nType nArgs

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
unwrapAllMethod (VRef (VCopy x))       = VRef x
unwrapAllMethod (VPointer (VCopy x) y) = VPointer x y
unwrapAllMethod (VPointer t NativePtr) = VPointer t SharedPtr
unwrapAllMethod x                      = x

markNativePtr (VPointer t NativePtr, i, res) = (VPointer t SharedPtr, i, NativePtrRes res)
markNativePtr x = x

checkAssignFn :: FunctionStmt -> Analyzer' [FunctionStmt]
checkAssignFn a@(AssignFn o var@(Var vo vname [] Nothing Nothing) ret aExpr) = do
  s <- get
  retTmp <- gets rType
  setType ret
  (type', inject, res) <- markNativePtr <$> injectAnalyzer aExprAnalyzerGetter aExpr
  setType retTmp
  firstSig <- listToMaybe <$> find' ["", vname]
  nType <-
    case firstSig of
      Just (SVar o' n _ t "") -> check o t ret type' VBlank
      Just {}                 -> add type'
      Nothing                 -> add type'
  return $ inject ++ [AssignFn o (TypedVar (VName vname) VAuto Nothing Nothing) (unwrapAllMethod nType) res]
  where
    add type' = addVar o vname Nothing (unwrapAllMethod type') "" >> check o type' ret type' type'
checkAssignFn a@(AssignFn o nameExpr ret aExpr) =
  case hasSet nameExpr aExpr of
    Just resExpr -> do
      (nType, nInject, nRes) <- injectAnalyzer aExprAnalyzerGetter resExpr
      return $ nInject ++ [OtherFn o nRes]
    Nothing -> do
      (type', inject, res) <- markNativePtr <$> injectAnalyzer aExprAnalyzerGetter aExpr
      (nType, nInject, nRes) <- injectAnalyzer aExprAnalyzerGetter nameExpr
      nType <- check o nType ret type' VBlank
      let assign = AssignFn o nRes nType res
      return $ nInject ++ inject ++ [assign]
  where
    hasSet :: AExpr -> AExpr -> Maybe AExpr
    hasSet t@(Var o "set" g (Just args) Nothing) rSide = return $ Var o "set" g (Just (rSide : args)) Nothing
    hasSet t@(Var _ _ _ _ Nothing) _ = Nothing
    hasSet (Var a b c d (Just more)) rSide = return $ Var a b c d $ hasSet more rSide
    hasSet (ScopeMark o n a) rSide = do
      res <- hasSet a rSide
      return $ ScopeMark o n res

checkNativeAssign :: Stmt -> Analyzer' Stmt
checkNativeAssign s@(NativeAssignDeclaration o p n t) = do
  addVar o n (Just p) t ""
  return s

checkAssign :: Stmt -> Analyzer' Stmt
checkAssign (Assign o (Var _ name _ _ Nothing) ret aExpr) = do
  setType ret
  (type', inject, res) <- markNativePtr <$> injectAnalyzer aExprAnalyzerGetter aExpr
  firstSig <- listToMaybe <$> find' [name]
  nType <-
    case firstSig of
      Just e@(SVar _ n _ t s) -> makeError o $ NotAllowedGlobalMod n
      Nothing                 -> add type'
  let mergedNameWithScope = concat . scaleNameWithScope $ "g" : [name]
  return $ Assign o (TypedVar (VName mergedNameWithScope) VAuto Nothing Nothing) nType res
  where
    add type' = addVar o name Nothing (unwrapAllMethod type') "g" >> check o type' ret type' type'

checkWhile :: FunctionStmt -> Analyzer' [FunctionStmt]
checkWhile t@(WhileFn o cond block) = do
  addScope "while"
  cond' <- injectAnalyzer bExprAnalyzerGetter cond
  block' <- concat <$> mapM (injectAnalyzer functionStmtAnalyzerGetter) block
  removeScope
  return . return $ WhileFn o cond' block'

checkIfFunction :: FunctionStmt -> Analyzer' [FunctionStmt]
checkIfFunction t@(IfFn o ifs) = do
  newIfs <- mapM makeIf ifs
  return [IfFn o newIfs]
  where
    makeIf (cond, body) = do
      addScope "if"
      body' <- concat <$> mapM (injectAnalyzer functionStmtAnalyzerGetter) body
      cond' <- injectAnalyzer bExprAnalyzerGetter cond
      removeScope
      return (cond', body')

checkFor :: FunctionStmt -> Analyzer' [FunctionStmt]
-- TODO
checkFor (ForFn o (Var vo n _ _ _) range body) = do
  (t, _, range') <- injectAnalyzer aExprAnalyzerGetter range
  addVar vo n Nothing (itemType t) ""
  addScope "for"
  body' <- concat <$> mapM (injectAnalyzer functionStmtAnalyzerGetter) body
  removeScope
  return [ForFn o (TypedVar (VName n) (itemType t) Nothing Nothing) range' body']
  where
    itemType (VClass (VName "ArrayList") [VGenPair "T" t]) = t
    itemType (VClass (VName "ArrayList") [t])              = t
    itemType t                                             = t

-- | CLASS
checkDecorator :: ClassStmt -> Analyzer' ClassStmt
checkDecorator dec@(ClassDecorator offset name classStmt) =
  case classStmt of
    ClassDecorator {} -> updateStmt =<< injectAnalyzer classStmtAnalyzerGetter classStmt
    _ -> injectAnalyzer classStmtAnalyzerGetter =<< updateStmt classStmt
  where
    updateStmt classStmt =
      case (classStmt, name) of
        (Method {}, PrivateDec) ->
          return $ classStmt {methodDetails = (methodDetails classStmt) {visibilityMD = "private"}}
        (Method {}, PublicDec) ->
          return $ classStmt {methodDetails = (methodDetails classStmt) {visibilityMD = "public"}}
        (Method {}, OverrideDec) -> return $ classStmt {methodDetails = (methodDetails classStmt) {isOverrideMD = True}}
        (ClassAssign {}, PublicDec) ->
          return $ classStmt {classAssignDetails = (classAssignDetails classStmt) {visibilityMD = "public"}}
        (ClassAssign {}, PrivateDec) ->
          return $ classStmt {classAssignDetails = (classAssignDetails classStmt) {visibilityMD = "private"}}
        (_, x) -> makeError offset $ CustomError ("Custom decorators are unsupported yet - unsupported " ++ show x)

checkClass :: Stmt -> Analyzer' Stmt
checkClass c@(ClassExpr o name gen parents body) = do
  setClassName name
  addScope "this"
  mapM_ (addField . SGen VAuto) gen
  body' <- mapM (injectAnalyzer classStmtAnalyzerGetter) body
  cScope <- removeScope
  let parents' = map (`markGen` gen) parents
  r <- checkParentsUniqueness o =<< addClass o name Nothing gen parents' cScope
  return $ ClassExpr o name gen parents' body'

checkParentsUniqueness :: Offset -> ScopeField -> Analyzer' (S.Set String)
checkParentsUniqueness offset (SClass o n p g parents (Scope _ fields)) = do
  parentClasses <- eachParentExist =<< mapM (\x -> find' ["", unwrapVarNameForce (unwrapClassName x)]) parents
  checked <- mapM (checkParentsUniqueness offset) parentClasses
  S.union namesSet <$> foldM checkUniqueness S.empty checked
  where
    eachParentExist res =
      if all (not . null) res
        then return (concat res)
        else makeError offset $ CustomError ""
    namesSet = S.fromList $ map getFieldName fields
    checkUniqueness acc current
      | length current == length (S.difference current acc) = return $ S.union acc current
      | otherwise =
        makeError offset $
        CustomError $
        "Members " ++ show (getNotUnique acc current) ++ " find in multiple parent classes of different type!"
    getNotUnique acc current = current S.\\ S.difference current acc

-- TODO
checkNativeClass :: Stmt -> Analyzer' Stmt
checkNativeClass c@(NativeClass o p name cast body) = do
  setClassName name
  addScope "this"
  mapM_ (addField . SGen VAuto) cast
  body' <- mapM (injectAnalyzer classStmtAnalyzerGetter) body
  cScope <- removeScope
  addClass o name (Just p) cast [] cScope
  return $ NativeClass o p name cast body'

checkClassAssign :: ClassStmt -> Analyzer' ClassStmt
checkClassAssign aa@(ClassAssign o (Var oV name [] Nothing Nothing) ret details aExpr) = do
  setType ret
  (type', inject, res) <- markNativePtr <$> injectAnalyzer aExprAnalyzerGetter aExpr
  gen <- getClassGens
  firstSig <- listToMaybe <$> find' ["this", name]
  nType <-
    unwrapAllMethod . flip markGen gen <$>
    case firstSig of
      Just (SVar _ n _ t s) -> makeError o $ NotAllowedGlobalMod n
      Nothing               -> check ret type'
  addVar o name Nothing nType "this"
  let name' = concat . scaleNameWithScope $ "this" : [name]
  let newLeft = ScopeMark oV "this" (TypedVar (VName name') VAuto Nothing Nothing)
  return $ ClassAssign o newLeft nType details res
  where
    check a b
      | b == VBlank = return a
      | a == b || a == VAuto = return b
      | otherwise = makeError o $ AssignTypesMismatch b a

forceNoScopeMarker _ ("":_) = return ()
forceNoScopeMarker o _      = makeError o NotAllowedScopeMarker

-- | OTHER EXPR
checkOtherExpr :: FunctionStmt -> Analyzer' [FunctionStmt]
checkOtherExpr (OtherFn o aExpr) = do
  retTmp <- gets rType
  setType VAuto
  res <- trd <$> injectAnalyzer aExprAnalyzerGetter aExpr
  setType retTmp
  return [OtherFn o res]

checkBreak :: FunctionStmt -> Analyzer' [FunctionStmt]
checkBreak t@(Break o) = do
  cond <- isInsideLoop
  if cond
    then return [t]
    else makeError o NotAllowedBrakeStmt
