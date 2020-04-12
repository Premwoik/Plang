module Compiler.Analyzer.AExpr
  ( checkVar
  , checkIfStatement
  , checkABinary
  , checkNegBlock
  , checkFnBlock
  , checkFn
  , checkRange
  , checkListVar
  , checkBracket
  , checkScopeMark) where

import           Compiler.Analyzer.Type
import           Control.Monad.State      (get, gets, put, modify)
import Control.Monad.Writer(tell)
import           AST
import Compiler.Analyzer.Browser
import Data.Maybe(fromMaybe)
import Debug.Trace
import Control.Monad.Except(throwError)
import Data.List(find)
import Compiler.Analyzer.AExpr.Var --(markVarClassAsPointer, markClassAsPointer)




--extractPtr (Just (VPointer t _)) = Just t
--extractPtr t = t
--
--unwrapGen (VGenPair _ t) = t
--unwrapGen x = x
--
--fixPtrGenInArgs :: [VarType] -> AExpr -> AExpr
--fixPtrGenInArgs gens (TypedVar n t a m) = TypedVar n newType a m
--  where
--    newType = case find (\t' -> unwrapGen t' == t) gens of
--      (Just t') -> unwrapGen t'
--      Nothing -> t
--fixPtrGenInArgs _ t = t  
--
--
--checkVar :: AExpr -> Maybe VarType -> String -> AExprAnalyzer -> Analyzer' AExprRes
--checkVar var wantedType scopeName analyzer = 
----  trace ("ACTUAL POSITION: " ++ show var ++ " | " ++ show wantedType) $ return ()
--  case (var, extractPtr wantedType) of
---- |  previous was a class
--    (Var offset name _ args more, Just(VClass cName gen _)) -> do
----      trace ("GENS:::: " ++ show gen) $ return ()
--      args' <- case args of
----      TODO mark arg class type as a pointer if function accept pointer | modify checkArgs function to do that?
--        (Just a) -> Just . map (fixPtrGenInArgs gen .(\(_, _, x) -> x)) <$> mapM analyzer a
--        Nothing -> return Nothing
--
----      trace ("ARGS:::: " ++ show args') $ return ()
--      method <- findInClass cName name
--      case method of
--        [SVar _ n p t _] -> makeOutput t (TypedVar (defaultPath p n) t Nothing) <$> handleMore more (Just t)
--        f@(SFunction _ n p t _ :_) -> do
----          trace (show gen ++ "   || ARGS ||  " ++ show args') $ return ()
--          checkArgs args' gen f
--          let nType = fixType gen t
--          makeOutput nType (TypedVar (defaultPath p n) nType args') <$> handleMore more (Just nType)
--        x -> throwError $ UnknownMethodName (show x ++ " | " ++ name ++ " | " ++ cName)
--
---- |  this is first, so it has to be variable global or local
--    (Var offset name gen args more, Nothing) -> do
--      args' <- case args of
--        (Just a) -> Just . map (\(_, _, x) -> x) <$> mapM analyzer a
--        Nothing -> return Nothing
--      obj <- find' [scopeName, name]
--      case obj of
--        f@(SVar _ n p t s: _) -> makeOutput t (TypedVar (defaultPath p (scaleNameWithScope' [s, n])) t args')
--          <$> handleMore more (Just t)
--
--        f@[SClass _ n p g b] -> do
----          TODO make this generics to zip recurrently not only on actual layer ready
--          if length g == length gen then return () else throwError $ AException offset ("Generic is missing! - " ++ show g)
--          let gen' = zipWith VGenPair g gen
--          let newType = VClass n (markClassAsPointer gen') False
--          checkArgs args' gen' f
--          makeOutput newType (TypedVar (defaultPath p n) newType args') <$> handleMore more (Just newType)
--
--        f@[SFunction _ n p t _] -> do
--          checkArgs args' [] f
--          makeOutput t (TypedVar (defaultPath p n) t args') <$> handleMore more (Just t)
--
--        p -> do
--          storage <- gets scopes
--          throwError $ VariableNotExist (show var ++ " | " ++ show p ++ " | " ++ show storage)
--
----  error previous was not a class
--    (v@Var {}, Just x) -> throwError $  NotAClass  (show x ++ " | " ++ show v)
--  where
--    handleMore (Just m) x = Just <$> checkVar m x "" analyzer
--    handleMore Nothing _ = return Nothing
--    makeOutput _ wrapper (Just (v,i, m)) = (v, i, wrapper (Just m))
--    makeOutput t wrapper Nothing = (t, [], wrapper Nothing)
--    defaultPath p n = case p of
--      Just "" -> VName n
--      Just p -> VNameNative n p
--      Nothing -> VName n
--    checkArgs args gen f =
--      if maybeArgsMatch args gen f then return ()
--      else error $ makeError var f gen
--    makeError var l gen = "WRONG ARGUMENTS PASSED TO FUNCTION \n Given var: ("
--      ++ show var ++ ")\n Generics: " ++ show gen ++ "\n doesn't match with any of following expresions: \n"
--      ++ show l
--


checkScopeMark :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkScopeMark (ScopeMark o scopeName aExpr) analyzer = do
  addOffset o 
  trace ("SCOPE_MARK " ++ scopeName ++ " | " ++ show aExpr) $ return ()
  res <- (\(a, b, res) -> (a, b, ScopeMark o scopeName res)) <$> checkVar aExpr Nothing scopeName analyzer
  removeOffset
  return res




checkListVar :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkListVar (ListVar _ [] (Just t)) _ = 
  return (VClass "ArrayList" [t] False, [], TypedVar (VName "ArrayList") (VClass "ArrayList" [t] False) (Just []) Nothing)
checkListVar (ListVar o [] Nothing) _ = 
  throwError $ AException o "Empty list must have providen a type"
checkListVar a@(ListVar o elems wantedType) analyzer = do
  let len = show $ length elems
  (types', injs, elems') <-
    classToPointer . foldr (\(t', inj, res) (ts, injs, ress) -> (t':ts, inj: injs, res:ress)) ([], [], []) 
    <$> mapM analyzer elems
  if checkType types' then return () else throwError $ AException o ("Not all elems are the same type in list: " ++ show elems ++ " wantedType: " ++ show wantedType)
  let itemType = head types'
  let args = Just [TypedListVar elems' itemType, TypedVar (VName len) VInt Nothing Nothing, TypedVar (VName len) VInt Nothing Nothing]
  let t = VClass "ArrayList" [VGenPair "T" itemType] False
  return (t, concat injs, TypedVar (VName "ArrayList") t args Nothing)
  where
--    typesMatchOrError types = 
    checkType (t:ts) = all (\x -> t == x) ts && t == fromMaybe t wantedType
    classToPointer (t, i, e) = (markClassAsPointer t, i, markVarClassAsPointer e)

checkRange a = return (VAuto,[], a)

checkFn a = return (VAuto,[], a)

checkFnBlock a = return (VAuto,[], a)

checkNegBlock a = return (VAuto,[], a)

checkBracket :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkBracket (ABracket o aExpr) analyzer = do
  (t', inc, aExpr') <- analyzer aExpr
  return (t', inc, ABracket o aExpr)

checkABinary :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkABinary t@(ABinary op a b) aAnalyzer = do
  (ta, _, a') <- aAnalyzer a
  (tb, _,  b') <- aAnalyzer b
  let nType = checkType ta tb
  return (nType,[], ABinary op a' b')
  where
    checkType t1 t2
      | t1 == t2 = t1
      | otherwise = error $ "checkABinary t1 =/ t2  |  " ++ show t1 ++ "   " ++ show t2

checkIfStatement :: AExpr -> FnStmtAnalyzer -> BExprAnalyzer -> Analyzer' AExprRes
checkIfStatement (If o ifs) analyzer bAnalyzer = do
  varName <- takeVarName
  (types, newIfs) <- unzip <$> mapM (makeIf varName) ifs
  let readyRetType = retType types
  let newCache = [AssignFn (-1) [varName] readyRetType Nop, IfFn (-1) newIfs]
  return (readyRetType, newCache, Var o varName [] Nothing Nothing)
  where
    makeIf varName (cond, body) = do
      cond' <- bAnalyzer cond
      (aE, rest) <- unpackLastExpr . reverse . concat <$> mapM analyzer body
      let ret = reverse $ AssignFn (-1) [varName] VBlank aE : rest
      return (aExprToType aE, (cond', ret))
    unpackLastExpr (OtherFn _ aE: rest) = (aE, rest)
--    unpackLastExpr (a:_) = throwError $ UnsupportedTypeException (show a)
    retType (t:s) = if all (==t) s then t else error "If statement return' type is not the same in every block"
    retType _ = error "If statement return' type is not the same in every block"


aExprToType :: AExpr -> VarType
aExprToType a = case a of
  IntConst {} -> VInt
  FloatConst {} -> VFloat
  StringVal {} -> VString
  TypedVar _ t _ _ -> t
  _ -> VAuto