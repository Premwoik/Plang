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
  , checkStringConst
  , checkFloatConst
  , checkIntConst
  , checkABool
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
  makeError o "Empty list must have providen a type"
checkListVar a@(ListVar o elems wantedType) analyzer = do
  let len = show $ length elems
  (types', injs, elems') <-
    classToPointer . foldr (\(t', inj, res) (ts, injs, ress) -> (t':ts, inj: injs, res:ress)) ([], [], []) 
    <$> mapM analyzer elems
  if checkType types' then return () else makeError o ("Not all elems are the same type in list: " ++ show elems ++ " wantedType: " ++ show wantedType)
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
  return (t', inc, ABracket o aExpr')

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
  let newCache = [AssignFn (-1) (toVar varName) readyRetType Nop, IfFn (-1) newIfs]
  return (readyRetType, newCache, Var o varName [] Nothing Nothing)
  where
    makeIf varName (cond, body) = do
      cond' <- bAnalyzer cond
      (aE, rest) <- unpackLastExpr . reverse . concat <$> mapM analyzer body
      let ret = reverse $ AssignFn (-1) (toVar varName) VBlank aE : rest
      return (aExprToType aE, (cond', ret))
    unpackLastExpr (OtherFn _ aE: rest) = (aE, rest)
--    unpackLastExpr (a:_) = throwError $ UnsupportedTypeException (show a)
    retType (t:s) = if all (==t) s then t else error "If statement return' type is not the same in every block"
    retType _ = error "If statement return' type is not the same in every block"
    toVar n = Var (-1) n [] Nothing Nothing

aExprToType :: AExpr -> VarType
aExprToType a = case a of
  IntConst {} -> VInt
  FloatConst {} -> VFloat
  StringVal {} -> VString
  ABool {} -> VBool
  TypedVar _ t _ _ -> t
  _ -> VAuto
  


checkIntConst :: AExpr -> Analyzer' AExprRes
checkIntConst e@(IntConst o i) = 
  return (VInt, [], e)

checkFloatConst :: AExpr -> Analyzer' AExprRes
checkFloatConst e@(FloatConst o f) =
  return (VFloat, [], e)

checkStringConst :: AExpr -> Analyzer' AExprRes
checkStringConst e@(StringVal o s) =
  return (VString, [], e)

checkABool :: AExpr -> BExprAnalyzer -> Analyzer' AExprRes
checkABool (ABool bExpr) analyzer = do
  bExpr' <- analyzer bExpr
  return (VBool, [], ABool bExpr')