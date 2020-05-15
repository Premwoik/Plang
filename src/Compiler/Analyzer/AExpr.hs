module Compiler.Analyzer.AExpr
  ( checkVar
  , checkIfStatement
  , checkABinary
  , checkNegBlock
  , checkLambdaFn
  , checkRange
  , checkListVar
  , checkBracket
  , checkStringConst
  , checkFloatConst
  , checkIntConst
  , checkABool
  , checkOptional
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
import Compiler.Analyzer.UniversalCheckers

checkOptional :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkOptional (Optional o aExpr _) analyzer = do
-- TODO add checking if class has implemented bool interface
  (t,_, a) <- analyzer aExpr
  case t of
    VPointer {} -> 
      return (VBool, [], Optional o a NullOT)
    VClass {} -> 
      return (VBool, [], Optional o a BoolOT)
    _ -> makeError o "O chuj"

checkScopeMark :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkScopeMark (ScopeMark o scopeName aExpr) analyzer = do
  addOffset o 
--  trace ("SCOPE_MARK " ++ scopeName ++ " | " ++ show aExpr) $ return ()
  res <- (\(a, b, res) -> (a, b, ScopeMark o scopeName res)) <$> checkVar aExpr Nothing scopeName analyzer
  removeOffset
  return res


checkListVar :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkListVar (ListVar _ [] (Just t)) _ = 
  return (VClass (VName "ArrayList") [t], [], TypedVar (VName "ArrayList") (VClass (VName "ArrayList") [t]) (Just []) Nothing)
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
  let t = VClass (VName "ArrayList") [VGenPair "T" itemType]
  return (t, concat injs, TypedVar (VName "ArrayList") t args Nothing)
  where
--    typesMatchOrError types = 
    checkType (t:ts) = all (\x -> t == x) ts && t == fromMaybe t wantedType
    classToPointer (t, i, e) = (markClassAsPointer t, i, markVarClassAsPointer e)

checkRange :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkRange (Range o s a b) analyzer  = do
  (ta, _, a') <- analyzer a
  (tb, _, b') <- analyzer b
  (ts, _, s') <- analyzer . fromMaybe (IntConst o 1) $ s
  allInt ta tb ts
  return (VInt,[], Range o (Just s') a' b')
  where
    allInt a b c 
      | a == b && a == c && a == VInt = return () 
      | otherwise = makeError o "In range all numbers have to be an Integers"
      
-- False is for normal assign and True is for invoking checking
--TODO return can be done only at the end of the last line???
checkLambdaFn :: Bool -> AExpr -> FnStmtAnalyzer ->  Analyzer' AExprRes
checkLambdaFn False a@(LambdaFn offset retType args stmts) analyzer = do
  ret <- gets rType
  allTypesKnown ret
  where
    allTypesKnown (VFn types) 
      | VAuto `notElem` types = do
        let args' = zipWith (\t (FunArg _ n) -> FunArg t n) types args
        let ret' = last types
        checkLambdaFn True (LambdaFn offset ret' args' stmts) analyzer
      | otherwise = makeError offset "All arguments must have defined strict type!"
    allTypesKnown _ = return (VFn [], [], a) --makeError offset "Lambda expression must have defined strict type!"
    
checkLambdaFn True a@(LambdaFn offset retType args stmts) analyzer = do
  args' <- checkFnArgs args 
  addArgsScope offset args
  addScope "lambda"
  stmts' <- concat <$> mapM analyzer stmts -- =<< checkReturn stmts)
  removeScope
  removeScope
  return (rType, [], LambdaFn offset retType args' stmts')
  where
    rType = VFn $ map(\(FunArg t _) -> t) args ++ [retType]
--    checkReturn stmts =
--      case last stmts of
--        ReturnFn {} -> return stmts
--        OtherFn o e -> return $ init stmts  ++ [ReturnFn o e]
----        TODO add check also for returns inside function body - not only last return
--        _ -> makeError offset "Anonymous last statement must be returnable."

checkNegBlock (Neg a) analyzer = do
  (t, _, a') <- analyzer a
  return (t,[], addNeg a')
  where
    addNeg (IntConst o x) = IntConst o (-x)
    addNeg (FloatConst o x) = FloatConst o (-x)
    addNeg x = Neg x

checkBracket :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkBracket (ABracket o aExpr) analyzer = do
  (t', inc, aExpr') <- analyzer aExpr
  return (t', inc, ABracket o aExpr')

checkABinary :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkABinary t@(ABinary op a b) aAnalyzer = do
  (ta, _, a') <- aAnalyzer a
  (tb, _,  b') <- aAnalyzer b
  nType <- compareGens 0 ta tb
  return (nType,[], TypedABinary nType op a' b')

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