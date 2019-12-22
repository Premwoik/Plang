module Compiler.Analyzer.AExpr where

import           Compiler.Analyzer.Pre
import           Compiler.Analyzer.Type
import           Control.Exception
import           Control.Monad.State      (get, gets, put, modify)
import Control.Monad.Writer(tell)
import           AST
import Compiler.Analyzer.Browser

checkVar :: AExpr -> Maybe VarType -> Analyzer' AExprRes
checkVar var wantedType = do
  s <- get
  let global' = global s
  let local' = local s
  case (var, wantedType) of

--  previous was class
    (Var name args more, Just(VClass cName)) ->
      case findMethod cName name global' of
        [Method n t _ _] -> makeOutput t (TypedVar n t args) <$> handleMore more (Just t)
        _ -> throw UnknownMethodName
        
--   this is first, so it has to be variable global or local
    (Var name args more, Nothing) ->
      case find name local' global' of
        [Assign n t _] -> makeOutput t (TypedVar name t args) <$> handleMore more (Just t)
        [ClassExpr n _ _] -> makeOutput (VClass n) (TypedVar name (VClass n) args) <$> handleMore more (Just (VClass n))
        p -> throw $ VariableNotExist (show p ++ show name ++ "  |  " ++ show local')
        

--  error previous was not a class
    (Var {}, Just _) -> throw $ NotAClass ""
  where
    handleMore (Just m) x = Just <$> checkVar m x
    handleMore Nothing _ = return Nothing
    makeOutput _ wrapper (Just (v,i, m)) = (v, i, wrapper (Just m))
    makeOutput t wrapper Nothing = (t, [], wrapper Nothing)

checkListVar a = return (VAuto,[], a)

checkRange a = return (VAuto,[], a)

checkFn a = return (VAuto,[], a)

checkFnBlock a = return (VAuto,[], a)

checkNegBlock a = return (VAuto,[], a)

checkABinary a = return (VAuto,[], a)

checkIfStatement :: AExpr -> FnStmtAnalyzer-> Analyzer' AExprRes
checkIfStatement (If ifs) analyzer = do
  (types, newIfs) <- unzip <$> mapM makeIf ifs
  let readyRetType = retType types
  let newCache = [AssignFn "fuckT12" readyRetType Nop, IfFn newIfs]
  return (readyRetType, newCache, Var "fuckT12" Nothing Nothing)
  where
    makeIf (cond, body) = do
      (aE, rest) <- unpackLastExpr . reverse . concat <$> mapM analyzer body
      let ret = reverse $ AssignFn "fuckT12" VBlank aE : rest
      return (aExprToType aE, (cond, ret))
    unpackLastExpr (OtherFn aE: rest) = (aE, rest)
    unpackLastExpr (a:_) = throw $ UnsupportedTypeException (show a)
    retType (t:s) = if all (==t) s then t else error "If statement return' type is not the same in every block"
    retType _ = error "If statement return' type is not the same in every block"


aExprToType :: AExpr -> VarType
aExprToType a = case a of
  IntConst {} -> VInt
  FloatConst {} -> VFloat
  StringVal {} -> VString
  TypedVar _ t _ _ -> t
  _ -> VAuto