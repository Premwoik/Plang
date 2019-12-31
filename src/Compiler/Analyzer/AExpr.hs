module Compiler.Analyzer.AExpr where

import           Compiler.Analyzer.Type
import           Control.Exception
import           Control.Monad.State      (get, gets, put, modify)
import Control.Monad.Writer(tell)
import           AST
import Compiler.Analyzer.Browser
import Data.Maybe(fromMaybe)
import Debug.Trace

checkVar :: AExpr -> Maybe VarType -> AExprAnalyzer -> Analyzer' AExprRes
checkVar var wantedType analyzer = do
  s <- get
  let global' = global s
  let local' = local s
  case (var, wantedType) of

--   TODO merge bottom function with upper (maybe)?
    (Var name _ args more, Just(VClass cName [x])) ->
      case findMethod cName name global' of
        [Method _ n t _ _] -> makeOutput x (TypedVar n x args) <$> handleMore more (Just x)
        MethodDeclaration _ n t _:_ -> makeOutput x (TypedVar n x args) <$> handleMore more (Just x)
        x -> error (show x ++ " | " ++ name) --throw UnknownMethodName
 

--  previous was class
    (Var name _ args more, Just(VClass cName _ )) ->
      case findMethod cName name global' of
        [Method _ n t _ _] -> makeOutput t (TypedVar n t args) <$> handleMore more (Just t)
        MethodDeclaration _ n t _:_ -> makeOutput t (TypedVar n t args) <$> handleMore more (Just t)
        x -> error (show x ++ " | " ++ name) --throw UnknownMethodName
       
--   this is first, so it has to be variable global or local
    (Var name gen args more, Nothing) -> do
      args' <- case args of
        (Just a) -> Just . map (\(_, _, x) -> x) <$> mapM analyzer a
        Nothing -> return Nothing
      case find name local' global' of
        Assign _ n t _ : _ -> makeOutput t (TypedVar name t args') <$> handleMore more (Just t)
        [ClassExpr _ n _ _] -> makeOutput (VClass n gen) (TypedVar name (VClass n gen) args') <$> handleMore more (Just (VClass n gen))
        [NativeClass _ p n _ _] -> makeOutput (VClass n gen) (TypedVar p (VClass n gen) args') <$> handleMore more (Just (VClass n gen))
        [Function _ n t _ _] -> makeOutput t (TypedVar name t args') <$> handleMore more (Just t)
        [NativeFunction _ p n t _] -> makeOutput t (TypedVar (defaultPath p n) t (if t == VAuto then Nothing else args')) <$> handleMore more (Just t)
        [NativeAssignDeclaration _ p n t] -> makeOutput (classToRef t) (TypedVar (defaultPath p n) (classToRef t) Nothing) <$> handleMore more (Just t)
        p -> throw $ VariableNotExist (show var ++ " | " ++ show p ++ "  |  " ++ show local')
        

--  error previous was not a class
    (Var {}, Just x) -> throw $ NotAClass $ show x
  where
    handleMore (Just m) x = Just <$> checkVar m x analyzer
    handleMore Nothing _ = return Nothing
    makeOutput _ wrapper (Just (v,i, m)) = (v, i, wrapper (Just m))
    makeOutput t wrapper Nothing = (t, [], wrapper Nothing)
    defaultPath "" n = n
    defaultPath p _ = p
    classToRef (VClass n _) = VRef n
    classToRef t = t

checkListVar a = return (VAuto,[], a)

checkRange a = return (VAuto,[], a)

checkFn a = return (VAuto,[], a)

checkFnBlock a = return (VAuto,[], a)

checkNegBlock a = return (VAuto,[], a)

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
checkIfStatement (If ifs) analyzer bAnalyzer = do
  (types, newIfs) <- unzip <$> mapM makeIf ifs
  let readyRetType = retType types
  let newCache = [AssignFn (-1) "fuckT12" readyRetType Nop, IfFn (-1) newIfs]
  return (readyRetType, newCache, Var "fuckT12" [] Nothing Nothing)
  where
    makeIf (cond, body) = do
      cond' <- bAnalyzer cond
      (aE, rest) <- unpackLastExpr . reverse . concat <$> mapM analyzer body
      let ret = reverse $ AssignFn (-1) "fuckT12" VBlank aE : rest
      return (aExprToType aE, (cond', ret))
    unpackLastExpr (OtherFn _ aE: rest) = (aE, rest)
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