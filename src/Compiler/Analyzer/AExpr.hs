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
--  previous was a class
    (Var name _ args more, Just(VClass cName gen)) -> do
      args' <- case args of
        (Just a) -> Just . map (\(_, _, x) -> x) <$> mapM analyzer a
        Nothing -> return Nothing
        
      case findMethod cName name global' of
        [Assign _ [n] t _] -> makeOutput t (TypedVar n t Nothing) <$> handleMore more (Just t)
        f@(Function _ n t _ _:_) -> do
         checkArgs args' gen f
         let nType = fixType gen t
         makeOutput nType (TypedVar n nType args') <$> handleMore more (Just nType)
        x -> error (show x ++ " | " ++ name) --throw UnknownMethodName

--   this is first, so it has to be variable global or local
    (Var name gen args more, Nothing) -> do
      args' <- case args of
        (Just a) -> Just . map (\(_, _, x) -> x) <$> mapM analyzer a
        Nothing -> return Nothing
      case find [name] local' global' of
        f@(Assign _ n t _ : _) -> trace ("ASSIGN" ++ show local') $
          makeOutput t (TypedVar name t args') <$> handleMore more (Just t)
        [NativeAssignDeclaration _ p n t] ->
          makeOutput (classToRef t) (TypedVar (defaultPath p n) (classToRef t) Nothing) <$> handleMore more (Just t)

        f@[ClassExpr _ n g b] -> do
          let gen' = makeGenPair g gen
          let newType = VClass n gen'
          checkArgs args' gen' f
          makeOutput newType (TypedVar name newType args') <$> handleMore more (Just newType)
        f@[NativeClass _ p n g b] -> do
          let gen' = makeGenPair g gen
          let newType = VClass n gen'
          checkArgs args' gen' f
          makeOutput newType (TypedVar p newType args') <$> handleMore more (Just newType)
        f@[Function _ n t _ _] -> do
          checkArgs args' [] f
          makeOutput t (TypedVar name t args') <$> handleMore more (Just t)
        
        f@[NativeFunction _ p n t _] -> do
          checkArgs args' [] f
          makeOutput t (TypedVar (defaultPath p n) t (if t == VAuto then Nothing else args')) <$> handleMore more (Just t)
        p ->
          throw $ VariableNotExist (show var ++ " | " ++ show p ++ "  |  " ++ show local')
--  error previous was not a class
    (v@Var {}, Just x) -> throw $ NotAClass $ (show x ++ " | " ++ show v)
  where
    handleMore (Just m) x = Just <$> checkVar m x analyzer
    handleMore Nothing _ = return Nothing
    makeOutput _ wrapper (Just (v,i, m)) = (v, i, wrapper (Just m))
    makeOutput t wrapper Nothing = (t, [], wrapper Nothing)
    defaultPath "" n = n
    defaultPath p _ = p
    classToRef (VClass n _) = VRef n
    classToRef t = t
    checkArgs args gen f =  
      if maybeArgsMatch args gen f then return () 
      else error $ makeError var f
    makeGenPair = zipWith VGenPair
    makeError var l = "WRONG ARGUMENTS PASSED TO FUNCTION \n Given var: ("
      ++ show var ++ ")\n doesn't match with any of following expresions: \n"
      ++ show l


checkListVar :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkListVar a@(ListVar elems) analyzer = do
  let len = show $ length elems
  let t = VClass "ArrayList" [VInt]
  (types', injs, elems') <-
    foldr (\(t', inj, res) (ts, injs, ress) -> (t':ts, inj: injs, res:ress)) ([], [], []) <$> mapM analyzer elems
  if checkType types' then return () else error ("Not all elems are the same type in list: " ++ show elems)
  let args = Just [a, TypedVar len VInt Nothing Nothing, TypedVar len VInt Nothing Nothing]
  return (t, [], TypedVar "ArrayList" t args Nothing)
  where
    checkType (t:ts) = all (\x -> t == x) ts

checkRange a = return (VAuto,[], a)

checkFn a = return (VAuto,[], a)

checkFnBlock a = return (VAuto,[], a)

checkNegBlock a = return (VAuto,[], a)

checkBracket :: AExpr -> AExprAnalyzer -> Analyzer' AExprRes
checkBracket (ABracket aExpr) analyzer = do
  (t', inc, aExpr') <- analyzer aExpr
  return (t', inc, ABracket aExpr)

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
  let newCache = [AssignFn (-1) ["fuckT12"] readyRetType Nop, IfFn (-1) newIfs]
  return (readyRetType, newCache, Var "fuckT12" [] Nothing Nothing)
  where
    makeIf (cond, body) = do
      cond' <- bAnalyzer cond
      (aE, rest) <- unpackLastExpr . reverse . concat <$> mapM analyzer body
      let ret = reverse $ AssignFn (-1) ["fuckT12"] VBlank aE : rest
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