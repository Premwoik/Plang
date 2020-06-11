{-# LANGUAGE FlexibleContexts #-}
module Compiler.Analyzer.AExpr.Var where

import           Compiler.Analyzer.Type
import           Control.Monad.State      (get, gets, put, modify)
import Control.Monad(filterM)
import Control.Monad.Writer(tell)
import           AST
import Compiler.Analyzer.Browser
import Data.Maybe(fromMaybe, listToMaybe, isNothing, fromJust)
import Debug.Trace
import Data.List(find)
import Compiler.Analyzer.Error
import Compiler.Analyzer.UniversalCheckers(checkTypesMatchGens)

markClassAsPointer :: [VarType] -> [VarType]
markClassAsPointer = map mapper
  where
    mapper (VGenPair n c@VClass {}) = VGenPair n (VPointer c SharedPtr)
    mapper c@VClass {} = VPointer c SharedPtr
    mapper e = e

markVarClassAsPointer :: [AExpr] -> [AExpr]
markVarClassAsPointer = map mapper
  where
    mapper (TypedVar n c@VClass {} a m) = TypedVar n (VPointer c SharedPtr) a m
    mapper x = x

markNativePtr :: [FunArg] -> [AExpr] -> [AExpr]
markNativePtr = zipWith extractPtr
--  don't mark when given arg is just native ptr
  where
    extractPtr (FunArg (VPointer _ NativePtr) _) v@(TypedVar _ (VPointer _ NativePtr) _ _) = v
--    TODO be carefull with this
    extractPtr (FunArg VClass {} _) (TypedVar n t@VPointer {} c d) = TypedVar n (VCopy t) c d
    extractPtr (FunArg (VPointer _ NativePtr) _) v@(TypedVar _ VPointer {} _ _) = NativePtrInput v
    extractPtr (FunArg (VPointer _ NativePtr) _) v@(TypedVar _ (VClass (VName "ArrayList") _) _ _) = NativePtrInput v
    extractPtr (FunArg (VPointer _ NativePtr) _) (TypedVar a t b c)  = TypedVar a (VRef (VCopy t)) b c
    extractPtr a v = v
 
extractPtr (Just (VPointer t _)) = Just t
extractPtr t = t

unwrapGen (VGenPair _ t) = t
unwrapGen x = x

fixPtrGenInArgs :: [VarType] -> AExpr -> AExpr
fixPtrGenInArgs gens (TypedVar n t a m) = TypedVar n newType a m
  where
    newType = case find (\t' -> unwrapGen t' == t) gens of
      (Just t') -> unwrapGen t'
      Nothing -> t
fixPtrGenInArgs _ t = t

checkArgs :: [VarType] -> Maybe [AExpr] -> AExprAnalyzer -> Analyzer' (Maybe [AExpr])
checkArgs gen args analyzer =
  case args of
    (Just a) -> Just . map (fixPtrGenInArgs gen .(\(_, _, x) -> x)) <$> mapM analyzer a
    Nothing -> return Nothing

-- | filterARgsMatch args gen field
filterArgsMatch :: Maybe [AExpr] -> [VarType] -> AExprAnalyzer -> ScopeField -> Analyzer' Bool
filterArgsMatch Nothing _ _ SVar {} = return True
filterArgsMatch Nothing [] _ SFunction {} = return True
filterArgsMatch (Just a) gen analyzer v@SVar {} =  maybeArgsMatchVar a gen analyzer v
filterArgsMatch (Just args) gen analyzer f = maybeArgsMatch (Just args) gen analyzer f
filterArgsMatch a g _ f = error (show a ++ " | " ++  show g ++ " | " ++ show f)
-- this function must have access to monad to check if lambdas is ok with given params

maybeArgsMatchVar :: [AExpr] -> [VarType] -> AExprAnalyzer -> ScopeField -> Analyzer' Bool
maybeArgsMatchVar args gen analyzer (SVar o n p (VFn t) _) =
  let
    funArgs = map (`FunArg` "") $ init t
  in maybeArgsMatch (Just args) gen analyzer (SFunction o n p (last t) funArgs)
maybeArgsMatchVar _ _ _ _= return False
 
wrapAllocationMethod :: VarType -> Analyzer' VarType
wrapAllocationMethod v = do
  ret <- gets rType
  return $ case (v, ret) of
    (VPointer t _, VCopy {}) -> VCopy v
    (VPointer {}, _ ) -> v
    (VClass {}, VPointer {}) -> VPointer (VCopy v) SharedPtr
    (_, VPointer {}) -> VPointer v SharedPtr
    (VRef {}, VRef {}) -> v 
    (_, VRef {}) -> VRef v 
    _ -> v


checkVarFirst :: AExpr -> Maybe [AExpr] -> RetBuilderT -> Maybe ScopeField -> String -> Analyzer' AExprRes
checkVarFirst var@(Var offset name gen _ more) Nothing retBuilder obj scopeName=
  case obj of
    Just tvar@(SVar _ n p t s) -> do
     t' <- if isNothing more then wrapAllocationMethod t else return t
     retBuilder t' (TypedVar (defaultPath p (scaleNameWithScope' [s, n])) t' Nothing) more

    Just(SFunction _ n p t a)-> do
      scope <- getFileName scopeName
      let newType = VFn $ map (\(FunArg t _) -> t) a ++ [t]
      retBuilder newType (TypedVar (defaultPath p (newName scope)) newType Nothing) more
      where
--        TODO merge this with scaleNameWithScope
        newName scope = case scopeName of
          "" -> n
          "g" -> scaleNameWithScope' ["::", n]
          "global" -> scaleNameWithScope' ["::", n] 
          "args" -> scaleNameWithScope' ["args", n]
          "this" -> scaleNameWithScope' ["this", n]
          _ -> scaleNameWithScope' [scope, "::", n]
    p -> do
      storage <- gets scopes
      makeError offset $ VariableMissing name

checkVarFirst var@(Var offset name gen _ more) args' retBuilder obj scopeName= 
  case obj of

    Just (SVar i n p (VFn t) s)  -> do
      let funArgs = map (`FunArg` "") $ init t
      checkVarFirst var args' retBuilder (Just (SFunction i n p (last t) funArgs)) s 
      
    Just cl@(SClass _ n p g sc) -> do
      if length g == length gen then return () else makeError offset $ GenericMissing (show g)
      let gen' = zipWith VGenPair g gen
      checkTypesMatchGens offset sc gen'
      constructor <- filterConstructor args' gen' cl
      let (SFunction o n _ _ cArgs) = constructor
      let args'' = args' >>= (Just . markNativePtr cArgs)
      newType <- wrapAllocationMethod $ VClass (VName n) (markClassAsPointer gen')
      retBuilder newType (TypedVar (defaultPath p n) newType args'') more
    Just(SFunction _ n p t a)-> do
--      t' <- fixNativeClassT t
      let args'' = args' >>= (Just . markNativePtr a)
      scope <- getFileName scopeName
      retBuilder t (TypedVar (defaultPath p (newName scope)) t args'') more
      where
--        TODO merge this with scaleNameWithScope
        newName scope = case scopeName of
          "" -> n
          "g" -> scaleNameWithScope' ["::", n]
          "global" -> scaleNameWithScope' ["::", n] 
          "args" -> scaleNameWithScope' ["args", n]
          "this" -> scaleNameWithScope' ["this", n]
          _ -> scaleNameWithScope' [scope, "::", n]
    p -> do
      storage <- gets scopes
      makeError offset $ VariableMissing name 

checkVarMore (Var offset name _ args more) (VClass cName gen) args' retBuilder method =
      case method of
        Just (SVar _ n p t _) -> 
          retBuilder t (TypedVar (defaultPath p (scaleNameWithScope' ["this", n])) t Nothing) more
        Just (SFunction _ n p t a) -> do
          let args'' = Just $ markNativePtr a (fromMaybe [] args')
          let nType = fixType gen t
          retBuilder nType (TypedVar (defaultPath p n) nType args'') more
        x -> makeError offset $ ClassVariableMissing (unwrapVarName cName) name


checkVar :: AExpr -> Maybe VarType -> String -> AExprAnalyzer -> Analyzer' AExprRes
checkVar v@(Var offset name gen args more) wantedType scopeName analyzer = 
  case extractPtr wantedType of
-- |  previous was a class
    Just c@(VClass cName gen) -> do
      args' <- checkArgs gen args analyzer
      candidate <- listToMaybe <$> (filterM (filterArgsMatch args' gen analyzer) =<< findInClass cName name)
      readyArgs <- updatePostProcessedArgs args'
      checkVarMore v c readyArgs retBuilder candidate

-- |  this is first, so it has to be variable global or local
    Nothing -> do
      args' <- checkArgs gen args analyzer
      r <- gets rType
      tt <- find' [scopeName, name]
      candidate <- listToMaybe . checkFunPtr r <$> (filterM (filterArgsMatch args' gen analyzer) =<< find' [scopeName, name])
      readyArgs <- updatePostProcessedArgs args'
      checkVarFirst v readyArgs retBuilder candidate scopeName

-- |  error previous was not a class
    (Just x) -> makeError offset  $ NotAClass name --("NotAClass: " ++ show x ++ " | " ++ show v)
  where
    retBuilder :: RetBuilderT
    retBuilder type' var more =
          makeOutput type' var <$> handleMore more (Just type')
    handleMore (Just m) x = Just <$> checkVar m x "" analyzer
    handleMore Nothing _ = return Nothing
    makeOutput _ wrapper (Just (v,i, m)) = (v, i, wrapper (Just m))
    makeOutput t wrapper Nothing = (t, [], wrapper Nothing)
    
--
type RetBuilderT = VarType -> (Maybe AExpr -> AExpr) -> Maybe AExpr -> Analyzer' AExprRes

updatePostProcessedArgs :: Maybe [AExpr] -> Analyzer' (Maybe [AExpr])
updatePostProcessedArgs (Just args) = Just . reverse <$> mapM updateArg (reverse args)
  where
    updateArg LambdaFn {} = takePostAExpr
    updateArg x = return x
updatePostProcessedArgs Nothing = return Nothing

defaultPath p n = case p of
  Just "" -> VName n
  Just p -> VNameNative n p
  Nothing -> VName n
  
 
checkFunPtr :: VarType -> [ScopeField] -> [ScopeField] 
checkFunPtr t@(VFn a) = isOnlyOne . filter matchArgs 
  where
    matchArgs (SFunction _ _ _ t args) = last a == t && length a - 1 == length args && compareArgs args
    matchArgs _ = False
    compareArgs = all (\(t, FunArg t' _) -> t == t'). zip (init a) 
    isOnlyOne l 
      | length l < 2 = l
      | otherwise = error "Jebać pis"
checkFunPtr _ = id
