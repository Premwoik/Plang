{-# LANGUAGE FlexibleContexts #-}
module Compiler.Analyzer.AExpr.Var where

import           Compiler.Analyzer.Type
import           Control.Monad.State      (get, gets, put, modify)
import Control.Monad(filterM)
import Control.Monad.Writer(tell)
import           AST
import Compiler.Analyzer.Browser
import Data.Maybe(fromMaybe, listToMaybe, isNothing)
import Debug.Trace
import Control.Monad.Except(throwError)
import Data.List(find)

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
  where
    extractPtr (FunArg (VPointer _ NativePtr) _) v = NativePtrInput v
    extractPtr _ v = v
 
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
maybeArgsMatchVar args gen analyzer (SVar o n p (VFn t) _ _) =
  let
    funArgs = map (`FunArg` "") $ init t
  in maybeArgsMatch (Just args) gen analyzer (SFunction o n p (last t) funArgs NoNeedCheck)
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
    Just tvar@(SVar _ n p t s _) -> do
     trace ("VarScope :: " ++ s ++ "  |  "  ++  (scaleNameWithScope' [s, n]) ++ " *** "++ show tvar) $ return ()
     t' <- if isNothing more then wrapAllocationMethod t else return t
     retBuilder t' (TypedVar (defaultPath p (scaleNameWithScope' [s, n])) t' Nothing) more

    Just(SFunction _ n p t a _)-> do
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
      makeError offset ("2Can't find given name! " ++ show var ++ " | " ++ show p) -- ++ " | " ++ show storage)
  
checkVarFirst var@(Var offset name gen _ more) args' retBuilder obj scopeName=
  case obj of
    
    Just (SVar i n p (VFn t) s h)  -> do
      let funArgs = map (`FunArg` "") $ init t
      checkVarFirst var args' retBuilder (Just (SFunction i n p (last t) funArgs h)) s 
      
    Just (SClass _ n p g sc postCheck) -> do
      if length g == length gen then return () else makeError offset ("Generic is missing! - " ++ show g)
      let gen' = zipWith VGenPair g gen
      checkTypesMatchGens offset sc gen'
      trace ("Class args: " ++ show args') $ return ()
      newType <- wrapAllocationMethod $ VClass n (markClassAsPointer gen') False
      retBuilder newType (TypedVar (defaultPath p n) newType args') more
    Just(SFunction _ n p t a _)-> do
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
      makeError offset ("1Can't find given name! " ++ show var ++ " | " ++ show p ++ " | " ++ show storage)

checkVarMore (Var offset name _ args more) (VClass cName gen _) args' retBuilder method =
      case method of
        Just (SVar _ n p t _ NoNeedCheck) -> 
          retBuilder t (TypedVar (defaultPath p n) t Nothing) more
        Just (SFunction _ n p t a NoNeedCheck) -> do
--          let args'' = Just $ markNativePtr a (fromMaybe [] args')
          let nType = fixType gen t
          trace ("Func: " ++ n ++ " retType: " ++ show t) $ return ()
          retBuilder nType (TypedVar (defaultPath p n) nType args') more
        x -> makeError offset ("Can't find that method or field in given class. " ++ show x ++ " | " ++ name ++ " | " ++ cName ++ " | " ++ show gen ++ " | " ++ show args')


checkVar :: AExpr -> Maybe VarType -> String -> AExprAnalyzer -> Analyzer' AExprRes
checkVar v@(Var offset name gen args more) wantedType scopeName analyzer = 
  case extractPtr wantedType of
-- |  previous was a class
    Just c@(VClass cName gen _) -> do
      args' <- checkArgs gen args analyzer 
      candidate <- listToMaybe <$> (filterM (filterArgsMatch args' gen analyzer) =<< findInClass cName name)
      readyArgs <- updatePostProcessedArgs args'
      checkVarMore v c readyArgs retBuilder candidate

-- |  this is first, so it has to be variable global or local
    Nothing -> do
      args' <- checkArgs gen args analyzer
      r <- gets rType
      candidate <- listToMaybe . checkFunPtr r <$> (filterM (filterArgsMatch args' gen analyzer) =<< find' [scopeName, name])
      readyArgs <- updatePostProcessedArgs args'
      checkVarFirst v readyArgs retBuilder candidate scopeName

-- |  error previous was not a class
    (Just x) -> throwError $  NotAClass  (show x ++ " | " ++ show v)
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
    matchArgs (SFunction _ _ _ t args _) = last a == t && length a - 1 == length args && compareArgs args
    matchArgs _ = False
    compareArgs = all (\(t, FunArg t' _) -> t == t'). zip (init a) 
    isOnlyOne l 
      | length l < 2 = l
      | otherwise = error "Jebać pis"
checkFunPtr _ = id
