{-# LANGUAGE FlexibleContexts #-}

module Compiler.Analyzer.AExpr.Var where

import           AST
import           Compiler.Analyzer.Browser
import           Compiler.Analyzer.Error
import           Compiler.Analyzer.Type
import           Compiler.Analyzer.Universal
import           Compiler.Analyzer.UniversalCheckers (checkTypesMatchGens)
import           Control.Monad                       (filterM, zipWithM)
import           Control.Monad.State                 (get, gets, modify, put)
import           Control.Monad.Writer                (tell)
import           Data.List                           (find)
import           Data.Maybe                          (fromJust, fromMaybe,
                                                      isNothing, listToMaybe)

markClassAsPointer :: [VarType] -> [VarType]
markClassAsPointer = map mapper
  where
    mapper (VGenPair n c@VClass {}) = VGenPair n (VPointer c SharedPtr)
    mapper c@VClass {}              = VPointer c SharedPtr
    mapper e                        = e

markVarClassAsPointer :: [AExpr] -> [AExpr]
markVarClassAsPointer = map mapper
  where
    mapper (TypedVar n c@VClass {} a m) = TypedVar n (VPointer c SharedPtr) a m
    mapper x = x

-- | markNativePtr acceptedArgs actualArgs
-- function that
markNativePtr :: [FunArg] -> [AExpr] -> [AExpr]
markNativePtr = zipWith extractPtr
    -- native pointer
    -- native -> native
  where
    extractPtr (FunArg (VPointer _ NativePtr) _) v@(TypedVar _ (VPointer _ NativePtr) _ _) = v
    -- native -> shared
    extractPtr (FunArg (VPointer _ NativePtr) _) v@(TypedVar _ VPointer {} _ _) = NativePtrInput v
    -- native -> value
    extractPtr (FunArg (VPointer _ NativePtr) _) v@(TypedVar _ (VClass (VName "ArrayList") _) _ _) = NativePtrInput v
    -- native -> ??
    extractPtr (FunArg (VPointer _ NativePtr) _) (TypedVar a t b c) = TypedVar a (VRef (VCopy t)) b c
    -- |  not native
    -- shared <- extract addr (&) from value
    extractPtr (FunArg (VPointer _ SharedPtr) _) (TypedVar n (t@VClass {}) a m) = TypedVar n (VRef (VCopy t)) a m
    -- copy <- shared
    extractPtr (FunArg VClass {} _) (TypedVar n t@VPointer {} c d) = TypedVar n (VCopy t) c d
    extractPtr a v = v

--  don't mark when given arg is just native ptr
--    TODO be carefull with this
extractPtr (Just (VPointer t _)) = Just t
extractPtr t                     = t

unwrapGen (VGenPair _ t) = t
unwrapGen x              = x

fixPtrGenInArgs :: [VarType] -> AExpr -> AExpr
fixPtrGenInArgs gens (TypedVar n t a m) = TypedVar n newType a m
  where
    newType =
      case find (\t' -> unwrapGen t' == t) gens of
        (Just t') -> unwrapGen t'
        Nothing   -> t
fixPtrGenInArgs _ t = t

checkArgs :: [VarType] -> Maybe [AExpr] -> Analyzer' (Maybe [AExpr])
checkArgs gen args =
  case args of
    (Just a) -> Just . map (fixPtrGenInArgs gen . (\(_, _, x) -> x)) <$> mapM (injectAnalyzer aExprAnalyzerGetter) a
    Nothing -> return Nothing

-- | filterARgsMatch args gen field
filterArgsMatch :: Maybe [AExpr] -> [VarType] -> ScopeField -> Analyzer' Bool
filterArgsMatch Nothing _ SVar {} = return True
filterArgsMatch Nothing [] SFunction {} = return True
filterArgsMatch (Just a) gen v@SVar {} = maybeArgsMatchVar a gen v
filterArgsMatch (Just args) gen f = maybeArgsMatch (Just args) gen f
filterArgsMatch a g f = error (show a ++ " | " ++ show g ++ " | " ++ show f)

-- this function must have access to monad to check if lambdas is ok with given params
maybeArgsMatchVar :: [AExpr] -> [VarType] -> ScopeField -> Analyzer' Bool
maybeArgsMatchVar args gen (SVar o n p (VFn t _) _ _) =
  let funArgs = map (`FunArg` "") $ init t
   in maybeArgsMatch (Just args) gen (SFunction o n p (last t) funArgs defaultMethodDetails)
maybeArgsMatchVar _ _ _ = return False

wrapAllocationMethod :: VarType -> Analyzer' (VarType, VarType)
wrapAllocationMethod v = do
  ret <- gets rType
  return $
    case (v, ret) of
      (VPointer t _, VCopy {}) -> (t, VCopy v)
      (VPointer {}, _) -> same v
      (VClass {}, VPointer {}) -> (VPointer v SharedPtr, VPointer (VCopy v) SharedPtr)
      (_, VPointer {}) -> (VPointer v SharedPtr, VPointer (VCopy v) SharedPtr)
      (VRef {}, VRef {}) -> same v
      (_, VRef {}) -> same $ VRef v
      _ -> same v
  where
    same x = (x, x)

zipTypesWithClassGen (VClass n g) = do
  cl <- listToMaybe <$> find' ["", unwrapVarNameForce n]
  case cl of
    Just (SClass _ _ _ gn _ _) -> return $ VClass n (zipWith VGenPair gn g)
    Nothing -> makeError 0 $ CustomError $ "Cant find class - " ++ show (unwrapVarNameForce n)
zipTypesWithClassGen (VPointer x y) = flip VPointer y <$> zipTypesWithClassGen x
zipTypesWithClassGen x = error "zipTypesWithClassGen - not class or poiter to a class"

-- | variable that have not given args - is not executed
checkVarFirst :: AExpr -> Maybe [AExpr] -> RetBuilderT -> Maybe ScopeField -> String -> Analyzer' AExprRes
checkVarFirst var@(Var offset name gen _ more) Nothing retBuilder obj scopeName =
  case obj
    -- | casual variable that is not executed
        of
    Just tvar@(SVar _ n p t s _) -> do
      scope <- getFileName scopeName
      t' <-
        if isNothing more
          then wrapAllocationMethod t
          else do
            zt <- zipTypesWithClassGen t
            return (zt, zt)
      let newName = wrapVarInsideScopeMark scope scopeName (scaleNameWithScope' [s, n])
      retBuilder (fst t') (TypedVar (defaultPath p newName) (snd t') Nothing) more
    -- | pointer to a function - not executed
    Just (SFunction _ n p t a _) -> do
      scope <- getFileName scopeName
      let newType = VFn (map (\(FunArg t _) -> t) a ++ [t]) CMOff
      retBuilder newType (TypedVar (defaultPath p (wrapInsideScopeMark scope scopeName n)) newType Nothing) more
    p -> do
      storage <- gets scopes
      makeError offset $ VariableMissing name
-- | variable that have given args - is executed
checkVarFirst var@(Var offset name gen _ more) args' retBuilder obj scopeName =
  case obj
    -- | name is variable
        of
    Just (SVar i n p (VFn t _) s details) -> do
      let funArgs = map (`FunArg` "") $ init t
      checkVarFirst var args' retBuilder (Just (SFunction i n p (last t) funArgs details)) s
    -- | name is class
    Just cl@(SClass _ n p g ps sc) -> do
      if length g == length gen
        then return ()
        else makeError offset $ GenericMissing (show g)
      let gen' = zipWith VGenPair g gen
      checkTypesMatchGens offset sc gen'
      constructor <- filterConstructor offset args' gen' cl
      let (SFunction o n _ _ cArgs _) = constructor
      let args'' = args' >>= (Just . markNativePtr cArgs)
      newType <- wrapAllocationMethod $ VClass (VName n) (markClassAsPointer gen')
      retBuilder (fst newType) (TypedVar (defaultPath p n) (snd newType) args'') more
    -- | name is function
    Just f@(SFunction _ n p t a _) -> do
      gen <- prepareFunctionGens gen f <$> zipWithM aExprExtractType a (fromJust args')
      let t' = replaceGenWithType gen t
      let args'' = args' >>= (Just . markNativePtr a)
      scope <- getFileName scopeName
      retBuilder t' (TypedVar (defaultPath p (wrapInsideScopeMark scope scopeName n)) t' args'') more
    p -> do
      storage <- gets scopes
      makeError offset $ VariableMissing name

wrapInsideScopeMark scope scopeName n =
  case scopeName of
    ""       -> n
    "g"      -> scaleNameWithScope' ["::", n]
    "global" -> scaleNameWithScope' ["::", n]
    "args"   -> scaleNameWithScope' ["args", n]
    "this"   -> scaleNameWithScope' ["this", n]
    _        -> scaleNameWithScope' [scope, "::", n]

wrapVarInsideScopeMark scope scopeName n
  | scopeName `notElem` ["", "g", "global", "args", "this", "fun"] = scaleNameWithScope' [scope, "::", n]
  | otherwise = n

checkVarMore (Var offset name _ args more) (VClass cName gen) args' retBuilder method =
  case method of
    Just (SVar _ n p t _ _) -> retBuilder t (TypedVar (defaultPath p (scaleNameWithScope' ["this", n])) t Nothing) more
    Just (SFunction _ n p t a _) -> do
      let args'' = Just $ markNativePtr a (fromMaybe [] args')
      nType <- fixType gen t
      retBuilder nType (TypedVar (defaultPath p n) nType args'') more
    x -> makeError offset $ ClassVariableMissing (unwrapVarName cName) name

markPtrToUnwrap :: ()
markPtrToUnwrap = ()

checkVar :: AExpr -> Maybe VarType -> String -> Analyzer' AExprRes
checkVar v@(Var offset name gen args more) wantedType scopeName = do
  case extractPtr wantedType of
    Just c@(VClass cName gen) -> do
      args' <- checkArgs gen args
      candidate <- listToMaybe . filter isPublic <$> (filterM (filterArgsMatch args' gen) =<< findInClass cName name)
      readyArgs <- updatePostProcessedArgs args'
      checkVarMore v c readyArgs retBuilder candidate
    Nothing -> do
      r <- gets rType
      setType VAuto
      args' <- checkArgs gen args
      setType r
      candidate <-
        detectCaptureInLambda =<<
        (listToMaybe . checkFunPtr r <$> (filterM (filterArgsMatch args' gen) =<< find' [scopeName, name]))
      readyArgs <- updatePostProcessedArgs args'
      checkVarFirst v readyArgs retBuilder candidate scopeName
    (Just x) -> makeError offset $ NotAClass name
  where
    retBuilder :: RetBuilderT
    retBuilder type' var more = makeOutput type' var <$> handleMore more (Just type')
    handleMore (Just m) x = Just <$> checkVar m x ""
    handleMore Nothing _  = return Nothing
    makeOutput _ wrapper (Just (v, i, m)) = (v, i, wrapper (Just m))
    makeOutput t wrapper Nothing          = (t, [], wrapper Nothing)

detectCaptureInLambda :: Maybe ScopeField -> Analyzer' (Maybe ScopeField)
detectCaptureInLambda v@(Just f@SVar {}) = do
  sName <- getScopeName
  if sName /= "lambda" || sVarOwnerName f == "g"
    then return v
    else do
      inLambda <- isVarInLambda (sVarName f)
      if not inLambda
        then setCapture True >> return v
        else return v
detectCaptureInLambda v = return v

type RetBuilderT = VarType -> (Maybe AExpr -> AExpr) -> Maybe AExpr -> Analyzer' AExprRes

updatePostProcessedArgs :: Maybe [AExpr] -> Analyzer' (Maybe [AExpr])
updatePostProcessedArgs (Just args) = Just . reverse <$> mapM updateArg (reverse args)
  where
    updateArg LambdaFn {} = takePostAExpr
    updateArg x           = return x
updatePostProcessedArgs Nothing = return Nothing

defaultPath p n =
  case p of
    Just "" -> VName n
    Just p  -> VNameNative n p
    Nothing -> VName n

checkFunPtr :: VarType -> [ScopeField] -> [ScopeField]
checkFunPtr t@(VFn a _) = isOnlyOne . filter matchArgs
  where
    matchArgs f@SFunction {} =
      last a == getType f && length a - 1 == length (sFunctionArgs f) && compareArgs (sFunctionArgs f)
    matchArgs (SVar _ _ _ (VFn ts _) _ _) = a == ts
    matchArgs _ = False
    compareArgs = all (\(t, FunArg t' _) -> t == t') . zip (init a)
    isOnlyOne l
      | length l < 2 = l
      | otherwise = error "more then one"
checkFunPtr _ = id
