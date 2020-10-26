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
  , checkBracketApply
  , checkOptional
  , checkScopeMark
  , checkNull
  ) where

import           AST
import           Compiler.Analyzer.AExpr.Var
import           Compiler.Analyzer.Browser
import           Compiler.Analyzer.Error
import           Compiler.Analyzer.Type
import           Compiler.Analyzer.UniversalCheckers (checkFnArgs, compareGens, check)
import           Control.Monad.Except                (throwError)
import           Control.Monad.State                 (get, gets, modify, put)
import           Control.Monad.Writer                (tell)
import Control.Monad(when)
import           Data.List                           (find)
import           Data.Maybe                          (fromMaybe)
import           Debug.Trace
import Data.Functor

checkOptional :: AExpr -> Analyzer' AExprRes
checkOptional (Optional o (Optional o2 aExpr _) _) = do
  (t, _, a) <- injectAnalyzer aExprAnalyzerGetter aExpr
  case t of
    VPointer c@VClass {} _ ->
      checkBoolInterface o c *> return (VBool, [], Optional o a BoolPtrOT)
    t           -> makeError o $ NotAllowedOptionalUse t
checkOptional (Optional o aExpr _) = do
  (t, _, a) <- injectAnalyzer aExprAnalyzerGetter aExpr
  case t of
    VPointer {} -> return (VBool, [], Optional o a NullOT)
    c@VClass {}   -> checkBoolInterface o c *> return (VBool, [], Optional o a BoolOT)
    t           -> makeError o $ NotAllowedOptionalUse t


checkBoolInterface o t = checkInterface o "boolOp" t

checkInterface :: Offset -> String -> VarType -> Analyzer' ()
checkInterface o name t@(VClass n _) = do
  ops <- findInClass n name
  when (null ops) $ makeError o $ NotAllowedOptionalUse t


-- TODO add checking if class has implemented bool interface
checkScopeMark :: AExpr -> Analyzer' AExprRes
checkScopeMark (ScopeMark o scopeName aExpr) = do
  addOffset o
  res <- (\(a, b, res) -> (a, b, ScopeMark o scopeName res)) <$> checkVar aExpr Nothing scopeName 
  removeOffset
  return res

checkListVar :: AExpr -> Analyzer' AExprRes
checkListVar (ListVar _ [] (Just t)) = do
  type' <- wrapAllocationMethod $ VClass (VName "ArrayList") [VGenPair "T" t]
  return
    ( fst type'
    , []
    , TypedVar (VName "ArrayList") (snd type') (Just []) Nothing)
    where
checkListVar (ListVar o [] Nothing) = makeError o NotProvidedListType
checkListVar a@(ListVar o elems wantedType) = do
  let len = show $ length elems
  (types', injs, elems') <-
    classToPointer . foldr (\(t', inj, res) (ts, injs, ress) -> (t' : ts, inj : injs, res : ress)) ([], [], []) <$>
    mapM (injectAnalyzer aExprAnalyzerGetter) elems
  if checkType types'
    then return ()
    else makeError o $ NotAllElementsHaveSameType elems wantedType
  let itemType = fromMaybe (head types') wantedType
  let args =
        Just
          [ TypedListVar elems' itemType
          , TypedVar (VName len) VInt Nothing Nothing
          , TypedVar (VName len) VInt Nothing Nothing
          ]
  t <- wrapAllocationMethod (VClass (VName "ArrayList") [VGenPair "T" itemType])
  return (fst t, concat injs, TypedVar (VName "ArrayList") (snd t) args Nothing)
  where
    checkType (t:ts) = all (\x -> t == x) ts && t == fromMaybe t wantedType
    classToPointer (t, i, e) = (markClassAsPointer t, i, markVarClassAsPointer e)

--    typesMatchOrError types =
checkRange :: AExpr -> Analyzer' AExprRes
checkRange (Range o s a b) = do
  let analyzer = injectAnalyzer aExprAnalyzerGetter
  (ta, _, a') <- analyzer a
  (tb, _, b') <- analyzer b
  (ts, _, s') <- analyzer . fromMaybe (IntConst o 1) $ s
  allInt ta tb ts
  return (VInt, [], Range o (Just s') a' b')
  where
    allInt a b c
      | a == b && a == c && a == VInt = return ()
      | otherwise = makeError o NotAllowedRangeType

-- False is for normal assign and True is for invoking checking
--TODO return can be done only at the end of the last line???
checkLambdaFn :: Bool -> AExpr -> Analyzer' AExprRes
checkLambdaFn False a@(LambdaFn offset capture retType args stmts) = do
  ret <- gets rType
  allTypesKnown ret
  where
    allTypesKnown (VFn types _)
      | VAuto `notElem` init types = do
        let args' = zipWith (\t (FunArg _ n) -> FunArg t n) types args
        let ret' = last types
        checkLambdaFn True (LambdaFn offset capture ret' args' stmts) 
      | otherwise = makeError offset ArgumentsTypeMissing
    allTypesKnown _ = return (VFn [] CMAuto, [], a) --makeError offset "Lambda expression must have defined strict type!"
checkLambdaFn True a@(LambdaFn offset capture retType args stmts) = do
  args' <- checkFnArgs args
  addArgsScope offset args
  addScope "lambda"
  captureTmp <- gets useCapture
  setCapture False
  retTmp <- gets rType
  setType retType
  stmts' <- concat <$> mapM (injectAnalyzer functionStmtAnalyzerGetter) stmts  -- =<< checkReturn stmts
  setType retTmp
  nRet <- checkReturn stmts'
  cs <- toCaptureMode <$> gets useCapture
  setCapture captureTmp
  removeScope
  removeScope
  return (makeRet nRet cs, [], LambdaFn offset cs nRet args' stmts')
  where
    makeRet nRet = VFn (map (\(FunArg t _) -> t) args ++ [nRet])
    toCaptureMode True = CMOn
    toCaptureMode _    = CMOff
    checkReturn stmts 
      | retType /= VVoid = do
        r <- case last stmts of
          ReturnFn o (Just e) -> aExprExtractType (FunArg VAuto "") e
          OtherFn o e -> aExprExtractType (FunArg VAuto "") e
          _ -> makeError offset $ CustomError "cant match return type in case Analyzer/AExpr 124"
        check offset r retType r r
      | otherwise = return VVoid
      
      
checkNegBlock (Neg a) = do
  (t, _, a') <- injectAnalyzer aExprAnalyzerGetter a
  return (t, [], addNeg a')
  where
    addNeg (IntConst o x)   = IntConst o (-x)
    addNeg (FloatConst o x) = FloatConst o (-x)
    addNeg x                = Neg x

checkBracket :: AExpr -> Analyzer' AExprRes
checkBracket (ABracket o aExpr) = do
  (t', inc, aExpr') <- injectAnalyzer aExprAnalyzerGetter aExpr
  return (t', inc, ABracket o aExpr')

checkBracketApply :: AExpr -> Analyzer' AExprRes
checkBracketApply (ABracketApply o aExpr args) = do
  args' <- fromMaybe [] <$> checkArgs [] (Just args) 
  argTypes <- mapM (aExprExtractType (FunArg VAuto "")) args'
  tmpType <- gets rType
  setType (VFn (argTypes ++ [VAuto]) CMAuto) 
  (t', inc, aExpr') <- injectAnalyzer aExprAnalyzerGetter aExpr
  setType tmpType
  ret <-
    case t' of
      VFn ts _ -> return $ last ts
      _        -> makeError o ApplyNotAFunction
  return (ret, inc, ABracketApply o aExpr' args')

checkABinary :: AExpr -> Analyzer' AExprRes
checkABinary t@(ABinary op a b) = do
  let aAnalyzer = injectAnalyzer aExprAnalyzerGetter
  (ta, _, a') <- aAnalyzer a
  (tb, _, b') <- aAnalyzer b
  nType <- compareGens 0 ta tb
  return (nType, [], TypedABinary nType op a' b')

checkIfStatement :: AExpr -> Analyzer' AExprRes
checkIfStatement (If o ifs) = do
  varName <- takeVarName
  (types, newIfs) <- unzip <$> mapM (makeIf varName) ifs
  let readyRetType = retType types
  let newCache = [AssignFn (-1) (toVar varName) readyRetType Nop, IfFn (-1) newIfs]
  return (readyRetType, newCache, Var o varName [] Nothing Nothing)
  where
    makeIf varName (cond, body) = do
      cond' <- injectAnalyzer bExprAnalyzerGetter cond
      (aE, rest) <- unpackLastExpr . reverse . concat <$> mapM (injectAnalyzer functionStmtAnalyzerGetter) body
      let ret = reverse $ AssignFn (-1) (toVar varName) VBlank aE : rest
      return (aExprToType aE, (cond', ret))
    unpackLastExpr (OtherFn _ aE:rest) = (aE, rest)
    retType (t:s) =
      if all (== t) s
        then t
        else error "If statement return' type is not the same in every block"
    retType _ = error "If statement return' type is not the same in every block"
    toVar n = Var (-1) n [] Nothing Nothing

--    unpackLastExpr (a:_) = throwError $ UnsupportedTypeException (show a)
aExprToType :: AExpr -> VarType
aExprToType a =
  case a of
    IntConst {}      -> VInt
    FloatConst {}    -> VFloat
    StringVal {}     -> VString
    ABool {}         -> VBool
    TypedVar _ t _ _ -> t
    _                -> VAuto

checkIntConst :: AExpr -> Analyzer' AExprRes
checkIntConst e@(IntConst o i) = return (VInt, [], e)

checkFloatConst :: AExpr -> Analyzer' AExprRes
checkFloatConst e@(FloatConst o f) = return (VFloat, [], e)

checkStringConst :: AExpr -> Analyzer' AExprRes
checkStringConst e@(StringVal o s) = return (VString, [], e)

checkABool :: AExpr -> Analyzer' AExprRes
checkABool (ABool bExpr) = do
  bExpr' <- injectAnalyzer bExprAnalyzerGetter bExpr
  return (VBool, [], ABool bExpr')

checkNull :: AExpr -> Analyzer' AExprRes
checkNull n@(Null offset) = do
  ret <- gets rType
  case ret of
    VPointer {} -> return (ret, [], n)
    t -> makeError offset $ CustomError $ "Null can be used only with ptr type. You tried to used it with type - " ++ show t