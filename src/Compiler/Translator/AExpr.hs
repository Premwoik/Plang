module Compiler.Translator.AExpr where

import Compiler.Translator.Type
import Control.Monad.Reader(asks)
import Data.List(intercalate)
import Debug.Trace
import AST


aExprNegTranslator (Neg a) = do
  a' <- injectTranslator aExprTranslatorGetter a
  return . concat $ [["(-"], a', [")"]]

bracketTranslator (ABracket _ aExpr) = do
  res <- injectTranslator aExprTranslatorGetter aExpr
  return ["(" ++ head res ++ ")"]

bracketApplyTranslator (ABracketApply _ aExpr args) = do
  res <- injectTranslator aExprTranslatorGetter aExpr
  args' <- intercalate ", " . concat <$> mapM (injectTranslator aExprTranslatorGetter) args
  return ["(" ++ head res ++ ")(" ++ args' ++ ")"]

binaryTranslator :: AExpr -> Translator
binaryTranslator (ABinary op a b) = do
  let op' = binaryOperatorToString op
  a' <- injectTranslator aExprTranslatorGetter a
  b' <- injectTranslator aExprTranslatorGetter b
  return [head a' ++ " " ++ op' ++ " " ++ head b']

binaryOperatorToString :: ABinOp -> String
binaryOperatorToString a =
  case a of
    Add      -> "+"
    Subtract -> "-"
    Multiply -> "*"
    Divide   -> "/"
    Modulo   -> "%"

varTranslator :: AExpr -> Translator
varTranslator a@(TypedVar name type' Nothing more') = do
--  trace ("varTranslator :: no args :: var : " ++ show a) $ return ()
  readyMore <- moreVarTranslator type' more'
--  trace ("varTranslator :: readyMore : " ++ show readyMore) $ return ()
  let n = unwrapVarName name
  let n' =
        case type' of
          VCopy VPointer {} -> "*" ++ n
          VRef VCopy {} -> "&" ++ n
          VPointer (VCopy t) _ -> "new " ++ typeToString t ++ "{" ++ unwrapVarName name ++ "}"
          _ -> n
  return . return . concat $ n' : readyMore

varTranslator a@(TypedVar name type' (Just args) more') = do
--  trace ("varTranslator :: with args :: var : " ++ show a) $ return ()
  readyMore <- moreVarTranslator type' more'
--  trace ("varTranslator :: readyMore : " ++ show readyMore) $ return ()
  args' <- intercalate ", " . concat <$> mapM (injectTranslator aExprTranslatorGetter) args
  return . return . concat $ unwrapType type' ("(" ++ args' ++ ")") : readyMore
  where
    uName = unwrapVarName name
    uNameForce = unwrapVarNameForce name
    unwrapType (VPointer c@(VClass n g) SharedPtr) a
--    invoke class constructor
      | unwrapVarNameForce n == uNameForce = "shared_ptr<" ++ unwrapType c "" ++ ">(new " ++ unwrapType c a ++ ")"
      | otherwise = unwrapType c a
    unwrapType (VPointer (VCopy t) SharedPtr) a = "new " ++ unwrapType t a
    unwrapType c@(VClass n g) args
--    invoke class constructor
      | unwrapVarNameForce n == uNameForce = typeToString c ++ args
      | otherwise = uName ++ args
    unwrapType _ args = uName ++ args
varTranslator a = error (show a)

moreVarTranslator :: VarType -> Maybe AExpr -> Translator
-- TODO handle pointers
-- TODO args passed in func application probably are not lexical analyzed
moreVarTranslator VPointer {} (Just e) = return . (\[x] -> '-' : '>' : x) <$> varTranslator e
moreVarTranslator a (Just e) = return . (\[x] -> '.' : x) <$> varTranslator e
moreVarTranslator _ Nothing = return []

scopeMarkTranslator :: AExpr -> Translator
scopeMarkTranslator (ScopeMark _ sName (TypedVar n t a m)) = 
  injectTranslator aExprTranslatorGetter $ TypedVar n t a m
scopeMarkTranslator x = error $ show x

optionalTranslator :: AExpr -> Translator
optionalTranslator (Optional _ aExpr NullOT) = do
  a <- concat <$> injectTranslator aExprTranslatorGetter aExpr
  return [a ++ ".isNotNull()"]
optionalTranslator (Optional _ aExpr BoolOT) = do
  a <- concat <$> injectTranslator aExprTranslatorGetter aExpr
  return [a]
optionalTranslator (Optional _ aExpr BoolPtrOT) = do
  a <- concat <$> injectTranslator aExprTranslatorGetter aExpr
  return [a ++ ".isNotNull() && " ++ a ++ "*"]
optionalTranslator _ = return []

lambdaTranslator :: AExpr -> Translator
lambdaTranslator (LambdaFn offset capture t args stmts) = do
  stmts' <- concat <$> tStmts
  return ["["++ mode capture ++"](" ++ tArgs ++ "){" ++ stmts' ++ "}"]
  where
--    mode CMOn = "="
    mode CMOn = "&"
    mode _ = ""
    tArgs = intercalate "," $ map (\(FunArg t n) -> unwrapType t n) args
    tStmts =
      case stmts of
        [OtherFn o expr] -> do
          res <- head <$> injectTranslator aExprTranslatorGetter expr
          return ["return " ++ res ++ ";\n"]
        _  -> blockTranslator' (injectTranslator fnStmtTranslatorGetter) stmts
        
    unwrapType (VFn t cm) n = typeToString (VFnNamed n t cm)
    unwrapType x n = typeToString x ++ " " ++ n

listVarTranslator :: AExpr -> Translator
listVarTranslator (TypedListVar expr t) = do
  let size = length expr
  res <- concat <$> mapM (injectTranslator aExprTranslatorGetter) expr
  let resAsString = intercalate "," res
  return . return $ "new " ++ typeToString t ++ "[" ++ show size ++ "]{" ++ resAsString ++ "}"

nativePtrInputWrapper (NativePtrInput aExpr) = do
  res <- head <$> injectTranslator aExprTranslatorGetter aExpr
  return [res ++ ".getNativePtr()"]
  

nativePtrResWrapper (NativePtrRes aExpr) = do
  res <- head <$> injectTranslator aExprTranslatorGetter aExpr
  let t = getType aExpr
  return ["shared_ptr<" ++ typeToString t ++ ">(" ++ res ++ ")"]
  where
    getType (TypedVar _ (VPointer t _) _ Nothing) = t
    getType (TypedVar _ _ _ (Just more)) = getType more
    getType t = error $ "HAHA" ++ show t