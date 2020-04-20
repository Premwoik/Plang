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

varTranslator :: AExpr -> Translator
varTranslator a@(TypedVar name type' Nothing more') = do
  readyMore <- moreVarTranslator type' more'
  let n = unwrapVarName name
  let n' =
        case type' of
          VCopy VPointer {} -> "*" ++ n
          VRef VCopy {} -> "&" ++ n
          VPointer (VCopy t) _ -> "new " ++ typeToString t ++ "{" ++ unwrapVarName name ++ "}"
          _ -> n
  return . return . concat $ n' : readyMore
varTranslator a@(TypedVar name type' (Just args) more') = do
  trace ("args: " ++ show a) $ return ()
  readyMore <- moreVarTranslator type' more'
  args' <- intercalate ", " . concat <$> mapM (injectTranslator aExprTranslatorGetter) args
  return . return . concat $ unwrapType type' ("(" ++ args' ++ ")") : readyMore
  where
    uName = unwrapVarName name
    uNameForce = unwrapVarNameForce name
    unwrapType (VPointer c@(VClass n g p) SharedPtr) a
--    invoke class constructor
      | n == uNameForce = "shared_ptr<" ++ unwrapType c "" ++ ">(new " ++ unwrapType c a ++ ")"
      | otherwise = unwrapType c a
    unwrapType (VPointer (VCopy t) SharedPtr) a = "new " ++ unwrapType t a
    unwrapType c@(VClass n g p) args
--    invoke class constructor
      | n == uNameForce = typeToString c ++ args
      | otherwise = uName ++ args
    unwrapType _ args = uName ++ args



moreVarTranslator :: VarType -> Maybe AExpr -> Translator
-- TODO handle pointers
-- TODO args passed in func application probably are not lexical analyzed
moreVarTranslator (VClass _ _ True) (Just e) = return . (\[x] -> '-' : '>' : x) <$> varTranslator e
moreVarTranslator _ (Just e) = return . (\[x] -> '.' : x) <$> varTranslator e
moreVarTranslator _ Nothing = return []

scopeMarkTranslator :: AExpr -> Translator
scopeMarkTranslator (ScopeMark _ sName (TypedVar n t a m)) = do
  trace ("SCOPE_TRANSLATOR " ++ sName ++ " | name: " ++ show n ++ " |> ") $ return ()
  injectTranslator aExprTranslatorGetter $ TypedVar n t a m
scopeMarkTranslator x = error $ show x

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
    getType (TypedVar _ (VPointer t _) _ _) = t