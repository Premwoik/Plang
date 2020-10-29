module Compiler.Translator.AExpr where

import Compiler.Translator.Type
import Control.Monad.Reader(asks)
import Data.List(intercalate)
import AST


aExprNegTranslator (Neg a) = do
  a' <- injectTranslator aExprTranslatorGetter a
  return . concat $ [["(-"], a', [")"]]

bracketTranslator (ABracket aExpr) = do
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
varTranslator (TypedVar name type' Nothing more') = do
  readyMore <- moreVarTranslator type' more'
  return . return . concat $ name : readyMore
varTranslator (TypedVar name type' (Just args) more') = do
  readyMore <- moreVarTranslator type' more'
  args' <- intercalate ", " . concat <$> mapM (injectTranslator aExprTranslatorGetter) args
  return . return . concat $ (name ++ "(" ++ args' ++ ")") : readyMore

moreVarTranslator :: VarType -> Maybe AExpr -> Translator
-- TODO handle pointers
-- TODO args passed in func application probably are not lexical analyzed
moreVarTranslator (VClass _ _ True) (Just e) = return . (\[x] -> '-' : '>' : x) <$> varTranslator e
moreVarTranslator _ (Just e) = return . (\[x] -> '.' : x) <$> varTranslator e
moreVarTranslator _ Nothing = return []

listVarTranslator :: AExpr -> Translator
listVarTranslator (ListVar expr) = do
  let wantedType = VInt
--  backup <- get
--  put $ backup {cache = TypeCache []}
  let size = length expr
  res <- concat <$> mapM (injectTranslator aExprTranslatorGetter) expr
  let resAsString = intercalate "," res
  return . return $ "new " ++ typeToString wantedType ++ "[" ++ show size ++ "]{" ++ resAsString ++ "}"

