module Compiler.Translator.BExpr where

import           AST
import           Compiler.Translator.Type
import           Control.Monad.Reader     (asks)

--  TODO replace mock with real feature
negationTranslator :: BExpr -> Translator
negationTranslator (Not bExpr) = do
  res <- concat <$> injectTranslator bExprTranslatorGetter bExpr
  return ["!" ++ res]

--  bExprTranslator <- asks bExprTranslatorGetter
bBinaryTranslator :: BExpr -> Translator
bBinaryTranslator (BBinary op a b) = do
  bExprTranslator <- asks bExprTranslatorGetter
  let op' = bBinOpToString op
  a' <- bExprTranslator a
  b' <- bExprTranslator b
  return ["(" ++ head a' ++ ") " ++ op' ++ " (" ++ head b' ++ ")"]

bBinOpToString :: BBinOp -> String
bBinOpToString a =
  case a of
    And -> "&&"
    Or  -> "||"

rBinaryTranslator :: BExpr -> Translator
rBinaryTranslator (RBinary op a b) = do
  aExprTranslator <- asks aExprTranslatorGetter
  let op' = rBinOpToString op
  a' <- aExprTranslator a
  b' <- aExprTranslator b
  return [head a' ++ " " ++ op' ++ " " ++ head b']

--  return  ["(" ++ head a' ++ ") " ++ op' ++ " (" ++ head b' ++ ")"]
rBinOpToString :: RBinOp -> String
rBinOpToString a =
  case a of
    Greater   -> ">"
    Less      -> "<"
    Equal     -> "=="
    EqLess    -> "<="
    EqGreater -> ">="
