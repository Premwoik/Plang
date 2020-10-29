module Compiler.Analyzer.BExpr where

import           AST
import           Compiler.Analyzer.Type
import           Control.Monad.State    (gets)
import           Control.Monad.Writer   (tell)

notAnalyzer :: BExpr -> Analyzer' BExpr
notAnalyzer (Not bExpr) = Not <$> injectAnalyzer bExprAnalyzerGetter bExpr

boolVarAnalyzer :: BExpr -> Analyzer' BExpr
boolVarAnalyzer (BoolVar aExpr) = do
  tmpType <- gets rType
  setType VBool
  (_, _, a) <- injectAnalyzer aExprAnalyzerGetter aExpr
  setType tmpType
  return $ BoolVar a

rBinaryAnalyzer :: BExpr -> Analyzer' BExpr
rBinaryAnalyzer t@(RBinary op a b) = do
  (_, _, a') <- injectAnalyzer aExprAnalyzerGetter a
  (_, _, b') <- injectAnalyzer aExprAnalyzerGetter b
  return $ RBinary op a' b'

bBinaryAnalyzer :: BExpr -> Analyzer' BExpr
bBinaryAnalyzer (BBinary op a b) = do
  a' <- injectAnalyzer bExprAnalyzerGetter a
  b' <- injectAnalyzer bExprAnalyzerGetter b
  return $ BBinary op a' b'
