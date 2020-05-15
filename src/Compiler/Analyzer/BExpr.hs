module Compiler.Analyzer.BExpr where

import Compiler.Analyzer.Type
import AST
import Control.Monad.Writer(tell)

notAnalyzer :: BExpr -> BExprAnalyzer -> Analyzer' BExpr
notAnalyzer (Not bExpr) analyzer = 
  Not <$> analyzer bExpr

boolVarAnalyzer :: BExpr -> AExprAnalyzer -> Analyzer' BExpr
boolVarAnalyzer (BoolVar aExpr) analyzer = do
  (_, _, a) <- analyzer aExpr
  return $ BoolVar a

rBinaryAnalyzer :: BExpr -> AExprAnalyzer -> Analyzer' BExpr
rBinaryAnalyzer t@(RBinary op a b) analyzer = do
  (_, _, a') <- analyzer a
  (_, _, b') <- analyzer b
  return $ RBinary op a' b'

bBinaryAnalyzer :: BExpr -> BExprAnalyzer -> Analyzer' BExpr
bBinaryAnalyzer (BBinary op a b) analyzer = do
  a' <- analyzer a 
  b' <- analyzer b
  return $ BBinary op a' b'
