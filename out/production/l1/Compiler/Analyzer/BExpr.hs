module Compiler.Analyzer.BExpr where

import Compiler.Analyzer.Type
import AST
import Control.Monad.Writer(tell)

rBinaryAnalyzer :: BExpr -> AExprAnalyzer -> Analyzer' BExpr
rBinaryAnalyzer t@(RBinary op a b) analyzer = do
  (_, _, a') <- analyzer a
  (_, _, b') <- analyzer b
  return $ RBinary op a' b'

