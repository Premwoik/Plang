module Compiler.Analyzer.UniversalCheckers where

import AST

import Compiler.Analyzer.Type (Analyzer', scaleNameWithScope)


checkFnArgs :: [FunArg] -> Analyzer' [FunArg]
checkFnArgs = return . map (\(FunArg t n) -> FunArg t (concat (scaleNameWithScope ["args", n])))



