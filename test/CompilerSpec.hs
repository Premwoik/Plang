module CompilerSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified CompilerTest.LambdaFnSpec as LambdaFn
import qualified CompilerTest.FunctionSpec as Function
import qualified CompilerTest.ScopeSpec as Scope 
import qualified CompilerTest.ClassSpec as Class 


main = do
  Function.main
  LambdaFn.main
  Scope.main
  Class.main

