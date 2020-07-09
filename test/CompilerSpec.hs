module CompilerSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec
import qualified CompilerTest.LambdaFnSpec as LambdaFn
import qualified CompilerTest.FunctionSpec as Function
import qualified CompilerTest.ScopeSpec as Scope 
import qualified CompilerTest.ClassSpec as Class 
import qualified CompilerTest.ListSpec as List
import qualified CompilerTest.OptionalSpec as Optional 
import qualified CompilerTest.BasicSpec as Basic 


main = do
  Function.main
  LambdaFn.main
  Scope.main
  Class.main
  List.main
  Optional.main
  Basic.main
  

