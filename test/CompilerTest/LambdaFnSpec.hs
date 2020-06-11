module CompilerTest.LambdaFnSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec
--
--tmp = do
--  (ast, out) <- compile path "constFunArgExample"
--  let expectedAST = []
--  let expectedOUT = []
--  ast `shouldBe` expectedAST
--  out `shouldBe` expectedOUT

path = "res/test/lambdaFn/" 

main =
  describe "Lambda function" $ do
    context "without body block" $ do
      it "can be declared" canBeDeclaredTest
      it "can be invoked" pending
    context "without body block" $ do
      it "can be declared" canBeBlockDeclaredTest
      it "can be invoked" pending

canBeDeclaredTest = do
  (ast, out) <- compile path "simpleLambdaExample"
  let expectedAST = []
  let expectedOUT = []

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canBeInvokedTest = pending

canBeBlockDeclaredTest = do
  (ast, out) <- compile path "simpleBlockLambdaExample"
  let expectedAST = []
  let expectedOUT = []

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT
