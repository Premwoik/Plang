module CompilerTest.ScopeSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec

path = "res/tests/scope/"

main =
  describe "Scope" $ -- do
    context "in lambda" $ do
      it "with capture specifiers" pending --canBeDeclaredTest
      it "without capture specifiers" pending
--    context "without body block" $ do
--      it "can be declared" canBeBlockDeclaredTest
--      it "can be invoked" pending

canBeDeclaredTest = do
  (ast, out) <- compile path "lambdaScopeExample"
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
