module CompilerTest.ClassSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec

path = "res/tests/class/"

main =
  describe "Class" $ -- do
    context "with inheritance" $ do
      it "can parse" canParseInheritanceTest--canBeDeclaredTest
--    context "without body block" $ do
--      it "can be declared" canBeBlockDeclaredTest
--      it "can be invoked" pending

canParseInheritanceTest = do
  (ast, out) <- compile path "classInheritanceExample"
  let expectedAST = []
  let expectedOUT = []

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canBeBlockDeclaredTest = do
  (ast, out) <- compile path "simpleBlockLambdaExample"
  let expectedAST = []
  let expectedOUT = []

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT
