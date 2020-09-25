module CompilerTest.BasicSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec

path = "res/tests/variable/"

main =
  describe "Scope" $ -- do
    context "in lambda" $ do
      it "with capture specifiers" assignOperatorTest

assignOperatorTest = do
  (ast, out) <- compile path "assignOperatorsExample"
  let expectedAST = [IFile "assignOperatorsExample" "res/tests/variable/assignOperatorsExample.mard" (AST [Skip 0,Function 37 "Main" [] VVoid [] [AssignFn 55 (TypedVar (VName "a") VAuto Nothing Nothing) VInt (IntConst 59 0),AssignFn 63 (TypedVar (VName "a") VAuto Nothing Nothing) VBlank (TypedABinary VInt Add (TypedVar (VName "a") VInt Nothing Nothing) (IntConst 68 1)),AssignFn 72 (TypedVar (VName "a") VAuto Nothing Nothing) VBlank (TypedABinary VInt Subtract (TypedVar (VName "a") VInt Nothing Nothing) (IntConst 77 5)),AssignFn 81 (TypedVar (VName "a") VAuto Nothing Nothing) VBlank (TypedABinary VInt Multiply (TypedVar (VName "a") VInt Nothing Nothing) (IntConst 86 2)),AssignFn 90 (TypedVar (VName "a") VAuto Nothing Nothing) VBlank (TypedABinary VInt Divide (TypedVar (VName "a") VInt Nothing Nothing) (IntConst 95 2))]])]
  let expectedOUT = ["namespace assignOperatorsExample{\n","void Main();\n","void Main(){\n","   int a = 0;\n","    a = a + 1;\n","    a = a - 5;\n","    a = a * 2;\n","    a = a / 2;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

