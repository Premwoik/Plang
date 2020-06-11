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

path = "res/tests/lambdaFn/"

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
  let expectedAST =[IFile "simpleLambdaExample" "res/tests/lambdaFn/Main.mard" (AST [Skip 0,Function 33 "testFunc" VVoid [] [AssignFn 55 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt]) (LambdaFn 77 VInt [FunArg VInt "args___x"] [OtherFn 83 (TypedABinary VInt Add (TypedVar (VName "args___x") VInt Nothing Nothing) (IntConst 87 10))]),Pass]])]
  let expectedOUT = ["namespace simpleLambdaExample{\n","void testFunc();\n","void testFunc(){\n","   int(*lambda)(int) = [](int args___x){return args___x + 10;\n};\n","   ","}\n","}\n"]



  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canBeInvokedTest = pending

canBeBlockDeclaredTest = do
  (ast, out) <- compile path "simpleBlockLambdaExample"
  let expectedAST = [IFile "simpleBlockLambdaExample" "res/tests/lambdaFn/Main.mard" (AST [Skip 0,Function 33 "testFunc" VVoid [] [AssignFn 55 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt,VInt]) (LambdaFn 88 VInt [FunArg VInt "args___x",FunArg VInt "args___z"] [AssignFn 104 (TypedVar (VName "y") VAuto Nothing Nothing) VInt (TypedABinary VInt Add (TypedVar (VName "args___x") VInt Nothing Nothing) (IntConst 112 10)),AssignFn 122 (TypedVar (VName "y") VAuto Nothing Nothing) VBlank (TypedABinary VInt Add (TypedVar (VName "y") VInt Nothing Nothing) (IntConst 130 13))]),Pass]])]
  let expectedOUT = ["namespace simpleBlockLambdaExample{\n","void testFunc();\n","void testFunc(){\n","   int(*lambda)(int,int) = [](int args___x,int args___z){   int y = args___x + 10;\n    y = y + 13;\n};\n","   ","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT
