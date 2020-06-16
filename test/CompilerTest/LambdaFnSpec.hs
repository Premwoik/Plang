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
      it "can capture version be declared" canBeCaptureDeclaredTest
      it "is global var not classified as capture" simpleLambdaGlobalVarTest
      it "can lambda be passed to function in args" canPassedLambdaToFunctionTest
      it "can capture lambda be passed to function in args" canPassedCaptureLambdaToFunctionTest
      it "can be invoked" pending
    context "with body block" $ do
      it "can be declared" canBeBlockDeclaredTest
      it "can be invoked" pending


canPassedCaptureLambdaToFunctionTest = do
  (ast, out) <- compile path "simpleCaptureLambdaPassExample"
  let expectedAST = [IFile "simpleCaptureLambdaPassExample" "res/tests/lambdaFn/simpleCaptureLambdaPassExample.mard" (AST [Skip 0,Function 33 "consumeFn" [] VVoid [FunArg (VFn [VInt,VInt] CMAuto) "args___fn"] [Pass],Function 79 "testFunc" [] VVoid [] [AssignFn 101 (TypedVar (VName "varToCapture") VAuto Nothing Nothing) VInt (IntConst 116 12),AssignFn 121 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt] CMOn) (LambdaFn 143 CMOn VInt [FunArg VInt "args___x"] [OtherFn 149 (TypedABinary VInt Add (TypedABinary VInt Add (TypedVar (VName "varToCapture") VInt Nothing Nothing) (TypedVar (VName "args___x") VInt Nothing Nothing)) (IntConst 168 10))]),OtherFn 173 (TypedVar (VName "consumeFn") VVoid (Just [TypedVar (VName "lambda") (VFn [VInt,VInt] CMOn) Nothing Nothing]) Nothing)]])]
  let expectedOUT = ["namespace simpleCaptureLambdaPassExample{\n","void consumeFn(nonstd::function<int(int)> args___fn);\n","void testFunc();\n","void consumeFn(nonstd::function<int(int)> args___fn){\n","}\n","void testFunc(){\n","   int varToCapture = 12;\n","   auto lambda = [=](int args___x){return varToCapture + args___x + 10;\n};\n","   consumeFn(lambda)","   ;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canPassedLambdaToFunctionTest = do
  (ast, out) <- compile path "simpleLambdaExample2"
  let expectedAST = [IFile "simpleLambdaExample2" "res/tests/lambdaFn/simpleLambdaExample2.mard" (AST [Skip 0,Function 33 "consumeFn" [] VVoid [FunArg (VFn [VInt,VInt] CMAuto) "args___fn"] [Pass],Function 79 "testFunc" [] VVoid [] [AssignFn 101 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt] CMOff) (LambdaFn 123 CMOff VInt [FunArg VInt "args___x"] [OtherFn 129 (TypedABinary VInt Add (TypedVar (VName "args___x") VInt Nothing Nothing) (IntConst 133 10))]),OtherFn 138 (TypedVar (VName "consumeFn") VVoid (Just [TypedVar (VName "lambda") (VFn [VInt,VInt] CMOff) Nothing Nothing]) Nothing)]])]
  let expectedOUT = ["namespace simpleLambdaExample2{\n","void consumeFn(nonstd::function<int(int)> args___fn);\n","void testFunc();\n","void consumeFn(nonstd::function<int(int)> args___fn){\n","}\n","void testFunc(){\n","   int(*lambda)(int) = [](int args___x){return args___x + 10;\n};\n","   consumeFn(lambda)","   ;\n","}\n","}\n"]


  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT


canBeDeclaredTest = do
  (ast, out) <- compile path "simpleLambdaExample"
  let expectedAST =[IFile "simpleLambdaExample" "res/tests/lambdaFn/simpleLambdaExample.mard" (AST [Skip 0,Function 33 "testFunc" [] VVoid [] [AssignFn 55 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt] CMAuto) (LambdaFn 77 CMOff VInt [FunArg VInt "args___x"] [OtherFn 83 (TypedABinary VInt Add (TypedVar (VName "args___x") VInt Nothing Nothing) (IntConst 87 10))]),Pass]])]
  let expectedOUT = ["namespace simpleLambdaExample{\n","void testFunc();\n","void testFunc(){\n","   int(*lambda)(int) = [](int args___x){return args___x + 10;\n};\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canBeCaptureDeclaredTest = do
  (ast, out) <- compile path "simpleLambdaCaptureExample"
  let expectedAST = [IFile "simpleLambdaCaptureExample" "res/tests/lambdaFn/simpleLambdaCaptureExample.mard" (AST [Skip 0,Function 33 "testFunc" [] VVoid [] [AssignFn 55 (TypedVar (VName "a") VAuto Nothing Nothing) VInt (IntConst 59 12),AssignFn 64 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt] CMOn) (LambdaFn 86 CMOn VInt [FunArg VInt "args___x"] [OtherFn 92 (TypedABinary VInt Add (TypedABinary VInt Add (TypedVar (VName "args___x") VInt Nothing Nothing) (TypedVar (VName "a") VInt Nothing Nothing)) (IntConst 100 10))]),Pass]])]
  let expectedOUT = ["namespace simpleLambdaCaptureExample{\n","void testFunc();\n","void testFunc(){\n","   int a = 12;\n","   auto lambda = [=](int args___x){return args___x + a + 10;\n};\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

simpleLambdaGlobalVarTest = do
  (ast, out) <- compile path "simpleLambdaGlobalVarExample"
  let expectedAST = [IFile "simpleLambdaGlobalVarExample" "res/tests/lambdaFn/simpleLambdaGlobalVarExample.mard" (AST [Skip 0,Assign 42 (TypedVar (VName "g___k") VAuto Nothing Nothing) VInt (IntConst 46 3),Function 48 "testFunc" [] VVoid [] [AssignFn 70 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt] CMOff) (LambdaFn 92 CMOff VInt [FunArg VInt "args___x"] [OtherFn 98 (TypedABinary VInt Add (TypedVar (VName "args___x") VInt Nothing Nothing) (TypedABinary VInt Multiply (IntConst 102 10) (TypedVar (VName "g___k") VInt Nothing Nothing)))]),Pass]])]

  let expectedOUT = ["namespace simpleLambdaGlobalVarExample{\n","void testFunc();\n","int g___k = 3;\n","void testFunc(){\n","   int(*lambda)(int) = [](int args___x){return args___x + 10 * g___k;\n};\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT


canBeInvokedTest = pending

canBeBlockDeclaredTest = do
  (ast, out) <- compile path "simpleBlockLambdaExample"
  let expectedAST = [IFile "simpleBlockLambdaExample" "res/tests/lambdaFn/simpleBlockLambdaExample.mard" (AST [Skip 0,Function 33 "testFunc" [] VVoid [] [AssignFn 55 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt,VInt] CMAuto) (LambdaFn 88 CMOff VInt [FunArg VInt "args___x",FunArg VInt "args___z"] [AssignFn 104 (TypedVar (VName "y") VAuto Nothing Nothing) VInt (TypedABinary VInt Add (TypedVar (VName "args___x") VInt Nothing Nothing) (IntConst 112 10)),AssignFn 122 (TypedVar (VName "y") VAuto Nothing Nothing) VBlank (TypedABinary VInt Add (TypedVar (VName "y") VInt Nothing Nothing) (IntConst 130 13))]),Pass]])]
  let expectedOUT = ["namespace simpleBlockLambdaExample{\n","void testFunc();\n","void testFunc(){\n","   int(*lambda)(int,int) = [](int args___x,int args___z){   int y = args___x + 10;\n    y = y + 13;\n};\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT
