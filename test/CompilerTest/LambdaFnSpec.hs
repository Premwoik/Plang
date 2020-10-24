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
      it "can capture lambda be passed to native function in args" canPassedCaptureLambdaToNativeFunctionTest
      it "can be apply in place" applyLambdaInPlaceTest
    context "with body block" $ do
      it "can be declared" canBeBlockDeclaredTest
      it "can compile void return " voidLambdaTest
      it "can compile class capture lambda" classCapturedLambdaTest

voidLambdaTest = do
  (ast, out) <- compile path "voidLambdaExample"
  let expectedAST = [IFile "voidLambdaExample" "res/tests/lambdaFn/voidLambdaExample.mard" (AST [Skip 0,Function 33 "testFunc" [] VVoid [] [AssignFn 55 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt,VVoid] CMOff) (LambdaFn 89 CMOff VVoid [FunArg VInt "args___x",FunArg VInt "args___z"] [AssignFn 105 (TypedVar (VName "y") VAuto Nothing Nothing) VInt (TypedABinary VInt Add (TypedVar (VName "args___x") VInt Nothing Nothing) (IntConst 113 10)),AssignFn 123 (TypedVar (VName "y") VAuto Nothing Nothing) VBlank (TypedABinary VInt Add (TypedVar (VName "y") VInt Nothing Nothing) (IntConst 131 13))]),Pass]])]
  let expectedOUT = ["namespace voidLambdaExample{\n","void testFunc();\n","void testFunc(){\n","   void(*lambda)(int,int) = [](int args___x,int args___z){   int y = args___x + 10;\n    y = y + 13;\n};\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

applyLambdaInPlaceTest = do
  (ast, out) <- compile path "applyLambdaInPlaceExample"
  let expectedAST = [IFile "applyLambdaInPlaceExample" "res/tests/lambdaFn/applyLambdaInPlaceExample.mard" (AST [Skip 0,Function 35 "testFunc" [] VVoid [] [AssignFn 57 (TypedVar (VName "result") VAuto Nothing Nothing) VInt (ABracketApply 66 (LambdaFn 67 CMOff VInt [FunArg VInt "args___x"] [OtherFn 73 (TypedABinary VInt Add (TypedVar (VName "args___x") VInt Nothing Nothing) (IntConst 77 10))]) [IntConst 81 11]),Pass]])]

  let expectedOUT = ["namespace applyLambdaInPlaceExample{\n","void testFunc();\n","void testFunc(){\n","   int result = ([](int args___x){return args___x + 10;\n})(11);\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canPassedCaptureLambdaToFunctionTest = do
  (ast, out) <- compile path "simpleCaptureLambdaPassExample"
  let expectedAST = [IFile "simpleCaptureLambdaPassExample" "res/tests/lambdaFn/simpleCaptureLambdaPassExample.mard" (AST [Skip 0,Function 33 "consumeFn" [] VVoid [FunArg (VFn [VInt,VInt] CMAuto) "args___fn"] [Pass],Function 79 "testFunc" [] VVoid [] [AssignFn 101 (TypedVar (VName "varToCapture") VAuto Nothing Nothing) VInt (IntConst 116 12),AssignFn 121 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt] CMOn) (LambdaFn 143 CMOn VInt [FunArg VInt "args___x"] [OtherFn 149 (TypedABinary VInt Add (TypedABinary VInt Add (TypedVar (VName "varToCapture") VInt Nothing Nothing) (TypedVar (VName "args___x") VInt Nothing Nothing)) (IntConst 168 10))]),OtherFn 173 (TypedVar (VName "consumeFn") VVoid (Just [TypedVar (VName "lambda") (VFn [VInt,VInt] CMOn) Nothing Nothing]) Nothing)]])]
  let expectedOUT = ["namespace simpleCaptureLambdaPassExample{\n","void consumeFn(nonstd::function<int(int)> args___fn);\n","void testFunc();\n","void consumeFn(nonstd::function<int(int)> args___fn){\n","}\n","void testFunc(){\n","   int varToCapture = 12;\n","   auto lambda = [=](int args___x){return varToCapture + args___x + 10;\n};\n","   consumeFn(lambda)","   ;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canPassedCaptureLambdaToNativeFunctionTest = do
  (ast, out) <- compile path "simpleCaptureLambdaPassNativeExample"
  let expectedAST = [IFile "simpleCaptureLambdaPassNativeExample" "res/tests/lambdaFn/simpleCaptureLambdaPassNativeExample.mard" (AST [Skip 0,NativeFunction 34 "" "attachInterrupt" [] VVoid [FunArg VInt "args___id",FunArg (VFn [VVoid] CMAuto) "args___fun",FunArg VInt "args___mode"],NativeFunction 102 "" "detachInterrupt" [] VVoid [FunArg VInt "args___id"],NativeFunction 144 "" "digitalPinToInterrupt" [] VInt [FunArg VInt "args___id"],NativeAssignDeclaration 192 "INPUT_PULLUP" "PULLUP" VInt,Function 231 "testFunc" [] VVoid [] [AssignFn 253 (TypedVar (VName "varToCapture") VAuto Nothing Nothing) VInt (IntConst 268 12),AssignFn 273 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VVoid] CMOff) (LambdaFn 297 CMOff VVoid [] []),OtherFn 310 (TypedVar (VName "attachInterrupt") VVoid (Just [TypedVar (VName "digitalPinToInterrupt") VInt (Just [IntConst 348 2]) Nothing,TypedVar (VName "lambda") (VFn [VVoid] CMOff) Nothing Nothing,TypedVar (VNameNative "PULLUP" "INPUT_PULLUP") VInt Nothing Nothing]) Nothing)]])]

  let expectedOUT = ["namespace simpleCaptureLambdaPassNativeExample{\n","void testFunc();\n","void testFunc(){\n","   int varToCapture = 12;\n","   void(*lambda)() = [](){};\n","   attachInterrupt(digitalPinToInterrupt(2), lambda, INPUT_PULLUP)","   ;\n","}\n","}\n"]

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
  let expectedAST = [IFile "simpleBlockLambdaExample" "res/tests/lambdaFn/simpleBlockLambdaExample.mard" (AST [Skip 0,Function 33 "testFunc" [] VVoid [] [AssignFn 55 (TypedVar (VName "lambda") VAuto Nothing Nothing) (VFn [VInt,VInt,VInt] CMOff) (LambdaFn 88 CMOff VInt [FunArg VInt "args___x",FunArg VInt "args___z"] [AssignFn 104 (TypedVar (VName "y") VAuto Nothing Nothing) VInt (TypedABinary VInt Add (TypedVar (VName "args___x") VInt Nothing Nothing) (IntConst 112 10)),AssignFn 122 (TypedVar (VName "y") VAuto Nothing Nothing) VBlank (TypedABinary VInt Add (TypedVar (VName "y") VInt Nothing Nothing) (IntConst 130 13)),ReturnFn 140 (Just (IntConst 144 0))]),Pass]])]
  let expectedOUT = ["namespace simpleBlockLambdaExample{\n","void testFunc();\n","void testFunc(){\n","   int(*lambda)(int,int) = [](int args___x,int args___z){   int y = args___x + 10;\n    y = y + 13;\n   return 0;\n};\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

classCapturedLambdaTest = do
  (ast, out) <- compile path "classCaptureLambdaExample"
  let expectedAST = [IFile "classCaptureLambdaExample" "res/tests/lambdaFn/classCaptureLambdaExample.mard" (AST [Skip 0,Function 40 "millis" [] VInt [] [ReturnFn 59 (Just (IntConst 63 11))],ClassExpr 67 "EnergyMeter" [] [] [ClassAssign {classAssignOffset = 90, classAssignLeft = ScopeMark 90 "this" (TypedVar (VName "this___counter") VAuto Nothing Nothing), classAssignType = VInt, classAssignDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False, parentNameMD = ""}, classAssignRight = IntConst 100 0},ClassAssign {classAssignOffset = 104, classAssignLeft = ScopeMark 104 "this" (TypedVar (VName "this___pin") VAuto Nothing Nothing), classAssignType = VInt, classAssignDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False, parentNameMD = ""}, classAssignRight = IntConst 110 0},ClassAssign {classAssignOffset = 125, classAssignLeft = ScopeMark 125 "this" (TypedVar (VName "this___lastRead") VAuto Nothing Nothing), classAssignType = VInt, classAssignDetails = MethodDetails {visibilityMD = "private", isOverrideMD = False, parentNameMD = ""}, classAssignRight = IntConst 136 0},Constructor {constructorOffset = 141, constructorName = "EnergyMeter", constructorArgs = [], constructorBody = [Pass]},Constructor {constructorOffset = 168, constructorName = "EnergyMeter", constructorArgs = [FunArg VInt "args___pin"], constructorBody = [AssignFn 196 (ScopeMark 196 "this" (TypedVar (VName "this___pin") VInt Nothing Nothing)) VBlank (TypedVar (VName "args___pin") VInt Nothing Nothing),AssignFn 216 (TypedVar (VName "listener") VAuto Nothing Nothing) (VFn [VVoid] CMOn) (LambdaFn 244 CMOn VVoid [] [AssignFn 257 (TypedVar (VName "currentRead") VAuto Nothing Nothing) VInt (TypedVar (VName "millis") VInt (Just []) Nothing),IfFn 288 [(BoolVar (ABool (RBinary Greater (TypedABinary VInt Subtract (TypedVar (VName "currentRead") VInt Nothing Nothing) (ScopeMark 305 "this" (TypedVar (VName "this___lastRead") VInt Nothing Nothing))) (IntConst 321 10))),[AssignFn 339 (ScopeMark 339 "this" (TypedVar (VName "this___counter") VInt Nothing Nothing)) VBlank (TypedABinary VInt Add (ScopeMark 339 "this" (TypedVar (VName "this___counter") VInt Nothing Nothing)) (IntConst 355 1)),AssignFn 367 (ScopeMark 367 "this" (TypedVar (VName "this___lastRead") VInt Nothing Nothing)) VBlank (TypedVar (VName "currentRead") VInt Nothing Nothing)])]])]},Method {methodOffset = 501, methodName = "clear", methodRet = VVoid, methodArgs = [], methodDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False, parentNameMD = "EnergyMeter"}, methodBody = [Pass]}]])]

  let expectedOUT = ["namespace classCaptureLambdaExample{\n","int millis();\n","class EnergyMeter;\n","int millis(){\n","   return 11;\n","}\n","class EnergyMeter{\n","   public:\nint this___counter = 0;\n","   public:\nint this___pin = 0;\n","   private:\nint this___lastRead = 0;\n","   public:\n","   EnergyMeter(){\n","   }\n","   public:\n","   EnergyMeter(int args___pin){\n","       this___pin = args___pin;\n","      auto listener = [=](){   int currentRead = millis();\n   if(currentRead - this___lastRead > 10){\n    this___counter = this___counter + 1;\n    this___lastRead = currentRead;\n}\n};\n","   }\n","   public:\n","   void clear() {\n","   }\n","};\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT
