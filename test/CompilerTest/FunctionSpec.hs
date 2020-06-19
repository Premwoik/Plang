module CompilerTest.FunctionSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec


example = do
  (ast, out) <- compile path "genericFunExample"
  let expectedAST = []
  let expectedOUT = []

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT


path = "res/tests/function/"

main =
  describe "Function" $ do
    context "with arguments behave as " $ do
      it "can parser generic" pending
      it "can parser copy" pending
      it "can parse generic function" testGenericFunction
      it "can apply generic function" testApplyGenericFunction
      it "can parse generic native function" testGenericNativeFunction
      it "can't modify fun argument" testFunArgCantBeModified
    context "with body behave as" $ do
      it "can return capture fun ptr variable" testReturningCaptureFunPtr
      it "can return fun ptr variable" testReturningFunPtrVariable
      it "can return fun ptr" testReturningFunPtr
      it "can return fun ptr more complex example" testReturningFunPtr2
      it "can return variable" testReturningVariable
      it "can access global and arg vars" testGlobalAndArgVars
      it "can invoke class method" testInvokeClassMethod
      it "can invoke gen class method" testInvokeGenClassMethod
      it "can invoke gen (gen is class) class method" testInvokeGen2ClassMethod

testApplyGenericFunction = do
  (ast, out) <- compile path "genericFunApplyExample"
  let expectedAST = [IFile "genericFunApplyExample" "res/tests/function/genericFunApplyExample.mard" (AST [Function 0 "nfun" ["T"] VInt [FunArg (VGen "T") "args___arg"] [ReturnFn 25 (Just (TypedABinary VInt Add (TypedVar (VName "args___arg") (VGen "T") Nothing Nothing) (IntConst 35 1)))],Function 38 "fun" [] VInt [] [AssignFn 54 (TypedVar (VName "a") VAuto Nothing Nothing) VInt (TypedVar (VName "nfun") VInt (Just [FloatConst 63 10.0]) Nothing),ReturnFn 71 (Just (TypedVar (VName "nfun") VInt (Just [IntConst 80 10]) Nothing))]])]
  let expectedOUT = ["namespace genericFunApplyExample{\n","int nfun(T args___arg);\n","int fun();\n","template<typename T>","int nfun(T args___arg){\n","   return args___arg + 1;\n","}\n","int fun(){\n","   int a = nfun(10.0);\n","   return nfun(10);\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testGenericNativeFunction = do
  (ast, out) <- compile path "nativeFunExample"
  let expectedAST = [IFile "nativeFunExample" "res/tests/function/nativeFunExample.mard" (AST [NativeFunction 0 "" "nfun" ["T"] (VGen "T") [FunArg (VGen "T") "args___arg"],Function 31 "fun" [] VInt [] [AssignFn 47 (TypedVar (VName "a") VAuto Nothing Nothing) VInt (TypedVar (VName "nfun") VInt (Just [IntConst 56 10]) Nothing),ReturnFn 62 (Just (TypedVar (VName "nfun") VInt (Just [IntConst 71 10]) Nothing))]])]
  let expectedOUT = ["namespace nativeFunExample{\n","int fun();\n","int fun(){\n","   int a = nfun(10);\n","   return nfun(10);\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testGenericFunction = do
  (ast, out) <- compile path "genericFunctionExample"
  let expectedAST = [IFile "genericFunctionExample" "res/tests/function/genericFunctionExample.mard" (AST [Function 0 "fun" ["T"] VInt [FunArg (VGen "T") "args___arg"] [ReturnFn 24 (Just (TypedABinary VInt Add (TypedVar (VName "args___arg") (VGen "T") Nothing Nothing) (IntConst 34 12)))]])]
  let expectedOUT = ["namespace genericFunctionExample{\n","int fun(T args___arg);\n","template<typename T>","int fun(T args___arg){\n","   return args___arg + 12;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testReturningCaptureFunPtr = do
  (ast, out) <- compile path "returnFunPtrExample4"
  let expectedAST = [IFile "returnFunPtrExample4" "res/tests/function/returnFunPtrExample4.mard" (AST [Function 0 "fun" [] (VFn [VInt,VInt] CMOn) [FunArg VInt "args___i"] [AssignFn 31 (TypedVar (VName "a") VAuto Nothing Nothing) VInt (IntConst 35 13),ReturnFn 40 (Just (LambdaFn 44 CMOn VInt [FunArg VInt "args___i"] [OtherFn 50 (TypedABinary VInt Add (TypedABinary VInt Add (TypedVar (VName "a") VInt Nothing Nothing) (TypedVar (VName "args___i") VInt Nothing Nothing)) (IntConst 58 12))]))]])]
  let expectedOUT = ["namespace returnFunPtrExample4{\n","auto fun(int args___i);\n","auto fun(int args___i){\n","   int a = 13;\n","   return [=](int args___i){return a + args___i + 12;\n};\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testReturningFunPtr2 = do
  (ast, out) <- compile path "returnFunPtrExample3"
  let expectedAST = [IFile "returnFunPtrExample3" "res/tests/function/returnFunPtrExample3.mard" (AST [Function 0 "fun" [] (VFn [VInt,VInt] CMOff) [FunArg VInt "args___i"] [ReturnFn 31 (Just (LambdaFn 35 CMOff VInt [FunArg VInt "args___i"] [OtherFn 41 (TypedABinary VInt Add (TypedVar (VName "args___i") VInt Nothing Nothing) (IntConst 45 12))]))]])]
  let expectedOUT = ["namespace returnFunPtrExample3{\n","int(*fun(int args___i))(int);\n","int(*fun(int args___i))(int){\n","   return [](int args___i){return args___i + 12;\n};\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testReturningFunPtr = do
  (ast, out) <- compile path "returnFunPtrExample2"
  let expectedAST = [IFile "returnFunPtrExample2" "res/tests/function/returnFunPtrExample2.mard" (AST [Function 0 "fun" [] (VFn [VInt] CMAuto) [] [ReturnFn 20 (Just (LambdaFn 24 CMOff VInt [] [OtherFn 29 (IntConst 29 12)]))]])]
  let expectedOUT = ["namespace returnFunPtrExample2{\n","int(*fun())();\n","int(*fun())(){\n","   return [](){return 12;\n};\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testReturningFunPtrVariable = do
  (ast, out) <- compile path "returnFunPtrExample"
  let expectedAST = [IFile "returnFunPtrExample" "res/tests/function/returnFunPtrExample.mard" (AST [Function 0 "fun" [] (VFn [VInt] CMAuto) [] [AssignFn 20 (TypedVar (VName "a") VAuto Nothing Nothing) (VFn [VInt] CMAuto) (LambdaFn 33 CMOff VInt [] [OtherFn 38 (IntConst 38 12)]),ReturnFn 43 (Just (TypedVar (VName "a") (VFn [VInt] CMAuto) Nothing Nothing))]])]
  let expectedOUT = ["namespace returnFunPtrExample{\n","int(*fun())();\n","int(*fun())(){\n","   int(*a)() = [](){return 12;\n};\n","   return a;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testReturningVariable = do
  (ast, out) <- compile path "returnExample"
  let expectedAST = [IFile "returnExample" "res/tests/function/returnExample.mard" (AST [Function 0 "fun" [] VInt [] [AssignFn 16 (TypedVar (VName "a") VAuto Nothing Nothing) VInt (IntConst 20 12),ReturnFn 25 (Just (TypedVar (VName "a") VInt Nothing Nothing))]])]
  let expectedOUT = ["namespace returnExample{\n","int fun();\n","int fun(){\n","   int a = 12;\n","   return a;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testInvokeGen2ClassMethod = do
  (ast, out) <- compile path "invokeGenClassMethodExample2"
  let expectedAST = [IFile "invokeGenClassMethodExample2" "res/tests/function/invokeGenClassMethodExample2.mard" (AST [ClassExpr 0 "Obj" ["T"] [] [ClassAssign {classAssignOffset = 18, classAssignLeft = ScopeMark 18 "this" (TypedVar (VName "this___i") VAuto Nothing Nothing), classAssignType = VGen "T", classAssignDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False}, classAssignRight = Nop},Constructor {constructorOffset = 25, constructorName = "Obj", constructorArgs = [FunArg (VGen "T") "args___i"], constructorBody = [AssignFn 41 (ScopeMark 41 "this" (TypedVar (VName "this___i") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___i") (VGen "T") Nothing Nothing)]},Method {methodOffset = 55, methodName = "getConst", methodRet = VGen "T", methodArgs = [], methodDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False}, methodBody = [ReturnFn 76 (Just (TypedVar (VName "this___i") (VGen "T") Nothing Nothing))]}],ClassExpr 83 "Item" ["T"] [] [Constructor {constructorOffset = 102, constructorName = "Item", constructorArgs = [], constructorBody = [Pass]},Method {methodOffset = 122, methodName = "get", methodRet = VInt, methodArgs = [], methodDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False}, methodBody = [ReturnFn 140 (Just (IntConst 144 0))]}],Function 147 "testFunc" [] VVoid [] [AssignFn 169 (TypedVar (VName "o") VAuto Nothing Nothing) (VClass (VName "Obj") [VGenPair "T" (VPointer (VClass (VName "Item") [VInt]) SharedPtr)]) (TypedVar (VName "Obj") (VClass (VName "Obj") [VGenPair "T" (VPointer (VClass (VName "Item") [VInt]) SharedPtr)]) (Just [TypedVar (VName "Item") (VClass (VName "Item") [VInt]) (Just []) Nothing]) Nothing),OtherFn 203 (TypedVar (VName "o") (VClass (VName "Obj") [VGenPair "T" (VPointer (VClass (VName "Item") [VInt]) SharedPtr)]) Nothing (Just (TypedVar (VName "getConst") (VPointer (VClass (VName "Item") [VInt]) SharedPtr) (Just []) Nothing))),Pass]])]
  let expectedOUT = ["namespace invokeGenClassMethodExample2{\n","void testFunc();\n","template<typename T>","class Obj{\n","   public:\nT this___i;\n","   public:\n","   Obj(T args___i){\n","       this___i = args___i;\n","   }\n","   public:\n","   T getConst() {\n","      return this___i;\n","   }\n","};\n","template<typename T>","class Item{\n","   public:\n","   Item(){\n","   }\n","   public:\n","   int get() {\n","      return 0;\n","   }\n","};\n","void testFunc(){\n","   Obj<shared_ptr<Item<int>>> o = Obj<shared_ptr<Item<int>>>(Item<int>());\n","   o.getConst()","   ;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testInvokeGenClassMethod = do
  (ast, out) <- compile path "invokeGenClassMethodExample"
  let expectedAST = [IFile "invokeGenClassMethodExample" "res/tests/function/invokeGenClassMethodExample.mard" (AST [ClassExpr 0 "Obj" ["T"] [] [ClassAssign {classAssignOffset = 18, classAssignLeft = ScopeMark 18 "this" (TypedVar (VName "this___i") VAuto Nothing Nothing), classAssignType = VGen "T", classAssignDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False}, classAssignRight = Nop},Constructor {constructorOffset = 25, constructorName = "Obj", constructorArgs = [FunArg (VGen "T") "args___i"], constructorBody = [AssignFn 41 (ScopeMark 41 "this" (TypedVar (VName "this___i") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___i") (VGen "T") Nothing Nothing)]},Method {methodOffset = 55, methodName = "getConst", methodRet = VGen "T", methodArgs = [], methodDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False}, methodBody = [ReturnFn 76 (Just (TypedVar (VName "this___i") (VGen "T") Nothing Nothing))]}],Function 83 "testFunc" [] VVoid [FunArg (VClass (VName "Obj") []) "args___a"] [AssignFn 112 (TypedVar (VName "o") VAuto Nothing Nothing) (VClass (VName "Obj") [VGenPair "T" VInt]) (TypedVar (VName "Obj") (VClass (VName "Obj") [VGenPair "T" VInt]) (Just [IntConst 125 1]) Nothing),OtherFn 130 (TypedVar (VName "o") (VClass (VName "Obj") [VGenPair "T" VInt]) Nothing (Just (TypedVar (VName "getConst") VInt (Just []) Nothing))),Pass]])]
  let expectedOUT = ["namespace invokeGenClassMethodExample{\n","void testFunc(Obj& args___a);\n","template<typename T>","class Obj{\n","   public:\nT this___i;\n","   public:\n","   Obj(T args___i){\n","       this___i = args___i;\n","   }\n","   public:\n","   T getConst() {\n","      return this___i;\n","   }\n","};\n","void testFunc(Obj& args___a){\n","   Obj<int> o = Obj<int>(1);\n","   o.getConst()","   ;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testInvokeClassMethod = do
  (ast, out) <- compile path "invokeClassMethodExample"
  let expectedAST = [IFile "invokeClassMethodExample" "res/tests/function/invokeClassMethodExample.mard" (AST [ClassExpr 0 "Obj" [] [] [Constructor 16 "Obj" [] [Pass],Method 35 "getConst" VInt [] (MethodDetails {visibilityMD = "public", isOverrideMD = False}) [ReturnFn 58 (Just (IntConst 62 1))]],Function 65 "testFunc" [] VVoid [FunArg (VClass (VName "Obj") []) "args___a"] [AssignFn 94 (TypedVar (VName "o") VAuto Nothing Nothing) (VClass (VName "Obj") []) (TypedVar (VName "Obj") (VClass (VName "Obj") []) (Just []) Nothing),OtherFn 106 (TypedVar (VName "o") (VClass (VName "Obj") []) Nothing (Just (TypedVar (VName "getConst") VInt (Just []) Nothing))),Pass]])]
  let expectedOUT = ["namespace invokeClassMethodExample{\n","void testFunc(Obj& args___a);\n","class Obj{\n","   public:\n","   Obj(){\n","   }\n","   public:\n","   int getConst() {\n","      return 1;\n","   }\n","};\n","void testFunc(Obj& args___a){\n","   Obj o = Obj();\n","   o.getConst()","   ;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testFunArgCantBeModified = pending

testGlobalAndArgVars = do
  (ast, out) <- compile path "funArgsAndGlobalVarExample"
  let expectedAST = [IFile "funArgsAndGlobalVarExample" "res/tests/function/funArgsAndGlobalVarExample.mard" (AST [Assign 0 (TypedVar (VName "g___i") VAuto Nothing Nothing) VInt (IntConst 9 20),Assign 12 (TypedVar (VName "g___g") VAuto Nothing Nothing) VInt (IntConst 16 10),Function 20 "testFunc" [] VInt [FunArg VInt "args___b",FunArg VInt "args___g"] [AssignFn 57 (TypedVar (VName "i") VAuto Nothing Nothing) VInt (TypedABinary VInt Add (TypedVar (VName "g___i") VInt Nothing Nothing) (IntConst 65 10)),AssignFn 70 (TypedVar (VName "g") VAuto Nothing Nothing) VInt (TypedABinary VInt Add (TypedABinary VInt Add (TypedVar (VName "args___g") VInt Nothing Nothing) (ScopeMark 78 "g" (TypedVar (VName "g___g") VInt Nothing Nothing))) (IntConst 84 10)),AssignFn 125 (ScopeMark 125 "g" (TypedVar (VName "g___i") VInt Nothing Nothing)) VBlank (ScopeMark 131 "args" (TypedVar (VName "args___g") VInt Nothing Nothing)),ReturnFn 140 (Just (TypedABinary VInt Add (TypedVar (VName "i") VInt Nothing Nothing) (TypedVar (VName "g") VInt Nothing Nothing)))]])]
  let expectedOUT = ["namespace funArgsAndGlobalVarExample{\n","int testFunc(int args___b, int args___g);\n","int g___i = 20;\n","int g___g = 10;\n","int testFunc(int args___b, int args___g){\n","   int i = g___i + 10;\n","   int g = args___g + g___g + 10;\n","    g___i = args___g;\n","   return i + g;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testConstFunArg = do
  (ast, out) <- compile path "constFunArgExample"
  let expectedAST = []
  let expectedOUT = []

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT
