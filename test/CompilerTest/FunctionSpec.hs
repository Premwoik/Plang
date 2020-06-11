module CompilerTest.FunctionSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec


path = "res/tests/function/"

main =
  describe "Function" $ do
    context "with arguments behave as " $ do
      it "can parser generic" $ pending
      it "can parser copy" $ pending
      it "can't modify fun argument" testFunArgCantBeModified
    context "with body behave as" $ do
      it "can access global and arg vars" testGlobalAndArgVars
      it "can invoke class method" testInvokeClassMethod
      it "can invoke gen class method" testInvokeGenClassMethod
      it "can invoke gen (gen is class) class method" testInvokeGen2ClassMethod

--      it "can parser const" testConstFunArg
testInvokeGen2ClassMethod = do
  (ast, out) <- compile path "invokeGenClassMethodExample2"
  let expectedAST = [IFile "invokeGenClassMethodExample2" "res/tests/function/Main.mard" (AST [ClassExpr 0 "Obj" ["T"] [ClassAssign 18 (ScopeMark 18 "this" (TypedVar (VName "this___i") VAuto Nothing Nothing)) (VGen "T") Nop,Constructor 25 "Obj" [FunArg (VGen "T") "args___i"] [AssignFn 41 (ScopeMark 41 "this" (TypedVar (VName "this___i") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___i") (VClass (VName "T") []) Nothing Nothing)],Method 55 "getConst" (VGen "T") [] [ReturnFn 76 (Just (TypedVar (VName "this___i") (VGen "T") Nothing Nothing))]],ClassExpr 83 "Item" ["T"] [Constructor 102 "Item" [] [Pass],Method 122 "get" VInt [] [ReturnFn 140 (Just (IntConst 144 0))]],Function 147 "testFunc" VVoid [] [AssignFn 169 (TypedVar (VName "o") VAuto Nothing Nothing) (VClass (VName "Obj") [VGenPair "T" (VPointer (VClass (VName "Item") [VInt]) SharedPtr)]) (TypedVar (VName "Obj") (VClass (VName "Obj") [VGenPair "T" (VPointer (VClass (VName "Item") [VInt]) SharedPtr)]) (Just [TypedVar (VName "Item") (VClass (VName "Item") [VInt]) (Just []) Nothing]) Nothing),OtherFn 203 (TypedVar (VName "o") (VClass (VName "Obj") [VGenPair "T" (VPointer (VClass (VName "Item") [VInt]) SharedPtr)]) Nothing (Just (TypedVar (VName "getConst") (VPointer (VClass (VName "Item") [VInt]) SharedPtr) (Just []) Nothing))),Pass]])]

  let expectedOUT = ["namespace invokeGenClassMethodExample2{\n","void testFunc();\n","template<typename T>\nclass Obj{\npublic:\n","   T this___i;\n","   Obj(T args___i){\n","       this___i = args___i;\n","   }\n","   T getConst(){\n","      return this___i;\n","   }\n","};\n","template<typename T>\nclass Item{\npublic:\n","   Item(){\n","      ","   }\n","   int get(){\n","      return 0;\n","   }\n","};\n","void testFunc(){\n","   Obj<shared_ptr<Item<int>>> o = Obj<shared_ptr<Item<int>>>(Item<int>());\n","   o.getConst()","   ;\n","   ","}\n","}\n"]


  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testInvokeGenClassMethod = do
  (ast, out) <- compile path "invokeGenClassMethodExample"
  let expectedAST = [IFile "invokeGenClassMethodExample" "res/tests/function/Main.mard" (AST [ClassExpr 0 "Obj" ["T"] [ClassAssign 18 (ScopeMark 18 "this" (TypedVar (VName "this___i") VAuto Nothing Nothing)) (VGen "T") Nop,Constructor 25 "Obj" [FunArg (VGen "T") "args___i"] [AssignFn 41 (ScopeMark 41 "this" (TypedVar (VName "this___i") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___i") (VClass (VName "T") []) Nothing Nothing)],Method 55 "getConst" (VGen "T") [] [ReturnFn 76 (Just (TypedVar (VName "this___i") (VGen "T") Nothing Nothing))]],Function 83 "testFunc" VVoid [FunArg (VClass (VName "Obj") []) "args___a"] [AssignFn 112 (TypedVar (VName "o") VAuto Nothing Nothing) (VClass (VName "Obj") [VGenPair "T" VInt]) (TypedVar (VName "Obj") (VClass (VName "Obj") [VGenPair "T" VInt]) (Just [IntConst 125 1]) Nothing),OtherFn 130 (TypedVar (VName "o") (VClass (VName "Obj") [VGenPair "T" VInt]) Nothing (Just (TypedVar (VName "getConst") VInt (Just []) Nothing))),Pass]])]

  let expectedOUT =  ["namespace invokeGenClassMethodExample{\n","void testFunc(Obj& args___a);\n","template<typename T>\nclass Obj{\npublic:\n","   T this___i;\n","   Obj(T args___i){\n","       this___i = args___i;\n","   }\n","   T getConst(){\n","      return this___i;\n","   }\n","};\n","void testFunc(Obj& args___a){\n","   Obj<int> o = Obj<int>(1);\n","   o.getConst()","   ;\n","   ","}\n","}\n"]


  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testInvokeClassMethod = do
  (ast, out) <- compile path "invokeClassMethodExample"
  let expectedAST = [IFile "invokeClassMethodExample" "res/tests/function/Main.mard" (AST [ClassExpr 0 "Obj" [] [Constructor 16 "Obj" [] [Pass],Method 35 "getConst" VInt [] [ReturnFn 58 (Just (IntConst 62 1))]],Function 65 "testFunc" VVoid [FunArg (VClass (VName "Obj") []) "args___a"] [AssignFn 94 (TypedVar (VName "o") VAuto Nothing Nothing) (VClass (VName "Obj") []) (TypedVar (VName "Obj") (VClass (VName "Obj") []) (Just []) Nothing),OtherFn 106 (TypedVar (VName "o") (VClass (VName "Obj") []) Nothing (Just (TypedVar (VName "getConst") VInt (Just []) Nothing))),Pass]])]

  let expectedOUT = ["namespace invokeClassMethodExample{\n","void testFunc(Obj& args___a);\n","class Obj{\npublic:\n","   Obj(){\n","      ","   }\n","   int getConst(){\n","      return 1;\n","   }\n","};\n","void testFunc(Obj& args___a){\n","   Obj o = Obj();\n","   o.getConst()","   ;\n","   ","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testFunArgCantBeModified = pending

testGlobalAndArgVars = do
  (ast, out) <- compile path "funArgsAndGlobalVarExample"
  let expectedAST = [IFile "funArgsAndGlobalVarExample" "res/tests/function/Main.mard" (AST [Assign 0 (TypedVar (VName "g___i") VAuto Nothing Nothing) VInt (IntConst 9 20),Assign 12 (TypedVar (VName "g___g") VAuto Nothing Nothing) VInt (IntConst 16 10),Function 20 "testFunc" VInt [FunArg VInt "args___b",FunArg VInt "args___g"] [AssignFn 57 (TypedVar (VName "i") VAuto Nothing Nothing) VInt (TypedABinary VInt Add (TypedVar (VName "g___i") VInt Nothing Nothing) (IntConst 65 10)),AssignFn 70 (TypedVar (VName "g") VAuto Nothing Nothing) VInt (TypedABinary VInt Add (TypedABinary VInt Add (TypedVar (VName "args___g") VInt Nothing Nothing) (ScopeMark 78 "g" (TypedVar (VName "g___g") VInt Nothing Nothing))) (IntConst 84 10)),AssignFn 125 (ScopeMark 125 "g" (TypedVar (VName "g___i") VInt Nothing Nothing)) VBlank (ScopeMark 131 "args" (TypedVar (VName "args___g") VInt Nothing Nothing)),ReturnFn 140 (Just (TypedABinary VInt Add (TypedVar (VName "i") VInt Nothing Nothing) (TypedVar (VName "g") VInt Nothing Nothing)))]])]

  let expectedOUT = ["namespace funArgsAndGlobalVarExample{\n","int testFunc(int args___b, int args___g);\n","int g___i = 20;\n","int g___g = 10;\n","int testFunc(int args___b, int args___g){\n","   int i = g___i + 10;\n","   int g = args___g + g___g + 10;\n","    g___i = args___g;\n","   return i + g;\n","}\n","}\n"]


  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testConstFunArg = do
  (ast, out) <- compile path "constFunArgExample"
  let expectedAST = []
  let expectedOUT = []

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT
