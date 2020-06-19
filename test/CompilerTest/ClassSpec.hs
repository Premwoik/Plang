module CompilerTest.ClassSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec

path = "res/tests/class/"

main =
  describe "Class" $ do
    context "with inheritance" $ do
      it "can parse" canParseInheritanceTest
      it "can check types" canCheckTypeTest
      it "can apply methods from parent" canApplyMethodFromParentTest
      it "can apply method from base reference object" canParentReferenceWorksTest
    context "casual" $ do
      it "can parse casual class" canParseClassTest
      it "can parse native class" canParseNativeClassTest
      it "can parse native class with cptr as constructor argument" canParseNativeClassWithCptrTest

canParseNativeClassWithCptrTest = do
  (ast, out) <- compile path "nativeClassWithCptrExample"
  let expectedAST = [IFile "nativeClassWithCptrExample" "res/tests/class/nativeClassWithCptrExample.mard" (AST [ClassExpr 0 "Ethernet" [] [] [Constructor 20 "Ethernet" [] [Pass]],NativeClass 42 "" "MessageProcessor" [] [NativeMethod 80 "MessageProcessor" VAuto [FunArg (VPointer (VClass (VName "Ethernet") []) NativePtr) "client"]],Function 124 "main" [] VInt [] [AssignFn 141 (TypedVar (VName "e") VAuto Nothing Nothing) (VPointer (VClass (VName "Ethernet") []) SharedPtr) (TypedVar (VName "Ethernet") (VPointer (VCopy (VClass (VName "Ethernet") [])) SharedPtr) (Just []) Nothing),AssignFn 172 (TypedVar (VName "proc") VAuto Nothing Nothing) (VClass (VName "MessageProcessor") []) (TypedVar (VName "MessageProcessor") (VClass (VName "MessageProcessor") []) (Just [NativePtrInput (TypedVar (VName "e") (VPointer (VClass (VName "Ethernet") []) SharedPtr) Nothing Nothing)]) Nothing)]])]
  let expectedOUT =  ["namespace nativeClassWithCptrExample{\n","int main();\n","class Ethernet{\npublic:\n","   Ethernet(){\n","   }\n","};\n","int main(){\n","   shared_ptr<Ethernet> e = new Ethernet();\n","   MessageProcessor proc = MessageProcessor(e.getNativePtr());\n","}\n","}\n"]


  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canParseNativeClassTest = do
  (ast, out) <- compile path "nativeClassExample"
  let expectedAST = [IFile "nativeClassExample" "res/tests/class/nativeClassExample.mard" (AST [NativeClass 0 "" "MessageProcessor" [] [NativeMethod 38 "MessageProcessor" VAuto [FunArg VInt "client"]],Function 72 "main" [] VInt [] [AssignFn 89 (TypedVar (VName "proc") VAuto Nothing Nothing) (VClass (VName "MessageProcessor") []) (TypedVar (VName "MessageProcessor") (VClass (VName "MessageProcessor") []) (Just [IntConst 113 12]) Nothing)]])]
  let expectedOUT = ["namespace nativeClassExample{\n","int main();\n","int main(){\n","   MessageProcessor proc = MessageProcessor(12);\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canParseClassTest = do
  (ast, out) <- compile path "classExample"
  let expectedAST = [IFile "classExample" "res/tests/class/classExample.mard" (AST [ClassExpr 0 "ThisTest" ["T"] [] [ClassAssign 24 (ScopeMark 24 "this" (TypedVar (VName "this___a") VAuto Nothing Nothing)) (VGen "T") Nop,ClassAssign 32 (ScopeMark 32 "this" (TypedVar (VName "this___b") VAuto Nothing Nothing)) VInt (IntConst 41 0),Constructor 46 "ThisTest" [] [Pass],Constructor 72 "ThisTest" [FunArg (VGen "T") "args___a"] [AssignFn 95 (ScopeMark 95 "this" (TypedVar (VName "this___a") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___a") (VGen "T") Nothing Nothing),AssignFn 112 (ScopeMark 112 "this" (TypedVar (VName "this___b") VInt Nothing Nothing)) VBlank (IntConst 121 100)],Constructor 128 "ThisTest" [FunArg (VGen "T") "args___a",FunArg VInt "args___b"] [AssignFn 159 (ScopeMark 159 "this" (TypedVar (VName "this___a") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___a") (VGen "T") Nothing Nothing),AssignFn 176 (ScopeMark 176 "this" (TypedVar (VName "this___b") VInt Nothing Nothing)) VBlank (IntConst 185 1)],Method 190 "getA" (VGen "T") [] [ReturnFn 209 (Just (TypedVar (VName "this___a") (VGen "T") Nothing Nothing))],Method 218 "setA" VVoid [FunArg (VGen "T") "args___val"] [AssignFn 247 (ScopeMark 247 "this" (TypedVar (VName "this___a") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___val") (VGen "T") Nothing Nothing)]]])]
  let expectedOUT = ["namespace classExample{\n","template<typename T>","class ThisTest{\npublic:\n","   T this___a;\n","   int this___b = 0;\n","   ThisTest(){\n","   }\n","   ThisTest(T args___a){\n","       this___a = args___a;\n","       this___b = 100;\n","   }\n","   ThisTest(T args___a, int args___b){\n","       this___a = args___a;\n","       this___b = 1;\n","   }\n","   T getA(){\n","      return this___a;\n","   }\n","   void setA(T args___val){\n","       this___a = args___val;\n","   }\n","};\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canParseInheritanceTest = do
  (ast, out) <- compile path "classInheritanceExample"
  let expectedAST = [IFile "classInheritanceExample" "res/tests/class/classInheritanceExample.mard" (AST [ClassExpr 0 "Base" [] [] [Constructor 16 "Base" [] [Pass]],ClassExpr 34 "Interface" ["T"] [] [Constructor 58 "Interface" [] [Pass],NativeMethod 82 "print" VVoid [FunArg VString "arg"]],ClassExpr 113 "ThisTest" ["T"] [VClass (VName "Base") [],VClass (VName "Interface") [VInt]] [ClassAssign 160 (ScopeMark 160 "this" (TypedVar (VName "this___a") VAuto Nothing Nothing)) (VGen "T") Nop,ClassAssign 168 (ScopeMark 168 "this" (TypedVar (VName "this___b") VAuto Nothing Nothing)) VInt (IntConst 177 0),Constructor 182 "ThisTest" [] [Pass],Constructor 208 "ThisTest" [FunArg (VGen "T") "args___a"] [AssignFn 231 (ScopeMark 231 "this" (TypedVar (VName "this___a") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___a") (VGen "T") Nothing Nothing),AssignFn 248 (ScopeMark 248 "this" (TypedVar (VName "this___b") VInt Nothing Nothing)) VBlank (IntConst 257 100)],Constructor 264 "ThisTest" [FunArg (VGen "T") "args___a",FunArg VInt "args___b"] [AssignFn 295 (ScopeMark 295 "this" (TypedVar (VName "this___a") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___a") (VGen "T") Nothing Nothing),AssignFn 312 (ScopeMark 312 "this" (TypedVar (VName "this___b") VInt Nothing Nothing)) VBlank (IntConst 321 1)],Method 326 "getA" (VGen "T") [] [ReturnFn 345 (Just (TypedVar (VName "this___a") (VGen "T") Nothing Nothing))],Method 354 "setA" VVoid [FunArg (VGen "T") "args___val"] [AssignFn 383 (ScopeMark 383 "this" (TypedVar (VName "this___a") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___val") (VGen "T") Nothing Nothing)]]])]
  let expectedOUT = ["namespace classInheritanceExample{\n","class Base{\npublic:\n","   Base(){\n","   }\n","};\n","template<typename T>","class Interface{\npublic:\n","   Interface(){\n","   }\n","   virtual void print(String arg) = 0;\n","};\n","template<typename T>","class ThisTest : public Base,public Interface<int>{\npublic:\n","   T this___a;\n","   int this___b = 0;\n","   ThisTest(){\n","   }\n","   ThisTest(T args___a){\n","       this___a = args___a;\n","       this___b = 100;\n","   }\n","   ThisTest(T args___a, int args___b){\n","       this___a = args___a;\n","       this___b = 1;\n","   }\n","   T getA(){\n","      return this___a;\n","   }\n","   void setA(T args___val){\n","       this___a = args___val;\n","   }\n","};\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canCheckTypeTest = do
  (ast, out) <- compile path "classInheritanceTypeCheckExample"
  let expectedAST = [IFile "classInheritanceTypeCheckExample" "res/tests/class/classInheritanceTypeCheckExample.mard" (AST [ClassExpr 0 "Base" [] [] [Constructor 16 "Base" [] [Pass]],ClassExpr 34 "Interface" ["T"] [] [Constructor 58 "Interface" [] [Pass],NativeMethod 82 "print" VVoid [FunArg VString "arg"]],ClassExpr 113 "ThisTest" ["T"] [VClass (VName "Base") [],VClass (VName "Interface") [VInt]] [ClassAssign 160 (ScopeMark 160 "this" (TypedVar (VName "this___a") VAuto Nothing Nothing)) (VGen "T") Nop,ClassAssign 168 (ScopeMark 168 "this" (TypedVar (VName "this___b") VAuto Nothing Nothing)) VInt (IntConst 177 0),Constructor 182 "ThisTest" [] [Pass],Constructor 208 "ThisTest" [FunArg (VGen "T") "args___a"] [AssignFn 231 (ScopeMark 231 "this" (TypedVar (VName "this___a") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___a") (VGen "T") Nothing Nothing),AssignFn 248 (ScopeMark 248 "this" (TypedVar (VName "this___b") VInt Nothing Nothing)) VBlank (IntConst 257 100)],Constructor 264 "ThisTest" [FunArg (VGen "T") "args___a",FunArg VInt "args___b"] [AssignFn 295 (ScopeMark 295 "this" (TypedVar (VName "this___a") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___a") (VGen "T") Nothing Nothing),AssignFn 312 (ScopeMark 312 "this" (TypedVar (VName "this___b") VInt Nothing Nothing)) VBlank (IntConst 321 1)],Method 326 "getA" (VGen "T") [] [ReturnFn 345 (Just (TypedVar (VName "this___a") (VGen "T") Nothing Nothing))],Method 354 "setA" VVoid [FunArg (VGen "T") "args___val"] [AssignFn 383 (ScopeMark 383 "this" (TypedVar (VName "this___a") (VGen "T") Nothing Nothing)) VBlank (TypedVar (VName "args___val") (VGen "T") Nothing Nothing)]],Function 397 "main" [] VInt [] [AssignFn 414 (TypedVar (VName "a") VAuto Nothing Nothing) (VClass (VName "ThisTest") [VGenPair "T" VInt]) (TypedVar (VName "ThisTest") (VClass (VName "ThisTest") [VGenPair "T" VInt]) (Just [IntConst 432 10]) Nothing),AssignFn 438 (TypedVar (VName "base") VAuto Nothing Nothing) (VRef (VClass (VName "Base") [])) (TypedVar (VName "a") (VRef (VClass (VName "ThisTest") [VGenPair "T" VInt])) Nothing Nothing),AssignFn 459 (TypedVar (VName "interface") VAuto Nothing Nothing) (VRef (VClass (VName "Interface") [VInt])) (TypedVar (VName "a") (VRef (VClass (VName "ThisTest") [VGenPair "T" VInt])) Nothing Nothing)]])]
  let expectedOUT = ["namespace classInheritanceTypeCheckExample{\n","int main();\n","class Base{\npublic:\n","   Base(){\n","   }\n","};\n","template<typename T>","class Interface{\npublic:\n","   Interface(){\n","   }\n","   virtual void print(String arg) = 0;\n","};\n","template<typename T>","class ThisTest : public Base,public Interface<int>{\npublic:\n","   T this___a;\n","   int this___b = 0;\n","   ThisTest(){\n","   }\n","   ThisTest(T args___a){\n","       this___a = args___a;\n","       this___b = 100;\n","   }\n","   ThisTest(T args___a, int args___b){\n","       this___a = args___a;\n","       this___b = 1;\n","   }\n","   T getA(){\n","      return this___a;\n","   }\n","   void setA(T args___val){\n","       this___a = args___val;\n","   }\n","};\n","int main(){\n","   ThisTest<int> a = ThisTest<int>(10);\n","   Base& base = a;\n","   Interface<int>& interface = a;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canApplyMethodFromParentTest = do
  (ast, out) <- compile path "classInheritanceMethodApplicationExample"
  let expectedAST = [IFile "classInheritanceMethodApplicationExample" "res/tests/class/classInheritanceMethodApplicationExample.mard" (AST [ClassExpr 0 "Base" [] [] [Constructor 16 "Base" [] [Pass],Method 35 "print" VVoid [FunArg VString "args___arg"] [Pass]],ClassExpr 73 "Base2" [] [] [Constructor 90 "Base2" [] [Pass],Method 110 "print1" VVoid [FunArg VString "args___arg"] [Pass]],ClassExpr 150 "ThisTest" [] [VClass (VName "Base") [],VClass (VName "Base2") []] [Constructor 185 "ThisTest" [] [Pass]],Function 209 "main" [] VInt [] [AssignFn 226 (TypedVar (VName "a") VAuto Nothing Nothing) (VClass (VName "ThisTest") []) (TypedVar (VName "ThisTest") (VClass (VName "ThisTest") []) (Just []) Nothing),OtherFn 243 (TypedVar (VName "a") (VClass (VName "ThisTest") []) Nothing (Just (TypedVar (VName "print1") VVoid (Just [StringVal 252 "12"]) Nothing))),OtherFn 260 (TypedVar (VName "a") (VClass (VName "ThisTest") []) Nothing (Just (TypedVar (VName "print") VVoid (Just [StringVal 268 "12"]) Nothing)))]])]
  let expectedOUT = ["namespace classInheritanceMethodApplicationExample{\n","int main();\n","class Base{\npublic:\n","   Base(){\n","   }\n","   void print(String args___arg){\n","   }\n","};\n","class Base2{\npublic:\n","   Base2(){\n","   }\n","   void print1(String args___arg){\n","   }\n","};\n","class ThisTest : public Base,public Base2{\npublic:\n","   ThisTest(){\n","   }\n","};\n","int main(){\n","   ThisTest a = ThisTest();\n","   a.print1(\"12\")","   ;\n","   a.print(\"12\")","   ;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canParentReferenceWorksTest = do
  (ast, out) <- compile path "classInheritanceParentRefExample"
  let expectedAST = [IFile "classInheritanceParentRefExample" "res/tests/class/classInheritanceParentRefExample.mard" (AST [ClassExpr 0 "Base" [] [] [Constructor 16 "Base" [] [Pass],Method 35 "print" VVoid [FunArg VString "args___arg"] [Pass]],ClassExpr 73 "Base2" [] [] [Constructor 90 "Base2" [] [Pass],Method 110 "print1" VVoid [FunArg VString "args___arg"] [Pass]],ClassExpr 150 "ThisTest" [] [VClass (VName "Base") [],VClass (VName "Base2") []] [Constructor 185 "ThisTest" [] [Pass]],Function 209 "tester" [] VVoid [FunArg (VClass (VName "Base") []) "args___b",FunArg VString "args___txt"] [OtherFn 250 (TypedVar (VName "args___b") (VClass (VName "Base") []) Nothing (Just (TypedVar (VName "print") VVoid (Just [TypedVar (VName "args___txt") VString Nothing Nothing]) Nothing)))],Function 264 "main" [] VInt [] [AssignFn 281 (TypedVar (VName "a") VAuto Nothing Nothing) (VClass (VName "ThisTest") []) (TypedVar (VName "ThisTest") (VClass (VName "ThisTest") []) (Just []) Nothing),OtherFn 298 (TypedVar (VName "tester") VVoid (Just [TypedVar (VName "a") (VClass (VName "ThisTest") []) Nothing Nothing,StringVal 308 "JD"]) Nothing)]])]
  let expectedOUT = ["namespace classInheritanceParentRefExample{\n","void tester(Base& args___b, String args___txt);\n","int main();\n","class Base{\npublic:\n","   Base(){\n","   }\n","   void print(String args___arg){\n","   }\n","};\n","class Base2{\npublic:\n","   Base2(){\n","   }\n","   void print1(String args___arg){\n","   }\n","};\n","class ThisTest : public Base,public Base2{\npublic:\n","   ThisTest(){\n","   }\n","};\n","void tester(Base& args___b, String args___txt){\n","   args___b.print(args___txt)","   ;\n","}\n","int main(){\n","   ThisTest a = ThisTest();\n","   tester(a, \"JD\")","   ;\n","}\n","}\n"]


  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT
