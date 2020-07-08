module CompilerTest.OptionalSpec  where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec

path = "res/tests/optional/"

main =
  describe "Optional expression" $ do
      it "can bool optional" boolOptionalTest
      it "can bool and ptr optional" boolPtrOptionalTest
      it "can ptr optional" ptrOptionalTest

boolOptionalTest = do
  (ast, out) <- compile path "boolOptionalExample"
  let expectedAST = [IFile "boolOptionalExample" "res/tests/optional/boolOptionalExample.mard" (AST [Skip 0,ClassExpr 34 "ThisTest" [] [] [Constructor {constructorOffset = 55, constructorName = "ThisTest", constructorArgs = [], constructorBody = [Pass]},Method {methodOffset = 82, methodName = "boolOp", methodRet = VBool, methodArgs = [], methodDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False, parentNameMD = "ThisTest"}, methodBody = [ReturnFn 104 (Just (ABool (BoolConst False)))]}],Function 115 "Main" [] VInt [] [AssignFn 132 (TypedVar (VName "a") VAuto Nothing Nothing) (VClass (VName "ThisTest") []) (TypedVar (VName "ThisTest") (VClass (VName "ThisTest") []) (Just []) Nothing),IfFn 155 [(BoolVar (Optional 0 (TypedVar (VName "a") (VClass (VName "ThisTest") []) Nothing Nothing) BoolOT),[Pass])],ReturnFn 177 (Just (IntConst 181 0))]])]
  let expectedOUT = ["namespace boolOptionalExample{\n","class ThisTest;\n","int Main();\n","class ThisTest{\n","   public:\n","   ThisTest(){\n","   }\n","   public:\n","   bool boolOp() {\n","      return false;\n","   }\n","};\n","int Main(){\n","   ThisTest a = ThisTest();\n","   if(a){\n}\n","   return 0;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

boolPtrOptionalTest = do
  (ast, out) <- compile path "boolPtrOptionalExample"
  let expectedAST = [IFile "boolPtrOptionalExample" "res/tests/optional/boolPtrOptionalExample.mard" (AST [Skip 0,ClassExpr 37 "ThisTest" [] [] [Constructor {constructorOffset = 58, constructorName = "ThisTest", constructorArgs = [], constructorBody = [Pass]},Method {methodOffset = 85, methodName = "boolOp", methodRet = VBool, methodArgs = [], methodDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False, parentNameMD = "ThisTest"}, methodBody = [ReturnFn 107 (Just (ABool (BoolConst False)))]}],Function 118 "Main" [] VInt [] [AssignFn 135 (TypedVar (VName "a") VAuto Nothing Nothing) (VPointer (VClass (VName "ThisTest") []) SharedPtr) (TypedVar (VName "ThisTest") (VPointer (VCopy (VClass (VName "ThisTest") [])) SharedPtr) (Just []) Nothing),IfFn 162 [(BoolVar (Optional 0 (TypedVar (VName "a") (VPointer (VClass (VName "ThisTest") []) SharedPtr) Nothing Nothing) BoolPtrOT),[Pass])],ReturnFn 185 (Just (IntConst 189 0))]])]
  let expectedOUT = ["namespace boolPtrOptionalExample{\n","class ThisTest;\n","int Main();\n","class ThisTest{\n","   public:\n","   ThisTest(){\n","   }\n","   public:\n","   bool boolOp() {\n","      return false;\n","   }\n","};\n","int Main(){\n","   shared_ptr<ThisTest> a = new ThisTest();\n","   if(a.isNotNull() && a*){\n}\n","   return 0;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

ptrOptionalTest = do
  (ast, out) <- compile path "ptrOptionalExample"
  let expectedAST = [IFile "ptrOptionalExample" "res/tests/optional/ptrOptionalExample.mard" (AST [Skip 0,ClassExpr 33 "ThisTest" [] [] [Constructor {constructorOffset = 54, constructorName = "ThisTest", constructorArgs = [], constructorBody = [Pass]},Method {methodOffset = 81, methodName = "boolOp", methodRet = VBool, methodArgs = [], methodDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False, parentNameMD = "ThisTest"}, methodBody = [ReturnFn 103 (Just (ABool (BoolConst False)))]}],Function 114 "Main" [] VInt [] [AssignFn 131 (TypedVar (VName "a") VAuto Nothing Nothing) (VPointer (VClass (VName "ThisTest") []) SharedPtr) (TypedVar (VName "ThisTest") (VPointer (VCopy (VClass (VName "ThisTest") [])) SharedPtr) (Just []) Nothing),IfFn 158 [(BoolVar (Optional 0 (TypedVar (VName "a") (VPointer (VClass (VName "ThisTest") []) SharedPtr) Nothing Nothing) NullOT),[Pass])],ReturnFn 180 (Just (IntConst 184 0))]])]
  let expectedOUT = ["namespace ptrOptionalExample{\n","class ThisTest;\n","int Main();\n","class ThisTest{\n","   public:\n","   ThisTest(){\n","   }\n","   public:\n","   bool boolOp() {\n","      return false;\n","   }\n","};\n","int Main(){\n","   shared_ptr<ThisTest> a = new ThisTest();\n","   if(a.isNotNull()){\n}\n","   return 0;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT


