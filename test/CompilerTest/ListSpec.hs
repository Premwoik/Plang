module CompilerTest.ListSpec  where

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

path = "res/tests/list/"

main =
  describe "List expression" $ do
    context "syntactic sugar" $ do
      it "can create list of type" canCreateListOfTypeTest
      it "can get works" canGetWorksTest
      it "can native get works" canNativeGetWorksTest

canCreateListOfTypeTest = do
  (ast, out) <- compile path "listOfTypeExample"
  let expectedAST = [IFile "listOfTypeExample" "res/tests/list/listOfTypeExample.mard" (AST [Skip 0,Function 32 "Main" [] VInt [] [AssignFn 49 (TypedVar (VName "l") VAuto Nothing Nothing) (VClass (VName "ArrayList") [VGenPair "T" (VNum NUInt8)]) (TypedVar (VName "ArrayList") (VClass (VName "ArrayList") [VGenPair "T" (VNum NUInt8)]) (Just [TypedListVar [IntConst 61 10,IntConst 65 12] (VNum NUInt8),TypedVar (VName "2") VInt Nothing Nothing,TypedVar (VName "2") VInt Nothing Nothing]) Nothing)]])]
  let expectedOUT = ["namespace listOfTypeExample{\n","int Main();\n","int Main(){\n","   ArrayList<uint8_t> l = ArrayList<uint8_t>(new uint8_t[2]{10,12}, 2, 2);\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT


canGetWorksTest = do
  (ast, out) <- compile path "getExample"
  let expectedAST = [IFile "getExample" "res/tests/list/getExample.mard" (AST [Skip 0,ClassExpr 25 "List" [] [] [Constructor {constructorOffset = 41, constructorName = "List", constructorArgs = [], constructorBody = [Pass]},Method {methodOffset = 61, methodName = "get", methodRet = VInt, methodArgs = [FunArg VInt "args___index"], methodDetails = MethodDetails {visibilityMD = "public", isOverrideMD = False, parentNameMD = "List"}, methodBody = [ReturnFn 90 (Just (TypedVar (VName "args___index") VInt Nothing Nothing))]}],Function 101 "Main" [] VInt [] [AssignFn 118 (TypedVar (VName "l") VAuto Nothing Nothing) (VClass (VName "List") []) (TypedVar (VName "List") (VClass (VName "List") []) (Just []) Nothing),AssignFn 131 (TypedVar (VName "p") VAuto Nothing Nothing) VInt (TypedVar (VName "l") (VClass (VName "List") []) Nothing (Just (TypedVar (VName "get") VInt (Just [IntConst 137 10]) Nothing)))]])]
  let expectedOUT = ["namespace getExample{\n","class List;\n","int Main();\n","class List{\n","   public:\n","   List(){\n","   }\n","   public:\n","   int get(int args___index) {\n","      return args___index;\n","   }\n","};\n","int Main(){\n","   List l = List();\n","   int p = l.get(10);\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canNativeGetWorksTest = do
  (ast, out) <- compile path "getNativeExample"
  let expectedAST = [IFile "getNativeExample" "res/tests/list/getNativeExample.mard" (AST [Skip 0,NativeClass 31 "" "List" ["T"] [NativeMethod 60 "List" VAuto [],NativeMethod 71 "get" (VGen "T") [FunArg VInt "index"]],Function 96 "Main" [] VInt [] [AssignFn 113 (TypedVar (VName "l") VAuto Nothing Nothing) (VClass (VName "List") [VGenPair "T" VInt]) (TypedVar (VName "List") (VClass (VName "List") [VGenPair "T" VInt]) (Just []) Nothing),AssignFn 131 (TypedVar (VName "p") VAuto Nothing Nothing) (VGenPair "T" VInt) (TypedVar (VName "l") (VClass (VName "List") [VGenPair "T" (VGenPair "T" VInt)]) Nothing (Just (TypedVar (VName "get") (VGenPair "T" VInt) (Just [IntConst 137 10]) Nothing)))]])]
  let expectedOUT = ["namespace getNativeExample{\n","int Main();\n","int Main(){\n","   List<int> l = List<int>();\n","   int p = l.get(10);\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT


