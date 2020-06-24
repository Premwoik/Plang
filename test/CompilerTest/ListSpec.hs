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

canCreateListOfTypeTest = do
  (ast, out) <- compile path "listOfTypeExample"
  let expectedAST = [IFile "listOfTypeExample" "res/tests/list/listOfTypeExample.mard" (AST [Skip 0,Function 32 "Main" [] VInt [] [AssignFn 49 (TypedVar (VName "l") VAuto Nothing Nothing) (VClass (VName "ArrayList") [VGenPair "T" (VNum NUInt8)]) (TypedVar (VName "ArrayList") (VClass (VName "ArrayList") [VGenPair "T" (VNum NUInt8)]) (Just [TypedListVar [IntConst 61 10,IntConst 65 12] (VNum NUInt8),TypedVar (VName "2") VInt Nothing Nothing,TypedVar (VName "2") VInt Nothing Nothing]) Nothing)]])]
  let expectedOUT = ["namespace listOfTypeExample{\n","int Main();\n","int Main(){\n","   ArrayList<uint8_t> l = ArrayList<uint8_t>(new uint8_t[2]{10,12}, 2, 2);\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT


