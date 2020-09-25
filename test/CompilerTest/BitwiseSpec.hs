module CompilerTest.BitwiseSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec

path = "res/tests/bitwise/"

main =
  describe "Bitwise" $ -- do
      it "does bitwise works" bitwiseTest

bitwiseTest = do
  (ast, out) <- compile path "bitwiseExample"
  let expectedAST = [IFile "CoreNativeBitwise" "res/tests/bitwise/Core/Native/Bitwise.mard" (AST [Skip 0,LinkPath 33 "Bitwise.h",NativeFunction 56 "setBitHigh" "setBitHigh" [] VVoid [FunArg (VPointer (VNum NUInt8) NativePtr) "args___register",FunArg VInt "args___num"],NativeFunction 126 "setBitLow" "setBitLow" [] VVoid [FunArg (VPointer (VNum NUInt8) NativePtr) "args___register",FunArg VInt "args___num"],NativeFunction 194 "setAllLow" "setAllLow" [] VVoid [FunArg (VPointer (VNum NUInt8) NativePtr) "args___register"],NativeFunction 252 "toggleBit" "toggleBit" [] VVoid [FunArg (VPointer (VNum NUInt8) NativePtr) "args___register",FunArg VInt "args___num"]]),IFile "bitwiseExample" "res/tests/bitwise/bitwiseExample.mard" (AST [Skip 0,Import 28 "" ["Core","Native","Bitwise"],NativeAssignDeclaration 56 "" "PORTB" (VPointer (VNum NUInt8) NativePtr),Function 89 "test" [] VVoid [] [OtherFn 107 (TypedVar (VNameNative "setBitHigh" "setBitHigh") VVoid (Just [TypedVar (VName "PORTB") (VPointer (VNum NUInt8) NativePtr) Nothing Nothing,IntConst 125 1]) Nothing),OtherFn 130 (TypedVar (VNameNative "setBitLow" "setBitLow") VVoid (Just [TypedVar (VName "PORTB") (VPointer (VNum NUInt8) NativePtr) Nothing Nothing,IntConst 147 1]) Nothing),OtherFn 152 (TypedVar (VNameNative "setAllLow" "setAllLow") VVoid (Just [TypedVar (VName "PORTB") (VPointer (VNum NUInt8) NativePtr) Nothing Nothing]) Nothing),OtherFn 171 (TypedVar (VNameNative "toggleBit" "toggleBit") VVoid (Just [TypedVar (VName "PORTB") (VPointer (VNum NUInt8) NativePtr) Nothing Nothing,IntConst 188 2]) Nothing)]])]
  let expectedOUT = ["#include \"Bitwise.h\"\n","namespace CoreNativeBitwise{\n","}\n","namespace bitwiseExample{\n","using namespace CoreNativeBitwise;\n","void test();\n","void test(){\n","   setBitHigh(PORTB, 1)","   ;\n","   setBitLow(PORTB, 1)","   ;\n","   setAllLow(PORTB)","   ;\n","   toggleBit(PORTB, 2)","   ;\n","}\n","}\n"]

  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

