module Full.LambdaFn where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec

tmp = do
  (ast, out) <- compile $ path "constFunArgExample.mard"
  let expectedAST = []
  let expectedOUT = []
  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

path fName = "res/test/lambdaFn/" ++ fName

main =
  describe "Lambda function" $ do
    context "without body block" $ do
      it "can be declared" canBeDeclaredTest
      it "can be invoked" pending
    context "without body block" $ do
      it "can be declared" canBeBlockDeclaredTest
      it "can be invoked" pending

canBeDeclaredTest = do
  (ast, out) <- compile $ path "simpleLambdaExample.mard"
  let expectedAST =
        [ IFile
            "main"
            "res/test/lambdaFn/simpleLambdaExample.mard"
            (AST
               [ Skip 0
               , Function
                   33
                   "testFunc"
                   VVoid
                   []
                   [ AssignFn
                       55
                       (TypedVar (VName "lambda") VAuto Nothing Nothing)
                       VAuto
                       (LambdaFn
                          64
                          VAuto
                          [FunArg VAuto "x"]
                          [OtherFn 70 (ABinary Add (Var 70 "x" [] Nothing Nothing) (IntConst 74 10))])
                   , Pass
                   ]
               ])
        ]
  let expectedOUT = ["void testFunc();\n", "void testFunc(){\n", "   auto lambda = \"TODO\";\n", "   ", "}\n"]
  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

canBeInvokedTest = pending -- do

--  (ast, out) <- compile $ path "constFunArgExample.mard"
--  let expectedAST = []
--  let expectedOUT = []
--  ast `shouldBe` expectedAST
--  out `shouldBe` expectedOUT
canBeBlockDeclaredTest = do
  (ast, out) <- compile $ path "simpleBlockLambdaExample.mard"
  let expectedAST =
        [ IFile
            "main"
            "res/test/lambdaFn/simpleBlockLambdaExample.mard"
            (AST
               [ Skip 0
               , Function
                   33
                   "testFunc"
                   VVoid
                   []
                   [ AssignFn
                       55
                       (TypedVar (VName "lambda") VAuto Nothing Nothing)
                       VAuto
                       (LambdaFn
                          69
                          VAuto
                          [FunArg VAuto "x", FunArg VAuto "z"]
                          [ AssignFn
                              85
                              (Var 85 "y" [] Nothing Nothing)
                              VAuto
                              (ABinary Add (Var 89 "x" [] Nothing Nothing) (IntConst 93 10))
                          , OtherFn 103 (ABinary Add (Var 103 "y" [] Nothing Nothing) (IntConst 107 13))
                          ])
                   , Pass
                   ]
               ])
        ]
  let expectedOUT = ["void testFunc();\n", "void testFunc(){\n", "   auto lambda = \"TODO\";\n", "   ", "}\n"]
  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT
