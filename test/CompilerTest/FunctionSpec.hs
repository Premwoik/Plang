module CompilerTest.FunctionSpec where

import           AST
import           Compiler              (compile)
import           Test.Hspec
import           Test.Hspec.Megaparsec


path = "res/test/function/"

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
  let expectedAST = []
  let expectedOUT = []
       
  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testInvokeGenClassMethod = do
  (ast, out) <- compile path "invokeGenClassMethodExample"
  let expectedAST = []
  let expectedOUT = []
 
  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testInvokeClassMethod = do
  (ast, out) <- compile path "invokeClassMethodExample"
  let expectedAST = []
  let expectedOUT = []
  
  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testFunArgCantBeModified = pending

testGlobalAndArgVars = do
  (ast, out) <- compile path "funArgsAndGlobalVarExample"
  let expectedAST = []
  let expectedOUT = []
   
  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT

testConstFunArg = do
  (ast, out) <- compile path "constFunArgExample"
  let expectedAST = []
  let expectedOUT = []
     
  ast `shouldBe` expectedAST
  out `shouldBe` expectedOUT
