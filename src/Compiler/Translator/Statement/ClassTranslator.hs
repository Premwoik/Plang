module Compiler.Translator.Statement.ClassTranslator where

import Compiler.Translator.Type
import Compiler.Translator.Statement
import Data.List(intercalate)
import AST
import Debug.Trace


--  TODO replace mock with real feature
--  TODO add generics support (as template)
classTranslator :: Stmt -> Translator
classTranslator (ClassExpr _ name generics block) = do
  block' <- blockTranslator' (injectTranslator classStmtTranslatorGetter) block
  let generics' = makeGenerics generics
  return . concat $ [[generics' ++ "class " ++ name ++ "{\npublic:\n"], block', ["};\n"]]
  where
    makeGenerics [] = ""
    makeGenerics l = "template<" ++ (intercalate ", " . map (\x -> "typename " ++ x)) l ++ ">\n"
    
    
classAssignTranslator :: ClassStmt -> Translator
-- Only declaration without assigning value
classAssignTranslator (ClassAssign _ name type' Nop) = do
  n <- head <$> injectTranslator aExprTranslatorGetter name
  return [unwrapType type' n ++ ";\n"]
  where
    unwrapType (VFn t) n = typeToString (VFnNamed n t)
    unwrapType x n = typeToString x ++ " " ++ n
-- Pointer
classAssignTranslator (ClassAssign _ name type' (TypedVar cName (VClass t []) (Just args) Nothing)) = do
  let uName = unwrapVarName cName
  args' <- intercalate ", " . concat <$> mapM (injectTranslator aExprTranslatorGetter) args
  n <- head <$> injectTranslator aExprTranslatorGetter name
  return ["unique_ptr<" ++ uName ++ "> " ++ n ++ "{new " ++ uName ++ "{" ++ args' ++ "}};\n"]
-- Generic Pointer
classAssignTranslator (ClassAssign _ name type' (TypedVar cName (VClass t gen) (Just args) Nothing)) = do
  let uName = unwrapVarName cName
  n <- head <$> injectTranslator aExprTranslatorGetter name
  args' <- intercalate ", " . concat <$> mapM (injectTranslator aExprTranslatorGetter) args
  return
    [ "unique_ptr<" ++
      uName ++ genStr ++ "> " ++ n ++ "{new " ++ uName ++ genStr ++ "{" ++ args' ++ "}};\n"
    ]
  where
     genStr = genStr' gen
     genStr' g =  "<" ++ (intercalate ", " . map translateGen) g ++ ">"
     translateGen (VClass n g) = unwrapVarName n ++ genStr' g
     translateGen g = typeToString g
     

     
-- Default assign
classAssignTranslator (ClassAssign _ name type' expr) = do
  let t = typeToString type'
  e <- injectTranslator aExprTranslatorGetter expr
  n <- head <$> injectTranslator aExprTranslatorGetter name
  return . return . concat $ ((t ++ " " ++ n ++ " = ") : e) ++ [";\n"]


constTranslator :: ClassStmt -> Translator
constTranslator (Constructor _ name args block) = do
  let readyArgs = argumentsTranslator args
  readyBlock <- blockTranslator' (injectTranslator fnStmtTranslatorGetter) block
  return . concat $ [[name ++ "(" ++ readyArgs ++ "){\n"] ++ readyBlock ++ ["}\n"]]

