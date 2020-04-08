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
classAssignTranslator (ClassAssign _ name type' Nop) = return [typeToString type' ++ " " ++ intercalate "." name ++ ";\n"]
-- Pointer
classAssignTranslator (ClassAssign _ name type' (TypedVar cName (VClass t [] _) (Just args) Nothing)) = do
  let uName = unwrapVarName cName
  args' <- intercalate ", " . concat <$> mapM (injectTranslator aExprTranslatorGetter) args
  return ["unique_ptr<" ++ uName ++ "> " ++ intercalate "." name ++ "{new " ++ uName ++ "{" ++ args' ++ "}};\n"]
-- Generic Pointer
classAssignTranslator (ClassAssign _ name type' (TypedVar cName (VClass t gen _) (Just args) Nothing)) = do
  let uName = unwrapVarName cName
  args' <- intercalate ", " . concat <$> mapM (injectTranslator aExprTranslatorGetter) args
  return
    [ "unique_ptr<" ++
      uName ++ genStr ++ "> " ++ intercalate "." name ++ "{new " ++ uName ++ genStr ++ "{" ++ args' ++ "}};\n"
    ]
  where
     genStr = genStr' gen
     genStr' g =  "<" ++ (intercalate ", " . map translateGen) g ++ ">"
     translateGen (VClass n g _) = n ++ genStr' g
     translateGen g = typeToString g
        
-- Default assign
classAssignTranslator (ClassAssign _ name type' expr) = do
  let t = typeToString type'
  e <- injectTranslator aExprTranslatorGetter expr
  return . return . concat $ ((t ++ " " ++ intercalate "." name ++ " = ") : e) ++ [";\n"]


constTranslator :: ClassStmt -> Translator
constTranslator (Constructor _ name args block) = do
  let readyArgs = argumentsTranslator args
  readyBlock <- blockTranslator' (injectTranslator fnStmtTranslatorGetter) block
  return . concat $ [[name ++ "(" ++ readyArgs ++ "){\n"] ++ readyBlock ++ ["}\n"]]

