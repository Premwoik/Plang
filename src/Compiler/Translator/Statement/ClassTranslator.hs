module Compiler.Translator.Statement.ClassTranslator where

import           AST
import           Compiler.Translator.Statement
import           Compiler.Translator.Type
import           Data.List                     (intercalate)

classTranslator :: Stmt -> Translator
classTranslator (ClassExpr _ name generics parents block) = do
  block' <- blockTranslator' (injectTranslator classStmtTranslatorGetter) block
  return . concat $
    [[translateGenerics generics], ["class " ++ name ++ translateInheritance parents ++ "{\n"], block', ["};\n"]]

translateInheritance :: [VarType] -> String
translateInheritance [] = ""
translateInheritance parens = " : " ++ (intercalate "," . map translateGen $ parens)
  where
    genStr' [] = ""
    genStr' g  = "<" ++ (intercalate ", " . map translateGen) g ++ ">"
    translateGen (VClass n g) = "public " ++ unwrapVarName n ++ genStr' g
    translateGen g            = typeToString g

classAssignTranslator :: ClassStmt -> Translator
-- Only declaration without assigning value
classAssignTranslator (ClassAssign _ name type' d Nop) = do
  n <- head <$> injectTranslator aExprTranslatorGetter name
  return [visibilityMD d ++ ":\n" ++ unwrapType type' n ++ ";\n"]
  where
    unwrapType (VFn t cm) n = typeToString (VFnNamed n t cm)
    unwrapType x n          = typeToString x ++ " " ++ n
-- Pointer
--classAssignTranslator (ClassAssign _ name type' d (TypedVar cName (VClass t []) (Just args) Nothing)) = do
--  let uName = unwrapVarName cName
--  args' <- intercalate ", " . concat <$> mapM (injectTranslator aExprTranslatorGetter) args
--  n <- head <$> injectTranslator aExprTranslatorGetter name
--  return [visibilityMD d ++ ":\n" ++ "unique_ptr<" ++ uName ++ "> " ++ n ++ "{new " ++ uName ++ "{" ++ args' ++ "}};\n"]
-- Generic Pointer
--classAssignTranslator (ClassAssign _ name type' d (TypedVar cName (VClass t gen) (Just args) Nothing)) = do
--  let uName = unwrapVarName cName
--  n <- head <$> injectTranslator aExprTranslatorGetter name
--  args' <- intercalate ", " . concat <$> mapM (injectTranslator aExprTranslatorGetter) args
--  return [visibilityMD d ++ ":\n" ++ "unique_ptr<" ++ uName ++ genStr ++ "> " ++ n ++ "{new " ++ uName ++ genStr ++ "{" ++ args' ++ "}};\n"]
--  where
--    genStr = genStr' gen
--    genStr' g = "<" ++ (intercalate ", " . map translateGen) g ++ ">"
--    translateGen (VClass n g) = unwrapVarName n ++ genStr' g
--    translateGen g            = typeToString g
-- Default assign
classAssignTranslator (ClassAssign _ name type' d expr) = do
  let t = typeToString type'
  e <- injectTranslator aExprTranslatorGetter expr
  n <- head <$> injectTranslator aExprTranslatorGetter name
  return . return . concat $ [visibilityMD d ++ ":\n"] ++ ((t ++ " " ++ n ++ " = ") : e) ++ [";\n"]

constTranslator :: ClassStmt -> Translator
constTranslator (Constructor _ name args block) = do
  let readyArgs = argumentsTranslator args
  readyBlock <- blockTranslator' (injectTranslator fnStmtTranslatorGetter) block
  return . concat $ [["public:\n"], [name ++ "(" ++ readyArgs ++ "){\n"] ++ readyBlock ++ ["}\n"]]

abstractFunctionTranslator :: ClassStmt -> Translator
abstractFunctionTranslator (NativeMethod o n r a) = do
  let readyArgs = argumentsTranslator a
  return . concat $ [["public:\n"], ["virtual " ++ typeToString r ++ " " ++ n ++ "(" ++ readyArgs ++ ") = 0;\n"]]

methodTranslator :: ClassStmt -> Translator
methodTranslator (Method _ name (VFn t cm) args d block) = do
  let readyArgs = argumentsTranslator args
  readyBlock <- blockTranslator' (injectTranslator fnStmtTranslatorGetter) block
  return . concat $
    [ [visibilityMD d ++ ":\n"]
    , [typeToString (VFnNamed (name ++ "(" ++ readyArgs ++ ")") t cm) ++ " " ++ translateOverride d ++ "{\n"]
    , readyBlock
    , ["}\n"]
    ]
methodTranslator (Method _ name ret args d block) = do
  let readyArgs = argumentsTranslator args
  readyBlock <- blockTranslator' (injectTranslator fnStmtTranslatorGetter) block
  return . concat $
    [ [visibilityMD d ++ ":\n"]
    , [typeToString ret ++ " " ++ name ++ "(" ++ readyArgs ++ ") " ++ translateOverride d ++ "{\n"]
    , readyBlock
    , ["}\n"]
    ]

translateOverride details =
  if isOverrideMD details
    then "override "
    else ""
