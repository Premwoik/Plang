module Compiler.Translator.Statement where

import           AST
import           Compiler.Translator.Type
import           Data.List                (intercalate)
import           Debug.Trace

importTranslator :: Stmt -> Translator
importTranslator (Import _ [] name) = return ["using namespace " ++ intercalate "" name ++ ";\n"]
importTranslator (Import _ _ name) = return []

linkPathTranslator :: Stmt -> Translator
linkPathTranslator (LinkPath _ name) = addImport $ "#include \"" ++ name ++ "\"\n"

functionTranslator :: Stmt -> Translator
functionTranslator (Function _ name ret args block) = do
  let readyArgs = argumentsTranslator args
  readyBlock <- blockTranslator' (injectTranslator fnStmtTranslatorGetter) block
  return . concat $ [[typeToString ret ++ " " ++ name ++ "(" ++ readyArgs ++ "){\n"], readyBlock, ["}\n"]]

argumentsTranslator :: [FunArg] -> String
argumentsTranslator = intercalate ", " . map (\(FunArg t name) -> unwrapType t name)
  where
    markRef t = case t of
      VClass {} -> "&"
      _ -> ""
    unwrapType (VFn t) n = typeToString (VFnNamed n t)
    unwrapType t name = typeToString t ++ markRef t ++ " " ++ name

nativeTranslator :: Stmt -> Translator
nativeTranslator (NativeFunction _ _ name type' args) = return []

whileTranslator :: BExpr -> [a] -> (a -> Translator) -> Translator
whileTranslator bExpr block trans = do
  bExpr' <- injectTranslator bExprTranslatorGetter bExpr
  block' <- blockTranslator' trans block
  return . concat $ [["while(" ++ head bExpr' ++ "){\n"], block', ["}\n"]]

forTranslator :: AExpr -> AExpr -> [a] -> (a -> Translator) -> Translator
forTranslator (TypedVar name type' Nothing Nothing) (Range _ _ a b) block trans = do
  let uName = unwrapVarName name
  a' <- injectTranslator aExprTranslatorGetter a
  b' <- injectTranslator aExprTranslatorGetter b
  block' <- blockTranslator' trans block
  return . concat $
    [ ["for(" ++ typeToString type' ++ " " ++ uName ++ " = "]
    , a'
    , ["; " ++ uName ++ " < "]
    , b'
    , ["; " ++ uName ++ "++){\n"]
    , block'
    , ["}\n"]
    ]

ifTranslator :: FunctionStmt -> Translator
ifTranslator (IfFn _ l) = mapM build $ zip [1 ..] l
  where
    translateIf s (b, body) = do
      res <- concat <$> blockTranslator' (injectTranslator fnStmtTranslatorGetter) body
      cond <- injectTranslator bExprTranslatorGetter b
      return $ s ++ head cond ++ "){\n" ++ res ++ "}\n"
    translateElse (b, body) = do
      res <- concat <$> blockTranslator' (injectTranslator fnStmtTranslatorGetter) body
      return $ "else {\n" ++ res ++ "}\n"
    build (x, p)
      | x == 1 = translateIf "if(" p
      | x == length l = translateElse p
      | otherwise = translateIf "else if(" p


assignTranslator :: Stmt -> Translator
-- Only declaration without assigning value
assignTranslator (Assign _ name type' Nop) = do
  n <- injectTranslator aExprTranslatorGetter name
  return [typeToString type' ++ " " ++ head n ++ ";\n"]
-- assign without declaration
assignTranslator (Assign _ name type' expr) = do
  e <- injectTranslator aExprTranslatorGetter expr
  n <- head <$> injectTranslator aExprTranslatorGetter name
  return . return . concat $ ((lSite n ++ " = ") : e) ++ [";\n"]
  where
    lSite = unwrapType type'
    unwrapType (VPointer c SharedPtr) n = "shared_ptr<" ++ typeToString c ++ ">" ++ " " ++ n
    unwrapType (VFn t) n = typeToString (VFnNamed n t)
    unwrapType x n = typeToString x ++ " " ++ n

--  TODO replace mock with real feature
casualExprTranslator :: FunctionStmt -> Translator
casualExprTranslator (OtherFn _ aExpr) = (++ [";\n"]) <$> injectTranslator aExprTranslatorGetter aExpr

returnExprTranslator :: FunctionStmt -> Translator
returnExprTranslator (ReturnFn _ aExpr) = do
  aExpr' <- injectTranslator aExprTranslatorGetter aExpr
  return . return $ "return " ++ head aExpr' ++ ";\n"
