{-# LANGUAGE OverloadedStrings #-}

module Compiler.Translator where

import AST
import Compiler.Translator.Type
import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Control.Monad.Writer (WriterT, mapWriterT, tell)
import Data.List (intercalate)
import Data.Maybe as M

translate :: AST -> IO ()
translate (AST stmts) = mapM_ print stmts

translate' :: AST -> Translator
translate' (AST stmts) = concat <$> mapM translateStatement stmts

declaredFunctionNames :: [Stmt] -> [String]
declaredFunctionNames = map (\(Function n _ _ _) -> n) . filter isFunc
  where
    isFunc x =
      case x of
        Function {} -> True
        _ -> False

translateStatement :: Stmt -> Translator
translateStatement s =
  case s of
    t@Import {} -> importTranslator t
    t@Function {} -> functionTranslator t
    t@Assign {} -> assignTranslator t
    t@While {} -> whileTranslator t
    t@For {} -> forTranslator t
    t@CasualExpr {} -> casualExprTranslator t
    t@ClassExpr {} -> classTranslator t
    t@ReturnExpr {} -> returnExprTranslator t
    t@Skip -> return []

--    _ -> return ()
importTranslator :: Stmt -> Translator
importTranslator (Import name) = return ["#include \"" ++ name ++ "\"\n"]

functionTranslator :: Stmt -> Translator
functionTranslator (Function name ret args block) = do
  let readyArgs = argumentsTranslator . M.fromMaybe [] $ args
  readyBlock <- blockTranslator block
  return . concat $ [["void " ++ name ++ "(" ++ readyArgs ++ "){\n"], readyBlock, ["}\n"]]

argumentsTranslator :: [FunArg] -> String
argumentsTranslator = intercalate ", " . map (\(FunArg type' name) -> typeToString type' ++ " " ++ name)

typeToString :: VarType -> String
typeToString t =
  case t of
    VInt -> "int"
    VFloat -> "float"
    VString -> "char*"
    VVoid -> "void"
    VAuto -> "auto"
    VChar -> "char"
    _ -> "void"

blockTranslator :: BodyBlock -> Translator
blockTranslator x = concat' <$> mapM translateStatement x
  where
    concat' = map makeIndent . concat

makeIndent :: String -> String
makeIndent x = "   " ++ x

newLine :: String -> String
newLine x = x ++ "\n"

assignTranslator :: Stmt -> Translator
assignTranslator (Assign name type' expr) = do
  let t = typeToString type'
  e <- aExprTranslator expr 
  return . return . concat $ ((t ++ " " ++ name ++ " = ") : e) ++ ["\n"]

whileTranslator :: Stmt -> Translator
whileTranslator (While bExpr block) = do
  bExpr' <- bExprTranslator bExpr
  block' <- blockTranslator block
  return . concat $ 
    [ ["while(" ++ head bExpr' ++ "){\n"]
    , block'
    , ["}\n"]
    ]
forTranslator :: Stmt -> Translator
forTranslator (For (Var name Nothing Nothing) range block) = do
  block' <- blockTranslator block
--  TODO replace mock with real feature
  return . concat $ 
    [ ["for(int i = 0; i < 10; i++){\n"] 
    , block'
    , ["}\n"]
    ]

classTranslator :: Stmt -> Translator
classTranslator (ClassExpr name block) = do
--  TODO replace mock with real feature
  block' <- blockTranslator block
  return . concat $ 
    [ ["class " ++ name ++ "{\n"]
    , block'
    , ["}\n"]
    ]

casualExprTranslator :: Stmt -> Translator
casualExprTranslator (CasualExpr aExpr) = (++ ["\n"]) <$> aExprTranslator aExpr

returnExprTranslator :: Stmt -> Translator
returnExprTranslator (ReturnExpr aExpr) = do
  aExpr' <- aExprTranslator aExpr
  return . return $ "return " ++ head aExpr' ++ ";\n"

aExprTranslator :: AExpr -> Translator
aExprTranslator expr =
  case expr of
    e@Var {} -> varTranslator e
    IntConst i -> return . return $ show i
    FloatConst f -> return . return $ show f
    StringVal s -> return . return $ s
    e@ListVar {} -> return ["\"TODO\""]
    e@Range {} -> return ["\"TODO\""]
    e@Fn {} -> return ["\"TODO\""]
    e@Neg {} -> return ["\"TODO\""]
    e@ABinary {} -> return ["\"TODO\""]
    e@If {} -> return ["\"TODO\""]

varTranslator :: AExpr -> Translator
varTranslator (Var name Nothing more') = do
  tell [name]
  readyMore <- moreVarTranslator more'
  return . return . concat $ name : readyMore
varTranslator (Var name (Just args) more') = do
  tell ["invoke function:" ++ name]
  readyMore <- moreVarTranslator more'
  args' <- intercalate ", " . concat <$> mapM aExprTranslator args
  return . return . concat $ (name ++ "(" ++ args' ++ ")") : readyMore

moreVarTranslator :: Maybe AExpr -> Translator
moreVarTranslator (Just e) = return . (\[x] -> '.' : x) <$> varTranslator e
moreVarTranslator Nothing = return []

--listVarTranslator :: AExpr -> Translator
--listVarTranslator (ListVar es) = do
  
bExprTranslator :: BExpr -> Translator
bExprTranslator expr = case expr of
--  TODO replace mock with real feature
  e@BoolConst {} -> return ["true"]
  e@Not {} -> return ["true"]
  e@BBinary {} -> return ["true"]
  e@RBinary {} -> return ["true"]

