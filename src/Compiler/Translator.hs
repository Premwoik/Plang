{-# LANGUAGE OverloadedStrings #-}

module Compiler.Translator where

import AST
import Compiler.Translator.Type
import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Control.Monad.State (get, put, gets)
import Control.Monad.Writer (WriterT, mapWriterT, tell)
import Control.Monad.Reader (asks)
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe as M

translate :: AST -> IO ()
translate (AST stmts) = mapM_ print stmts

translate' :: AST -> Translator
translate' (AST stmts) = do
  imports <- initialImports ["Arduino.h", "ArrayList.h", "Map.h", "Maybe.h"]
  state <- get
  let initGlobal = Map.fromList . findAllDeclaredNames $ stmts
  put $ state {globalInstances = initGlobal}
  restOfCode <- mapM translateStatement stmts
  return . concat $ imports : restOfCode

initialImports :: [String] -> Translator
initialImports modules = return $ map merge modules
  where
    merge x = "#include \"" ++ x ++ "\"\n"

findAllDeclaredNames :: [Stmt] -> [(String, DPair)]
findAllDeclaredNames = map makeOutput . filter isName
  where
    makeOutput x =
      case x of
        ClassExpr n _ _-> (n, (VVoid, ClassDecl))
        Function n t _ _ -> (n, (t, FunctionDecl))
        Assign n t _ -> (n, (t, Instance))
    isName x =
      case x of
        ClassExpr {} -> True
        Function {} -> True
        Assign {} -> True
        _ -> False

translateStatement :: Stmt -> Translator
translateStatement s =
  case s of
    t@Import {} -> importTranslator t
    t@LinkPath{} -> linkPathTranslator t
    t@Function {} -> functionTranslator t
    t@NativeFunction {} -> nativeTranslator t
    t@Assign {} -> assignTranslator t
--    While a b -> whileTranslator a b translateStatement
--    For a b c -> forTranslator a b c translateStatement
--    t@CasualExpr {} -> casualExprTranslator t
    t@ClassExpr {} -> classTranslator t
    t@Skip -> return []


--    _ -> return ()
importTranslator :: Stmt -> Translator
importTranslator (Import name) = return ["#include \"" ++ intercalate "/" name ++ ".h\"\n"]

linkPathTranslator :: Stmt -> Translator
linkPathTranslator (LinkPath name) = return ["#include \"" ++ name ++ "\"\n"]

functionTranslator :: Stmt -> Translator
functionTranslator (Function name ret args block) = do
  let readyArgs = argumentsTranslator . M.fromMaybe [] $ args
  readyBlock <- blockTranslator' functionStmtTranslator block
  return . concat $ [["void " ++ name ++ "(" ++ readyArgs ++ "){\n"], readyBlock, ["}\n"]]
  
functionStmtTranslator :: FunctionStmt -> Translator
functionStmtTranslator s = case s of
  AssignFn a b c -> assignTranslator $ Assign a b c
  WhileFn a b -> whileTranslator a b functionStmtTranslator
  ForFn a b c -> forTranslator a b c functionStmtTranslator
  a@ReturnFn {} -> returnExprTranslator a
  a@OtherFn {} -> casualExprTranslator a
    

nativeTranslator :: Stmt -> Translator
nativeTranslator (NativeFunction name type' args) = return []

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
blockTranslator = blockTranslator' translateStatement

blockTranslator' :: (a -> Translator) -> [a] -> Translator
blockTranslator' trans x = concat' <$> mapM trans x
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

whileTranslator :: BExpr -> [a] -> (a -> Translator) -> Translator
whileTranslator bExpr block trans = do
  bExpr' <- bExprTranslator bExpr
  block' <- blockTranslator' trans block
  return . concat $ [["while(" ++ head bExpr' ++ "){\n"], block', ["}\n"]]

forTranslator :: AExpr -> AExpr -> [a] -> (a -> Translator) -> Translator
forTranslator (Var name Nothing Nothing) range block trans = do
  block' <- blockTranslator' trans block
  return . concat $ [["for(int i = 0; i < 10; i++){\n"], block', ["}\n"]]

--  TODO replace mock with real feature
--  TODO add generics support (as template)
classTranslator :: Stmt -> Translator
classTranslator (ClassExpr name generics block) = do
--  tell [show (fromMaybe [] generics)]
  block' <- blockTranslator' classStmtTranslator block
  return . concat $ [["class " ++ name ++ "{\n"], block', ["}\n"]]

classStmtTranslator :: ClassStmt -> Translator
classStmtTranslator c = case c of
  ClassAssign a b c -> assignTranslator $ Assign a b c
  Method a b c d -> functionTranslator $ Function a b c d

--  TODO replace mock with real feature
casualExprTranslator :: FunctionStmt -> Translator
casualExprTranslator (OtherFn aExpr) = (++ ["\n"]) <$> aExprTranslator aExpr

returnExprTranslator :: FunctionStmt -> Translator
returnExprTranslator (ReturnFn aExpr) = do
  aExpr' <- aExprTranslator aExpr
  return . return $ "return " ++ head aExpr' ++ ";\n"

aExprTranslator :: AExpr -> Translator
aExprTranslator expr =
  case expr of
    e@Var {} -> varTranslator e
    IntConst i -> return . return $ show i
    FloatConst f -> return . return $ show f
    StringVal s -> return . return $ s
    e@ListVar {} -> listVarTranslator e
    e@Range {} -> return ["\"TODO\""]
    e@Fn {} -> return ["\"TODO\""]
    e@FnBlock {} -> return [show e]
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

listVarTranslator :: AExpr -> Translator
listVarTranslator (ListVar expr) = do
-- TODO figure out how to check if all elements are the same type 
  let wantedType = VInt
  backup <- get
  put $ backup {cache = TypeCache []}
  let size = length expr
  res <- concat <$> mapM aExprTranslator expr
  let resAsString = intercalate "," res
  return . return $ "new ArrayList(new " ++ typeToString wantedType ++ "[" ++ show size ++ "]{" ++ resAsString ++ "}"
  

moreVarTranslator :: Maybe AExpr -> Translator
moreVarTranslator (Just e) = return . (\[x] -> '.' : x) <$> varTranslator e
moreVarTranslator Nothing = return []

--listVarTranslator :: AExpr -> Translator
--listVarTranslator (ListVar es) = do
bExprTranslator :: BExpr -> Translator
bExprTranslator expr =
  case expr
--  TODO replace mock with real feature
        of
    e@BoolConst {} -> return ["true"]
    e@Not {} -> return ["true"]
    e@BBinary {} -> return ["true"]
    e@RBinary {} -> return ["true"]
    