{-# LANGUAGE OverloadedStrings #-}

module Compiler.Translator where

import           AST
import           Compiler.Translator.Type
import           Control.Exception
import           Control.Monad            (join)
import           Control.Monad.Identity   (Identity)
import           Control.Monad.Reader     (asks)
import           Control.Monad.State      (get, gets, put)
import           Control.Monad.Writer     (WriterT, mapWriterT, tell)
import           Data.List                (intercalate)
import qualified Data.Map                 as Map
import           Data.Maybe               as M

translate :: AST -> IO ()
translate (AST stmts) = mapM_ print stmts

translate' :: AST -> Translator
translate' (AST stmts) = do
  imports <- initialImports ["Arduino.h", "ArrayList.h", "Map.h", "Maybe.h"]
  state <- get
  restOfCode <- mapM translateStatement stmts
  return . concat $ imports : restOfCode

initialImports :: [String] -> Translator
initialImports modules = return $ map merge modules
  where
    merge x = "#include \"" ++ x ++ "\"\n"

translateStatement :: Stmt -> Translator
translateStatement s =
  case s of
    t@Import {}         -> importTranslator t
    t@LinkPath {}       -> linkPathTranslator t
    t@Function {}       -> functionTranslator t
    t@NativeFunction {} -> nativeTranslator t
    t@Assign {}         -> assignTranslator t
    t@ClassExpr {}      -> classTranslator t
    t@Skip              -> return []

--    _ -> return ()
importTranslator :: Stmt -> Translator
importTranslator (Import name) = return ["#include \"" ++ intercalate "/" name ++ ".h\"\n"]

linkPathTranslator :: Stmt -> Translator
linkPathTranslator (LinkPath name) = return ["#include \"" ++ name ++ "\"\n"]

functionTranslator :: Stmt -> Translator
functionTranslator (Function name ret args block) = do
  let readyArgs = argumentsTranslator . M.fromMaybe [] $ args
  readyBlock <- blockTranslator' functionStmtTranslator block
  return . concat $ [[typeToString ret ++ " " ++ name ++ "(" ++ readyArgs ++ "){\n"], readyBlock, ["}\n"]]

functionStmtTranslator :: FunctionStmt -> Translator
functionStmtTranslator s =
  case s of
    AssignFn a b c -> assignTranslator $ Assign a b c
    WhileFn a b    -> whileTranslator a b functionStmtTranslator
    ForFn a b c    -> forTranslator a b c functionStmtTranslator
    a@IfFn {}      -> ifTranslator a
    a@ReturnFn {}  -> returnExprTranslator a
    a@OtherFn {}   -> casualExprTranslator a

ifTranslator :: FunctionStmt -> Translator
ifTranslator (IfFn l) = mapM build $ zip [1 ..] l
  where
    translateIf s (b, body) = do
      res <- concat <$> blockTranslator' functionStmtTranslator body
      cond <- bExprTranslator b
      return $ s ++ head cond ++ "){\n" ++ res ++ "}\n"
    translateElse (b, body) = do
      res <- concat <$> blockTranslator' functionStmtTranslator body
      return $ "else {\n" ++ res ++ "}\n"
    build (x, p)
      | x == length l = translateElse p
      | x == 1 = translateIf "if(" p
      | otherwise = translateIf "else if(" p 

nativeTranslator :: Stmt -> Translator
nativeTranslator (NativeFunction name type' args) = return []

argumentsTranslator :: [FunArg] -> String
argumentsTranslator = intercalate ", " . map (\(FunArg type' name) -> typeToString type' ++ " " ++ name)

typeToString :: VarType -> String
typeToString t =
  case t of
    VInt    -> "int"
    VFloat  -> "float"
    VString -> "char*"
    VVoid   -> "void"
    VAuto   -> "auto"
    VChar   -> "char"
    VBlank  -> "" 
    _       -> "void"

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
--assignTranslator (Assign name type' Nop) = 
--  return [typeToString type' ++ " " ++ name ++ ";\n"]
assignTranslator (Assign name type' expr) = do
  let t = typeToString type'
  e <- aExprTranslator expr
  return . return . concat $ ((t ++ " " ++ name ++ " = ") : e) ++ [";\n"]

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
  block' <- blockTranslator' classStmtTranslator block
  return . concat $ [["class " ++ name ++ "{\n"], block', ["}\n"]]

--  tell [show (fromMaybe [] generics)]
classStmtTranslator :: ClassStmt -> Translator
classStmtTranslator c =
  case c of
    ClassAssign a b c -> assignTranslator $ Assign a b c
    Method a b c d    -> functionTranslator $ Function a b c d
    t@Constructor {} -> constTranslator t

constTranslator :: ClassStmt -> Translator
constTranslator (Constructor name block) = do
  readyBlock <- blockTranslator' functionStmtTranslator block
  return . concat $ [[name ++ "{\n"] ++ readyBlock ++ ["}\n"]]

--  TODO replace mock with real feature
casualExprTranslator :: FunctionStmt -> Translator
casualExprTranslator (OtherFn aExpr) = (++ ["\n"]) <$> aExprTranslator aExpr

returnExprTranslator :: FunctionStmt -> Translator
returnExprTranslator (ReturnFn aExpr) = do
  aExpr' <- aExprTranslator aExpr
  return . return $ "return " ++ head aExpr' ++ ";\n"

-- (right_in, before, after)
aExprTranslator :: AExpr -> Translator
aExprTranslator expr =
  case expr of
    e@TypedVar {} -> varTranslator e
    Var a b c     -> varTranslator (TypedVar a VAuto b c)
    IntConst i    -> return . return $ show i
    FloatConst f  -> return . return $ show f
    StringVal s   -> return . return $ show s
    e@ListVar {}  -> listVarTranslator e
    e@Range {}    -> return ["\"TODO\""]
    e@Fn {}       -> return ["\"TODO\""]
    e@FnBlock {}  -> return [show e]
    e@Neg {}      -> return ["\"TODO\""]
    e@ABinary {}  -> return ["\"TODO\""]
    Nop -> return ["nullptr"]
    a             -> throw $ UnsupportedTypeException (show a)

--rangeTranslator :: AExpr -> Translator
--rangeTranslator (Range from to) =
--
--  return . return $ "new int[]"
--makeRange (Range (IntConst i) (IntConst j)) = [i .. j]
--makeRange (Range (FloatConst i) (FloatConst j)) = foldl (\a b -> [i .. j]) "}"
--makeRange (Range (IntConst i) (IntConst j)) = [i .. j]
varTranslator :: AExpr -> Translator
varTranslator (TypedVar name type' Nothing more') = do
  readyMore <- moreVarTranslator type' more'
  return . return . concat $ name : readyMore
varTranslator (TypedVar name type' (Just args) more') = do
  readyMore <- moreVarTranslator type' more'
  args' <- intercalate ", " . concat <$> mapM aExprTranslator args
  return . return . concat $ (name ++ "(" ++ args' ++ ")") : readyMore

moreVarTranslator :: VarType -> Maybe AExpr -> Translator
-- TODO handle pointers
moreVarTranslator (Pointer _) (Just e) = return . (\[x] -> '-' : '>' : x) <$> varTranslator e
moreVarTranslator _ (Just e) = return . (\[x] -> '.' : x) <$> varTranslator e
moreVarTranslator _ Nothing = return []

listVarTranslator :: AExpr -> Translator
listVarTranslator (ListVar expr) = do
  let wantedType = VInt
  backup <- get
  put $ backup {cache = TypeCache []}
  let size = length expr
  res <- concat <$> mapM aExprTranslator expr
  let resAsString = intercalate "," res
  return . return $ "new ArrayList(new " ++ typeToString wantedType ++ "[" ++ show size ++ "]{" ++ resAsString ++ "})"

bExprTranslator :: BExpr -> Translator
bExprTranslator expr =
  case expr
--  TODO replace mock with real feature
        of
    e@BoolConst {} -> return ["true"]
    e@Not {}       -> return ["true"]
    e@BBinary {}   -> return ["true"]
    e@RBinary {}   -> return ["true"]
