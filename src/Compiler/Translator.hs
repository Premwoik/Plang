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
--  imports <- initialImports ["Arduino.h", "ArrayList.h", "Map.h", "Maybe.h"]
  imports <- initialImports ["Arduino.h"]
  declarations <- declareFunctions stmts
--  state <- get
  restOfCode <- mapM translateStatement stmts
  return . concat $ imports : declarations : restOfCode

declareFunctions :: [Stmt]-> Translator
declareFunctions = return . map trans . filter cond
  where
    trans (Function _ n t args _) =
      typeToString t ++ " " ++ n ++ "(" ++ (argumentsTranslator . M.fromMaybe [] $ args) ++ ");\n"
    cond Function {} = True
    cond _ = False

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
    t@Skip {}              -> return []

--    _ -> return ()
importTranslator :: Stmt -> Translator
importTranslator (Import _ name) = return ["#include \"" ++ intercalate "/" name ++ ".h\"\n"]

linkPathTranslator :: Stmt -> Translator
linkPathTranslator (LinkPath _ name) = return ["#include \"" ++ name ++ "\"\n"]

functionTranslator :: Stmt -> Translator
functionTranslator (Function _ name ret args block) = do
  let readyArgs = argumentsTranslator . M.fromMaybe [] $ args
  readyBlock <- blockTranslator' functionStmtTranslator block
  return . concat $ [[typeToString ret ++ " " ++ name ++ "(" ++ readyArgs ++ "){\n"], readyBlock, ["}\n"]]

functionStmtTranslator :: FunctionStmt -> Translator
functionStmtTranslator s =
  case s of
    AssignFn o a b c -> assignTranslator $ Assign o a b c
    WhileFn _ a b    -> whileTranslator a b functionStmtTranslator
    ForFn _ a b c    -> forTranslator a b c functionStmtTranslator
    a@IfFn {}      -> ifTranslator a
    a@ReturnFn {}  -> returnExprTranslator a
    a@OtherFn {}   -> casualExprTranslator a

ifTranslator :: FunctionStmt -> Translator
ifTranslator (IfFn _ l) = mapM build $ zip [1 ..] l
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
nativeTranslator (NativeFunction _ name type' args) = return []

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
assignTranslator (Assign _ name type' expr) = do
  let t = typeToString type'
  e <- aExprTranslator expr
  return . return . concat $ ((t ++ " " ++ name ++ " = ") : e) ++ [";\n"]

whileTranslator :: BExpr -> [a] -> (a -> Translator) -> Translator
whileTranslator bExpr block trans = do
  bExpr' <- bExprTranslator bExpr
  block' <- blockTranslator' trans block
  return . concat $ [["while(" ++ head bExpr' ++ "){\n"], block', ["}\n"]]

forTranslator :: AExpr -> AExpr -> [a] -> (a -> Translator) -> Translator
forTranslator (TypedVar name type' Nothing Nothing) (Range a b) block trans = do
  a' <- aExprTranslator a
  b' <- aExprTranslator b
  block' <- blockTranslator' trans block
  return . concat $ [["for(" ++ typeToString type' ++ " " ++ name ++ " = "], a', ["; " ++ name ++ " < "], b', ["; " ++ name ++ "++){\n"], block', ["}\n"]]

--  TODO replace mock with real feature
--  TODO add generics support (as template)
classTranslator :: Stmt -> Translator
classTranslator (ClassExpr _ name generics block) = do
  block' <- blockTranslator' classStmtTranslator block
  return . concat $ [["class " ++ name ++ "{\n"], block', ["};\n"]]

--  tell [show (fromMaybe [] generics)]
classStmtTranslator :: ClassStmt -> Translator
classStmtTranslator c =
  case c of
    ClassAssign o a b c -> assignTranslator $ Assign o a b c
    Method o a b c d    -> functionTranslator $ Function o a b c d
    t@Constructor {} -> constTranslator t

constTranslator :: ClassStmt -> Translator
constTranslator (Constructor _ name block) = do
  readyBlock <- blockTranslator' functionStmtTranslator block
  return . concat $ [[name ++ "(){\n"] ++ readyBlock ++ ["}\n"]]

--  TODO replace mock with real feature
casualExprTranslator :: FunctionStmt -> Translator
casualExprTranslator (OtherFn _ aExpr) = (++ [";\n"]) <$> aExprTranslator aExpr

returnExprTranslator :: FunctionStmt -> Translator
returnExprTranslator (ReturnFn _ aExpr) = do
  aExpr' <- aExprTranslator aExpr
  return . return $ "return " ++ head aExpr' ++ ";\n"

-- (right_in, before, after)
aExprTranslator :: AExpr -> Translator
aExprTranslator expr =
  case expr of
    e@TypedVar {} -> varTranslator e
    Var a b c     -> varTranslator (TypedVar a VAuto b c)
    ABracket a ->  aExprTranslator a
    IntConst i    -> return . return $ show i
    FloatConst f  -> return . return $ show f
    StringVal s   -> return . return $ show s
    e@ListVar {}  -> listVarTranslator e
    e@Range {}    -> return ["\"TODO\""]
    e@Fn {}       -> return ["\"TODO\""]
    e@FnBlock {}  -> return [show e]
    e@Neg {}      -> return ["\"TODO\""]
    e@ABinary {}  -> binaryTranslator e
    Nop -> return ["nullptr"]
    a             -> throw $ UnsupportedTypeException (show a)

bracketTranslator (ABracket aExpr) = do
  res <- aExprTranslator aExpr
  return . concat $ ["("] ++ res ++ [")"]

binaryTranslator :: AExpr -> Translator
binaryTranslator (ABinary op a b) = do
  let op' = binaryOperatorToString op
  a' <- aExprTranslator a
  b' <- aExprTranslator b
  return . concat $ [a',  [" " ++ op' ++ " "], b']

binaryOperatorToString :: ABinOp -> String
binaryOperatorToString a = case a of
  Add -> "+"
  Subtract -> "-"
  Multiply -> "*"
  Divide -> "/"

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
    BoolConst a -> return $ if a then ["true"] else ["false"]
    e@Not {}       -> negationTranslator e
    e@BBinary {}   -> bBinaryTranslator e
    e@RBinary {}   -> rBinaryTranslator e

negationTranslator :: BExpr -> Translator
negationTranslator (Not bExpr) = do
  res <- bExprTranslator bExpr
  return . return . concat $ "!" : res



bBinaryTranslator :: BExpr -> Translator
bBinaryTranslator (BBinary op a b) = do
  let op' = bBinOpToString op
  a' <- bExprTranslator a
  b' <- bExprTranslator b
  return  ["(" ++ head a' ++ ") " ++ op' ++ " (" ++ head b' ++ ")"]

bBinOpToString :: BBinOp -> String
bBinOpToString a = case a of
  And -> "&&"
  Or -> "||"


rBinaryTranslator :: BExpr -> Translator
rBinaryTranslator (RBinary op a b) = do
  let op' = rBinOpToString op
  a' <- aExprTranslator a
  b' <- aExprTranslator b
  return  [head a' ++ " " ++ op' ++ " " ++ head b']
--  return  ["(" ++ head a' ++ ") " ++ op' ++ " (" ++ head b' ++ ")"]


rBinOpToString :: RBinOp -> String
rBinOpToString a = case a of
  Greater -> ">"
  Less -> "<"
  Equal -> "=="
  EqLess -> "<="
  EqGreater -> ">="