{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Compiler.Parser(langParser) where

import           Control.Applicative            hiding (some)
import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Control.Monad.State.Lazy       as S
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Void
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Debug
import           AST
import           Compiler.Lexing

sep :: Parser Text
sep = symbol ","

listParser :: Parser [AExpr]
listParser = between (symbol "[") (symbol "]") (sepBy1 aExpr sep)

rangeParser :: Parser AExpr
rangeParser = between (symbol "[") (symbol "]") p
  where
    p = do start <- aExpr
           void (symbol "..")
           Range start <$> aExpr

functionArgsParser :: Parser [FunArg]
functionArgsParser = sepBy1 p sep
  where
    p = do
      n <- identifier
      type' <-
        optional $ do
          void (symbol ":")
          matchType pItem
      return $ FunArg (fromMaybe VAuto type') n


returnParser :: Parser FunctionStmt
returnParser = lexeme p
  where
    p = do
      void (symbol "ret")
      ReturnFn <$> aExpr

functionParser :: Parser Stmt
functionParser = lexeme (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      args <- optional functionArgsParser
      type' <-
        optional $ do
          void (symbol "->")
          matchType pItem
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . Function header (fromMaybe VAuto type') args) functionStmt)

nativeParser :: Parser Stmt
nativeParser = lexeme $ do
  void $ symbol "native"
  header <- pItem
  args <- optional functionArgsParser
  type' <-
    optional $ do
      void (symbol "->")
      matchType pItem
  return $ NativeFunction header (fromMaybe VAuto type') args

blockParser :: Parser BodyBlock
blockParser = L.indentBlock scn p
  where
    p = return $ L.IndentSome Nothing return stmt'

anonymousFunctionParser :: Parser AExpr
anonymousFunctionParser = lexeme p
  where
    p = do void (symbol "\\")
           args <- optional functionArgsParser
           void (symbol "->")
           Fn True args <$> aExpr
           
anonymousFunctionBlockParser :: Parser AExpr
anonymousFunctionBlockParser = blockMark $ lexeme $ L.indentBlock scn p
-- TODO as
 where
  
   p = do void (symbol "\\")
          args <- optional functionArgsParser
          void (symbolEnd "->")
          return (L.IndentMany Nothing (return . FnBlock args) functionStmt)


skipStmt :: Parser Stmt
skipStmt = do
  void (symbol "skip")
  return Skip

linkPathParser:: Parser Stmt
linkPathParser= do
  void (symbol "link_path")
  name <- stringLiteral
  return $ LinkPath name

assignParser :: (String -> VarType -> AExpr -> a) -> Parser a
assignParser wrapper = do
  var <- identifier
  type' <-
    optional $ do
      void (symbol ":")
      matchType pItem
  void (symbol "=")
  wrapper var (fromMaybe VAuto type') <$> aExpr

matchType :: Parser String -> Parser VarType
matchType t = do
  t' <- t
  case t' of
    "int"    -> return VInt
    "void"   -> return VVoid
    "auto"   -> return VAuto
    "float"  -> return VFloat
    "string" -> return VString
    "char"   -> return VChar
    x        -> return $ VClass x

importParser :: Parser Stmt
importParser --dbg "import" $
 = L.nonIndented sc p
  where
    p = do
      void (symbol "import")
      Import <$> lexeme (sepBy1 identifier (symbol "."))

functionExecutionArgParser :: Parser [AExpr]
functionExecutionArgParser = do
  v <- optional $ sepBy1 aExpr sep
--  returns empty list if there were no args to parse
  return $ fromMaybe [] v

varParser :: Parser AExpr
varParser = do
  fun <- identifier
  args <- optional (parens functionExecutionArgParser)
  m <- optional more'
  return $ Var fun args m
  where
    more' = do
      void (symbol ".")
      varParser

ifStmtParser :: Parser Cond
ifStmtParser = lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "if")
      cond' <- bExpr
      void (symbolEnd "then")
      return (L.IndentMany Nothing (return . (cond',)) functionStmt)

elifStmtParser :: Parser Cond
elifStmtParser = lexeme (L.indentBlock scn p)
  where
    p = do
      void (symbol "elif")
      cond <- bExpr
      void (symbolEnd "then")
      return (L.IndentMany Nothing (return . (cond,)) functionStmt)

elseStmtParser :: Parser Cond
elseStmtParser = L.indentBlock scn p
  where
    p = do
      void (symbolEnd "else")
      return (L.IndentSome Nothing (return . (BoolConst True,)) functionStmt)

fullIfStmt :: Parser AExpr
fullIfStmt = do
  if' <- ifStmtParser
  elif' <- Text.Megaparsec.many elifStmtParser
  else' <- elseStmtParser
  return . If $ if' : elif' ++ [else']

classParser :: Parser Stmt
classParser = lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "class")
      name <-identifier
      gen <- optional generics
--      gen <- return Nothing
      void (symbolEnd ":")
      return (L.IndentSome Nothing (return . ClassExpr name gen) classStmt)


whileStmt :: (BExpr -> [b] -> b) -> Parser b-> Parser b
whileStmt wrapper parser= lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "while")
      name <- bExpr
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . wrapper name) parser)

forStmt :: (AExpr -> AExpr -> [b] -> b) -> Parser b-> Parser b
forStmt wrapper parser= lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "for")
      var <- guardedVar aExpr
      void (symbol "<-")
      range <- aExpr
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . wrapper var range ) parser)

guardedVar :: Parser AExpr -> Parser AExpr
guardedVar expr = do
  uExpr <- expr
  case uExpr of
    Var n e m-> return $ Var n e m
    _ -> fail "aExpr should be Var here"

stmt' :: Parser Stmt
stmt'
  = importParser
  <|> linkPathParser
  <|> nativeParser
  <|> skipStmt
--  <|> whileStmt While stmt'
--  <|> forStmt For stmt'
  <|> classParser
  <|> try (assignParser Assign)
  <|> functionParser
--  <|> CasualExpr <$> aExpr

functionStmt :: Parser FunctionStmt
functionStmt
  = returnParser 
  <|> whileStmt WhileFn functionStmt
  <|> forStmt ForFn functionStmt
  <|> try (assignParser AssignFn)
  <|> OtherFn <$> aExpr

classStmt :: Parser ClassStmt
classStmt
  = try (assignParser ClassAssign)
  <|> ((\(Function a b c d) -> Method a b c d) <$> functionParser)


langParser :: Parser AST
langParser = AST <$> (some stmt' <* eof)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

blockMark :: Parser a -> Parser a
blockMark = between (symbol "{") (symbol "}")

generics :: Parser [String]
generics = between (symbol "<") (symbol ">") (sepBy identifier sep)

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-")]
  , [InfixL (ABinary Multiply <$ symbol "*"), InfixL (ABinary Divide <$ symbol "/")]
  , [InfixL (ABinary Add <$ symbol "+"), InfixL (ABinary Subtract <$ symbol "-")]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [[Prefix (Not <$ rword "not")]
  , [InfixL (BBinary And <$ rword "and")
  , InfixL (BBinary Or <$ rword "or")]]


aTerm :: Parser AExpr
aTerm
  = parens aExpr
  <|> try varParser
  <|> try (ListVar <$> listParser)
  <|> rangeParser
  <|> try (FloatConst <$> float')
  <|> IntConst <$> integer
  <|> StringVal <$> stringLiteral
  <|> anonymousFunctionBlockParser
  <|> anonymousFunctionParser
  <|> fullIfStmt

bTerm :: Parser BExpr
bTerm
  = parens bExpr
  <|> (BoolConst True <$ rword "true")
  <|> (BoolConst False <$ rword "false")
  <|> rExpr

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  RBinary op a1 <$> aExpr

relation :: Parser RBinOp
relation
  = (Greater <$ symbol ">")
  <|> (Less <$ symbol "<")
  <|> (Equal <$ symbol "==")
  <|> (EqGreater <$ symbol ">=")
  <|> (EqLess <$ symbol "<=")

