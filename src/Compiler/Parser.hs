{-# LANGUAGE OverloadedStrings #-}
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


returnParser :: Parser Stmt
returnParser = lexeme p
  where
    p = do
      void (symbol "ret")
      ReturnExpr <$> aExpr

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
      return (L.IndentSome Nothing (return . Function header (fromMaybe VAuto type') args) stmt')

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

skipStmt :: Parser Stmt
skipStmt = do
  void (symbol "skip")
  return Skip

assignParser :: Parser Stmt
assignParser = do
  var <- identifier
  type' <-
    optional $ do
      void (symbol ":")
      matchType pItem
  void (symbol "=")
  Assign var (fromMaybe VAuto type') <$> aExpr

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
      Import <$> lexeme stringLiteral
      
functionExecutionArgParser :: Parser [AExpr]
functionExecutionArgParser = do 
  v <- optional $ sepBy1 aExpr sep
--  returns empty list if there were no args to parse 
  return $ fromMaybe [] v

functionExecution :: Parser AExpr
functionExecution = do
  fun <- identifier
  args <- optional (parens functionExecutionArgParser)
  m <- optional more'
  return $ Var fun args m
  where
    more' = do
      void (symbol ".")
      functionExecution

ifStmtParser :: Parser IfCondBody
ifStmtParser = lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "if")
      cond' <- bExpr
      void (symbolEnd "then")
      return (L.IndentMany Nothing (return . IfCondBody cond') stmt')

elifStmtParser :: Parser IfCondBody
elifStmtParser = lexeme (L.indentBlock scn p)
  where
    p = do
      void (symbol "elif")
      cond <- bExpr
      void (symbolEnd "then")
      return (L.IndentMany Nothing (return . IfCondBody cond) stmt')

elseStmtParser :: Parser ElseCondBody
elseStmtParser = L.indentBlock scn p
  where
    p = do
      void (symbolEnd "else")
      return (L.IndentSome Nothing (return . ElseCondBody) stmt')

fullIfStmt :: Parser AExpr
fullIfStmt = do
  if' <- ifStmtParser
  elif' <- Text.Megaparsec.many elifStmtParser
  If if' elif' <$> elseStmtParser

classStmt :: Parser Stmt
classStmt = lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "class")
      name <- identifier
      void (symbolEnd ":")
      return (L.IndentSome Nothing (return . ClassExpr name) stmt')

whileStmt :: Parser Stmt
whileStmt = lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "while")
      name <- bExpr
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . While name) stmt')

forStmt :: Parser Stmt
forStmt = lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "for")
      var <- guardedVar aExpr
      void (symbol "<-")
      range <- aExpr
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . For var range ) stmt')

guardedVar :: Parser AExpr -> Parser AExpr
guardedVar expr = do
  uExpr <- expr
  case uExpr of
    Var n e m-> return $ Var n e m
    _ -> fail "aExpr should be Var here"

stmt' :: Parser Stmt
stmt'
  = importParser
  <|> returnParser
  <|> skipStmt
  <|> whileStmt
  <|> forStmt
  <|> classStmt
  <|> try assignParser
  <|> try functionParser
  <|> CasualExpr <$> aExpr

langParser :: Parser AST
langParser = AST <$> (some stmt' <* eof)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

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


--dbg "DOT PARSER" $ do
--  left <- aExpr
--  void (symbol ".")
--  right <- aExpr
--  return $ Dot left right

aTerm :: Parser AExpr
aTerm
  = parens aExpr
  <|> try functionExecution
--  <|> Var <$> identifier
  <|> try (ListVar <$> listParser)
  <|> rangeParser
  <|> try (FloatConst <$> float')
  <|> IntConst <$> integer
  <|> StringVal <$> stringLiteral
  <|> anonymousFunctionParser
  <|> fullIfStmt
--  <|> dotParser

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

