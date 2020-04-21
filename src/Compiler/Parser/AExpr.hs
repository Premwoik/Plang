{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Compiler.Parser.AExpr where

import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           AST
import           Compiler.Parser.Lexer
import           Compiler.Parser.Universal
import           Control.Monad
import           Data.Maybe                 (fromMaybe)
import           Debug.Trace

bracketParser :: Parser AExpr -> Parser AExpr
bracketParser aExpr = do
  o <- getOffset
  p <- parens aExpr
  return $ ABracket o p

anonymousFunctionParser :: Parser [FunArg] -> Parser AExpr -> Parser AExpr
anonymousFunctionParser functionArgsParser aExpr = lexeme p
  where
    p = do
      o <- getOffset
      void (symbol "\\")
      args <- optional functionArgsParser
      void (symbol "->")
      Fn o True args <$> aExpr

anonymousFunctionBlockParser :: Parser [FunArg] -> Parser FunctionStmt -> Parser AExpr
anonymousFunctionBlockParser functionArgsParser functionStmt = blockMark $ lexeme $ L.indentBlock scn p
  where
    p = do
      o <- getOffset
      void (symbol "\\")
      args <- optional functionArgsParser
      void (symbolEnd "->")
      return (L.IndentMany Nothing (return . FnBlock o args) functionStmt)

-- TODO as

--  returns empty list if there were no args to parse
scopeMarkParser :: Parser AExpr -> Parser AExpr
scopeMarkParser aExpr = do
  o <- getOffset
  s <- identifier
  void (symbol "|")
  var <- varExtendedParser aExpr
  return $ ScopeMark o s var

varParser :: Parser AExpr -> Parser AExpr
varParser aExpr = do
  o <- getOffset
  fun <- identifier
  args <- optional (parens (functionExecutionArgParser aExpr))
  m <- optional more'
  return $ Var o fun [] args m
  where
    more' = do
      void (symbol ".")
      varParser aExpr

varLeftAssignParser :: Parser AExpr -> Parser AExpr
varLeftAssignParser aExpr = do
  o <- getOffset
  fun <- identifier
  gen <- fromMaybe [] <$> optional generics'
  args <- optional (parens (functionExecutionArgParser aExpr))
  accessor <- optional accParser
  accO <- getOffset
  m <- optional more'
  return $ case (accessor, m) of
    (Just a, Just {}) ->
      Var o fun gen args (Just (Var accO "get" [] (Just a) m))
    (Just a, Nothing) ->
      Var o fun gen args (Just (Var accO "set" [] (Just a) m))
    (Nothing, _) -> 
      Var o fun gen args m
  where
    accParser = 
      between (symbol "[") (symbol "]") (functionExecutionArgParser aExpr)
    more' = do
      void (symbol ".")
      varParser aExpr


--  trace (show fun) $ return ()
varExtendedParser :: Parser AExpr -> Parser AExpr
varExtendedParser aExpr = do
  o <- getOffset
  fun <- identifier
  gen <- fromMaybe [] <$> optional generics'
  args <- optional (parens (functionExecutionArgParser aExpr))
  accessor <- optional accParser
  m <- optional more'
  return $ case accessor of
    Just a ->
      Var o fun gen args (Just (Var o "get" [] (Just a) m))
    Nothing -> 
      Var o fun gen args m
  where
    accParser = 
      between (symbol "[") (symbol "]") (functionExecutionArgParser aExpr)
    more' = do
      void (symbol ".")
      varParser aExpr


ifStmtParser :: Parser BExpr -> Parser FunctionStmt -> Parser Cond
ifStmtParser bExpr functionStmt = lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "if")
      cond' <- bExpr
      void (symbolEnd "then")
      return (L.IndentMany Nothing (return . (cond', )) functionStmt)

elifStmtParser :: Parser BExpr -> Parser FunctionStmt -> Parser Cond
elifStmtParser bExpr functionStmt = lexeme (L.indentBlock scn p)
  where
    p = do
      void (symbol "elif")
      cond <- bExpr
      void (symbolEnd "then")
      return (L.IndentMany Nothing (return . (cond, )) functionStmt)

elseStmtParser :: Parser FunctionStmt -> Parser Cond
elseStmtParser functionStmt = L.indentBlock scn p
  where
    p = do
      void (symbolEnd "else")
      return (L.IndentSome Nothing (return . (BoolConst True, )) functionStmt)

fullIfStmt :: Parser BExpr -> Parser FunctionStmt -> Parser AExpr
fullIfStmt bExpr functionStmt = do
  o <- getOffset
  if' <- ifStmtParser bExpr functionStmt
  elif' <- Text.Megaparsec.many (elifStmtParser bExpr functionStmt)
  else' <- elseStmtParser functionStmt
  return . If o $ if' : elif' ++ [else']

guardedVar :: Parser AExpr -> Parser AExpr
guardedVar expr = do
  uExpr <- expr
  case uExpr of
    v@Var {} -> return v
    _           -> fail "aExpr should be Var here"

listParser :: Parser AExpr -> Parser AExpr
listParser aExpr = between (symbol "[") (symbol "]") p
  where
    p = do
      t <-
        optional . try $ do
          t <- typeParser
          void (symbol "|")
          return t
      o <- getOffset
      args <- sepBy aExpr sep
      return $ ListVar o args t

rangeParser :: Parser AExpr -> Parser AExpr
rangeParser aExpr = between (symbol "[") (symbol "]") p
  where
    p = do
      o <- getOffset
      start <- aExpr
      step <- optional $ do
        void (symbol ",")
        aExpr
      void (symbol "..")
      Range o step start <$> aExpr

intParser :: Parser AExpr
intParser = do
  o <- getOffset
  IntConst o <$> integer

floatParser :: Parser AExpr
floatParser = do
  o<- getOffset
  FloatConst o <$> float'

stringParser :: Parser AExpr
stringParser = do
  o <- getOffset
  StringVal o <$> stringLiteral

--aBoolParser :: Parser AExpr
--aBoolParser = do
--  return $ ABool $ BoolConst
  