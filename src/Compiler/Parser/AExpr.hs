{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Compiler.Parser.AExpr where

import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

import AST
import Compiler.Parser.Lexer
import Control.Monad
import Compiler.Parser.Universal
import Data.Maybe(fromMaybe)
import Debug.Trace


bracketParser :: Parser AExpr -> Parser AExpr
bracketParser aExpr = do 
  p <- parens aExpr
  trace "BRACKET" return () 
  return $ ABracket p

anonymousFunctionParser :: Parser [FunArg] -> Parser AExpr -> Parser AExpr
anonymousFunctionParser functionArgsParser aExpr = lexeme p
  where
    p = do void (symbol "\\")
           args <- optional functionArgsParser
           void (symbol "->")
           Fn True args <$> aExpr
           
anonymousFunctionBlockParser :: Parser [FunArg] -> Parser FunctionStmt-> Parser AExpr
anonymousFunctionBlockParser functionArgsParser functionStmt 
  = blockMark $ lexeme $ L.indentBlock scn p
-- TODO as
 where
   p = do void (symbol "\\")
          args <- optional functionArgsParser
          void (symbolEnd "->")
          return (L.IndentMany Nothing (return . FnBlock args) functionStmt)



functionExecutionArgParser :: Parser AExpr -> Parser [AExpr]
functionExecutionArgParser aExpr = do
  v <- optional $ sepBy1 aExpr sep
--  returns empty list if there were no args to parse
  return $ fromMaybe [] v

varParser :: Parser AExpr -> Parser AExpr
varParser aExpr = do
  fun <- identifier
  args <- optional (parens (functionExecutionArgParser aExpr))
  m <- optional more'
  return $ Var fun [] args m
  where
    more' = do
      void (symbol ".")
      varParser aExpr
      
varExtendedParser :: Parser AExpr -> Parser AExpr
varExtendedParser aExpr = do
  fun <- identifier
  gen <- map matchType . fromMaybe [] <$> optional generics
  args <- optional (parens (functionExecutionArgParser aExpr))
  m <- optional more'
  return $ Var fun gen args m
  where
    more' = do
      void (symbol ".")
      varParser aExpr


ifStmtParser :: Parser BExpr -> Parser FunctionStmt ->  Parser Cond
ifStmtParser bExpr functionStmt= lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "if")
      cond' <- bExpr
      void (symbolEnd "then")
      return (L.IndentMany Nothing (return . (cond',)) functionStmt)

elifStmtParser :: Parser BExpr -> Parser FunctionStmt -> Parser Cond
elifStmtParser bExpr functionStmt = lexeme (L.indentBlock scn p)
  where
    p = do
      void (symbol "elif")
      cond <- bExpr
      void (symbolEnd "then")
      return (L.IndentMany Nothing (return . (cond,)) functionStmt)

elseStmtParser :: Parser FunctionStmt -> Parser Cond
elseStmtParser functionStmt = L.indentBlock scn p
  where
    p = do
      void (symbolEnd "else")
      return (L.IndentSome Nothing (return . (BoolConst True,)) functionStmt)

fullIfStmt :: Parser BExpr -> Parser FunctionStmt -> Parser AExpr
fullIfStmt bExpr functionStmt= do
  if' <- ifStmtParser bExpr functionStmt
  elif' <- Text.Megaparsec.many (elifStmtParser bExpr functionStmt)
  else' <- elseStmtParser functionStmt
  return . If $ if' : elif' ++ [else']


guardedVar :: Parser AExpr -> Parser AExpr
guardedVar expr = do
  uExpr <- expr
  case uExpr of
    Var n g e m-> return $ Var n g e m
    _ -> fail "aExpr should be Var here"
    
listParser :: Parser AExpr -> Parser [AExpr]
listParser aExpr = between (symbol "[") (symbol "]") (sepBy1 aExpr sep)

rangeParser :: Parser AExpr -> Parser AExpr
rangeParser aExpr = between (symbol "[") (symbol "]") p
  where
    p = do start <- aExpr
           void (symbol "..")
           Range start <$> aExpr
   
