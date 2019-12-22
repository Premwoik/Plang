{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Statement where

import Compiler.Parser.Universal
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import Control.Monad.State(modify)

import AST
import Compiler.Parser.Lexer
import Control.Monad
import Data.Maybe(fromMaybe)
import Compiler.Parser.AExpr

importParser :: Parser Stmt
importParser --dbg "import" $
 = L.nonIndented sc p
  where
    p = do
      void (symbol "import")
      Import <$> lexeme (sepBy1 identifier (symbol "."))


--blockParser :: Parser Stmt -> Parser BodyBlock
--blockParser stmt = L.indentBlock scn p
--  where
--    p = return $ L.IndentSome Nothing return stmt

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

--scopeParser :: String -> Parser b -> Parser ([b] -> a) -> Parser a
--scopeParser name stmt p = lexeme (L.indentBlock scn p')
--  where
--    p' = do
--      wrapper <- p
--      modify (addScope name)
--      let scopes = L.IndentSome Nothing (return . wrapper) stmt
--      modify popScope
--      return scopes
--      
--functionDeclParser:: Parser ([FunctionStmt] ->  Stmt)
--functionDeclParser = do
--  header <- pItem
--  args <- optional functionArgsParser
--  type' <-
--    optional $ do
--      void (symbol "->")
--      matchType pItem
--  void (symbolEnd "do")
--  return (Function header (fromMaybe VAuto type') args)
--  

functionParser :: Parser FunctionStmt -> Parser Stmt
functionParser functionStmt = lexeme (L.indentBlock scn p)
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

returnParser :: Parser AExpr -> Parser FunctionStmt
returnParser aExpr = lexeme p
  where
    p = do
      void (symbol "ret")
      ReturnFn <$> aExpr


skipStmt :: Parser Stmt
skipStmt = do
  void (symbol "skip")
  return Skip


linkPathParser:: Parser Stmt
linkPathParser= do
  void (symbol "link_path")
  name <- stringLiteral
  return $ LinkPath name


assignParser :: Parser AExpr -> (String -> VarType -> AExpr -> a) -> Parser a
assignParser aExpr wrapper = do
  var <- identifier
  type' <-
    optional $ do
      void (symbol ":")
      matchType pItem
  void (symbol "=")
  wrapper var (fromMaybe VAuto type') <$> aExpr


classAssignParser :: Parser AExpr -> (String -> VarType -> AExpr -> a) -> Parser a
classAssignParser aExpr wrapper = do
  var <- identifier
  type' <-
    optional $ do
      void (symbol ":")
      matchType pItem
  void (symbol "=")
  wrapper var (fromMaybe VAuto type') <$> aExpr

classParser :: Parser ClassStmt -> Parser Stmt
classParser classStmt = lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "class")
      name <-identifier
      gen <- optional generics
--      gen <- return Nothing
      void (symbolEnd ":")
      return (L.IndentSome Nothing (return . ClassExpr name gen) classStmt)


whileStmt :: Parser BExpr -> (BExpr -> [b] -> b) -> Parser b-> Parser b
whileStmt bExpr wrapper parser= lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "while")
      name <- bExpr
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . wrapper name) parser)

forStmt :: Parser AExpr -> (AExpr -> AExpr -> [b] -> b) -> Parser b-> Parser b
forStmt aExpr wrapper parser= lexeme $ L.indentBlock scn p
  where
    p = do
      void (symbol "for")
      var <- guardedVar aExpr
      void (symbol "<-")
      range <- aExpr
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . wrapper var range ) parser)

