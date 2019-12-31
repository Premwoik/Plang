{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Compiler.Parser.Statement where

import           Compiler.Parser.Universal
import           Control.Monad.State        (modify)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           AST
import           Compiler.Parser.AExpr      hiding (elifStmtParser,
                                             elseStmtParser, ifStmtParser)
import           Compiler.Parser.Lexer
import           Control.Monad
import           Data.Maybe                 (fromMaybe)
import Text.Megaparsec.Debug(dbg)

importParser :: Parser Stmt
importParser --dbg "import" $
 = L.nonIndented sc p
  where
    p = do
      o <- getOffset
      void (symbol "import")
      Import o <$> lexeme (sepBy1 identifier (symbol "."))

functionArgsParser :: Parser [FunArg]
functionArgsParser = sepBy1 p sep
  where
    p = do
      n <- identifier
      type' <-
        optional $ do
          void (symbol ":")
          matchType <$> pItem
      return $ FunArg (fromMaybe VAuto type') n

functionParser :: Parser FunctionStmt -> Parser Stmt
functionParser functionStmt = lexeme (L.indentBlock scn p)
  where
    p = do
      o <- getOffset
      header <- pItem
      args <- optional functionArgsParser
      type' <-
        optional $ do
          void (symbol "->")
          matchType <$> pItem
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . Function o header (fromMaybe VAuto type') args) functionStmt)

nativeFunctionParser :: Parser Stmt
nativeFunctionParser =
  lexeme $ do
    o <- getOffset
    void $ symbol "native"
    path <- lexeme stringLiteral
    header <- pItem
    args <- optional functionArgsParser
    type' <-
      optional $ do
        void (symbol "->")
        matchType <$> pItem
    return $ NativeFunction o path header (fromMaybe VAuto type') args

returnParser :: Parser AExpr -> Parser FunctionStmt
returnParser aExpr = lexeme p
  where
    p = do
      o <- getOffset
      void (symbol "ret")
      ReturnFn o <$> aExpr

skipStmt :: Parser Stmt
skipStmt = do
  o <- getOffset
  void (symbol "skip")
  return $ Skip o

linkPathParser :: Parser Stmt
linkPathParser = lexeme $ L.nonIndented sc $ do
  o <- getOffset
  void (symbol "link_path")
  name <- lexemeEnd stringLiteral
  return $ LinkPath o name



assignParser :: Parser AExpr -> (Int -> String -> VarType -> AExpr -> a) -> Parser a
assignParser aExpr wrapper = lexeme $ do
  o <- getOffset
  var <- identifier
  type' <-
    optional $ do
      void (symbol ":")
      typeParser
  void (symbol "=")
  wrapper o var (fromMaybe VAuto type') <$> aExpr
  
nativeAssignDeclParser :: Parser Stmt
nativeAssignDeclParser = lexeme $ do
  o <- getOffset
  void (symbol "native var")
  path <- lexeme stringLiteral
  name <- identifier
  type' <-
    optional $ do
      void (symbol ":")
      matchType <$> pItem
  return $ NativeAssignDeclaration o path name (fromMaybe VAuto type')
  

classAssignParser :: Parser AExpr -> (Int -> String -> VarType -> AExpr -> a) -> Parser a
classAssignParser aExpr wrapper = do
  o <- getOffset
  var <- identifier
  type' <-
    optional $ do
      void (symbol ":")
      typeParser
  void (symbol "=")
  wrapper o var (fromMaybe VAuto type') <$> aExpr


methodDeclarationParser :: Parser ClassStmt
methodDeclarationParser =
  lexeme $ do
    o <- getOffset
    void (symbol "def")
    header <- pItem
    args <- optional functionArgsParser
    type' <-
      optional $ do
        void (symbol "->")
        matchType <$> pItem
    return $ MethodDeclaration o header (fromMaybe VAuto type') args


classParser :: Parser ClassStmt -> Parser Stmt
classParser classStmt = lexeme $ L.indentBlock scn p
  where
    p = do
      o <- getOffset
      void (symbol "class")
      name <- identifier
      gen <- optional generics
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . ClassExpr o name gen) classStmt)

--      gen <- return Nothing
nativeClassParser :: Parser ClassStmt -> Parser Stmt
nativeClassParser classStmt = lexeme $ L.indentBlock scn p
  where
    p = do
      o <- getOffset
      void (symbol "native class")
      path <- lexeme stringLiteral
      name <- identifier
      gen <- optional generics
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . NativeClass o path name gen) classStmt)

whileStmt :: Parser BExpr -> (Int -> BExpr -> [b] -> b) -> Parser b -> Parser b
whileStmt bExpr wrapper parser = lexeme $ L.indentBlock scn p
  where
    p = do
      o <- getOffset
      void (symbol "while")
      name <- bExpr
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . wrapper o name) parser)

forStmt :: Parser AExpr -> (Int -> AExpr -> AExpr -> [b] -> b) -> Parser b -> Parser b
forStmt aExpr wrapper parser = lexeme $ L.indentBlock scn p
  where
    p = do
      o <- getOffset
      void (symbol "for")
      var <- guardedVar aExpr
      void (symbol "<-")
      range <- aExpr
      void (symbolEnd "do")
      return (L.IndentSome Nothing (return . wrapper o var range) parser)

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

fullIfFuncParser :: Parser BExpr -> Parser FunctionStmt -> Parser FunctionStmt
fullIfFuncParser bExpr functionStmt = do
  o <- getOffset
  if' <- ifStmtParser bExpr functionStmt
  elif' <- Text.Megaparsec.many (elifStmtParser bExpr functionStmt)
  else' <- elseStmtParser functionStmt
  return . IfFn o $ if' : elif' ++ [else']
