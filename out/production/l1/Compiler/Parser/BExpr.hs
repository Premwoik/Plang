{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.BExpr where
import AST
import Compiler.Parser.Lexer

import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L

rExpr :: Parser AExpr -> Parser BExpr
rExpr aExpr= do
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


