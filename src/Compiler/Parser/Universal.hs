{-# LANGUAGE OverloadedStrings #-}

module Compiler.Parser.Universal where

import           Compiler.Parser.Lexer
import           Data.Text                  (Text)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (fromMaybe)
import           AST

sep :: Parser Text
sep = symbol ","

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

blockMark :: Parser a -> Parser a
blockMark = between (symbol "{") (symbol "}")

--data Gene = Simple String | Complex [Gene]
--generics :: Parser [Gene]
--generics = between (symbol "<") (symbol ">") (sepBy ((Complex <$> generics) <|> (Simple <$> identifier)) sep)

generics :: Parser [String]
generics = between (symbol "<") (symbol ">") (sepBy identifier sep)

array = between (symbol "[") (symbol "]") 

matchType :: String -> VarType
matchType = matchType' []

matchType' :: [String] -> String -> VarType
matchType' g t =
  case t of
    "int" -> VInt
    "void" -> VVoid
    "auto" -> VAuto
    "float" -> VFloat
    "string" -> VString
    "char" -> VChar
    x -> VClass x (map (matchType' []) g) False

typeParser :: Parser VarType
typeParser = do
  t <- pItem
  gen <- fromMaybe [] <$> optional generics
  return $ matchType' gen t
