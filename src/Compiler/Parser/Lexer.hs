{-# LANGUAGE OverloadedStrings #-}

module Compiler.Parser.Lexer where

import           AST
import           Control.Applicative        hiding (some)
import           Control.Monad
import           Control.Monad.State.Lazy   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe(fromMaybe)
import Debug.Trace

lineCmnt = L.skipLineComment "//"

blockCmnt = L.skipBlockComment "/*" "*/"

scn :: Parser ()
scn = L.space space1 lineCmnt blockCmnt

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineCmnt blockCmnt

rws :: [String] -- list of reserved words
rws = ["if", "then", "else", "while", "do", "skip", "true", "false", "not", "and", "or", "do"]

rword :: String -> Parser ()
rword w = (lexeme . try) (string (T.pack w) *> notFollowedBy alphaNumChar)

  
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> Control.Applicative.many alphaNumChar
    check x =
      if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier or type"
        else return x

lexemeEnd :: Parser a -> Parser a
lexemeEnd = L.lexeme sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme scn

symbol :: Text -> Parser Text
symbol = L.symbol scn

symbolEnd :: Text -> Parser Text
symbolEnd = L.symbol sc


integer :: Parser Integer
integer = lexemeEnd L.decimal

float' :: Parser Float
float' = lexemeEnd L.float

pItem :: Parser String
pItem = lexemeEnd (some (hidden (alphaNumChar <|> char '-' <|> char '_')) <?> "type")

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')
