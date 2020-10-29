{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Compiler.Parser.Lexer where

import           Compiler.Parser.Type
import           Control.Applicative        hiding (some)
import           Control.Monad
import           Control.Monad.State.Lazy   as S
import qualified Data.Char                  as Char
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

charLiteral :: (MonadParsec e s m, Token s ~ Char) => m String
charLiteral =
  label "literal character" $
  -- The @~@ is needed to avoid requiring a MonadFail constraint,
  -- and we do know that r will be non-empty if count' succeeds.
   do
    r <- lookAhead (count' 1 10 anySingle)
    case listToMaybe (Char.lexLitChar r) of
      Just (c, r') -> c <$ skipCount (length r - length r') anySingle
      Nothing      -> unexpected (Tokens (head r :| []))

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill charLiteral' (char '\"')
  where
    charLiteral' :: Parser Char
    charLiteral' = do
      p <- charLiteral
      if p == "\n"
        then fail "unexpectd \"\\n\"\nString literal must be in one line!"
        else return $ head p
