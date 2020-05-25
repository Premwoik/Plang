{-# LANGUAGE OverloadedStrings #-}

module Compiler.Parser.Universal where

import           Compiler.Parser.Lexer
import           Data.Text                  (Text)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (fromMaybe)
import           AST
import Control.Monad (void)
import Debug.Trace

sep :: Parser Text
sep = symbol ","

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

blockMark :: Parser a -> Parser a
blockMark = between (symbol "{") (symbol "}")

functionExecutionArgParser :: Parser AExpr -> Parser [AExpr]
functionExecutionArgParser aExpr = do
  v <- optional $ sepBy1 aExpr sep
  return $ fromMaybe [] v

identifiers :: Parser AExpr -> Parser [String]
-- TODO add parsing for list accessor <?
identifiers aExpr = do
  scope <- optional . try $ do
    s <- identifier
    void (symbol "|")
    return s
  ids<- sepBy identifier "."
  return $ fromMaybe "" scope : ids
  where
    id = do
      i <- identifier
      optional $ 
        between (symbol "[") (symbol "]") (functionExecutionArgParser aExpr)


generics :: Parser [String]
generics = between (symbol "<") (symbol ">") (sepBy identifier sep)

generics' :: Parser [VarType]
generics' = between (symbol "<") (symbol ">") (sepBy typeParser sep)
--  where
--    p = do
--      id <- typeParser
--      gens <- optional generics'
--      case gens of
--        Just g -> return $ matchType' g id
--        Nothing -> return $ matchType id

array = between (symbol "[") (symbol "]") 

matchTypeWithAccess :: [VarType] -> String -> String -> VarType
matchTypeWithAccess g t "ref" = VRef $ matchType' g t
matchTypeWithAccess g t "copy" = VCopy $ matchType' g t
matchTypeWithAccess g t "ptr" = VPointer (matchType' g t) SharedPtr
matchTypeWithAccess g t "cptr" = VPointer (matchType' g t) NativePtr 
matchTypeWithAccess g t "" = matchType' g t

matchType :: String -> VarType
matchType = matchType' []

matchType' :: [VarType] -> String -> VarType
matchType' g t =
  case t of
    "int" -> VInt
    "uint8" -> VNum NUInt8
    "uint16" -> VNum NUInt16
    "uint32" -> VNum NUInt32
    "int8" -> VNum NInt8
    "int16" -> VNum NInt16
    "int32" -> VNum NInt32
    "void" -> VVoid
    "auto" -> VAuto
    "bool" -> VBool
    "float" -> VFloat
    "string" -> VString
    "char" -> VChar
    "list" -> VClass (VName "ArrayList") g
    "fn" -> VFn g
    x -> VClass (VName x) g

typeParser :: Parser VarType
typeParser = do
  access <- fromMaybe [] <$> optional (refParser <|> copyParser <|> pointerParser <|> cPointerParser)
  t <- pItem
  gen <- fromMaybe [] <$> optional generics'
  return $ matchTypeWithAccess gen t access
  where
    refParser = do
      void (symbol "ref")
      return "ref"
    copyParser = do
      void (symbol "copy")
      return "copy"
    pointerParser = do
      void (symbol "ptr")
      return "ptr"
    cPointerParser = do
      void (symbol "cptr")
      return "cptr"
