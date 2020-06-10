{-# LANGUAGE OverloadedStrings #-}

module Compiler.Parser.Universal where

import AST
import Compiler.Parser.Lexer
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Debug.Trace
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

sep :: Parser Text
sep = symbol ","

parens :: Parser a -> Parser a
parens = addContext "Maybe ending bracket - \")\" is missing." . between (symbol "(") (symbol ")")

blockMark :: Parser a -> Parser a
blockMark = between (symbol "{") (symbol "}")

functionExecutionArgParser :: Parser AExpr -> Parser [AExpr]
functionExecutionArgParser aExpr = do
    v <- addContext "Items need to be separated by comma eg: a, b, c." 
      . addContext "Comma cannot be left alone at the end. ;]" 
      . optional $ sepBy1 aExpr sep
    return $ fromMaybe [] v

generics :: Parser [String]
generics = between (symbol "<") (symbol ">") (sepBy identifier sep)

generics' :: Parser [VarType]
generics' = between (symbol "<") (symbol ">") (sepBy typeParser sep)

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
typeParser = addContext "Generics fail" $ do
  access <- fromMaybe [] <$> optional (refParser <|> copyParser <|> pointerParser <|> cPointerParser)
  t <- identifier
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

