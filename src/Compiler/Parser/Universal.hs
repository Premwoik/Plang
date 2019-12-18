{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Universal where

import Data.Text(Text)
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import Compiler.Parser.Lexer

import AST

sep :: Parser Text
sep = symbol ","

        
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

blockMark :: Parser a -> Parser a
blockMark = between (symbol "{") (symbol "}")

generics :: Parser [String]
generics = between (symbol "<") (symbol ">") (sepBy identifier sep)
        
           
matchType :: Parser String -> Parser VarType
matchType t = do
  t' <- t
  case t' of
    "int"    -> return VInt
    "void"   -> return VVoid
    "auto"   -> return VAuto
    "float"  -> return VFloat
    "string" -> return VString
    "char"   -> return VChar
    x        -> return $ VClass x

