{-# LANGUAGE TypeSynonymInstances #-}

module Compiler.Translator.Type where

import           AST                    (FunctionStmt, VarType (..))
import           Control.Exception
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader   (ReaderT)
import           Control.Monad.State
import           Control.Monad.Writer   (WriterT)
import qualified Data.Map               as Map

type Translator = Translator' [String]

type RTranslator t = RTranslator' t [String]

type RTranslator' t a = ReaderT t Translator' a

type Translator' = WriterT [String] (State Storage)

type DPair = (VarType, ExecutableType)

-- vars, funcDec, classDec
data ExecutableType
  = ClassDecl
  | FunctionDecl
  | Instance
  deriving (Show)

data Storage =
  Storage
    { toImport :: [String]
    }
  deriving (Show)

emptyStorage = Storage []

addImport :: String -> Translator
addImport n = do
  modify (\s -> s {toImport = n : toImport s})
  return []

data TranslatorException =
  UnsupportedTypeException String
  deriving (Show)

instance Exception TranslatorException
