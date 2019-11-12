{-# LANGUAGE TypeSynonymInstances #-}

module Compiler.Translator.Type where

import           AST                    (VarType(..), FunctionStmt)
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
    { global   :: [DeclarationTree]
    , local :: DeclarationTree
    , cache :: StorageCache
    }
  deriving (Show)

data DeclarationTree =
  Decl
    { decType  :: DecType
    , name     :: String
    , argTypes :: [DeclarationTree]
    , retType  :: VarType
    , children :: [DeclarationTree]
    } | EmptyDecl
  deriving (Show)

checkRetType :: VarType -> DeclarationTree -> Bool
checkRetType v (Decl _ _ _ t _) = v == t || t == VAuto 
checkRetType _ EmptyDecl = True

data DecType
  = ClassT
  | FunctionT
  | VariableT
  deriving (Show, Eq)


data StorageCache
  = EmptyCache
  | TypeCache [VarType]
  | InjectBefore [FunctionStmt]
  deriving (Show)

emptyStorage = Storage [] EmptyDecl EmptyCache

data TranslatorException =
  UnsupportedTypeException String
  deriving (Show)

instance Exception TranslatorException

