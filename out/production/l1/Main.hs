{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text                  as T
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Debug
import           Text.Megaparsec.Error      as Err

import           AST
import           Compiler.Parser
import           Compiler.Translator
import Control.Monad.Writer(runWriterT)
import Control.Monad.State(evalState, runState, runStateT)
import Control.Monad.Reader(runReaderT)
import Compiler.Analyzer.Type(emptyStorage)
import qualified Compiler.Translator.Type as TT
import Compiler.LexicalAnalyzer
import Control.Monad(foldM)
import Data.List(intercalate)
import qualified Control.Exception as E
import Data.Void(Void)
import Debug.Trace
import Compiler.Importer


main :: IO ()
main = do
  let path = "res/test.mard"
  p <- tryReadAndParseFile path
  res <- importMain p
  let (a, w) = evalState (runWriterT (analyze' res)) emptyStorage
  print w
  print a
  let (a', w') = evalState (runWriterT (runReaderT (translate' a) getDependencies)) TT.emptyStorage
  writeFile "res/out.h" $ concat a'
  return ()



newtype ParseException = ParseException (ParseErrorBundle T.Text Void) deriving (Show)
instance E.Exception ParseException

