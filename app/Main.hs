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
import Compiler

main :: IO ()
main = do
  let path = "res/test.mard"
  res <- compile path 
  print $ fst res
  print $ snd res
  writeFile "res/out.h" $ concat $ snd res
 
--compile :: String -> IO ()

--translate

newtype ParseException = ParseException (ParseErrorBundle T.Text Void) deriving (Show)
instance E.Exception ParseException

