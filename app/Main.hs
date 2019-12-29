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

import Compiler.Analyzer.Type(emptyStorage)
import qualified Compiler.Translator.Type as TT
import Compiler.LexicalAnalyzer

main :: IO ()
main = do
  let path = "res/test.mard"
  let corePath = "res/Core.mard"
  input <- T.pack <$> readFile path
  inputCore <- T.pack <$> readFile corePath
  let p = parse langParser path (T.append inputCore input)
  case p of
--    Right res -> translate res
    Right res -> do 
--      print $ findAllDeclaredNames $ (\(AST x) -> x) res
      let ((a, w), s) = runState (runWriterT (analyze res)) emptyStorage
      print w
      print a
      let (a', w') = evalState (runWriterT (translate' a)) TT.emptyStorage
      let res = concat a'
      writeFile "res/out.h" res
      return ()
    Left err  -> putStrLn $ Err.errorBundlePretty err

