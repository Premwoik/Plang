{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text                  as T
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Debug
import           Text.Megaparsec.Error      as Err

import           AST
import           Compiler.Parser
import           Compiler.Translator
import Control.Monad.Writer(runWriter)

main :: IO ()
main = do
  let path = "res/test"
  input <- T.pack <$> readFile path
  let p = parse langParser path input
  case p of
--    Right res -> translate res
    Right res -> do 
      let (a, w) = runWriter (translate' res)
      let res = concat a
      writeFile "res/out.h" res
      print w
--      print res
      return ()
    Left err  -> putStrLn $ Err.errorBundlePretty err
