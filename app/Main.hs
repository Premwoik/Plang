{-# LANGUAGE OverloadedStrings #-}

module Main where

import           AST
import qualified CLI
import           Compiler
import           Text.Megaparsec hiding (State)
import Text.Megaparsec.Byte

main = CLI.main
--
--main :: IO ()
--main = do
--  return ()
--  let path = "Main"
--  res <- compile "res/" path
--  print $ fst res
--  print $ snd res
--  writeFile "res/out.h" $ concat $ snd res
