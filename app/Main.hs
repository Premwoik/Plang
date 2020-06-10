{-# LANGUAGE OverloadedStrings #-}

module Main where

import Compiler
import qualified CLI

main = CLI.main
--
--main :: IO ()
--main = do
--  let path = "Main"
--  res <- compile "res/" path
--  print $ fst res
--  print $ snd res
--  writeFile "res/out.h" $ concat $ snd res

