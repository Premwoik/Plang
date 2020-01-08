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
import Control.Monad(foldM)
import Data.List(intercalate)
import qualified Control.Exception as E
import Data.Void(Void)
import Debug.Trace

main :: IO ()
main = do
  let path = "res/test.mard"
  p <- tryReadAndParseFile path
  res <-  fst <$> importOthers p [path]
  let ((a, w), s) = runState (runWriterT (analyze res)) emptyStorage
  print w
  print a
  let (a', w') = evalState (runWriterT (translate' a)) TT.emptyStorage
  let res = concat a'
  writeFile "res/out.h" res
  return ()

--main = do
--  let path = "res/test.mard"
--  let corePath = "res/Core.mard"
--  input <- T.pack <$> readFile path
--  inputCore <- T.pack <$> readFile corePath
--  let p = parse langParser path (T.append inputCore input)
--  case p of
----    Right res -> translate res
--    Right res -> do
----      print $ findAllDeclaredNames $ (\(AST x) -> x) res
--      let ((a, w), s) = runState (runWriterT (analyze res)) emptyStorage
--      print w
--      print a
--      let (a', w') = evalState (runWriterT (translate' a)) TT.emptyStorage
--      let res = concat a'
--      writeFile "res/out.h" res
--      return ()
--    Left err  -> putStrLn $ Err.errorBundlePretty err
--parseFile :: String -> IO _
--parseFile path = do
--  input <- T.pack <$> readFile path
--  case parse langParser path input of

importOthers :: AST -> [String] -> IO (AST, [String])
importOthers (AST []) _ = return (AST [], [])
importOthers (AST s) imported = foldM aggregate acc (filter filterImport s)
  where
    aggregate :: (AST, [String]) -> Stmt -> IO (AST, [String])
    aggregate (asts, paths) (Import _ path) = do
      fileAst <-
        if makePath path `elem` paths
          then return (AST [])
          else tryReadAndParseFile (makePath path)
      (asts', paths') <- importOthers fileAst (makePath path : paths)
      return (appendAST asts asts', paths')
    acc = (AST s, imported)
    makePath p = "res/" ++ intercalate "/" p ++ ".mard"
    filterImport Import {} = True
    filterImport _ = False
    appendAST (AST all) (AST f) = AST $ f ++ all

newtype ParseException = ParseException (ParseErrorBundle T.Text Void) deriving (Show)
instance E.Exception ParseException

tryReadAndParseFile :: String -> IO AST
tryReadAndParseFile path = do
  input <- T.pack <$> readFile path
  case parse langParser path input of
    Right res -> return res
    Left e -> do 
      putStrLn $ Err.errorBundlePretty e
      error ""

