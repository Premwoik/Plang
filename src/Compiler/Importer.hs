module Compiler.Importer where

import qualified Data.Text                as T
import           Text.Megaparsec          hiding (State)
import           Text.Megaparsec.Debug
import           Text.Megaparsec.Error    as Err

import           AST
import           Compiler.Parser
import           Compiler.Translator
import           Control.Monad.State      (evalState, runState, runStateT)
import           Control.Monad.Writer     (runWriterT)

import           Compiler.Analyzer.Type   (emptyStorage)
import qualified Compiler.Translator.Type as TT
import qualified Control.Exception        as E
import           Control.Monad            (foldM, zipWithM)
import           Data.List                (intercalate)
import           Data.Void                (Void)
import           Debug.Trace
import qualified Data.Map as Map
import Data.List(sortBy, find)
import Data.Maybe(fromJust)


type Counter = Map.Map String Int
snd3 (_, y, _) = y
trd (_, _, z) = z

importMain :: AST -> IO [Imported]
importMain main = do 
  res <- importAll 1 [] "main" (Map.fromList [("main", 0)]) main 
  let files = snd3 res
  let order = map fst . sortBy (\(_, v1) (_, v2) -> compare v2 v1) . Map.toList $ trd res
  return $ map (\n -> fromJust (find (\(IFile n' _) -> n == n') files)) order

importAll :: Int -> [String] -> String -> Counter -> AST -> IO ([String], [Imported], Counter)
importAll depth loaded n counter main = do
  let allImports = filterImport main
  let counter' = foldl (\acc i -> Map.insert i depth acc) counter . map getImportName $ allImports
  let notLoadedImports = filterNotLoaded loaded allImports
  let paths = map getImportPath notLoadedImports
  let names = map getImportName notLoadedImports
  let loaded' = names ++ loaded
  asts <- mapM tryReadAndParseFile paths
  res <-
    foldM (\(ld, res, c) (n', a) -> (\(ld', res', c') -> (ld', res ++ res', c')) <$> importAll (depth+1) ld n' c a) (loaded', [], counter') $
    zip names asts
  let (fLoaded, files, counter'') = res
  return (fLoaded, IFile n main : files, counter'')

--  zipWithM importAll names asts
filterNotLoaded :: [String] -> [Stmt] -> [Stmt]
filterNotLoaded loaded = filter (\x -> getImportName x `notElem` loaded)

getImportName :: Stmt -> String
getImportName (Import _ path) = intercalate "" path

getImportPath :: Stmt -> String
getImportPath (Import _ path) = "res/" ++ intercalate "/" path ++ ".mard"

filterImport :: AST -> [Stmt]
filterImport (AST stmt) = filter cond stmt
  where
    cond (Import o path) = True
    cond _               = False


tryReadAndParseFile :: String -> IO AST
tryReadAndParseFile path = do
  input <- T.pack <$> readFile path
  case parse langParser path input of
    Right res -> return res
    Left e -> do
      putStrLn $ Err.errorBundlePretty e
      error ""
