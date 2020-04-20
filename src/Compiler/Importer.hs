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
type Memory = Map.Map String [String]
snd3 (_, y, _) = y
trd (_, _, z) = z

importMain :: String -> IO [Imported]
importMain path = do
  main <- tryReadAndParseFile path
  res <- importAll [] "main" path Map.empty main
  let files = snd3 res
  let order = map fst . sortBy (\(_, v1) (_, v2) -> compare v2 v1) . Map.toList . countOccurrence $ trd res
  trace (show (order)) $ return ()
  return $ map (\n -> fromJust (find (\(IFile n' _ _) -> n == n') files)) order

countOccurrence :: Memory -> Counter
countOccurrence mem = count startKey 1 (Map.fromList [("main", 0)])
  where
    startKey = fromJust . Map.lookup "main" $ mem
    get k = fromJust . Map.lookup k $ mem
    count i depth c = foldl (\c k -> count (get k) (depth + 1) (updateCounter k depth c)) c i
    updateCounter k v c= case Map.lookup k c of
      Just v' -> if v' < v then Map.insert k v c else c
      Nothing -> Map.insert k v c
    
importAll :: [String] -> String -> String -> Memory -> AST -> IO ([String], [Imported], Memory)
importAll loaded name path mem main = do
  let allImports = filterImport main
  let mem' = Map.insert name (map getImportName allImports) mem
  let notLoadedImports = filterNotLoaded loaded allImports
  let paths = map getImportPath notLoadedImports
  let names = map getImportName notLoadedImports
  let loaded' = names ++ loaded
  asts <- mapM tryReadAndParseFile paths
  res <- foldM (\(ld, res, c) (n', p', a) -> (\(ld', res', c') -> (ld', res ++ res', c')) <$> importAll ld n' p' c a) (loaded', [], mem') $ zip3 names paths asts
  let (fLoaded, files, counter'') = res
  return (fLoaded, IFile name path main : files, counter'')


--  zipWithM importAll names asts
filterNotLoaded :: [String] -> [Stmt] -> [Stmt]
filterNotLoaded loaded = filter (\x -> getImportName x `notElem` loaded)

getImportAlias:: Stmt -> String
getImportAlias (Import _ alias _) = alias

getImportName :: Stmt -> String
getImportName (Import _ _ path) = intercalate "" path

getImportPath :: Stmt -> String
getImportPath (Import _ _ path) = "res/" ++ intercalate "/" path ++ ".mard"

filterImport :: AST -> [Stmt]
filterImport (AST stmt) = filter cond stmt
  where
    cond Import {}       = True
    cond _               = False

tryReadAndParseFile :: String -> IO AST
tryReadAndParseFile path = do
  input <- T.pack <$> readFile path
  case parse langParser path input of
    Right res -> return res
    Left e -> do
      putStrLn $ Err.errorBundlePretty e
      error ""

