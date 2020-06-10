module Compiler.Importer where

import qualified Data.Text                as T
import           Text.Megaparsec          (parse)
import qualified Text.Megaparsec.Error    as Err

import           AST
import           Compiler.Parser
import           Compiler.Translator
import           Control.Monad.State      (evalState, runState, runStateT)
import           Control.Monad.Writer     (runWriterT)

import           Compiler.Analyzer.Type   (emptyStorage)
import qualified Compiler.Translator.Type as TT
import Control.Exception(tryJust, try)
import           Control.Monad            (foldM, zipWithM)
import           Data.List                (intercalate)
import           Data.List                (find, sortBy)
import qualified Data.Map                 as Map
import           Data.Maybe               (fromJust)
import           Data.Void                (Void)
import           Debug.Trace
import           DirUtil                  (getExePath)
import System.IO.Error

type Counter = Map.Map String Int

type Memory = Map.Map String [String]

snd3 (_, y, _) = y

trd (_, _, z) = z

importMain :: String -> String -> IO [Imported]
importMain dir mainFileName = do
  main <- tryReadAndParseFile (dir ++ mainFileName ++ ".mard")
  res <- importAll [] mainFileName dir (dir ++ "Main.mard") Map.empty main
  let files = snd3 res
  let order = map fst . sortBy (\(_, v1) (_, v2) -> compare v2 v1) . Map.toList . countOccurrence mainFileName $ trd res
  return $ map (\n -> fromJust (find (\(IFile n' _ _) -> n == n') files)) order

countOccurrence :: String -> Memory -> Counter
countOccurrence mainFile mem = count startKey 1 (Map.fromList [(mainFile, 0)])
  where
    startKey = fromJust . Map.lookup mainFile $ mem
    get k = fromJust . Map.lookup k $ mem
    count i depth c = foldl (\c k -> count (get k) (depth + 1) (updateCounter k depth c)) c i
    updateCounter k v c =
      case Map.lookup k c of
        Just v' ->
          if v' < v
            then Map.insert k v c
            else c
        Nothing -> Map.insert k v c

importAll :: [String] -> String -> String -> String -> Memory -> AST -> IO ([String], [Imported], Memory)
importAll loaded name dir path mem main = do
  let allImports = filterImport main
  let mem' = Map.insert name (map getImportName allImports) mem
  let notLoadedImports = filterNotLoaded loaded allImports
  let paths = map (getImportPath dir) notLoadedImports
  let names = map getImportName notLoadedImports
  let loaded' = names ++ loaded
  asts <- mapM tryReadAndParseFile paths
  res <-
    foldM
      (\(ld, res, c) (n', p', a) -> (\(ld', res', c') -> (ld', res ++ res', c')) <$> importAll ld n' dir p' c a)
      (loaded', [], mem') $
    zip3 names paths asts
  let (fLoaded, files, counter'') = res
  return (fLoaded, IFile name path main : files, counter'')

--  zipWithM importAll names asts
filterNotLoaded :: [String] -> [Stmt] -> [Stmt]
filterNotLoaded loaded = filter (\x -> getImportName x `notElem` loaded)

getImportAlias :: Stmt -> String
getImportAlias (Import _ alias _) = alias

getImportName :: Stmt -> String
getImportName (Import _ _ path) = intercalate "" path

getImportPath :: String -> Stmt -> String
getImportPath dir (Import _ _ path) = dir ++ intercalate "/" path ++ ".mard"

filterImport :: AST -> [Stmt]
filterImport (AST stmt) = filter cond stmt
  where
    cond Import {} = True
    cond _         = False

tryReadAndParseFile :: String -> IO AST
tryReadAndParseFile path = do
  putStrLn $ "Parsing file under path: " ++ path
  input <- tryReadFile path
  case parse langParser path input of
    Right res -> return res
    Left e -> do
      putStrLn $ Err.errorBundlePretty e
      error "compilation failed" --Err.errorBundlePretty e

tryReadFile :: String -> IO T.Text
tryReadFile path = do
  file <- try (readFile path) :: IO (Either IOError String)
  T.pack <$>
    case file of
      Left _ -> do
        exePath <- getExePath
        let libPath = exePath ++ "lib" ++ drop 3 path
        readFile libPath
      Right c -> traceShow c $ return c
