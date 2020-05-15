module Compiler where

import qualified Data.Text                as T
import           Text.Megaparsec.Debug
import           Text.Megaparsec.Error    as Err
import Text.Megaparsec
import           AST
import           Compiler.Importer
import           Compiler.LexicalAnalyzer
import           Compiler.Parser
import           Compiler.Translator
import qualified Compiler.Translator.Type as TT
import           Control.Monad            (foldM)
import           Control.Monad.Except     (runExcept)
import           Control.Monad.Reader     (runReaderT)
import           Control.Monad.State      (evalState, evalStateT, runState,
                                           runStateT)
import           Control.Monad.Writer     (runWriterT)
import           Data.List                (intercalate)
import           Data.Void                (Void)
import           Debug.Trace
import Compiler.Analyzer.Type(emptyStorage, AnalyzerException(..), FileInfo(..))
import Text.Megaparsec (PosState)
import           Data.List.NonEmpty as NonEmpty
import qualified Data.Set           as Set

compile path = do
  res <- importMain path
  case runExcept (evalStateT (runWriterT (analyze' res)) emptyStorage) of
    Right (a, w) -> do
      let (a', w') = evalState (runWriterT (runReaderT (translate' a) getDependencies)) TT.emptyStorage
      return (a, a')
    Left e -> do
      err <- createParserError e
      putStrLn $ Err.errorBundlePretty err
      error ""

createParserError :: AnalyzerException -> IO (ParseErrorBundle String Void)
createParserError (CustomAException (FileInfo offset name path) text) = do
  input <- readFile path
  let initialState =
        PosState
          { pstateInput = input
          , pstateOffset = 0
          , pstateSourcePos = initialPos path
          , pstateTabWidth = defaultTabWidth
          , pstateLinePrefix = ""
          }
  let errorBundle =
        ParseErrorBundle
          { bundleErrors = NonEmpty.fromList [FancyError offset (Set.fromList [ErrorFail text])]
                        -- ^ A collection of 'ParseError's that is sorted by parse error offsets
          , bundlePosState = initialState
                        -- ^ State that is used for line\/column calculation
          }
  return errorBundle
createParserError err = error $ show err
