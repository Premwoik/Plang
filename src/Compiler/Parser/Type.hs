{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Compiler.Parser.Type where

import           AST
import           Control.Applicative  hiding (some)
import           Control.Monad.Reader (ReaderT, asks)
import           Data.List            (intercalate, nub)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text, unpack)
import           Data.Text            (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = ReaderT Dependencies (Parsec Custom Text)

injectParser getter arg = asks getter >>= (\x -> x arg)

data Dependencies =
  Dependencies
    { aExprParserGetter        :: Parser AExpr
    , bExprParserGetter        :: Parser BExpr
    , stmtParserGetter         :: Parser Stmt
    , functionStmtParserGetter :: Parser FunctionStmt
    , classStmtParserGetter    :: Parser ClassStmt
    }

data Custom
  = TrivialWithLocation
      [String] -- position stack
      [String] -- context stack
      (Maybe (ErrorItem Char))
      (Set (ErrorItem Char))
  | FancyWithLocation
      [String] -- position stack
      [String] -- context stack
      (ErrorFancy Void) -- Void, because we do not want to allow to nest Customs
  deriving (Eq, Ord, Show)

instance ShowErrorComponent Custom where
  showErrorComponent (TrivialWithLocation stack ctxStack us es) =
    parseErrorTextPretty (TrivialError @Text @Void undefined us es) ++
    showPosStack stack ++ "\nTIPS:\n -" ++ showCtxStack (nub ctxStack)
  showErrorComponent (FancyWithLocation stack ctxStack cs) =
    parseErrorTextPretty (FancyError @Text @Void undefined (Set.singleton cs)) ++
    showPosStack stack ++ "\nTIPS:\n -" ++ showCtxStack (nub ctxStack)

showPosStack :: [String] -> String
showPosStack = intercalate ", " . fmap ("in " ++)

showCtxStack :: [String] -> String
showCtxStack = intercalate "\n -"

emptyError :: Parser ()
emptyError = failure Nothing (Set.fromList [])

addCtxToIndent :: String -> Parser a -> Parser a
addCtxToIndent context p = do
  r <- observing p
  case r of
    Left (TrivialError _ us es) -> fancyFailure . Set.singleton . ErrorCustom $ TrivialWithLocation [] [context] us es
    Left (FancyError _ xs) -> do
      let f (ErrorIndentation ord rlvl alvl) =
            ErrorCustom $ FancyWithLocation [] [context] (ErrorIndentation ord rlvl alvl)
          f (ErrorCustom (FancyWithLocation ps pcs cs@ErrorIndentation {})) =
            ErrorCustom $ FancyWithLocation ps (context : pcs) cs
          f x = x
      fancyFailure (Set.map f xs)
    Right x -> return x

addContext :: String -> Parser a -> Parser a
addContext context p = do
  r <- observing p
  case r of
    Left (TrivialError _ us es) -> fancyFailure . Set.singleton . ErrorCustom $ TrivialWithLocation [] [context] us es
    Left (FancyError _ xs) -> do
      let f (ErrorFail msg) = ErrorCustom $ FancyWithLocation [] [context] (ErrorFail msg)
          f (ErrorIndentation ord rlvl alvl) =
            ErrorCustom $ FancyWithLocation [] [context] (ErrorIndentation ord rlvl alvl)
          f (ErrorCustom (TrivialWithLocation ps pcs us es)) =
            ErrorCustom $ TrivialWithLocation ps (context : pcs) us es
          f (ErrorCustom (FancyWithLocation ps pcs cs)) = ErrorCustom $ FancyWithLocation ps (context : pcs) cs
      fancyFailure (Set.map f xs)
    Right x -> return x

addLocation :: String -> Parser a -> Parser a
addLocation location p = do
  r <- observing p
  case r of
    Left (TrivialError _ us es) -> fancyFailure . Set.singleton . ErrorCustom $ TrivialWithLocation [location] [] us es
    Left (FancyError _ xs) -> do
      let f (ErrorFail msg) = ErrorCustom $ FancyWithLocation [location] [] (ErrorFail msg)
          f (ErrorIndentation ord rlvl alvl) =
            ErrorCustom $ FancyWithLocation [location] [] (ErrorIndentation ord rlvl alvl)
          f (ErrorCustom (TrivialWithLocation ps pcs us es)) =
            ErrorCustom $ TrivialWithLocation (location : ps) pcs us es
          f (ErrorCustom (FancyWithLocation ps pcs cs)) = ErrorCustom $ FancyWithLocation (location : ps) pcs cs
      fancyFailure (Set.map f xs)
    Right x -> return x
