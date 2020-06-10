module Compiler.Parser.Type where

import           Data.Text (Text, unpack)
import           Text.Megaparsec     hiding (State)
import           Data.Void
import Data.Set (Set)
import qualified Data.Set as E

data Custom = NotKeyword Text
  deriving (Eq, Show, Ord)

instance ShowErrorComponent Custom where
  showErrorComponent (NotKeyword txt) = unpack txt ++ " is not a keyword"

type Parser = Parsec Custom String




