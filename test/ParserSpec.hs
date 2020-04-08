module ParserSpec where

import           Test.Hspec
import           Test.Hspec.Megaparsec

main = do
  describe "" $ it "can parser int" $ shouldBe 1 1
