import Test.Hspec
import Test.Hspec.Megaparsec

import qualified ParserSpec
import qualified AnalyzerSpec
import qualified TranslatorSpec

main :: IO ()
main =
  hspec $ do
    describe "Parser" ParserSpec.main
    describe "Lexical Analyzer" AnalyzerSpec.main
    describe "Translator" TranslatorSpec.main