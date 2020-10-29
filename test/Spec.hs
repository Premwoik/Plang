module Spec where
import Test.Hspec
import Test.Hspec.Megaparsec

import qualified ParserSpec
import qualified AnalyzerSpec
import qualified TranslatorSpec
import qualified CompilerSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec =  do
  describe "Parser" ParserSpec.main
  describe "Lexical Analyzer" AnalyzerSpec.main
  describe "Translator" TranslatorSpec.main
  describe "Compiler Tests" CompilerSpec.main
