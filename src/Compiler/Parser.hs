{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser(langParser) where

import           Control.Applicative            hiding (some)
import           Control.Monad
import           Control.Monad.Combinators.Expr
import           Control.Monad.State.Lazy       as S
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Void
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L
import           Text.Megaparsec.Debug
import           AST
import           Compiler.Parser.Lexer

import Compiler.Parser.Statement
import Compiler.Parser.AExpr
import Compiler.Parser.BExpr
import Compiler.Parser.Universal



langParser :: Parser AST
langParser = AST <$> (some stmt' <* eof)


stmt' :: Parser Stmt
stmt'
  = importParser
  <|> linkPathParser
  <|> skipStmt
  <|> try (assignParser aExprExtended Assign)
  <|> nativeClassParser classStmt
  <|> try nativeFunctionParser
  <|> nativeAssignDeclParser
  <|> classParser classStmt
  <|> functionParser functionStmt
--  <|> CasualExpr <$> aExpr

functionStmt :: Parser FunctionStmt
functionStmt
  = returnParser aExpr
  <|> whileStmt bExpr WhileFn functionStmt
  <|> forStmt aExpr ForFn functionStmt
  <|> fullIfFuncParser bExpr functionStmt
  <|> try (assignParser aExprExtended AssignFn)
  <|> otherStmt

otherStmt = do
  o <- getOffset
  OtherFn o <$> aExpr

classStmt :: Parser ClassStmt
classStmt
  = try (assignParser aExprExtended ClassAssign)
  <|> try defaultAssignParser
  <|> methodDeclarationParser
  <|> ((\(Function o a b c d) -> Method o a b c d) <$> functionParser functionStmt)
  

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

aExprExtended:: Parser AExpr
aExprExtended = makeExprParser aTermExtended aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-")]
  , [InfixL (ABinary Multiply <$ symbol "*"), InfixL (ABinary Divide <$ symbol "/")]
  , [InfixL (ABinary Add <$ symbol "+"), InfixL (ABinary Subtract <$ symbol "-")]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [[Prefix (Not <$ rword "not")]
  , [InfixL (BBinary And <$ rword "and")
  , InfixL (BBinary Or <$ rword "or")]]


aTerm :: Parser AExpr
aTerm
  = bracketParser aExpr
  <|> try (varParser aExpr)
  <|> try (ListVar <$> listParser aExpr)
  <|> rangeParser aExpr
  <|> try (FloatConst <$> float')
  <|> IntConst <$> integer
--  <|> ABool <$> bExpr
  <|> StringVal <$> stringLiteral
  <|> anonymousFunctionBlockParser functionArgsParser functionStmt
  <|> anonymousFunctionParser functionArgsParser aExpr
  <|> fullIfStmt bExpr functionStmt
  
aTermExtended :: Parser AExpr
aTermExtended
  = bracketParser aExpr
  <|> try (varExtendedParser aExpr)
  <|> try (ListVar <$> listParser aExpr)
  <|> rangeParser aExpr
  <|> try (FloatConst <$> float')
  <|> IntConst <$> integer
  <|> StringVal <$> stringLiteral
  <|> anonymousFunctionBlockParser functionArgsParser functionStmt
  <|> anonymousFunctionParser functionArgsParser aExpr
  <|> fullIfStmt bExpr functionStmt


bTerm :: Parser BExpr
bTerm
  = parens bExpr
  <|> (BoolConst True <$ rword "true")
  <|> (BoolConst False <$ rword "false")
  <|> rExpr aExpr


