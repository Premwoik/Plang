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
  = moduleParser 
  <|> importParser
  <|> linkPathParser
  <|> skipStmt
  <|> try (assignParser varLeftAssignParser aExprExtended Assign)
  <|> nativeClassParser classStmt
  <|> nativeAssignDeclParser
  <|> try nativeFunctionParser
  <|> classParser classStmt
  <|> functionParser functionStmt

functionStmt :: Parser FunctionStmt
functionStmt
  = returnParser aExprExtended
  <|> passStmt
  <|> whileStmt bExpr WhileFn functionStmt
  <|> forStmt aExprExtended ForFn functionStmt
  <|> fullIfFuncParser bExpr functionStmt
  <|> try (assignParser leftParser aExprExtended AssignFn)
  <|> otherStmt

leftParser x 
  = try ((scopeMarkParser . varLeftAssignParser)  x)
  <|> varLeftAssignParser x

otherStmt = do
  o <- getOffset
  OtherFn o <$> aExprExtended

classStmt :: Parser ClassStmt
classStmt
  = try (assignParser varLeftAssignParser aExprExtended ClassAssign)
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
  <|> try (scopeMarkParser aExprExtended)
  <|> try (varExtendedParser aExprExtended)
  <|> try (listParser aExprExtended)
  <|> rangeParser aExpr
  <|> try floatParser
  <|> intParser
  <|> stringParser
  <|> anonymousFunctionBlockParser functionArgsParser functionStmt
  <|> anonymousFunctionParser functionArgsParser aExprExtended
  <|> fullIfStmt bExpr functionStmt
--  <|> ABool <$> bExpr
 
aTermExtended :: Parser AExpr
aTermExtended
  = bracketParser aExprExtended
  <|> try (scopeMarkParser aExprExtended)
  <|> try (varExtendedParser aExprExtended)
  <|> try (listParser aExprExtended)
  <|> rangeParser aExpr
  <|> try floatParser
  <|> intParser
  <|> stringParser
  <|> anonymousFunctionBlockParser functionArgsParser functionStmt
  <|> anonymousFunctionParser functionArgsParser aExprExtended
  <|> fullIfStmt bExpr functionStmt
  <|> ABool <$> bExpr


bTerm :: Parser BExpr
bTerm
  = parens bExpr
  <|> (BoolConst True <$ rword "true")
  <|> (BoolConst False <$ rword "false")
  <|> rExpr aExpr


