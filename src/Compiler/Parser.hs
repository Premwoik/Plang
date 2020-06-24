{-# LANGUAGE OverloadedStrings #-}

module Compiler.Parser
  ( langParser
  ) where

import           AST
import           Compiler.Parser.Lexer
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

import           Compiler.Parser.AExpr
import           Compiler.Parser.BExpr
import           Compiler.Parser.Statement
import           Compiler.Parser.Universal

langParser :: Parser AST
langParser = AST <$> (some stmt' <* eof)

stmt' :: Parser Stmt
stmt' =
  moduleParser <|> importParser <|> linkPathParser <|> skipStmt <|>
  try (assignParser varLeftAssignParser aExprExtended Assign) <|>
  nativeClassParser classStmt <|>
  nativeAssignDeclParser <|>
  try nativeFunctionParser <|>
  classParser classStmt <|>
  functionParser functionStmt

lambdaStmt :: Parser FunctionStmt
lambdaStmt = functionStmt

functionStmt :: Parser FunctionStmt
functionStmt =
  returnParser aExprExtended <|> breakStmt <|> passStmt <|> forStmt aExprExtended ForFn functionStmt <|>
  whileStmt bExpr WhileFn functionStmt <|>
  fullIfFuncParser bExpr functionStmt <|>
  caseStmtParser aExprExtended functionStmt <|>
  assignOrOtherParser leftParser aExprExtended

leftParser x = try ((scopeMarkParser . varLeftAssignParser) x) <|> varLeftAssignParser x

classStmt :: Parser ClassStmt
classStmt =
  decoratorParser classStmt <|> try (classAssignParser leftParser aExprExtended) <|> try defaultAssignParser <|>
  methodDeclarationParser <|>
  ((\(Function o a _ b c d) -> Method o a b c defaultMethodDetails d) <$> functionParser functionStmt)

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

aExprExtended :: Parser AExpr
aExprExtended = makeExprParser aTermExtended aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ hidden (symbol "-")), InfixL (ABinary Modulo <$ hidden (symbol "%"))]
  , [InfixL (ABinary Multiply <$ hidden (symbol "*")), InfixL (ABinary Divide <$ hidden (symbol "/"))]
  , [InfixL (ABinary Add <$ hidden (symbol "+")), InfixL (ABinary Subtract <$ hidden (symbol "-"))]
  , [ InfixL ((\x y -> ABool (RBinary Equal x y)) <$ hidden (symbol "=="))
    , InfixL ((\x y -> ABool (RBinary NotEqual x y)) <$ hidden (symbol "!="))
    , InfixL ((\x y -> ABool (RBinary Greater x y)) <$ hidden (symbol ">"))
    , InfixL ((\x y -> ABool (RBinary Less x y)) <$ hidden (symbol "<"))
    , InfixL ((\x y -> ABool (RBinary EqGreater x y)) <$ hidden (symbol ">="))
    , InfixL ((\x y -> ABool (RBinary EqLess x y)) <$ hidden (symbol "=<"))
    ]
  ]

bOperators :: [[Operator Parser BExpr]]
bOperators = [[Prefix (Not <$ rword "not")], [InfixL (BBinary And <$ rword "and"), InfixL (BBinary Or <$ rword "or")]]

aTerm :: Parser AExpr
aTerm =
  bracketParser aExpr <|> try (scopeMarkParser aExprExtended) <|> try (varExtendedParser aExprExtended) <|>
  try (listParser aExprExtended) <|>
  rangeParser aExpr <|>
  try floatParser <|>
  intParser <|>
  stringParser <|>
  anonymousFunctionBlockParser functionArgsParser functionStmt <|>
  anonymousFunctionParser functionArgsParser aExprExtended <|>
  fullIfStmt bExpr functionStmt

--  <|> ABool <$> bExpr
aTermExtended :: Parser AExpr
aTermExtended =
  nullParser <|> bracketParser aExprExtended <|> try (scopeMarkParser aExprExtended) <|>
  try (varExtendedParser aExprExtended) <|>
  try (listParser aExprExtended) <|>
  rangeParser aExpr <|>
  try floatParser <|>
  intParser <|>
  stringParser <|>
  anonymousFunctionBlockParser functionArgsParser functionStmt <|>
  anonymousFunctionParser functionArgsParser aExprExtended <|>
  fullIfStmt bExpr functionStmt <|>
  try (ABool <$> bExpr)

bTerm :: Parser BExpr
bTerm =
  parens bExpr <|> (BoolConst True <$ rword "true") <|> (BoolConst False <$ rword "false") <|> try (rExpr aExpr) <|>
  boolVarParser aExpr
