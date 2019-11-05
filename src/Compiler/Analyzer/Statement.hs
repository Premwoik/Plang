module Compiler.Analyzer.Statement where

import AST
import Compiler.Analyzer.Type
import Control.Exception (throw)



checkImport :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkImport t@(Import []) = return t

checkLinkPath :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkLinkPath t@(LinkPath p) = return t

checkFunction :: Stmt -> FnStmtAnalyzer -> Analyzer' Stmt
-- TODO check if function returns what it is wanted to and check if arguments are good
checkFunction t@(Function name type' args body) bodyAnalyzer = do
  checkedBody' <- mapM bodyAnalyzer body
  return t


checkCasualExpr :: FunctionStmt -> AExprAnalyzer -> Analyzer' FunctionStmt
checkCasualExpr (OtherFn aexpr) aEx =
  OtherFn <$>
  case aexpr of
    Var {} -> continue
    If {}  -> continue
    _      -> throw IncorrectExprException
  where
    continue = aEx aexpr

checkNative :: Stmt -> Analyzer' Stmt
-- TODO 
checkNative t@(NativeFunction name ret args') = return t

checkAssign :: RawAssign -> RawAssignConst a -> Analyzer' a
--TODO 
checkAssign (name, ret, aExpr) wrapper = return (wrapper name ret aExpr)

checkWhile :: FunctionStmt -> Analyzer' FunctionStmt
-- TODO
checkWhile t@(WhileFn cond block) = return t

checkFor:: FunctionStmt -> Analyzer' FunctionStmt
-- TODO
checkFor t = return t

checkClass :: Stmt -> ClassStmtAnalyzer -> Analyzer' Stmt
-- TODO
checkClass t _ = return t

--checkForStmt :: Stmt -> Analyzer' Stmt
--checkForStmt (For var range block) = For var range <$>
--  (checkVar . checkRange $ mapM statementAnalyzer block)
--  where
--    checkVar f =
--      case var of
--        Var name Nothing Nothing -> f
--        _                        -> throw IncorrectExprException
--    checkRange f =
--      case range of
--        Var name Nothing Nothing -> f
--        Range s e                -> f
--        _                        -> throw IncorrectExprException
