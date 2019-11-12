module Compiler.Analyzer.Statement where

import           AST
import           Compiler.Analyzer.Pre
import           Compiler.Analyzer.Type
import           Compiler.Translator.Type
import           Control.Exception        (throw)
import           Control.Monad.State      (get, gets, modify, put)
import           Control.Monad.Writer     (tell)

checkImport :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkImport t@(Import _) = return t

checkLinkPath :: Stmt -> Analyzer' Stmt
-- TODO check if import path exist
checkLinkPath t@(LinkPath p) = return t

checkFunction :: RawFunction -> RawFunctionConst a -> FnStmtAnalyzer -> Analyzer' a
-- TODO check if arguments are good
checkFunction (name, type', args, body) wrapper bodyAnalyzer = do
  s <- get
  loadFunction name
  checkedBody' <- concat <$> mapM bodyAnalyzer body
  nType <- retType <$> gets local
  put s
  return $ wrapper name nType args checkedBody'

checkReturn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkReturn (ReturnFn aExpr) analyzer = do
  local' <- gets local
  (t, inject, res) <- analyzer aExpr
  checkTypes local' t
  modify (\s -> s {local = local' {retType = t}})
  return $ inject ++ [ReturnFn res]

checkTypes (Decl FunctionT n _ t _) t' =
  if t == t' || t == VAuto
    then return ()
    else throw $ TypesMismatch ("In function named [" ++ n ++ "] return missmatch: " ++ show t ++ " =/= " ++ show t')
checkTypes _ _ = throw $ TypesMismatch "Local don't contain function struct, so types cant be checked!!!"

--checkCasualExpr :: FunctionStmt -> AExprAnalyzer -> Analyzer' FunctionStmt
--checkCasualExpr (OtherFn aexpr) aEx =
--  OtherFn <$>
--  case aexpr of
--    Var {} -> continue
--    If {}  -> continue
--    _      -> throw IncorrectExprException
--  where
--    continue = aEx aexpr
checkNative :: Stmt -> Analyzer' Stmt
checkNative t@(NativeFunction name ret args') =
  case ret of
    VAuto -> throw IncorrectExprException
    _     -> NativeFunction name ret <$> checkArgs args'

checkArgs :: Maybe [FunArg] -> Analyzer' (Maybe [FunArg])
checkArgs = return

checkAssignFn :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
--TODO
checkAssignFn (AssignFn name ret aExpr) analyzer = do
  s <- get
  (type', inject, res) <- analyzer aExpr
   
--  res' <-
--    case isVar name (children (local s) ++ global s) of
--      (True, Decl _ name _ t _) -> checkType t type' VBlank res
--      (False, _)                -> checkType ret type' type' res
  res' <- checkType ret type' type' res
  return $ inject ++ res'
  where
    checkType a b nt r
      | a == b || a == VAuto = return [AssignFn name nt r]
      | otherwise = throw $ TypesMismatch (show a ++ " =/= " ++ show b)

checkAssign :: RawAssign -> RawAssignConst a -> AExprAnalyzer -> Analyzer' [a]
--TODO
checkAssign (name, ret, aExpr) wrapper analyzer = do
  (type', inject, res) <- analyzer aExpr
  return [wrapper name ret res]

checkWhile :: FunctionStmt -> Analyzer' [FunctionStmt]
-- TODO
checkWhile t@(WhileFn cond block) = return . return $ t

checkFor :: FunctionStmt -> Analyzer' [FunctionStmt]
-- TODO
checkFor = return . return

checkClass :: Stmt -> ClassStmtAnalyzer -> Analyzer' Stmt
-- TODO
checkClass (ClassExpr name cast body) analyzer = do
  loadClass name
  body' <- mapM analyzer body
  return $ ClassExpr name cast body'

checkOtherExpr :: FunctionStmt -> AExprAnalyzer -> Analyzer' [FunctionStmt]
checkOtherExpr (OtherFn aExpr) analyzer = return . OtherFn . trd <$> analyzer aExpr
