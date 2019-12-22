module Compiler.Analyzer.Browser where

import           AST
import Compiler.Analyzer.Type

findVarGlobal :: String -> [Stmt] -> [Stmt]
findVarGlobal name = filter cond
  where
    cond (Assign vName _ _) = name == vName
    cond _                  = False

findVar :: String -> [FunctionStmt] -> [FunctionStmt]
findVar name = filter cond
  where
    cond (AssignFn vName _ _) = name == vName
    cond _                    = False

varToGlobal :: [FunctionStmt] -> [Stmt]
varToGlobal = map (\(AssignFn a b c) -> Assign a b c)

classVarToGlobal :: [ClassStmt] -> [Stmt]
classVarToGlobal = map to
  where
    to (ClassAssign a b c ) = Assign a b c
    to (Method a b c d) = Function a b c d
    

findVarAll :: String -> [FunctionStmt] -> [Stmt] -> [Stmt]
findVarAll name l g = findVarGlobal name g ++ varToGlobal (findVar name l)

findVarInFunc :: String -> Stmt -> [Stmt]
findVarInFunc name (Function _  _ args l) =
  checkArgs args ++ varToGlobal (findVar name l)
  where
    checkArgs (Just args) = map (\(FunArg t n) -> Assign n t Nop) . filter (\(FunArg t n) -> n == name) $ args
    checkArgs Nothing = []

findVarInClass :: String -> Stmt -> [Stmt]
findVarInClass name (ClassExpr n _ body) = classVarToGlobal . filter cond $ body
  where
    cond (ClassAssign vName _ _) =  vName == n
    cond (Method mName _ _ _) = mName == n


find :: String -> LocalStorage -> [Stmt] -> [Stmt]
find name (LocalScope body) g = []
find name (LocalClassScope c (Just (Method n t a b))) g = findVarInFunc name (Function n t a b) ++ findVarInClass name c ++ findGlobal name g
--find name (LocalClassScope c Nothing) g = varToGlobal (findVar name b)
find name (LocalFunc f) g = findVarInFunc name f ++ findGlobal name g 
find name LocalEmpty g = findGlobal name g
find _ _ _ = []

findGlobal :: String -> [Stmt] -> [Stmt]
findGlobal name = filter cond
  where
    cond (ClassExpr cName _ _ ) = cName == name
    cond (Function fName _ _ _) = fName == name
    cond _ = False

findClassGlobal :: String -> [Stmt] -> [Stmt]
findClassGlobal name = filter cond
  where
    cond (ClassExpr cName _ _) = cName == name
    cond _                     = False

unwrapClass (ClassExpr _ _ body) = body

findMethod :: String -> String -> [Stmt] -> [ClassStmt]
findMethod className methodName = filter cond . concatMap unwrapClass . findClassGlobal className
  where
    cond (Method mName _ _ _) = methodName == mName
    cond _                    = False
--
--findClassVar :: String -> String -> [Stmt] -> [ClassStmt]
--findClassVar className varName = filter cond . concatMap unwrapClass . findClassGlobal className
--  where
--    cond (ClassAssign name _ _) = varName == name
--    cond _                      = False
--
--findFuncDeclaration :: String -> [Stmt] -> [Stmt]
--findFuncDeclaration name = filter cond
--  where
--    cond (Function fName _ _ _) = name == fName
--    cond _                      = False

