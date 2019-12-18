module Compiler.Analyzer.Browser where

import           AST

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

findVarAll :: String -> [FunctionStmt] -> [Stmt] -> [Stmt]
findVarAll name l g = findVarGlobal name g ++ varToGlobal (findVar name l)

findVarAll' :: String -> Stmt -> [Stmt] -> [Stmt]
findVarAll' name (Function _  _ args l) g =
  checkArgs args ++ findVarGlobal name g ++ varToGlobal (findVar name l)
  where
    checkArgs (Just args) = map (\(FunArg t n) -> Assign n t (IntConst 1)) . filter (\(FunArg t n) -> n == name) $ args
    checkArgs Nothing = []

findNameAll :: String -> Stmt -> [Stmt] -> [Stmt]
findNameAll name f s = findVarAll' name f s ++ findClassGlobal name s

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

findClassVar :: String -> String -> [Stmt] -> [ClassStmt]
findClassVar className varName = filter cond . concatMap unwrapClass . findClassGlobal className
  where
    cond (ClassAssign name _ _) = varName == name
    cond _                      = False

findFuncDeclaration :: String -> [Stmt] -> [Stmt]
findFuncDeclaration name = filter cond
  where
    cond (Function fName _ _ _) = name == fName
    cond _                      = False
