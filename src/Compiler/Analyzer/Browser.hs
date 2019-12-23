module Compiler.Analyzer.Browser where

import           AST
import Compiler.Analyzer.Type

findVarGlobal :: String -> [Stmt] -> [Stmt]
findVarGlobal name = filter cond
  where
    cond (Assign _ vName _ _) = name == vName
    cond _                  = False

findVar :: String -> [FunctionStmt] -> [FunctionStmt]
findVar name = filter cond
  where
    cond (AssignFn _ vName _ _) = name == vName
    cond _                    = False

varToGlobal :: [FunctionStmt] -> [Stmt]
varToGlobal = map (\(AssignFn o a b c) -> Assign o a b c)

classVarToGlobal :: [ClassStmt] -> [Stmt]
classVarToGlobal = map to
  where
    to (ClassAssign o a b c ) = Assign o a b c
    to (Method o a b c d) = Function o a b c d
    

findVarAll :: String -> [FunctionStmt] -> [Stmt] -> [Stmt]
findVarAll name l g = findVarGlobal name g ++ varToGlobal (findVar name l)

findVarInFunc :: String -> Stmt -> [Stmt]
findVarInFunc name (Function _ _  _ args l) =
  checkArgs args ++ varToGlobal (findVar name l)
  where
    checkArgs (Just args) = map (\(FunArg t n) -> Assign 0 n t Nop) . filter (\(FunArg t n) -> n == name) $ args
    checkArgs Nothing = []

findVarInClass :: String -> Stmt -> [Stmt]
findVarInClass name (ClassExpr _ n _ body) = classVarToGlobal . filter cond $ body
  where
    cond (ClassAssign _ vName _ _) =  vName == n
    cond (Method _ mName _ _ _) = mName == n


find :: String -> LocalStorage -> [Stmt] -> [Stmt]
find name (LocalScope body) g = []
find name (LocalClassScope c (Just (Method o n t a b))) g = findVarInFunc name (Function o n t a b) ++ findVarInClass name c ++ findGlobal name g
--find name (LocalClassScope c Nothing) g = varToGlobal (findVar name b)
find name (LocalFunc f) g = findVarInFunc name f ++ findGlobal name g 
find name LocalEmpty g = findGlobal name g
find _ _ _ = []

findFirst :: String -> LocalStorage -> [Stmt] -> Maybe Stmt
findFirst name l g = case find name l g of
  h : _ -> Just h 
  [] -> Nothing


--findAllReturn :: [FunctionStmt] -> [FunctionStmt]
--findAllReturn = concat . foldr scan []
--  where
--    scan (WhileFn _ exp) acc = findAllReturn exp : acc
--    scan (ForFn _ _ exp) acc = findAllReturn exp : acc
--    scan (IfFn lExp) acc = concatMap (\(_, exp) -> findAllReturn exp) lExp : acc
--    scan (ReturnFn)

findGlobal :: String -> [Stmt] -> [Stmt]
findGlobal name = filter cond
  where
    cond (ClassExpr _ cName _ _ ) = cName == name
    cond (Function _ fName _ _ _) = fName == name
    cond _ = False

findClassGlobal :: String -> [Stmt] -> [Stmt]
findClassGlobal name = filter cond
  where
    cond (ClassExpr _ cName _ _) = cName == name
    cond _                     = False

unwrapClass (ClassExpr _ _ _ body) = body

findMethod :: String -> String -> [Stmt] -> [ClassStmt]
findMethod className methodName = filter cond . concatMap unwrapClass . findClassGlobal className
  where
    cond (Method _ mName _ _ _) = methodName == mName
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

