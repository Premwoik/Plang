module Compiler.Analyzer.Browser where

import           AST
import Compiler.Analyzer.Type
import Debug.Trace


--       FIND
findFirst :: [String] -> LocalStorage -> [Stmt] -> Maybe Stmt
findFirst name l g = case find name l g of
  h : _ -> Just h 
  [] -> Nothing

find :: [String] -> LocalStorage -> [Stmt] -> [Stmt]
find [name] (LocalClassScope c (Just (Method o n t a b)) scopes) g =
  findVarInFunc name (Function o n t a b) ++ findInScopes name scopes ++ findVarInClass name c ++ findGlobal name g
find ["this", name] (LocalClassScope c _ _) _ = 
  findVarInClass name c
find [name] (LocalFunc f s) g =
  findVarInFunc name f ++ findInScopes name s ++ findGlobal name g
find [name] LocalEmpty g =
  findGlobal name g
find _ _ _ = []


findInScopes :: String -> [Scope] -> [Stmt]
findInScopes name = varToGlobal . concatMap (findVar name)

findVarInFunc :: String -> Stmt -> [Stmt]
findVarInFunc name (Function _ _ _ args l) =
  checkArgs args ++ varToGlobal (findVar name l)
  where
    checkArgs (Just args) = map (\(FunArg t n) -> Assign 0 [n] t Nop) . filter (\(FunArg t n) -> n == name) $ args
    checkArgs Nothing = []

findVarInClass :: String -> Stmt -> [Stmt]
findVarInClass name (ClassExpr _ n _ body) = classVarToGlobal . filter cond $ body
  where
    cond (ClassAssign _ [vName] _ _) =  vName == name
    cond (ClassAssign _ ["this", vName] _ _) =  vName == name
    cond (ClassAssignDeclaration _ vName _) = vName == name
    cond (Method _ mName _ _ _) = mName == name
    cond (MethodDeclaration _ mName _ _) = mName == name
    cond _ = False

findVar :: String -> [FunctionStmt] -> [FunctionStmt]
findVar name = filter cond
  where
    cond (AssignFn _ vName _ _) = name == head vName
    cond _                    = False

varToGlobal :: [FunctionStmt] -> [Stmt]
varToGlobal = map (\(AssignFn o a b c) -> Assign o a b c)

classVarToGlobal :: [ClassStmt] -> [Stmt]
classVarToGlobal = map to
  where
    to (ClassAssign o a b c ) = Assign o a b c
    to (Method o a b c d) = Function o a b c d
    to (MethodDeclaration o a b c) = Function o a b c []
    

findGlobal :: String -> [Stmt] -> [Stmt]
findGlobal name = filter cond
  where
    cond (ClassExpr _ cName _ _ ) = cName == name
    cond (Function _ fName _ _ _) = fName == name
    cond (Assign _ aName _ _) = head aName == name
    cond (NativeFunction _ _ aName _ _ ) = aName == name
    cond (NativeClass _ _ cName _ _) = cName == name
    cond (NativeAssignDeclaration _ _ aName _) = aName == name
    cond _ = False

findClassGlobal :: String -> [Stmt] -> [Stmt]
findClassGlobal name = filter cond
  where
    cond (ClassExpr _ cName _ _) = cName == name
    cond (NativeClass _ _ cName _ _) = cName == name
    cond _                     = False

unwrapClass (ClassExpr _ _ _ body) = body
unwrapClass (NativeClass _ _ _ _ body) = body

findMethod :: String -> String -> [Stmt] -> [ClassStmt]
findMethod className methodName = filter cond . concatMap unwrapClass . findClassGlobal className
  where
    cond (Method _ mName _ _ _) = methodName == mName
    cond (MethodDeclaration _ mName _ _) = methodName == mName
    cond (ClassAssign _ [aName] _ _) = aName == methodName
    cond _                    = False
    
    
--    MATCH

maybeArgsMatch :: Maybe [AExpr] -> [Stmt] -> Bool
maybeArgsMatch (Just args) s = any (argsMatch prepareArgs) s
  where
    prepareArgs = map aExprExtractType args
maybeArgsMatch Nothing _ = True

argsMatch :: [VarType] -> Stmt -> Bool
argsMatch t (NativeFunction _ _ _ _ Nothing) = True
argsMatch t (NativeFunction _ _ _ _ (Just args)) =
  argsMatch' t args
argsMatch t (Function _ _ _  Nothing _) = True
argsMatch t (Function _ _ _ (Just args) _) =
  argsMatch' t args
--  CLASS
argsMatch t s = any match (getConstructors s)
  where
    match (Method _ _ _ Nothing _) = 
      argsMatch' t []
    match (Method _ _ _ (Just args) _) =
      argsMatch' t args
    match (MethodDeclaration _ _ _ Nothing) =
      argsMatch' t []
    match (MethodDeclaration _ _ _ (Just args)) =
      argsMatch' t args
    match _ = False

argsMatch' :: [VarType] -> [FunArg] -> Bool
argsMatch' t args = (length t == length args) && all (\(w, FunArg ot _) -> w == ot) (zip t args)


aExprExtractType :: AExpr -> VarType
aExprExtractType (TypedVar _ t _ _) = t
aExprExtractType IntConst {} = VInt
aExprExtractType FloatConst {} = VFloat
aExprExtractType StringVal {} = VString

getConstructors :: Stmt -> [ClassStmt]
getConstructors (NativeClass o _ n g b) = filter cond b
  where
    cond (MethodDeclaration _ n' _ _) = n' == n
    cond _ = False
getConstructors (ClassExpr o n g b) = filter cond b
  where
    cond (Method _ n' _ _ _) = n' == n
    cond _ = False
getConstructors x = error $ "Given object: " ++ show x ++ "have no constructors"

