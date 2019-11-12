module Compiler.Analyzer.Pre where

import           AST
import           Compiler.Translator.Type
import Compiler.Analyzer.Type
import Control.Monad.State(get, put)
import Control.Monad.Writer(tell)


-- TODO don't search names in block deeper then function. So for example if block body is not scanned :< 
findAllDeclaredNames :: [Stmt] -> [DeclarationTree]
findAllDeclaredNames = map makeOutput . filter isName
  where
    makeOutput x =
      case x of
        ClassExpr n _ body ->
          Decl {decType = ClassT, name = n, argTypes = [], retType = VClass n, children = findAllDeclaredInClass body}
        Function n t args body ->
          Decl
            { decType = FunctionT
            , name = n
            , argTypes = parseArgs args
            , retType = t
            , children = findAllDeclaredInFunction body
            }
        Assign n t _ -> Decl {decType = VariableT, name = n, argTypes = [], retType = t, children = []}
    isName x =
      case x of
        ClassExpr {} -> True
        Function {}  -> True
        Assign {}    -> True
        _            -> False

findAllDeclaredInClass :: [ClassStmt] -> [DeclarationTree]
findAllDeclaredInClass = map makeOutput . filter isName
  where
    makeOutput (ClassAssign name' type' _) =
      Decl {decType = VariableT, name = name', argTypes = [], retType = type', children = []}
    makeOutput (Method name' type' args body) =
      Decl
        { decType = FunctionT
        , name = name'
        , argTypes = parseArgs args
        , retType = type'
        , children = findAllDeclaredInFunction body
        }
    isName _ = True

findAllDeclaredInFunction :: [FunctionStmt] -> [DeclarationTree]
findAllDeclaredInFunction = map makeOutput . filter isName
  where
    makeOutput (AssignFn name' type' _) =
      Decl {decType = VariableT, name = name', argTypes = [], retType = type', children = []}
    isName AssignFn {} = True
    isName _           = False

parseArgs :: Maybe [FunArg] -> [DeclarationTree]
parseArgs (Just args) = map (\(FunArg t n) -> Decl VariableT n [] t []) args
parseArgs Nothing     = []

loadFunction :: String -> Analyzer' ()
loadFunction name = do
  s <- get
  let res = head . filter f $ makeList (local s) s
  put $ s {local = res}
  where
    f (Decl FunctionT name' _ _ _) = name == name'
    f _                            = False
    makeList EmptyDecl s = global s
    makeList _  s = children (local s) ++ global s
    
    
getClass :: String -> Analyzer' DeclarationTree
getClass name = do
  s <- get
  return . head . filter f $ global s
  where
    f (Decl ClassT name' _ _ _) = name == name'
    f _                            = False

loadClass :: String -> Analyzer' ()
loadClass name = do
  s <- get
  c <- getClass name
  put $ s {local = c}
  return ()

isMethodInClass :: String -> DeclarationTree -> (Bool, DeclarationTree)
isMethodInClass name (Decl ClassT _ _ _ children) = makePair . filter f $ children
  where
    makePair [x] = (True, x)
    makePair _ = (False, EmptyDecl)
    f (Decl FunctionT n _ _ _) = name == n
    f _ = False
    
isVar :: String -> [DeclarationTree] -> (Bool, DeclarationTree)
isVar name = makePair . filter f 
  where 
   makePair [x] = (True, x)
   makePair _ = (False, EmptyDecl)
   f (Decl _ n _ _ _) = name == n
   f _ = False


wasVarDeclared :: String -> [DeclarationTree] -> (Bool, DeclarationTree)
wasVarDeclared name = makePair . filter f 
 where 
  makePair [x] = (True, x)
  makePair _ = (False, EmptyDecl)
  f (Decl _ n _ _ _) = name == n
  f _ = False
       
