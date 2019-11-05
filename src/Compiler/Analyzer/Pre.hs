module Compiler.Analyzer.Pre where

import AST
import Compiler.Translator.Type

findAllDeclaredNames :: [Stmt] -> [(String, DPair)]
findAllDeclaredNames = map makeOutput . filter isName
  where
    makeOutput x =
      case x of
        ClassExpr n _ _  -> (n, (VVoid, ClassDecl))
        Function n t _ _ -> (n, (t, FunctionDecl))
        Assign n t _     -> (n, (t, Instance))
    isName x =
      case x of
        ClassExpr {} -> True
        Function {}  -> True
        Assign {}    -> True
        _            -> False


