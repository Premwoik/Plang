{-# LANGUAGE OverloadedStrings #-}
module Statement.Translator where


import AST


translate :: AST -> IO ()
translate (AST stmts) = mapM_ print stmts


declaredFunctionNames :: [Stmt] -> [String]
declaredFunctionNames =
  map (\(Function n _ _ _ )-> n) . filter isFunc
  where
    isFunc x = case x of
      Function {} -> True
      _ -> False


translateStatement :: Stmt -> IO ()
translateStatement s =
  case s of
    Import {} -> return ()
    Function {} -> return ()
    Assign {} -> return ()
    While {} -> return ()
    For {} -> return ()
    CasualExpr {} -> return ()
    ClassExpr {} -> return ()
    ReturnExpr {} -> return ()
    Skip -> return ()
--    _ -> return ()
