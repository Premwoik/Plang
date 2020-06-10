module Compiler.Analyzer.UniversalCheckers(checkFnArgs, checkTypesMatchGens, compareGens) where

import AST
import Compiler.Analyzer.Type
import Compiler.Analyzer.Error
import Data.Maybe(fromMaybe, fromJust)
import Data.List(find)


checkFnArgs :: [FunArg] -> Analyzer' [FunArg]
checkFnArgs = return . map (\(FunArg t n) -> FunArg t (concat (scaleNameWithScope ["args", n])))

checkTypesMatchGens :: Offset -> Scope -> [VarType] -> Analyzer' ()
checkTypesMatchGens o (Scope _ f) types = do
  let gens = map (\(SGen t n) -> (n, t)) . filter genFilter $ f
  mapM_ (\(VGenPair n t) -> noError (fromMaybe False (checkEq t (lookup n gens)))) types
  where
    genFilter SGen {} = True
    genFilter _       = False
    noError True = return ()
    noError False = makeError o WrongGenericType
    checkEq t1 maybeT = do
      t2 <- maybeT
      return $ t1 == t2 || t2 == VAuto

compareGens :: Offset -> VarType -> VarType -> Analyzer' VarType
compareGens o (VGen n1) (VGen n2)
  | n1 == n2 = return (VGen n1)
  | otherwise = do
    gens <- getClassGens'
    let t1 = getType n1 gens
    t2 <- replaceAuto t1 n2 $ getType n2 gens
    t1' <- replaceAuto t2 n1 t1
    cmp t1' t2
compareGens o (VGen n1) t2 = do
  gens <- getClassGens'
  t1 <- replaceAuto t2 n1 $ getType n1 gens
  cmp t1 t2
compareGens o t1 (VGen n2) = do
  gens <- getClassGens'
  t2 <- replaceAuto t1 n2 $ getType n2 gens
  cmp t1 t2
compareGens o t1 t2 = cmp t1 t2

getType n = (\(SGen t _) -> t) . fromJust . find (\(SGen _ n') -> n == n')

cmp t1 t2
  | t1 == t2 = return t1
  | otherwise = makeError 0 $ AssignTypesMismatch t1 t2

replaceAuto :: VarType -> String -> VarType -> Analyzer' VarType
replaceAuto notAuto n auto
  | auto == VAuto && notAuto /= VAuto = do
    updateScope =<< updateField (SGen notAuto n) . fromJust <$> getClassScope
    return notAuto
  | otherwise = return auto


