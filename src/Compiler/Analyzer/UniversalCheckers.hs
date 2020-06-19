module Compiler.Analyzer.UniversalCheckers
  ( checkFnArgs
  , checkTypesMatchGens
  , compareGens
  , check
  , checkFunctionUniqueness
  ) where

import           AST
import           Compiler.Analyzer.Browser
import           Compiler.Analyzer.Error
import           Compiler.Analyzer.Type
import           Data.List                 (all, find, group)
import           Data.Maybe                (fromJust, fromMaybe)
import           Debug.Trace




checkFnArgs :: [FunArg] -> Analyzer' [FunArg]
checkFnArgs = return . map (\(FunArg t n) -> FunArg t (concat (scaleNameWithScope ["args", n])))

checkTypesMatchGens :: Offset -> Scope -> [VarType] -> Analyzer' ()
checkTypesMatchGens o (Scope _ f) types = do
  let gens = map (\(SGen t n) -> (n, t)) . filter genFilter $ f
  mapM_ (\(VGenPair n t) -> noError (fromMaybe False (checkEq t (lookup n gens)))) types
  where
    genFilter SGen {} = True
    genFilter _       = False
    noError True  = return ()
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
    cmp o t1' t2
compareGens o (VGen n1) t2 = do
  gens <- getClassGens'
  t1 <- replaceAuto t2 n1 $ getType n1 gens
  cmp o t1 t2
compareGens o t1 (VGen n2) = do
  gens <- getClassGens'
  t2 <- replaceAuto t1 n2 $ getType n2 gens
  cmp o t1 t2
compareGens o t1 t2 = cmp o t1 t2

getType n = (\(SGen t _) -> t) . fromJust . find (\(SGen _ n') -> n == n')

cmp o t1 t2 = do
  res <- compareTypes o t1 t2
  if res
    then return t1
    else makeError o $ AssignTypesMismatch t1 t2

replaceAuto :: VarType -> String -> VarType -> Analyzer' VarType
replaceAuto notAuto n auto
  | auto == VAuto && notAuto /= VAuto = do
    updateScope =<< updateField (SGen notAuto n) . fromJust <$> getClassScope
    return notAuto
  | otherwise = return auto

check :: Offset -> VarType -> VarType -> VarType -> VarType -> Analyzer' VarType
check o wantedDecl wanted actual res = do
  a <- compareTypes o wantedDecl actual
  b <- compareTypes o wanted actual
  recheck a b
  where
    recheck a b
      | a && (b || wanted == VAuto) = return extendedPolyCheck
      | otherwise = makeError o $ AssignTypesMismatch actual wanted
    extendedPolyCheck 
      | wanted /= actual && wanted /= VAuto = wanted 
      | otherwise = extendedIntCheck
    extendedIntCheck =
      case wanted of
        VNum {} -> wanted
        _       -> res

checkFunctionUniqueness o name t args = do
  fns <- find' name
  noVarWithSameName fns
  allReturnTheSame fns
  notSameArguments fns
  where
    noVarWithSameName fns =
      if all isFunction fns
        then return ()
        else makeError o $ VariableWithSameName (show name)
    allReturnTheSame fns =
      if all (\(SFunction o n p t' a) -> t == t') fns
        then return ()
        else makeError o $ FunctionDifferentReturnType (show name) fns t
    notSameArguments fns =
      if all (\g -> length g == 1) .
         group .
         map (\(SFunction _ _ _ _ a) -> map (\(FunArg t _) -> t) a) . filter (\(SFunction i _ _ _ _) -> fOffset i <= o) $
         fns
        then return ()
        else makeError o $ FunctionRepetition fns
