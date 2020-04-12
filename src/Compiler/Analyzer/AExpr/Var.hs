{-# LANGUAGE FlexibleContexts #-}
module Compiler.Analyzer.AExpr.Var where

import           Compiler.Analyzer.Type
import           Control.Monad.State      (get, gets, put, modify)
import Control.Monad.Writer(tell)
import           AST
import Compiler.Analyzer.Browser
import Data.Maybe(fromMaybe, listToMaybe)
import Debug.Trace
import Control.Monad.Except(throwError)
import Data.List(find)

markClassAsPointer :: [VarType] -> [VarType]
markClassAsPointer = map mapper
  where
    mapper (VGenPair n c@VClass {}) = VGenPair n (VPointer c SharedPtr)
    mapper c@VClass {} = VPointer c SharedPtr
    mapper e = e

markVarClassAsPointer :: [AExpr] -> [AExpr]
markVarClassAsPointer = map mapper
  where
    mapper (TypedVar n c@VClass {} a m) = TypedVar n (VPointer c SharedPtr) a m
    mapper x = x


extractPtr (Just (VPointer t _)) = Just t
extractPtr t = t

unwrapGen (VGenPair _ t) = t
unwrapGen x = x

fixPtrGenInArgs :: [VarType] -> AExpr -> AExpr
fixPtrGenInArgs gens (TypedVar n t a m) = TypedVar n newType a m
  where
    newType = case find (\t' -> unwrapGen t' == t) gens of
      (Just t') -> unwrapGen t'
      Nothing -> t
fixPtrGenInArgs _ t = t

checkArgs :: [VarType] -> Maybe [AExpr] -> AExprAnalyzer -> Analyzer' (Maybe [AExpr])
checkArgs gen args analyzer =
  case args of
    (Just a) -> Just . map (fixPtrGenInArgs gen .(\(_, _, x) -> x)) <$> mapM analyzer a
    Nothing -> return Nothing

filterArgsMatch Nothing _ SVar {} = True
filterArgsMatch Nothing [] (SFunction _ _ _ _ [])= True
filterArgsMatch Just {} _ SVar {} = False
filterArgsMatch (Just args) gen f = maybeArgsMatch (Just args) gen [f]

checkVarFirst var@(Var offset name gen args more) args' retBuilder obj scopeName=
  case obj of
    Just (SVar _ n p t s) -> retBuilder t (TypedVar (defaultPath p (scaleNameWithScope' [s, n])) t args') more

    Just (SClass _ n p g b) -> do
      if length g == length gen then return () else throwError $ AException offset ("Generic is missing! - " ++ show g)
--          TODO make this generics to zip recurrently not only on actual layer ready
      let gen' = zipWith VGenPair g gen
      let newType = VClass n (markClassAsPointer gen') False
      retBuilder newType (TypedVar (defaultPath p n) newType args') more

    Just(SFunction _ n p t _)-> do
      scope <- getFileName scopeName
      retBuilder t (TypedVar (defaultPath p (newName scope)) t args') more
      where
        newName scope = case scopeName of
          "" -> n
          "g" -> scaleNameWithScope' ["::", n]
          "global" -> scaleNameWithScope' ["::", n] 
          _ -> scaleNameWithScope' [scope, "::", n]
    p -> do
      storage <- gets scopes
      throwError $ AException offset ("Can't find given name! " ++ show var ++ " | " ++ show p ++ " | " ++ show storage)

checkVarMore (Var offset name _ args more) (VClass cName gen _) args' retBuilder method =
      case method of
        Just (SVar _ n p t _) -> retBuilder t (TypedVar (defaultPath p n) t Nothing) more
        Just (SFunction _ n p t _) -> do
          let nType = fixType gen t
          retBuilder nType (TypedVar (defaultPath p n) nType args') more
        x -> throwError $ AException offset ("Can't find that method or field in given class. " ++ show x ++ " | " ++ name ++ " | " ++ cName)


checkVar :: AExpr -> Maybe VarType -> String -> AExprAnalyzer -> Analyzer' AExprRes
checkVar v@(Var offset name gen args more) wantedType scopeName analyzer = 
  case extractPtr wantedType of
-- |  previous was a class
    Just c@(VClass cName gen _) -> do
      args' <- checkArgs gen args analyzer 
      candidate <- listToMaybe . filter (filterArgsMatch args' gen) <$> findInClass cName name
      checkVarMore v c args' retBuilder candidate

-- |  this is first, so it has to be variable global or local
    Nothing -> do
      args' <- checkArgs gen args analyzer 
      candidate <- listToMaybe . filter (filterArgsMatch args' []) <$> find' [scopeName, name]
      checkVarFirst v args' retBuilder candidate scopeName

-- |  error previous was not a class
    (Just x) -> throwError $  NotAClass  (show x ++ " | " ++ show v)
  where
    retBuilder type' var more =
          makeOutput type' var <$> handleMore more (Just type')
    handleMore (Just m) x = Just <$> checkVar m x "" analyzer
    handleMore Nothing _ = return Nothing
    makeOutput _ wrapper (Just (v,i, m)) = (v, i, wrapper (Just m))
    makeOutput t wrapper Nothing = (t, [], wrapper Nothing)

defaultPath p n = case p of
  Just "" -> VName n
  Just p -> VNameNative n p
  Nothing -> VName n
