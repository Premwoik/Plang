module Compiler.Translator.Type where

import Control.Monad.Writer (WriterT)
import Control.Monad.Reader(ReaderT)
import Control.Monad.State
import Control.Monad.Identity (Identity)
import AST(VarType)
import qualified Data.Map as Map


type Translator = Translator' [String]
type RTranslator t = RTranslator' t [String]

type RTranslator' t a = ReaderT t Translator' a
type Translator' = WriterT [String] (State Storage)


type DPair = (VarType, ExecutableType)
-- vars, funcDec, classDec

data ExecutableType = ClassDecl | FunctionDecl | Instance deriving (Show)

data Storage = Storage
  { globalInstances :: Map.Map String DPair
  , localInstances :: Map.Map String DPair
  , cache :: StorageCache
  } deriving (Show)

lookupInstance :: String -> Storage -> Maybe DPair
lookupInstance key s =
  case Map.lookup key (globalInstances s) of
    t@Just {} -> t
    Nothing -> case Map.lookup key (localInstances s) of
      t@Just {} -> t
      Nothing -> Nothing

data StorageCache = EmptyCache | TypeCache [VarType] deriving (Show)

emptyStorage = Storage Map.empty Map.empty EmptyCache

--runTranslator :: Translator ->








--
--declaredClassNames :: [Stmt] -> [(String, DPair)]
--declaredClassNames = map (\ (ClassExpr n _) -> (n, (VVoid, ClassDecl))) . filter isClass
--  where
--    isClass x =
--     case x of
--      ClassExpr {} -> True
--      _ -> False
--
--declaredFunctionNames :: [Stmt] -> [(String, DPair)]
--declaredFunctionNames = map (\(Function n t _ _) -> (n, (t, FunctionDecl))) . filter isFunc
--  where
--    isFunc x =
--      case x of
--        Function {} -> True
--        _ -> False
--
--declaredGlobalVariables :: [Stmt] -> [(String, DPair)]
--declaredGlobalVariables = map (\(Assign n t _) -> (n, (t, Instance))) . filter isVar
--  where
--    isVar x =
--      case x of
--        Assign {} -> True
--        _ -> False
--
