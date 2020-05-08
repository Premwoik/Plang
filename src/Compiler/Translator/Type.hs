{-# LANGUAGE TypeSynonymInstances #-}

module Compiler.Translator.Type where

import           AST
import           Control.Exception
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader   (ReaderT, asks)
import           Control.Monad.State
import           Control.Monad.Writer   (WriterT)
import qualified Data.Map               as Map
import Data.List(intercalate)

type Translator = ReaderT Dependencies Translator' [String]

--type RTranslator t = RTranslator' t [String]
--
--type RTranslator' t a = ReaderT t Translator' a
type Translator' = WriterT [String] (State Storage)

type DPair = (VarType, ExecutableType)

-- vars, funcDec, classDec
data ExecutableType
  = ClassDecl
  | FunctionDecl
  | Instance
  deriving (Show)
  
injectTranslator getter arg = asks getter >>= (\x -> x arg)

data Dependencies =
  Dependencies
    { aExprTranslatorGetter     :: AExpr -> Translator
    , bExprTranslatorGetter     :: BExpr -> Translator
    , stmtTranslatorGetter      :: Stmt -> Translator
    , fnStmtTranslatorGetter    :: FunctionStmt -> Translator
    , classStmtTranslatorGetter :: ClassStmt -> Translator
    }
newtype Storage =
  Storage
    { toImport :: [String]
    }
  deriving (Show)

emptyStorage = Storage []

addImport :: String -> Translator
addImport n = do
  modify (\s -> s {toImport = n : toImport s})
  return []

newtype TranslatorException =
  UnsupportedTypeException String
  deriving (Show)

instance Exception TranslatorException



typeToString :: VarType -> String
typeToString t =
  case t of
    VInt         -> "int"
    VFloat       -> "float"
    VString      -> "String"
    VVoid        -> "void"
    VAuto        -> "auto"
    VChar        -> "char"
    VBool        -> "bool"
    VBlank       -> ""
    VFn t -> typeToString (last t) ++ "(*)" ++ "(" ++ intercalate "," (map typeToString (init t)) ++ ")"
    VFnNamed n t -> typeToString (last t) ++ "(*"++ n ++")" ++ "(" ++ intercalate "," (map typeToString (init t)) ++ ")"
    VRef t -> typeToString t ++ "&"
    VCopy t -> typeToString t
    VClass c gen _ -> c ++ genStr gen
    VGen t       -> t
    VGenPair _ t -> typeToString t
    VPointer t SharedPtr -> "shared_ptr<" ++ typeToString t ++ ">"
    VPointer t UniquePtr -> "unique_ptr<" ++ typeToString t ++ ">"
    x            -> error (show x)
  where
    genStr [] = ""
    genStr g  = "<" ++ (intercalate ", " . map translateGen) g ++ ">"
    translateGen (VGenPair _ (VClass n g _)) = n ++ genStr g
    translateGen g                           = typeToString g


newLine :: String -> String
newLine x = x ++ "\n"

makeIndent :: String -> String
makeIndent x = "   " ++ x

blockTranslator :: BodyBlock -> Translator
blockTranslator = blockTranslator' (injectTranslator stmtTranslatorGetter)

blockTranslator' :: (a -> Translator) -> [a] -> Translator
blockTranslator' trans x = concat' <$> mapM trans x
  where
    concat' = map makeIndent . concat

