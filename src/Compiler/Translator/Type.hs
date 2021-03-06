{-# LANGUAGE TypeSynonymInstances #-}

module Compiler.Translator.Type where

import           AST
import           Control.Exception
import           Control.Monad.Identity (Identity)
import           Control.Monad.Reader   (ReaderT, asks)
import           Control.Monad.State
import           Control.Monad.Writer   (WriterT)
import           Data.List              (intercalate)
import qualified Data.Map               as Map

type Translator = ReaderT Dependencies (WriterT [String] (State Storage)) [String]

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
    VInt -> "int"
    VFloat -> "float"
    VString -> "String"
    VVoid -> "void"
    VAuto -> "auto"
    VChar -> "char"
    VBool -> "bool"
    VBlank -> ""
    VNum NUInt8 -> "uint8_t"
    VNum NUInt16 -> "uint16_t"
    VNum NUInt32 -> "uint32_t"
    VNum NInt8 -> "int8_t"
    VNum NInt16 -> "int16_t"
    VNum NInt32 -> "int32_t"
    VFn t CMOff -> typeToString (last t) ++ "(*)" ++ "(" ++ intercalate "," (map typeToString (init t)) ++ ")"
    VFn t CMOn -> "auto"
    VFn t CMAuto ->
      "nonstd::function<" ++ typeToString (last t) ++ "(" ++ intercalate "," (map typeToString (init t)) ++ ")>"
    VFnNamed n t CMOff ->
      typeToString (last t) ++ "(*" ++ n ++ ")" ++ "(" ++ intercalate "," (map typeToString (init t)) ++ ")"
    VFnNamed n t x -> typeToString (VFn t x) ++ " " ++ n
    VRef t -> typeToString t ++ "&"
    VCopy t -> typeToString t
    VClass c gen -> unwrapVarName c ++ genStr gen
    VGen t -> t
    VGenPair _ t -> typeToString t
    VPointer t SharedPtr -> "shared_ptr<" ++ typeToString t ++ ">"
    VPointer t UniquePtr -> "unique_ptr<" ++ typeToString t ++ ">"
    VPointer t NativePtr -> typeToString t ++ "*"
  where
    genStr [] = ""
    genStr g  = "<" ++ (intercalate ", " . map translateGen) g ++ ">"
    translateGen (VGenPair _ (VClass n g)) = unwrapVarName n ++ genStr g
    translateGen g                         = typeToString g

--    x            -> error (show x)
newLine :: String -> String
newLine x = x ++ "\n"

makeIndent :: String -> String
makeIndent x = "   " ++ x

blockTranslator :: BodyBlock -> Translator
blockTranslator = blockTranslator' (injectTranslator stmtTranslatorGetter)

blockTranslator' :: (a -> Translator) -> [a] -> Translator
blockTranslator' trans x = concat' <$> mapM trans x
  where
    concat' = map makeIndent . filter (not . null) . concat

translateGenerics [] = ""
translateGenerics l = "template<" ++ (intercalate ", " . map (\x -> "typename " ++ x)) l ++ ">"
