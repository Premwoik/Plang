{-# LANGUAGE OverloadedStrings #-}

module AST where

import           Control.Monad.State (StateT)
import           Data.Text           (Text)
import           Data.Void
import           Text.Megaparsec     hiding (State)

type Parser = Parsec Void Text

newtype AST =
  AST [Stmt]
  deriving (Show)

data Imported =
  IFile String AST
  deriving (Show)

data Stmt
  = Function Int String VarType [FunArg] [FunctionStmt]
  | Import Int [String]
  | ClassExpr Int String [String] [ClassStmt]
  | Skip Int
  | Assign Int [String] VarType AExpr
  -- | NATIVE
  | NativeAssignDeclaration Int String String VarType
  | NativeFunction Int String String VarType [FunArg]
  | NativeClass Int String String [String] [ClassStmt]
  | LinkPath Int String
  deriving (Show)

--  | FunctionDecl String VarType (Maybe [FunArg])
--  FOR TRANSLATION ONLY
--  -------------------------------------------------------
data ClassStmt
  = ClassAssign Int [String] VarType AExpr
  | Method Int String VarType [FunArg] [FunctionStmt]
  | Constructor Int String [FunArg] [FunctionStmt]
  -- | NATIVE
  | NativeMethod Int String VarType [FunArg]
  deriving (Show)

--  FOR TRANSLATION ONLY
data FunctionStmt
  = AssignFn Int [String] VarType AExpr
  | WhileFn Int BExpr [FunctionStmt]
  | ForFn Int AExpr AExpr [FunctionStmt]
  | IfFn Int [(BExpr, [FunctionStmt])]
  | ReturnFn Int AExpr
  | OtherFn Int AExpr
  | Pass
  deriving (Show)

data AssignId a
  = AListId String (AListGetSet a)
  | AValueId String

data AListGetSet a
  = ALRange a a
  | ALIndex a

data BExpr
  = BoolConst Bool
  | Not BExpr
  | BBinary BBinOp BExpr BExpr
  | RBinary RBinOp AExpr AExpr
  deriving (Show)

data BBinOp
  = And
  | Or
  deriving (Show)

data RBinOp
  = Greater
  | Less
  | Equal
  | EqLess
  | EqGreater
  deriving (Show)

data AExpr
-- | Var name generics (func args/if is a func) more
  = Var String [VarType] (Maybe [AExpr]) (Maybe AExpr)
  | ABracket AExpr
  | IntConst Integer
  | FloatConst Float
  | ListVar [AExpr]
  | StringVal String
  | Range AExpr AExpr
  | Fn Bool (Maybe [FunArg]) AExpr
  | FnBlock (Maybe [FunArg]) [FunctionStmt]
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | ABool BExpr
  | If [(BExpr, [FunctionStmt])]
 -- | Only for translation
  | TypedVar String VarType (Maybe [AExpr]) (Maybe AExpr)
  | Nop
  deriving (Show)

type Cond = (BExpr, [FunctionStmt])

data ABinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show)

data VarType
  = VInt
  | VFloat
  | VString
  | VVoid
  | VAuto
  | VChar
  | VGenComplex VarType [VarType]
  -- | VClass name type isPointer (isPointer - default value is false)
  | VClass String [VarType] Bool
  -- | Not for parsing
  | VGen String
  | VGenPair String VarType
  | VRef String
  | Pointer VarType
  | VBlank
  deriving (Show, Eq)

data BoolOp =
  BoolOp
  deriving (Show)

type BodyBlock = [Stmt]

data FunArg =
  FunArg VarType String
  deriving (Show)