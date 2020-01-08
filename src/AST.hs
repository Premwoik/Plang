{-# LANGUAGE OverloadedStrings #-}

module AST where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Control.Monad.State(StateT)

type Parser = Parsec Void Text

newtype AST =
  AST [Stmt]
  deriving (Show)

data Stmt
  = Function Int String VarType [FunArg] [FunctionStmt]
  | Import Int [String]
  | ClassExpr Int String [String] [ClassStmt]
  | Skip Int
  | Assign Int [String] VarType AExpr
  | NativeAssignDeclaration Int String String VarType
  | NativeFunction Int String String VarType [FunArg]
  | NativeClass Int String String [String] [ClassStmt]
  | LinkPath Int String
--  FOR TRANSLATION ONLY
  | FunctionDecl String VarType (Maybe [FunArg])
  deriving (Show)

--  -------------------------------------------------------
data ClassStmt
  = ClassAssign Int [String] VarType AExpr
  | Method Int String VarType [FunArg] [FunctionStmt]
  | NativeMethod Int String VarType [FunArg]
--  FOR TRANSLATION ONLY
  | Constructor Int String [FunArg] [FunctionStmt]
  deriving (Show)

data FunctionStmt
  = AssignFn Int [String] VarType AExpr
  | WhileFn Int BExpr [FunctionStmt]
  | ForFn Int AExpr AExpr [FunctionStmt]
  | IfFn Int [(BExpr, [FunctionStmt])]
  | ReturnFn Int AExpr
  | OtherFn Int AExpr
  ----
  deriving (Show)

data AssignId a = AListId String (AListGetSet a) | AValueId String
data AListGetSet a = ALRange a a | ALIndex a

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
--  FOR TRANSLATION ONLY
  | DefineVar String (Maybe [AExpr])
  | TypedVar String VarType (Maybe [AExpr]) (Maybe AExpr)
  | Nop
  deriving (Show)

-- ONLY FOR TRANSLATION
--  | 
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
  | VClass String [VarType]
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

--newtype IndentedBlock = IndentedBlock
data FunArg =
  FunArg VarType String
  deriving (Show)

