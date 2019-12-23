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
  = Function Int String VarType (Maybe [FunArg]) [FunctionStmt]
  | FunctionDecl String VarType (Maybe [FunArg])
  | Import Int [String]
  | ClassExpr Int String (Maybe [String]) [ClassStmt]
  | NativeFunction Int String VarType (Maybe [FunArg])
  | LinkPath Int String
  | Skip Int
  | Assign Int String VarType AExpr
  deriving (Show)

--  -------------------------------------------------------
data ClassStmt
  = ClassAssign Int String VarType AExpr
  | Method Int String VarType (Maybe [FunArg]) [FunctionStmt]
  | Constructor Int String [FunctionStmt]
  deriving (Show)

data FunctionStmt
  = AssignFn Int String VarType AExpr
  | WhileFn Int BExpr [FunctionStmt]
  | ForFn Int AExpr AExpr [FunctionStmt]
  | IfFn Int [(BExpr, [FunctionStmt])]
  | ReturnFn Int AExpr
  | OtherFn Int AExpr
  ----
  deriving (Show)


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
  = Var String (Maybe [AExpr]) (Maybe AExpr)
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
  | If [(BExpr, [FunctionStmt])]
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
  | VClass String
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

