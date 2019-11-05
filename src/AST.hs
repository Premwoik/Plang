{-# LANGUAGE OverloadedStrings #-}
module AST where


import Text.Megaparsec hiding (State)
import Data.Text (Text)
import Data.Void

type Parser = Parsec Void Text

newtype AST
  = AST [Stmt]
  deriving (Show)

data Stmt
  = Function String VarType (Maybe [FunArg]) [FunctionStmt]
   | Import [String]
   | ClassExpr String (Maybe [String]) [ClassStmt]
   | NativeFunction String VarType (Maybe [FunArg])
   | LinkPath String
   | Skip 
--  -------------------------------------------------------
   | Assign String VarType AExpr
--   | While BExpr BodyBlock 
--   | For AExpr AExpr BodyBlock
--   | CasualExpr AExpr
  deriving (Show)

data ClassStmt
  = ClassAssign String VarType AExpr
  | Method String VarType (Maybe [FunArg]) [FunctionStmt]
  deriving (Show)
  
data FunctionStmt
  = AssignFn String VarType AExpr
  | WhileFn BExpr [FunctionStmt]
  | ForFn AExpr AExpr [FunctionStmt]
  | ReturnFn AExpr
  | OtherFn AExpr
  deriving (Show)


data IfCondBody = IfCondBody BExpr [FunctionStmt] deriving (Show)
newtype ElseCondBody = ElseCondBody [FunctionStmt] deriving (Show)

data BoolExpr = BoolExpr deriving (Show)


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
  | IntConst Integer
  | FloatConst Float
  | ListVar [AExpr]
  | StringVal String
  | Range AExpr AExpr
  | Fn Bool (Maybe [FunArg]) AExpr
  | FnBlock (Maybe [FunArg]) [FunctionStmt]
  
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | If IfCondBody [IfCondBody] ElseCondBody -- Maybe ElseCondBody
  deriving (Show)
  

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
  deriving (Show, Eq)

data BoolOp = BoolOp deriving (Show)

type BodyBlock = [Stmt]
--newtype IndentedBlock = IndentedBlock
data FunArg = FunArg VarType String deriving (Show)

