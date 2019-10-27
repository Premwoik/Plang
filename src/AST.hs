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
  = Function String VarType (Maybe [FunArg]) BodyBlock
  | Import String
  | Assign String VarType AExpr
  | While BExpr BodyBlock 
  | For AExpr AExpr BodyBlock
  | CasualExpr AExpr
  | ClassExpr String [Stmt]
  | ReturnExpr AExpr
  | Skip
  deriving (Show)

data ClassStmt
  = ClassStmtOther Stmt
  | ClassConstructor 
  | Method String (Maybe [FunArg]) [MethodStmt]
  deriving (Show)

--data Range = Range AExpr AExpr deriving (Show)

data MethodStmt
  = MethodStmtFn FunctionStmt
  deriving (Show)
  
data FunctionStmt
  = FunctionExpr AExpr
  deriving (Show)

data IfCondBody = IfCondBody BExpr BodyBlock deriving (Show)
newtype ElseCondBody = ElseCondBody BodyBlock deriving (Show)

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
  deriving (Show)

data BoolOp = BoolOp deriving (Show)

type BodyBlock = [Stmt]
--newtype IndentedBlock = IndentedBlock
data FunArg = FunArg VarType String deriving (Show)

