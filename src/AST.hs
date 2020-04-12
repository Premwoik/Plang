{-# LANGUAGE OverloadedStrings #-}

module AST where

import           Control.Monad.State (StateT)
import           Data.Text           (Text)
import           Data.Void
import           Text.Megaparsec     hiding (State)
import Debug.Trace

type Parser = Parsec Void Text

newtype AST =
  AST [Stmt]
  deriving (Show, Eq)

data Imported =
  IFile String AST
  deriving (Show, Eq)

data Stmt
  = Function Int String VarType [FunArg] [FunctionStmt]
  | Import Int String [String]
  | ClassExpr Int String [String] [ClassStmt]
  | Skip Int
  | Assign Int [String] VarType AExpr
  -- | NATIVE
  | NativeAssignDeclaration Int String String VarType
  | NativeFunction Int String String VarType [FunArg]
  | NativeClass Int String String [String] [ClassStmt]
  | LinkPath Int String
  deriving (Show, Eq)

--  | FunctionDecl String VarType (Maybe [FunArg])
--  FOR TRANSLATION ONLY
--  -------------------------------------------------------
data ClassStmt
  = ClassAssign Int [String] VarType AExpr
  | Method Int String VarType [FunArg] [FunctionStmt]
  | Constructor Int String [FunArg] [FunctionStmt]
  -- | NATIVE
  | NativeMethod Int String VarType [FunArg]
  deriving (Show, Eq)

--  FOR TRANSLATION ONLY
data FunctionStmt
  = AssignFn Int [String] VarType AExpr
  | WhileFn Int BExpr [FunctionStmt]
  | ForFn Int AExpr AExpr [FunctionStmt]
  | IfFn Int [(BExpr, [FunctionStmt])]
  | ReturnFn Int AExpr
  | OtherFn Int AExpr
  | Pass
  deriving (Show, Eq)

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
  deriving (Show, Eq)

data BBinOp
  = And
  | Or
  deriving (Show, Eq)

data RBinOp
  = Greater
  | Less
  | Equal
  | EqLess
  | EqGreater
  deriving (Show, Eq)

data AExpr
-- | Var name generics (func args/if is a func) more
  = Var Offset String [VarType] (Maybe [AExpr]) (Maybe AExpr)
  | ScopeMark Offset String AExpr
  | ABracket Offset AExpr
  | IntConst Offset Integer
  | FloatConst Offset Float
  | ListVar Offset [AExpr] (Maybe VarType)
  | StringVal Offset String
  | Range Offset AExpr AExpr
  | Fn Offset Bool (Maybe [FunArg]) AExpr
  | FnBlock Offset (Maybe [FunArg]) [FunctionStmt]
  | If Offset [(BExpr, [FunctionStmt])]
  
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | ABool BExpr
 -- | Only for translation
  | TypedVar VarName VarType (Maybe [AExpr]) (Maybe AExpr)
  | TypedListVar [AExpr] VarType
  | Nop
  deriving (Show, Eq)

type Offset = Int

data VarName 
-- | VName name
  = VName String
-- | VNameNative name native_name
  | VNameNative String String
  deriving (Show, Eq)

type Cond = (BExpr, [FunctionStmt])

data ABinOp
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Show, Eq)

data VarType
  = VInt
  | VFloat
  | VString
  | VVoid
  | VAuto
  | VChar
  -- | VClass name type isPointer (isPointer - default value is false)
  | VClass String [VarType] Bool
  -- | Not for parsing
  | VGen String
  | VGenPair String VarType
  | VRef VarType
  | VPointer VarType PointerType
  | VBlank
  deriving (Show)

instance Eq VarType where
  VInt  == VInt = True
  VFloat == VFloat = True
  VString == VString = True
  VVoid == VVoid = True
  VAuto == VAuto = True
  VBlank == VBlank = True
  (VClass n g p) == (VClass n2 g2 p2) = n == n && g == g2 && p == p2
  (VGen n) == (VGen n2) = n == n2 
  (VGenPair n t) == (VGenPair n2 t2) = n == n2 && t == t2
  (VGenPair _ t) == t2 = t == t2
  t2 == (VGenPair _ t) = t == t2
  (VRef n) == (VRef n2) = n == n2
  (VRef t) == t2 = t == t2
  t2 == (VRef t) = t == t2
  (VPointer t pt) == (VPointer t2 pt2) = t == t2 && pt == pt2
  (VPointer t _) == t2 = t == t2
  t2 == (VPointer t _) = t == t2
  a == b = trace ("Eq error ### left = " ++ show a ++ " | right = " ++ show b) False

data PointerType = UniquePtr | SharedPtr | SharedPtrStar deriving(Show, Eq)

data BoolOp =
  BoolOp
  deriving (Show, Eq)

type BodyBlock = [Stmt]

data FunArg =
  FunArg VarType String
  deriving (Show, Eq)

unwrapVarName (VName n) = n
unwrapVarName (VNameNative _ p) = p

unwrapVarNameForce (VName n) = n
unwrapVarNameForce (VNameNative n _) = n

