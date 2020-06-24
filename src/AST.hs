{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module AST where

import           Control.Monad.State  (StateT)
import qualified Data.Set             as Set
import           Data.Text            (Text, unpack)
import           Data.Void
import           Debug.Trace
import           Text.Megaparsec      hiding (State)

--type Parser = Parsec Void Text
import           Control.Applicative  hiding (some)
import           Data.List            (intercalate, nub)
import           Data.Set             (Set)
import qualified Data.Set             as Set
import           Data.Text            (Text)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

data Custom
  = TrivialWithLocation
      [String] -- position stack
      [String] -- context stack
      (Maybe (ErrorItem Char))
      (Set (ErrorItem Char))
  | FancyWithLocation
      [String] -- position stack
      [String] -- context stack
      (ErrorFancy Void) -- Void, because we do not want to allow to nest Customs
  deriving (Eq, Ord, Show)

instance ShowErrorComponent Custom where
  showErrorComponent (TrivialWithLocation stack ctxStack us es) =
    parseErrorTextPretty (TrivialError @Text @Void undefined us es) ++
    showPosStack stack ++ "\nTIPS:\n -" ++ showCtxStack (nub ctxStack)
  showErrorComponent (FancyWithLocation stack ctxStack cs) =
    parseErrorTextPretty (FancyError @Text @Void undefined (Set.singleton cs)) ++
    showPosStack stack ++ "\nTIPS:\n -" ++ showCtxStack (nub ctxStack)

showPosStack :: [String] -> String
showPosStack = intercalate ", " . fmap ("in " ++)

showCtxStack :: [String] -> String
showCtxStack = intercalate "\n -"

emptyError :: Parser ()
emptyError = failure Nothing (Set.fromList [])

type Parser = Parsec Custom Text

addCtxToIndent :: String -> Parser a -> Parser a
addCtxToIndent context p = do
  r <- observing p
  case r of
    Left (TrivialError _ us es) -> fancyFailure . Set.singleton . ErrorCustom $ TrivialWithLocation [] [context] us es
    Left (FancyError _ xs) -> do
      let f (ErrorIndentation ord rlvl alvl) =
            ErrorCustom $ FancyWithLocation [] [context] (ErrorIndentation ord rlvl alvl)
          f (ErrorCustom (FancyWithLocation ps pcs cs@ErrorIndentation {})) =
            ErrorCustom $ FancyWithLocation ps (context : pcs) cs
          f x = x
      fancyFailure (Set.map f xs)
    Right x -> return x

addContext :: String -> Parser a -> Parser a
addContext context p = do
  r <- observing p
  case r of
    Left (TrivialError _ us es) -> fancyFailure . Set.singleton . ErrorCustom $ TrivialWithLocation [] [context] us es
    Left (FancyError _ xs) -> do
      let f (ErrorFail msg) = ErrorCustom $ FancyWithLocation [] [context] (ErrorFail msg)
          f (ErrorIndentation ord rlvl alvl) =
            ErrorCustom $ FancyWithLocation [] [context] (ErrorIndentation ord rlvl alvl)
          f (ErrorCustom (TrivialWithLocation ps pcs us es)) =
            ErrorCustom $ TrivialWithLocation ps (context : pcs) us es
          f (ErrorCustom (FancyWithLocation ps pcs cs)) = ErrorCustom $ FancyWithLocation ps (context : pcs) cs
      fancyFailure (Set.map f xs)
    Right x -> return x

addLocation :: String -> Parser a -> Parser a
addLocation location p = do
  r <- observing p
  case r of
    Left (TrivialError _ us es) -> fancyFailure . Set.singleton . ErrorCustom $ TrivialWithLocation [location] [] us es
    Left (FancyError _ xs) -> do
      let f (ErrorFail msg) = ErrorCustom $ FancyWithLocation [location] [] (ErrorFail msg)
          f (ErrorIndentation ord rlvl alvl) =
            ErrorCustom $ FancyWithLocation [location] [] (ErrorIndentation ord rlvl alvl)
          f (ErrorCustom (TrivialWithLocation ps pcs us es)) =
            ErrorCustom $ TrivialWithLocation (location : ps) pcs us es
          f (ErrorCustom (FancyWithLocation ps pcs cs)) = ErrorCustom $ FancyWithLocation (location : ps) pcs cs
      fancyFailure (Set.map f xs)
    Right x -> return x

newtype AST =
  AST [Stmt]
  deriving (Show, Eq)

data Imported
  -- | IFile name path ast
      =
  IFile String String AST
  deriving (Show, Eq)

data Stmt
  -- | Function offset name generics returnType arguments body
  = Function Int String [String] VarType [FunArg] [FunctionStmt]
  -- | Import offset alias path
  | Import Int String [String]
  -- | ClassExpr offset name generics parents body
  | ClassExpr Int String [String] [VarType] [ClassStmt]
  -- | Skip offset
  | Skip Int
  -- | Assign offset leftAExpr type rightAExpr
  | Assign Int AExpr VarType AExpr

  -- | NATIVE
  | NativeAssignDeclaration Int String String VarType
  | NativeFunction Int String String [String] VarType [FunArg]
  | NativeClass Int String String [String] [ClassStmt]
  | LinkPath Int String
  deriving (Show, Eq)

--  | FunctionDecl String VarType (Maybe [FunArg])
--  FOR TRANSLATION ONLY
--  -------------------------------------------------------
data ClassStmt
  = ClassAssign
      { classAssignOffset  :: Int
      , classAssignLeft    :: AExpr
      , classAssignType    :: VarType
      , classAssignDetails :: MethodDetails
      , classAssignRight   :: AExpr
      }
  | Method
      { methodOffset  :: Int
      , methodName    :: String
      , methodRet     :: VarType
      , methodArgs    :: [FunArg]
      , methodDetails :: MethodDetails
      , methodBody    :: [FunctionStmt]
      }
  | Constructor
      { constructorOffset :: Int
      , constructorName   :: String
      , constructorArgs   :: [FunArg]
      , constructorBody   :: [FunctionStmt]
      }
  | ClassDecorator
      { decOffset :: Offset
      , decType   :: DecoratorType
      , decStmt   :: ClassStmt
      }
  -- | NATIVE
  | NativeMethod Int String VarType [FunArg]
  deriving (Show, Eq)

defaultMethodDetails = MethodDetails "public" False

data MethodDetails =
  MethodDetails
    { visibilityMD :: String
    , isOverrideMD :: Bool
    }
  deriving (Show, Eq)

data DecoratorType
  = PrivateDec
  | PublicDec
  | OverrideDec
  | CustomDec String
  deriving (Show, Eq)

--  FOR TRANSLATION ONLY
data FunctionStmt
  = AssignFn Int AExpr VarType AExpr
  | WhileFn Int BExpr [FunctionStmt]
  | ForFn Int AExpr AExpr [FunctionStmt]
  | IfFn Int [(BExpr, [FunctionStmt])]
  | ReturnFn Int (Maybe AExpr)
  | OtherFn Int AExpr
  | Break Int
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
  | BoolVar AExpr
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
  | NotEqual
  | EqLess
  | EqGreater
  deriving (Show, Eq)

data AExpr
  = Var Offset String [VarType] (Maybe [AExpr]) (Maybe AExpr)
  | Optional Offset AExpr OptionalType
  | ScopeMark Offset String AExpr
  | ABracket Offset AExpr
  | ABracketApply Offset AExpr [AExpr]
  | IntConst Offset Integer
  | FloatConst Offset Float
  | ListVar Offset [AExpr] (Maybe VarType)
  | StringVal Offset String
  | Range Offset (Maybe AExpr) AExpr AExpr
  | LambdaFn Offset CaptureMode VarType [FunArg] [FunctionStmt]
  | If Offset [(BExpr, [FunctionStmt])]
  | Neg AExpr
  | ABinary ABinOp AExpr AExpr
  | ABool BExpr
  -- | Null offset
  | Null Int
 -- | Only for translation
  | NativePtrRes AExpr
  | NativePtrInput AExpr
  | TypedVar VarName VarType (Maybe [AExpr]) (Maybe AExpr)
  | TypedABinary VarType ABinOp AExpr AExpr
  | TypedListVar [AExpr] VarType
  | Nop
  deriving (Show, Eq)

-- | Var offset name generics (func args/if is a func) more
data OptionalType
  = UnknownOT
  | NullOT
  | BoolOT
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
  | Modulo
  deriving (Show, Eq)

data VarType
  = VInt
  | VFloat
  | VNum NumType
  | VString
  | VBool
  | VVoid
  | VAuto
  | VChar
  | VFn [VarType] CaptureMode
  -- | VClass name type
  | VClass VarName [VarType]
  -- | Not for parsing
  | VFnNamed String [VarType] CaptureMode
  | VGen String
  | VGenPair String VarType
  | VRef VarType
  | VCopy VarType
  | VPointer VarType PointerType
  | VBlank
  deriving (Show)

data CaptureMode
  = CMOn
  | CMAuto
  | CMOff
  deriving (Show, Eq)

instance Eq VarType where
  VInt == VInt = True
  VFloat == VFloat = True
  VString == VString = True
  VChar == VNum NUInt8 = True
  VNum x == VNum y = True
  VBool == VBool = True
  VVoid == VVoid = True
  VAuto == VAuto = True
  VInt == VFloat = True
  VNum _ == VInt = True
  VInt == VNum _ = True
  VFloat == VInt = True
  VBlank == VBlank = True
  VFn x1 _ == VFn x2 _ = x1 == x2
  (VClass n g) == (VClass n2 g2) = unwrapVarNameForce n == unwrapVarNameForce n2 && g == g2
  (VGen n) == (VGen n2) = n == n2
  (VGen n) == (VClass n2 _) = eqClassName n n2
  (VClass n2 _) == (VGen n) = eqClassName n n2
  (VGenPair n t) == (VGenPair n2 t2) = n == n2 && t == t2
  (VGenPair _ t) == t2 = t == t2
  t2 == (VGenPair _ t) = t == t2
  (VRef n) == (VRef n2) = n == n2
  (VRef t) == t2 = t == t2
  t2 == (VRef t) = t == t2
  VCopy t1 == VCopy t2 = t1 == t2
  (VCopy t) == t2 = t == t2
  t2 == (VCopy t) = t == t2
  (VPointer t pt) == (VPointer t2 pt2) = t == t2 && pt == pt2
  (VPointer t _) == t2 = t == t2
  t2 == (VPointer t _) = t == t2
  a == b = False --trace ("Eq error ### left = " ++ show a ++ " | right = " ++ show b) False

eqClassName :: String -> VarName -> Bool
eqClassName n (VName a)         = n == a
eqClassName n (VNameNative a _) = n == a

data PointerType
  = UniquePtr
  | SharedPtr
  | NativePtr
  deriving (Show, Eq)

data NumType
  = NUInt8
  | NUInt16
  | NUInt32
  | NInt8
  | NInt16
  | NInt32
  deriving (Show, Eq)

--  = NInt
--  | NFloat
data BoolOp =
  BoolOp
  deriving (Show, Eq)

type BodyBlock = [Stmt]

data FunArg =
  FunArg VarType String
  deriving (Show, Eq)

unwrapClassName (VClass n _) = n

unwrapVarName (VName n)          = n
unwrapVarName (VNameNative n "") = n
unwrapVarName (VNameNative _ p)  = p

unwrapVarNameForce (VName n)         = n
unwrapVarNameForce (VNameNative n _) = n
