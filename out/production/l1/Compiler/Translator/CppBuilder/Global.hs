module Compiler.Translator.CppBuilder.Global where

import Control.Monad.Writer

data Nop

newtype CppInclude =
  CppInclude String

data CppType
  = VInt
  | VFloat
  | VVoid
  | VDouble
  | VClass

data CppFunctionBody =
  CppFunctionBody

data CppFunctionArgument =
  CppFunctionArgument

data CppFunction =
  CppFunction
    { cppFunctionName      :: String
    , cppFunctionRType     :: CppType
    , cppFunctionArguments :: [CppFunctionArgument]
    , cppFunctionBody      :: CppFunctionBody
    }

data CppVariable =
  CppVariable
    { cppVariablesType :: CppType
    , cppVariablesName :: String
    , cppVariableValue :: Nop
    }



data CppClass =
  CppClass
    { cppClassMethods      :: Nop
    , cppClassVariables    :: [CppVariable]
    , cppClassConstructors :: Nop
    , cppMemToDeallocate   :: Nop
    , cppMemToAllocate     :: Nop
    }

data CppNamespace =
  CppNamespace
    { cppClasses   :: [CppClass]
    , cppFunctions :: [CppFunction]
    , cppVariables :: [CppVariable]
    }

data CppBuilder =
  CppBuilder
    { cppNamespaces :: [CppNamespace]
    , cppGlobal     :: CppNamespace
    , cppInclude    :: [CppInclude]
    }


--
--instance Monad CppBuilder where
--  return $ CppBuilder [] (CppNamespace [] [] []) []
--  (CppBuilder a b c) >>= f = f a
--
--addInclude :: String -> CppBuilder -> CppBuilder 
--addInclude path b = b
--
--addConst :: String
--addConst = []
