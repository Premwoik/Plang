module Compiler.Translator.CppBuilder.Class where

import           Control.Monad.State

class Builder a where
  build :: a -> String

type CppBuilder = String

data CppClass =
  CppClass
    { cppClassVars    :: [CppClassVar]
    , cppClassMethods :: [CppClassMethod]
    }

data CppClassVar =
  CppClassVar

data CppClassMethod =
  CppClassMethod

data CppFn =
  CppFn
  
data CppVar =
  CppVar

data CppNamespace =
  CppNamespace
    { cppNsClasses :: [CppClass]
    , cppNsFns     :: [CppFn]
    , cppNsVars :: [CppVar]
    }
