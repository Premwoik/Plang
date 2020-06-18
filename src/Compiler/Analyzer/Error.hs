module Compiler.Analyzer.Error
  ( makeError
  , SemanticError(..)
  ) where

import           AST
import           Compiler.Analyzer.Type
import           Control.Monad.Except

data SemanticError
  = AssignTypesMismatch VarType VarType
  | NotAllowedGlobalMod String
  | NotAllowedScopeMarker
  | NotAllowedBrakeStmt
  | NotAllowedOptionalUse VarType
  | NotProvidedListType
  | NotAllElementsHaveSameType [AExpr] (Maybe VarType)
  | NotAllowedRangeType
  | ArgumentsTypeMissing
  | ScopeNoExist String [String]
  | VariableWithSameName String
  | FunctionDifferentReturnType String [ScopeField] VarType
  | FunctionRepetition [ScopeField]
  | WrongGenericType
  | VariableMissing String
  | GenericMissing String
  | ClassVariableMissing String String
  | NotAClass String
  | ApplyNotAFunction
  | CustomError String
  deriving (Show)

makeError :: Int -> SemanticError -> Analyzer' a
makeError offset err = do
  mod <- getModInfo offset
  let text = errorToText err
  throwError $ CustomAException mod text

errorToText err =
  case err of
    AssignTypesMismatch actual wanted ->
      "Types don't match. You tried to assign " ++
      show actual ++ " when should be " ++ show wanted ++ ".\n" ++ show actual ++ " =/= " ++ show wanted
    NotAllowedGlobalMod name -> "Global variables cannot be redefined and reallocated. Var name: " ++ name
    NotAllowedScopeMarker -> "Global and class assign can't hava a scope marker."
    NotAllowedBrakeStmt -> "You can't use break statement here! Break stmt can be only used inside a loop body."
    NotAllowedOptionalUse t ->
      "Optional mark \"?\" can't be use type - " ++
      show t ++
      "\nIt can be used only on: Pointers - to check if pointer is not null or on native Classes - to check class bool operator (it need to be implemented)"
    NotProvidedListType -> "Empty list must have providen a type"
    NotAllElementsHaveSameType elems wantedType ->
      "Not all elems are the same type in list: " ++ show elems ++ " wantedType: " ++ show wantedType
    NotAllowedRangeType -> "In range all numbers have to be an Integers"
    ArgumentsTypeMissing -> "All arguments type have to be defined!"
    ScopeNoExist name scopes -> "Scope with name \"" ++ name ++ "\" doesn't exist.\nExist scopes: " ++ show scopes
    VariableWithSameName name -> "There exists a variable with same name - " ++ name
    FunctionDifferentReturnType name fns t ->
      "Not each function with name - " ++
      name ++ " - return the same type.\n" ++ show fns ++ "\nAll above declarations should return: " ++ show t
    FunctionRepetition fns -> "The same function just exists!\n" ++ show fns
    WrongGenericType -> "There is something wrong with generic type that you passed"
    VariableMissing name -> "Can't find given name - " ++ name ++ "!"
    GenericMissing name -> "Generic is missing! - " ++ name ++ "!"
    ClassVariableMissing cName name ->
      "Class of type - " ++ cName ++ " don't have method or variable named - " ++ name ++ "!"
    NotAClass name -> "Variable - " ++ name ++ " is not of Class type."
    ApplyNotAFunction -> "Only functions can be applied!"
    CustomError s -> s
--    e -> error $ "Unsupported error code - " ++ show e
