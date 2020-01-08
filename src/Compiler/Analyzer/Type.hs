module Compiler.Analyzer.Type where

import           Control.Exception

import           AST
import           Control.Monad.State  (State)

import           Control.Monad.State  (get, gets, modify, put)
--import Compiler.Translator.Type
import           Control.Monad.Writer (WriterT)
import Debug.Trace

type Analyzer' a = WriterT [String] (State Storage) a

type Analyzer = Analyzer' [String]

data AnalyzerException
  = IncorrectExprException
  | UnknownMethodName
  | NotAClass String
  | NotAMethod String
  | VariableNotExist String
  | TypesMismatch String
  | UnsupportedTypeException String
  deriving (Show)

data Storage =
  Storage
    { global :: [Stmt]
    , local  :: LocalStorage
    , cache  :: StorageCache
    }
  deriving (Show)

emptyStorage = Storage [] LocalEmpty EmptyCache

type Scope = [FunctionStmt]

data LocalStorage
  = LocalClassScope Stmt (Maybe ClassStmt) [Scope]
  | LocalFunc Stmt [Scope]
  | LocalEmpty
  deriving (Show)

getType :: LocalStorage -> VarType
getType (LocalClassScope (ClassExpr _ n _ _) Nothing _) = VClass n []
getType (LocalClassScope _ (Just (Method _ _ t _ _)) _) = t
getType (LocalFunc (Function _ _ t _ _) _) = t
getType t = throw $ UnsupportedTypeException $ show t

setType :: VarType -> LocalStorage -> LocalStorage
setType t (LocalClassScope c (Just (Method o n _ a b)) sc) = LocalClassScope c (Just (Method o n t a b)) sc
setType t (LocalFunc (Function o n _ a b) x) = LocalFunc (Function o n t a b) x
setType t l = throw $ UnsupportedTypeException $ show  l

setStmt:: Stmt -> Analyzer' ()
setStmt s =
  case s of
    class'@ClassExpr {} -> modify (\s -> s {local = LocalClassScope class' Nothing []})
    class'@NativeClass {} -> modify (\s -> s {local = LocalClassScope class' Nothing []})
    func@Function {} -> modify (\s -> s {local = LocalFunc func []})
    _ -> throw $ UnsupportedTypeException $ "setStmt | " ++ show s
--    _ -> throw $ NotAClass $ "Function setClass need class as parameter. Indstead of that it got: " ++ show s

setMethod :: ClassStmt -> Analyzer' ()
setMethod s =
  case s of
    method@Method {} -> do
      state <- get
      let (LocalClassScope class' _ _) = local state
      put $ state {local = LocalClassScope class' (Just method) []}
    _ -> throw $ NotAMethod $ "Function setMethod need a method as parameter. Instead of that it got: " ++ show s


replaceInFunc:: FunctionStmt -> Analyzer' ()
replaceInFunc s = do
  scopes <- unwrap <$> gets local
  updateFnScopes . map (map tryReplace) $ scopes
  where
    unwrap (LocalFunc _ x ) = x
    unwrap (LocalClassScope _ _ x) = x
    unwrap _ = error "Accept only LocalFunc wrapper"
    tryReplace a = case (a, s) of
      (l@(AssignFn o1 _ _ _ ), r@(AssignFn o2 _ _ _ )) -> if o1 == o2 then r else l 
      (l, _) ->  l

--replaceInClass :: ClassStmt -> Analyzer'()
--replaceInClass s = do
--  class' <- getClass
--  let (ClassExpr o n g b) = class'
--  let nBody = map replace b 
--  return ()
--  where
--    replace a = case (a, s) of
--      (old@(ClassAssign o1 _ _ _), new@(ClassAssign o2 _ _ _)) -> if o1 == o2 then new else old
--      (old, _) -> old
    
getClass :: Analyzer' (Maybe Stmt)
getClass = do
  local' <- gets local
  case local' of
    LocalClassScope c _ _-> return $ Just c
    _ -> return Nothing

getGen :: Analyzer' [String]
getGen = do
  class' <- getClass
  case class' of
    Just(ClassExpr _ _ g _) -> return g
    Just(NativeClass _ _ _ g _) -> return g
    Nothing -> return []
    


addFnScope :: Scope -> Analyzer' ()
addFnScope scope = modify (\s -> s {local = update (local s) scope })
  where
    update (LocalFunc f s) x = LocalFunc f (x : s)
    update (LocalClassScope c f s) x = LocalClassScope c f (x:s)
    update x _ = error $ show x

removeFnScope :: Analyzer' ()
removeFnScope = modify (\s -> s {local = update (local s)})
  where
    update (LocalFunc f (x : xs)) = LocalFunc f xs
    update (LocalClassScope c f (x : xs)) = LocalClassScope c f xs
    update x = error $ show x

updateFnScopes :: [Scope] -> Analyzer' ()
updateFnScopes scopes = modify (\s -> s {local = update (local s)})
  where
    update (LocalFunc f _ ) = LocalFunc f scopes
    update (LocalClassScope c f _) = LocalClassScope c f scopes
    update x = error $ show x

updateClassInGlobal :: Stmt -> Analyzer' ()
updateClassInGlobal t@(NativeClass o _ _ _ _) = do
  global' <- map update <$> gets global
  modify (\s -> s {global = global'})
  where
    update t2@(NativeClass o' _ _ _ _) =
      if o' == o then t else t2
    update x = x
updateClassInGlobal t@(ClassExpr o _ _ _) = do
 global' <- map update <$> gets global
 modify (\s -> s {global = global'})
 where
   update t2@(ClassExpr o' _ _ _) =
     if o' == o then t else t2
   update x = x

manyFixGenInFuncArgs gen = map (fixGenInFuncArgs gen)

fixGenInFuncArgs x (Function o n t args b) = Function o n t okArgs b
  where
    okArgs = map update args
    update (FunArg (VGen "T") n') = FunArg x n'
    update x = x
fixGenInFuncArgs _ f = f

data StorageCache
  = EmptyCache
  | ScopeCache [FunctionStmt]
  | ClassScopeCache [ClassStmt]
  | TypeCache [VarType]
  | InjectBefore [FunctionStmt]
  deriving (Show)

--data LocalScope = LSGlobal
instance Exception AnalyzerException

type AExprRes = (VarType, [FunctionStmt], AExpr)

type AExprAnalyzer = AExpr -> Analyzer' AExprRes

type BExprAnalyzer = BExpr -> Analyzer' BExpr

type FnStmtAnalyzer = FunctionStmt -> Analyzer' [FunctionStmt]

type ClassStmtAnalyzer = String -> ClassStmt -> Analyzer' ClassStmt

type RawAssign = (Int, [String], VarType, AExpr)

type RawAssignConst a = (Int -> [String] -> VarType -> AExpr -> a)

type RawWhile b = (BExpr, [b])

type RawWhileConst a b = (BExpr -> [b] -> a)

type RawFunction = (Int, String, VarType, [FunArg], [FunctionStmt])

type RawFunctionConst a = Int -> String -> VarType -> [FunArg] -> [FunctionStmt] -> a

trd (_, _, c) = c
