module Compiler.Analyzer.Type where

import AST
import Control.Exception
import Control.Monad.State (State, get, gets, modify, put)

--import Compiler.Translator.Type
import Control.Monad.Writer (WriterT)
import Data.List
import Data.Map (Map)
import Data.Maybe (fromMaybe)
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
    { imports :: [FileScopes]
    , scopes :: Scopes
    , rType :: VarType
    , cName :: String
    , fName :: String
    }
  deriving (Show)

type Scopes = [Scope]

data Scope =
  Scope String [ScopeField]
  deriving (Show)

data FileScopes =
  FileScope String Scopes
  deriving (Show)

data ScopeField
  -- | SFunction ord name path type args
  = SFunction Int String (Maybe String) VarType [FunArg]
  -- | SVar ord name path type
  | SVar Int String (Maybe String) VarType
  -- | SClass ord name path gen scope
  | SClass Int String (Maybe String) [String] Scope
  -- | SGen name
  | SGen String
  deriving (Show)

emptyStorage = Storage [] [] VBlank "" ""

addArgsScope :: [FunArg] -> Analyzer' ()
addArgsScope args = modify (\s -> s {scopes = Scope "args" argsToFields : scopes s})
  where
    argsToFields = map (\(FunArg t n) -> SVar (-1) n Nothing t) args

addScope :: String -> Analyzer' ()
addScope sName = modify (\s -> s {scopes = Scope sName [] : scopes s})

removeScope :: Analyzer' Scope
removeScope = do
  s <- gets scopes
  remove s
  where
    remove :: Scopes -> Analyzer' Scope
    remove s
      | null s = error "Storage doesn't contain any scope"
      | otherwise = do
        modify (\storage -> storage {scopes = tail s})
        return $ head s

addField :: ScopeField -> Analyzer' ()
addField field = do
  s <- gets scopes
  modify (\storage -> storage {scopes = add (head s) : tail s})
  where
    add s@(Scope n f)
      | isInScope field s = updateField field s
      | otherwise = Scope n (field : f)

saveFile :: String -> Analyzer' ()
saveFile n = modify (\s -> s {imports = FileScope n (scopes s) : imports s})

loadFiles :: [String] -> Analyzer' [ScopeField]
loadFiles names = do
  i <- gets imports
  trace (show i) $ return ()
  concatMap (\(FileScope _ [Scope _ x]) -> x) . filter (\(FileScope n _) -> n `elem` names) <$> gets imports

setType :: VarType -> Analyzer' ()
setType t = modify (\s -> s {rType = t})

setClassName :: String -> Analyzer' ()
setClassName n = modify (\s -> s {cName = n})

setFunName :: String -> Analyzer' ()
setFunName n = modify (\s -> s {fName = n})

getClassScope :: Analyzer' (Maybe Scope)
getClassScope = do
--  cl <- gets cName
  let cl = "this"
  find (\(Scope n _) -> n == cl) <$> gets scopes

getClassGens :: Analyzer' [String]
getClassGens = do
  s <- getClassScope
  return . map (\(SGen s) -> s) . filter genFilter . fromMaybe [] $ s >>= (\(Scope _ f) -> Just f)
  where
    genFilter SGen {} = True
    genFilter _ = False

-- | private
updateField :: ScopeField -> Scope -> Scope
updateField f (Scope n fs) = Scope n $ map (updater f) fs
  where
    updater a@(SFunction o1 _ _ _ _) b@(SFunction o2 _ _ _ _) = match o1 o2 a b
    updater a@(SVar o1 _ _ _) b@(SVar o2 _ _ _) = match o1 o2 a b
    updater a@(SClass o1 _ _ _ _) b@(SClass o2 _ _ _ _) = match o1 o2 a b
    updater _ v = v
    match o1 o2 a b =
      if o1 == o2
        then a
        else b

-- | private
isInScope :: ScopeField -> Scope -> Bool
isInScope field (Scope _ f) = any (cond field) f
  where
    cond (SFunction o1 _ _ _ _) (SFunction o2 _ _ _ _) = o1 == o2
    cond (SVar o1 _ _ _) (SVar o2 _ _ _) = o1 == o2
    cond (SClass o1 _ _ _ _) (SClass o2 _ _ _ _) = o1 == o2
    cond _ _ = False

--type Scope = [FunctionStmt]
--
--data LocalStorage
--  = LocalClassScope Stmt (Maybe ClassStmt) [Scope]
--  | LocalFunc Stmt [Scope]
--  | LocalEmpty
--  deriving (Show)
--
--updateFile :: Imported -> Analyzer' ()
--updateFile f@(IFile n _) = modify (\storage -> storage {files = map update (files storage)})
--  where
--    update f2@(IFile n2 _) =
--      if n == n2
--        then f
--        else f2
--add
--- OLD
--
--getType :: LocalStorage -> VarType
--getType (LocalClassScope (ClassExpr _ n _ _) Nothing _) = VClass n []
--getType (LocalClassScope _ (Just (Method _ _ t _ _)) _) = t
--getType (LocalFunc (Function _ _ t _ _) _) = t
--getType t = throw $ UnsupportedTypeException $ show t
--
--setType :: VarType -> LocalStorage -> LocalStorage
--setType t (LocalClassScope c (Just (Method o n _ a b)) sc) = LocalClassScope c (Just (Method o n t a b)) sc
--setType t (LocalFunc (Function o n _ a b) x) = LocalFunc (Function o n t a b) x
--setType t l = throw $ UnsupportedTypeException $ show l
--
--
--
--setStmt :: Stmt -> Analyzer' ()
--setStmt s =
--  case s of
--    class'@ClassExpr {} -> modify (\s -> s {local = LocalClassScope class' Nothing []})
--    class'@NativeClass {} -> modify (\s -> s {local = LocalClassScope class' Nothing []})
--    func@Function {} -> modify (\s -> s {local = LocalFunc func []})
--    _ -> throw $ UnsupportedTypeException $ "setStmt | " ++ show s
--
----    _ -> throw $ NotAClass $ "Function setClass need class as parameter. Indstead of that it got: " ++ show s
--setMethod :: ClassStmt -> Analyzer' ()
--setMethod s =
--  case s of
--    method@Method {} -> do
--      state <- get
--      let (LocalClassScope class' _ _) = local state
--      put $ state {local = LocalClassScope class' (Just method) []}
--    _ -> throw $ NotAMethod $ "Function setMethod need a method as parameter. Instead of that it got: " ++ show s
--
--replaceInFunc :: FunctionStmt -> Analyzer' ()
--replaceInFunc s = do
--  scopes <- unwrap <$> gets local
--  updateFnScopes . map (map tryReplace) $ scopes
--  where
--    unwrap (LocalFunc _ x)         = x
--    unwrap (LocalClassScope _ _ x) = x
--    unwrap _                       = error "Accept only LocalFunc wrapper"
--    tryReplace a =
--      case (a, s) of
--        (l@(AssignFn o1 _ _ _), r@(AssignFn o2 _ _ _)) ->
--          if o1 == o2
--            then r
--            else l
--        (l, _) -> l
--
----replaceInClass :: ClassStmt -> Analyzer'()
----replaceInClass s = do
----  class' <- getClass
----  let (ClassExpr o n g b) = class'
----  let nBody = map replace b
----  return ()
----  where
----    replace a = case (a, s) of
----      (old@(ClassAssign o1 _ _ _), new@(ClassAssign o2 _ _ _)) -> if o1 == o2 then new else old
----      (old, _) -> old
--getClass :: Analyzer' (Maybe Stmt)
--getClass = do
--  local' <- gets local
--  case local' of
--    LocalClassScope c _ _ -> return $ Just c
--    _                     -> return Nothing
--
--getGen :: Analyzer' [String]
--getGen = do
--  class' <- getClass
--  case class' of
--    Just (ClassExpr _ _ g _)     -> return g
--    Just (NativeClass _ _ _ g _) -> return g
--    Nothing                      -> return []
--
--addFnScope :: Scope -> Analyzer' ()
--addFnScope scope = modify (\s -> s {local = update (local s) scope})
--  where
--    update (LocalFunc f s) x         = LocalFunc f (x : s)
--    update (LocalClassScope c f s) x = LocalClassScope c f (x : s)
--    update x _                       = error $ show x
--
--removeFnScope :: Analyzer' ()
--removeFnScope = modify (\s -> s {local = update (local s)})
--  where
--    update (LocalFunc f (x:xs))         = LocalFunc f xs
--    update (LocalClassScope c f (x:xs)) = LocalClassScope c f xs
--    update x                            = error $ show x
--
--updateFnScopes :: [Scope] -> Analyzer' ()
--updateFnScopes scopes = modify (\s -> s {local = update (local s)})
--  where
--    update (LocalFunc f _)         = LocalFunc f scopes
--    update (LocalClassScope c f _) = LocalClassScope c f scopes
--    update x                       = error $ show x
--
--updateClassInGlobal :: Stmt -> Analyzer' ()
--updateClassInGlobal t@(NativeClass o _ _ _ _) = do
--  global' <- map update <$> gets global
--  modify (\s -> s {global = global'})
--  where
--    update t2@(NativeClass o' _ _ _ _) =
--      if o' == o
--        then t
--        else t2
--    update x = x
--updateClassInGlobal t@(ClassExpr o _ _ _) = do
--  global' <- map update <$> gets global
--  modify (\s -> s {global = global'})
--  where
--    update t2@(ClassExpr o' _ _ _) =
--      if o' == o
--        then t
--        else t2
--    update x = x
--
--manyFixGenInFuncArgs gen = map (fixGenInFuncArgs gen)
--
--fixGenInFuncArgs x (Function o n t args b) = Function o n t okArgs b
--  where
--    okArgs = map update args
--    update (FunArg (VGen "T") n') = FunArg x n'
--    update x                      = x
--fixGenInFuncArgs _ f = f
--
--data StorageCache
--  = EmptyCache
--  | ScopeCache [FunctionStmt]
--  | ClassScopeCache [ClassStmt]
--  | TypeCache [VarType]
--  | InjectBefore [FunctionStmt]
--  deriving (Show)
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

--[ FileScope
--    "Core"
--    [ Scope
--        "args"
--        [ SClass
--            2786
--            "ThisTest"
--            Nothing
--            ["T"]
--            (Scope "args" [SVar 3007 "a" Nothing (VClass "T" []), SVar (-1) "val" Nothing (VClass "T" [])])
--        ]
--    , Scope
--        "args"
--        [ SVar 2936 "b" Nothing VInt
--        , SVar 2919 "a" Nothing (VClass "T" [])
--        , SVar (-1) "a" Nothing (VClass "T" [])
--        , SVar (-1) "b" Nothing VInt
--        ]
--    , Scope
--        "args"
--        [SVar 2872 "b" Nothing VInt, SVar 2855 "a" Nothing (VClass "T" []), SVar (-1) "a" Nothing (VClass "T" [])]
--    , Scope "this" []
--    , Scope "args" [SVar 2645 "fragmentation" Nothing VFloat]
--    , Scope
--        "global"
--        [ SVar 2520 "Serial" (Just "Serial") (VClass "HardwareSerial" [])
--        , SFunction 155 "pinMode" (Just "") VVoid [FunArg VInt "pin", FunArg VInt "mode"]
--        , SFunction 201 "digitalWrite" (Just "") VVoid [FunArg VInt "pin", FunArg VInt "state"]
--        , SFunction 253 "sleep" (Just "delay") VVoid [FunArg VInt "time"]
--        , SFunction 293 "digitalRead" (Just "") VInt [FunArg VInt "port"]
--        , SFunction 332 "analogRead" (Just "") VInt [FunArg VInt "port"]
--        , SFunction 370 "analogReference" (Just "") VVoid [FunArg VInt "mode"]
--        , SFunction 414 "analogWrite" (Just "") VVoid [FunArg VInt "port", FunArg VInt "value"]
--        , SFunction 467 "millis" (Just "") VInt []
--        , SFunction 491 "micros" (Just "") VInt []
--        , SFunction 515 "delay" (Just "") VVoid [FunArg VInt "ms"]
--        , SFunction 547 "delayMicroseconds" (Just "") VVoid [FunArg VInt "us"]
--        , SFunction 592 "pulseIn" (Just "") VInt [FunArg VInt "pin", FunArg VInt "state", FunArg VInt "timeout"]
--        , SFunction 652 "pulseInLong" (Just "") VInt [FunArg VInt "pin", FunArg VInt "state", FunArg VInt "timeout"]
--        , SFunction
--            717
--            "shiftOut"
--            (Just "")
--            VVoid
--            [FunArg VInt "dataPin", FunArg VInt "clockPin", FunArg VInt "bitOrder", FunArg VInt "val"]
--        , SFunction 797 "shiftIn" (Just "") VInt [FunArg VInt "dataPin", FunArg VInt "clockPin", FunArg VInt "bitOrder"]
--        , SFunction 866 "attachInterrupt" (Just "") VVoid [FunArg VInt "id", FunArg VVoid "fn", FunArg VInt "mode"]
--        , SFunction 929 "detachInterrupt" (Just "") VVoid [FunArg VInt "id"]
--        , SFunction 983 "HIGH" (Just "") VInt []
--        , SFunction 1005 "LOW" (Just "") VInt []
--        , SFunction 1026 "INPUT" (Just "") VInt []
--        , SFunction 1049 "OUTPUT" (Just "") VInt []
--        , SFunction 1073 "INPUT_PULLUP" (Just "") VInt []
--        , SFunction 1104 "PI" (Just "") VAuto []
--        , SFunction 1117 "HALF_PI" (Just "") VAuto []
--        , SFunction 1135 "TWO_PI" (Just "") VAuto []
--        , SFunction 1152 "DEG_TO_RAD" (Just "") VAuto []
--        , SFunction 1173 "RAD_TO_DEG" (Just "") VAuto []
--        , SFunction 1194 "EULER" (Just "") VAuto []
--        , SFunction 1211 "SERIAL" (Just "") VAuto []
--        , SFunction 1228 "DISPLAY" (Just "") VAuto []
--        , SFunction 1247 "LSBFIRST" (Just "") VAuto []
--        , SFunction 1266 "MSBFIRST" (Just "") VAuto []
--        , SFunction 1286 "CHANGE" (Just "") VAuto []
--        , SFunction 1303 "FALLING" (Just "") VAuto []
--        , SFunction 1321 "RISING" (Just "") VAuto []
--        , SFunction 1350 "min" (Just "") VAuto [FunArg VAuto "a", FunArg VAuto "b"]
--        , SFunction 1369 "max" (Just "") VAuto [FunArg VAuto "a", FunArg VAuto "b"]
--        , SFunction 1388 "abs" (Just "") VAuto [FunArg VAuto "x"]
--        , SFunction 1404 "constrain" (Just "") VAuto [FunArg VAuto "atm", FunArg VAuto "low", FunArg VAuto "high"]
--        , SFunction 1439 "round" (Just "") VAuto [FunArg VAuto "x"]
--        , SFunction 1457 "radians" (Just "") VAuto [FunArg VAuto "deg"]
--        , SFunction 1479 "degrees" (Just "") VAuto [FunArg VAuto "rad"]
--        , SFunction 1501 "sq" (Just "") VAuto [FunArg VAuto "x"]
--        , SFunction 1517 "interrupts" (Just "") VVoid []
--        , SFunction 1546 "noInterrupts" (Just "") VVoid []
--        , SFunction 1578 "clockCyclesPerMicrosecond" (Just "") VInt []
--        , SFunction 1621 "clockCyclesToMicroseconds" (Just "") VInt [FunArg VInt "x"]
--        , SFunction 1671 "microsecondsToClockCycles" (Just "") VInt [FunArg VInt "x"]
--        , SFunction 1722 "lowByte" (Just "") VInt [FunArg VAuto "w"]
--        , SFunction 1749 "highByte" (Just "") VInt [FunArg VAuto "w"]
--        , SFunction 1778 "bitRead" (Just "") VInt [FunArg VAuto "value", FunArg VAuto "bit"]
--        , SFunction 1814 "bitSet" (Just "") VInt [FunArg VAuto "value", FunArg VAuto "bit"]
--        , SFunction 1849 "bitClear" (Just "") VInt [FunArg VAuto "value", FunArg VAuto "bit"]
--        , SFunction 1886 "bitWrite" (Just "") VInt [FunArg VAuto "value", FunArg VAuto "bit", FunArg VAuto "bitValue"]
--        , SFunction 1944 "F" (Just "F") VString [FunArg VString "s"]
--        , SClass
--            1978
--            "HardwareSerial"
--            (Just "HardwareSerial")
--            []
--            (Scope
--               "this"
--               [ SFunction 2493 "print" Nothing VInt [FunArg VInt "val"]
--               , SFunction 2461 "print" Nothing VInt [FunArg VFloat "val"]
--               , SFunction 2428 "print" Nothing VInt [FunArg VString "val"]
--               , SFunction 2396 "println" Nothing VInt [FunArg VInt "val"]
--               , SFunction 2362 "println" Nothing VInt [FunArg VFloat "val"]
--               , SFunction 2327 "println" Nothing VInt [FunArg VString "val"]
--               , SFunction 2283 "write" Nothing VInt [FunArg VString "val"]
--               , SFunction 2242 "write" Nothing VInt [FunArg VInt "val"]
--               , SFunction 2220 "flush" Nothing VVoid []
--               , SFunction 2187 "availableForWrite" Nothing VInt []
--               , SFunction 2167 "read" Nothing VInt []
--               , SFunction 2147 "peek" Nothing VInt []
--               , SFunction 2122 "available" Nothing VInt []
--               , SFunction 2102 "end" Nothing VVoid []
--               , SFunction 2062 "begin" Nothing VVoid [FunArg VInt "baud", FunArg VInt "x"]
--               , SFunction 2030 "begin" Nothing VVoid [FunArg VInt "baud"]
--               ])
--        , SFunction 2564 "getFragmentation" (Just "getFragmentation") VFloat []
--        , SFunction 2617 "printMemStats" Nothing VVoid []
--        , SClass 2786 "ThisTest" Nothing ["T"] (Scope "ThisTest" [])
--        , SClass
--            25
--            "List"
--            (Just "ArrayList")
--            ["T"]
--            (Scope
--               "this"
--               [ SFunction 204 "size" Nothing VInt []
--               , SFunction 182 "clear" Nothing VVoid []
--               , SFunction 154 "add" Nothing VVoid [FunArg (VClass "T" []) "elem"]
--               , SFunction 126 "get" Nothing (VClass "T" []) [FunArg VInt "index"]
--               , SFunction 78 "List" Nothing VAuto [FunArg VAuto "arr", FunArg VInt "size", FunArg VInt "maxSize"]
--               , SFunction 65 "List" Nothing VAuto []
--               ])
--        ]
--    ]
--, FileScope
--    "CoreNativeList"
--    [ Scope
--        "global"
--        [ SClass
--            25
--            "List"
--            (Just "ArrayList")
--            ["T"]
--            (Scope
--               "this"
--               [ SFunction 204 "size" Nothing VInt []
--               , SFunction 182 "clear" Nothing VVoid []
--               , SFunction 154 "add" Nothing VVoid [FunArg (VClass "T" []) "elem"]
--               , SFunction 126 "get" Nothing (VClass "T" []) [FunArg VInt "index"]
--               , SFunction 78 "List" Nothing VAuto [FunArg VAuto "arr", FunArg VInt "size", FunArg VInt "maxSize"]
--               , SFunction 65 "List" Nothing VAuto []
--               ])
--        ]
--    ]
--]