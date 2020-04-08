module Compiler.Analyzer.Browser where

import           AST
import           Compiler.Analyzer.Type
import           Control.Monad.State    (get)
import qualified Data.List              as L
import           Data.Maybe             (fromJust, listToMaybe)
import           Debug.Trace



find' :: [String] -> Analyzer' [ScopeField]
find' name = do
  storage <- get
  let scopes' = scopes storage
  return $
    case name of
      [n]         -> searchNameInScopes n scopes'
      ["", n]         -> searchNameInScopes n scopes'
      ["this", n] -> searchInThisScope n scopes'
      ["g", n]    -> searchInScope n (last scopes')
      ["global", n]    -> searchInScope n (last scopes')
      ["", "this", n] -> searchInThisScope n scopes'
      [sName, n] -> searchInScopeWithName n sName scopes'
      _           -> []

findInClass :: String -> String -> Analyzer' [ScopeField]
findInClass cName name = do
  c <- listToMaybe <$> find' [cName]
  return $
    case c of
      Just (SClass o _ _ gen s) -> searchInScope name s
      Nothing                 -> []

searchInThisScope :: String -> Scopes -> [ScopeField]
searchInThisScope name = searchInScopeWithName name "this"


searchInScopeWithName :: String -> String -> Scopes -> [ScopeField]
-- TODO add error when given scope is not exist :<>
searchInScopeWithName name scopeName = searchInScope name . fromJust . L.find (\(Scope n _) -> n == scopeName)

searchNameInScopes :: String -> Scopes -> [ScopeField]
searchNameInScopes name = concatMap (searchInScope name)

searchInScope :: String -> Scope -> [ScopeField]
searchInScope name (Scope _ scope) = filter cond scope
  where
    cond (SFunction o n _ t a) = n == name
    cond (SVar o n _ t _)        = n == name
    cond (SClass o n _ g s)    = n == name
    cond _                   = False

-- | PRIVATE
getScope :: String -> Scopes -> Maybe Scope
getScope name = L.find (\(Scope n _) -> name == n)

----    MATCH
maybeArgsMatch :: Maybe [AExpr] -> [VarType] -> [ScopeField] -> Bool
maybeArgsMatch (Just args) gen s = any (argsMatch prepareArgs gen) s
  where
    prepareArgs = map aExprExtractType args
maybeArgsMatch Nothing _ _ = True
--
argsMatch :: [VarType] -> [VarType] -> ScopeField -> Bool
argsMatch t gen (SFunction _ _ _ _ args) = argsMatch' t (fixArgs gen args)
argsMatch t gen (SClass _ n _ _ (Scope _ s)) =  any match s
  where
    match (SFunction o n' _ _ args) = n' == n && argsMatch' t (fixArgs gen args)
    match _ = False

argsMatch' :: [VarType] -> [FunArg] -> Bool
argsMatch' t args = (length t == length args) && all (\(w, FunArg ot _) -> w == ot) (zip t args)
--
fixArgs :: [VarType] -> [FunArg] -> [FunArg]
fixArgs [] args  = args
fixArgs gen args = map (\(FunArg t n) -> FunArg (fixType gen t) n) args

-- | fixType - replace generic type with exact type 
--   e.g
--   [VGenPair T VInt] -> VGen T -> VInt
fixType :: [VarType] -> VarType -> VarType
fixType gen (VGen tName) = makeOutput . filter fGen $ gen
  where
    makeOutput (VGenPair _ t:_) = t
    makeOutput _ = error $ "Cannont find type for template: " ++ tName ++ " | | " ++ show gen
    fGen (VGenPair tName' _) = tName' == tName
    fGen _ = False
    
fixType gen t = t

aExprExtractType :: AExpr -> VarType
aExprExtractType (TypedVar _ _ _ (Just more)) = aExprExtractType more
aExprExtractType (TypedVar _ t _ Nothing)     = fixClassGen t
aExprExtractType IntConst {}                  = VInt
aExprExtractType FloatConst {}                = VFloat
aExprExtractType StringVal {}                 = VString



-- TODO remember that pointer is always selected to false as default value 
fixClassGen (VClass n gens p) = VClass n (map nGen gens) False
  where
    nGen (VGenPair _ t) = t
    nGen x = x
fixClassGen x = x
