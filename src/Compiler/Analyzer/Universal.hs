module Compiler.Analyzer.Universal where

import           AST
import           Compiler.Analyzer.Error
import           Compiler.Analyzer.Type
import           Data.List               (all, find, group)
import           Data.Maybe              (fromJust, fromMaybe)

-- | `replaceGenWithType` replace generic type with strict defined in gen list
--
-- @signature@ replaceGenWithType @gen[VGenPair{}] @genType
replaceGenWithType :: [VarType] -> VarType -> VarType
replaceGenWithType gen (VGen t) = unwrapGen . fromJust . find name $ gen
  where
    name (VGenPair n _) = n == t
    unwrapGen (VGenPair _ t) = t
    unwrapGen x              = x
replaceGenWithType _ t = t

markGen (VClass (VName g) []) gen =
  if g `elem` gen
    then VGen g
    else VClass (VName g) []
markGen x _ = x

addTypeToGens :: [VarType] -> [String] -> [VarType]
addTypeToGens = zipWith add
  where
    add p@VGenPair {} _ = p
    add p g             = VGenPair g p
