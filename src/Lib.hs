module Lib
  ( someFunc
  , run
  , myfmap
  , MyMaybe(..)
  , MyBinTree(..)
  ) where

import           Control.Monad       (forM_, join)
import           Control.Monad.State (StateT, evalStateT, get, gets, liftIO,
                                      runStateT)

newtype MyState =
  MyState
    { getMyState :: [String]
    }

someFunc :: StateT MyState IO String
someFunc = do
  unwrapped <- gets getMyState
  forM_ unwrapped (liftIO . print)
  return $ join unwrapped

run :: IO ()
run = do
  res <- evalStateT someFunc (MyState ["Patryk", "zygmunt", "HEHE"])
  print res
  return ()
  
class MyFunctor f where
  myfmap :: (a -> b) -> f a -> f b

data MyMaybe x
  = MyJust x
  | MyNothing deriving (Show)


 
maybeFmap :: (a -> b) -> MyMaybe a -> MyMaybe b 
maybeFmap f (MyJust x) = MyJust (f x)
maybeFmap _ MyNothing = MyNothing


data MyBinTree x = Empty | Node x (MyBinTree x) (MyBinTree x) deriving(Show)

treeFmap  :: (a -> b) -> MyBinTree a -> MyBinTree b
treeFmap _ Empty = Empty
treeFmap f (Node val left right) = Node (f val) (treeFmap f left) (treeFmap f right)


instance MyFunctor MyMaybe where
  myfmap = maybeFmap
  
instance MyFunctor MyBinTree where
  myfmap = treeFmap
  
instance Functor MyMaybe where
  fmap = maybeFmap 
  
instance Applicative MyMaybe where
  pure = MyJust
  (MyJust f) <*> val = fmap f val
  _ <*> _ = MyNothing
  
  
data A = A Int String String String String

--test = (\x -> (\y -> (\z -> z + y + x)))  
--test = do
--  x <- [1,2,3]
--  y <- [1,2,3]
--  return $ x + y
  
