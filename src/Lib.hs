module Lib
    ( someFunc
    , run
    ) where

import Control.Monad.State (StateT, liftIO, get, runStateT, evalStateT, gets)
import Control.Monad (forM_, join)

newtype MyState = MyState {
  getMyState :: [String]
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
