module Data.State (nullState, nextGensymCounter) where

import Control.Monad.Except
import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import Types

nullState :: IO StateRef
nullState = newIORef (State {gensymCounter = 0})

nextGensymCounter :: StateRef -> IO Integer
nextGensymCounter stateRef = do
  state <- readIORef stateRef
  let counter = getCounter state
  writeIORef stateRef State {gensymCounter = counter + 1}
  return (counter + 1)
  where
    getCounter (State gensymCounter) = gensymCounter