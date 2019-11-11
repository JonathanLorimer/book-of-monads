module CH08 where

import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.List (lookup)

addName :: TVar Integer
        -> TVar [(Integer, String)]
        -> String
        -> STM ()
addName counter names name = do
  i <- readTVar counter
  ns <- readTVar names
  case lookup i ns of
    Nothing -> do
      modifyTVar names ((i, name) :)
      writeTVar counter (i + 1)
    Just _ -> return ()


