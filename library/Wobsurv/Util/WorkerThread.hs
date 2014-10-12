module Wobsurv.Util.WorkerThread where

import BasePrelude hiding (fork)
import Wobsurv.Util.MasterThread
import Control.Concurrent.STM.TBQueue
import Control.Monad.Trans.Class


type WorkerThread =
  TBQueue (MT ())

new :: Int -> MT WorkerThread
new size =
  do
    queue <- lift $ atomically $ newTBQueue size
    fork $ do
      forever $ join $ lift $ atomically $ readTBQueue queue
    return queue

schedule :: MT () -> WorkerThread -> MT ()
schedule task queue =
  lift $ atomically $ writeTBQueue queue task

