module Wobsurv.Util.MasterThread where

import BasePrelude
import Control.Monad.Trans.Reader
import qualified STMContainers.Set as Set
import qualified Wobsurv.Util.PartialHandler as H

-- |
-- A monad, which adds a functionality of forking of slave threads,
-- while binding them to their master thread in such a manner
-- that when the master is killed, they get killed too.
-- It also rethrows exceptions from the slave threads in the main thread,
-- so they don't get lost.
type MasterThread =
  ReaderT Context IO

type Context =
  (Set.Set ThreadId)

type MT = 
  MasterThread

run :: MT a -> IO a
run mt =
  do
    context <- atomically $ Set.new
    catch (runReaderT mt context) $ \(e :: SomeException) -> do
      -- Kill all slaves
      traverse_ killThread =<< do
        atomically $ Set.foldM (\l -> return . (: l)) [] context
      -- Wait for all slaves to die
      atomically $ Set.null context >>= bool retry (return ())
      throwIO e

forkFinally :: MT () -> IO () -> MT ThreadId
forkFinally main finalizer =
  ReaderT $ \context -> do
    thread <- myThreadId
    slaveContext <- atomically $ Set.new
    let
      onDeath r =
        do
          -- Finalization and rethrowing of exceptions into the master thread:
          do
            r' <- try $ finalizer
            forM_ (left r <|> left r') $ 
              H.toTotal $ H.onThreadKilled (return ()) <> H.rethrowTo thread
          -- Context management:
          do
            traverse_ killThread =<< do
              atomically $ Set.foldM (\l -> return . (: l)) [] slaveContext
            slaveThread <- myThreadId
            -- Ensures that it waits for all slaves to die 
            -- before informing the master that it died itself.
            -- And so on recursively.
            atomically $ do
              Set.null slaveContext >>= \case
                True -> Set.delete slaveThread context
                False -> retry
        where
          left = either Just (const Nothing)
    slaveThread <- BasePrelude.forkFinally (runReaderT main slaveContext) onDeath
    atomically $ Set.insert slaveThread context
    return slaveThread

    







