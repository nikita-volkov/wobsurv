module Wobsurv.Util.OpenServer.ConnectionsManager where

import BasePrelude
import qualified Data.Text as Text
import qualified Network.Socket as Network
import qualified Network.Simple.TCP as Network.Simple
import qualified Wobsurv.Util.PartialHandler as Handler


-- |
-- Settings for running the server.
data Settings =
  Settings {
    -- |
    -- A port to listen on.
    port :: Word,
    -- | 
    -- A maximum amount of clients.
    -- When this amount is reached the server rejects all the further connections.
    connectionsLimit :: Word,
    -- |
    -- A connection socket handler, 
    -- which is triggered for connections, 
    -- which do not exceed the limit.
    allowedConnectionHandler :: Network.Socket -> IO (),
    -- |
    -- A connection socket handler, 
    -- which is triggered for connections, 
    -- which do exceed the limit.
    disallowedConnectionHandler :: Network.Socket -> IO ()
  }

listen :: Settings -> IO ()
listen settings =
  Network.Simple.listen Network.Simple.HostAny (show (port settings)) $ \(socket, address) -> do
    availableSlotsVar <- newMVar (connectionsLimit settings)
    let
      acquirer = 
        fst <$> Network.accept socket
      releaser socket = 
        do
          Network.close socket
          modifyMVar_ availableSlotsVar $ return . succ
      handler socket = 
        join $ modifyMVar availableSlotsVar $ \availableSlots -> 
          if availableSlots > 0
            then return (pred availableSlots, allowedConnectionHandler settings socket)
            else return (availableSlots, disallowedConnectionHandler settings socket)
      in acquisitionLoop acquirer releaser handler

acquisitionLoop ::
  -- | A resource acquirer.
  IO a ->
  -- | A resource releaser.
  (a -> IO ()) ->
  -- | A resource handler, which gets run in a subthread.
  (a -> IO ()) ->
  IO ()
acquisitionLoop acquirer releaser handler =
  do
    exceptionHandler <- do
      threadId <- myThreadId
      return $ Handler.toTotal $ Handler.ignoreThreadKilled <> Handler.rethrowTo threadId
    forever $ do
      resource <- acquirer
      forkFinally (handler resource) $ \r -> do
        r' <- try $ releaser resource
        traverse_ exceptionHandler (left r <|> left r')
  where
    left = either Just (const Nothing)
