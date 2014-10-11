module Wobsurv.Util.OpenServer.ConnectionsManager where

import BasePrelude
import Control.Monad.Trans.Control
import Control.Monad.Trans.Class
import qualified Data.Text as Text
import qualified Network.Socket as Network
import qualified Network.Simple.TCP as Network.Simple
import qualified Wobsurv.Util.PartialHandler as Handler
import qualified Wobsurv.Util.MasterThread as MT


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
    allowedConnectionHandler :: Network.Socket -> MT.MT (),
    -- |
    -- A connection socket handler, 
    -- which is triggered for connections, 
    -- which do exceed the limit.
    disallowedConnectionHandler :: Network.Socket -> MT.MT ()
  }

listen :: Settings -> MT.MT ()
listen settings =
  liftBaseWith $ \unlift -> do
    Network.Simple.listen Network.Simple.HostAny (show (port settings)) $ \(socket, address) -> do
      availableSlotsVar <- newMVar (connectionsLimit settings)
      forever $ do
        socket' <- fst <$> Network.accept socket
        let
          handler = 
            liftBaseWith $ \unlift -> do
              void $ join $ modifyMVar availableSlotsVar $ \availableSlots -> 
                if availableSlots > 0
                  then return (pred availableSlots, unlift $ allowedConnectionHandler settings socket')
                  else return (availableSlots, unlift $ disallowedConnectionHandler settings socket')
          releaser = 
            do
              Network.close socket'
              modifyMVar_ availableSlotsVar $ return . succ
        unlift $ MT.forkFinally handler releaser

