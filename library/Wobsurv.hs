module Wobsurv where

import BasePrelude
import Control.Monad.Trans.Class
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Filesystem.Path (FilePath)
import qualified Wobsurv.Util.OpenServer.ConnectionsManager as OpenServer.ConnectionsManager
import qualified Wobsurv.Util.OpenServer.Connection as OpenServer.Connection
import qualified Wobsurv.Util.Mustache.Renderer as Mustache.Renderer
import qualified Wobsurv.Util.MasterThread as MasterThread
import qualified Wobsurv.Util.WorkerThread as WorkerThread
import qualified Wobsurv.Interaction
import qualified Wobsurv.Response
import qualified Wobsurv.Logging
import qualified Data.ByteString as ByteString
import qualified Filesystem
import qualified Filesystem.Path.CurrentOS as FilePath


-- |
-- Settings for running the server.
data Settings =
  Settings {
    -- |
    -- Whether to log requests in the 'stdout'.
    logging :: Bool,
    -- |
    -- A port to listen on.
    port :: Word,
    -- | 
    -- A maximum amount of clients.
    -- When this amount is reached the server rejects all the further connections 
    -- with a "Service Unavailable" status.
    connectionsLimit :: Word,
    -- |
    -- A path to the directory containing template files for server responses.
    templatesDir :: FilePath,
    -- |
    -- A directory, the contents of which should be served.
    contentDir :: FilePath,
    -- |
    -- MIME content-type mappings.
    mimeMappings :: HashMap Text ByteString
  }
  deriving (Show)

-- |
-- Run the server with the provided settings.
-- 
-- This operation is blocking. 
-- If you need to be able to stop the server run it in a separate thread,
-- killing that thread will stop the server and 
-- properly release all the resources acquired by the server.
serve :: Settings -> IO ()
serve settings =
  MasterThread.run $ do
    renderer <- lift $ Mustache.Renderer.new (templatesDir settings)
    printerThread <- WorkerThread.new 1000
    let
      logger =
        if logging settings
          then 
            \m -> MasterThread.runWithoutForking $ 
              WorkerThread.schedule (lift $ Wobsurv.Logging.log m) printerThread
          else 
            const $ return ()
      allowedConnectionHandler =
        lift . OpenServer.Connection.session timeout interactor
        where
          interactor =
            Wobsurv.Interaction.run Wobsurv.Interaction.interaction $
              Wobsurv.Interaction.Settings 
                (logger)
                (contentDir settings)
                (mimeMappings settings)
                (Nothing)
                (renderer)
      disallowedConnectionHandler =
        lift . OpenServer.Connection.rejection timeout rejector
        where
          rejector =
            Wobsurv.Response.runInProducer Wobsurv.Response.serviceUnavailable renderer
    OpenServer.ConnectionsManager.listen $
      OpenServer.ConnectionsManager.Settings 
        (port settings) 
        (connectionsLimit settings)
        allowedConnectionHandler
        disallowedConnectionHandler
  where
    timeout = 1 * 10 ^ 6

