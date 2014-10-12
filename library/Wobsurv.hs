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


data Settings =
  Settings {
    logging :: Bool,
    port :: Word,
    connectionsLimit :: Word,
    templatesDir :: FilePath,
    contentDir :: FilePath,
    mimeMappings :: HashMap Text ByteString
  }
  deriving (Show)

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

