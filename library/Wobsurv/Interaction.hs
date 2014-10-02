module Wobsurv.Interaction where

import BasePrelude hiding (bracket, for, yield, log)
import Pipes
import Pipes.Safe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import qualified Wobsurv.Util.PipesAttoparsec as PipesAttoparsec
import qualified Wobsurv.Util.HTTP.Parser as Parser
import qualified Wobsurv.Util.HTTP.Model as Protocol
import qualified Wobsurv.Util.HTTP.URLEncoding as URLEncoding
import qualified Wobsurv.Util.Mustache.Renderer as TemplatesRenderer
import qualified Wobsurv.TemplateModels.NotFound as NotFound
import qualified Wobsurv.Response as Response
import qualified Pipes.Parse
import qualified Data.ByteString
import qualified Data.HashMap.Strict
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Filesystem.Path.CurrentOS as Path
import qualified Filesystem
import qualified Network.HTTP.Types.URI


type BS = 
  Data.ByteString.ByteString

type Path =
  Path.FilePath

type Text =
  Data.Text.Text

data Settings =
  Settings {
    logger :: Summary -> IO (),
    contentDir :: Path,
    mimeMappings :: Data.HashMap.Strict.HashMap Text BS,
    -- | In microseconds.
    keepAliveTimeout :: Maybe Word,
    templatesRenderer :: TemplatesRenderer.Renderer
  }

type Summary =
  (Maybe (Protocol.Method, Protocol.RelativeURI), Protocol.Status)

-- | 
-- A producing parser.
-- Consumes the input and generates the output.
type Interaction r = 
  Pipes.Parse.Parser BS (ReaderT Settings (Producer BS (SafeT IO))) r

run :: Interaction r -> Settings -> (Producer BS (SafeT IO) () -> Producer BS (SafeT IO) r)
run server settings = 
  flip runReaderT settings . evalStateT server . hoist (lift . lift)

-- |
-- Returns the next keep-alive-timeout, 
-- if it's nothing, then the connection should be closed.
interaction :: Interaction (Maybe Word)
interaction =
  do
    settings <- lift $ ask
    PipesAttoparsec.liftParserWithLimit 2048 Parser.head >>= \case
      Right (method, uri, version, headers) -> do
        let
          path = 
            contentDir settings <> uriPath
          uriPath =
            maybe mempty URLEncoding.toFilePath $ case uri of (p, _, _) -> p
        case method of
          Left Protocol.Get ->
            (liftIO . Filesystem.isFile) path >>= \case
              False -> do
                (liftIO . Filesystem.isDirectory) path >>= \case
                  False -> do
                    log (Just (method, uri), Protocol.notFound)
                    liftResponse (Response.notFound uri)
                    return Nothing
                  True -> do
                    log (Just (method, uri), Protocol.ok)
                    liftResponse (Response.okIndex uriPath path (keepAliveTimeoutMicros settings))
                    return (keepAliveTimeout settings)
              True -> do
                log (Just (method, uri), Protocol.ok)
                liftResponse (Response.okFile path (keepAliveTimeoutMicros settings) (mimeMappings settings))
                return (keepAliveTimeout settings)
          _ -> do
            log (Nothing, Protocol.notImplemented)
            liftResponse Response.notImplemented
            return Nothing
      Left PipesAttoparsec.ConsumedTooMuch -> do
        log (Nothing, Protocol.entityTooLarge)
        liftResponse Response.entityTooLarge
        return Nothing
      _ -> do
        log (Nothing, Protocol.badRequest)
        liftResponse Response.badRequest
        return Nothing
  where
    keepAliveTimeoutMicros = 
      fmap (`div` 1000000) . keepAliveTimeout

liftResponse :: Response.Response a -> Interaction a
liftResponse response =
  do
    settings <- lift $ ask
    lift $ lift $ Response.runInProducer response (templatesRenderer settings)

log :: Summary -> Interaction ()
log request =
  do
    settings <- lift $ ask
    liftIO $ (logger settings) request
