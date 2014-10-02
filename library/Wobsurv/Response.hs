module Wobsurv.Response where

import BasePrelude hiding (bracket, for, yield, head)
import Pipes
import Pipes.Safe
import Control.Monad.Trans.Reader
import qualified Wobsurv.Util.HTTP.Renderer as ProtocolRenderer
import qualified Wobsurv.Util.HTTP.Model as Protocol
import qualified Wobsurv.Util.HTTP.URLEncoding as URLEncoding
import qualified Wobsurv.Util.Mustache.Renderer as TemplatesRenderer
import qualified Wobsurv.TemplateModels.NotFound as NotFound
import qualified Wobsurv.TemplateModels.Index as Index
import qualified Pipes.ByteString
import qualified Pipes.Text
import qualified Data.ByteString
import qualified Data.ByteString.Builder
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Lazy
import qualified Data.HashMap.Strict
import qualified Filesystem.Path as Path
import qualified Filesystem.Path.CurrentOS as Path.CurrentOS
import qualified Filesystem.Path.Rules as Path.Rules
import qualified Filesystem


type BS = 
  Data.ByteString.ByteString

type FilePath =
  Path.CurrentOS.FilePath

type Text =
  Data.Text.Text

type LazyText =
  Data.Text.Lazy.Text

type KeepAliveTimeout =
  Word

type MimeMappings =
  Data.HashMap.Strict.HashMap Text BS

type Env =
  TemplatesRenderer.Renderer

type Response r =
  ReaderT Env (Producer BS (SafeT IO)) r

runInProducer :: Response a -> Env -> Producer BS (SafeT IO) a
runInProducer = 
  runReaderT

-- * Responses
-------------------------

serviceUnavailable :: Response ()
serviceUnavailable =
  do
    statusLine Protocol.serviceUnavailable
    connectionHeader False
    contentTypeHeader ("text/html", Just Protocol.UTF8)
    newLine
    template "service-unavailable" ()

badRequest :: Response ()
badRequest =
  do
    statusLine Protocol.badRequest
    connectionHeader False
    contentTypeHeader ("text/html", Just Protocol.UTF8)
    newLine
    template "bad-request" ()

notImplemented :: Response ()
notImplemented =
  do
    statusLine Protocol.notImplemented
    connectionHeader False
    contentTypeHeader ("text/html", Just Protocol.UTF8)
    newLine
    template "not-implemented" ()

entityTooLarge :: Response ()
entityTooLarge =
  do
    statusLine Protocol.entityTooLarge
    connectionHeader False
    contentTypeHeader ("text/html", Just Protocol.UTF8)
    newLine
    template "entity-too-large" ()

notFound :: Protocol.RelativeURI -> Response ()
notFound uri =
  do
    statusLine Protocol.notFound
    connectionHeader False
    contentTypeHeader ("text/html", Just Protocol.UTF8)
    newLine
    template "not-found" $
      NotFound.NotFound {
        NotFound.uri = 
          URLEncoding.toText $ ProtocolRenderer.toByteString $ ProtocolRenderer.relativeURI uri 
      }

okFile :: FilePath -> Maybe KeepAliveTimeout -> MimeMappings -> Response ()
okFile path keepAliveTimeout mimeMappings =
  do
    statusLine Protocol.ok
    case keepAliveTimeout of
      Nothing -> do
        connectionHeader False
      Just v -> do
        connectionHeader True
        keepAliveHeader (v, Nothing)
    forM_ contentType $ \x -> contentTypeHeader (x, Nothing)
    newLine
    file path
    where
      contentType = 
        Path.extension path >>= \e -> Data.HashMap.Strict.lookup e mimeMappings

okIndex :: FilePath -> FilePath -> Maybe KeepAliveTimeout -> Response ()
okIndex uriPath path keepAliveTimeout =
  do
    statusLine Protocol.ok
    case keepAliveTimeout of
      Nothing -> do
        connectionHeader False
      Just v -> do
        connectionHeader True
        keepAliveHeader (v, Nothing)
    contentTypeHeader ("text/html", Just Protocol.UTF8)
    newLine
    contents <- do
      files <- do
        paths <- liftIO $ Filesystem.listDirectory path
        return $ map Path.filename paths
      if publicPath /= "/"
        then return $ ".." : files
        else return files
    template "index" $ Index.Index (pathRepr publicPath) (map pathRepr contents)
    where
      publicPath =
        Path.parent $ "/" <> uriPath <> "./"
      pathRepr =
        fromString . Path.CurrentOS.encodeString


-- * Headers
-------------------------

contentTypeHeader :: Protocol.ContentTypeHeader -> Response ()
contentTypeHeader =
  liftBSBuilder . ProtocolRenderer.contentTypeHeader

connectionHeader :: Protocol.ConnectionHeader -> Response ()
connectionHeader =
  liftBSBuilder . ProtocolRenderer.connectionHeader

keepAliveHeader :: Protocol.KeepAliveHeader -> Response ()
keepAliveHeader =
  liftBSBuilder . ProtocolRenderer.keepAliveHeader

contentLengthHeader :: Protocol.ContentLengthHeader -> Response ()
contentLengthHeader =
  liftBSBuilder . ProtocolRenderer.contentLengthHeader

-- * Other
-------------------------

newLine :: Response ()
newLine =
  liftBSBuilder ProtocolRenderer.newLine

template :: (Data model) => Text -> model -> Response ()
template name model =
  do
    templatesRenderer <- ask
    traverse_ lazyText $ 
      TemplatesRenderer.render model name templatesRenderer

lazyText :: LazyText -> Response ()
lazyText t =
  lift $ for (Pipes.Text.fromLazy t) (yield . Data.Text.Encoding.encodeUtf8)

file :: FilePath -> Response ()
file path =
  lift $
    bracket
      (liftIO $ Filesystem.openFile path Filesystem.ReadMode)
      (liftIO . hClose)
      Pipes.ByteString.fromHandle

statusLine :: Protocol.Status -> Response ()
statusLine status =
  liftBSBuilder $ ProtocolRenderer.statusLine (1, 1) status

liftBSBuilder :: Data.ByteString.Builder.Builder -> Response ()
liftBSBuilder =
  lift . Pipes.ByteString.fromLazy . Data.ByteString.Builder.toLazyByteString
