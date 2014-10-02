module Wobsurv.Logging where

import BasePrelude hiding (log)
import qualified Wobsurv.Interaction
import qualified System.Locale                 as Locale
import qualified Data.Time                     as Time
import qualified Wobsurv.Util.HTTP.Renderer    as HTTP.Renderer
import qualified Wobsurv.Util.HTTP.Model       as HTTP.Model
import qualified Wobsurv.Util.HTTP.URLEncoding as HTTP.URLEncoding
import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Builder       as ByteString.Builder
import qualified Data.ByteString.Lazy.Char8    as ByteString.Lazy.Char8
import qualified Data.Text.Lazy.IO             as Text.Lazy.IO
import qualified Data.Text.Lazy.Encoding       as Text.Lazy.Encoding
import qualified Data.Text.Lazy.Builder        as Text.Lazy.Builder


log :: Wobsurv.Interaction.Summary -> IO ()
log (request, status) =
  Text.Lazy.IO.putStrLn =<< do
    time <- Time.formatTime Locale.defaultTimeLocale "%F %X %Z" <$> Time.getZonedTime
    return $ Text.Lazy.Builder.toLazyText $
      case request of
        Just (method, uri) ->
          Text.Lazy.Builder.fromString time <> 
          Text.Lazy.Builder.fromString ": " <>
          (liftBSB $ HTTP.Renderer.status status) <> 
          Text.Lazy.Builder.fromString " <-- " <>
          (liftBSB $ HTTP.Renderer.method method) <>
          Text.Lazy.Builder.singleton ' ' <>
          (Text.Lazy.Builder.fromText $ HTTP.URLEncoding.toText $ HTTP.Renderer.toByteString $ HTTP.Renderer.relativeURI uri)
        Nothing ->
          Text.Lazy.Builder.fromString time <> 
          Text.Lazy.Builder.fromString ": " <>
          (liftBSB $ HTTP.Renderer.status status)
  where
    liftBSB =
      Text.Lazy.Builder.fromLazyText . Text.Lazy.Encoding.decodeLatin1 . ByteString.Builder.toLazyByteString

newSynchronizedLogger :: IO (Wobsurv.Interaction.Summary -> IO ())
newSynchronizedLogger = do
  loggerLock <- newMVar ()
  return $ 
    \m -> withMVar loggerLock $ const $ log m
