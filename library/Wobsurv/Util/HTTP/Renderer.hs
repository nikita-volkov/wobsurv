module Wobsurv.Util.HTTP.Renderer where

import BasePrelude
import Control.Monad.Trans.Writer
import Data.ByteString.Builder
import Wobsurv.Util.HTTP.Model
import qualified Pipes
import qualified Pipes.ByteString
import qualified Data.ByteString
import qualified Data.ByteString.Lazy


toProducer :: Monad m => Builder -> Pipes.Producer BS m ()
toProducer =
  Pipes.ByteString.fromLazy . toLazyByteString

toByteString :: Builder -> Data.ByteString.ByteString
toByteString =
  Data.ByteString.Lazy.toStrict . toLazyByteString

newLine :: Builder
newLine =
  string7 "\r\n"

statusLine :: Version -> Status -> Builder
statusLine versionV statusV =
  version versionV <> char7 ' ' <> status statusV <> newLine

relativeURI :: RelativeURI -> Builder
relativeURI (path, query, fragment) =
  execWriter $ do
    tell $ char7 '/'
    traverse_ (tell . byteString) path
    traverse_ (tell . (char7 '?' <>) . byteString) query
    traverse_ (tell . (char7 '#' <>) . byteString) fragment

version :: Version -> Builder =
  \(major, minor) ->
    string7 "HTTP/" <> wordDec major <> char7 '.' <> wordDec minor

status :: Status -> Builder
status (code, message) =
  wordDec code <> char7 ' ' <> byteString message

headers :: [Header] -> Builder
headers =
  mconcat . map header

header :: Header -> Builder
header =
  \case
    ConnectionHeader x -> connectionHeader x
    ContentLengthHeader x -> contentLengthHeader x
    ContentTypeHeader x -> contentTypeHeader x
    KeepAliveHeader x -> keepAliveHeader x

connectionHeader :: ConnectionHeader -> Builder
connectionHeader keepAlive =
  string7 "Connection: " <> 
  string7 (if keepAlive then "keep-alive" else "close") <>
  newLine

contentLengthHeader :: ContentLengthHeader -> Builder
contentLengthHeader length =
  string7 "Content-Length: " <> wordDec length <> newLine

contentTypeHeader :: ContentTypeHeader -> Builder
contentTypeHeader (mimeType, charsetV) =
  string7 "Content-Type: " <> fields <> newLine
  where
    fields =
      mconcat $ intersperse ";" $ catMaybes $
        [ Just (byteString mimeType), 
          charsetField <$> charsetV ] 
      where
        charsetField x =
          string7 "charset=" <> charset x

keepAliveHeader :: KeepAliveHeader -> Builder
keepAliveHeader (timeout, max) =
  string7 "Keep-Alive: " <> fields <> newLine
  where
    fields =
      mconcat $ intersperse ", " $ catMaybes $
        [ Just $ timeoutField $ timeout, 
          maxField <$> max ]
      where
        timeoutField x =
          string7 "timeout=" <> wordDec x
        maxField x =
          string7 "max=" <> wordDec x

charset :: Charset -> Builder
charset =
  \case
    UTF8 -> "utf-8"

method :: Method -> Builder
method =
  either standardMethod byteString

standardMethod :: StandardMethod -> Builder
standardMethod = 
  \case
    Options -> string7 "OPTIONS"
    Get     -> string7 "GET"
    Head    -> string7 "HEAD"
    Post    -> string7 "POST"
    Put     -> string7 "PUT"
    Delete  -> string7 "DELETE"
    Trace   -> string7 "TRACE"
    Connect -> string7 "CONNECT"
