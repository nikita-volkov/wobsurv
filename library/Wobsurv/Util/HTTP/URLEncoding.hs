module Wobsurv.Util.HTTP.URLEncoding where

import BasePrelude
import Data.Text (Text)
import Data.ByteString (ByteString)
import Filesystem.Path (FilePath)
import qualified Network.HTTP.Types.URI as URI
import qualified Data.Text.Encoding as Text.Encoding
import qualified Filesystem.Path.Rules as Path.Rules


toText :: ByteString -> Text
toText = 
  Text.Encoding.decodeUtf8 . URI.urlDecode True

toFilePath :: ByteString -> FilePath
toFilePath =
  Path.Rules.decode Path.Rules.posix . URI.urlDecode True

fromText :: Text -> ByteString
fromText =
  URI.urlEncode True . Text.Encoding.encodeUtf8

fromFilePath :: FilePath -> ByteString
fromFilePath =
  URI.urlEncode True . Path.Rules.encode Path.Rules.posix


