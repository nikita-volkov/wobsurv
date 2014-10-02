module Wobsurv.Util.HTTP.Model where

import BasePrelude
import qualified Data.ByteString as ByteString


type BS = 
  ByteString.ByteString

type Version = 
  (Word, Word)

-- * Headers
-------------------------

data Header =
  ConnectionHeader ConnectionHeader |
  ContentLengthHeader ContentLengthHeader |
  ContentTypeHeader ContentTypeHeader |
  KeepAliveHeader KeepAliveHeader
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

-- | Specifies whether to keep the connection alive.
type ConnectionHeader =
  Bool

-- | A length of the content in octets.
type ContentLengthHeader =
  Word

-- | A MIME type of content and possibly a charset
type ContentTypeHeader =
  (BS, Maybe Charset)

-- | A timeout in seconds and possibly a maximum amount of requests.
type KeepAliveHeader =
  (Word, Maybe Word)

data Charset =
  UTF8
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Generic)


-- * Methods
-------------------------

data StandardMethod = 
  Options | Get | Head | Post | Put | Delete | Trace | Connect
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Generic)

type Method =
  Either StandardMethod BS


-- * Statuses
-------------------------

type Status = 
  (Word, BS)

ok                       :: Status = (200, "OK")
badRequest               :: Status = (400, "Bad Request")
unauthorized             :: Status = (401, "Unauthorized")
forbidden                :: Status = (403, "Forbidden")
notFound                 :: Status = (404, "Not Found")
methodNotAllowed         :: Status = (405, "Method Not Allowed")
requestTimeOut           :: Status = (408, "Request Time-out")
entityTooLarge           :: Status = (413, "Entity Too Large")
requestURITooLarge       :: Status = (414, "Request-URI Too Large")
notImplemented           :: Status = (501, "Not Implemented")
serviceUnavailable       :: Status = (503, "Service Unavailable")
httpVersionNotSupported  :: Status = (505, "HTTP Version not supported")


-- * URI
-------------------------

data URI =
  AbsoluteURI !Scheme !Authority !(Maybe Port) !RelativeURI |
  SchemeRelativeURI !Authority !(Maybe Port) !RelativeURI |
  RelativeURI !RelativeURI
  deriving (Show, Read, Eq, Ord, Typeable, Generic)

type RelativeURI =
  (Maybe RelativePath, Maybe Query, Maybe Fragment)

type Authority =
  Either Domain IP

data Scheme = 
  HTTP | HTTPS 
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Typeable, Generic)

type Domain = 
  BS

type IP = 
  BS

type Port = 
  Int

type RelativePath = 
  BS

type Query = 
  BS

type Fragment = 
  BS
