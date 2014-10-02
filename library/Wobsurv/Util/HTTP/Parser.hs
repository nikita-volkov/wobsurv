module Wobsurv.Util.HTTP.Parser where

import BasePrelude hiding (takeWhile, isSpace)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Attoparsec.ByteString.Char8
import Wobsurv.Util.HTTP.Model
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)


type UnknownHeader = (ByteString, ByteString)

labeling :: String -> Parser a -> Parser a
labeling n p = 
  p <?> n

head :: Parser (Method, RelativeURI, Version, [UnknownHeader])
head = 
  labeling "head" $ 
    (,,,) <$> 
      (method <* space) <*> (relativeURI <* space) <*>
      (version <* endOfLine) <*> (many unknownHeader <* endOfLine)

method :: Parser Method
method =
  labeling "method" $
    (Left <$> standardMethod) <|> (Right <$> takeWhile isAlpha)

standardMethod :: Parser StandardMethod
standardMethod = 
  labeling "standardMethod" $
    p "OPTIONS" Options <|>
    p "GET" Get <|>
    p "HEAD" Head <|>
    p "POST" Post <|>
    p "PUT" Put <|>
    p "DELETE" Delete <|>
    p "TRACE" Trace <|>
    p "CONNECT" Connect 
  where
    p n m = 
      string n *> pure m

version :: Parser Version
version =
  labeling "version" $
    (,) <$> (string "HTTP/" *> decimal) <*> (char '.' *> decimal)

relativeURI :: Parser RelativeURI
relativeURI =
  labeling "relativeURI" $
    (,,) <$> path <*> optional query <*> optional fragment
  where
    path = 
      char '/' *> optional (takeWhile1 (\c -> c /= '?' && not (isSpace c)))
    query = 
      char '?' *> takeWhile (\c -> c /= '#' && not (isSpace c))
    fragment = 
      char '#' *> takeWhile (not . isSpace)

header :: Parser Header
header =
  labeling "header" $
    (ConnectionHeader <$> connectionHeader) <|> 
    (ContentLengthHeader <$> contentLengthHeader) <|>
    (ContentTypeHeader <$> contentTypeHeader) <|> 
    (KeepAliveHeader <$> keepAliveHeader)

connectionHeader :: Parser ConnectionHeader
connectionHeader =
  labeling "connectionHeader" $
    string "Connection: " *> (keepAlive <|> close)
  where
    keepAlive = string "keep-alive" *> pure True
    close = string "close" *> pure False

contentLengthHeader :: Parser ContentLengthHeader
contentLengthHeader =
  labeling "contentLengthHeader" $
    string "Content-Length: " *> decimal

contentTypeHeader :: Parser ContentTypeHeader
contentTypeHeader =
  labeling "contentTypeHeader" $
    undefined

keepAliveHeader :: Parser KeepAliveHeader
keepAliveHeader =
  labeling "keepAliveHeader" $
    undefined

unknownHeader :: Parser UnknownHeader
unknownHeader =
  labeling "unknownHeader" $
    (,) <$> (key <* char ':' <* skipSpace) <*> value <* endOfLine
  where
    key = takeWhile1 (\c -> isAlphaNum c || c == '-')
    value = takeWhile1 (/= '\n')


