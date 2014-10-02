module Wobsurv.RequestHeaders where

import BasePrelude
import qualified Wobsurv.Util.HTTP.Model as Protocol


data RequestHeaders =
  RequestHeaders {
    connection :: Maybe Protocol.ConnectionHeader
  }

fromProtocolHeaders :: [Protocol.Header] -> RequestHeaders
fromProtocolHeaders =
  foldr step init
  where
    init =
      RequestHeaders Nothing
    step =
      \case
        Protocol.ConnectionHeader h -> \r -> r {connection = Just h}
        _ -> id
