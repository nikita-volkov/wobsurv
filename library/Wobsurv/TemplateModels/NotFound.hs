module Wobsurv.TemplateModels.NotFound where

import BasePrelude
import Data.Text (Text)
import Data.ByteString (ByteString)


data NotFound = 
  NotFound {
    uri :: Text
  }
  deriving (Show, Data, Typeable)
