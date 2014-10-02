module Wobsurv.TemplateModels.Index where

import BasePrelude
import Data.Text (Text)
import Data.ByteString (ByteString)


data Index = 
  Index {
    path :: Text,
    contents :: [Text]
  }
  deriving (Show, Data, Typeable)
