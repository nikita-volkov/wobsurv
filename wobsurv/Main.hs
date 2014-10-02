module Main where

import BasePrelude
import Safe
import Data.Text (Text)
import Data.ByteString (ByteString)
import Filesystem.Path (FilePath)
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Yaml as Yaml
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding as Text.Encoding
import qualified Filesystem
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Wobsurv
import qualified Network


main :: IO ()
main = 
  Network.withSocketsDo $ do
    portArg <- fmap read . headMay <$> getArgs
    settings <-
      loadSettings >>= \case
        Left e -> do
          putStrLn $ "Config error: " <> e
          exitFailure
        Right s -> 
          return $ maybe s (\x -> s {Wobsurv.port = x}) portArg
    Wobsurv.serve settings

loadSettings :: IO (Either String Wobsurv.Settings)
loadSettings =
  do
    Just json <- Yaml.decodeFile "config.yaml"
    return $ Aeson.parseEither settingsParser json

settingsParser :: Aeson.Object -> Aeson.Parser Wobsurv.Settings
settingsParser o =
  do
    logging          <- o .:? "logging" .!= True
    port             <- o .:? "default-port" .!= 53000
    connectionsLimit <- o .:? "connections-limit" .!= 48
    templatesDir     <- filePath =<< o .:? "templates-dir" .!= "templates"
    contentDir       <- filePath =<< o .:? "content-dir" .!= "www"
    mimeMappings     <- traverse byteString =<< o .:? "mime-types" .!= mempty
    return $ 
      Wobsurv.Settings logging port connectionsLimit templatesDir contentDir mimeMappings
  where
    filePath = 
      Aeson.withText "FilePath" $ pure . FilePath.decode
    byteString =
      Aeson.withText "ByteString" $ pure . Text.Encoding.encodeUtf8


