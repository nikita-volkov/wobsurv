module Wobsurv.Util.Mustache.Renderer where

import BasePrelude
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import qualified Data.ByteString
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text
import qualified Data.Text.Lazy
import qualified Filesystem
import qualified Filesystem.Path.CurrentOS as Path
import qualified Text.Hastache
import qualified Text.Hastache.Context


type BS =
  Data.ByteString.ByteString

type Text =
  Data.Text.Text

type LazyText =
  Data.Text.Lazy.Text

type Path =
  Path.FilePath

-- |
-- A fast Hastache renderer, 
-- which preloads all templates instead of reading them during each rendering.
type Renderer =
  TemplatesCache

-- |
-- Mappings from template names to their contents
-- stored in RAM for faster processing.
-- 
-- Supposed to be loaded once during the initialization of the app.
type TemplatesCache =
  HashMap.HashMap TemplateName Text

type TemplateName =
  Text

type Context =
  HashMap.HashMap Text Value

data Value where
  Variable :: Text.Hastache.MuVar a => a -> Value
  Contexts :: [Context] -> Value


-- |
-- Initialize a renderer by loading all templates from the specified folder.
new :: Path -> IO Renderer
new path =
  initTemplatesCache
  where
    initTemplatesCache =
      flip execStateT mempty $ do
        subpaths <- lift $ Filesystem.listDirectory path
        forM_ subpaths $ \subpath -> do
          contents <- lift $ Filesystem.readTextFile subpath
          modify $ HashMap.insert (fromString $ Path.encodeString $ Path.basename subpath) contents 

-- |
-- Workarounds over Hastache API to enable it with caching.
render :: (Data model) => model -> TemplateName -> Renderer -> Maybe LazyText
render model name cache =
  do
    template <- HashMap.lookup name cache
    return $ unsafePerformIO $ 
      Text.Hastache.hastacheStr config template context
  where
    config =
      Text.Hastache.MuConfig Text.Hastache.htmlEscape Nothing Nothing getTemplate
      where
        getTemplate s = 
          return $ HashMap.lookup (fromString s) cache
    context =
      Text.Hastache.Context.mkGenericContext model

