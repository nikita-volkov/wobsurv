{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import BasePrelude hiding (getEnv)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text (Text)
import Data.ByteString (ByteString)
import Filesystem.Path.CurrentOS (FilePath)
import qualified Control.Concurrent.Async.Lifted as Async
import qualified Network
import qualified Network.HTTP.Client as Client
import qualified Wobsurv
import qualified Filesystem
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.ByteString.Lazy
import qualified Data.ByteString as ByteString
import qualified Network.HTTP.Types.Status
import qualified Filesystem.Path.CurrentOS
import qualified System.Random.MWC as MWC


type LazyByteString =
  Data.ByteString.Lazy.ByteString

main = 
  Network.withSocketsDo $ htfMain htf_thisModulesTests


-- * Tests
-------------------------

test_notFound =
  runTest env1 $ do
    status <- getResponseStatus =<< resourceURI =<< missingResource
    lift $ assertEqual "Not Found" status

test_ok =
  runTest env1 $ do
    response <- getResponse =<< resourceURI =<< existentResource
    lift $ assertEqual "OK" $ responseStatus response
    lift $ assertBool $ (/= "") $ responseBody response

test_corruptRequestsDontMakeTheServerThrowExceptions =
  unitTestPending ""

test_corruptRequestsDontReduceTheConnectionSlots =
  unitTestPending ""

test_highLoad =
  runTest (env1 {envConnectionsLimit = 48}) $ do
    results <-
      Async.mapConcurrently id $ replicate 48 $ do
        replicateM 5 $ 
          getResponseStatus =<< resourceURI =<< someResource
    lift $ assertBool $ all (/= "Service Unavailable") $ concat $ results

test_tooManyConnections =
  runTest (env1 {envConnectionsLimit = 10}) $ do
    results <- 
      Async.mapConcurrently id $ replicate 20 $ do
        getResponseStatus =<< resourceURI =<< largeResource
    lift $ assertBool $ (> 1) $ length $ filter (== "Service Unavailable") $ results

test_multipleClientsOnTooManyConnectionsStillGetDismissedProperly =
  test_tooManyConnections

test_slotsGetReleased =
  runTest (env1 {envConnectionsLimit = 10}) $ do
    Async.mapConcurrently id $ replicate 20 $ do
      getResponseStatus =<< resourceURI =<< someResource
    results <- 
      Async.mapConcurrently id $ replicate 10 $ do
        getResponseStatus =<< resourceURI =<< someResource
    lift $ assertBool $ all (/= "Service Unavailable") $ results

test_uriDecoding =
  runTest env1 $ do
    let path = "Директория/Название в Юникоде с пробелами.расширение"
    createFile 1 path
    lift . assertEqual "OK" =<< getResponseStatus =<< resourceURI path


-- * Test monad
-------------------------

type Test =
  ReaderT Setup IO

type Setup =
  (Client.Manager, ThreadId, Env, MWC.GenIO)

runTest :: Env -> Test a -> IO a
runTest env test =
  bracket acquire release $ runReaderT test
  where
    acquire =
      do
        manager <- Client.newManager Client.defaultManagerSettings
        serverThread <- 
          forkIO $ Wobsurv.serve $ 
            Wobsurv.Settings
              (False)
              (envPort env) 
              (envConnectionsLimit env) 
              ("templates")
              (envRoot env)
              (mempty)
        traverse_ (prepFile 10000) (envSmallFiles env)
        traverse_ (prepFile 1000000) (envLargeFiles env)
        rand <- MWC.create
        return (manager, serverThread, env, rand)
      where
        prepFile size path =
          do
            Filesystem.createTree (Filesystem.Path.CurrentOS.directory fullPath)
            Filesystem.writeFile fullPath $ 
              ByteString.replicate size (fromIntegral $ ord 'x')
          where
            fullPath = envRoot env <> path
    release (manager, serverThread, env, rand) =
      do
        Client.closeManager manager
        killThread serverThread
        Filesystem.removeTree (envRoot env)

createFile :: Int -> FilePath -> Test ()
createFile size path =
  do
    fullPath <- (<> path) . envRoot <$> getEnv
    lift $ do
      Filesystem.createTree (Filesystem.Path.CurrentOS.directory fullPath)
      Filesystem.writeFile fullPath $ 
        ByteString.replicate size (fromIntegral $ ord 'x')

-- | Send a "GET" request.
getResponse :: URI -> Test Response
getResponse uri =
  do
    (manager, _, _, _) <- ask
    request <- Client.parseUrl uri
    lift $ Client.httpLbs request manager

getResponseStatus :: URI -> Test Status
getResponseStatus uri =
  do
    (manager, _, _, _) <- ask
    request <- Client.parseUrl uri
    lift $ fmap Network.HTTP.Types.Status.statusMessage $ handle getHTTPExceptionStatus $ 
      Client.responseStatus <$> Client.httpLbs request manager
  where
    getHTTPExceptionStatus =
      \case
        Client.StatusCodeException s _ _ -> return s
        e -> throwIO e

random :: MWC.Variate a => (a, a) -> Test a
random range =
  do
    (_, _, _, gen) <- ask
    lift $ MWC.uniformR range gen

oneOf :: [a] -> Test (Maybe a)
oneOf list =
  case length list of
    0 -> return Nothing
    l -> do
      i <- random (0, pred l)
      return $ Just $ list !! i

someResource :: Test FilePath
someResource =
  random (0 :: Int, 1) >>= \case
    0 -> missingResource
    1 -> existentResource

getEnv :: Test Env
getEnv =
  ReaderT $ \(_, _, x, _) -> return x

largeResource :: Test FilePath
largeResource =
  return . fromMaybe (error "No large files") =<< oneOf . envLargeFiles =<< getEnv

smallResource :: Test FilePath
smallResource =
  return . fromMaybe (error "No small files") =<< oneOf . envSmallFiles =<< getEnv

missingResource :: Test FilePath
missingResource =
  return . fromMaybe (error "No missing files") =<< oneOf . envMissingFiles =<< getEnv

existentResource :: Test FilePath
existentResource =
  random (0 :: Int, 1) >>= \case
    0 -> smallResource
    1 -> largeResource

responseStatus :: Response -> Status
responseStatus =
  Network.HTTP.Types.Status.statusMessage . Client.responseStatus

responseBody :: Response -> LazyByteString
responseBody =
  Client.responseBody

resourceURI :: FilePath -> Test URI
resourceURI path = do
  (_, _, env, _) <- ask
  return $ 
    "http://localhost:" <> show (envPort env) <> "/" <> pathString
  where
    pathString =
      Filesystem.Path.CurrentOS.encodeString path

type Response =
  Client.Response LazyByteString

type URI =
  [Char]

type Status =
  ByteString


-- ** Predefined environments
-------------------------

data Env =
  Env {
    envPort :: Word,
    envConnectionsLimit :: Word,
    envRoot :: FilePath,
    envLargeFiles :: [FilePath],
    envSmallFiles :: [FilePath],
    envMissingFiles :: [FilePath]
  }

env1 :: Env
env1 =
  Env 53000 10 root largeFiles smallFiles missingFiles
  where
    root = 
      "dist/wobsurv-test-tmp/"
    smallFiles = 
      [ "a.txt", "dir/b.txt", "Ёжик лижет мёд.jpg" ]
    largeFiles = 
      [ "c.mp3", "d/d d d" ]
    missingFiles =
      [ "e.txt", "f.nfo" ]


