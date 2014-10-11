module Wobsurv.Util.OpenServer.Connection where

import BasePrelude hiding (Interrupted)
import Pipes
import Pipes.Safe hiding (handle)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import qualified Data.ByteString as ByteString
import qualified Network.Socket as Socket
import qualified Pipes.Network.TCP as PipesNetwork


type Timeout = 
  Int

type BS =
  ByteString.ByteString


type Interactor = 
  Producer BS (SafeT IO) () -> Producer BS (SafeT IO) (Maybe Word)

session :: Timeout -> Interactor -> Socket.Socket -> IO ()
session initTimeout interactor socket =
  handlingExceptions $ runSafeT $ runEffect $ loop initTimeout
  where
    loop timeout = 
      do
        nextTimeout <- interactor request >-> response
        traverse_ (loop . fromIntegral) nextTimeout
      where
        request = 
          PipesNetwork.fromSocketTimeout timeout socket 4096
        response = 
          PipesNetwork.toSocketTimeout timeout socket


type Rejector = 
  Producer BS (SafeT IO) ()

rejection :: Timeout -> Rejector -> Socket.Socket -> IO ()
rejection timeout rejector socket =
  handlingExceptions $ runSafeT $ runEffect $ do
    -- Consume something to keep linux clients happy:
    next $ PipesNetwork.fromSocketTimeout 500000 socket 512
    rejector >-> PipesNetwork.toSocketTimeout timeout socket


handlingExceptions :: IO () -> IO ()
handlingExceptions =
  handle $ \e -> 
    case ioeGetErrorType e of
      ResourceVanished -> return ()
      TimeExpired -> return ()
      _ -> error $ "Wobsurv.Util.OpenServer.Connection.handlingExceptions: Unexpected IOException: " <> show e

