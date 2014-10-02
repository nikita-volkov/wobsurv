-- |
-- Utils for integration of Pipes and Attoparsec.
module Wobsurv.Util.PipesAttoparsec where

import BasePrelude
import Pipes.Parse hiding (execStateT, evalStateT)
import Control.Monad.Trans.State
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.ByteString as BS


type BS = 
  BS.ByteString

type AttoParser =
  Attoparsec.Parser

type PipesParser m r =
  Parser BS m r

data Error =
  UnfinishedInput |
  ConsumedTooMuch |
  AttoparsecError ![String] !String
  deriving (Show)


liftParserWithLimit :: (Monad m) => Int -> AttoParser a -> PipesParser m (Either Error a)
liftParserWithLimit limit parser =
  evalStateT (loop initCont) limit
  where
    loop cont =
      do
        chunk <- return . fromMaybe BS.empty =<< lift draw
        modify (subtract (BS.length chunk))
        n <- get
        if n < 0
          then 
            return $ Left $ ConsumedTooMuch
          else
            case cont chunk of
              Attoparsec.Fail remainder context message -> 
                do
                  lift $ unDraw remainder
                  return $ Left $ AttoparsecError context message
              Attoparsec.Partial cont' -> 
                if BS.length chunk > 0
                  then loop cont'
                  else return $ Left $ UnfinishedInput
              Attoparsec.Done remainder result -> 
                do
                  lift $ unDraw remainder
                  return $ Right result
    initCont =
      Attoparsec.parse parser

