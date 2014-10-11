module Wobsurv.Util.PartialHandler where

import BasePrelude


-- |
-- A composable exception handler.
newtype PartialHandler a =
  PartialHandler (SomeException -> Maybe (IO a))


instance Monoid (PartialHandler a) where
  mempty = 
    PartialHandler $ const Nothing
  mappend (PartialHandler h1) (PartialHandler h2) =
    PartialHandler $ \e -> h1 e <|> h2 e


-- |
-- Construct from a typed handler.
typed :: Exception e => (e -> Maybe (IO a)) -> PartialHandler a
typed h =
  PartialHandler $ \e -> 
    case fromException e of
      Just e' -> h e'
      Nothing -> Nothing


-- |
-- Convert a partial handler into a total "SomeException" handler,
-- which rethrows exceptions for unhandled cases.
toTotal :: PartialHandler a -> (SomeException -> IO a)
toTotal (PartialHandler h) =
  \e -> fromMaybe (throwIO e) (h e)


-- * Standard handlers
-------------------------

onThreadKilled :: IO a -> PartialHandler a
onThreadKilled io =
  typed $ \case
    ThreadKilled -> Just io
    _ -> Nothing

-- |
-- A handler which rethrows all exceptions to the specified thread.
rethrowTo :: ThreadId -> PartialHandler ()
rethrowTo t =
  PartialHandler $ \e -> Just (throwTo t e)


-- * Utils
-------------------------

-- |
-- Like 'forkFinally' but rethrows all unhandled exceptions to the parent thread.
forkFinallyRethrowing :: IO () -> PartialHandler () -> IO () -> IO ThreadId
forkFinallyRethrowing performer handler releaser =
  do
    t <- myThreadId
    forkFinally performer $ \r -> do
      releaser
      either (toTotal (handler <> rethrowTo t)) return r

