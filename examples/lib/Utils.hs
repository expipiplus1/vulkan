module Utils
  ( loopJust
  , loopUntilM
  , noSuchThing
  ) where

import Control.Concurrent ()
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.IO.Exception
  ( IOErrorType (NoSuchThing)
  , IOException (IOError)
  )
import UnliftIO.Exception (throwIO)

loopJust :: (Monad m) => (a -> m (Maybe a)) -> a -> m ()
loopJust f x =
  f x >>= \case
    Nothing -> pure ()
    Just x' -> loopJust f x'

loopUntilM :: (Monad m) => m Bool -> m ()
loopUntilM m = do
  q <- m
  unless q $ loopUntilM m

{- | Throw 'IOError' with 'NoSuchThing' as the error type. Mirrors the small
helper duplicated across several example executables.
-}
noSuchThing :: (MonadIO m) => String -> m a
noSuchThing message =
  liftIO . throwIO $ IOError Nothing NoSuchThing "" message Nothing Nothing
