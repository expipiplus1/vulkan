module Utils where

import           Control.Concurrent             ( )
import           Control.Monad

loopJust :: Monad m => (a -> m (Maybe a)) -> a -> m ()
loopJust f x = f x >>= \case
  Nothing -> pure ()
  Just x' -> loopJust f x'

loopUntilM :: Monad m => m Bool -> m ()
loopUntilM m = do
  q <- m
  unless q $ loopUntilM m
