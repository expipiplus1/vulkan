module Utils where

import           Control.Concurrent             ( )
import           Control.Monad

loopUntilM :: Monad m => m Bool -> m ()
loopUntilM m = do
  q <- m
  unless q $ loopUntilM m
