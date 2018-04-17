module Data.Functor.Extra
  ( (<&>)
  , module Data.Functor
  ) where

import           Data.Functor

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap
