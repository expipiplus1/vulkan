module Data.Vector.Extra
  ( module Data.Vector
  , pattern Empty
  , pattern (:<|)
  , pattern Singleton
  ) where

import           Data.Vector
import qualified Data.Vector                   as V
import           Prelude

{-# complete Empty, (:<|) #-}

pattern Empty :: Vector a
pattern Empty <- (V.null -> True) where
  Empty = V.empty

pattern (:<|) :: a -> Vector a -> Vector a
pattern x :<| xs <- (V.uncons -> Just (x, xs)) where
  (:<|) = V.cons
infixr :<|

pattern Singleton :: a -> Vector a
pattern Singleton x <- (V.uncons -> Just (x, Empty)) where
  Singleton = V.singleton

