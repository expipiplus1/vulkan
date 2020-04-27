{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Write.Segment
  where

import           Data.Functor
import           Data.Vector                    ( Vector )
import           Prelude

data Segment i a = Segment i (Vector a)
  deriving (Functor, Foldable, Traversable, Show)

