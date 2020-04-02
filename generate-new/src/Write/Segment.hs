{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Write.Segment
  where

import           Data.Functor
import           Prelude
import           Data.Vector                    ( Vector )

data Segment i a = Segment i (Vector a)
  deriving (Functor, Foldable, Traversable, Show)

