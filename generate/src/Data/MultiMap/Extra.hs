{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.MultiMap.Extra
  ( -- orphans
  ) where

import qualified Data.Map       as Map
import           Data.MultiMap
import           Data.Semigroup

instance Ord k => Semigroup (MultiMap k v) where
  a <> b = fromMap (Map.unionWith (<>) (toMap a) (toMap b))

instance Ord k => Monoid (MultiMap k v) where
  mempty = empty
  mappend = (<>)
