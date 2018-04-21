{-# LANGUAGE ScopedTypeVariables #-}

module Data.List.Extra2
  ( iterateSuffixes
  , iterateSuffixesM
  , module Data.List.Extra
  ) where

import           Data.Bifunctor
import           Data.Foldable
import           Data.List.Extra

iterateSuffixes
  :: forall a b
   . ([a] -> (b, [a]))
  -- ^ A function which takes a list transforming it and returning something
  -> [a]
  -- ^ A list to extract parts from
  -> ([b], [a])
  -- ^ (The list of (non-empty) extracted prefixes, the list without those
  -- prefixes)
iterateSuffixes split = foldr go ([], [])
  where
    go :: a -> ([b], [a]) -> ([b], [a])
    go x (ss, xs) = first (: ss) $ split (x : xs)

iterateSuffixesM
  :: forall m a b
   . Monad m
  => ([a] -> m (b, [a]))
  -- ^ A function which takes a list transforming it and returning something
  -> [a]
  -- ^ A list to extract parts from
  -> m ([b], [a])
  -- ^ (The list of (non-empty) extracted prefixes, the list without those
  -- prefixes)
iterateSuffixesM split = foldrM go ([], [])
  where
    go :: a -> ([b], [a]) -> m ([b], [a])
    go x (ss, xs) = first (: ss) <$> split (x : xs)
