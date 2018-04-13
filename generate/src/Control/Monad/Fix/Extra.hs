{-# LANGUAGE RecursiveDo #-}

module Control.Monad.Fix.Extra
  ( fixLookup
  , fixLookupM
  , fixEndo
  ) where

import           Control.Monad.Fix
import           Data.Foldable
import qualified Data.Map.Strict   as Map
import           Data.Semigroup

-- | Run a commputation with the ability to look up elements in its own output
--
-- The usual things for fixed point computations hold.
-- - Make sure that there are no dependency cycles in the output list.
-- - Make sure that the dependent parameters in the output are sufficiently
--   lazy
fixLookup
  :: (Ord k, Foldable f)
  => (a -> k)
  -- ^ Get a key from an output
  -> ((k -> Maybe a) -> f a)
  -- ^ A computation taking a function with which to look up its own output
  -> f a
  -- ^ The fixed point
fixLookup getName make =
  let m    = Map.fromList [ (getName x, x) | x <- toList made ]
      made = make (`Map.lookup` m)
  in  made

-- | Run a commputation with the ability to look up elements in its own output
--
-- The catch is that you must specify the keys of the output elements ahead of
-- time.
--
-- The usual things for fixed point computations hold.
-- - Make sure that there are no dependency cycles in the output list.
-- - Make sure that the dependent parameters in the output are sufficiently
--   lazy
fixLookupM
  :: (MonadFix m, Foldable f, Ord k)
  => [k]
  -- ^ The keys associated with the output.
  --
  -- If there is an element missing from here then you will not be able to look
  -- it up.
  --
  -- If there is an extra key and you look it up then you will get an
  -- irrefutable pattern match.
  -> (a -> k)
  -- ^ Get a key from an output
  -> ((k -> Maybe a) -> m (f a))
  -- ^ A computation taking a function with which to look up its own output
  -> m (f a)
  -- ^ The fixed point
fixLookupM keys getName make = mdo
  let m = Map.fromList [ (getName x, x) | x <- toList made ]
      get k = if k `elem` keys then knownJust (Map.lookup k m) else Nothing
  made <- make get
  pure made

-- | Make the 'Just' match irrefutable
knownJust :: Maybe a -> Maybe a
knownJust ~(Just x) = Just x

fixEndo :: Endo a -> a
fixEndo (Endo f) = fix f
