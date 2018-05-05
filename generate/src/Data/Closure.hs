module Data.Closure
  ( close
  , closeNonReflexive
  , closeL
  , closeNonReflexiveL
  ) where

import           Data.Function
import qualified Data.Set      as Set

----------------------------------------------------------------
-- Graph utils
----------------------------------------------------------------

-- TODO: Make these operate on sets

-- | Compute the reflexive transitive closure of some seed values using a
-- function providing all the descendants of an input
close :: Ord a => (a -> Set.Set a) -> Set.Set a -> Set.Set a
close next = fix (step (nubNext next))

-- | Compute the reflexive transitive closure of some seed values using a
-- function providing all the descendants of an input
closeL :: Ord a => (a -> [a]) -> [a] -> [a]
closeL f l =
  Set.toList $ close (Set.fromList . f) (Set.fromList l)

-- | Compute the reflexive transitive closure of some seed values using a
-- function providing all the descendants of an input
closeNonReflexive :: Ord a => (a -> Set.Set a) -> Set.Set a -> Set.Set a
closeNonReflexive next i = fix (step (nubNext next)) (nubNext next i)

-- | Compute the reflexive transitive closure of some seed values using a
-- function providing all the descendants of an input
closeNonReflexiveL :: Ord a => (a -> [a]) -> [a] -> [a]
closeNonReflexiveL f l =
  Set.toList $ closeNonReflexive (Set.fromList . f) (Set.fromList l)

-- | Apply the step function to all the inputs and make sure there are no
-- duplicates.
nubNext :: Ord a => (a -> Set.Set a) -> Set.Set a -> Set.Set a
nubNext = bindSet

-- | Check if there are any new entries this step, if there are then recurse
-- using those, otherwise return the input.
--
-- This is important to make the function sufficiently lazy
step
  :: Ord a
  => (Set.Set a -> Set.Set a)
     -- ^ Perform one iteration
  -> (Set.Set a -> Set.Set a)
     -- ^ Perform all iterations
  -> Set.Set a
  -> Set.Set a
step next nextN source =
  let new = next source Set.\\ source
  in  if null new then source else (source `Set.union` nextN new)

bindSet :: Ord a => (a -> Set.Set a) -> Set.Set a -> Set.Set a
bindSet f s = Set.unions (f <$> Set.toList s)
