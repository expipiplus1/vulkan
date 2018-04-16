module Data.Closure
  ( close
  , closeNonReflexive
  ) where

import           Data.Function
import           Data.List.Extra

----------------------------------------------------------------
-- Graph utils
----------------------------------------------------------------

-- | Compute the reflexive transitive closure of some seed values using a
-- function providing all the descendants of an input
close :: Ord a => (a -> [a]) -> [a] -> [a]
close next = fix (step (nubNext next))

-- | Compute the reflexive transitive closure of some seed values using a
-- function providing all the descendants of an input
closeNonReflexive :: Ord a => (a -> [a]) -> [a] -> [a]
closeNonReflexive next i = fix (step (nubNext next)) (nubNext next i)

-- | Apply the step function to all the inputs and make sure there are no
-- duplicates.
nubNext :: Ord a => (a -> [a]) -> [a] -> [a]
nubNext next = nubOrd . concatMap next

-- | Check if there are any new entries this step, if there are then recurse
-- using those, otherwise return the input.
--
-- This is important to make the function sufficiently lazy
step
  :: Ord a
  => ([a] -> [a])
     -- ^ Perform one iteration
  -> ([a] -> [a])
     -- ^ Perform all iterations
  -> [a]
  -> [a]
step next nextN source =
  let new = next source \\ source
  in  if null new then source else nubOrd (source ++ nextN new)
