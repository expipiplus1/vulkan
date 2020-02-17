module Write.Segment
  where

import           Data.Functor
import           Prelude
import           Data.Text                      ( Text )
import           Control.Monad
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Polysemy
import           Data.Foldable           hiding ( find )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.List                      ( sortOn )
import           Algebra.Graph.Relation

import           Error

-- - Find the transitive closure of the graph
-- - Partition any vertices which are reachable from just one seed
-- - Return these partitions along with the leftovers
segmentGraph
  :: forall r a n
   . (HasErr r, Ord n)
  => (a -> Text)
  -> (n -> Text)
  -> (a -> [n])
  -> (a -> [n])
  -> Vector a
  -> Vector (Vector n)
  -> Sem r (Vector (Vector a), Vector a)
segmentGraph debugVertex debugName getNames getWants xs seeds = do
  let numbered = zip (toList xs) [0 :: Int ..]
      unNumber = (xs V.!)
      allNames = sortOn fst [ (n, a) | (x, a) <- numbered, n <- getNames x ]
  assertUnique (debugVertex . (V.!) xs) debugName allNames
  let nameMap = Map.fromDistinctAscList allNames
      locate :: n -> Maybe Int
      locate = (`Map.lookup` nameMap)
      find n = case locate n of
        Nothing -> throw ("unable to find " <> debugName n <> " in any vertex")
        Just i  -> pure i
  relation <- edges . concat <$> forV
    numbered
    (\(x, n) -> forV (getWants x) (fmap (n, ) . find))
  seedIndices <- traverseV (traverseV find) seeds
  let
    closed = closure relation
    closedSeeds :: Vector (Set.Set Int)
    closedSeeds = seedIndices <&> \is -> Set.unions ((`postSet` closed) <$> is)
    all'        = Set.fromDistinctAscList (snd <$> numbered)
    us          = uniques closedSeeds
  pure
    ( V.fromList . fmap unNumber . Set.toList <$> us
    , V.fromList . fmap unNumber . Set.toList $ all' `Set.difference` Set.unions
      (V.toList us)
    )

-- | For each element get the union of all the other elements
uniques :: Ord a => Vector (Set.Set a) -> Vector (Set.Set a)
uniques xs =
  let increasing = V.prescanl Set.union Set.empty xs
      decreasing = V.prescanr Set.union Set.empty xs
  in  V.zipWith3 (\x i d -> (x `Set.difference` i) `Set.difference` d)
                 xs
                 increasing
                 decreasing

assertUnique
  :: (HasErr r, Eq n) => (a -> Text) -> (n -> Text) -> [(n, a)] -> Sem r ()
assertUnique debugVertex debugName xs =
  forV_ (zip xs (tail xs)) $ \((n1, x1), (n2, x2)) ->
    when (n1 == n2)
      .  throw
      $  debugName n1
      <> " exported from both "
      <> debugVertex x1
      <> " and "
      <> debugVertex x2
