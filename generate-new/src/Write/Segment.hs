{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language GeneralizedNewtypeDeriving #-}
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


newtype SegmentSeed n = SegmentSeed (Vector n)
  deriving (Functor, Foldable, Traversable)

-- | A group which will take priority over the subsequent groups
newtype SegmentGroup n = SegmentGroup (Vector (SegmentSeed n))
  deriving (Functor, Foldable, Traversable)

newtype SegmentGroups n = SegmentGroups (Vector (SegmentGroup n))
  deriving (Functor, Foldable, Traversable)

newtype Segment a = Segment (Vector a)
  deriving (Functor, Foldable, Traversable, Semigroup, Monoid, Show)

data SegmentedGroup a = SegmentedGroup
  { groupSegments :: Vector (Segment a)
  , groupExtras   :: Segment a
  }
  deriving (Functor, Foldable, Traversable, Show)

instance Semigroup (SegmentedGroup a) where
  SegmentedGroup a1 b1 <> SegmentedGroup a2 b2 = SegmentedGroup (a1<> a2) (b1  <> b2)

instance Monoid (SegmentedGroup a) where
  mempty = SegmentedGroup mempty mempty

newtype SegmentedGroups a = SegmentedGroups (Vector (SegmentedGroup a))
  deriving (Functor, Foldable, Traversable, Show)

-- - Find the transitive closure of the graph
-- - Partition any vertices which are reachable from just one seed
-- - Return these partitions along with the leftovers
segmentGraph
  :: forall r a n
   . (HasErr r, Ord n, Show n)
  => (a -> Text)
  -> (n -> Text)
  -> (a -> [n])
  -> (a -> [n])
  -> Vector a
  -> SegmentGroups n
  -> Sem r (SegmentedGroups a)
segmentGraph debugVertex debugName getNames getWants xs groups = do
  let numbered = zip (toList xs) [0 :: Int ..]
      unNumber = (xs V.!)
      allNames = sortOn fst [ (n, a) | (x, a) <- numbered, n <- getNames x ]
  -- assertUnique (debugVertex . (V.!) xs) debugName allNames
  -- let nameMap = Map.fromDistinctAscList allNames
  let nameMap = Map.fromAscList allNames
      locate :: n -> Maybe Int
      locate = (`Map.lookup` nameMap)
      find n = case locate n of
        Nothing -> throw ("unable to find " <> debugName n <> " in any vertex")
        Just i  -> pure i
  es <- concat
    <$> forV numbered (\(x, n) -> forV (getWants x) (fmap (n, ) . find))

  let relation = vertices (snd <$> numbered) `overlay` edges es
  SegmentGroups groupIndices <- traverseV find groups
  let closed = closure relation
      -- For each group, for each segment in that group, get the set of
      -- elements
      closedSeeds :: Vector (Vector (Set.Set Int))
      closedSeeds = groupIndices <&> \(SegmentGroup g) ->
        g <&> \(SegmentSeed s) -> Set.unions ((`postSet` closed) <$> s)
      us   = uniqueGroups closedSeeds
  pure (unNumber <$> us)

-- | For each group assign the elements to their unique segment, or leave in
-- leftovers.
uniqueGroups
  :: forall a . Ord a => Vector (Vector (Set.Set a)) -> SegmentedGroups a
uniqueGroups xs = SegmentedGroups . fmap fst $ V.postscanl' go mempty xs
 where
  go
    :: (SegmentedGroup a, Set.Set a)
     --- ^ The last group and the a's spoken for so far
    -> Vector (Set.Set a)
     --- ^ This group's segments
    -> (SegmentedGroup a, Set.Set a)
     --- ^ This group and the a's spoken for so far
  go (_, spokenFor) segments =
    let --- | Only new elements
        segmentsNew = (`Set.difference` spokenFor) <$> segments
        (us, extra) = uniques segmentsNew
        toSegment v = Segment (V.fromList (Set.toList v))
        newSegment = SegmentedGroup (toSegment <$> us) (toSegment extra)
    in  (newSegment, Set.unions (spokenFor : toList segmentsNew))

-- | For each set get the elements unique to that set, also return the elements
-- present in more than one set.
uniques :: Ord a => Vector (Set.Set a) -> (Vector (Set.Set a), Set.Set a)
uniques xs =
  let -- don't use prescanl because the last element is used in computing the
      -- shared elements later
      increasing = V.scanl' Set.union Set.empty xs
      decreasing = V.prescanr' Set.union Set.empty xs
      us = V.zipWith3 (\x i d -> (x `Set.difference` i) `Set.difference` d)
                      xs
                      increasing
                      decreasing
      shared = V.last increasing `Set.difference` Set.unions (toList us)
  in  (us, shared)

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
