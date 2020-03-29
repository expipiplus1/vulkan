{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Write.Segment
  where

import           Data.Functor
import           Prelude
import           Data.Text                      ( Text )
import           Control.Monad
import qualified Data.Map                      as Map
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as Set
import           Polysemy
import           Data.Foldable           hiding ( find )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.List                      ( sortOn )
import           Algebra.Graph.AdjacencyIntMap

import           Error


data SegmentSeed i n = SegmentSeed i (Vector n)
  deriving (Functor, Foldable, Traversable)

-- | A group which will take priority over the subsequent groups
data SegmentGroup i n = SegmentGroup i (Vector (SegmentSeed i n))
  deriving (Functor, Foldable, Traversable)

data Segment i a = Segment i (Vector a)
  deriving (Functor, Foldable, Traversable, Show)

data SegmentedGroup i a = SegmentedGroup
  { groupSegments :: Vector (Segment i a)
  , groupExtras   :: Segment i a
  }
  deriving (Functor, Foldable, Traversable, Show)

-- - Find the transitive closure of the graph
-- - Partition any vertices which are reachable from just one seed
-- - Return these partitions along with the leftovers
segmentGraph
  :: forall r a i n
   . (HasErr r, Ord n, Show n)
  => (a -> Text)
  -> (n -> Text)
  -> (i -> Text)
  -> (a -> [n])
  -> (a -> [n])
  -> Vector a
  -> Vector (SegmentGroup i n)
  -> Sem r (Vector (SegmentedGroup i a))
segmentGraph debugVertex debugName debugIndex getNames getWants xs groups = do
  let numbered = zip (toList xs) [0 :: Int ..]
      unNumber = (xs V.!)
      allNames = sortOn fst [ (n, a) | (x, a) <- numbered, n <- getNames x ]
  -- There are duplicated requirements in the spec sadly
  -- assertUnique (debugVertex . (V.!) xs) debugName allNames
  -- let nameMap = Map.fromDistinctAscList allNames
  let nameMap = Map.fromAscList allNames
      locate :: n -> Maybe Int
      locate = (`Map.lookup` nameMap)
      find n = case locate n of
        Nothing -> throw ("unable to find " <> debugName n <> " in any vertex")
        Just i  -> pure i
  es <- concat <$> forV
    numbered
    (\(x, n) -> context (debugVertex x) $ forV (getWants x) (fmap (n, ) . find))

  let relation = vertices (snd <$> numbered) `overlay` edges es
  groupIndices <- traverseVGroupsSeeds
    (\(SegmentSeed i n) ->
      context (debugIndex i) $ SegmentSeed i <$> traverseV find n
    )

    groups
  let
    closed = closure relation
    -- For each group, for each segment in that group, get the set of
    -- elements
    closedSeeds :: Vector (i, Vector (i, IntSet))
    closedSeeds = groupIndices <&> \(SegmentGroup groupName g) ->
      ( groupName
      , g <&> \(SegmentSeed i s) ->
        (i, Set.unions ((`postIntSet` closed) <$> s))
      )
    us = uniqueGroups closedSeeds
  pure (fmap unNumber <$> us)

-- | For each group assign the elements to their unique segment, or leave in
-- leftovers.
uniqueGroups
  :: forall i . Vector (i, Vector (i, IntSet)) -> Vector (SegmentedGroup i Int)
uniqueGroups xs = fst <$> V.postscanl' go (error "unused", mempty) xs
 where
  go
    :: (SegmentedGroup i Int, IntSet)
     --- ^ The last group and the a's spoken for so far
    -> (i, Vector (i, IntSet))
     --- ^ This group's segments
    -> (SegmentedGroup i Int, IntSet)
     --- ^ This group and the a's spoken for so far
  go (_, spokenFor) (groupName, segments) =
    let --- | Only new elements
        (us, extra, spokenFor') = uniques spokenFor (snd <$> segments)
        toSegment i v = Segment i (V.fromList (Set.toList v))
        newSegment = SegmentedGroup
          (V.zipWith toSegment (fst <$> segments) us)
          (toSegment groupName extra)
    in  (newSegment, spokenFor')

-- | For each set get the elements unique to that set, also return the elements
-- present in more than one set.
uniques
  :: IntSet
  -- ^ To ignore
  -> Vector IntSet
  -> (Vector IntSet, IntSet, IntSet)
uniques ignored xs =
  let -- don't use prescanl because the last element is used in computing the
      -- shared elements later
      increasing = V.scanl' Set.union ignored xs
      decreasing = V.prescanr' Set.union Set.empty xs
      us = V.zipWith3 (\x i d -> (x `Set.difference` i) `Set.difference` d)
                      xs
                      increasing
                      decreasing
      shared =
          V.last increasing
            `Set.difference` Set.unions (toList us)
            `Set.difference` ignored
  in  (us, shared, V.last increasing)

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

traverseVGroupsSeeds
  :: (HasErr r, Traversable t)
  => (SegmentSeed i a -> Sem r (SegmentSeed i b))
  -> t (SegmentGroup i a)
  -> Sem r (t (SegmentGroup i b))
traverseVGroupsSeeds f =
  traverseV (\(SegmentGroup i ss) -> SegmentGroup i <$> traverseV f ss)
