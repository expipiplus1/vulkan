module Data.Vector.TopTraverse
  ( traverseInTopOrder
  ) where

import           Algebra.Graph.AdjacencyIntMap
                                         hiding ( empty )
import           Algebra.Graph.AdjacencyIntMap.Algorithm
import qualified Data.Foldable                 as F
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe
import qualified Data.Text                     as T
import           Data.Vector
import qualified Data.Vector                   as V
import           Error                          ( HasErr
                                                , throw
                                                , traverseV
                                                )
import           Polysemy
import           Prelude
import           Relude                         ( Hashable )

traverseInTopOrder
  :: (Eq k, Hashable k, HasErr r, Show k)
  => (a -> k)
  -> (a -> [k])
  -> (a -> Sem r b)
  -> Vector a
  -> Sem r (Vector b)
traverseInTopOrder getKey getPostSet f xs = do
  perm      <- topoSortedPermutation getKey getPostSet xs
  traversed <- traverseV f (V.backpermute xs perm)
  inv       <- case inversePermutation perm of
    Nothing -> throw "impossible: Unable to find inverse permutation"
    Just i  -> pure i
  pure $ V.backpermute traversed inv

inversePermutation :: Vector Int -> Maybe (Vector Int)
inversePermutation indices = traverse
  (\x -> V.findIndex (x ==) indices)
  (V.fromList [0 .. V.length indices - 1])

topoSortedPermutation
  :: forall a k r
   . (Eq k, Hashable k, HasErr r, Show k)
  => (a -> k)
  -> (a -> [k])
  -> Vector a
  -> Sem r (Vector Int)
topoSortedPermutation getKey getPostSet as = do
  let keyed  = getKey <$> as
      keymap = Map.fromList (Prelude.zip (V.toList keyed) [0 ..])
      lookupIndex :: k -> Maybe Int
      lookupIndex = (`Map.lookup` keymap)
      graph       = stars
        [ (i, ps)
        | (a, i) <- Prelude.zip (V.toList as) [0 ..]
        , let ks = getPostSet a
        , -- TODO: error on Nothing here
          let ps = catMaybes (lookupIndex <$> ks)
        ]
  case topSort graph of
    Left is ->
      throw
        $  "Cycle found in direct dependencies of structs: "
        <> (T.pack . show) (V.backpermute keyed (V.fromList (F.toList is)))
    Right is -> pure (V.reverse . V.fromList $ is)
