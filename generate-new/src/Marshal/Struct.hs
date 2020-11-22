module Marshal.Struct
  ( marshalStruct
  , marshalStructAndUnions
  , MarshaledStruct(..)
  , MarshaledStructMember(..)
  ) where

import           Algebra.Graph.AdjacencyIntMap
                                         hiding ( empty )
import           Algebra.Graph.AdjacencyIntMap.Algorithm
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Polysemy
import           Polysemy.Fail
import           Polysemy.Input
import           Polysemy.NonDet
import           Relude

import           CType                          ( CType
                                                  ( Array
                                                  , Bitfield
                                                  , TypeName
                                                  )
                                                )
import qualified Data.HashMap.Strict           as Map
import           Error
import           Marshal.Scheme
import           Render.SpecInfo
import           Spec.Parse
import qualified Data.Text as T

data MarshaledStruct t = MarshaledStruct
  { msName    :: CName
  , msStruct  :: StructOrUnion t 'WithSize 'WithChildren
  , msMembers :: V.Vector MarshaledStructMember
  }

data MarshaledStructMember = MarshaledStructMember
  { msmStructMember :: StructMember
  , msmScheme       :: MarshalScheme StructMember
  }
  deriving Show

marshalStruct
  :: (HasMarshalParams r, HasErr r, HasSpecInfo r)
  => StructOrUnion t 'WithSize 'WithChildren
  -> Sem r (MarshaledStruct t)
marshalStruct s@Struct {..} = contextShow sName $ do
  let msName   = sName
      msStruct = s
  msMembers <- forV sMembers $ \sm -> contextShow (smName sm) $ do
    scheme <- structMemberScheme s sm
    pure $ MarshaledStructMember sm scheme
  pure MarshaledStruct { .. }

structMemberScheme
  :: (HasMarshalParams r, HasErr r, HasSpecInfo r)
  => StructOrUnion t 'WithSize 'WithChildren
  -> StructMember
  -> Sem r (MarshalScheme StructMember)
structMemberScheme Struct {..} member = do
  MarshalParams {..} <- input
  let
    schemes =
      [ maybe empty pure . getBespokeScheme sName
      , -- These two are for value constrained params:
        univaluedScheme
      , lengthScheme sMembers
        -- Pointers to Void have some special handling
      , voidPointerScheme
        -- Pointers to return values in, unmarshaled at the moment
      , returnPointerInStructScheme
        -- Optional and non optional arrays
      , arrayScheme WrapExtensibleStructs DoNotWrapDispatchableHandles sMembers
      , fixedArrayScheme WrapExtensibleStructs DoNotWrapDispatchableHandles
        -- Optional things:
      , optionalDefaultScheme WrapExtensibleStructs DoNotWrapDispatchableHandles
      , optionalScheme WrapExtensibleStructs DoNotWrapDispatchableHandles
        -- Structs which can be extended, so need to be wrapped in a GADT
      , extensibleStruct
        -- Structs don't have wrapped handles because it's annoying to pass
        -- the command record into the peek functions
      , rawDispatchableHandles
        -- Everything left over is treated as a boring scalar parameter
      , scalarScheme
      ]
  m <- runNonDet . failToNonDet . asum . fmap ($ member) $ schemes
  case m of
    Just x  -> pure x
    Nothing -> throw
      ("Not handled by any marshaling scheme. Type: " <> show (smType member))


----------------------------------------------------------------
-- Marshaling structs and unions sortedly
----------------------------------------------------------------

marshalStructAndUnions
  :: (HasErr r, HasMarshalParams r, HasSpecInfo r)
  => Vector Struct
  -> Vector Union
  -> Sem
       r
       ( Vector (MarshaledStruct AStruct)
       , Vector (MarshaledStruct AUnion)
       )
marshalStructAndUnions ss us = V.partitionWith id <$> traverseInTopOrder
  (either sName sName)
  (either immediateDepends immediateDepends)
  (either (fmap Left . marshalStruct) (fmap Right . marshalStruct))
  ((Left <$> ss) <> (Right <$> us))

immediateDepends :: StructOrUnion t s c -> [CName]
immediateDepends Struct {..} =
  [ n
  | StructMember {..} <- V.toList sMembers
  , Just n            <- pure $ immediateDepend smType
  ]
 where
  immediateDepend = \case
    TypeName n   -> Just n
    Bitfield t _ -> immediateDepend t
    Array _ _ t  -> immediateDepend t
    _            -> Nothing

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
inversePermutation indices = traverse (\x -> V.findIndex (x ==) indices)
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
      keymap = Map.fromList (zip (V.toList keyed) [0 ..])
      lookupIndex :: k -> Maybe Int
      lookupIndex = (`Map.lookup` keymap)
      graph       = stars
        [ (i, ps)
        | (a, i) <- zip (V.toList as) [0 ..]
        , let ks = getPostSet a
        , -- TODO: error on Nothing here
          let ps = catMaybes (lookupIndex <$> ks)
        ]
  case topSort graph of
    Left is ->
      throw
        $  "Cycle found in direct dependencies of structs: "
        <> (T.pack . show) (V.backpermute keyed (V.fromList (toList is)))
    Right is -> pure (V.fromList is)
