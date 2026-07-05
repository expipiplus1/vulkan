{-# LANGUAGE NoFieldSelectors #-}

{-| A self-contained, reversible description of a block's memory layout, plus a
value-level unifier over those layouts.

This is the IR the reflection-driven type machinery is built on. Two levels:

  * 'Layout' \/ 'Member' \/ 'FieldType' — the /structured/ form. It keeps every
    member's name, byte offset, and full semantic shape (scalar kind, vector \/
    matrix dimensions, array sizes, nested structs), so it carries enough
    information to drive code generation /and/ to be run backwards (e.g. towards
    authoring SPIR-V), not merely the flattened offsets.

  * 'OffsetMap' — the /normalized/ form: the flattened offset table of occupied
    scalar 'Slot's (each a byte offset + scalar type), plus an optional
    runtime-array tail. This is exactly the offset map the GPU sees, so it is the
    canonical form for /layout equivalence/: @mat4@, @vec4[4]@ and @float[16]@
    reduce to the same std430 offset map (and correctly diverge under std140,
    where the array strides round to 16). This is structural/layout equivalence —
    identical normalized bytes — not memory aliasing in the SPIR-V @Aliased@ \/ C
    pointer sense.

'unify' is the compatibility relation: not equality and not a prefix match, but
unification. Two layouts unify when their offset maps can be reconciled (a runtime
tail absorbs a concrete repeat, pinning its length); the result is the
most-defined offset map, which can be folded against further layouts ('foldUnify')
to accumulate a combined picture across pipelines. A failure is reported as a
'Mismatch' with a legible 'renderMismatch'.
-}
module Vulkan.Utils.SpirV.Layout
  ( -- * Layout mode
    layoutForDescriptor

    -- * Structured layout
  , Layout (..)
  , Member (..)
  , FieldType (..)
  , ArraySize (..)
  , layoutOf
  , fromFields
  , leafFieldType

    -- * Normalized offset map
  , Slot (..)
  , OffsetMap (..)
  , RuntimeTail (..)
  , normalize
  , offsetMapOf

    -- * Unification
  , Mismatch (..)
  , renderMismatch
  , unify
  , foldUnify
  , mergeKeyed
  ) where

import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Graphics.Gl.Block (roundUp)

import Data.SpirV.Reflect.Enums.DescriptorType qualified as R
import Data.SpirV.Reflect.TypeDescription (TypeDescription)
import Data.SpirV.Reflect.TypeDescription qualified

import Vulkan.Utils.SpirV.Types (LayoutMode (..), MemberShape (..), NumericType (..), ScalarType (..), arrayBaseAlign, arrayDims, classifyType, isBdaPointer, leafAlignment, scalarWidth, structBaseAlign)

{- | The gl-block layout a descriptor's block follows: uniform buffers use
std140, storage buffers std430. Other descriptor types carry no block layout
('Nothing').
-}
layoutForDescriptor :: R.DescriptorType -> Maybe LayoutMode
layoutForDescriptor = \case
  R.DESCRIPTOR_TYPE_UNIFORM_BUFFER -> Just Std140Layout
  R.DESCRIPTOR_TYPE_STORAGE_BUFFER -> Just Std430Layout
  _ -> Nothing

{- | A struct's layout under a given 'LayoutMode': its members at their resolved
byte offsets, total size and base alignment. 'size' is 'Nothing' when the struct
ends in a runtime-sized array (its size is not known statically).
-}
data Layout = Layout
  { name :: Maybe Text
  , mode :: LayoutMode
  , members :: [Member]
  , size :: Maybe Int
  , align :: Int
  }
  deriving (Eq, Show)

-- | A struct member: its name, byte offset within the struct, and structured type.
data Member = Member
  { name :: Text
  , offset :: Int
  , type' :: FieldType
  }
  deriving (Eq, Show)

{- | The structured type of a member. Multi-dimensional arrays nest
(@'ArrayOf' a ('ArrayOf' b t)@, outermost dimension first).
-}
data FieldType
  = Scalar ScalarType
  | -- | component count (2..4)
    Vector ScalarType Int
  | -- | columns, rows
    Matrix ScalarType Int Int
  | ArrayOf ArraySize FieldType
  | Struct Layout
  deriving (Eq, Show)

-- | An array dimension: a fixed length, or a runtime (unsized) trailing array.
data ArraySize = Sized Int | Runtime
  deriving (Eq, Show)

-- Layout arithmetic (std140/std430). --------------------------------------------

{- | @(base alignment, size)@ of a field type; size is 'Nothing' for a runtime
array (open).
-}
fieldExtent :: LayoutMode -> FieldType -> (Int, Maybe Int)
fieldExtent mode = \case
  Scalar s -> (leafAlignment mode s ShScalar, Just (scalarWidth s))
  Vector s n -> (leafAlignment mode s (ShVector n), Just (n * scalarWidth s))
  Matrix s c r ->
    let colStride = leafAlignment mode s (ShMatrix c r)
    in (colStride, Just (c * colStride))
  ArrayOf Runtime e ->
    let (ea, _) = fieldExtent mode e
    in (arrayBaseAlign mode ea, Nothing)
  ArrayOf (Sized n) e ->
    let
      (ea, es) = fieldExtent mode e
      align = arrayBaseAlign mode ea
      stride = roundUp (fromMaybe 0 es) align
    in
      (align, Just (n * stride))
  Struct l -> (l.align, l.size)

{- | The stride between elements of an array of @e@ under the layout. Defined only
when @e@ has a static size (i.e. is not itself a runtime array).
-}
arrayStride :: LayoutMode -> FieldType -> Int
arrayStride mode e =
  let (ea, es) = fieldExtent mode e
  in roundUp (fromMaybe 0 es) (arrayBaseAlign mode ea)

-- Building from reflection. -----------------------------------------------------

-- | Compute the layout of a reflected @OpTypeStruct@ under the given mode.
layoutOf :: LayoutMode -> TypeDescription -> Either String Layout
layoutOf mode td = do
  members <- traverse (memberOf mode) (V.toList td.members)
  pure (fromFields mode (structName td) members)

{- | Build a layout from named fields directly, computing each member's offset
and the struct's size and alignment (no reflection). Besides constructing
expected layouts, this is the direction an authoring front end would use.
-}
fromFields :: LayoutMode -> Maybe Text -> [(Text, FieldType)] -> Layout
fromFields mode name members =
  Layout
    { name
    , mode
    , members = [m | (m, _, _) <- placed]
    , size = if openEnded then Nothing else Just (roundUp end baseAlign)
    , align = baseAlign
    }
  where
    placed = place mode members
    baseAlign = structBaseAlign mode [a | (_, a, _) <- placed]
    openEnded = any (\(_, _, sz) -> sz == Nothing) placed
    end = case reverse placed of
      [] -> 0
      ((Member _ off _, _, msz) : _) -> maybe off (off +) msz

{- | Place members in declaration order, returning each at its offset together
with its alignment and (maybe-open) size.
-}
place :: LayoutMode -> [(Text, FieldType)] -> [(Member, Int, Maybe Int)]
place mode = go 0
  where
    go _ [] = []
    go cursor ((nm, ft) : rest) =
      let
        (align, msz) = fieldExtent mode ft
        off = roundUp cursor align
        cursor' = maybe off (off +) msz
      in
        (Member nm off ft, align, msz) : go cursor' rest

-- | The structured type of a single reflected member.
memberOf :: LayoutMode -> TypeDescription -> Either String (Text, FieldType)
memberOf mode mem = do
  nm <- maybe (Left "member without a name") Right mem.struct_member_name
  base <- baseFieldType mode mem
  let
    dims = arrayDims mem
    wrap d ft = ArrayOf (if d == 0 then Runtime else Sized (fromIntegral d)) ft
  pure (nm, foldr wrap base dims)

{- | The element type of a member, ignoring array dimensions: a leaf
(scalar\/vector\/matrix) or a nested struct.
-}
baseFieldType :: LayoutMode -> TypeDescription -> Either String FieldType
baseFieldType mode mem
  -- A buffer_reference pointer is an 8-byte device address, not the pointee
  -- inline; the REF check precedes 'classifyType' because the pointer also
  -- carries the pointee's leaf flags (e.g. @REF INT@).
  | isBdaPointer mem = Right (Scalar STAddress)
  | otherwise =
      case classifyType mem of
        Just numeric -> Right (leafFieldType numeric)
        Nothing -> Struct <$> layoutOf mode (fromMaybe mem mem.struct_type_description)

leafFieldType :: NumericType -> FieldType
leafFieldType NumericType{scalar, shape} = case shape of
  ShScalar -> Scalar scalar
  ShVector n -> Vector scalar n
  ShMatrix c r -> Matrix scalar c r

-- Normalization to the scalar offset map. ---------------------------------------------

-- | One occupied scalar component at a byte offset.
data Slot = Slot
  { offset :: Int
  , scalar :: ScalarType
  }
  deriving (Eq, Ord, Show)

{- | A runtime-sized array tail: a repeating element offset map starting at 'base'
with the given stride. The element 'Slot's are relative to the element start.
-}
data RuntimeTail = RuntimeTail
  { base :: Int
  , stride :: Int
  , element :: [Slot]
  }
  deriving (Eq, Show)

{- | A normalized layout: the concrete occupied scalar slots (sorted by offset),
the total size (if statically known), and an optional runtime-array tail.
-}
data OffsetMap = OffsetMap
  { slots :: [Slot]
  , size :: Maybe Int
  , tail :: Maybe RuntimeTail
  }
  deriving (Eq, Show)

-- | Flatten a structured 'Layout' to its offset map.
normalize :: Layout -> OffsetMap
normalize l =
  OffsetMap
    { slots = sortOn (.offset) (concatMap fst pieces)
    , size = l.size
    , tail = listToMaybe (mapMaybe snd pieces)
    }
  where
    mode = l.mode
    pieces = map (flattenMember mode) l.members

-- | Build the offset map straight from reflection.
offsetMapOf :: LayoutMode -> TypeDescription -> Either String OffsetMap
offsetMapOf mode = fmap normalize . layoutOf mode

flattenMember :: LayoutMode -> Member -> ([Slot], Maybe RuntimeTail)
flattenMember mode (Member _ off ft) = case ft of
  ArrayOf Runtime e ->
    ([], Just (RuntimeTail off (arrayStride mode e) (flattenAt mode 0 e)))
  _ -> (flattenAt mode off ft, Nothing)

-- | The scalar slots of a (statically-sized) field type at a base offset.
flattenAt :: LayoutMode -> Int -> FieldType -> [Slot]
flattenAt mode off = \case
  Scalar s -> [Slot off s]
  Vector s n -> let w = scalarWidth s in [Slot (off + i * w) s | i <- [0 .. n - 1]]
  Matrix s c r ->
    let
      w = scalarWidth s
      colStride = leafAlignment mode s (ShMatrix c r)
    in
      [Slot (off + col * colStride + row * w) s | col <- [0 .. c - 1], row <- [0 .. r - 1]]
  ArrayOf (Sized n) e ->
    let stride = arrayStride mode e
    in concat [flattenAt mode (off + i * stride) e | i <- [0 .. n - 1]]
  ArrayOf Runtime _ -> [] -- a runtime array can only be a struct's last member
  Struct l -> concatMap (\(Member _ mo mft) -> flattenAt mode (off + mo) mft) l.members

-- Unification. ------------------------------------------------------------------

-- | Why two offset maps fail to unify.
data Mismatch
  = -- | differing scalar at a byte offset
    SlotMismatch Int ScalarType ScalarType
  | -- | differing number of occupied slots
    CountMismatch Int Int
  | -- | differing total size
    SizeMismatch Int Int
  | -- | incompatible runtime-array tails
    TailMismatch String
  deriving (Eq, Show)

renderMismatch :: Mismatch -> String
renderMismatch = \case
  SlotMismatch off l r ->
    "scalar mismatch at byte offset "
      <> show off
      <> ": "
      <> showScalar l
      <> " vs "
      <> showScalar r
  CountMismatch l r ->
    "different number of components: " <> show l <> " vs " <> show r
  SizeMismatch l r ->
    "different total size: " <> show l <> " vs " <> show r <> " bytes"
  TailMismatch msg ->
    "incompatible runtime arrays: " <> msg
  where
    showScalar STFloat = "float"
    showScalar STDouble = "double"
    showScalar STInt = "int"
    showScalar STUInt = "uint"
    showScalar STInt64 = "int64"
    showScalar STUInt64 = "uint64"
    showScalar STBool = "bool"
    showScalar STAddress = "address"

{- | Unify two offset maps into their most-defined common offset map, or report the first
'Mismatch'. A runtime tail on one side absorbs a matching concrete repeat on
the other (pinning the array length); two closed offset maps must coincide exactly.
-}
unify :: OffsetMap -> OffsetMap -> Either Mismatch OffsetMap
unify a b = case (a.tail, b.tail) of
  (Nothing, Nothing) -> do
    matchSlots a.slots b.slots
    matchSize a.size b.size
    pure a
  (Just _, Nothing) -> absorb a b
  (Nothing, Just _) -> absorb b a
  (Just ta, Just tb) -> do
    matchSlots a.slots b.slots
    if ta == tb
      then pure a
      else Left (TailMismatch "tails differ in stride, element shape or base offset")

{- | Reconcile an open offset map (with a runtime tail) against a closed one, requiring
the closed offset map to be the open offset map's prefix followed by whole repeats of its
tail element. Returns the closed (pinned) offset map.
-}
absorb :: OffsetMap -> OffsetMap -> Either Mismatch OffsetMap
absorb open closed = do
  let
    pre = open.slots
    n = length pre
  matchSlots pre (take n closed.slots)
  let
    tl = fromMaybe (error "absorb: open offset map has no tail") open.tail
    rest = drop n closed.slots
  matchRepeats tl rest
  pure closed

-- | The remaining slots must be zero or more whole repeats of the tail element.
matchRepeats :: RuntimeTail -> [Slot] -> Either Mismatch ()
matchRepeats tl = go 0
  where
    el = tl.element
    m = length el
    go _ [] = Right ()
    go j rest
      | length chunk < m =
          Left (TailMismatch "trailing bytes are not a whole array element")
      | otherwise = do
          matchSlots (shift (tl.base + j * tl.stride) el) chunk
          go (j + 1) more
      where
        (chunk, more) = splitAt m rest
    shift base = map (\(Slot o s) -> Slot (o + base) s)

matchSlots :: [Slot] -> [Slot] -> Either Mismatch ()
matchSlots xs ys
  | length xs /= length ys = Left (CountMismatch (length xs) (length ys))
  | otherwise =
      case [ (x.offset, x.scalar, y.scalar)
           | (x, y) <- zip xs ys
           , x.scalar /= y.scalar || x.offset /= y.offset
           ] of
        ((off, l, r) : _) -> Left (SlotMismatch off l r)
        [] -> Right ()

matchSize :: Maybe Int -> Maybe Int -> Either Mismatch ()
matchSize (Just l) (Just r) | l /= r = Left (SizeMismatch l r)
matchSize _ _ = Right ()

{- | Fold a layout against further layouts, accumulating the combined offset map (the
running most-general unifier). Use to add pipelines into a shared layout.
-}
foldUnify :: OffsetMap -> [OffsetMap] -> Either Mismatch OffsetMap
foldUnify = foldl' (\acc g -> acc >>= (`unify` g)) . Right

{- | Merge layouts tagged by a key (e.g. a @(set, binding)@ or vertex @location@):
entries sharing a key must unify, disjoint keys union in. Reports the offending
key with its 'Mismatch'.
-}
mergeKeyed :: (Ord k) => [(k, OffsetMap)] -> Either (k, Mismatch) (Map k OffsetMap)
mergeKeyed = foldl' step (Right Map.empty)
  where
    step acc (k, g) = do
      m <- acc
      case Map.lookup k m of
        Nothing -> Right (Map.insert k g m)
        Just g' -> case unify g' g of
          Right u -> Right (Map.insert k u m)
          Left e -> Left (k, e)

-- | The struct's @type_name@, treating an empty name as absent.
structName :: TypeDescription -> Maybe Text
structName td = td.type_name >>= \t -> if Text.null t then Nothing else Just t
