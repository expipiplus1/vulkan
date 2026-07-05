{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-| Type-level layout signatures, tied to the records reflection generates.

Each generated block record carries a ground 'SigOffsetMap' — the promotion of its
normalized 'OffsetMap' ("Vulkan.Utils.SpirV.Layout"), computed value-level at splice
time (the value-level layout is the oracle; see 'knownLayoutInstance'). Because
the offset map is /normalized/ before promotion, layout-equivalent layouts —
@mat4@, @vec4[4]@, @float[16]@ under std430 — promote to the /same/ 'SigOffsetMap',
so the type-level matcher 'Fits' accepts them with no special cases.

'Fits' is intentionally thin: a use-site /matcher/ against an already-ground
signature, not a unifier (the heavy unification stays value-level in @Q@). The
ground offset map can be reflected back to a value with 'layoutSig' \/
'sigOffsetMapVal', which is how the type level is validated against the value-level
unifier.
-}
module Vulkan.Utils.SpirV.Signature
  ( -- * Type-level offset map
    SigOffsetMap (..)
  , SigSlot (..)
  , SigTail (..)

    -- * Records carrying a signature
  , KnownLayout (..)
  , layoutSig

    -- * Reflecting a signature back to a value
  , KnownSigOffsetMap (..)
  , KnownArrayTail (..)
  , KnownMaybeSig (..)
  , KnownKeyedSigs (..)

    -- * Matching
  , Fits
  , FitsTail
  , ArrayOf

    -- * Generation (Template Haskell)
  , knownLayoutInstance
  , promoteOffsetMap
  , promoteList
  , promoteMaybe
  , natT
  ) where

import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import Data.Word (Word32)
import GHC.TypeLits (ErrorMessage (..), KnownNat, Nat, TypeError, natVal)
import Language.Haskell.TH

import Vulkan.Utils.SpirV.Layout (OffsetMap (..), RuntimeTail (..), Slot (..))
import Vulkan.Utils.SpirV.Types (ScalarType (..))

-- | One occupied scalar component at a byte offset (type-level mirror of 'Slot').
data SigSlot = SigSlot Nat ScalarType

{- | A runtime-array tail (mirror of 'RuntimeTail'): base offset, stride, and the
element's slots (relative to the element start).
-}
data SigTail = SigTail Nat Nat [SigSlot]

{- | A normalized layout at the type level (mirror of 'OffsetMap'): concrete slots, the
total size if statically known, and an optional runtime-array tail.
-}
data SigOffsetMap = SigOffsetMap [SigSlot] (Maybe Nat) (Maybe SigTail)

{- | A Haskell type with a ground, reflection-derived layout signature. Generated
records get an instance via 'knownLayoutInstance'.
-}
class (KnownSigOffsetMap (Sig a)) => KnownLayout a where
  type Sig a :: SigOffsetMap

-- | The value-level normal form of a record's signature (the oracle view).
layoutSig :: forall a. (KnownLayout a) => OffsetMap
layoutSig = sigOffsetMapVal (Proxy @(Sig a))

-- Reflection: type-level offset map -> value 'OffsetMap'. ----------------------------------

class KnownScalar (s :: ScalarType) where
  scalarVal :: Proxy s -> ScalarType

instance KnownScalar 'STFloat where scalarVal _ = STFloat
instance KnownScalar 'STDouble where scalarVal _ = STDouble
instance KnownScalar 'STInt where scalarVal _ = STInt
instance KnownScalar 'STUInt where scalarVal _ = STUInt
instance KnownScalar 'STInt64 where scalarVal _ = STInt64
instance KnownScalar 'STUInt64 where scalarVal _ = STUInt64
instance KnownScalar 'STBool where scalarVal _ = STBool
instance KnownScalar 'STAddress where scalarVal _ = STAddress

class KnownSlots (xs :: [SigSlot]) where
  slotsVal :: Proxy xs -> [Slot]

instance KnownSlots '[] where
  slotsVal _ = []

instance (KnownNat o, KnownScalar s, KnownSlots rest) => KnownSlots ('SigSlot o s ': rest) where
  slotsVal _ =
    Slot (fromIntegral (natVal (Proxy @o))) (scalarVal (Proxy @s)) : slotsVal (Proxy @rest)

class KnownMaybeNat (m :: Maybe Nat) where
  maybeNatVal :: Proxy m -> Maybe Int

instance KnownMaybeNat 'Nothing where
  maybeNatVal _ = Nothing

instance (KnownNat n) => KnownMaybeNat ('Just n) where
  maybeNatVal _ = Just (fromIntegral (natVal (Proxy @n)))

class KnownTail (t :: SigTail) where
  tailVal :: Proxy t -> RuntimeTail

instance (KnownNat b, KnownNat s, KnownSlots es) => KnownTail ('SigTail b s es) where
  tailVal _ =
    RuntimeTail
      (fromIntegral (natVal (Proxy @b)))
      (fromIntegral (natVal (Proxy @s)))
      (slotsVal (Proxy @es))

class KnownMaybeTail (m :: Maybe SigTail) where
  maybeTailVal :: Proxy m -> Maybe RuntimeTail

instance KnownMaybeTail 'Nothing where
  maybeTailVal _ = Nothing

instance (KnownTail t) => KnownMaybeTail ('Just t) where
  maybeTailVal _ = Just (tailVal (Proxy @t))

-- | Reflect a ground type-level offset map back to its value form.
class KnownSigOffsetMap (g :: SigOffsetMap) where
  sigOffsetMapVal :: Proxy g -> OffsetMap

instance (KnownSlots ss, KnownMaybeNat sz, KnownMaybeTail tl) => KnownSigOffsetMap ('SigOffsetMap ss sz tl) where
  sigOffsetMapVal _ =
    OffsetMap (slotsVal (Proxy @ss)) (maybeNatVal (Proxy @sz)) (maybeTailVal (Proxy @tl))

{- | The @(base offset, element stride)@ of a buffer signature's runtime-array
tail, recovered straight from the type level. Only a layout that /has/ a tail has
an instance, so reading it is total — and unlike reflecting the whole 'OffsetMap' and
inspecting its 'Maybe' tail ('sigOffsetMapVal'), it forces only the two 'Nat's it
needs, not the element's slot list. So an element loop pays @O(1)@ per element,
not @O(offset map)@.
-}
class KnownArrayTail (t :: SigOffsetMap) where
  arrayTailBaseStride :: (Int, Int)

instance (KnownNat base, KnownNat stride) => KnownArrayTail ('SigOffsetMap cs csz ('Just ('SigTail base stride es))) where
  arrayTailBaseStride =
    (fromIntegral (natVal (Proxy @base)), fromIntegral (natVal (Proxy @stride)))

-- | Reflect an optional ground offset map (e.g. a push-constant layout) back to a value.
class KnownMaybeSig (m :: Maybe SigOffsetMap) where
  maybeSigVal :: Proxy m -> Maybe OffsetMap

instance KnownMaybeSig 'Nothing where
  maybeSigVal _ = Nothing

instance (KnownSigOffsetMap g) => KnownMaybeSig ('Just g) where
  maybeSigVal _ = Just (sigOffsetMapVal (Proxy @g))

-- | Reflect a list of @((set, binding), offset map)@ entries back to values.
class KnownKeyedSigs (xs :: [((Nat, Nat), SigOffsetMap)]) where
  keyedSigsVal :: Proxy xs -> [((Word32, Word32), OffsetMap)]

instance KnownKeyedSigs '[] where
  keyedSigsVal _ = []

instance
  (KnownNat s, KnownNat b, KnownSigOffsetMap g, KnownKeyedSigs rest)
  => KnownKeyedSigs ('( '(s, b), g) ': rest)
  where
  keyedSigsVal _ =
    ((nat32 (Proxy @s), nat32 (Proxy @b)), sigOffsetMapVal (Proxy @g))
      : keyedSigsVal (Proxy @rest)

nat32 :: (KnownNat n) => Proxy n -> Word32
nat32 = fromIntegral . natVal

-- Matching. ---------------------------------------------------------------------

{- | The use-site matcher: @'Fits' r t@ holds when a value of layout @r@ may be
viewed at a slot of layout @t@. Since both are normalized before promotion,
layout-equivalent layouts share a signature and match by equality; a difference is
a legible compile error. (Runtime-tail absorption is handled value-level when a
buffer's signature is built; this matcher covers the ground case.)
-}
type family Fits (r :: SigOffsetMap) (t :: SigOffsetMap) :: Constraint where
  Fits g g = ()
  Fits r t =
    TypeError
      ( 'Text "Layout mismatch."
          ':$$: 'Text "  have: "
          ':<>: 'ShowType r
          ':$$: 'Text "  want: "
          ':<>: 'ShowType t
      )

{- | The signature of a tightly-packed runtime array @r[]@ — an SSBO whose content
is a runtime array of the element layout @r@. The element becomes the array
tail: its (statically known) size is the stride, the base offset is 0, and the
closed slots clear. Use to tag a mapped SSBO pointer, e.g.
@'Vulkan.Utils.SpirV.Buffer.unsafeAsBufferPtr' p :: io (Buffer ('ArrayOf' ('Sig' Vertex)))@.
(The element's std430 standalone size equals its array stride, so this is exact
for a tightly-packed @T[]@.)
-}
type family ArrayOf (r :: SigOffsetMap) :: SigOffsetMap where
  ArrayOf ('SigOffsetMap slots ('Just size) 'Nothing) =
    'SigOffsetMap '[] 'Nothing ('Just ('SigTail 0 size slots))
  ArrayOf r =
    TypeError
      ( 'Text "ArrayOf: the element layout must be statically sized and not itself open:"
          ':$$: 'Text "  "
          ':<>: 'ShowType r
      )

{- | @'FitsTail' r t@ holds when a record of layout @r@ is one element of the
runtime-array tail of buffer layout @t@: @r@ is closed with a known size equal
to the tail stride, and its slots are exactly the tail element's. Layout-equivalent
element records share a signature, so they match too; a mismatch (or a @t@
without a tail) is a legible compile error.
-}
type family FitsTail (r :: SigOffsetMap) (t :: SigOffsetMap) :: Constraint where
  FitsTail ('SigOffsetMap es ('Just sz) 'Nothing) ('SigOffsetMap _ _ ('Just ('SigTail _ sz es))) = ()
  FitsTail r t =
    TypeError
      ( 'Text "Element layout does not fit the buffer's array tail."
          ':$$: 'Text "  element: "
          ':<>: 'ShowType r
          ':$$: 'Text "  buffer:  "
          ':<>: 'ShowType t
      )

-- Generation. -------------------------------------------------------------------

{- | Emit @instance 'KnownLayout' <name> where type 'Sig' <name> = <promoted
offset map>@, promoting the value-level normal form to the type level.
-}
knownLayoutInstance :: Name -> OffsetMap -> [Dec]
knownLayoutInstance name om =
  [ InstanceD
      Nothing
      []
      (ConT ''KnownLayout `AppT` ConT name)
      [TySynInstD (TySynEqn Nothing (ConT ''Sig `AppT` ConT name) (promoteOffsetMap om))]
  ]

promoteOffsetMap :: OffsetMap -> Type
promoteOffsetMap (OffsetMap slots msize mtail) =
  PromotedT 'SigOffsetMap
    `AppT` promoteSlots slots
    `AppT` promoteMaybe natT msize
    `AppT` promoteMaybe promoteTail mtail

promoteTail :: RuntimeTail -> Type
promoteTail (RuntimeTail base stride es) =
  PromotedT 'SigTail `AppT` natT base `AppT` natT stride `AppT` promoteSlots es

promoteSlots :: [Slot] -> Type
promoteSlots = promoteList promoteSlot

promoteSlot :: Slot -> Type
promoteSlot (Slot off sc) =
  PromotedT 'SigSlot `AppT` natT off `AppT` PromotedT (scalarName sc)

-- | Promote a list, element-wise, to a promoted @'[..]@.
promoteList :: (a -> Type) -> [a] -> Type
promoteList f = foldr (\x acc -> PromotedConsT `AppT` f x `AppT` acc) PromotedNilT

-- | Promote a 'Maybe' to a promoted @'Nothing@ \/ @'Just@.
promoteMaybe :: (a -> Type) -> Maybe a -> Type
promoteMaybe _ Nothing = PromotedT 'Nothing
promoteMaybe f (Just x) = PromotedT 'Just `AppT` f x

-- | An integral value as a type-level 'Nat' literal.
natT :: (Integral a) => a -> Type
natT = LitT . NumTyLit . fromIntegral

scalarName :: ScalarType -> Name
scalarName = \case
  STFloat -> 'STFloat
  STDouble -> 'STDouble
  STInt -> 'STInt
  STUInt -> 'STUInt
  STInt64 -> 'STInt64
  STUInt64 -> 'STUInt64
  STBool -> 'STBool
  STAddress -> 'STAddress
