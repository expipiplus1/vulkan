{-# LANGUAGE NoFieldSelectors #-}

{-| Classify a reflected block member into a base scalar + shape, and map it to
a Haskell type that carries a 'Graphics.Gl.Block.Block' instance.

The mapping is pluggable ('TypeMap'). The default is 'geomancyTypeMap', onto
[geomancy](https://hackage.haskell.org/package/geomancy)'s @Vec*@ \/ @IVec*@ \/
@UVec*@ \/ @Mat4@ (which carry native std140/std430 'Block' instances).
'linearTypeMap' is a worked /example/ of the same recipe for a library whose
types are parametric (it is not usable for codegen yet — see its note).

A map names its types by /raw qualified name/ against the library's base module
(e.g. @Geomancy.Vec3@) rather than a compile-time @''@ quote, so this package
depends on no vector library. The names resolve at the /splice site/, so a module
that splices these records must @import Geomancy qualified@ — under the base
module's own name, not an @as@ alias — and have a 'Graphics.Gl.Block.Block'
instance in scope for each mapped type. A missing import surfaces as a plain
\"Not in scope: type constructor Geomancy.Vec3\".

Build a map for any other vector library the same way with 'mkTypeMap' +
'qualType' (+ 'scalarLeaf' for the base components): 'geomancyTypeMap' shows the
monomorphic case (the scalar is baked into the name), 'linearTypeMap' the
parametric case (the scalar is applied as a type argument).
-}
module Vulkan.Utils.SpirV.Types
  ( ScalarType (..)
  , MemberShape (..)
  , NumericType (..)
  , classifyType
  , isBdaPointer
  , arrayDims
  , LayoutMode (..)
  , scalarWidth
  , arrayBaseAlign
  , leafAlignment
  , structBaseAlign
  , TypeMap
  , mkTypeMap
  , scalarLeaf
  , qualType
  , geomancyTypeMap
  , linearTypeMap
  ) where

import Data.Bits ((.&.))
import Data.Int (Int32, Int64)
import Data.Vector.Storable qualified as VS
import Data.Word (Word32, Word64)
import Graphics.Gl.Block (roundUp)
import Language.Haskell.TH (Type (AppT, ConT), mkName)

import Data.SpirV.Reflect.Enums.TypeFlags qualified as TypeFlags
import Data.SpirV.Reflect.Traits qualified as Traits
import Data.SpirV.Reflect.TypeDescription (TypeDescription)
import Data.SpirV.Reflect.TypeDescription qualified as TypeDescription

{- | The base component type of a block member.

'STInt64' \/ 'STUInt64' are 64-bit integers (@int64_t@ \/ @uint64_t@, the
@GL_EXT_shader_explicit_arithmetic_types_int64@ scalars): 8-byte slots, like
'STDouble'. 'STAddress' is a 64-bit buffer device address (@buffer_reference@ /
@PhysicalStorageBuffer@ pointer): no GLSL scalar spelling, but one 8-byte slot
like any other scalar. Sub-32-bit scalars (@float16@ \/ @int16@) are out of scope.
-}
data ScalarType = STFloat | STDouble | STInt | STUInt | STInt64 | STUInt64 | STBool | STAddress
  deriving (Eq, Ord, Show)

-- | The shape of a block member.
data MemberShape
  = ShScalar
  | -- | component count (2..4)
    ShVector Int
  | -- | columns, rows
    ShMatrix Int Int
  deriving (Eq, Ord, Show)

{- | Which gl-block layout a block follows. Uniform buffers use std140; storage
buffers and push constants use std430.
-}
data LayoutMode = Std140Layout | Std430Layout
  deriving (Eq, Show)

-- | The byte width of a scalar component (64-bit scalars are 8, the rest 4).
scalarWidth :: ScalarType -> Int
scalarWidth STDouble = 8
scalarWidth STInt64 = 8
scalarWidth STUInt64 = 8
scalarWidth STAddress = 8
scalarWidth _ = 4

{- | The base alignment of an array or matrix column: std140 rounds the
element\/column alignment up to a multiple of 16, std430 keeps it.
-}
arrayBaseAlign :: LayoutMode -> Int -> Int
arrayBaseAlign Std140Layout a = roundUp a 16
arrayBaseAlign Std430Layout a = a

{- | The base alignment of a leaf member (scalar\/vector\/matrix) under the
layout. A matrix aligns as its column vector (rounded up to 16 under std140).
-}
leafAlignment :: LayoutMode -> ScalarType -> MemberShape -> Int
leafAlignment mode s = \case
  ShScalar -> w
  ShVector 2 -> 2 * w
  ShVector _ -> 4 * w
  ShMatrix _ r -> arrayBaseAlign mode (if r <= 2 then 2 * w else 4 * w)
  where
    w = scalarWidth s

{- | The base alignment of a struct: the largest member alignment (at least 1),
rounded up to 16 under std140.
-}
structBaseAlign :: LayoutMode -> [Int] -> Int
structBaseAlign mode alignments = case mode of
  Std140Layout -> roundUp base 16
  Std430Layout -> base
  where
    base = maximum (1 : alignments)

{- | A numeric (non-struct) leaf type: a base scalar component, a shape
(scalar\/vector\/matrix), and zero or more array dimensions (outermost first;
empty means not an array).
-}
data NumericType = NumericType
  { scalar :: ScalarType
  , shape :: MemberShape
  , array :: [Word32]
  }
  deriving (Eq, Ord, Show)

{- | Classify a reflected struct member (a 'TypeDescription') from its numeric
traits and type flags.
-}
classifyType :: TypeDescription -> Maybe NumericType
classifyType
  TypeDescription.TypeDescription
    { TypeDescription.type_flags = flags
    , TypeDescription.traits = mtraits
    } = do
    Traits.Numeric
      { Traits.scalar = Traits.Scalar{Traits.width = width, Traits.signed = signed}
      , Traits.vector = Traits.Vector{Traits.component_count = vecN}
      , Traits.matrix = Traits.Matrix{Traits.column_count = cols, Traits.row_count = rows}
      } <-
      numericOf mtraits
    let
      shape
        | cols > 0 = ShMatrix (fromIntegral cols) (fromIntegral rows)
        | vecN > 1 = ShVector (fromIntegral vecN)
        | otherwise = ShScalar
      wide = width >= 64
      baseScalar
        | has TypeFlags.TYPE_FLAG_FLOAT = Just $ if wide then STDouble else STFloat
        | has TypeFlags.TYPE_FLAG_INT = Just $ case (signed, wide) of
            (True, False) -> STInt
            (False, False) -> STUInt
            (True, True) -> STInt64
            (False, True) -> STUInt64
        | has TypeFlags.TYPE_FLAG_BOOL = Just STBool
        | otherwise = Nothing
    scalar <- baseScalar
    pure
      NumericType
        { scalar
        , shape
        , array = maybe [] dimsOf (arrayOf mtraits)
        }
    where
      has bit = (flags .&. bit) /= TypeFlags.TypeFlagBits 0

      numericOf t = do
        TypeDescription.Traits{TypeDescription.numeric = n} <- t
        Just n

      arrayOf t = do
        TypeDescription.Traits{TypeDescription.array = a} <- t
        Just a

{- | True when this type is a buffer device address: a @buffer_reference@ \/
@PhysicalStorageBuffer@ pointer (the @REF@ type flag). Such a member is stored as
an 8-byte address, not its pointee inline.
-}
isBdaPointer :: TypeDescription -> Bool
isBdaPointer td = (td.type_flags .&. TypeFlags.TYPE_FLAG_REF) /= TypeFlags.TypeFlagBits 0

{- | The array dimensions of a member (outermost first); @[]@ for a non-array,
@[0]@ for a runtime (unsized) array.
-}
arrayDims :: TypeDescription -> [Word32]
arrayDims td = maybe [] dimsOf (fmap (.array) td.traits)

dimsOf :: Traits.Array -> [Word32]
dimsOf a
  | a.dims_count == 0 = []
  | otherwise = VS.toList a.dims

{- | A pluggable mapping from a classified member to the Haskell type used for
the generated record field. The returned type must have a
'Graphics.Gl.Block.Block' instance. Returning 'Nothing' rejects the member
(the block record is then not generated).
-}
type TypeMap = NumericType -> Maybe Type

{- | Assemble a 'TypeMap' from a vector and a matrix spelling — the shared core
of 'geomancyTypeMap' / 'linearTypeMap'. Scalars always map to base types (via
'scalarLeaf'); only the vector/matrix cases differ between libraries.

This maps an /element/ type only: array dimensions are handled one layer up (in
"Vulkan.Utils.SpirV.Block", which wraps the result in 'Vulkan.Utils.SpirV.Array.Array'),
so a member that still carries array dims here is rejected.
-}
mkTypeMap
  :: (ScalarType -> Int -> Maybe Type)
  -- ^ vector: component scalar and count (2..4)
  -> (ScalarType -> Int -> Int -> Maybe Type)
  -- ^ matrix: component scalar, columns, rows
  -> TypeMap
mkTypeMap vector matrix NumericType{scalar, shape, array}
  | not (null array) = Nothing
  | otherwise = case shape of
      ShScalar -> scalarLeaf scalar
      ShVector n -> vector scalar n
      ShMatrix c r -> matrix scalar c r

{- | The Haskell type for a base scalar component, as a global @''@ quote: 'base'
is always in scope, so a field of this type needs no import at the splice site.
'STAddress' has no scalar spelling — a pointer member is mapped to
'Vulkan.Utils.SpirV.DeviceAddress.DeviceAddress' before it reaches a 'TypeMap'.
-}
scalarLeaf :: ScalarType -> Maybe Type
scalarLeaf = \case
  STFloat -> Just (ConT ''Float)
  STDouble -> Just (ConT ''Double)
  STInt -> Just (ConT ''Int32)
  STUInt -> Just (ConT ''Word32)
  STInt64 -> Just (ConT ''Int64)
  STUInt64 -> Just (ConT ''Word64)
  STBool -> Just (ConT ''Bool)
  STAddress -> Nothing

{- | A type referenced by /raw qualified name/ (@Module.Type@), resolved at the
splice site. This is how a default 'TypeMap' names a vector library's types
without this package depending on it: the splice site supplies the import.
-}
qualType :: String -> String -> Type
qualType modName typeName = ConT (mkName (modName <> "." <> typeName))

{- | The default mapping, onto [geomancy](https://hackage.haskell.org/package/geomancy)'s
vector/matrix types (which carry native std140/std430 'Block' instances). The
names resolve against geomancy's base module, so the splice site needs only a
single @import Geomancy qualified@.
-}
geomancyTypeMap :: TypeMap
geomancyTypeMap = mkTypeMap vector matrix
  where
    -- geomancy spells the scalar into the type name: Vec3 / IVec3 / UVec3.
    vector STFloat n = geomancy <$> sized "Vec" n
    vector STInt n = geomancy <$> sized "IVec" n
    vector STUInt n = geomancy <$> sized "UVec" n
    vector _ _ = Nothing
    matrix STFloat 4 4 = Just (geomancy "Mat4")
    matrix _ _ _ = Nothing
    geomancy = qualType "Geomancy"

{- | A worked /example/ of mapping a vector library whose types are /parametric/:
[linear](https://hackage.haskell.org/package/linear)'s @V2@ \/ @V3@ \/ @V4@ \/
@M44@ take the component scalar as a type argument, so the map applies it
(@V3 Float@, @M44 Float@) instead of spelling it into the name as 'geomancyTypeMap'
does. Names resolve against linear's base module (a single @import Linear qualified@).

Not usable for record generation yet: neither linear nor gl-block ships a
'Graphics.Gl.Block.Block' instance for @V3 Float@ etc., so a generated record
could not derive its 'Foreign.Storable.Storable'. It stands as the template for
the parametric case until linear gains those instances, at which point it becomes
a real alternative default.
-}
linearTypeMap :: TypeMap
linearTypeMap = mkTypeMap vector matrix
  where
    -- linear's vectors are parametric (V3 a): apply the component scalar.
    vector s n
      | s `elem` [STFloat, STInt, STUInt] = AppT . linear <$> sized "V" n <*> scalarLeaf s
      | otherwise = Nothing
    matrix STFloat 4 4 = AppT (linear "M44") <$> scalarLeaf STFloat
    matrix _ _ _ = Nothing
    linear = qualType "Linear"

-- | A library type whose name carries its component count, e.g. @Vec3@ \/ @V4@.
sized :: String -> Int -> Maybe String
sized prefix n
  | n >= 2 && n <= 4 = Just (prefix <> show n)
  | otherwise = Nothing
