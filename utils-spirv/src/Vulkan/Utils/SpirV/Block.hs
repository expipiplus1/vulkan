{-# LANGUAGE NoFieldSelectors #-}

{-| Generate a Haskell record type for a reflected uniform/storage/push-constant
block, deriving its layout from gl-block: @deriving anyclass 'Block'@ (via
"GHC.Generics") plus @deriving 'Storable' via ('Std140' \/ 'Std430')@.

Splice sites must therefore have @Graphics.Gl.Block (Std140(..), Std430(..))@ in
scope (the @via@ coercion needs the newtype constructor visible) and enable
@DataKinds@ and @TypeFamilies@ (each record also gets a
'Vulkan.Utils.SpirV.Signature.KnownLayout' instance carrying its type-level
layout signature).

== Guardrail
gl-block's @Generic@ 'Block' instance lays a struct out correctly only when its
field alignments are non-increasing. A lower-aligned field that precedes a
higher-aligned one (e.g. @float@ before @uvec2@) is over-padded and no longer
matches the shader's std140/std430 offsets. Until that is fixed in gl-block,
'structRecordDec' refuses (with a compile error) to generate such a record rather
than emit a silently-wrong layout.
-}
module Vulkan.Utils.SpirV.Block
  ( structTypeName
  , structRecordDec
  , collectStructs
  , allMembers16
  ) where

import Control.Monad (filterM)
import Data.Char (toLower)
import Data.List (intercalate, mapAccumL)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import Graphics.Gl.Block (Block, Std140, Std430)
import Language.Haskell.TH

import Data.SpirV.Reflect.TypeDescription (TypeDescription)
import Data.SpirV.Reflect.TypeDescription qualified

import Vulkan.Utils.SpirV.Array (Array)
import Vulkan.Utils.SpirV.DeviceAddress (DeviceAddress)
import Vulkan.Utils.SpirV.Layout (offsetMapOf)
import Vulkan.Utils.SpirV.Signature (knownLayoutInstance)
import Vulkan.Utils.SpirV.Types (LayoutMode (..), NumericType (..), TypeMap, arrayBaseAlign, arrayDims, classifyType, isBdaPointer, leafAlignment, structBaseAlign)

{- | A block's struct together with every nested and array-element struct
reachable from it, each tagged with the layout to generate it under. This lets
an SSBO array of structs (e.g. @Sphere spheres[]@) yield a record for the
element type even though the wrapping block — a runtime array — isn't itself
representable as a flat record (the caller skips it).
-}
collectStructs :: LayoutMode -> TypeDescription -> [(LayoutMode, TypeDescription)]
collectStructs layout root = snd (go [] root)
  where
    -- Thread a visited set of SPIR-V ids so a recursive @buffer_reference@ type
    -- (a shared DAG / cycle) is collected exactly once rather than unrolled
    -- forever — a self-referential @Node@ would otherwise loop here.
    go seen td
      | maybe False (`elem` seen) td.id = (seen, [])
      | otherwise =
          let
            seen' = maybe seen (: seen) td.id
            (seenN, kids) = mapAccumL go seen' (concatMap targets (V.toList td.members))
          in
            (seenN, (layout, td) : concat kids)

    -- The struct(s) reachable from a member: an array- or pointer-element struct
    -- (via @struct_type_description@), or a directly-nested struct. Leaf members
    -- (scalars/vectors/matrices, and device-address pointers) contribute none.
    targets m =
      case m.struct_type_description of
        Just s -> [s]
        Nothing
          | not (V.null m.members) -> [m]
          | otherwise -> []

{- | The Haskell type name to generate for an @OpTypeStruct@ 'TypeDescription':
its @type_name@, used verbatim (SPIR-V struct names are already capitalised
Haskell-friendly identifiers).
-}
structTypeName :: TypeDescription -> Maybe String
structTypeName td = case td.type_name of
  Just t | not (Text.null t) -> Just (Text.unpack t)
  _ -> Nothing

{- | A classified record field: its name, Haskell type, base alignment (bytes)
under the layout, and whether it is a leaf (scalar/vector/matrix) as opposed to
a nested struct.
-}
data Field = Field
  { name :: Name
  , type' :: Type
  , align :: Int
  , leaf :: Bool
  }

{- | Generate the record @data@ declaration for an @OpTypeStruct@, deriving its
'Block' / 'Storable' from gl-block. Returns 'Nothing' if it has no usable name
or a member cannot be represented as a field (e.g. an array — see the
arrays-in-records gap), in which case the caller should skip it. Fails the
splice (guardrail) if the field order would defeat gl-block's std140/std430
layout. Nested struct members become fields of the corresponding generated
record type.
-}
structRecordDec :: TypeMap -> LayoutMode -> TypeDescription -> Q (Maybe [Dec])
structRecordDec tymap layout td =
  case (structTypeName td, classifiedFields tymap layout (V.toList td.members)) of
    (Just nameStr, Just classified) ->
      case layoutViolation classified of
        Just msg ->
          fail $
            "vulkan-utils-spirv: refusing to generate record '"
              <> nameStr
              <> "': "
              <> msg
        Nothing -> do
          let
            tyName = mkName nameStr
            recFields =
              [ (f.name, Bang NoSourceUnpackedness NoSourceStrictness, f.type')
              | f <- classified
              ]
            wrapper = case layout of
              Std140Layout -> ''Std140
              Std430Layout -> ''Std430
          -- Derive @Show@/@Eq@ only when every field type supports them. Some
          -- mapped types (e.g. geomancy's @Mat4@) lack @Eq@, and a nested struct
          -- field's instances aren't yet visible mid-splice, so a record with a
          -- struct field derives neither.
          stockExtra <-
            if all (.leaf) classified
              then filterM (allFieldsAreInstances [f.type' | f <- classified]) [''Show, ''Eq]
              else pure []
          let
            dataDec =
              DataD
                []
                tyName
                []
                Nothing
                [RecC tyName recFields]
                [ DerivClause (Just StockStrategy) (ConT ''Generic : map ConT stockExtra)
                , DerivClause (Just AnyclassStrategy) [ConT ''Block]
                , DerivClause
                    (Just (ViaStrategy (ConT wrapper `AppT` ConT tyName)))
                    [ConT ''Storable]
                ]
            -- Tie a type-level layout signature to the record, computed from
            -- the same reflection (the value-level offset map is the oracle). Skipped
            -- if the offset map can't be computed (shouldn't happen for a record we
            -- already classified).
            sigDecs = either (const []) (knownLayoutInstance tyName) (offsetMapOf layout td)
          pure (Just (dataDec : sigDecs))
    _ -> pure Nothing

-- | True when every field type is an instance of the given class.
allFieldsAreInstances :: [Type] -> Name -> Q Bool
allFieldsAreInstances tys cls = and <$> traverse (\t -> isInstance cls [t]) tys

{- | Classify every member, failing (with 'Nothing') if any cannot be represented
as a record field (e.g. an array member).
-}
classifiedFields :: TypeMap -> LayoutMode -> [TypeDescription] -> Maybe [Field]
classifiedFields tymap layout = traverse $ \mem -> do
  fname <- memberFieldName mem
  (ty, align, isLeaf) <- fieldOf tymap layout mem
  pure Field{name = mkName fname, type' = ty, align, leaf = isLeaf}

{- | The Haskell type, base alignment and leaf-ness of a single member, or
'Nothing' if it can't be a field (a runtime\/multi-dimensional array, or an
unrecognised type). A fixed-size array becomes an @'Array' n@ field.
-}
fieldOf :: TypeMap -> LayoutMode -> TypeDescription -> Maybe (Type, Int, Bool)
fieldOf tymap layout mem =
  case arrayDims mem of
    [] -> elementOf tymap layout mem
    dims
      | all (> 0) dims -> do
          -- Fixed-size array (any dimensionality). Multi-dimensional arrays nest:
          -- @a[h][w]@ -> @Array h (Array w a)@ (outermost dimension first).
          (elemTy, elemAlign, _) <- elementOf tymap layout mem
          pure (foldr wrapArray elemTy dims, arrayBaseAlign layout elemAlign, False)
      | otherwise -> Nothing -- contains a runtime (0) dimension
  where
    wrapArray d ty = ConT ''Array `AppT` LitT (NumTyLit (fromIntegral d)) `AppT` ty

{- | The Haskell type, base alignment and leaf-ness of a member's element type
(i.e. ignoring any array dimensions).
-}
elementOf :: TypeMap -> LayoutMode -> TypeDescription -> Maybe (Type, Int, Bool)
elementOf tymap layout mem
  -- A buffer_reference pointer: an 8-byte device address, typed by its pointee
  -- record when that is a named struct (else @DeviceAddress ()@). The pointee
  -- struct is generated separately by 'collectStructs', not inlined here. The
  -- REF check precedes 'classifyType' (a pointer also carries the pointee's leaf
  -- flags, e.g. @REF INT@).
  | isBdaPointer mem =
      -- Reported as non-leaf so the record skips auto-deriving Show/Eq: the
      -- pointee (often the record being defined — a self-referential @Node@) is
      -- not in scope mid-splice, so probing @Show (DeviceAddress Node)@ would
      -- fail, exactly as for a nested-struct field.
      let
        pointee = mem.struct_type_description >>= structTypeName
        arg = maybe (TupleT 0) (ConT . mkName) pointee
      in
        Just (ConT ''DeviceAddress `AppT` arg, 8, False)
  | otherwise =
      case classifyType mem of
        Just numeric -> do
          let numeric' = numeric{array = []}
          ty <- tymap numeric'
          pure (ty, leafAlignment layout numeric'.scalar numeric'.shape, True)
        Nothing -> do
          -- A nested struct member: field of the corresponding generated record.
          let s = fromMaybe mem mem.struct_type_description
          nm <- structTypeName s
          align <- structAlignment layout s
          pure (ConT (mkName nm), align, False)

{- | Guardrail: gl-block's 'Generic' layout matches std140/std430 only when field
alignments are non-increasing. Report the first inversion, if any.
-}
layoutViolation :: [Field] -> Maybe String
layoutViolation classified = go [(nameBase f.name, f.align) | f <- classified]
  where
    go ((n1, a1) : rest@((n2, a2) : _))
      | a1 < a2 =
          Just $
            intercalate
              " "
              [ "field '" <> n1 <> "' (alignment " <> show a1 <> ")"
              , "precedes higher-aligned field '" <> n2 <> "' (alignment " <> show a2 <> ");"
              , "gl-block's Generic layout would not match std140/std430."
              , "Reorder the block's fields by non-increasing alignment"
              , "(temporary gl-block limitation)."
              ]
      | otherwise = go rest
    go _ = Nothing

{- | The base alignment (bytes) of a struct under the given layout: the largest
member alignment, rounded up to 16 for std140. 'Nothing' if any member's
alignment can't be determined.
-}
structAlignment :: LayoutMode -> TypeDescription -> Maybe Int
structAlignment layout td = do
  as <- traverse (memberBaseAlignment layout) (V.toList td.members)
  pure (structBaseAlign layout as)

-- | The base alignment of any member (leaf, nested struct, or fixed array).
memberBaseAlignment :: LayoutMode -> TypeDescription -> Maybe Int
memberBaseAlignment layout mem =
  case arrayDims mem of
    [] -> elementBaseAlignment layout mem
    dims
      | all (> 0) dims -> arrayBaseAlign layout <$> elementBaseAlignment layout mem
      | otherwise -> Nothing

-- | The base alignment of a member's element type (ignoring array dimensions).
elementBaseAlignment :: LayoutMode -> TypeDescription -> Maybe Int
elementBaseAlignment layout mem
  | isBdaPointer mem = Just 8 -- device address: 8-byte scalar
  | otherwise =
      case classifyType mem of
        Just numeric -> Just (leafAlignment layout numeric.scalar numeric.shape)
        Nothing -> structAlignment layout (fromMaybe mem mem.struct_type_description)

{- | True when every member of the struct has 16-byte base alignment. Such a
struct lays out identically under std140 and std430 (same offsets, same size),
so a single generated record can be shared across both layouts ("promotion").
Anything else used across layouts is rejected by the sharing guardrail.
-}
allMembers16 :: TypeDescription -> Bool
allMembers16 td = not (null ms) && all is16 ms
  where
    ms = V.toList td.members
    is16 m = memberBaseAlignment Std430Layout m == Just 16

uncapitalize :: String -> String
uncapitalize [] = []
uncapitalize (c : cs) = toLower c : cs

{- | The Haskell record-field name for a reflected member: its declared name,
uncapitalised (SPIR-V capitalises member names, Haskell fields are lower-case).
-}
memberFieldName :: TypeDescription -> Maybe String
memberFieldName = fmap (uncapitalize . Text.unpack) . (.struct_member_name)
