{-# LANGUAGE OverloadedStrings #-}

{-| Tests for the structured 'Layout' IR, its normalization to a scalar 'OffsetMap',
and the value-level unifier.
-}
module LayoutSpec (tests) where

import Data.Either (isLeft)
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Text (unpack)
import Data.Vector qualified as V
import Test.Tasty
import Test.Tasty.HUnit

import Data.SpirV.Reflect.BlockVariable qualified
import Data.SpirV.Reflect.DescriptorBinding qualified
import Data.SpirV.Reflect.FFI (load)
import Data.SpirV.Reflect.Module (Module)
import Data.SpirV.Reflect.Module qualified
import Data.SpirV.Reflect.TypeDescription (TypeDescription)

import Vulkan.Utils.SpirV.Layout
import Vulkan.Utils.SpirV.Layout qualified as OffsetMap (OffsetMap (..))
import Vulkan.Utils.SpirV.Types (LayoutMode (..), ScalarType (..))

tests :: TestTree
tests =
  testGroup
    "layout IR + unification"
    [ structuredTests
    , normalizeTests
    , unifyTests
    , runtimeTests
    , mergeTests
    , reflectionTests
    ]

-- Convenience constructors. -----------------------------------------------------

-- | A single-member std430 layout.
field430 :: FieldType -> Layout
field430 ft = fromFields Std430Layout Nothing [("x", ft)]

-- | A single-member std140 layout.
field140 :: FieldType -> Layout
field140 ft = fromFields Std140Layout Nothing [("x", ft)]

structuredTests :: TestTree
structuredTests =
  testGroup
    "structured layout (fromFields)"
    [ testCase "std430 packs by member alignment" $ do
        -- mat4 (align 16) @0 size 64, vec2 (align 8) @64, float @72, int @76
        let l =
              fromFields
                Std430Layout
                (Just "Push")
                [ ("transform", Matrix STFloat 4 4)
                , ("offset", Vector STFloat 2)
                , ("scale", Scalar STFloat)
                , ("count", Scalar STInt)
                ]
        offsets l @?= [("transform", 0), ("offset", 64), ("scale", 72), ("count", 76)]
        l.size @?= Just 80
        l.align @?= 16
    , testCase "std140 rounds a scalar array's stride and the struct align to 16" $ do
        let l = field140 (ArrayOf (Sized 4) (Scalar STFloat))
        l.align @?= 16
        l.size @?= Just 64 -- stride 16 * 4
    , testCase "std430 packs a scalar array tightly" $ do
        let l = field430 (ArrayOf (Sized 4) (Scalar STFloat))
        l.align @?= 4
        l.size @?= Just 16
    , testCase "vec3 occupies 12 bytes but aligns to 16" $ do
        let l = fromFields Std430Layout Nothing [("a", Vector STFloat 3), ("b", Scalar STFloat)]
        offsets l @?= [("a", 0), ("b", 12)]
    ]

normalizeTests :: TestTree
normalizeTests =
  testGroup
    "normalize to scalar offset map"
    [ testCase "vec4 flattens to four floats" $
        (normalize (field430 (Vector STFloat 4))).slots
          @?= [Slot 0 STFloat, Slot 4 STFloat, Slot 8 STFloat, Slot 12 STFloat]
    , testCase "mat4 flattens column-major to a 16-float offset map" $
        map (.offset) (normalize (field430 (Matrix STFloat 4 4))).slots
          @?= [0, 4, 8, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 52, 56, 60]
    , testCase "std140 float[4] strides by 16" $
        map (.offset) (normalize (field140 (ArrayOf (Sized 4) (Scalar STFloat)))).slots
          @?= [0, 16, 32, 48]
    ]

unifyTests :: TestTree
unifyTests =
  testGroup
    "unify (layout equivalence)"
    [ testCase "mat4 == vec4[4] == float[16] under std430" $ do
        let
          m = normalize (field430 (Matrix STFloat 4 4))
          v = normalize (field430 (ArrayOf (Sized 4) (Vector STFloat 4)))
          f = normalize (field430 (ArrayOf (Sized 16) (Scalar STFloat)))
        foldUnify m [v, f] @?= Right m
    , testCase "float[16] under std140 is NOT layout-equivalent to mat4" $ do
        let
          m = normalize (field430 (Matrix STFloat 4 4))
          f = normalize (field140 (ArrayOf (Sized 16) (Scalar STFloat)))
        case unify m f of
          Left (SlotMismatch off _ _) -> off @?= 4 -- first divergence: mat4 has a float here
          other -> assertFailure ("expected a SlotMismatch at 4, got " <> show other)
    , testCase "a scalar-kind mismatch is reported with its offset" $ do
        let
          a = normalize (field430 (Vector STFloat 2))
          b = normalize (field430 (Vector STInt 2))
        unify a b @?= Left (SlotMismatch 0 STFloat STInt)
    , testCase "differing total size fails even when slots agree" $ do
        -- Same scalar slots, different declared size (e.g. extra trailing padding).
        let
          a = normalize (field430 (Vector STFloat 4))
          big = a{OffsetMap.size = Just 32}
        unify a big @?= Left (SizeMismatch 16 32)
    , testCase "renderMismatch is legible" $
        assertBool "mentions offset and types" $
          "offset 4" `isInfixOf` renderMismatch (SlotMismatch 4 STFloat STInt)
            && "float" `isInfixOf` renderMismatch (SlotMismatch 4 STFloat STInt)
    ]

runtimeTests :: TestTree
runtimeTests =
  testGroup
    "runtime arrays (unification with a hole)"
    [ testCase "an open vec4[] offset map is open (no size, has a tail)" $ do
        let g = normalize (field430 (ArrayOf Runtime (Vector STFloat 4)))
        g.size @?= Nothing
        fmap (.stride) g.tail @?= Just 16
    , testCase "a runtime vec4[] unifies with a concrete vec4[3], pinning the length" $ do
        let
          open = normalize (field430 (ArrayOf Runtime (Vector STFloat 4)))
          closed = normalize (field430 (ArrayOf (Sized 3) (Vector STFloat 4)))
        unify open closed @?= Right closed
        unify closed open @?= Right closed -- symmetric
    , testCase "a runtime vec4[] rejects an offset map that is not whole elements" $ do
        let
          open = normalize (field430 (ArrayOf Runtime (Vector STFloat 4)))
          ragged = normalize (field430 (ArrayOf (Sized 5) (Scalar STFloat)))
        assertBool "should not unify" (isLeft (unify open ragged))
    ]

mergeTests :: TestTree
mergeTests =
  testGroup
    "mergeKeyed (row unification across pipelines)"
    [ testCase "disjoint keys union; shared keys must agree" $ do
        let
          camera = normalize (field140 (Vector STFloat 4))
          lights = normalize (field430 (ArrayOf (Sized 2) (Vector STFloat 4)))
          merged =
            mergeKeyed
              [ ((0, 0) :: (Int, Int), camera) -- vertex stage: set 0 binding 0
              , ((0, 1), lights) -- vertex stage: set 0 binding 1
              , ((0, 0), camera) -- fragment stage: same UBO, must agree
              ]
        fmap Map.keys merged @?= Right [(0, 0), (0, 1)]
    , testCase "a conflicting shared key reports the key and the mismatch" $ do
        let
          a = normalize (field430 (Vector STFloat 4))
          b = normalize (field430 (Vector STInt 4))
        case mergeKeyed [((0, 0) :: (Int, Int), a), ((0, 0), b)] of
          Left (k, SlotMismatch{}) -> k @?= (0, 0)
          other -> assertFailure ("expected a keyed SlotMismatch, got " <> show other)
    ]

reflectionTests :: TestTree
reflectionTests =
  testGroup
    "built from reflected SPIR-V"
    [ testCase "push.comp Push has std430 offsets 0/64/72/76, size 80" $ do
        m <- load "test/fixtures/push.comp.spv"
        td <- maybe (assertFailure "no push constant block") pure (pushBlock m)
        l <- either (assertFailure . ("layoutOf: " <>)) pure (layoutOf Std430Layout td)
        map snd (offsets l) @?= [0, 64, 72, 76]
        l.size @?= Just 80
    , testCase "push.comp Output (vec4 data[]) is an open offset map with stride 16" $ do
        m <- load "test/fixtures/push.comp.spv"
        td <- maybe (assertFailure "no SSBO binding") pure (firstBinding m)
        g <- either (assertFailure . ("offsetMapOf: " <>)) pure (offsetMapOf Std430Layout td)
        g.size @?= Nothing
        fmap (.stride) g.tail @?= Just 16
    , testCase "ssbo-struct Particle[] tail unifies with a concrete Particle[2]" $ do
        m <- load "test/fixtures/ssbo-struct.comp.spv"
        td <- maybe (assertFailure "no SSBO binding") pure (firstBinding m)
        open <- either (assertFailure . ("offsetMapOf: " <>)) pure (offsetMapOf Std430Layout td)
        -- Particle = vec3 @0, vec2 @16, float @24, uint @28 (stride 32).
        let
          particle =
            Struct
              ( fromFields
                  Std430Layout
                  (Just "Particle")
                  [ ("position", Vector STFloat 3)
                  , ("velocity", Vector STFloat 2)
                  , ("mass", Scalar STFloat)
                  , ("flags", Scalar STUInt)
                  ]
              )
          closed = normalize (fromFields Std430Layout Nothing [("items", ArrayOf (Sized 2) particle)])
        open `unifies` closed
    ]

-- Helpers. ----------------------------------------------------------------------

offsets :: Layout -> [(String, Int)]
offsets l = [(unpack m.name, m.offset) | m <- l.members]

unifies :: OffsetMap -> OffsetMap -> Assertion
unifies a b = case unify a b of
  Right _ -> pure ()
  Left e -> assertFailure (renderMismatch e)

pushBlock :: Module -> Maybe TypeDescription
pushBlock m = case V.toList m.push_constants of
  (pc : _) -> pc.type_description
  [] -> Nothing

firstBinding :: Module -> Maybe TypeDescription
firstBinding m = case V.toList m.descriptor_bindings of
  (b : _) -> b.type_description
  [] -> Nothing
