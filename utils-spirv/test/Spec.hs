{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Data.Bits ((.|.))
import Data.Either (isLeft)
import Data.Int (Int32, Int64)
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Word (Word32, Word64)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable (..))
import GHC.Generics (Generic)
import Geomancy qualified
import Geomancy.Mat4 (identity)
import Geomancy.UVec2 (uvec2)
import Geomancy.Vec2 (Vec2, vec2)
import Geomancy.Vec3 (vec3)
import Geomancy.Vec4 (Vec4, vec4)
import Graphics.Gl.Block (Block, Std140 (..), Std430 (..))
import Language.Haskell.TH (Type (AppT, ConT), mkName)
import Test.Tasty
import Test.Tasty.HUnit

import Vulkan.Core10 qualified as Vk

import Data.SpirV.Reflect.FFI (load)
import Vulkan.Utils.SpirV.Array qualified as Array
import Vulkan.Utils.SpirV.Buffer (allocArrayBuffer, newBuffer, readBuffer, readBufferElem, writeBufferElem)
import Vulkan.Utils.SpirV.Descriptors (descriptorSetLayoutInfos, mergedDescriptorSetLayoutInfos, mergedPushConstantRanges, pushConstantRanges)
import Vulkan.Utils.SpirV.DeviceAddress (DeviceAddress (..))
import Vulkan.Utils.SpirV.Layout (ArraySize (..), FieldType (..), fromFields, normalize)
import Vulkan.Utils.SpirV.Layout qualified
import Vulkan.Utils.SpirV.Signature (ArrayOf, Fits, Sig, knownLayoutInstance, layoutSig)
import Vulkan.Utils.SpirV.Specialization (specializationConstants, specializationMapEntries)
import Vulkan.Utils.SpirV.Stage (CompatibleResources, KnownLayoutSig (..), MatchInterface, linkStages, matchInterface, mergeLayout, reflectPipelineLayoutSig, reflectStageSig, stageInfoOf)
import Vulkan.Utils.SpirV.Stage qualified
import Vulkan.Utils.SpirV.Stage qualified as StageInfo (StageInfo (..))
import Vulkan.Utils.SpirV.TH (reflectShaderTypes)
import Vulkan.Utils.SpirV.Types (LayoutMode (..), MemberShape (..), NumericType (..), ScalarType (..), geomancyTypeMap, linearTypeMap)
import Vulkan.Utils.SpirV.VertexInput (vertexInputAttributes, vertexInputBinding, vertexInputState)

import LayoutSpec qualified

-- Generate the @Params@ record from the committed compute fixture.
reflectShaderTypes "test/fixtures/julia.comp.spv"

-- Generate the @Push@ push-constant record (std430) from its fixture.
reflectShaderTypes "test/fixtures/push.comp.spv"

-- Generate the @Particle@ element record of an SSBO array of structs. The
-- wrapping @Particles@ runtime-array block is (correctly) not generated.
reflectShaderTypes "test/fixtures/ssbo-struct.comp.spv"

-- Generate @Material@ (shared across a std140 UBO field and a std430 SSBO array,
-- safe because all members are 16-byte aligned) and @Scene@, which has the
-- nested @Material@ as a field.
reflectShaderTypes "test/fixtures/nested.comp.spv"

-- Generate @Kernel140@ / @Kernel430@: records with fixed-size @Array n a@ fields
-- under std140 (element stride 16) and std430 (tight) layouts.
reflectShaderTypes "test/fixtures/array-field.comp.spv"

-- Generate @Grid430@ / @Grid140@: a 2D array field @float grid[3][4]@ maps to
-- @Array 3 (Array 4 Float)@.
reflectShaderTypes "test/fixtures/array2d.comp.spv"

-- Generate @Node@ (a self-referential @buffer_reference@ struct) and @Bvh@ (a
-- push-constant block holding a @Node@ device address). The pointer members map
-- to @DeviceAddress Node@; @Node@ is generated once despite the cycle.
reflectShaderTypes "test/fixtures/bda.comp.spv"

-- Generate @Wide@: a std430 push-constant block with 64-bit integer members
-- (@uint64_t hi@ / @int64_t lo@), which must occupy 8-byte slots like a double.
reflectShaderTypes "test/fixtures/wide.comp.spv"

-- A std430 element whose array stride (32) exceeds its packed size (20): vec4 @0
-- + float @16 -> size 20, rounded up to its 16-byte alignment for the stride.
data Pad = Pad Vec4 Float
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Block)
  deriving (Storable) via (Std430 Pad)

-- Three types that are layout-equivalent under std430 (same scalar offset map): mat4, vec4[4] and
-- float[16]. Their layout signatures are produced from the value-level normal
-- form (the oracle), so they promote to the SAME 'Sig' and 'Fits' accepts them.
data EquivMat4
data EquivVec4x4
data EquivFloat16

pure
  ( knownLayoutInstance
      ''EquivMat4
      (normalize (fromFields Std430Layout Nothing [("m", Matrix STFloat 4 4)]))
  )
pure
  ( knownLayoutInstance
      ''EquivVec4x4
      (normalize (fromFields Std430Layout Nothing [("a", ArrayOf (Sized 4) (Vector STFloat 4))]))
  )
pure
  ( knownLayoutInstance
      ''EquivFloat16
      (normalize (fromFields Std430Layout Nothing [("a", ArrayOf (Sized 16) (Scalar STFloat))]))
  )

-- Compile-time witnesses: each body only type-checks if the 'Fits' constraint
-- holds (a mismatch is a compile error). Returned as 'Bool' so a test can run.
mat4FitsVec4x4 :: (Fits (Sig EquivMat4) (Sig EquivVec4x4)) => Bool
mat4FitsVec4x4 = True

vec4x4FitsFloat16 :: (Fits (Sig EquivVec4x4) (Sig EquivFloat16)) => Bool
vec4x4FitsFloat16 = True

pushFitsItself :: (Fits (Sig Push) (Sig Push)) => Bool
pushFitsItself = True

-- Two records that are layout-equivalent under std430: two vec4s vs four vec2s both flatten to
-- eight floats at offsets 0,4,..,28 (size 32). Real 'Storable' records, so a
-- value written as one can be read back through the other.
data PairA = PairA Vec4 Vec4
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Block)
  deriving (Storable) via (Std430 PairA)

data PairB = PairB Vec2 Vec2 Vec2 Vec2
  deriving stock (Generic, Eq, Show)
  deriving anyclass (Block)
  deriving (Storable) via (Std430 PairB)

pure
  ( knownLayoutInstance
      ''PairA
      (normalize (fromFields Std430Layout Nothing [("a", Vector STFloat 4), ("b", Vector STFloat 4)]))
  )
pure
  ( knownLayoutInstance
      ''PairB
      ( normalize
          ( fromFields
              Std430Layout
              Nothing
              [("a", Vector STFloat 2), ("b", Vector STFloat 2), ("c", Vector STFloat 2), ("d", Vector STFloat 2)]
          )
      )
  )

-- Stage signatures for a matched vertex+fragment pair (shared Scene UBO, vertex
-- push constant, fragment SSBO). Phase 1 exercises only the interface.
reflectStageSig "MeshVert" "test/fixtures/mesh.vert.spv"
reflectStageSig "MeshFrag" "test/fixtures/mesh.frag.spv"

-- The merged pipeline-layout signature of the same pair, promoted to the type level.
reflectPipelineLayoutSig "MeshLayout" ["test/fixtures/mesh.vert.spv", "test/fixtures/mesh.frag.spv"]

-- Compile-time witness: only type-checks if the vertex→fragment interface matches.
meshInterfaceOk :: (MatchInterface MeshVert MeshFrag) => Bool
meshInterfaceOk = True

-- Compile-time witness: only type-checks if the shared resources are compatible
-- (the Scene UBO is declared identically in both stages).
meshResourcesOk :: (CompatibleResources MeshVert MeshFrag) => Bool
meshResourcesOk = True

main :: IO ()
main =
  defaultMain $
    testGroup
      "vulkan-utils-spirv"
      [ testGroup
          "block record (std140)"
          [ testCase "alignment is std140 vec4 (16)" $
              alignment params @?= 16
          , testCase "fields land at the shader's std140 offsets" $ do
              -- shader.comp: center@0, resolution@8, escapeRadius@16, maxIterations@20
              let n = sizeOf params
              (resX, esc, maxIt) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) params
                (,,)
                  <$> (peekByteOff ptr 8 :: IO Word32)
                  <*> (peekByteOff ptr 16 :: IO Float)
                  <*> (peekByteOff ptr 20 :: IO Int32)
              resX @?= 512
              esc @?= 2.0
              maxIt @?= 1000
          , testCase "round-trips through Storable" $ do
              let n = sizeOf params
              rt <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) params
                peek (castPtr ptr)
              rt @?= params
          ]
      , testGroup
          "field type spelling (TypeMap)"
          -- geomancyTypeMap is also exercised end-to-end by the record fixtures.
          -- linearTypeMap is a worked example of the parametric case with no Block
          -- instances to splice against yet, so these pure checks pin its spelling.
          [ testCase "geomancy: scalar in name, single qualifier" $ do
              geomancyTypeMap (NumericType STFloat (ShVector 3) []) @?= Just (ConT (mkName "Geomancy.Vec3"))
              geomancyTypeMap (NumericType STUInt (ShVector 2) []) @?= Just (ConT (mkName "Geomancy.UVec2"))
              geomancyTypeMap (NumericType STFloat (ShMatrix 4 4) []) @?= Just (ConT (mkName "Geomancy.Mat4"))
          , testCase "linear: parametric, scalar applied" $ do
              linearTypeMap (NumericType STFloat (ShVector 3) []) @?= Just (AppT (ConT (mkName "Linear.V3")) (ConT ''Float))
              linearTypeMap (NumericType STUInt (ShVector 2) []) @?= Just (AppT (ConT (mkName "Linear.V2")) (ConT ''Word32))
              linearTypeMap (NumericType STFloat (ShMatrix 4 4) []) @?= Just (AppT (ConT (mkName "Linear.M44")) (ConT ''Float))
          , testCase "both reject array members and address scalars" $ do
              geomancyTypeMap (NumericType STFloat (ShVector 3) [4]) @?= Nothing
              linearTypeMap (NumericType STAddress ShScalar []) @?= Nothing
          ]
      , testGroup
          "push-constant block (std430)"
          [ testCase "alignment is mat4 (16)" $
              alignment pushc @?= 16
          , testCase "fields land at the shader's std430 offsets" $ do
              -- push.comp: transform@0, offset@64, scale@72, count@76
              let n = sizeOf pushc
              (sc, cnt) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) pushc
                (,)
                  <$> (peekByteOff ptr 72 :: IO Float)
                  <*> (peekByteOff ptr 76 :: IO Int32)
              sc @?= 2.5
              cnt @?= 7
          , testCase "size covers the last member (80)" $
              sizeOf pushc @?= 80
          , testCase "round-trips through Storable" $ do
              -- @Push@ has a @Mat4@ field (no @Eq@), so compare the comparable
              -- fields after a Storable round-trip rather than the whole record.
              let n = sizeOf pushc
              rt <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) pushc
                peek (castPtr ptr) :: IO Push
              (rt.offset, rt.scale, rt.count) @?= (pushc.offset, pushc.scale, pushc.count)
          ]
      , testCase "push-constant range from reflection" $ do
          m <- load "test/fixtures/push.comp.spv"
          case pushConstantRanges m of
            [r] -> do
              r.offset @?= 0
              r.size @?= 80
              r.stageFlags @?= Vk.SHADER_STAGE_COMPUTE_BIT
            other -> assertFailure ("expected one range, got " <> show (length other))
      , testGroup
          "64-bit integer scalars (std430)"
          [ testCase "uint64_t/int64_t members land at 8-byte-aligned offsets" $ do
              -- wide.comp: hi@0, lo@8, tag@16. A 64-bit int is an 8-byte slot, so
              -- tag sits at 16 — under the old 32-bit assumption it would be at 8.
              let n = sizeOf wide
              (hi, lo, tag) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) wide
                (,,)
                  <$> (peekByteOff ptr 0 :: IO Word64)
                  <*> (peekByteOff ptr 8 :: IO Int64)
                  <*> (peekByteOff ptr 16 :: IO Word32)
              (hi, lo, tag) @?= (0xDEADBEEFCAFEF00D, -42, 7)
          , testCase "record rounds up to its 8-byte alignment (24)" $
              -- gl-block pads the record to a multiple of its base alignment (8);
              -- the SPIR-V block extent below is the unpadded 20.
              sizeOf wide @?= 24
          , testCase "push-constant range from reflection ends at 20" $ do
              -- 16 + 4: tag (a uint) ends at 20, so each int64 before it took 8
              -- bytes — a 4-byte misclassification would put the end at 12.
              m <- load "test/fixtures/wide.comp.spv"
              case pushConstantRanges m of
                [r] -> (r.offset, r.size) @?= (0, 20)
                other -> assertFailure ("expected one range, got " <> show (length other))
          , testCase "Wide's std430 Sig has 8-byte int slots at 0/8, uint at 16" $
              layoutSig @Wide
                @?= normalize
                  ( fromFields
                      Std430Layout
                      Nothing
                      [ ("hi", Scalar STUInt64)
                      , ("lo", Scalar STInt64)
                      , ("tag", Scalar STUInt)
                      ]
                  )
          ]
      , testGroup
          "buffer_reference (BDA)"
          [ testCase "Node's std430 Sig has DeviceAddress slots at 32/40, size 64" $
              -- boundsMin@0, boundsMax@16, left@32, right@40, primCount@48; size 64.
              layoutSig @Node
                @?= normalize
                  ( fromFields
                      Std430Layout
                      Nothing
                      [ ("boundsMin", Vector STFloat 4)
                      , ("boundsMax", Vector STFloat 4)
                      , ("left", Scalar STAddress)
                      , ("right", Scalar STAddress)
                      , ("primCount", Scalar STUInt)
                      ]
                  )
          , testCase "DeviceAddress fields land at std430 byte offsets 32 and 40" $ do
              let n = Node (vec4 0 0 0 0) (vec4 0 0 0 0) (DeviceAddress 0xCAFE) (DeviceAddress 0xBEEF) 0
              (a32, a40) <- allocaBytes (sizeOf (Std430 n)) $ \p -> do
                poke (castPtr p) (Std430 n)
                (,) <$> (peekByteOff p 32 :: IO Word64) <*> (peekByteOff p 40 :: IO Word64)
              (a32, a40) @?= (0xCAFE, 0xBEEF)
          , testCase "push-constant block is a single 8-byte device address" $ do
              m <- load "test/fixtures/bda.comp.spv"
              case pushConstantRanges m of
                [r] -> (r.offset, r.size) @?= (0, 8)
                other -> assertFailure ("expected one range, got " <> show (length other))
          ]
      , testGroup
          "SSBO array-of-struct element (std430)"
          [ testCase "element record alignment is vec3 (16)" $
              alignment particle @?= 16
          , testCase "fields land at the shader's std430 offsets" $ do
              -- ssbo-struct.comp: position@0, velocity@16, mass@24, flags@28
              let n = sizeOf particle
              (vx, m, fl) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) particle
                (,,)
                  <$> (peekByteOff ptr 16 :: IO Float)
                  <*> (peekByteOff ptr 24 :: IO Float)
                  <*> (peekByteOff ptr 28 :: IO Word32)
              vx @?= 3.0
              m @?= 1.5
              fl @?= 7
          , testCase "element size is std430-padded (32)" $
              sizeOf particle @?= 32
          ]
      , testGroup
          "nested struct as a field (shared across layouts)"
          [ testCase "nested element record offsets (Material)" $ do
              let n = sizeOf mat
              (al, em) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) mat
                (,) <$> (peekByteOff ptr 0 :: IO Float) <*> (peekByteOff ptr 16 :: IO Float)
              alignment mat @?= 16
              sizeOf mat @?= 32
              al @?= 1.0
              em @?= 2.0
          , testCase "parent embeds the nested struct at its std140 offsets" $ do
              -- nested.comp: sun@0 (albedo@0, emission@16), tint@32
              let n = sizeOf scene
              (al, em, ti) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) scene
                (,,)
                  <$> (peekByteOff ptr 0 :: IO Float)
                  <*> (peekByteOff ptr 16 :: IO Float)
                  <*> (peekByteOff ptr 32 :: IO Float)
              alignment scene @?= 16
              sizeOf scene @?= 48
              al @?= 1.0
              em @?= 2.0
              ti @?= 3.0
          ]
      , testGroup
          "fixed-size array fields (Array n a)"
          [ testCase "std140 rounds element stride up to 16" $ do
              -- Kernel140: taps[4] vec4 @0 (stride 16), weights[4] float @64 (stride 16)
              let n = sizeOf k140
              (t0, t1, w0, w1, w3) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) k140
                (,,,,)
                  <$> (peekByteOff ptr 0 :: IO Float)
                  <*> (peekByteOff ptr 16 :: IO Float)
                  <*> (peekByteOff ptr 64 :: IO Float)
                  <*> (peekByteOff ptr 80 :: IO Float) -- std140 stride 16
                  <*> (peekByteOff ptr 112 :: IO Float)
              alignment k140 @?= 16
              sizeOf k140 @?= 128
              (t0, t1) @?= (1, 2)
              (w0, w1, w3) @?= (10, 20, 40)
          , testCase "std430 packs the float array tightly (stride 4)" $ do
              -- Kernel430: taps[4] vec4 @0, weights[4] float @64 (stride 4)
              let n = sizeOf k430
              (w0, w1, w3) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) k430
                (,,)
                  <$> (peekByteOff ptr 64 :: IO Float)
                  <*> (peekByteOff ptr 68 :: IO Float) -- std430 stride 4
                  <*> (peekByteOff ptr 76 :: IO Float)
              sizeOf k430 @?= 80
              (w0, w1, w3) @?= (10, 20, 40)
          ]
      , testGroup
          "multi-dimensional array field (Array h (Array w a))"
          [ testCase "std430 nested strides (inner 4, outer 16)" $ do
              -- Grid430: head@0, grid[3][4] @16; grid[i][j] @ 16 + i*16 + j*4
              let n = sizeOf g430
              (h, g00, g01, g10, g23) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) g430
                (,,,,)
                  <$> (peekByteOff ptr 0 :: IO Float)
                  <*> (peekByteOff ptr 16 :: IO Float)
                  <*> (peekByteOff ptr 20 :: IO Float)
                  <*> (peekByteOff ptr 32 :: IO Float)
                  <*> (peekByteOff ptr 60 :: IO Float)
              sizeOf g430 @?= 64
              h @?= 9
              (g00, g01, g10, g23) @?= (1, 2, 5, 12)
          , testCase "std140 nested strides (inner 16, outer 64)" $ do
              -- Grid140: head@0, grid[3][4] @16; grid[i][j] @ 16 + i*64 + j*16
              let n = sizeOf g140
              (g00, g01, g10) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                poke (castPtr ptr) g140
                (,,)
                  <$> (peekByteOff ptr 16 :: IO Float)
                  <*> (peekByteOff ptr 32 :: IO Float) -- inner stride 16
                  <*> (peekByteOff ptr 80 :: IO Float) -- outer stride 64
              sizeOf g140 @?= 208
              (g00, g01, g10) @?= (1, 2, 5)
          ]
      , testGroup
          "runtime-sized array (Storable.Vector at the std430 stride)"
          [ testCase "elements placed at the array stride (32 > element size 20)" $ do
              let
                pads = VS.fromList [Pad (vec4 1 0 0 0) 2, Pad (vec4 9 0 0 0) 8]
                stride = Array.std430Stride (Proxy :: Proxy Pad)
                n = 2 * stride
              (a0, b0, a1, b1) <- allocaBytes n $ \ptr -> do
                fillBytes ptr 0 n
                Array.pokeStd430 ptr pads
                (,,,)
                  <$> (peekByteOff ptr 0 :: IO Float)
                  <*> (peekByteOff ptr 16 :: IO Float)
                  <*> (peekByteOff ptr 32 :: IO Float) -- second element at stride 32
                  <*> (peekByteOff ptr 48 :: IO Float)
              stride @?= 32
              (a0, b0, a1, b1) @?= (1, 2, 9, 8)
          , testCase "round-trips through pokeStd430/peekStd430" $ do
              let
                pads = VS.fromList [Pad (vec4 1 2 3 4) 5, Pad (vec4 6 7 8 9) 10]
                stride = Array.std430Stride (Proxy :: Proxy Pad)
              rt <- allocaBytes (2 * stride) $ \ptr -> do
                fillBytes ptr 0 (2 * stride)
                Array.pokeStd430 ptr pads
                Array.peekStd430 2 ptr
              VS.toList rt @?= VS.toList pads
          ]
      , testGroup
          "specialization constants from reflection"
          [ testCase "reflects ids and names (ascending, non-contiguous)" $ do
              m <- load "test/fixtures/spec.comp.spv"
              specializationConstants m
                @?= [(0, Just "count"), (3, Just "scale")]
          , testCase "map entries keep real ids but pack tightly" $ do
              m <- load "test/fixtures/spec.comp.spv"
              let es = toL (specializationMapEntries m)
              map (\e -> (e.constantID, e.offset, e.size)) es
                @?= [(0, 0, 4), (3, 4, 4)]
          ]
      , testCase "descriptor set layout from reflection" $ do
          m <- load "test/fixtures/julia.comp.spv"
          case descriptorSetLayoutInfos m of
            [(setNo, info)] -> do
              setNo @?= 0
              let bs = toL info.bindings
              map (.binding) bs @?= [0, 1]
              map (.descriptorType) bs
                @?= [Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER, Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER]
              all ((== Vk.SHADER_STAGE_COMPUTE_BIT) . (.stageFlags)) bs @? "all compute stage"
            other -> assertFailure ("expected one set, got " <> show (length other))
      , testGroup
          "vertex input from reflection"
          [ testCase "attributes: format + tightly-packed offset" $ do
              m <- load "test/fixtures/tri.vert.spv"
              let attrs = vertexInputAttributes m
              map (\a -> (a.location, a.format, a.offset)) attrs
                @?= [ (0, Vk.FORMAT_R32G32B32_SFLOAT, 0)
                    , (1, Vk.FORMAT_R32G32_SFLOAT, 12)
                    , (2, Vk.FORMAT_R32G32B32A32_SFLOAT, 20)
                    ]
          , testCase "binding stride is packed size" $ do
              m <- load "test/fixtures/tri.vert.spv"
              (vertexInputBinding m).stride @?= 36
          , testCase "vertexInputState packs the reflected binding + attributes" $ do
              m <- load "test/fixtures/tri.vert.spv"
              let vis = vertexInputState m
              map (.stride) (toL vis.vertexBindingDescriptions) @?= [36]
              map (.offset) (toL vis.vertexAttributeDescriptions) @?= [0, 12, 20]
          , testCase "vertexInputState is empty when a module declares no vertex inputs" $ do
              m <- load "test/fixtures/julia.comp.spv"
              let vis = vertexInputState m
              null (toL vis.vertexBindingDescriptions) @? "no bindings"
              null (toL vis.vertexAttributeDescriptions) @? "no attributes"
          ]
      , testGroup
          "type-level signatures"
          [ testCase "a record's Sig reflects back to its value-level offset map (the oracle)" $
              -- Push: mat4 @0, vec2 @64, float @72, int @76, size 80.
              layoutSig @Push
                @?= normalize
                  ( fromFields
                      Std430Layout
                      Nothing
                      [ ("transform", Matrix STFloat 4 4)
                      , ("offset", Vector STFloat 2)
                      , ("scale", Scalar STFloat)
                      , ("count", Scalar STInt)
                      ]
                  )
          , testCase "layout-equivalent layouts promote to the SAME Sig (mat4 == vec4[4] == float[16])" $ do
              layoutSig @EquivMat4 @?= layoutSig @EquivVec4x4
              layoutSig @EquivVec4x4 @?= layoutSig @EquivFloat16
          , testCase "Fits accepts layout-equivalent layouts at the type level" $ do
              -- These only type-check because the 'Fits' constraints are satisfied.
              mat4FitsVec4x4 @?= True
              vec4x4FitsFloat16 @?= True
              pushFitsItself @?= True
          ]
      , testGroup
          "buffer (structural views)"
          [ testCase "Storable size matches the layout signature size" $
              (layoutSig @PairA).size @?= Just (sizeOf (undefined :: PairA))
          , testCase "round-trips the same record" $ do
              let a = PairA (vec4 9 8 7 6) (vec4 5 4 3 2)
              buf <- newBuffer a
              a' <- readBuffer buf
              a' @?= a
          , testCase "write one layout, read a layout-compatible one back (typed view)" $ do
              buf <- newBuffer (PairA (vec4 1 2 3 4) (vec4 5 6 7 8))
              -- Fits (Sig PairB) (Sig PairA) holds because the layouts are equivalent.
              PairB x y z w <- readBuffer buf
              (x, y, z, w) @?= (vec2 1 2, vec2 3 4, vec2 5 6, vec2 7 8)
          ]
      , testGroup
          "buffer (runtime-array element access)"
          [ testCase "writes and reads back array elements by index" $ do
              -- ArrayOf (Sig PairA): a runtime PairA[] (stride 32, base 0).
              buf <- allocArrayBuffer @(ArrayOf (Sig PairA)) 2
              let
                p0 = PairA (vec4 1 2 3 4) (vec4 5 6 7 8)
                p1 = PairA (vec4 9 10 11 12) (vec4 13 14 15 16)
              writeBufferElem buf 0 p0
              writeBufferElem buf 1 p1
              e0 <- readBufferElem buf 0
              e1 <- readBufferElem buf 1
              (e0, e1) @?= (p0, p1)
          , testCase "reads an element back through a layout-compatible record (typed view)" $ do
              buf <- allocArrayBuffer @(ArrayOf (Sig PairA)) 2
              writeBufferElem buf 0 (PairA (vec4 1 2 3 4) (vec4 5 6 7 8))
              writeBufferElem buf 1 (PairA (vec4 10 20 30 40) (vec4 50 60 70 80))
              -- FitsTail (Sig PairB) (ArrayOf (Sig PairA)) holds: the element layout is equivalent.
              PairB x y z w <- readBufferElem buf 1
              (x, y, z, w) @?= (vec2 10 20, vec2 30 40, vec2 50 60, vec2 70 80)
          ]
      , testGroup
          "stage interface matching"
          [ testCase "vertex outputs reflect at locations 0 (vec3) and 1 (vec2)" $ do
              vm <- load "test/fixtures/mesh.vert.spv"
              let outs = (stageInfoOf vm).outputs
              map fst outs @?= [0, 1]
              map (length . (.slots) . snd) outs @?= [3, 2]
          , testCase "vertex outputs match fragment inputs (value oracle)" $ do
              vm <- load "test/fixtures/mesh.vert.spv"
              fm <- load "test/fixtures/mesh.frag.spv"
              matchInterface (stageInfoOf vm) (stageInfoOf fm) @?= Right ()
          , testCase "a mismatched interface is rejected (value oracle)" $ do
              vm <- load "test/fixtures/mesh.vert.spv"
              fm <- load "test/fixtures/mesh.frag.spv"
              -- frag outputs (vec4 @loc0) cannot satisfy the vertex's own inputs.
              assertBool "should not match" (isLeft (matchInterface (stageInfoOf fm) (stageInfoOf vm)))
          , testCase "MatchInterface holds at the type level" $
              meshInterfaceOk @?= True
          ]
      , testGroup
          "stage resource linking (shared UBO)"
          [ testCase "the Scene UBO reflects identically from both stages" $ do
              vm <- load "test/fixtures/mesh.vert.spv"
              fm <- load "test/fixtures/mesh.frag.spv"
              lookup (0, 0) (stageInfoOf vm).resources
                @?= lookup (0, 0) (stageInfoOf fm).resources
          , testCase "linking unions resources, push and the external interface" $ do
              vm <- load "test/fixtures/mesh.vert.spv"
              fm <- load "test/fixtures/mesh.frag.spv"
              case linkStages (stageInfoOf vm) (stageInfoOf fm) of
                Left e -> assertFailure e
                Right linked -> do
                  map fst linked.resources @?= [(0, 0), (0, 1)] -- Scene (shared) + Materials
                  isJust linked.push @?= True -- vertex Model push constant
                  map fst linked.inputs @?= [0, 1, 2] -- vertex attributes
                  map fst linked.outputs @?= [0] -- fragment colour output
          , testCase "a conflicting shared binding is rejected" $ do
              vm <- load "test/fixtures/mesh.vert.spv"
              fm <- load "test/fixtures/mesh.frag.spv"
              let bad =
                    (stageInfoOf fm)
                      { StageInfo.resources = [((0, 0), normalize (fromFields Std140Layout Nothing [("x", Vector STFloat 2)]))]
                      }
              assertBool "should conflict" (isLeft (linkStages (stageInfoOf vm) bad))
          , testCase "CompatibleResources holds at the type level" $
              meshResourcesOk @?= True
          , testCase "the merged layout signature round-trips to the value-level merge" $ do
              vm <- load "test/fixtures/mesh.vert.spv"
              fm <- load "test/fixtures/mesh.frag.spv"
              let info = layoutSigVal (Proxy @MeshLayout) -- reflected from the promoted type
              mergeLayout [vm, fm] @?= Right info -- equals the value-level merge
              map fst info.resources @?= [(0, 0), (0, 1)] -- Scene (shared) + Materials
              isJust info.push @?= True -- vertex Model push constant
          ]
      , testGroup
          "pipeline layout (depth-only vs depth+color from one vertex shader)"
          [ testCase "depth-only: the vertex stage alone" $ do
              vm <- load "test/fixtures/mesh.vert.spv"
              -- Only the vertex-visible resources: Scene UBO at (0,0), vertex stage.
              stageBindings [vm]
                @?= [(0, Vk.SHADER_STAGE_VERTEX_BIT)]
              fmap length (mergedPushConstantRanges [vm]) @?= Right 1 -- the Model push constant
          , testCase "depth+color: vertex+fragment, Scene gains the fragment stage" $ do
              vm <- load "test/fixtures/mesh.vert.spv"
              fm <- load "test/fixtures/mesh.frag.spv"
              stageBindings [vm, fm]
                @?= [ (0, Vk.SHADER_STAGE_VERTEX_BIT .|. Vk.SHADER_STAGE_FRAGMENT_BIT) -- Scene, shared
                    , (1, Vk.SHADER_STAGE_FRAGMENT_BIT) -- Materials, frag-only
                    ]
          ]
      , LayoutSpec.tests
      ]
  where
    -- (binding, stageFlags) across all sets, for the merged pipeline layout.
    stageBindings modules =
      sortOn
        fst
        [ (b.binding, b.stageFlags)
        | (_set, info) <- either error id (mergedDescriptorSetLayoutInfos modules)
        , b <- V.toList info.bindings
        ]
    params =
      Params
        { center = vec2 (-0.8) 0.156
        , resolution = uvec2 512 512
        , escapeRadius = 2.0
        , maxIterations = 1000
        }
    pushc =
      Push
        { transform = identity
        , offset = vec2 1 2
        , scale = 2.5
        , count = 7
        }
    wide =
      Wide
        { hi = 0xDEADBEEFCAFEF00D
        , lo = -42
        , tag = 7
        }
    particle =
      Particle
        { position = vec3 (-1) 0 0
        , velocity = vec2 3 4
        , mass = 1.5
        , flags = 7
        }
    mat =
      Material
        { albedo = vec4 1 0 0 0
        , emission = vec4 2 0 0 0
        }
    scene =
      Scene
        { sun = mat
        , tint = vec4 3 0 0 0
        }
    taps = Array.unsafeFromList [vec4 1 0 0 0, vec4 2 0 0 0, vec4 3 0 0 0, vec4 4 0 0 0] :: Array.Array 4 Vec4
    weights = Array.unsafeFromList [10, 20, 30, 40] :: Array.Array 4 Float
    k140 = Kernel140{taps = taps, weights = weights}
    k430 = Kernel430{taps = taps, weights = weights}
    grid =
      Array.unsafeFromList (map Array.unsafeFromList [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]])
        :: Array.Array 3 (Array.Array 4 Float)
    g430 = Grid430{head = vec4 9 0 0 0, grid = grid}
    g140 = Grid140{head = vec4 9 0 0 0, grid = grid}
    toL = foldr (:) []
