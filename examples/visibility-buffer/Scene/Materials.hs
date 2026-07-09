{-| The scene's material table — the single source of truth for surface shading.

A small CPU-defined set of PBR-lite materials uploaded to an SSBO, like "Lights". The
resolve indexes it by instanceId: cave cubes share the grey materials
(@hash % greyCount@), the knot's two instances pick @knotBase + kinst@.
-}
module Scene.Materials
  ( Shade.Material (..)
  , material
  , materials
  , count
  , greyCount
  , knotBase
  , bufferBytes
  , upload
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word32)
import Foreign.Storable (sizeOf)
import Geomancy (Vec3, vec3, vec4)
import qualified Geomancy.Vec4 as Vec4
import qualified Vulkan.Core10 as Vk

import qualified Pipeline.Shade as Shade
import qualified Upload

-- | A PBR-lite material: linear albedo, metalness, roughness.
material :: Vec3 -> Float -> Float -> Shade.Material
material albedo metalness roughness =
  Shade.Material (Vec4.fromVec3 albedo 0) (vec4 metalness roughness 0 0)

{- | The material list.

'greyCount' matte grey rock materials (the cubes share these), then the knot's
terracotta (kinst 0) and gold (kinst 1).
-}
materials :: [Shade.Material]
materials =
  [ material (vec3 0.30 0.30 0.32) 0.0 0.90
  , material (vec3 0.40 0.39 0.38) 0.0 0.85
  , material (vec3 0.50 0.49 0.47) 0.0 0.90
  , material (vec3 0.58 0.56 0.53) 0.0 0.82
  , material (vec3 0.66 0.64 0.61) 0.0 0.88
  , material (vec3 0.90 0.52 0.42) 0.0 0.55 -- terracotta (knot inst 0)
  , material (vec3 1.00 0.80 0.34) 1.0 0.25 -- gold (knot inst 1)
  ]

-- | Grey (cube) material count; cubes index @hash % greyCount@.
greyCount :: Word32
greyCount = 5

-- | Index of the first knot material (kinst adds to this).
knotBase :: Word32
knotBase = 5

count :: Word32
count = fromIntegral (length materials)

-- | The SSBO stride: one @Material { vec4 albedo; vec4 pbr; }@.
materialBytes :: Int
materialBytes = sizeOf (undefined :: Shade.Material)

-- | Bytes for the SSBO: two @vec4@ (albedo+pad, metal/rough+pad) per material.
bufferBytes :: Vk.DeviceSize
bufferBytes = fromIntegral (length materials * materialBytes)

-- | Fill the material SSBO.
upload :: (MonadIO m) => Vk.CommandBuffer -> Vk.Buffer -> m ()
upload cb buffer = Upload.slice cb buffer 0 materials
