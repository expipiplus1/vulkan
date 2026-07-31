{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-| The shade (deferred resolve) compute pipeline.

Maps visibility-buffer ids to HDR colour ("Pipeline.Shade.Shader"). The set-0 layout
is reflected from 'Shade.Shader.code' via 'allocateCompute', so it can't
drift from the shader. Callers allocate one descriptor set per render target with
'allocateDescriptorSet'.
-}
module Pipeline.Shade
  ( Camera (..)
  , Light (..)
  , Vertex (..)
  , Material (..)
  , Tuning (..)
  , defaultTuning
  , allocatePipeline
  , allocateDescriptorSet
  , workgroup
  ) where

import Control.Monad.Trans.Resource (ResourceT)
import Data.Word (Word32)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite, combinedImageSamplerWrite, imageWrite)
import Vulkan.Utils.Pipeline (Pipeline)
import qualified Vulkan.Utils.Pipeline as Pipeline
import Vulkan.Utils.SpirV.Pipeline (allocateCompute)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)

import qualified Pipeline.Shade.Shader as Shader
import Pipeline.Shadow.Params (Params)

-- Generate geomancy-backed records (with std140/std430 'Storable') for the shader's
-- blocks — notably the @Camera@ push constant — from the same SPIR-V the runtime loads.
reflectShaderTypesBytes Shader.code

-- | Workgroup size on each axis (matches @local_size_x\/y@ in the shader).
workgroup :: Word32
workgroup = 8

{- | The receiver-side shading knobs of the @Camera@ push.

Nothing is baked against them (unlike 'Params'), so they are free to change per frame.
-}
data Tuning = Tuning
  { ambient :: Float
  -- ^ Uniform environment radiance.
  , indirect :: Float
  -- ^ Crude indirect bounce: the fraction of irradiance seen as environment.
  , bleed :: Float
  -- ^ Light-bleed reduction: the Chebyshev bound below this reads as fully shadowed.
  , shadowBias :: Float
  -- ^ Receiver bias in normalized distance, against self-shadow acne.
  , normalBias :: Float
  -- ^ Offset along the normal, in metres, before the cube lookup.
  }
  deriving (Eq, Ord, Show)

{- | Tuned for the cave.

'normalBias' is about a third of the knot's tube radius; 'shadowBias' is in distance
already normalized by @Params.far@, so a change of scene scale leaves it alone.
-}
defaultTuning :: Tuning
defaultTuning =
  Tuning
    { ambient = 1 / 256
    , indirect = 0.65
    , bleed = 0.15
    , shadowBias = 0.0012
    , normalBias = 0.025
    }

-- | The pipeline; its set 0 layout reflected from 'Shader.code', @params@ specialized in.
allocatePipeline :: Vk.Device -> Params -> ResourceT IO Pipeline
allocatePipeline dev params = allocateCompute dev params Shader.code

{- | A descriptor set for the resolve.

Visibility (0) and colour (1) storage images, the shared vertex SSBO (2, DAIS), the
lights SSBO (3), the EVSM shadow cube-array sampler (4), the object table SSBO (5),
the material table SSBO (6), the mesh table SSBO (7), and the half-res SSAO factor
(8, upsampled by the shared linear @sampler@).
-}
allocateDescriptorSet :: Vk.Device -> Pipeline -> Vk.ImageView -> Vk.ImageView -> Vk.Buffer -> Vk.Buffer -> Vk.Sampler -> Vk.ImageView -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateDescriptorSet dev pl visView colorView verts lights sampler shadowCube objects materials meshes aoView = do
  set <- Pipeline.allocateSet dev pl 0
  Vk.updateDescriptorSets
    dev
    [ imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL visView
    , imageWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL colorView
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER verts
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER lights
    , combinedImageSamplerWrite set 4 sampler shadowCube Vk.IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
    , bufferWrite set 5 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER objects
    , bufferWrite set 6 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER materials
    , bufferWrite set 7 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER meshes
    , combinedImageSamplerWrite set 8 sampler aoView Vk.IMAGE_LAYOUT_GENERAL
    ]
    []
  pure set
