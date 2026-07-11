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

{-| The SSAO compute pipelines.

Three passes ("Pipeline.Ssao.Shader"): @normals@ resolves half-res world
normals + view depth from the visibility buffer, @ao@ marches the depth
pyramid around each pixel for raw obscurance, and @blur@ runs twice (X then Y)
as a separable cross-bilateral pair before the resolve samples the factor.
Set layouts are reflected from the shaders, so they can't drift.
-}
module Pipeline.Ssao
  ( Pipeline (..)
  , Ssao (..)
  , Prepass (..)
  , Ao (..)
  , Blur (..)
  , allocateSsao
  , allocateNormalsSet
  , allocateAoSet
  , allocateBlurSet
  , push
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable)
import qualified Geomancy
import Graphics.Gl.Block (Std430 (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (bufferWrite, combinedImageSamplerWrite, imageWrite)
import Vulkan.Utils.SpirV.Descriptors (pushConstantsSize)
import Vulkan.Utils.SpirV.Pipeline (allocateComputePipeline, allocateReflectedLayout, singleSetLayout)
import qualified Vulkan.Utils.SpirV.Pipeline
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Utils.SpirV.TH (reflectShaderTypesBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Ssao.Shader as Shader

-- Generate geomancy-backed records for the shaders' blocks — the @Prepass@,
-- @Ao@ and @Blur@ push constants — from the same SPIR-V the runtime loads.
reflectShaderTypesBytes Shader.normalsCode
reflectShaderTypesBytes Shader.aoCode
reflectShaderTypesBytes Shader.blurCode

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , layout :: Vk.PipelineLayout
  , setLayout :: Vk.DescriptorSetLayout
  , pushSize :: Word32
  -- ^ Reflected push-constant range size; push exactly this many bytes.
  }

-- | The normal prepass, the AO gather, and the bilateral blur (used for both axes).
data Ssao = Ssao
  { normals :: Pipeline
  , ao :: Pipeline
  , blur :: Pipeline
  }

allocateSsao :: Vk.Device -> ResourceT IO Ssao
allocateSsao dev = do
  normals <- buildCompute dev Shader.normalsCode
  ao <- buildCompute dev Shader.aoCode
  blur <- buildCompute dev Shader.blurCode
  pure Ssao{normals, ao, blur}

buildCompute :: Vk.Device -> ByteString -> ResourceT IO Pipeline
buildCompute dev code = do
  reflected <- reflectBytes code
  (_, reflectedLayout) <- allocateReflectedLayout dev [reflected]
  setLayout <- singleSetLayout reflectedLayout
  (_, pipeline) <- allocateComputePipeline dev reflectedLayout () (reflected, code)
  pure Pipeline{pipeline, layout = reflectedLayout.pipelineLayout, setLayout, pushSize = pushConstantsSize reflected}

{- | The prepass set: the visibility buffer (0), the normal target (1), and the
DAIS tables — vertices (2), objects (3), meshes (4).
-}
allocateNormalsSet :: Vk.Device -> Pipeline -> Vk.ImageView -> Vk.ImageView -> Vk.Buffer -> Vk.Buffer -> Vk.Buffer -> ResourceT IO Vk.DescriptorSet
allocateNormalsSet dev pl visView normalView verts objects meshes = do
  set <- allocateSet dev pl [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE 2, Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER 3]
  Vk.updateDescriptorSets
    dev
    [ imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL visView
    , imageWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL normalView
    , bufferWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER verts
    , bufferWrite set 3 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER objects
    , bufferWrite set 4 Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER meshes
    ]
    []
  pure set

{- | The gather set: the whole depth pyramid sampled across levels (0), the
prepass normals (1), and the AO target (2).
-}
allocateAoSet :: Vk.Device -> Pipeline -> Vk.Sampler -> Vk.ImageView -> Vk.ImageView -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateAoSet dev pl sampler pyramidView normalView aoView = do
  set <- allocateSet dev pl [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1, Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE 2]
  Vk.updateDescriptorSets
    dev
    [ combinedImageSamplerWrite set 0 sampler pyramidView Vk.IMAGE_LAYOUT_GENERAL
    , imageWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL normalView
    , imageWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL aoView
    ]
    []
  pure set

{- | One axis of the blur: the prepass normals + depth (0) for the edge
weights, the source (1) and target (2) factors. Allocated twice, with the
source/target images swapped, for the X-then-Y ping-pong.
-}
allocateBlurSet :: Vk.Device -> Pipeline -> Vk.ImageView -> Vk.ImageView -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateBlurSet dev pl normalView srcView dstView = do
  set <- allocateSet dev pl [Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE 3]
  Vk.updateDescriptorSets
    dev
    [ imageWrite set 0 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL normalView
    , imageWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL srcView
    , imageWrite set 2 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL dstView
    ]
    []
  pure set

-- | Push a reflected push-constant record (exactly 'pushSize' bytes, COMPUTE stage).
push :: (Storable a, MonadIO m) => Vk.CommandBuffer -> Pipeline -> a -> m ()
push cb pl pc =
  liftIO $ with pc \p ->
    Vk.cmdPushConstants cb pl.layout Vk.SHADER_STAGE_COMPUTE_BIT 0 pl.pushSize (castPtr p)

allocateSet :: Vk.Device -> Pipeline -> V.Vector Vk.DescriptorPoolSize -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl poolSizes = do
  (_, pool) <- Vk.withDescriptorPool dev zero{Vk.maxSets = 1, Vk.poolSizes = poolSizes} Nothing allocate
  V.head <$> Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [pl.setLayout]}
