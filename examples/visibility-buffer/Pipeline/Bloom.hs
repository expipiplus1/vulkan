{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}

{-| The bloom pyramid pipelines.

Progressive downsample and additive upsample over a mip chain (see
"Pipeline.Bloom.Downsample" / "Pipeline.Bloom.Upsample"). Both share a set layout — a
combined image sampler (blur source, binding 0) and a storage image (target, binding
1) — differing only in their push constant.
-}
module Pipeline.Bloom
  ( Pipeline (..)
  , Bloom (..)
  , allocateBloom
  , allocateSet
  , pushDownsample
  , pushUpsample
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, sizeOf)
import UnliftIO.Foreign (with)
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Shader (shaderModuleStage)
import Vulkan.Utils.SpirV.Descriptors (pushConstantRanges, singleDescriptorSetLayoutInfo)
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.Bloom.Downsample as Downsample
import qualified Pipeline.Bloom.Upsample as Upsample

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , pipelineLayout :: Vk.PipelineLayout
  , descriptorSetLayout :: Vk.DescriptorSetLayout
  }

data Bloom = Bloom
  { down :: Pipeline
  , up :: Pipeline
  }

allocateBloom :: Vk.Device -> ResourceT IO Bloom
allocateBloom dev = do
  down <- buildCompute dev Downsample.code
  up <- buildCompute dev Upsample.code
  pure Bloom{down, up}

buildCompute :: Vk.Device -> ByteString -> ResourceT IO Pipeline
buildCompute dev code = do
  reflected <- reflectBytes code
  setLayoutInfo <- either fail pure (singleDescriptorSetLayoutInfo reflected)
  (_, descriptorSetLayout) <- Vk.withDescriptorSetLayout dev setLayoutInfo Nothing allocate
  (_, pipelineLayout) <-
    Vk.withPipelineLayout
      dev
      zero{Vk.setLayouts = [descriptorSetLayout], Vk.pushConstantRanges = V.fromList (pushConstantRanges reflected)}
      Nothing
      allocate
  (_, stage) <- shaderModuleStage dev Vk.SHADER_STAGE_COMPUTE_BIT Nothing code
  let createInfo = zero{Vk.layout = pipelineLayout, Vk.stage = stage} :: Vk.ComputePipelineCreateInfo '[]
  (_, (_, [pipeline])) <- Vk.withComputePipelines dev zero [SomeStruct createInfo] Nothing allocate
  pure Pipeline{pipeline, pipelineLayout, descriptorSetLayout}

{- | A descriptor set for one down/upsample step.

The blur source sampled through @sampler@ (binding 0) and the target mip as a storage
image (binding 1). All views stay in @GENERAL@ (sampling is legal there, so no layout
churn per mip).
-}
allocateSet :: Vk.Device -> Pipeline -> Vk.Sampler -> Vk.ImageView -> Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateSet dev pl sampler srcView dstView = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE 1
            ]
        }
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [pl.descriptorSetLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets
    dev
    [ SomeStruct
        zero
          { Vk.dstSet = set
          , Vk.dstBinding = 0
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
          , Vk.descriptorCount = 1
          , Vk.imageInfo = [zero{Vk.sampler = sampler, Vk.imageView = srcView, Vk.imageLayout = Vk.IMAGE_LAYOUT_GENERAL} :: Vk.DescriptorImageInfo]
          }
    , SomeStruct
        zero
          { Vk.dstSet = set
          , Vk.dstBinding = 1
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
          , Vk.descriptorCount = 1
          , Vk.imageInfo = [zero{Vk.imageView = dstView, Vk.imageLayout = Vk.IMAGE_LAYOUT_GENERAL} :: Vk.DescriptorImageInfo]
          }
    ]
    []
  pure set

-- | Push the Karis flag (1 on the first, full-resolution downsample).
pushDownsample :: Vk.CommandBuffer -> Vk.PipelineLayout -> Bool -> IO ()
pushDownsample cb layout karis =
  pushCompute cb layout (if karis then 1 else 0 :: Word32)

-- | Push the upsample tent-filter radius (in the source mip's texture coordinates).
pushUpsample :: Vk.CommandBuffer -> Vk.PipelineLayout -> Float -> IO ()
pushUpsample cb layout radius = pushCompute cb layout radius

-- | Push a single scalar to the compute stage at offset 0.
pushCompute :: (Storable a) => Vk.CommandBuffer -> Vk.PipelineLayout -> a -> IO ()
pushCompute cb layout x =
  with x \p ->
    Vk.cmdPushConstants cb layout Vk.SHADER_STAGE_COMPUTE_BIT 0 (fromIntegral $ sizeOf x) (castPtr p)
