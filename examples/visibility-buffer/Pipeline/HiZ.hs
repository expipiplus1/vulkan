{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

{-| The depth-pyramid pipelines.

Wraps the "Pipeline.HiZ.Shader" min-reduces: one dispatch per mip while the levels
are big, then 'tail' finishes everything from a ≤32×32 level in a single dispatch
(a serial tail of tiny levels costs a pipeline drain each and no work). The
finished pyramid feeds the next frame's occlusion test ("Pipeline.Cull"). All
views stay in @GENERAL@, like the bloom chain.
-}
module Pipeline.HiZ
  ( Pipeline (..)
  , HiZ (..)
  , allocateHiZ
  , allocateSet
  , allocateTailSet
  , allocateChainView
  , format
  , mipCount
  , tailFits
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Vulkan.CStruct.Extends (SomeStruct (..))
import qualified Vulkan.Core10 as Vk
import Vulkan.Utils.Descriptors (combinedImageSamplerWrite, imageWrite)
import Vulkan.Utils.SpirV.Pipeline (allocateComputePipeline, allocateReflectedLayout, singleSetLayout)
import qualified Vulkan.Utils.SpirV.Pipeline
import Vulkan.Utils.SpirV.Reflect (reflectBytes)
import Vulkan.Zero (zero)

import qualified Pipeline.HiZ.Shader as Shader

data Pipeline = Pipeline
  { pipeline :: Vk.Pipeline
  , layout :: Vk.PipelineLayout
  , setLayout :: Vk.DescriptorSetLayout
  }

-- | The per-level reduce and the fused tail.
data HiZ = HiZ
  { reduce :: Pipeline
  , tail :: Pipeline
  }

allocateHiZ :: Vk.Device -> ResourceT IO HiZ
allocateHiZ dev = do
  reduce <- buildCompute dev Shader.code
  tail_ <- buildCompute dev Shader.tailCode
  pure HiZ{reduce, tail = tail_}

buildCompute :: Vk.Device -> ByteString -> ResourceT IO Pipeline
buildCompute dev code = do
  reflected <- reflectBytes code
  (_, reflectedLayout) <- allocateReflectedLayout dev [reflected]
  setLayout <- singleSetLayout reflectedLayout
  (_, pipeline) <- allocateComputePipeline dev reflectedLayout () (reflected, code)
  pure Pipeline{pipeline, layout = reflectedLayout.pipelineLayout, setLayout}

-- | A set for one reduce step: the source sampled (0), the target mip stored (1).
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
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [pl.setLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets
    dev
    [ combinedImageSamplerWrite set 0 sampler srcView Vk.IMAGE_LAYOUT_GENERAL
    , imageWrite set 1 Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE Vk.IMAGE_LAYOUT_GENERAL dstView
    ]
    []
  pure set

{- | The fused-tail set: the last reduced level sampled (0), the remaining mips
stored (1, an array of 5 — pad short chains with any valid view; @levels@ guards
the writes).
-}
allocateTailSet :: Vk.Device -> Pipeline -> Vk.Sampler -> Vk.ImageView -> V.Vector Vk.ImageView -> ResourceT IO Vk.DescriptorSet
allocateTailSet dev pl sampler srcView dstViews = do
  (_, pool) <-
    Vk.withDescriptorPool
      dev
      zero
        { Vk.maxSets = 1
        , Vk.poolSizes =
            [ Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER 1
            , Vk.DescriptorPoolSize Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE (fromIntegral (V.length dstViews))
            ]
        }
      Nothing
      allocate
  sets <- Vk.allocateDescriptorSets dev zero{Vk.descriptorPool = pool, Vk.setLayouts = [pl.setLayout]}
  let set = V.head sets
  Vk.updateDescriptorSets
    dev
    [ combinedImageSamplerWrite set 0 sampler srcView Vk.IMAGE_LAYOUT_GENERAL
    , SomeStruct
        zero
          { Vk.dstSet = set
          , Vk.dstBinding = 1
          , Vk.descriptorCount = fromIntegral (V.length dstViews)
          , Vk.descriptorType = Vk.DESCRIPTOR_TYPE_STORAGE_IMAGE
          , Vk.imageInfo = V.map (\v -> zero{Vk.imageView = v, Vk.imageLayout = Vk.IMAGE_LAYOUT_GENERAL}) dstViews
          }
    ]
    []
  pure set

-- | Does a level fit the fused tail's shared-memory tile?
tailFits :: Vk.Extent2D -> Bool
tailFits (Vk.Extent2D w h) = w <= 32 && h <= 32

-- | Pyramid texel format: one reverse-Z depth per texel (@r32f@ in the shaders).
format :: Vk.Format
format = Vk.FORMAT_R32_SFLOAT

-- | A view spanning the whole mip chain, for the cull's explicit-LOD sampling.
allocateChainView :: Vk.Device -> Vk.Image -> Int -> ResourceT IO Vk.ImageView
allocateChainView dev image mips = snd <$> Vk.withImageView dev info Nothing allocate
  where
    info =
      zero
        { Vk.image = image
        , Vk.viewType = Vk.IMAGE_VIEW_TYPE_2D
        , Vk.format = format
        , Vk.subresourceRange = Vk.ImageSubresourceRange Vk.IMAGE_ASPECT_COLOR_BIT 0 (fromIntegral mips) 0 1
        }
        :: Vk.ImageViewCreateInfo '[]

{- | Pyramid levels for a base extent, down to 1×1.

A full chain, so a screen-filling rect still resolves to texels that cover it —
stopping early would leave the top mip's corner samples missing the middle.
-}
mipCount :: Vk.Extent2D -> Int
mipCount (Vk.Extent2D w h) = length (takeWhile (>= 1) (iterate (`div` 2) (fromIntegral (max w h) :: Int)))
