{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Pipeline
  ( createPipeline
  , createRTPipelineLayout
  , createRTDescriptorSetLayout
  , createRTDescriptorSets
  , createShaderBindingTable
  , Pipeline.createRenderPass
  ) where

import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Foldable                  ( traverse_ )
import qualified Data.Vector                   as V
import           Foreign                        ( nullPtr )
import           MonadVulkan
import           Vulkan.CStruct.Extends
import           Vulkan.Core10                 as Vk
                                         hiding ( withBuffer
                                                , withImage
                                                )
import           Vulkan.Extensions.VK_KHR_ray_tracing
import           Vulkan.Utils.ShaderQQ
import           Vulkan.Zero
import           VulkanMemoryAllocator
import Data.Vector (Vector)

-- Create the most vanilla ray tracing pipeline
createPipeline :: PipelineLayout -> V (ReleaseKey, Pipeline)
createPipeline pipelineLayout = do
  (shaderKeys, shaderStages) <- V.unzip <$> sequence [createRayGenerationShader]

  let rtsgci :: RayTracingShaderGroupCreateInfoKHR
      rtsgci = RayTracingShaderGroupCreateInfoKHR
        RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR
        0 -- The index of our general shader
        SHADER_UNUSED_KHR
        SHADER_UNUSED_KHR
        SHADER_UNUSED_KHR
        nullPtr
      shaderGroups = [rtsgci]

  let pipelineCreateInfo :: RayTracingPipelineCreateInfoKHR '[]
      pipelineCreateInfo = zero { stages            = shaderStages
                                , groups            = shaderGroups
                                , maxRecursionDepth = 1
                                , layout            = pipelineLayout
                                }
  (key, (_, ~[rtPipeline])) <- withRayTracingPipelinesKHR'
    zero
    [SomeStruct pipelineCreateInfo]

  traverse_ release shaderKeys

  pure (key, rtPipeline)

createRTPipelineLayout :: DescriptorSetLayout -> V (ReleaseKey, PipelineLayout)
createRTPipelineLayout descriptorSetLayout =
  withPipelineLayout' zero { setLayouts = [descriptorSetLayout] }

createRTDescriptorSetLayout :: V (ReleaseKey, DescriptorSetLayout)
createRTDescriptorSetLayout = withDescriptorSetLayout' zero
  { bindings = [ zero { binding         = 1
                      , descriptorType  = DESCRIPTOR_TYPE_STORAGE_IMAGE
                      , descriptorCount = 1
                      , stageFlags      = SHADER_STAGE_RAYGEN_BIT_KHR
                      }
               ]
  }

createRTDescriptorSets
  :: DescriptorSetLayout -> Vector ImageView -> V (Vector DescriptorSet)
createRTDescriptorSets descriptorSetLayout imageViews = do
  -- Create a descriptor pool
  (_, descriptorPool) <- withDescriptorPool' zero
    { maxSets   = fromIntegral (V.length imageViews)
    , poolSizes = [ DescriptorPoolSize DESCRIPTOR_TYPE_STORAGE_IMAGE
                                       (fromIntegral (V.length imageViews))
                  ]
    }

  -- Allocate a descriptor set from the pool with that layout
  -- Don't use `withDescriptorSets` here as the set will be cleaned up when
  -- the pool is destroyed.
  descriptorSets <- allocateDescriptorSets' zero
    { descriptorPool = descriptorPool
    , setLayouts     = V.replicate (V.length imageViews) descriptorSetLayout
    }

  -- Assign the buffer in this descriptor set
  updateDescriptorSets'
    (V.zipWith
      (\set view -> SomeStruct zero
        { dstSet          = set
        , dstBinding      = 1
        , descriptorType  = DESCRIPTOR_TYPE_STORAGE_IMAGE
        , descriptorCount = 1
        , imageInfo = [ DescriptorImageInfo { sampler     = NULL_HANDLE
                                            , imageView   = view
                                            , imageLayout = IMAGE_LAYOUT_GENERAL
                                            }
                      ]
        }
      )
      descriptorSets
      imageViews
    )
    []

  pure descriptorSets

createRayGenerationShader
  :: V (ReleaseKey, SomeStruct PipelineShaderStageCreateInfo)
createRayGenerationShader = do
  let code = $(compileShaderQ "rgen" [glsl|
        #version 460
        #extension GL_EXT_ray_tracing : require

        layout(binding = 1, set = 0, rgba32f) uniform image2D image;

        void main()
        {
            imageStore(image, ivec2(gl_LaunchIDEXT.xy), vec4(0.5, 0.6, 0.8, 1.0));
        }
      |])

  (key, module') <- withShaderModule' zero { code }
  let shaderStageCreateInfo =
        zero { stage = SHADER_STAGE_RAYGEN_BIT_KHR, module', name = "main" }
  pure (key, SomeStruct shaderStageCreateInfo)

----------------------------------------------------------------
-- Shader binding table
----------------------------------------------------------------

createShaderBindingTable :: Pipeline -> V (ReleaseKey, Buffer)
createShaderBindingTable pipeline = do
  RTInfo {..} <- getRTInfo
  let groupCount      = 1 -- Just a generation shader
      groupHandleSize = rtiShaderGroupHandleSize
      baseAlignment   = rtiShaderGroupBaseAlignment
      sbtSize         = fromIntegral (baseAlignment * groupCount)

  (bufferReleaseKey, (sbtBuffer, sbtAllocation, _sbtAllocationInfo)) <- withBuffer'
    zero { usage = BUFFER_USAGE_TRANSFER_SRC_BIT, size = sbtSize }
    zero
      { requiredFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT
                          .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
      }
  nameObject' sbtBuffer "SBT"

  (memKey, mem) <- withMappedMemory' sbtAllocation
  -- TODO: Fix alignment here
  getRayTracingShaderGroupHandlesKHR' pipeline 0 groupCount sbtSize mem
  release memKey
  pure (bufferReleaseKey, sbtBuffer)

----------------------------------------------------------------
-- Render pass creation
----------------------------------------------------------------

-- | Create a renderpass with a single subpass
createRenderPass :: Format -> V (ReleaseKey, RenderPass)
createRenderPass imageFormat = do
  let
    attachmentDescription :: AttachmentDescription
    attachmentDescription = zero
      { format         = imageFormat
      , samples        = SAMPLE_COUNT_1_BIT
      , loadOp         = ATTACHMENT_LOAD_OP_CLEAR
      , storeOp        = ATTACHMENT_STORE_OP_STORE
      , stencilLoadOp  = ATTACHMENT_LOAD_OP_DONT_CARE
      , stencilStoreOp = ATTACHMENT_STORE_OP_DONT_CARE
      , initialLayout  = IMAGE_LAYOUT_UNDEFINED
      , finalLayout    = IMAGE_LAYOUT_PRESENT_SRC_KHR
      }
    subpass :: SubpassDescription
    subpass = zero
      { pipelineBindPoint = PIPELINE_BIND_POINT_GRAPHICS
      , colorAttachments  =
        [ zero { attachment = 0
               , layout     = IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
               }
        ]
      }
    subpassDependency :: SubpassDependency
    subpassDependency = zero
      { srcSubpass    = SUBPASS_EXTERNAL
      , dstSubpass    = 0
      , srcStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      , srcAccessMask = zero
      , dstStageMask  = PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
      , dstAccessMask = ACCESS_COLOR_ATTACHMENT_READ_BIT
                          .|. ACCESS_COLOR_ATTACHMENT_WRITE_BIT
      }
  withRenderPass' zero { attachments  = [attachmentDescription]
                       , subpasses    = [subpass]
                       , dependencies = [subpassDependency]
                       }

