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

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Foldable                  ( traverse_ )
import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import           Data.Word
import           Foreign                        ( nullPtr )
import           Foreign.Marshal.Utils          ( moveBytes )
import           Foreign.Ptr                    ( Ptr
                                                , plusPtr
                                                )
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
  :: DescriptorSetLayout -> Word32 -> V (Vector DescriptorSet)
createRTDescriptorSets descriptorSetLayout numDescriptorSets = do
  -- Create a descriptor pool
  (_, descriptorPool) <- withDescriptorPool' zero
    { maxSets   = numDescriptorSets
    , poolSizes = [ DescriptorPoolSize DESCRIPTOR_TYPE_STORAGE_IMAGE
                                       numDescriptorSets
                  ]
    }

  -- Allocate a descriptor set from the pool with that layout
  -- Don't use `withDescriptorSets` here as the set will be cleaned up when
  -- the pool is destroyed.
  allocateDescriptorSets' zero
    { descriptorPool = descriptorPool
    , setLayouts     = V.replicate (fromIntegral numDescriptorSets)
                                   descriptorSetLayout
    }

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
  let groupCount    = 1 -- Just a generation shader
      handleSize    = rtiShaderGroupHandleSize
      baseAlignment = rtiShaderGroupBaseAlignment
      handleStride  = max handleSize baseAlignment
      -- Make the buffer big enough for all the groups, with spacing between
      -- them equal to their alignment
      sbtSize = fromIntegral $ handleStride * (groupCount - 1) + handleSize

  (bufferReleaseKey, (sbtBuffer, sbtAllocation, _sbtAllocationInfo)) <-
    withBuffer'
      zero { usage = BUFFER_USAGE_TRANSFER_SRC_BIT, size = sbtSize }
      zero
        { requiredFlags = MEMORY_PROPERTY_HOST_VISIBLE_BIT
                            .|. MEMORY_PROPERTY_HOST_COHERENT_BIT
        }
  nameObject' sbtBuffer "SBT"

  (memKey, mem) <- withMappedMemory' sbtAllocation
  getRayTracingShaderGroupHandlesKHR' pipeline 0 groupCount sbtSize mem
  unpackObjects groupCount handleSize handleStride mem
  release memKey
  pure (bufferReleaseKey, sbtBuffer)

-- | Move densely packed objects so that they have a desired stride
unpackObjects
  :: MonadIO m
  => Word32
  -- ^ Num objects
  -> Word32
  -- ^ Object size, the initial stride
  -> Word32
  -- ^ Desired stride
  -> Ptr ()
  -- ^ Initial, packed data, in a buffer big enough for the unpacked data
  -> m ()
unpackObjects numObjs size desiredStride buf = do
  let
    objectInitalPosition n = buf `plusPtr` fromIntegral (size * n)
    objectFinalPosition n = buf `plusPtr` fromIntegral (desiredStride * n)
    moveObject n = moveBytes (objectFinalPosition n)
                             (objectInitalPosition n)
                             (fromIntegral size)
  -- Move the object last to first
  liftIO $ traverse_ @[] moveObject [(numObjs - 1) .. 0]

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
