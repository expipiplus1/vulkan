{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.PipelineLayout
  ( DescriptorSetLayout
  , PipelineLayoutCreateFlags
  , withCStructPipelineLayoutCreateInfo
  , fromCStructPipelineLayoutCreateInfo
  , PipelineLayoutCreateInfo(..)
  , withCStructPushConstantRange
  , fromCStructPushConstantRange
  , PushConstantRange(..)
  , ShaderStageFlags
  , createPipelineLayout
  , destroyPipelineLayout
  , withPipelineLayout
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkPipelineLayoutCreateFlags(..)
  , VkPipelineLayoutCreateInfo(..)
  , VkPushConstantRange(..)
  , VkDescriptorSetLayout
  , vkCreatePipelineLayout
  , vkDestroyPipelineLayout
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pipeline
  ( PipelineLayout
  , ShaderStageFlagBits
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- | VkDescriptorSetLayout - Opaque handle to a descriptor set layout object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkDestroyDescriptorSetLayout'
type DescriptorSetLayout = VkDescriptorSetLayout

-- | VkPipelineLayoutCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateFlags' is
-- a bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo'
type PipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags


-- No complete pragma for PipelineLayoutCreateFlags as it has no patterns


-- | VkPipelineLayoutCreateInfo - Structure specifying the parameters of a
-- newly created pipeline layout object
--
-- == Valid Usage
--
-- -   @setLayoutCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxBoundDescriptorSets@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     accessible to any shader stage across all elements of @pSetLayouts@
--     /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorSamplers@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     accessible to any shader stage across all elements of @pSetLayouts@
--     /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorUniformBuffers@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     accessible to any shader stage across all elements of @pSetLayouts@
--     /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorStorageBuffers@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     accessible to any shader stage across all elements of @pSetLayouts@
--     /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorSampledImages@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     accessible to any shader stage across all elements of @pSetLayouts@
--     /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorStorageImages@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorInputAttachments@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetSamplers@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetUniformBuffers@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetUniformBuffersDynamic@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetStorageBuffers@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetStorageBuffersDynamic@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetSampledImages@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetStorageImages@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetInputAttachments@
--
-- -   Any two elements of @pPushConstantRanges@ /must/ not include the
--     same stage in @stageFlags@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   If @setLayoutCount@ is not @0@, @pSetLayouts@ /must/ be a valid
--     pointer to an array of @setLayoutCount@ valid
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout'
--     handles
--
-- -   If @pushConstantRangeCount@ is not @0@, @pPushConstantRanges@ /must/
--     be a valid pointer to an array of @pushConstantRangeCount@ valid
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkPushConstantRange'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateFlags',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPushConstantRange',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.vkCreatePipelineLayout'
data PipelineLayoutCreateInfo = PipelineLayoutCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "PipelineLayoutCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineLayoutCreateInfo" "flags"
  flags :: PipelineLayoutCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineLayoutCreateInfo" "pSetLayouts"
  setLayouts :: Vector DescriptorSetLayout
  -- Length valued member elided
  , -- No documentation found for Nested "PipelineLayoutCreateInfo" "pPushConstantRanges"
  pushConstantRanges :: Vector PushConstantRange
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPipelineLayoutCreateInfo' and
-- marshal a 'PipelineLayoutCreateInfo' into it. The 'VkPipelineLayoutCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPipelineLayoutCreateInfo :: PipelineLayoutCreateInfo -> (VkPipelineLayoutCreateInfo -> IO a) -> IO a
withCStructPipelineLayoutCreateInfo marshalled cont = withVec withCStructPushConstantRange (pushConstantRanges (marshalled :: PipelineLayoutCreateInfo)) (\pPPushConstantRanges -> withVec (&) (setLayouts (marshalled :: PipelineLayoutCreateInfo)) (\pPSetLayouts -> maybeWith withSomeVkStruct (next (marshalled :: PipelineLayoutCreateInfo)) (\pPNext -> cont (VkPipelineLayoutCreateInfo VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO pPNext (flags (marshalled :: PipelineLayoutCreateInfo)) (fromIntegral (Data.Vector.length (setLayouts (marshalled :: PipelineLayoutCreateInfo)))) pPSetLayouts (fromIntegral (Data.Vector.length (pushConstantRanges (marshalled :: PipelineLayoutCreateInfo)))) pPPushConstantRanges))))

-- | A function to read a 'VkPipelineLayoutCreateInfo' and all additional
-- structures in the pointer chain into a 'PipelineLayoutCreateInfo'.
fromCStructPipelineLayoutCreateInfo :: VkPipelineLayoutCreateInfo -> IO PipelineLayoutCreateInfo
fromCStructPipelineLayoutCreateInfo c = PipelineLayoutCreateInfo <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineLayoutCreateInfo)))
                                                                 <*> pure (vkFlags (c :: VkPipelineLayoutCreateInfo))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkSetLayoutCount (c :: VkPipelineLayoutCreateInfo))) (peekElemOff (vkPSetLayouts (c :: VkPipelineLayoutCreateInfo))))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkPushConstantRangeCount (c :: VkPipelineLayoutCreateInfo))) (((fromCStructPushConstantRange <=<) . peekElemOff) (vkPPushConstantRanges (c :: VkPipelineLayoutCreateInfo))))

instance Zero PipelineLayoutCreateInfo where
  zero = PipelineLayoutCreateInfo Nothing
                                  zero
                                  Data.Vector.empty
                                  Data.Vector.empty



-- | VkPushConstantRange - Structure specifying a push constant range
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkShaderStageFlags'
data PushConstantRange = PushConstantRange
  { -- No documentation found for Nested "PushConstantRange" "stageFlags"
  stageFlags :: ShaderStageFlags
  , -- No documentation found for Nested "PushConstantRange" "offset"
  offset :: Word32
  , -- No documentation found for Nested "PushConstantRange" "size"
  size :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPushConstantRange' and
-- marshal a 'PushConstantRange' into it. The 'VkPushConstantRange' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPushConstantRange :: PushConstantRange -> (VkPushConstantRange -> IO a) -> IO a
withCStructPushConstantRange marshalled cont = cont (VkPushConstantRange (stageFlags (marshalled :: PushConstantRange)) (offset (marshalled :: PushConstantRange)) (size (marshalled :: PushConstantRange)))

-- | A function to read a 'VkPushConstantRange' and all additional
-- structures in the pointer chain into a 'PushConstantRange'.
fromCStructPushConstantRange :: VkPushConstantRange -> IO PushConstantRange
fromCStructPushConstantRange c = PushConstantRange <$> pure (vkStageFlags (c :: VkPushConstantRange))
                                                   <*> pure (vkOffset (c :: VkPushConstantRange))
                                                   <*> pure (vkSize (c :: VkPushConstantRange))

instance Zero PushConstantRange where
  zero = PushConstantRange zero
                           zero
                           zero


-- | VkShaderStageFlags - Bitmask of VkShaderStageFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkShaderStageFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTablePushConstantEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.VkPhysicalDeviceCooperativeMatrixPropertiesNV',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPushConstantRange',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.VkShaderStatisticsInfoAMD',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPushConstants'
type ShaderStageFlags = ShaderStageFlagBits


-- | vkCreatePipelineLayout - Creates a new pipeline layout object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the pipeline layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo'
--     structure specifying the state of the pipeline layout object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelineLayout@ points to a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' handle in which
--     the resulting pipeline layout object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo'
--     structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pPipelineLayout@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo'
createPipelineLayout :: Device ->  PipelineLayoutCreateInfo ->  Maybe AllocationCallbacks ->  IO (PipelineLayout)
createPipelineLayout = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pPipelineLayout' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructPipelineLayoutCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreatePipelineLayout commandTable device' pCreateInfo' pAllocator pPipelineLayout' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pPipelineLayout')))))


-- | vkDestroyPipelineLayout - Destroy a pipeline layout object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the pipeline layout.
--
-- -   @pipelineLayout@ is the pipeline layout to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @pipelineLayout@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @pipelineLayout@ was created, @pAllocator@ /must/
--     be @NULL@
--
-- -   @pipelineLayout@ /must/ not have been passed to any @vkCmd*@ command
--     for any command buffers that are still in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--     when
--     'Graphics.Vulkan.C.Core10.PipelineLayout.vkDestroyPipelineLayout' is
--     called
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @pipelineLayout@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @pipelineLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   If @pipelineLayout@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @pipelineLayout@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout'
destroyPipelineLayout :: Device ->  PipelineLayout ->  Maybe AllocationCallbacks ->  IO ()
destroyPipelineLayout = \(Device device' commandTable) -> \pipelineLayout' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyPipelineLayout commandTable device' pipelineLayout' pAllocator *> (pure ()))

-- | A safe wrapper for 'createPipelineLayout' and 'destroyPipelineLayout' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withPipelineLayout
  :: Device -> PipelineLayoutCreateInfo -> Maybe (AllocationCallbacks) -> (PipelineLayout -> IO a) -> IO a
withPipelineLayout device pipelineLayoutCreateInfo allocationCallbacks = bracket
  (createPipelineLayout device pipelineLayoutCreateInfo allocationCallbacks)
  (\o -> destroyPipelineLayout device o allocationCallbacks)
