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


-- | VkPipelineLayoutCreateInfo - Structure specifying the parameters of a
-- newly created pipeline layout object
--
-- == Valid Usage
--
-- -   @setLayoutCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxBoundDescriptorSets@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorSamplers@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorUniformBuffers@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorStorageBuffers@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorSampledImages@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorStorageImages@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPerStageDescriptorInputAttachments@
--
-- -   The total number of bindings in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockPropertiesEXT'::@maxPerStageDescriptorInlineUniformBlocks@
--
-- -   The total number of descriptors with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxPerStageDescriptorUpdateAfterBindSamplers@
--
-- -   The total number of descriptors with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxPerStageDescriptorUpdateAfterBindUniformBuffers@
--
-- -   The total number of descriptors with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxPerStageDescriptorUpdateAfterBindStorageBuffers@
--
-- -   The total number of descriptors with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxPerStageDescriptorUpdateAfterBindSampledImages@
--
-- -   The total number of descriptors with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxPerStageDescriptorUpdateAfterBindStorageImages@
--
-- -   The total number of descriptors with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxPerStageDescriptorUpdateAfterBindInputAttachments@
--
-- -   The total number of bindings with a @descriptorType@ of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     accessible to any given shader stage across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockPropertiesEXT'::@maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetSamplers@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetUniformBuffers@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetUniformBuffersDynamic@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetStorageBuffers@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetStorageBuffersDynamic@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetSampledImages@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetStorageImages@
--
-- -   The total number of descriptors in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDescriptorSetInputAttachments@
--
-- -   The total number of bindings in descriptor set layouts created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set with a @descriptorType@ of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockPropertiesEXT'::@maxDescriptorSetInlineUniformBlocks@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxDescriptorSetUpdateAfterBindSamplers@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxDescriptorSetUpdateAfterBindUniformBuffers@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxDescriptorSetUpdateAfterBindUniformBuffersDynamic@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxDescriptorSetUpdateAfterBindStorageBuffers@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxDescriptorSetUpdateAfterBindStorageBuffersDynamic@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxDescriptorSetUpdateAfterBindSampledImages@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     and
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxDescriptorSetUpdateAfterBindStorageImages@
--
-- -   The total number of descriptors of the type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT'::@maxDescriptorSetUpdateAfterBindInputAttachments@
--
-- -   The total number of bindings with a @descriptorType@ of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockPropertiesEXT'::@maxDescriptorSetUpdateAfterBindInlineUniformBlocks@
--
-- -   Any two elements of @pPushConstantRanges@ /must/ not include the
--     same stage in @stageFlags@
--
-- -   @pSetLayouts@ /must/ not contain more than one descriptor set layout
--     that was created with
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--     set
--
-- -   The total number of bindings with a @descriptorType@ of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV'
--     accessible across all shader stages and across all elements of
--     @pSetLayouts@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkPhysicalDeviceRayTracingPropertiesNV'::@maxDescriptorSetAccelerationStructures@
--
-- Unresolved directive in VkPipelineLayoutCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkPipelineLayoutCreateInfo.txt[]
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
-- == Valid Usage
--
-- Unresolved directive in VkPushConstantRange.txt -
-- include::{generated}\/validity\/structs\/VkPushConstantRange.txt[]
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
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPushConstantRange',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits',
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
-- = Description
--
-- Unresolved directive in vkCreatePipelineLayout.txt -
-- include::{generated}\/validity\/protos\/vkCreatePipelineLayout.txt[]
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
-- Unresolved directive in vkDestroyPipelineLayout.txt -
-- include::{generated}\/validity\/protos\/vkDestroyPipelineLayout.txt[]
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
