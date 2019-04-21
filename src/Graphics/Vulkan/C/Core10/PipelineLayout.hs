{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  , VkPipelineLayoutCreateFlags(..)
  , VkPipelineLayoutCreateInfo(..)
  , VkPushConstantRange(..)
  , VkShaderStageFlags
  , FN_vkCreatePipelineLayout
  , PFN_vkCreatePipelineLayout
  , vkCreatePipelineLayout
  , FN_vkDestroyPipelineLayout
  , PFN_vkDestroyPipelineLayout
  , vkDestroyPipelineLayout
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkShaderStageFlagBits(..)
  , VkPipelineLayout
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkDescriptorSetLayout_T
-- | VkDescriptorSetLayout - Opaque handle to a descriptor set layout object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'VkPipelineLayoutCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkDestroyDescriptorSetLayout'
type VkDescriptorSetLayout = Ptr VkDescriptorSetLayout_T

-- ** VkPipelineLayoutCreateFlags

-- | VkPipelineLayoutCreateFlags - Reserved for future use
--
-- = Description
--
-- 'VkPipelineLayoutCreateFlags' is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- 'VkPipelineLayoutCreateInfo'
newtype VkPipelineLayoutCreateFlags = VkPipelineLayoutCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkPipelineLayoutCreateFlags where
  
  showsPrec p (VkPipelineLayoutCreateFlags x) = showParen (p >= 11) (showString "VkPipelineLayoutCreateFlags " . showsPrec 11 x)

instance Read VkPipelineLayoutCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPipelineLayoutCreateFlags")
                        v <- step readPrec
                        pure (VkPipelineLayoutCreateFlags v)
                        )
                    )



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
-- 'VkDescriptorSetLayout', 'VkPipelineLayoutCreateFlags',
-- 'VkPushConstantRange', 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreatePipelineLayout'
data VkPipelineLayoutCreateInfo = VkPipelineLayoutCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkPipelineLayoutCreateFlags
  , -- | @setLayoutCount@ is the number of descriptor sets included in the
  -- pipeline layout.
  vkSetLayoutCount :: Word32
  , -- | @pSetLayouts@ is a pointer to an array of 'VkDescriptorSetLayout'
  -- objects.
  vkPSetLayouts :: Ptr VkDescriptorSetLayout
  , -- | @pushConstantRangeCount@ is the number of push constant ranges included
  -- in the pipeline layout.
  vkPushConstantRangeCount :: Word32
  , -- | @pPushConstantRanges@ is a pointer to an array of 'VkPushConstantRange'
  -- structures defining a set of push constant ranges for use in a single
  -- pipeline layout. In addition to descriptor set layouts, a pipeline
  -- layout also describes how many push constants /can/ be accessed by each
  -- stage of the pipeline.
  --
  -- __Note__
  --
  -- Push constants represent a high speed path to modify constant data in
  -- pipelines that is expected to outperform memory-backed resource updates.
  vkPPushConstantRanges :: Ptr VkPushConstantRange
  }
  deriving (Eq, Show)

instance Storable VkPipelineLayoutCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkPipelineLayoutCreateInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
                                        <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkSetLayoutCount (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPSetLayouts (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPushConstantRangeCount (poked :: VkPipelineLayoutCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPPushConstantRanges (poked :: VkPipelineLayoutCreateInfo))

instance Zero VkPipelineLayoutCreateInfo where
  zero = VkPipelineLayoutCreateInfo VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero

-- | VkPushConstantRange - Structure specifying a push constant range
--
-- == Valid Usage
--
-- Unresolved directive in VkPushConstantRange.txt -
-- include::{generated}\/validity\/structs\/VkPushConstantRange.txt[]
--
-- = See Also
--
-- 'VkPipelineLayoutCreateInfo', 'VkShaderStageFlags'
data VkPushConstantRange = VkPushConstantRange
  { -- | @stageFlags@ is a set of stage flags describing the shader stages that
  -- will access a range of push constants. If a particular stage is not
  -- included in the range, then accessing members of that range of push
  -- constants from the corresponding shader stage will return undefined
  -- values.
  vkStageFlags :: VkShaderStageFlags
  , -- | @offset@ /must/ be a multiple of @4@
  vkOffset :: Word32
  , -- | @size@ /must/ be less than or equal to
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPushConstantsSize@
  -- minus @offset@
  vkSize :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPushConstantRange where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkPushConstantRange <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 4)
                                 <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkStageFlags (poked :: VkPushConstantRange))
                *> poke (ptr `plusPtr` 4) (vkOffset (poked :: VkPushConstantRange))
                *> poke (ptr `plusPtr` 8) (vkSize (poked :: VkPushConstantRange))

instance Zero VkPushConstantRange where
  zero = VkPushConstantRange zero
                             zero
                             zero

-- | VkShaderStageFlags - Bitmask of VkShaderStageFlagBits
--
-- = Description
--
-- 'VkShaderStageFlags' is a bitmask type for setting a mask of zero or
-- more 'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties',
-- 'VkPushConstantRange',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPushConstants'
type VkShaderStageFlags = VkShaderStageFlagBits

-- | vkCreatePipelineLayout - Creates a new pipeline layout object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the pipeline layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkPipelineLayoutCreateInfo' structure specifying the state of the
--     pipeline layout object.
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
-- 'VkPipelineLayoutCreateInfo'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreatePipelineLayout" vkCreatePipelineLayout :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult
#else
vkCreatePipelineLayout :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult
vkCreatePipelineLayout deviceCmds = mkVkCreatePipelineLayout (pVkCreatePipelineLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePipelineLayout
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult)
#endif

type FN_vkCreatePipelineLayout = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult
type PFN_vkCreatePipelineLayout = FunPtr FN_vkCreatePipelineLayout

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
--     when 'vkDestroyPipelineLayout' is called
--
-- Unresolved directive in vkDestroyPipelineLayout.txt -
-- include::{generated}\/validity\/protos\/vkDestroyPipelineLayout.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyPipelineLayout" vkDestroyPipelineLayout :: ("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyPipelineLayout :: DeviceCmds -> ("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyPipelineLayout deviceCmds = mkVkDestroyPipelineLayout (pVkDestroyPipelineLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipelineLayout
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyPipelineLayout = ("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyPipelineLayout = FunPtr FN_vkDestroyPipelineLayout
