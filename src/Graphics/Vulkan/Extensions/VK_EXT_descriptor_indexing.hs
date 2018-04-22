{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
  ( VkDescriptorBindingFlagBitsEXT(..)
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT
  , pattern VK_ERROR_FRAGMENTATION_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  , pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
  , pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  , VkPhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , VkPhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  , VkDescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , VkDescriptorBindingFlagsEXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
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


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorPoolCreateFlagBits(..)
  , VkDescriptorSetLayoutCreateFlagBits(..)
  )


-- ** VkDescriptorBindingFlagBitsEXT

-- | VkDescriptorBindingFlagBitsEXT - Bitmask specifying descriptor set
-- layout binding properties
--
-- = Description
--
-- -   @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ indicates that if
--     descriptors in this binding are updated between when the descriptor
--     set is bound in a command buffer and when that command buffer is
--     submitted to a queue, then the submission will use the most recently
--     set descriptors for this binding and the updates do not invalidate
--     the command buffer. Descriptor bindings created with this flag are
--     also partially exempt from the external synchronization requirement
--     in
--     'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplateKHR'
--     and 'Graphics.Vulkan.Core10.DescriptorSet.vkUpdateDescriptorSets'.
--     They /can/ be updated concurrently with the set being bound to a
--     command buffer in another thread, but not concurrently with the set
--     being reset or freed.
--
-- -   @VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT@ indicates that
--     descriptors in this binding that are not /dynamically used/ need not
--     contain valid descriptors at the time the descriptors are consumed.
--     A descriptor is dynamically used if any shader invocation executes
--     an instruction that performs any memory access using the descriptor.
--
-- -   @VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT@
--     indicates that descriptors in this binding /can/ be updated after a
--     command buffer has bound this descriptor set, or while a command
--     buffer that uses this descriptor set is pending execution, as long
--     as the descriptors that are updated are not used by those command
--     buffers. If @VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT@ is also
--     set, then descriptors /can/ be updated as long as they are not
--     dynamically used by any shader invocations. If
--     @VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT@ is not set, then
--     descriptors /can/ be updated as long as they are not statically used
--     by any shader invocations.
--
-- -   @VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT@ indicates
--     that this descriptor binding has a variable size that will be
--     specified when a descriptor set is allocated using this layout. The
--     value of @descriptorCount@ is treated as an upper bound on the size
--     of the binding. This /must/ only be used for the last binding in the
--     descriptor set layout (i.e. the binding with the largest value of
--     @binding@). For the purposes of counting against limits such as
--     @maxDescriptorSet@* and @maxPerStageDescriptor@*, the full value of
--     @descriptorCount@ is counted.
--
-- __Note__
--
-- Note that while @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ and
-- @VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT@ both involve
-- updates to descriptor sets after they are bound,
-- @VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT@ is a weaker
-- requirement since it is only about descriptors that are not used,
-- whereas @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ requires the
-- implementation to observe updates to descriptors that are used.
--
-- = See Also
--
-- 'VkDescriptorBindingFlagsEXT'
newtype VkDescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDescriptorBindingFlagBitsEXT where
  showsPrec _ VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT"
  showsPrec _ VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT"
  showsPrec _ VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT"
  showsPrec _ VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT"
  showsPrec p (VkDescriptorBindingFlagBitsEXT x) = showParen (p >= 11) (showString "VkDescriptorBindingFlagBitsEXT " . showsPrec 11 x)

instance Read VkDescriptorBindingFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT",           pure VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT)
                             , ("VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT", pure VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT)
                             , ("VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT",             pure VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT)
                             , ("VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT",   pure VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorBindingFlagBitsEXT")
                        v <- step readPrec
                        pure (VkDescriptorBindingFlagBitsEXT v)
                        )
                    )

-- No documentation found for Nested "VkDescriptorBindingFlagBitsEXT" "VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT"
pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000001

-- No documentation found for Nested "VkDescriptorBindingFlagBitsEXT" "VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT"
pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000002

-- No documentation found for Nested "VkDescriptorBindingFlagBitsEXT" "VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT"
pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000004

-- No documentation found for Nested "VkDescriptorBindingFlagBitsEXT" "VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT"
pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000008
-- No documentation found for Nested "VkResult" "VK_ERROR_FRAGMENTATION_EXT"
pattern VK_ERROR_FRAGMENTATION_EXT :: VkResult
pattern VK_ERROR_FRAGMENTATION_EXT = VkResult (-1000161000)
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT = VkStructureType 1000161000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT = VkStructureType 1000161001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT = VkStructureType 1000161002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT = VkStructureType 1000161003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT = VkStructureType 1000161004
-- | @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
-- specifies that descriptor sets using this layout /must/ be allocated
-- from a descriptor pool created with the
-- @VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT@ bit set.
-- Descriptor set layouts created with this bit set have alternate limits
-- for the maximum number of descriptors per-stage and per-pipeline layout.
-- The non-UpdateAfterBind limits only count descriptors in sets created
-- without this flag. The UpdateAfterBind limits count all descriptors, but
-- the limits /may/ be higher than the non-UpdateAfterBind limits.
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT :: VkDescriptorSetLayoutCreateFlagBits
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT = VkDescriptorSetLayoutCreateFlagBits 0x00000002
-- | @VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT@ specifies that
-- descriptor sets allocated from this pool /can/ include bindings with the
-- @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ bit set. It is valid
-- to allocate descriptor sets that have bindings that don’t set the
-- @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ bit from a pool that
-- has @VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT@ set.
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT :: VkDescriptorPoolCreateFlagBits
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT = VkDescriptorPoolCreateFlagBits 0x00000002
-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION"
pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2
-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME"
pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME = "VK_EXT_descriptor_indexing"
-- | VkPhysicalDeviceDescriptorIndexingFeaturesEXT - Structure describing
-- descriptor indexing features that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceDescriptorIndexingFeaturesEXT@
-- structure describe the following features:
--
-- = Description
--
-- -   @shaderInputAttachmentArrayDynamicIndexing@ indicates whether arrays
--     of input attachments /can/ be indexed by dynamically uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@ /must/ be indexed only by
--     constant integral expressions when aggregated into arrays in shader
--     code. This also indicates whether shader modules /can/ declare the
--     @InputAttachmentArrayDynamicIndexingEXT@ capability.
--
-- -   @shaderUniformTexelBufferArrayDynamicIndexing@ indicates whether
--     arrays of uniform texel buffers /can/ be indexed by dynamically
--     uniform integer expressions in shader code. If this feature is not
--     enabled, resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ /must/ be indexed only by
--     constant integral expressions when aggregated into arrays in shader
--     code. This also indicates whether shader modules /can/ declare the
--     @UniformTexelBufferArrayDynamicIndexingEXT@ capability.
--
-- -   @shaderStorageTexelBufferArrayDynamicIndexing@ indicates whether
--     arrays of storage texel buffers /can/ be indexed by dynamically
--     uniform integer expressions in shader code. If this feature is not
--     enabled, resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@ /must/ be indexed only by
--     constant integral expressions when aggregated into arrays in shader
--     code. This also indicates whether shader modules /can/ declare the
--     @StorageTexelBufferArrayDynamicIndexingEXT@ capability.
--
-- -   @shaderUniformBufferArrayNonUniformIndexing@ indicates whether
--     arrays of uniform buffers /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ /must/ not be indexed by
--     non-uniform integer expressions when aggregated into arrays in
--     shader code. This also indicates whether shader modules /can/
--     declare the @UniformBufferArrayNonUniformIndexingEXT@ capability.
--
-- -   @shaderSampledImageArrayNonUniformIndexing@ indicates whether arrays
--     of samplers or sampled images /can/ be indexed by non-uniform
--     integer expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of @VK_DESCRIPTOR_TYPE_SAMPLER@,
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, or
--     @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@ /must/ not be indexed by
--     non-uniform integer expressions when aggregated into arrays in
--     shader code. This also indicates whether shader modules /can/
--     declare the @SampledImageArrayNonUniformIndexingEXT@ capability.
--
-- -   @shaderStorageBufferArrayNonUniformIndexing@ indicates whether
--     arrays of storage buffers /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ /must/ not be indexed by
--     non-uniform integer expressions when aggregated into arrays in
--     shader code. This also indicates whether shader modules /can/
--     declare the @StorageBufferArrayNonUniformIndexingEXT@ capability.
--
-- -   @shaderStorageImageArrayNonUniformIndexing@ indicates whether arrays
--     of storage images /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@ /must/ not be indexed by
--     non-uniform integer expressions when aggregated into arrays in
--     shader code. This also indicates whether shader modules /can/
--     declare the @StorageImageArrayNonUniformIndexingEXT@ capability.
--
-- -   @shaderInputAttachmentArrayNonUniformIndexing@ indicates whether
--     arrays of input attachments /can/ be indexed by non-uniform integer
--     expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@ /must/ not be indexed by
--     non-uniform integer expressions when aggregated into arrays in
--     shader code. This also indicates whether shader modules /can/
--     declare the @InputAttachmentArrayNonUniformIndexingEXT@ capability.
--
-- -   @shaderUniformTexelBufferArrayNonUniformIndexing@ indicates whether
--     arrays of uniform texel buffers /can/ be indexed by non-uniform
--     integer expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ /must/ not be indexed by
--     non-uniform integer expressions when aggregated into arrays in
--     shader code. This also indicates whether shader modules /can/
--     declare the @UniformTexelBufferArrayNonUniformIndexingEXT@
--     capability.
--
-- -   @shaderStorageTexelBufferArrayNonUniformIndexing@ indicates whether
--     arrays of storage texel buffers /can/ be indexed by non-uniform
--     integer expressions in shader code. If this feature is not enabled,
--     resources with a descriptor type of
--     @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@ /must/ not be indexed by
--     non-uniform integer expressions when aggregated into arrays in
--     shader code. This also indicates whether shader modules /can/
--     declare the @StorageTexelBufferArrayNonUniformIndexingEXT@
--     capability.
--
-- -   @descriptorBindingUniformBufferUpdateAfterBind@ indicates whether
--     the implementation supports updating uniform buffer descriptors
--     after a set is bound. If this feature is not enabled,
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ /must/ not be used
--     with @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@.
--
-- -   @descriptorBindingSampledImageUpdateAfterBind@ indicates whether the
--     implementation supports updating sampled image descriptors after a
--     set is bound. If this feature is not enabled,
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ /must/ not be used
--     with @VK_DESCRIPTOR_TYPE_SAMPLER@,
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, or
--     @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@.
--
-- -   @descriptorBindingStorageImageUpdateAfterBind@ indicates whether the
--     implementation supports updating storage image descriptors after a
--     set is bound. If this feature is not enabled,
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ /must/ not be used
--     with @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@.
--
-- -   @descriptorBindingStorageBufferUpdateAfterBind@ indicates whether
--     the implementation supports updating storage buffer descriptors
--     after a set is bound. If this feature is not enabled,
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ /must/ not be used
--     with @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@.
--
-- -   @descriptorBindingUniformTexelBufferUpdateAfterBind@ indicates
--     whether the implementation supports updating uniform texel buffer
--     descriptors after a set is bound. If this feature is not enabled,
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ /must/ not be used
--     with @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@.
--
-- -   @descriptorBindingStorageTexelBufferUpdateAfterBind@ indicates
--     whether the implementation supports updating storage texel buffer
--     descriptors after a set is bound. If this feature is not enabled,
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ /must/ not be used
--     with @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@.
--
-- -   @descriptorBindingUpdateUnusedWhilePending@ indicates whether the
--     implementation supports updating descriptors while the set is in
--     use. If this feature is not enabled,
--     @VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT@ /must/
--     not be used.
--
-- -   @descriptorBindingPartiallyBound@ indicates whether the
--     implementation supports statically using a descriptor set binding in
--     which some descriptors are not valid. If this feature is not
--     enabled, @VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT@ /must/ not
--     be used.
--
-- -   @descriptorBindingVariableDescriptorCount@ indicates whether the
--     implementation supports descriptor sets with a variable-sized last
--     binding. If this feature is not enabled,
--     @VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT@ /must/ not
--     be used.
--
-- -   @runtimeDescriptorArray@ indicates whether the implementation
--     supports the SPIR-V RuntimeDescriptorArrayEXT capability. If this
--     feature is not enabled, descriptors /must/ not be declared in
--     runtime arrays.
--
-- If the @VkPhysicalDeviceDescriptorIndexingFeaturesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2KHR',
-- it is filled with values indicating whether each feature is supported.
-- @VkPhysicalDeviceDescriptorIndexingFeaturesEXT@ /can/ also be used in
-- the @pNext@ chain of 'Graphics.Vulkan.Core10.Device.VkDeviceCreateInfo'
-- to enable features.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceDescriptorIndexingFeaturesEXT = VkPhysicalDeviceDescriptorIndexingFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderInputAttachmentArrayDynamicIndexing"
  vkShaderInputAttachmentArrayDynamicIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformTexelBufferArrayDynamicIndexing"
  vkShaderUniformTexelBufferArrayDynamicIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageTexelBufferArrayDynamicIndexing"
  vkShaderStorageTexelBufferArrayDynamicIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformBufferArrayNonUniformIndexing"
  vkShaderUniformBufferArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderSampledImageArrayNonUniformIndexing"
  vkShaderSampledImageArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageBufferArrayNonUniformIndexing"
  vkShaderStorageBufferArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageImageArrayNonUniformIndexing"
  vkShaderStorageImageArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderInputAttachmentArrayNonUniformIndexing"
  vkShaderInputAttachmentArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformTexelBufferArrayNonUniformIndexing"
  vkShaderUniformTexelBufferArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageTexelBufferArrayNonUniformIndexing"
  vkShaderStorageTexelBufferArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUniformBufferUpdateAfterBind"
  vkDescriptorBindingUniformBufferUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingSampledImageUpdateAfterBind"
  vkDescriptorBindingSampledImageUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageImageUpdateAfterBind"
  vkDescriptorBindingStorageImageUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageBufferUpdateAfterBind"
  vkDescriptorBindingStorageBufferUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUniformTexelBufferUpdateAfterBind"
  vkDescriptorBindingUniformTexelBufferUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageTexelBufferUpdateAfterBind"
  vkDescriptorBindingStorageTexelBufferUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUpdateUnusedWhilePending"
  vkDescriptorBindingUpdateUnusedWhilePending :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingPartiallyBound"
  vkDescriptorBindingPartiallyBound :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingVariableDescriptorCount"
  vkDescriptorBindingVariableDescriptorCount :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "runtimeDescriptorArray"
  vkRuntimeDescriptorArray :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDescriptorIndexingFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
                                                           <*> peek (ptr `plusPtr` 20)
                                                           <*> peek (ptr `plusPtr` 24)
                                                           <*> peek (ptr `plusPtr` 28)
                                                           <*> peek (ptr `plusPtr` 32)
                                                           <*> peek (ptr `plusPtr` 36)
                                                           <*> peek (ptr `plusPtr` 40)
                                                           <*> peek (ptr `plusPtr` 44)
                                                           <*> peek (ptr `plusPtr` 48)
                                                           <*> peek (ptr `plusPtr` 52)
                                                           <*> peek (ptr `plusPtr` 56)
                                                           <*> peek (ptr `plusPtr` 60)
                                                           <*> peek (ptr `plusPtr` 64)
                                                           <*> peek (ptr `plusPtr` 68)
                                                           <*> peek (ptr `plusPtr` 72)
                                                           <*> peek (ptr `plusPtr` 76)
                                                           <*> peek (ptr `plusPtr` 80)
                                                           <*> peek (ptr `plusPtr` 84)
                                                           <*> peek (ptr `plusPtr` 88)
                                                           <*> peek (ptr `plusPtr` 92)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkShaderInputAttachmentArrayDynamicIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkShaderUniformTexelBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 24) (vkShaderStorageTexelBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 28) (vkShaderUniformBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 32) (vkShaderSampledImageArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 36) (vkShaderStorageBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 40) (vkShaderStorageImageArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 44) (vkShaderInputAttachmentArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 48) (vkShaderUniformTexelBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 52) (vkShaderStorageTexelBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 56) (vkDescriptorBindingUniformBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 60) (vkDescriptorBindingSampledImageUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 64) (vkDescriptorBindingStorageImageUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 68) (vkDescriptorBindingStorageBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 72) (vkDescriptorBindingUniformTexelBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 76) (vkDescriptorBindingStorageTexelBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 80) (vkDescriptorBindingUpdateUnusedWhilePending (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 84) (vkDescriptorBindingPartiallyBound (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 88) (vkDescriptorBindingVariableDescriptorCount (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 92) (vkRuntimeDescriptorArray (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
-- | VkPhysicalDeviceDescriptorIndexingPropertiesEXT - Structure describing
-- descriptor indexing properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceDescriptorIndexingPropertiesEXT@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @maxUpdateAfterBindDescriptorsInAllPools@ is the maximum number of
--     descriptors (summed over all descriptor types) that /can/ be created
--     across all pools that are created with the
--     @VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT@ bit set. Pool
--     creation /may/ fail when this limit is exceeded, or when the space
--     this limit represents can’t satisfy a pool creation due to
--     fragmentation.
--
-- -   @shaderUniformBufferArrayNonUniformIndexingNative@ is a boolean
--     value indicating whether uniform buffer descriptors natively support
--     nonuniform indexing. If this is @VK_FALSE@, then a single dynamic
--     instance of an instruction that nonuniformly indexes an array of
--     uniform buffers /may/ execute multiple times in order to access all
--     the descriptors.
--
-- -   @shaderSampledImageArrayNonUniformIndexingNative@ is a boolean value
--     indicating whether sampler and image descriptors natively support
--     nonuniform indexing. If this is @VK_FALSE@, then a single dynamic
--     instance of an instruction that nonuniformly indexes an array of
--     samplers or images /may/ execute multiple times in order to access
--     all the descriptors.
--
-- -   @shaderStorageBufferArrayNonUniformIndexingNative@ is a boolean
--     value indicating whether storage buffer descriptors natively support
--     nonuniform indexing. If this is @VK_FALSE@, then a single dynamic
--     instance of an instruction that nonuniformly indexes an array of
--     storage buffers /may/ execute multiple times in order to access all
--     the descriptors.
--
-- -   @shaderStorageImageArrayNonUniformIndexingNative@ is a boolean value
--     indicating whether storage image descriptors natively support
--     nonuniform indexing. If this is @VK_FALSE@, then a single dynamic
--     instance of an instruction that nonuniformly indexes an array of
--     storage images /may/ execute multiple times in order to access all
--     the descriptors.
--
-- -   @shaderInputAttachmentArrayNonUniformIndexingNative@ is a boolean
--     value indicating whether input attachment descriptors natively
--     support nonuniform indexing. If this is @VK_FALSE@, then a single
--     dynamic instance of an instruction that nonuniformly indexes an
--     array of input attachments /may/ execute multiple times in order to
--     access all the descriptors.
--
-- -   @robustBufferAccessUpdateAfterBind@ is a boolean value indicating
--     whether
--     [@robustBufferAccess@](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     /can/ be enabled in a device simultaneously with
--     @descriptorBindingUniformBufferUpdateAfterBind@,
--     @descriptorBindingStorageBufferUpdateAfterBind@,
--     @descriptorBindingUniformTexelBufferUpdateAfterBind@, and\/or
--     @descriptorBindingStorageTexelBufferUpdateAfterBind@. If this is
--     @VK_FALSE@, then either @robustBufferAccess@ /must/ be disabled or
--     all of these update-after-bind features /must/ be disabled.
--
-- -   @quadDivergentImplicitLod@ is a boolean value indicating whether
--     implicit level of detail calculations for image operations have
--     well-defined results when the image and\/or sampler objects used for
--     the instruction are not uniform within a quad. See [Derivative Image
--     Operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-derivative-image-operations).
--
-- -   @maxPerStageDescriptorUpdateAfterBindSamplers@ is similar to
--     @maxPerStageDescriptorSamplers@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxPerStageDescriptorUpdateAfterBindUniformBuffers@ is similar to
--     @maxPerStageDescriptorUniformBuffers@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxPerStageDescriptorUpdateAfterBindStorageBuffers@ is similar to
--     @maxPerStageDescriptorStorageBuffers@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxPerStageDescriptorUpdateAfterBindSampledImages@ is similar to
--     @maxPerStageDescriptorSampledImages@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxPerStageDescriptorUpdateAfterBindStorageImages@ is similar to
--     @maxPerStageDescriptorStorageImages@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxPerStageDescriptorUpdateAfterBindInputAttachments@ is similar to
--     @maxPerStageDescriptorInputAttachments@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxPerStageUpdateAfterBindResources@ is similar to
--     @maxPerStageResources@ but counts descriptors from descriptor sets
--     created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxDescriptorSetUpdateAfterBindSamplers@ is similar to
--     @maxDescriptorSetSamplers@ but counts descriptors from descriptor
--     sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxDescriptorSetUpdateAfterBindUniformBuffers@ is similar to
--     @maxDescriptorSetUniformBuffers@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxDescriptorSetUpdateAfterBindUniformBuffersDynamic@ is similar to
--     @maxDescriptorSetUniformBuffersDynamic@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxDescriptorSetUpdateAfterBindStorageBuffers@ is similar to
--     @maxDescriptorSetStorageBuffers@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxDescriptorSetUpdateAfterBindStorageBuffersDynamic@ is similar to
--     @maxDescriptorSetStorageBuffersDynamic@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxDescriptorSetUpdateAfterBindSampledImages@ is similar to
--     @maxDescriptorSetSampledImages@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxDescriptorSetUpdateAfterBindStorageImages@ is similar to
--     @maxDescriptorSetStorageImages@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- -   @maxDescriptorSetUpdateAfterBindInputAttachments@ is similar to
--     @maxDescriptorSetInputAttachments@ but counts descriptors from
--     descriptor sets created with or without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set.
--
-- If the @VkPhysicalDeviceDescriptorIndexingPropertiesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2KHR',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceDescriptorIndexingPropertiesEXT = VkPhysicalDeviceDescriptorIndexingPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxUpdateAfterBindDescriptorsInAllPools"
  vkMaxUpdateAfterBindDescriptorsInAllPools :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderUniformBufferArrayNonUniformIndexingNative"
  vkShaderUniformBufferArrayNonUniformIndexingNative :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderSampledImageArrayNonUniformIndexingNative"
  vkShaderSampledImageArrayNonUniformIndexingNative :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderStorageBufferArrayNonUniformIndexingNative"
  vkShaderStorageBufferArrayNonUniformIndexingNative :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderStorageImageArrayNonUniformIndexingNative"
  vkShaderStorageImageArrayNonUniformIndexingNative :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderInputAttachmentArrayNonUniformIndexingNative"
  vkShaderInputAttachmentArrayNonUniformIndexingNative :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "robustBufferAccessUpdateAfterBind"
  vkRobustBufferAccessUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "quadDivergentImplicitLod"
  vkQuadDivergentImplicitLod :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindSamplers"
  vkMaxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
  vkMaxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
  vkMaxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindSampledImages"
  vkMaxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindStorageImages"
  vkMaxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindInputAttachments"
  vkMaxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageUpdateAfterBindResources"
  vkMaxPerStageUpdateAfterBindResources :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindSamplers"
  vkMaxDescriptorSetUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindUniformBuffers"
  vkMaxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
  vkMaxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageBuffers"
  vkMaxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
  vkMaxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindSampledImages"
  vkMaxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageImages"
  vkMaxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindInputAttachments"
  vkMaxDescriptorSetUpdateAfterBindInputAttachments :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
  sizeOf ~_ = 112
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDescriptorIndexingPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 20)
                                                             <*> peek (ptr `plusPtr` 24)
                                                             <*> peek (ptr `plusPtr` 28)
                                                             <*> peek (ptr `plusPtr` 32)
                                                             <*> peek (ptr `plusPtr` 36)
                                                             <*> peek (ptr `plusPtr` 40)
                                                             <*> peek (ptr `plusPtr` 44)
                                                             <*> peek (ptr `plusPtr` 48)
                                                             <*> peek (ptr `plusPtr` 52)
                                                             <*> peek (ptr `plusPtr` 56)
                                                             <*> peek (ptr `plusPtr` 60)
                                                             <*> peek (ptr `plusPtr` 64)
                                                             <*> peek (ptr `plusPtr` 68)
                                                             <*> peek (ptr `plusPtr` 72)
                                                             <*> peek (ptr `plusPtr` 76)
                                                             <*> peek (ptr `plusPtr` 80)
                                                             <*> peek (ptr `plusPtr` 84)
                                                             <*> peek (ptr `plusPtr` 88)
                                                             <*> peek (ptr `plusPtr` 92)
                                                             <*> peek (ptr `plusPtr` 96)
                                                             <*> peek (ptr `plusPtr` 100)
                                                             <*> peek (ptr `plusPtr` 104)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxUpdateAfterBindDescriptorsInAllPools (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkShaderUniformBufferArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkShaderSampledImageArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkShaderStorageBufferArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkShaderStorageImageArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkShaderInputAttachmentArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkRobustBufferAccessUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 44) (vkQuadDivergentImplicitLod (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 48) (vkMaxPerStageDescriptorUpdateAfterBindSamplers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 52) (vkMaxPerStageDescriptorUpdateAfterBindUniformBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 56) (vkMaxPerStageDescriptorUpdateAfterBindStorageBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 60) (vkMaxPerStageDescriptorUpdateAfterBindSampledImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 64) (vkMaxPerStageDescriptorUpdateAfterBindStorageImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 68) (vkMaxPerStageDescriptorUpdateAfterBindInputAttachments (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 72) (vkMaxPerStageUpdateAfterBindResources (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 76) (vkMaxDescriptorSetUpdateAfterBindSamplers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 80) (vkMaxDescriptorSetUpdateAfterBindUniformBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 84) (vkMaxDescriptorSetUpdateAfterBindUniformBuffersDynamic (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 88) (vkMaxDescriptorSetUpdateAfterBindStorageBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 92) (vkMaxDescriptorSetUpdateAfterBindStorageBuffersDynamic (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 96) (vkMaxDescriptorSetUpdateAfterBindSampledImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 100) (vkMaxDescriptorSetUpdateAfterBindStorageImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 104) (vkMaxDescriptorSetUpdateAfterBindInputAttachments (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
-- | VkDescriptorSetLayoutBindingFlagsCreateInfoEXT - Structure specifying
-- creation flags for descriptor set layout bindings
--
-- = Description
--
-- If @bindingCount@ is zero or if this structure is not in the @pNext@
-- chain, the 'VkDescriptorBindingFlagsEXT' for each descriptor set layout
-- binding is considered to be zero. Otherwise, the descriptor set layout
-- binding at
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@pBindings@[i]
-- uses the flags in @pBindingFlags@[i].
--
-- == Valid Usage
--
-- -   If @bindingCount@ is not zero, @bindingCount@ /must/ equal
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@bindingCount@
--
-- -   If
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@flags@
--     includes @VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR@,
--     then all elements of @pBindingFlags@ /must/ not include
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@,
--     @VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT@, or
--     @VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT@
--
-- -   If an element of @pBindingFlags@ includes
--     @VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT@, then all
--     other elements of
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@pBindings@
--     /must/ have a smaller value of @binding@
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingUniformBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ /must/ not use
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingSampledImageUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     @VK_DESCRIPTOR_TYPE_SAMPLER@,
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, or
--     @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@ /must/ not use
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingStorageImageUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@ /must/ not use
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingStorageBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ /must/ not use
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingUniformTexelBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ /must/ not use
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingStorageTexelBufferUpdateAfterBind@
--     is not enabled, all bindings with descriptor type
--     @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@ /must/ not use
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@
--
-- -   All bindings with descriptor type
--     @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@,
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@, or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ /must/ not use
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingUpdateUnusedWhilePending@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     @VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT@
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingPartiallyBound@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     @VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT@
--
-- -   If
--     'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingVariableDescriptorCount@
--     is not enabled, all elements of @pBindingFlags@ /must/ not include
--     @VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT@
--
-- -   If an element of @pBindingFlags@ includes
--     @VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT@, that
--     element’s @descriptorType@ /must/ not be
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT@
--
-- -   If @bindingCount@ is not @0@, @pBindingFlags@ /must/ be a valid
--     pointer to an array of @bindingCount@ valid combinations of
--     'VkDescriptorBindingFlagBitsEXT' values
--
-- -   Each element of @pBindingFlags@ /must/ not be @0@
--
-- = See Also
--
-- 'VkDescriptorBindingFlagsEXT',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkDescriptorSetLayoutBindingFlagsCreateInfoEXT = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @bindingCount@ is zero or the number of elements in @pBindingFlags@.
  vkBindingCount :: Word32
  , -- | @pBindingFlags@ is a pointer to an array of
  -- 'VkDescriptorBindingFlagsEXT' bitfields, one for each descriptor set
  -- layout binding.
  vkPBindingFlags :: Ptr VkDescriptorBindingFlagsEXT
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkBindingCount (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPBindingFlags (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
-- | VkDescriptorSetVariableDescriptorCountAllocateInfoEXT - Structure
-- specifying additional allocation parameters for descriptor sets
--
-- = Description
--
-- If @descriptorSetCount@ is zero or this structure is not included in the
-- @pNext@ chain, then the variable lengths are considered to be zero.
-- Otherwise, @pDescriptorCounts@[i] is the number of descriptors in the
-- variable count descriptor binding in the corresponding descriptor set
-- layout. If
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetAllocateInfo'::@pSetLayouts@[i]
-- does not include a variable count descriptor binding, then
-- @pDescriptorCounts@[i] is ignored.
--
-- == Valid Usage
--
-- -   If @descriptorSetCount@ is not zero, @descriptorSetCount@ /must/
--     equal
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetAllocateInfo'::@descriptorSetCount@
--
-- -   If
--     'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetAllocateInfo'::@pSetLayouts@[i]
--     has a variable descriptor count binding, then @pDescriptorCounts@[i]
--     /must/ be less than or equal to the descriptor count specified for
--     that binding when the descriptor set layout was created.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT@
--
-- -   If @descriptorSetCount@ is not @0@, @pDescriptorCounts@ /must/ be a
--     valid pointer to an array of @descriptorSetCount@ @uint32_t@ values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkDescriptorSetVariableDescriptorCountAllocateInfoEXT = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @descriptorSetCount@ is zero or the number of elements in
  -- @pDescriptorCounts@.
  vkDescriptorSetCount :: Word32
  , -- | @pDescriptorCounts@ is an array of descriptor counts, with each member
  -- specifying the number of descriptors in a variable descriptor count
  -- binding in the corresponding descriptor set being allocated.
  vkPDescriptorCounts :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetVariableDescriptorCountAllocateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                                   <*> peek (ptr `plusPtr` 8)
                                                                   <*> peek (ptr `plusPtr` 16)
                                                                   <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDescriptorSetCount (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPDescriptorCounts (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
-- | VkDescriptorSetVariableDescriptorCountLayoutSupportEXT - Structure
-- returning information about whether a descriptor set layout can be
-- supported
--
-- = Description
--
-- If the create info includes a variable-sized descriptor, then
-- @supported@ is determined assuming the requested size of the
-- variable-sized descriptor, and @maxVariableDescriptorCount@ is set to
-- the maximum size of that descriptor that /can/ be successfully created
-- (which is greater than or equal to the requested size passed in). If the
-- create info does not include a variable-sized descriptor or if the
-- 'VkPhysicalDeviceDescriptorIndexingFeaturesEXT'::@descriptorBindingVariableDescriptorCount@
-- feature is not enabled, then @maxVariableDescriptorCount@ is set to
-- zero. For the purposes of this command, a variable-sized descriptor
-- binding with a @descriptorCount@ of zero is treated as if the
-- @descriptorCount@ is one, and thus the binding is not ignored and the
-- maximum descriptor count will be returned. If the layout is not
-- supported, then the value written to @maxVariableDescriptorCount@ is
-- undefined.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkDescriptorSetVariableDescriptorCountLayoutSupportEXT = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxVariableDescriptorCount@ indicates the maximum number of descriptors
  -- supported in the highest numbered binding of the layout, if that binding
  -- is variable-sized.
  vkMaxVariableDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT <$> peek (ptr `plusPtr` 0)
                                                                    <*> peek (ptr `plusPtr` 8)
                                                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxVariableDescriptorCount (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
-- No documentation found for TopLevel "VkDescriptorBindingFlagsEXT"
type VkDescriptorBindingFlagsEXT = VkDescriptorBindingFlagBitsEXT
