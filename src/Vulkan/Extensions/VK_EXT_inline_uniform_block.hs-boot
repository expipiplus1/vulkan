{-# language CPP #-}
-- | = Name
--
-- VK_EXT_inline_uniform_block - device extension
--
-- = Registered Extension Number
--
-- 139
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- -   Requires @VK_KHR_maintenance1@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-08-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Rakos, AMD
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Slawomir Grajewski, Intel
--
--     -   Neil Henning, Codeplay
--
-- == Description
--
-- This extension introduces the ability to back uniform blocks directly
-- with descriptor sets by storing inline uniform data within descriptor
-- pool storage. Compared to push constants this new construct allows
-- uniform data to be reused across multiple disjoint sets of draw or
-- dispatch commands and /may/ enable uniform data to be accessed with less
-- indirections compared to uniforms backed by buffer memory.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo':
--
--     -   'DescriptorPoolInlineUniformBlockCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceInlineUniformBlockFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceInlineUniformBlockPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet':
--
--     -   'WriteDescriptorSetInlineUniformBlockEXT'
--
-- == New Enum Constants
--
-- -   'EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME'
--
-- -   'EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT'
--
-- == Issues
--
-- 1) Do we need a new storage class for inline uniform blocks vs uniform
-- blocks?
--
-- __RESOLVED__: No. The @Uniform@ storage class is used to allow the same
-- syntax used for both uniform buffers and inline uniform blocks.
--
-- 2) Is the descriptor array index and array size expressed in terms of
-- bytes or dwords for inline uniform block descriptors?
--
-- __RESOLVED__: In bytes, but both /must/ be a multiple of 4, similar to
-- how push constant ranges are specified. The @descriptorCount@ of
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding' thus provides
-- the total number of bytes a particular binding with an inline uniform
-- block descriptor type can hold, while the @srcArrayElement@,
-- @dstArrayElement@, and @descriptorCount@ members of
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet',
-- 'Vulkan.Core10.DescriptorSet.CopyDescriptorSet', and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateEntry'
-- (where applicable) specify the byte offset and number of bytes to
-- write\/copy to the binding’s backing store. Additionally, the @stride@
-- member of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateEntry'
-- is ignored for inline uniform blocks and a default value of one is used,
-- meaning that the data to update inline uniform block bindings with must
-- be contiguous in memory.
--
-- 3) What layout rules apply for uniform blocks corresponding to inline
-- constants?
--
-- __RESOLVED__: They use the same layout rules as uniform buffers.
--
-- 4) Do we need to add non-uniform indexing features\/properties as
-- introduced by @VK_EXT_descriptor_indexing@ for inline uniform blocks?
--
-- __RESOLVED__: No, because inline uniform blocks are not allowed to be
-- “arrayed”. A single binding with an inline uniform block descriptor type
-- corresponds to a single uniform block instance and the array indices
-- inside that binding refer to individual offsets within the uniform block
-- (see issue #2). However, this extension does introduce new
-- features\/properties about the level of support for update-after-bind
-- inline uniform blocks.
--
-- 5) Is the @descriptorBindingVariableDescriptorCount@ feature introduced
-- by @VK_EXT_descriptor_indexing@ supported for inline uniform blocks?
--
-- __RESOLVED__: Yes, as long as other inline uniform block specific limits
-- are respected.
--
-- 6) Do the robustness guarantees of @robustBufferAccess@ apply to inline
-- uniform block accesses?
--
-- __RESOLVED__: No, similarly to push constants, as they are not backed by
-- buffer memory like uniform buffers.
--
-- == Version History
--
-- -   Revision 1, 2018-08-01 (Daniel Rakos)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'DescriptorPoolInlineUniformBlockCreateInfoEXT',
-- 'PhysicalDeviceInlineUniformBlockFeaturesEXT',
-- 'PhysicalDeviceInlineUniformBlockPropertiesEXT',
-- 'WriteDescriptorSetInlineUniformBlockEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_inline_uniform_block Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_inline_uniform_block  ( DescriptorPoolInlineUniformBlockCreateInfoEXT
                                                      , PhysicalDeviceInlineUniformBlockFeaturesEXT
                                                      , PhysicalDeviceInlineUniformBlockPropertiesEXT
                                                      , WriteDescriptorSetInlineUniformBlockEXT
                                                      ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DescriptorPoolInlineUniformBlockCreateInfoEXT

instance ToCStruct DescriptorPoolInlineUniformBlockCreateInfoEXT
instance Show DescriptorPoolInlineUniformBlockCreateInfoEXT

instance FromCStruct DescriptorPoolInlineUniformBlockCreateInfoEXT


data PhysicalDeviceInlineUniformBlockFeaturesEXT

instance ToCStruct PhysicalDeviceInlineUniformBlockFeaturesEXT
instance Show PhysicalDeviceInlineUniformBlockFeaturesEXT

instance FromCStruct PhysicalDeviceInlineUniformBlockFeaturesEXT


data PhysicalDeviceInlineUniformBlockPropertiesEXT

instance ToCStruct PhysicalDeviceInlineUniformBlockPropertiesEXT
instance Show PhysicalDeviceInlineUniformBlockPropertiesEXT

instance FromCStruct PhysicalDeviceInlineUniformBlockPropertiesEXT


data WriteDescriptorSetInlineUniformBlockEXT

instance ToCStruct WriteDescriptorSetInlineUniformBlockEXT
instance Show WriteDescriptorSetInlineUniformBlockEXT

instance FromCStruct WriteDescriptorSetInlineUniformBlockEXT

