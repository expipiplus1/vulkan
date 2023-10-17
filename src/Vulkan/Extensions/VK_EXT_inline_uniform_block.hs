{-# language CPP #-}
-- | = Name
--
-- VK_EXT_inline_uniform_block - device extension
--
-- == VK_EXT_inline_uniform_block
--
-- [__Name String__]
--     @VK_EXT_inline_uniform_block@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     139
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance1 VK_KHR_maintenance1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_inline_uniform_block] @aqnuep%0A*Here describe the issue or question you have about the VK_EXT_inline_uniform_block extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-08-01
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
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
-- uniform data to be reused across multiple disjoint sets of drawing or
-- dispatching commands and /may/ enable uniform data to be accessed with
-- fewer indirections compared to uniforms backed by buffer memory.
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
--     -   'DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT'
--
--     -   'STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- EXT suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- Vulkan 1.3 adds
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#versions-1.3-new-features additional functionality related to this extension>
-- in the form of the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxInlineUniformTotalSize maxInlineUniformTotalSize>
-- limit.
--
-- == Issues
--
-- 1) Do we need a new storage class for inline uniform blocks vs. uniform
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
-- == See Also
--
-- 'DescriptorPoolInlineUniformBlockCreateInfoEXT',
-- 'PhysicalDeviceInlineUniformBlockFeaturesEXT',
-- 'PhysicalDeviceInlineUniformBlockPropertiesEXT',
-- 'WriteDescriptorSetInlineUniformBlockEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_inline_uniform_block Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_inline_uniform_block  ( pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
                                                      , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
                                                      , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
                                                      , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
                                                      , pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
                                                      , PhysicalDeviceInlineUniformBlockFeaturesEXT
                                                      , PhysicalDeviceInlineUniformBlockPropertiesEXT
                                                      , WriteDescriptorSetInlineUniformBlockEXT
                                                      , DescriptorPoolInlineUniformBlockCreateInfoEXT
                                                      , EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
                                                      , pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
                                                      , EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
                                                      , pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
                                                      ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block (DescriptorPoolInlineUniformBlockCreateInfo)
import Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockFeatures)
import Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockProperties)
import Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block (WriteDescriptorSetInlineUniformBlock)
import Vulkan.Core10.Enums.DescriptorType (DescriptorType(DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK))
-- No documentation found for TopLevel "VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT"
pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT = DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT"
pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT = STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT = STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO


-- No documentation found for TopLevel "VkPhysicalDeviceInlineUniformBlockFeaturesEXT"
type PhysicalDeviceInlineUniformBlockFeaturesEXT = PhysicalDeviceInlineUniformBlockFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceInlineUniformBlockPropertiesEXT"
type PhysicalDeviceInlineUniformBlockPropertiesEXT = PhysicalDeviceInlineUniformBlockProperties


-- No documentation found for TopLevel "VkWriteDescriptorSetInlineUniformBlockEXT"
type WriteDescriptorSetInlineUniformBlockEXT = WriteDescriptorSetInlineUniformBlock


-- No documentation found for TopLevel "VkDescriptorPoolInlineUniformBlockCreateInfoEXT"
type DescriptorPoolInlineUniformBlockCreateInfoEXT = DescriptorPoolInlineUniformBlockCreateInfo


type EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION"
pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION = 1


type EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME = "VK_EXT_inline_uniform_block"

-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME"
pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME = "VK_EXT_inline_uniform_block"

