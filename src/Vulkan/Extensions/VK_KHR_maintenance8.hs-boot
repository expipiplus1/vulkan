{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance8 - device extension
--
-- = VK_KHR_maintenance8
--
-- [__Name String__]
--     @VK_KHR_maintenance8@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     575
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance8] @zmike%0A*Here describe the issue or question you have about the VK_KHR_maintenance8 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance8.adoc VK_KHR_maintenance8>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-07
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Jon Leech, Khronos
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Spencer Fricke, LunarG
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Piers Daniell, NVIDIA
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Ricardo Garcia, Igalia
--
--     -   Lionel Landwerlin, Intel
--
--     -   Rick Hammerstone, Qualcomm
--
--     -   Daniel Story, Nintendo
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Caterina Shablia, Collabora
--
--     -   Georg Lehmann, Valve
--
--     -   Shahbaz Youssefi, Google
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance8 VK_KHR_maintenance8>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The new features are as follows:
--
-- -   Allow copies between depth\/stencil and “matching” color attachments
--
-- -   Allow @dstCache@ in
--     'Vulkan.Core10.PipelineCache.mergePipelineCaches' to be implicitly
--     synchronized.
--
-- -   Require src\/dst sync scopes to work when doing queue family
--     ownership transfers
--
-- -   Support @Offset@ (as an alternative to @ConstOffset@) image operand
--     in texture sampling and fetch operations
--
-- -   Use the SPIR-V definition of @OpSRem@ and @OpSMod@, making these
--     operations produce well-defined results for negative operands
--
-- -   Loosen layer restrictions when blitting from 3D images to other
--     image types
--
-- -   Add space for an additional 64 access flags for use with
--     VkMemoryBarrier2, VkBufferMemoryBarrier2, and VkImageMemoryBarrier2
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance8FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDependency2',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.BufferMemoryBarrier2',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.ImageMemoryBarrier2':
--
--     -   'MemoryBarrierAccessFlags3KHR'
--
-- == New Enums
--
-- -   'AccessFlagBits3KHR'
--
-- -   'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlagBits'
--
-- == New Bitmasks
--
-- -   'AccessFlags3KHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_8_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_8_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DependencyFlagBits.DependencyFlagBits':
--
--     -   'Vulkan.Core10.Enums.DependencyFlagBits.DEPENDENCY_QUEUE_FAMILY_OWNERSHIP_TRANSFER_USE_ALL_STAGES_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PipelineCacheCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_INTERNALLY_SYNCHRONIZED_MERGE_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_BARRIER_ACCESS_FLAGS_3_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_8_FEATURES_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2024-06-20 (Jon Leech)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_maintenance8 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance8  ( MemoryBarrierAccessFlags3KHR
                                              , PhysicalDeviceMaintenance8FeaturesKHR
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MemoryBarrierAccessFlags3KHR

instance ToCStruct MemoryBarrierAccessFlags3KHR
instance Show MemoryBarrierAccessFlags3KHR

instance FromCStruct MemoryBarrierAccessFlags3KHR


data PhysicalDeviceMaintenance8FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance8FeaturesKHR
instance Show PhysicalDeviceMaintenance8FeaturesKHR

instance FromCStruct PhysicalDeviceMaintenance8FeaturesKHR

