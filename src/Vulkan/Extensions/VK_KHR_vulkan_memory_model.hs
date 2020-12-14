{-# language CPP #-}
-- | = Name
--
-- VK_KHR_vulkan_memory_model - device extension
--
-- == VK_KHR_vulkan_memory_model
--
-- [__Name String__]
--     @VK_KHR_vulkan_memory_model@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     212
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_vulkan_memory_model:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_vulkan_memory_model.html SPV_KHR_vulkan_memory_model>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Alan Baker, Google
--
--     -   Tobias Hector, AMD
--
--     -   David Neto, Google
--
--     -   Robert Simpson, Qualcomm Technologies, Inc.
--
--     -   Brian Sumner, AMD
--
-- == Description
--
-- The @VK_KHR_vulkan_memory_model@ extension allows use of the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-model Vulkan Memory Model>,
-- which formally defines how to synchronize memory accesses to the same
-- memory locations performed by multiple shader invocations.
--
-- Note
--
-- Version 3 of the spec added a member
-- (@vulkanMemoryModelAvailabilityVisibilityChains@) to
-- 'PhysicalDeviceVulkanMemoryModelFeaturesKHR', which is an incompatible
-- change from version 2.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. However, if Vulkan 1.2 is supported and this
-- extension is not, the @vulkanMemoryModel@ capability is optional. The
-- original type, enum and command names are still available as aliases of
-- the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceVulkanMemoryModelFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME'
--
-- -   'KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-VulkanMemoryModel VulkanMemoryModelKHR>
--
-- == Version History
--
-- -   Revision 1, 2018-06-24 (Jeff Bolz)
--
--     -   Initial draft
--
-- -   Revision 3, 2018-12-10 (Jeff Bolz)
--
--     -   Add vulkanMemoryModelAvailabilityVisibilityChains member to the
--         VkPhysicalDeviceVulkanMemoryModelFeaturesKHR structure.
--
-- = See Also
--
-- 'PhysicalDeviceVulkanMemoryModelFeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_vulkan_memory_model Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_vulkan_memory_model  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
                                                     , PhysicalDeviceVulkanMemoryModelFeaturesKHR
                                                     , KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
                                                     , pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
                                                     , KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
                                                     , pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model (PhysicalDeviceVulkanMemoryModelFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceVulkanMemoryModelFeaturesKHR"
type PhysicalDeviceVulkanMemoryModelFeaturesKHR = PhysicalDeviceVulkanMemoryModelFeatures


type KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION"
pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = 3


type KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME = "VK_KHR_vulkan_memory_model"

-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME"
pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME = "VK_KHR_vulkan_memory_model"

