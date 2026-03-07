{-# language CPP #-}
-- | = Name
--
-- VK_KHR_copy_memory_indirect - device extension
--
-- = VK_KHR_copy_memory_indirect
--
-- [__Name String__]
--     @VK_KHR_copy_memory_indirect@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     550
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_copy_memory_indirect] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_KHR_copy_memory_indirect extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_copy_memory_indirect.adoc VK_KHR_copy_memory_indirect>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-25
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Stuart Smith, AMD
--
--     -   Faith Ekstrand, Collabora
--
--     -   Caterina Shablia, Collabora
--
--     -   Spencer Fricke, LunarG
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Alyssa Rosenzweig, Valve
--
-- == Description
--
-- This extension adds support for performing copies between memory and
-- image regions using indirect parameters that are read by the device from
-- a buffer during execution. This functionality may be useful for
-- performing copies where the copy parameters are not known during the
-- command buffer creation time.
--
-- == New Commands
--
-- -   'cmdCopyMemoryIndirectKHR'
--
-- -   'cmdCopyMemoryToImageIndirectKHR'
--
-- == New Structures
--
-- -   'CopyMemoryIndirectCommandKHR'
--
-- -   'CopyMemoryIndirectInfoKHR'
--
-- -   'CopyMemoryToImageIndirectCommandKHR'
--
-- -   'CopyMemoryToImageIndirectInfoKHR'
--
-- -   'StridedDeviceAddressRangeKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCopyMemoryIndirectFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceCopyMemoryIndirectPropertiesKHR'
--
-- == New Enums
--
-- -   'AddressCopyFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'AddressCopyFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_COPY_MEMORY_INDIRECT_EXTENSION_NAME'
--
-- -   'KHR_COPY_MEMORY_INDIRECT_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_COPY_IMAGE_INDIRECT_DST_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_INDIRECT_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_INDIRECT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INDIRECT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COPY_MEMORY_INDIRECT_PROPERTIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2025-01-25 (Daniel Koch, Vikram Kushwaha)
--
--     -   Initial external release
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_copy_memory_indirect Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_copy_memory_indirect  ( CopyMemoryIndirectCommandKHR
                                                      , CopyMemoryIndirectInfoKHR
                                                      , CopyMemoryToImageIndirectCommandKHR
                                                      , CopyMemoryToImageIndirectInfoKHR
                                                      , PhysicalDeviceCopyMemoryIndirectFeaturesKHR
                                                      , PhysicalDeviceCopyMemoryIndirectPropertiesKHR
                                                      , StridedDeviceAddressRangeKHR
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CopyMemoryIndirectCommandKHR

instance ToCStruct CopyMemoryIndirectCommandKHR
instance Show CopyMemoryIndirectCommandKHR

instance FromCStruct CopyMemoryIndirectCommandKHR


data CopyMemoryIndirectInfoKHR

instance ToCStruct CopyMemoryIndirectInfoKHR
instance Show CopyMemoryIndirectInfoKHR

instance FromCStruct CopyMemoryIndirectInfoKHR


data CopyMemoryToImageIndirectCommandKHR

instance ToCStruct CopyMemoryToImageIndirectCommandKHR
instance Show CopyMemoryToImageIndirectCommandKHR

instance FromCStruct CopyMemoryToImageIndirectCommandKHR


data CopyMemoryToImageIndirectInfoKHR

instance ToCStruct CopyMemoryToImageIndirectInfoKHR
instance Show CopyMemoryToImageIndirectInfoKHR

instance FromCStruct CopyMemoryToImageIndirectInfoKHR


data PhysicalDeviceCopyMemoryIndirectFeaturesKHR

instance ToCStruct PhysicalDeviceCopyMemoryIndirectFeaturesKHR
instance Show PhysicalDeviceCopyMemoryIndirectFeaturesKHR

instance FromCStruct PhysicalDeviceCopyMemoryIndirectFeaturesKHR


data PhysicalDeviceCopyMemoryIndirectPropertiesKHR

instance ToCStruct PhysicalDeviceCopyMemoryIndirectPropertiesKHR
instance Show PhysicalDeviceCopyMemoryIndirectPropertiesKHR

instance FromCStruct PhysicalDeviceCopyMemoryIndirectPropertiesKHR


data StridedDeviceAddressRangeKHR

instance ToCStruct StridedDeviceAddressRangeKHR
instance Show StridedDeviceAddressRangeKHR

instance FromCStruct StridedDeviceAddressRangeKHR

