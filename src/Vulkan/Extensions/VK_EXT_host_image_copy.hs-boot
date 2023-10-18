{-# language CPP #-}
-- | = Name
--
-- VK_EXT_host_image_copy - device extension
--
-- == VK_EXT_host_image_copy
--
-- [__Name String__]
--     @VK_EXT_host_image_copy@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     271
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_host_image_copy] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_host_image_copy extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_host_image_copy.adoc VK_EXT_host_image_copy>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-04-26
--
-- [__Contributors__]
--
--     -   Shahbaz Youssefi, Google
--
--     -   Faith Ekstrand, Collabora
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   James Fitzpatrick, Imagination
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- This extension allows applications to copy data between host memory and
-- images on the host processor, without staging the data through a
-- GPU-accessible buffer. This removes the need to allocate and manage the
-- buffer and its associated memory. On some architectures it may also
-- eliminate an extra copy operation. This extension additionally allows
-- applications to copy data between images on the host.
--
-- To support initializing a new image in preparation for a host copy, it
-- is now possible to transition a new image to
-- 'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_GENERAL' or other
-- host-copyable layouts via 'transitionImageLayoutEXT'. Additionally, it
-- is possible to perform copies that preserve the swizzling layout of the
-- image by using the 'HOST_IMAGE_COPY_MEMCPY_EXT' flag. In that case, the
-- memory size needed for copies to or from a buffer can be retrieved by
-- chaining 'SubresourceHostMemcpySizeEXT' to @pLayout@ in
-- 'getImageSubresourceLayout2EXT'.
--
-- == New Commands
--
-- -   'copyImageToImageEXT'
--
-- -   'copyImageToMemoryEXT'
--
-- -   'copyMemoryToImageEXT'
--
-- -   'getImageSubresourceLayout2EXT'
--
-- -   'transitionImageLayoutEXT'
--
-- == New Structures
--
-- -   'CopyImageToImageInfoEXT'
--
-- -   'CopyImageToMemoryInfoEXT'
--
-- -   'CopyMemoryToImageInfoEXT'
--
-- -   'HostImageLayoutTransitionInfoEXT'
--
-- -   'ImageSubresource2EXT'
--
-- -   'ImageToMemoryCopyEXT'
--
-- -   'MemoryToImageCopyEXT'
--
-- -   'SubresourceLayout2EXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2':
--
--     -   'HostImageCopyDevicePerformanceQueryEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceHostImageCopyFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceHostImageCopyPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_maintenance5.SubresourceLayout2KHR':
--
--     -   'SubresourceHostMemcpySizeEXT'
--
-- == New Enums
--
-- -   'HostImageCopyFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'HostImageCopyFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_HOST_IMAGE_COPY_EXTENSION_NAME'
--
-- -   'EXT_HOST_IMAGE_COPY_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE_EXT'
--
-- == Issues
--
-- 1) When uploading data to an image, the data is usually loaded from
-- disk. Why not have the application load the data directly into a
-- 'Vulkan.Core10.Handles.DeviceMemory' bound to a buffer (instead of host
-- memory), and use
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage'? The same
-- could be done when downloading data from an image.
--
-- __RESOLVED__: This may not always be possible. Complicated Vulkan
-- applications such as game engines often have decoupled subsystems for
-- streaming data and rendering. It may be unreasonable to require the
-- streaming subsystem to coordinate with the rendering subsystem to
-- allocate memory on its behalf, especially as Vulkan may not be the only
-- API supported by the engine. In emulation layers, the image data is
-- necessarily provided by the application in host memory, so an
-- optimization as suggested is not possible. Most importantly, the device
-- memory may not be mappable by an application, but still accessible to
-- the driver.
--
-- 2) Are @optimalBufferCopyOffsetAlignment@ and
-- @optimalBufferCopyRowPitchAlignment@ applicable to host memory as well
-- with the functions introduced by this extension? Or should there be new
-- limits?
--
-- __RESOLVED__: No alignment requirements for the host memory pointer.
--
-- 3) Should there be granularity requirements for image offsets and
-- extents?
--
-- __RESOLVED__: No granularity requirements, i.e. a granularity of 1 pixel
-- (for non-compressed formats) and 1 texel block (for compressed formats)
-- is assumed.
--
-- 4) How should the application deal with layout transitions before or
-- after copying to or from images?
--
-- __RESOLVED__: An existing issue with linear images is that when
-- emulating other APIs, it is impossible to know when to transition them
-- as they are written to by the host and then used bindlessly. The copy
-- operations in this extension are affected by the same limitation. A new
-- command is thus introduced by this extension to address this problem by
-- allowing the host to perform an image layout transition between a
-- handful of layouts.
--
-- == Version History
--
-- -   Revision 0, 2021-01-20 (Faith Ekstrand)
--
--     -   Initial idea and xml
--
-- -   Revision 1, 2023-04-26 (Shahbaz Youssefi)
--
--     -   Initial revision
--
-- == See Also
--
-- 'CopyImageToImageInfoEXT', 'CopyImageToMemoryInfoEXT',
-- 'CopyMemoryToImageInfoEXT', 'HostImageCopyDevicePerformanceQueryEXT',
-- 'HostImageCopyFlagBitsEXT', 'HostImageCopyFlagsEXT',
-- 'HostImageLayoutTransitionInfoEXT', 'ImageSubresource2EXT',
-- 'ImageToMemoryCopyEXT', 'MemoryToImageCopyEXT',
-- 'PhysicalDeviceHostImageCopyFeaturesEXT',
-- 'PhysicalDeviceHostImageCopyPropertiesEXT',
-- 'SubresourceHostMemcpySizeEXT', 'SubresourceLayout2EXT',
-- 'copyImageToImageEXT', 'copyImageToMemoryEXT', 'copyMemoryToImageEXT',
-- 'getImageSubresourceLayout2EXT', 'transitionImageLayoutEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_host_image_copy Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_host_image_copy  ( CopyImageToImageInfoEXT
                                                 , CopyImageToMemoryInfoEXT
                                                 , CopyMemoryToImageInfoEXT
                                                 , HostImageCopyDevicePerformanceQueryEXT
                                                 , HostImageLayoutTransitionInfoEXT
                                                 , ImageToMemoryCopyEXT
                                                 , MemoryToImageCopyEXT
                                                 , PhysicalDeviceHostImageCopyFeaturesEXT
                                                 , PhysicalDeviceHostImageCopyPropertiesEXT
                                                 , SubresourceHostMemcpySizeEXT
                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CopyImageToImageInfoEXT

instance ToCStruct CopyImageToImageInfoEXT
instance Show CopyImageToImageInfoEXT

instance FromCStruct CopyImageToImageInfoEXT


data CopyImageToMemoryInfoEXT

instance ToCStruct CopyImageToMemoryInfoEXT
instance Show CopyImageToMemoryInfoEXT

instance FromCStruct CopyImageToMemoryInfoEXT


data CopyMemoryToImageInfoEXT

instance ToCStruct CopyMemoryToImageInfoEXT
instance Show CopyMemoryToImageInfoEXT

instance FromCStruct CopyMemoryToImageInfoEXT


data HostImageCopyDevicePerformanceQueryEXT

instance ToCStruct HostImageCopyDevicePerformanceQueryEXT
instance Show HostImageCopyDevicePerformanceQueryEXT

instance FromCStruct HostImageCopyDevicePerformanceQueryEXT


data HostImageLayoutTransitionInfoEXT

instance ToCStruct HostImageLayoutTransitionInfoEXT
instance Show HostImageLayoutTransitionInfoEXT

instance FromCStruct HostImageLayoutTransitionInfoEXT


data ImageToMemoryCopyEXT

instance ToCStruct ImageToMemoryCopyEXT
instance Show ImageToMemoryCopyEXT

instance FromCStruct ImageToMemoryCopyEXT


data MemoryToImageCopyEXT

instance ToCStruct MemoryToImageCopyEXT
instance Show MemoryToImageCopyEXT

instance FromCStruct MemoryToImageCopyEXT


data PhysicalDeviceHostImageCopyFeaturesEXT

instance ToCStruct PhysicalDeviceHostImageCopyFeaturesEXT
instance Show PhysicalDeviceHostImageCopyFeaturesEXT

instance FromCStruct PhysicalDeviceHostImageCopyFeaturesEXT


data PhysicalDeviceHostImageCopyPropertiesEXT

instance ToCStruct PhysicalDeviceHostImageCopyPropertiesEXT
instance Show PhysicalDeviceHostImageCopyPropertiesEXT

instance FromCStruct PhysicalDeviceHostImageCopyPropertiesEXT


data SubresourceHostMemcpySizeEXT

instance ToCStruct SubresourceHostMemcpySizeEXT
instance Show SubresourceHostMemcpySizeEXT

instance FromCStruct SubresourceHostMemcpySizeEXT

