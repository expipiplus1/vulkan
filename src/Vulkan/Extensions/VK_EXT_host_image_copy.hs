{-# language CPP #-}
-- | = Name
--
-- VK_EXT_host_image_copy - device extension
--
-- = VK_EXT_host_image_copy
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
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--              or
--             
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>
--          and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
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
-- image by using the 'HOST_IMAGE_COPY_MEMCPY_BIT_EXT' flag. In that case,
-- the memory size needed for copies to or from a buffer can be retrieved
-- by chaining 'SubresourceHostMemcpySizeEXT' to @pLayout@ in
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
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.SubresourceLayout2':
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
--     -   'FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core14.Enums.HostImageCopyFlagBits.HostImageCopyFlagBits':
--
--     -   'HOST_IMAGE_COPY_MEMCPY_BIT_EXT'
--
--     -   'HOST_IMAGE_COPY_MEMCPY_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT'
--
--     -   'STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY_EXT'
--
--     -   'STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT'
--
--     -   'STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE_EXT'
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4, with the
-- EXT suffix omitted. However, the feature is made optional in Vulkan 1.4.
-- The original type, enum, and command names are still available as
-- aliases of the core functionality.
--
-- A Vulkan 1.4 implementation that has a
-- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' queue must
-- support either:
--
-- -   the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-hostImageCopy hostImageCopy>
--     feature; or
--
-- -   an additional queue that supports
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT'.
--
-- Additionally, all queues supporting
-- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
-- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT' must also
-- advertise 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_TRANSFER_BIT'.
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
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_host_image_copy Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_host_image_copy  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES_EXT
                                                 , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT
                                                 , pattern STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT
                                                 , pattern STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY_EXT
                                                 , pattern STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO_EXT
                                                 , pattern STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT
                                                 , pattern STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT
                                                 , pattern STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO_EXT
                                                 , pattern STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE_EXT
                                                 , pattern STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT
                                                 , pattern IMAGE_USAGE_HOST_TRANSFER_BIT_EXT
                                                 , pattern FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT
                                                 , pattern HOST_IMAGE_COPY_MEMCPY_BIT_EXT
                                                 , pattern HOST_IMAGE_COPY_MEMCPY_EXT
                                                 , copyMemoryToImageEXT
                                                 , copyImageToMemoryEXT
                                                 , copyImageToImageEXT
                                                 , transitionImageLayoutEXT
                                                 , getImageSubresourceLayout2EXT
                                                 , HostImageCopyFlagsEXT
                                                 , HostImageCopyFlagBitsEXT
                                                 , PhysicalDeviceHostImageCopyFeaturesEXT
                                                 , PhysicalDeviceHostImageCopyPropertiesEXT
                                                 , MemoryToImageCopyEXT
                                                 , ImageToMemoryCopyEXT
                                                 , CopyMemoryToImageInfoEXT
                                                 , CopyImageToMemoryInfoEXT
                                                 , CopyImageToImageInfoEXT
                                                 , HostImageLayoutTransitionInfoEXT
                                                 , SubresourceHostMemcpySizeEXT
                                                 , HostImageCopyDevicePerformanceQueryEXT
                                                 , ImageSubresource2EXT
                                                 , SubresourceLayout2EXT
                                                 , EXT_HOST_IMAGE_COPY_SPEC_VERSION
                                                 , pattern EXT_HOST_IMAGE_COPY_SPEC_VERSION
                                                 , EXT_HOST_IMAGE_COPY_EXTENSION_NAME
                                                 , pattern EXT_HOST_IMAGE_COPY_EXTENSION_NAME
                                                 ) where

import Data.String (IsString)
import Vulkan.Core14.PromotedStreamingTransfers' (copyImageToImage)
import Vulkan.Core14.PromotedStreamingTransfers' (copyImageToMemory)
import Vulkan.Core14.PromotedStreamingTransfers' (copyMemoryToImage)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap (getImageSubresourceLayout2)
import Vulkan.Core14.PromotedStreamingTransfers' (transitionImageLayout)
import Vulkan.Core14.PromotedStreamingTransfers' (CopyImageToImageInfo)
import Vulkan.Core14.PromotedStreamingTransfers' (CopyImageToMemoryInfo)
import Vulkan.Core14.PromotedStreamingTransfers' (CopyMemoryToImageInfo)
import Vulkan.Core14.PromotedStreamingTransfers' (HostImageCopyDevicePerformanceQuery)
import Vulkan.Core14.Enums.HostImageCopyFlagBits (HostImageCopyFlagBits)
import Vulkan.Core14.Enums.HostImageCopyFlagBits (HostImageCopyFlags)
import Vulkan.Core14.PromotedStreamingTransfers' (HostImageLayoutTransitionInfo)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap (ImageSubresource2)
import Vulkan.Core14.PromotedStreamingTransfers' (ImageToMemoryCopy)
import Vulkan.Core14.PromotedStreamingTransfers' (MemoryToImageCopy)
import Vulkan.Core14.PromotedStreamingTransfers' (PhysicalDeviceHostImageCopyFeatures)
import Vulkan.Core14.PromotedStreamingTransfers' (PhysicalDeviceHostImageCopyProperties)
import Vulkan.Core14.PromotedStreamingTransfers' (SubresourceHostMemcpySize)
import Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap (SubresourceLayout2)
import Vulkan.Core13.Enums.FormatFeatureFlags2 (FormatFeatureFlags2)
import Vulkan.Core13.Enums.FormatFeatureFlags2 (FormatFeatureFlagBits2(FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT))
import Vulkan.Core14.Enums.HostImageCopyFlagBits (HostImageCopyFlags)
import Vulkan.Core14.Enums.HostImageCopyFlagBits (HostImageCopyFlagBits(HOST_IMAGE_COPY_MEMCPY_BIT))
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlagBits(IMAGE_USAGE_HOST_TRANSFER_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_IMAGE_COPY_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT"
pattern STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY_EXT = STRUCTURE_TYPE_MEMORY_TO_IMAGE_COPY


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY_EXT"
pattern STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY_EXT = STRUCTURE_TYPE_IMAGE_TO_MEMORY_COPY


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO_EXT"
pattern STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO_EXT = STRUCTURE_TYPE_COPY_IMAGE_TO_MEMORY_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT"
pattern STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO_EXT = STRUCTURE_TYPE_COPY_MEMORY_TO_IMAGE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT"
pattern STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO_EXT = STRUCTURE_TYPE_HOST_IMAGE_LAYOUT_TRANSITION_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO_EXT"
pattern STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO_EXT = STRUCTURE_TYPE_COPY_IMAGE_TO_IMAGE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE_EXT"
pattern STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE_EXT = STRUCTURE_TYPE_SUBRESOURCE_HOST_MEMCPY_SIZE


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT"
pattern STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY_EXT = STRUCTURE_TYPE_HOST_IMAGE_COPY_DEVICE_PERFORMANCE_QUERY


-- No documentation found for TopLevel "VK_IMAGE_USAGE_HOST_TRANSFER_BIT_EXT"
pattern IMAGE_USAGE_HOST_TRANSFER_BIT_EXT = IMAGE_USAGE_HOST_TRANSFER_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT"
pattern FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT_EXT = FORMAT_FEATURE_2_HOST_IMAGE_TRANSFER_BIT


-- No documentation found for TopLevel "VK_HOST_IMAGE_COPY_MEMCPY_BIT_EXT"
pattern HOST_IMAGE_COPY_MEMCPY_BIT_EXT = HOST_IMAGE_COPY_MEMCPY_BIT


-- No documentation found for TopLevel "VK_HOST_IMAGE_COPY_MEMCPY_EXT"
pattern HOST_IMAGE_COPY_MEMCPY_EXT = HOST_IMAGE_COPY_MEMCPY_BIT


-- No documentation found for TopLevel "vkCopyMemoryToImageEXT"
copyMemoryToImageEXT = copyMemoryToImage


-- No documentation found for TopLevel "vkCopyImageToMemoryEXT"
copyImageToMemoryEXT = copyImageToMemory


-- No documentation found for TopLevel "vkCopyImageToImageEXT"
copyImageToImageEXT = copyImageToImage


-- No documentation found for TopLevel "vkTransitionImageLayoutEXT"
transitionImageLayoutEXT = transitionImageLayout


-- No documentation found for TopLevel "vkGetImageSubresourceLayout2EXT"
getImageSubresourceLayout2EXT = getImageSubresourceLayout2


-- No documentation found for TopLevel "VkHostImageCopyFlagsEXT"
type HostImageCopyFlagsEXT = HostImageCopyFlags


-- No documentation found for TopLevel "VkHostImageCopyFlagBitsEXT"
type HostImageCopyFlagBitsEXT = HostImageCopyFlagBits


-- No documentation found for TopLevel "VkPhysicalDeviceHostImageCopyFeaturesEXT"
type PhysicalDeviceHostImageCopyFeaturesEXT = PhysicalDeviceHostImageCopyFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceHostImageCopyPropertiesEXT"
type PhysicalDeviceHostImageCopyPropertiesEXT = PhysicalDeviceHostImageCopyProperties


-- No documentation found for TopLevel "VkMemoryToImageCopyEXT"
type MemoryToImageCopyEXT = MemoryToImageCopy


-- No documentation found for TopLevel "VkImageToMemoryCopyEXT"
type ImageToMemoryCopyEXT = ImageToMemoryCopy


-- No documentation found for TopLevel "VkCopyMemoryToImageInfoEXT"
type CopyMemoryToImageInfoEXT = CopyMemoryToImageInfo


-- No documentation found for TopLevel "VkCopyImageToMemoryInfoEXT"
type CopyImageToMemoryInfoEXT = CopyImageToMemoryInfo


-- No documentation found for TopLevel "VkCopyImageToImageInfoEXT"
type CopyImageToImageInfoEXT = CopyImageToImageInfo


-- No documentation found for TopLevel "VkHostImageLayoutTransitionInfoEXT"
type HostImageLayoutTransitionInfoEXT = HostImageLayoutTransitionInfo


-- No documentation found for TopLevel "VkSubresourceHostMemcpySizeEXT"
type SubresourceHostMemcpySizeEXT = SubresourceHostMemcpySize


-- No documentation found for TopLevel "VkHostImageCopyDevicePerformanceQueryEXT"
type HostImageCopyDevicePerformanceQueryEXT = HostImageCopyDevicePerformanceQuery


-- No documentation found for TopLevel "VkImageSubresource2EXT"
type ImageSubresource2EXT = ImageSubresource2


-- No documentation found for TopLevel "VkSubresourceLayout2EXT"
type SubresourceLayout2EXT = SubresourceLayout2


type EXT_HOST_IMAGE_COPY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_HOST_IMAGE_COPY_SPEC_VERSION"
pattern EXT_HOST_IMAGE_COPY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_HOST_IMAGE_COPY_SPEC_VERSION = 1


type EXT_HOST_IMAGE_COPY_EXTENSION_NAME = "VK_EXT_host_image_copy"

-- No documentation found for TopLevel "VK_EXT_HOST_IMAGE_COPY_EXTENSION_NAME"
pattern EXT_HOST_IMAGE_COPY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_HOST_IMAGE_COPY_EXTENSION_NAME = "VK_EXT_host_image_copy"

