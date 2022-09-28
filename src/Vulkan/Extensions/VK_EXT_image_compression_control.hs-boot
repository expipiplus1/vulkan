{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_compression_control - device extension
--
-- == VK_EXT_image_compression_control
--
-- [__Name String__]
--     @VK_EXT_image_compression_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     339
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_compression_control] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_image_compression_control extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_image_compression_control.adoc VK_EXT_image_compression_control>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Graeme Leese, Broadcom
--
--     -   Andrew Garrard, Imagination
--
--     -   Lisa Wu, Arm
--
--     -   Peter Kohaut, Arm
--
-- == Description
--
-- This extension enables fixed-rate image compression and adds the ability
-- to control when this kind of compression can be applied. Many
-- implementations support some form of framebuffer compression. This is
-- typically transparent to applications as lossless compression schemes
-- are used. With fixed-rate compression, the compression is done at a
-- defined bitrate. Such compression algorithms generally produce results
-- that are visually lossless, but the results are typically not bit-exact
-- when compared to a non-compressed result. The implementation may not be
-- able to use the requested compression rate in all cases. This extension
-- adds a query that can be used to determine the compression scheme and
-- rate that was applied to an image.
--
-- == New Commands
--
-- -   'getImageSubresourceLayout2EXT'
--
-- == New Structures
--
-- -   'ImageSubresource2EXT'
--
-- -   'SubresourceLayout2EXT'
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2':
--
--     -   'ImageCompressionControlEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2',
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceFormat2KHR',
--     'SubresourceLayout2EXT':
--
--     -   'ImageCompressionPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageCompressionControlFeaturesEXT'
--
-- == New Enums
--
-- -   'ImageCompressionFixedRateFlagBitsEXT'
--
-- -   'ImageCompressionFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'ImageCompressionFixedRateFlagsEXT'
--
-- -   'ImageCompressionFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_COMPRESSION_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_COMPRESSION_CONTROL_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_COMPRESSION_EXHAUSTED_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COMPRESSION_CONTROL_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_COMPRESSION_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-05-02 (Jan-Harald Fredriksen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'ImageCompressionControlEXT', 'ImageCompressionFixedRateFlagBitsEXT',
-- 'ImageCompressionFixedRateFlagsEXT', 'ImageCompressionFlagBitsEXT',
-- 'ImageCompressionFlagsEXT', 'ImageCompressionPropertiesEXT',
-- 'ImageSubresource2EXT',
-- 'PhysicalDeviceImageCompressionControlFeaturesEXT',
-- 'SubresourceLayout2EXT', 'getImageSubresourceLayout2EXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_compression_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_compression_control  ( ImageCompressionControlEXT
                                                           , ImageCompressionPropertiesEXT
                                                           , ImageSubresource2EXT
                                                           , PhysicalDeviceImageCompressionControlFeaturesEXT
                                                           , SubresourceLayout2EXT
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data ImageCompressionControlEXT

instance ToCStruct ImageCompressionControlEXT
instance Show ImageCompressionControlEXT

instance FromCStruct ImageCompressionControlEXT


data ImageCompressionPropertiesEXT

instance ToCStruct ImageCompressionPropertiesEXT
instance Show ImageCompressionPropertiesEXT

instance FromCStruct ImageCompressionPropertiesEXT


data ImageSubresource2EXT

instance ToCStruct ImageSubresource2EXT
instance Show ImageSubresource2EXT

instance FromCStruct ImageSubresource2EXT


data PhysicalDeviceImageCompressionControlFeaturesEXT

instance ToCStruct PhysicalDeviceImageCompressionControlFeaturesEXT
instance Show PhysicalDeviceImageCompressionControlFeaturesEXT

instance FromCStruct PhysicalDeviceImageCompressionControlFeaturesEXT


type role SubresourceLayout2EXT nominal
data SubresourceLayout2EXT (es :: [Type])

instance ( Extendss SubresourceLayout2EXT es
         , PokeChain es ) => ToCStruct (SubresourceLayout2EXT es)
instance Show (Chain es) => Show (SubresourceLayout2EXT es)

instance ( Extendss SubresourceLayout2EXT es
         , PeekChain es ) => FromCStruct (SubresourceLayout2EXT es)

