{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_compression_control_swapchain - device extension
--
-- == VK_EXT_image_compression_control_swapchain
--
-- [__Name String__]
--     @VK_EXT_image_compression_control_swapchain@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     438
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_compression_control_swapchain] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_image_compression_control_swapchain extension* >
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
--     -   Ian Elliott, Google
--
-- == Description
--
-- This extension enables fixed-rate image compression and adds the ability
-- to control when this kind of compression can be applied to swapchain
-- images.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_COMPRESSION_CONTROL_SWAPCHAIN_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-05-02 (Jan-Harald Fredriksen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_compression_control_swapchain Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_compression_control_swapchain  (PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT

instance ToCStruct PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT
instance Show PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT

instance FromCStruct PhysicalDeviceImageCompressionControlSwapchainFeaturesEXT

