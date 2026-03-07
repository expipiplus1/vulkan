{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_clamp_zero_one - device extension
--
-- = VK_EXT_depth_clamp_zero_one
--
-- [__Name String__]
--     @VK_EXT_depth_clamp_zero_one@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     422
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_depth_clamp_zero_one VK_KHR_depth_clamp_zero_one>
--         extension
--
-- [__Contact__]
--
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_depth_clamp_zero_one] @gnl21%0A*Here describe the issue or question you have about the VK_EXT_depth_clamp_zero_one extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-07-29
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
-- == Description
--
-- This extension gives defined behavior to fragment depth values which end
-- up outside the conventional [0, 1] range. It can be used to ensure
-- portability in edge cases of features like depthBias. The particular
-- behavior is chosen to match OpenGL to aid porting or emulation.
--
-- == Promotion to @VK_KHR_depth_clamp_zero_one@
--
-- All functionality in this extension is included in
-- @VK_KHR_depth_clamp_zero_one@, with the suffix change to KHR. The
-- original type, enum, and command names are still available as aliases of
-- the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthClampZeroOneFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-07-29 (Graeme Leese)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_depth_clamp_zero_one Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_clamp_zero_one  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_EXT
                                                      , PhysicalDeviceDepthClampZeroOneFeaturesEXT
                                                      , EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION
                                                      , pattern EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION
                                                      , EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME
                                                      , pattern EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME
                                                      , PhysicalDeviceDepthClampZeroOneFeaturesKHR(..)
                                                      ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_depth_clamp_zero_one (PhysicalDeviceDepthClampZeroOneFeaturesKHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_KHR))
import Vulkan.Extensions.VK_KHR_depth_clamp_zero_one (PhysicalDeviceDepthClampZeroOneFeaturesKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_KHR


-- No documentation found for TopLevel "VkPhysicalDeviceDepthClampZeroOneFeaturesEXT"
type PhysicalDeviceDepthClampZeroOneFeaturesEXT = PhysicalDeviceDepthClampZeroOneFeaturesKHR


type EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION"
pattern EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION = 1


type EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME = "VK_EXT_depth_clamp_zero_one"

-- No documentation found for TopLevel "VK_EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME"
pattern EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME = "VK_EXT_depth_clamp_zero_one"

