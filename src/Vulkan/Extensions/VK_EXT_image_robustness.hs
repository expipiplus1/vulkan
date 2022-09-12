{-# language CPP #-}
-- | = Name
--
-- VK_EXT_image_robustness - device extension
--
-- == VK_EXT_image_robustness
--
-- [__Name String__]
--     @VK_EXT_image_robustness@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     336
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_image_robustness] @gnl21%0A*Here describe the issue or question you have about the VK_EXT_image_robustness extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-04-27
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
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Spencer Fricke, Samsung
--
--     -   Courtney Goeltzenleuchter, Google
--
--     -   Slawomir Cygan, Intel
--
-- == Description
--
-- This extension adds stricter requirements for how out of bounds reads
-- from images are handled. Rather than returning undefined values, most
-- out of bounds reads return R, G, and B values of zero and alpha values
-- of either zero or one. Components not present in the image format may be
-- set to zero or to values based on the format as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-conversion-to-rgba Conversion to RGBA>.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceImageRobustnessFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME'
--
-- -   'EXT_IMAGE_ROBUSTNESS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- EXT suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == Issues
--
-- 1.  How does this extension differ from VK_EXT_robustness2?
--
-- The guarantees provided by this extension are a subset of those provided
-- by the robustImageAccess2 feature of VK_EXT_robustness2. Where this
-- extension allows return values of (0, 0, 0, 0) or (0, 0, 0, 1),
-- robustImageAccess2 requires that a particular value dependent on the
-- image format be returned. This extension provides no guarantees about
-- the values returned for an access to an invalid Lod.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2020-04-27 (Graeme Leese)
--
-- -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceImageRobustnessFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_image_robustness Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_robustness  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT
                                                  , PhysicalDeviceImageRobustnessFeaturesEXT
                                                  , EXT_IMAGE_ROBUSTNESS_SPEC_VERSION
                                                  , pattern EXT_IMAGE_ROBUSTNESS_SPEC_VERSION
                                                  , EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME
                                                  , pattern EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME
                                                  ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_EXT_image_robustness (PhysicalDeviceImageRobustnessFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceImageRobustnessFeaturesEXT"
type PhysicalDeviceImageRobustnessFeaturesEXT = PhysicalDeviceImageRobustnessFeatures


type EXT_IMAGE_ROBUSTNESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_IMAGE_ROBUSTNESS_SPEC_VERSION"
pattern EXT_IMAGE_ROBUSTNESS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_IMAGE_ROBUSTNESS_SPEC_VERSION = 1


type EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_image_robustness"

-- No documentation found for TopLevel "VK_EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME"
pattern EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_IMAGE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_image_robustness"

