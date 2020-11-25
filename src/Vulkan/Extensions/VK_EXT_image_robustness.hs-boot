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
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_image_robustness:%20&body=@gnl21%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-04-27
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-conversion-to-rgba Conversion to RGBA>.
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
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT'
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
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceImageRobustnessFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_robustness Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_image_robustness  (PhysicalDeviceImageRobustnessFeaturesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceImageRobustnessFeaturesEXT

instance ToCStruct PhysicalDeviceImageRobustnessFeaturesEXT
instance Show PhysicalDeviceImageRobustnessFeaturesEXT

instance FromCStruct PhysicalDeviceImageRobustnessFeaturesEXT

