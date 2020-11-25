{-# language CPP #-}
-- | = Name
--
-- VK_EXT_scalar_block_layout - device extension
--
-- = Registered Extension Number
--
-- 222
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- = Deprecation state
--
-- -   /Promoted/ to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-11-14
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__Contributors__]
--
--     -   Jeff Bolz
--
--     -   Jan-Harald Fredriksen
--
--     -   Graeme Leese
--
--     -   Jason Ekstrand
--
--     -   John Kessenich
--
-- == Description
--
-- This extension enables C-like structure layout for SPIR-V blocks. It
-- modifies the alignment rules for uniform buffers, storage buffers and
-- push constants, allowing non-scalar types to be aligned solely based on
-- the size of their components, without additional requirements.
--
-- == Promotion to Vulkan 1.2
--
-- Functionality in this extension is included in core Vulkan 1.2, with the
-- EXT suffix omitted. However, if Vulkan 1.2 is supported and this
-- extension is not, the @scalarBlockLayout@ capability is optional. The
-- original type, enum and command names are still available as aliases of
-- the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceScalarBlockLayoutFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME'
--
-- -   'EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-11-14 (Tobias Hector)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDeviceScalarBlockLayoutFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_scalar_block_layout Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_scalar_block_layout  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
                                                     , PhysicalDeviceScalarBlockLayoutFeaturesEXT
                                                     , EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
                                                     , pattern EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION
                                                     , EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
                                                     , pattern EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout (PhysicalDeviceScalarBlockLayoutFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceScalarBlockLayoutFeaturesEXT"
type PhysicalDeviceScalarBlockLayoutFeaturesEXT = PhysicalDeviceScalarBlockLayoutFeatures


type EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION"
pattern EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SCALAR_BLOCK_LAYOUT_SPEC_VERSION = 1


type EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME = "VK_EXT_scalar_block_layout"

-- No documentation found for TopLevel "VK_EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME"
pattern EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SCALAR_BLOCK_LAYOUT_EXTENSION_NAME = "VK_EXT_scalar_block_layout"

