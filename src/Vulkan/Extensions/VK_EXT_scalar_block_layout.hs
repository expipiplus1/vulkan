{-# language CPP #-}
-- | = Name
--
-- VK_EXT_scalar_block_layout - device extension
--
-- == VK_EXT_scalar_block_layout
--
-- [__Name String__]
--     @VK_EXT_scalar_block_layout@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     222
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_scalar_block_layout] @tobski%0A*Here describe the issue or question you have about the VK_EXT_scalar_block_layout extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-11-14
--
-- [__Contributors__]
--
--     -   Jeff Bolz
--
--     -   Jan-Harald Fredriksen
--
--     -   Graeme Leese
--
--     -   Faith Ekstrand
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
-- == See Also
--
-- 'PhysicalDeviceScalarBlockLayoutFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_scalar_block_layout Vulkan Specification>
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

