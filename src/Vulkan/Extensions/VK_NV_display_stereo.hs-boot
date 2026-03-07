{-# language CPP #-}
-- | = Name
--
-- VK_NV_display_stereo - instance extension
--
-- = VK_NV_display_stereo
--
-- [__Name String__]
--     @VK_NV_display_stereo@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     552
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_display VK_KHR_display>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 VK_KHR_get_display_properties2>
--
-- [__Contact__]
--
--     -   Russell Chou
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_display_stereo] @russellcnv%0A*Here describe the issue or question you have about the VK_NV_display_stereo extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_display_stereo.adoc VK_NV_display_stereo>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-11-20
--
-- [__Contributors__]
--
--     -   Russell Chou, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   James Jones, NVIDIA
--
-- == Description
--
-- This extension allows the application to choose which type of 3D stereo
-- hardware it wants to use so the driver can configure it properly. This
-- configuration is useful for swapchains created from display surfaces
-- because some environments do not have an intermediate windowing system
-- available for easy configuration. This extension will override any
-- stereo type configuration in the windowing system.
--
-- For HDMI 3D, only some display modes support stereo rendering, and a new
-- structure is needed to expose that information to the application.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayModeProperties2KHR':
--
--     -   'DisplayModeStereoPropertiesNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateInfoKHR':
--
--     -   'DisplaySurfaceStereoCreateInfoNV'
--
-- == New Enums
--
-- -   'DisplaySurfaceStereoTypeNV'
--
-- == New Enum Constants
--
-- -   'NV_DISPLAY_STEREO_EXTENSION_NAME'
--
-- -   'NV_DISPLAY_STEREO_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_MODE_STEREO_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_SURFACE_STEREO_CREATE_INFO_NV'
--
-- == Version History
--
-- -   Revision 1, 2024-11-20 (Russell Chou)
--
--     -   Initial release
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_display_stereo Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_display_stereo  ( DisplayModeStereoPropertiesNV
                                               , DisplaySurfaceStereoCreateInfoNV
                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DisplayModeStereoPropertiesNV

instance ToCStruct DisplayModeStereoPropertiesNV
instance Show DisplayModeStereoPropertiesNV

instance FromCStruct DisplayModeStereoPropertiesNV


data DisplaySurfaceStereoCreateInfoNV

instance ToCStruct DisplaySurfaceStereoCreateInfoNV
instance Show DisplaySurfaceStereoCreateInfoNV

instance FromCStruct DisplaySurfaceStereoCreateInfoNV

