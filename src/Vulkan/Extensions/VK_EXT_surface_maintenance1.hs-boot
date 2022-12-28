{-# language CPP #-}
-- | = Name
--
-- VK_EXT_surface_maintenance1 - instance extension
--
-- == VK_EXT_surface_maintenance1
--
-- [__Name String__]
--     @VK_EXT_surface_maintenance1@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     275
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@ to be enabled
--
--     -   Requires @VK_KHR_get_surface_capabilities2@ to be enabled
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_surface_maintenance1] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_surface_maintenance1 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_surface_maintenance1.adoc VK_EXT_surface_maintenance1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-11-09
--
-- [__Contributors__]
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Lionel Landwerlin, Intel
--
--     -   Shahbaz Youssefi, Google
--
--     -   Chris Forbes, Google
--
--     -   Ian Elliott, Google
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Daniel Stone, Collabora
--
-- == Description
--
-- @VK_EXT_surface_maintenance1@ adds a collection of window system
-- integration features that were intentionally left out or overlooked in
-- the original @VK_KHR_surface@ extension.
--
-- The new features are as follows:
--
-- -   Allow querying number of min\/max images from a surface for a
--     particular presentation mode.
--
-- -   Allow querying a surface’s scaled presentation capabilities.
--
-- -   Allow querying a surface for the set of presentation modes which can
--     be easily switched between without requiring swapchain recreation.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR':
--
--     -   'SurfacePresentModeEXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfacePresentModeCompatibilityEXT'
--
--     -   'SurfacePresentScalingCapabilitiesEXT'
--
-- == New Enums
--
-- -   'PresentGravityFlagBitsEXT'
--
-- -   'PresentScalingFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'PresentGravityFlagsEXT'
--
-- -   'PresentScalingFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_EXT'
--
-- == Version History
--
-- -   Revision 0, 2019-02-27 (Lionel Landwerlin)
--
--     -   Internal revisions
--
-- -   Revision 1, 2022-11-09 (Shahbaz Youssefi)
--
--     -   Add functionality and complete spec
--
-- == See Also
--
-- 'PresentGravityFlagBitsEXT', 'PresentGravityFlagsEXT',
-- 'PresentScalingFlagBitsEXT', 'PresentScalingFlagsEXT',
-- 'SurfacePresentModeCompatibilityEXT', 'SurfacePresentModeEXT',
-- 'SurfacePresentScalingCapabilitiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_surface_maintenance1  ( SurfacePresentModeCompatibilityEXT
                                                      , SurfacePresentModeEXT
                                                      , SurfacePresentScalingCapabilitiesEXT
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data SurfacePresentModeCompatibilityEXT

instance ToCStruct SurfacePresentModeCompatibilityEXT
instance Show SurfacePresentModeCompatibilityEXT

instance FromCStruct SurfacePresentModeCompatibilityEXT


data SurfacePresentModeEXT

instance ToCStruct SurfacePresentModeEXT
instance Show SurfacePresentModeEXT

instance FromCStruct SurfacePresentModeEXT


data SurfacePresentScalingCapabilitiesEXT

instance ToCStruct SurfacePresentScalingCapabilitiesEXT
instance Show SurfacePresentScalingCapabilitiesEXT

instance FromCStruct SurfacePresentScalingCapabilitiesEXT

