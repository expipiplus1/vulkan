{-# language CPP #-}
-- | = Name
--
-- VK_KHR_surface_maintenance1 - instance extension
--
-- = VK_KHR_surface_maintenance1
--
-- [__Name String__]
--     @VK_KHR_surface_maintenance1@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     487
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_surface_maintenance1] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_surface_maintenance1 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_surface_maintenance1.adoc VK_KHR_surface_maintenance1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-03-31
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
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
-- This extension is based off the @VK_EXT_surface_maintenance1@ extension.
--
-- @VK_KHR_surface_maintenance1@ adds a collection of window system
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
--     -   'SurfacePresentModeKHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfacePresentModeCompatibilityKHR'
--
--     -   'SurfacePresentScalingCapabilitiesKHR'
--
-- == New Enums
--
-- -   'PresentGravityFlagBitsKHR'
--
-- -   'PresentScalingFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'PresentGravityFlagsKHR'
--
-- -   'PresentScalingFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SURFACE_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'KHR_SURFACE_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_MODE_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2025-03-31 (Shahbaz Youssefi)
--
--     -   Based on VK_EXT_surface_maintenance1
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_surface_maintenance1 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_surface_maintenance1  ( SurfacePresentModeCompatibilityKHR
                                                      , SurfacePresentModeKHR
                                                      , SurfacePresentScalingCapabilitiesKHR
                                                      , PresentScalingFlagsKHR
                                                      , PresentScalingFlagBitsKHR
                                                      , PresentGravityFlagsKHR
                                                      , PresentGravityFlagBitsKHR
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data SurfacePresentModeCompatibilityKHR

instance ToCStruct SurfacePresentModeCompatibilityKHR
instance Show SurfacePresentModeCompatibilityKHR

instance FromCStruct SurfacePresentModeCompatibilityKHR


data SurfacePresentModeKHR

instance ToCStruct SurfacePresentModeKHR
instance Show SurfacePresentModeKHR

instance FromCStruct SurfacePresentModeKHR


data SurfacePresentScalingCapabilitiesKHR

instance ToCStruct SurfacePresentScalingCapabilitiesKHR
instance Show SurfacePresentScalingCapabilitiesKHR

instance FromCStruct SurfacePresentScalingCapabilitiesKHR


type PresentScalingFlagsKHR = PresentScalingFlagBitsKHR

data PresentScalingFlagBitsKHR


type PresentGravityFlagsKHR = PresentGravityFlagBitsKHR

data PresentGravityFlagBitsKHR

