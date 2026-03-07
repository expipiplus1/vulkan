{-# language CPP #-}
-- | = Name
--
-- VK_EXT_surface_maintenance1 - instance extension
--
-- = VK_EXT_surface_maintenance1
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
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface_maintenance1 VK_KHR_surface_maintenance1>
--         extension
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
--     2022-12-16
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to @VK_KHR_surface_maintenance1@
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
-- == Promotion to @VK_KHR_surface_maintenance1@
--
-- All functionality in this extension is included in
-- @VK_KHR_surface_maintenance1@, with the suffix changed to KHR. The
-- original type, enum, and command names are still available as aliases of
-- the KHR functionality.
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
--     -   'STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT'
--
--     -   'STRUCTURE_TYPE_SURFACE_PRESENT_MODE_EXT'
--
--     -   'STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_EXT'
--
-- == Version History
--
-- -   Revision 0, 2019-02-27 (Lionel Landwerlin)
--
--     -   Internal revisions
--
-- -   Revision 0, 2020-06-15 (James Jones)
--
--     -   Internal revisions
--
-- -   Revision 1, 2022-11-09 (Shahbaz Youssefi)
--
--     -   Add functionality and complete spec
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_surface_maintenance1 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_surface_maintenance1  ( pattern STRUCTURE_TYPE_SURFACE_PRESENT_MODE_EXT
                                                      , pattern STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_EXT
                                                      , pattern STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT
                                                      , PresentScalingFlagsEXT
                                                      , PresentGravityFlagsEXT
                                                      , PresentScalingFlagBitsEXT
                                                      , PresentGravityFlagBitsEXT
                                                      , SurfacePresentModeEXT
                                                      , SurfacePresentScalingCapabilitiesEXT
                                                      , SurfacePresentModeCompatibilityEXT
                                                      , EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION
                                                      , pattern EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION
                                                      , EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME
                                                      , pattern EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME
                                                      , SurfacePresentModeKHR(..)
                                                      , SurfacePresentScalingCapabilitiesKHR(..)
                                                      , SurfacePresentModeCompatibilityKHR(..)
                                                      , PresentModeKHR(..)
                                                      , PresentScalingFlagBitsKHR(..)
                                                      , PresentScalingFlagsKHR
                                                      , PresentGravityFlagBitsKHR(..)
                                                      , PresentGravityFlagsKHR
                                                      ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentGravityFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentGravityFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentScalingFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentScalingFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (SurfacePresentModeCompatibilityKHR)
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (SurfacePresentModeKHR)
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (SurfacePresentScalingCapabilitiesKHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_PRESENT_MODE_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_KHR))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentGravityFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentGravityFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentScalingFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentScalingFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (SurfacePresentModeCompatibilityKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (SurfacePresentModeKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (SurfacePresentScalingCapabilitiesKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SURFACE_PRESENT_MODE_EXT"
pattern STRUCTURE_TYPE_SURFACE_PRESENT_MODE_EXT = STRUCTURE_TYPE_SURFACE_PRESENT_MODE_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_EXT"
pattern STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_EXT = STRUCTURE_TYPE_SURFACE_PRESENT_SCALING_CAPABILITIES_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT"
pattern STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_EXT = STRUCTURE_TYPE_SURFACE_PRESENT_MODE_COMPATIBILITY_KHR


-- No documentation found for TopLevel "VkPresentScalingFlagsEXT"
type PresentScalingFlagsEXT = PresentScalingFlagsKHR


-- No documentation found for TopLevel "VkPresentGravityFlagsEXT"
type PresentGravityFlagsEXT = PresentGravityFlagsKHR


-- No documentation found for TopLevel "VkPresentScalingFlagBitsEXT"
type PresentScalingFlagBitsEXT = PresentScalingFlagBitsKHR


-- No documentation found for TopLevel "VkPresentGravityFlagBitsEXT"
type PresentGravityFlagBitsEXT = PresentGravityFlagBitsKHR


-- No documentation found for TopLevel "VkSurfacePresentModeEXT"
type SurfacePresentModeEXT = SurfacePresentModeKHR


-- No documentation found for TopLevel "VkSurfacePresentScalingCapabilitiesEXT"
type SurfacePresentScalingCapabilitiesEXT = SurfacePresentScalingCapabilitiesKHR


-- No documentation found for TopLevel "VkSurfacePresentModeCompatibilityEXT"
type SurfacePresentModeCompatibilityEXT = SurfacePresentModeCompatibilityKHR


type EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION"
pattern EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SURFACE_MAINTENANCE_1_SPEC_VERSION = 1


type EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME = "VK_EXT_surface_maintenance1"

-- No documentation found for TopLevel "VK_EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME"
pattern EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SURFACE_MAINTENANCE_1_EXTENSION_NAME = "VK_EXT_surface_maintenance1"

