{-# language CPP #-}
-- | = Name
--
-- VK_AMD_display_native_hdr - device extension
--
-- == VK_AMD_display_native_hdr
--
-- [__Name String__]
--     @VK_AMD_display_native_hdr@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     214
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
--     -   Requires @VK_KHR_get_surface_capabilities2@
--
--     -   Requires @VK_KHR_swapchain@
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_AMD_display_native_hdr:%20&body=@anteru%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Aaron Hagan, AMD
--
--     -   Aric Cyr, AMD
--
--     -   Timothy Lottes, AMD
--
--     -   Derrick Owens, AMD
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension introduces the following display native HDR features to
-- Vulkan:
--
-- -   A new 'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR' enum for
--     setting the native display colorspace. For example, this color space
--     would be set by the swapchain to use the native color space in
--     Freesync2 displays.
--
-- -   Local dimming control
--
-- == New Commands
--
-- -   'setLocalDimmingAMD'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'DisplayNativeHdrSurfaceCapabilitiesAMD'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SwapchainDisplayNativeHdrCreateInfoAMD'
--
-- == New Enum Constants
--
-- -   'AMD_DISPLAY_NATIVE_HDR_EXTENSION_NAME'
--
-- -   'AMD_DISPLAY_NATIVE_HDR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_DISPLAY_NATIVE_AMD'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD'
--
-- == Issues
--
-- None.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2018-12-18 (Daniel Rakos)
--
--     -   Initial revision
--
-- = See Also
--
-- 'DisplayNativeHdrSurfaceCapabilitiesAMD',
-- 'SwapchainDisplayNativeHdrCreateInfoAMD', 'setLocalDimmingAMD'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_display_native_hdr Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_display_native_hdr  ( DisplayNativeHdrSurfaceCapabilitiesAMD
                                                    , SwapchainDisplayNativeHdrCreateInfoAMD
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DisplayNativeHdrSurfaceCapabilitiesAMD

instance ToCStruct DisplayNativeHdrSurfaceCapabilitiesAMD
instance Show DisplayNativeHdrSurfaceCapabilitiesAMD

instance FromCStruct DisplayNativeHdrSurfaceCapabilitiesAMD


data SwapchainDisplayNativeHdrCreateInfoAMD

instance ToCStruct SwapchainDisplayNativeHdrCreateInfoAMD
instance Show SwapchainDisplayNativeHdrCreateInfoAMD

instance FromCStruct SwapchainDisplayNativeHdrCreateInfoAMD

