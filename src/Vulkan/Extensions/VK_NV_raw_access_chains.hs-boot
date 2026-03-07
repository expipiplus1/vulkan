{-# language CPP #-}
-- | = Name
--
-- VK_NV_raw_access_chains - device extension
--
-- = VK_NV_raw_access_chains
--
-- [__Name String__]
--     @VK_NV_raw_access_chains@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     556
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_raw_access_chains.html SPV_NV_raw_access_chains>
--
-- [__Contact__]
--
--     -   Rodrigo Locatti
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_raw_access_chains] @rlocatti%0A*Here describe the issue or question you have about the VK_NV_raw_access_chains extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-12-04
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_raw_access_chains.html SPV_NV_raw_access_chains>
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Rodrigo Locatti, NVIDIA
--
-- == Description
--
-- This extension allows the use of the @SPV_NV_raw_access_chains@
-- extension in SPIR-V shader modules. This enables SPIR-V producers to
-- efficiently implement interfaces similar to Direct3D structured buffers
-- and byte address buffers, allowing shaders compiled from an HLSL source
-- to generate more efficient code.
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-RawAccessChainsNV RawAccessChainsNV>
--
-- == Version History
--
-- -   Revision 1, 2023-12-04 (Rodrigo Locatti)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_raw_access_chains Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_raw_access_chains  (PhysicalDeviceRawAccessChainsFeaturesNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceRawAccessChainsFeaturesNV

instance ToCStruct PhysicalDeviceRawAccessChainsFeaturesNV
instance Show PhysicalDeviceRawAccessChainsFeaturesNV

instance FromCStruct PhysicalDeviceRawAccessChainsFeaturesNV

