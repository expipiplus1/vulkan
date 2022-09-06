{-# language CPP #-}
-- | = Name
--
-- VK_EXT_non_seamless_cube_map - device extension
--
-- == VK_EXT_non_seamless_cube_map
--
-- [__Name String__]
--     @VK_EXT_non_seamless_cube_map@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     423
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Special Uses__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse D3D support>
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Georg Lehmann
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_non_seamless_cube_map] @DadSchoorse%0A<<Here describe the issue or question you have about the VK_EXT_non_seamless_cube_map extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_non_seamless_cube_map.adoc VK_EXT_non_seamless_cube_map>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-04
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Georg Lehmann
--
-- == Description
--
-- This extension provides functionality to disable
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-cubemapedge cube map edge handling>
-- on a per sampler level which matches the behavior of other graphics
-- APIs.
--
-- This extension may be useful for building translation layers for those
-- APIs or for porting applications that rely on this cube map behavior.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceNonSeamlessCubeMapFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_NON_SEAMLESS_CUBE_MAP_EXTENSION_NAME'
--
-- -   'EXT_NON_SEAMLESS_CUBE_MAP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SamplerCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_NON_SEAMLESS_CUBE_MAP_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_NON_SEAMLESS_CUBE_MAP_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-09-04 (Georg Lehmann)
--
--     -   First Version
--
-- == See Also
--
-- 'PhysicalDeviceNonSeamlessCubeMapFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_non_seamless_cube_map Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_non_seamless_cube_map  (PhysicalDeviceNonSeamlessCubeMapFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceNonSeamlessCubeMapFeaturesEXT

instance ToCStruct PhysicalDeviceNonSeamlessCubeMapFeaturesEXT
instance Show PhysicalDeviceNonSeamlessCubeMapFeaturesEXT

instance FromCStruct PhysicalDeviceNonSeamlessCubeMapFeaturesEXT

