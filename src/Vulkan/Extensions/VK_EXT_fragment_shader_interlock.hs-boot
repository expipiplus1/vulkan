{-# language CPP #-}
-- | = Name
--
-- VK_EXT_fragment_shader_interlock - device extension
--
-- == VK_EXT_fragment_shader_interlock
--
-- [__Name String__]
--     @VK_EXT_fragment_shader_interlock@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     252
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
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_fragment_shader_interlock:%20&body=@pdaniell-nv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-02
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_fragment_shader_interlock.html SPV_EXT_fragment_shader_interlock>
--
--     -   This extension provides API support for
--         <https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_fragment_shader_interlock.txt GL_ARB_fragment_shader_interlock>
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jason Ekstrand, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Ruihao Zhang, Qualcomm
--
--     -   Slawomir Grajewski, Intel
--
--     -   Spencer Fricke, Samsung
--
-- == Description
--
-- This extension adds support for the @FragmentShaderPixelInterlockEXT@,
-- @FragmentShaderSampleInterlockEXT@, and
-- @FragmentShaderShadingRateInterlockEXT@ capabilities from the
-- @SPV_EXT_fragment_shader_interlock@ extension to Vulkan.
--
-- Enabling these capabilities provides a critical section for fragment
-- shaders to avoid overlapping pixels being processed at the same time,
-- and certain guarantees about the ordering of fragment shader invocations
-- of fragments of overlapping pixels.
--
-- This extension can be useful for algorithms that need to access
-- per-pixel data structures via shader loads and stores. Algorithms using
-- this extension can access per-pixel data structures in critical sections
-- without other invocations accessing the same per-pixel data.
-- Additionally, the ordering guarantees are useful for cases where the API
-- ordering of fragments is meaningful. For example, applications may be
-- able to execute programmable blending operations in the fragment shader,
-- where the destination buffer is read via image loads and the final value
-- is written via image stores.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentShaderInterlockFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FRAGMENT_SHADER_INTERLOCK_EXTENSION_NAME'
--
-- -   'EXT_FRAGMENT_SHADER_INTERLOCK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-FragmentShaderSampleInterlockEXT FragmentShaderInterlockEXT>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-FragmentShaderPixelInterlockEXT FragmentShaderPixelInterlockEXT>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-FragmentShaderShadingRateInterlockEXT FragmentShaderShadingRateInterlockEXT>
--
-- == Version History
--
-- -   Revision 1, 2019-05-24 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceFragmentShaderInterlockFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_shader_interlock Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_fragment_shader_interlock  (PhysicalDeviceFragmentShaderInterlockFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceFragmentShaderInterlockFeaturesEXT

instance ToCStruct PhysicalDeviceFragmentShaderInterlockFeaturesEXT
instance Show PhysicalDeviceFragmentShaderInterlockFeaturesEXT

instance FromCStruct PhysicalDeviceFragmentShaderInterlockFeaturesEXT

