{-# language CPP #-}
-- | = Name
--
-- VK_EXT_extended_dynamic_state2 - device extension
--
-- == VK_EXT_extended_dynamic_state2
--
-- [__Name String__]
--     @VK_EXT_extended_dynamic_state2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     378
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
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_extended_dynamic_state2:%20&body=@vkushwaha-nv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-04-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds some more dynamic state to support applications that
-- need to reduce the number of pipeline state objects they compile and
-- bind.
--
-- == New Commands
--
-- -   'cmdSetDepthBiasEnableEXT'
--
-- -   'cmdSetLogicOpEXT'
--
-- -   'cmdSetPatchControlPointsEXT'
--
-- -   'cmdSetPrimitiveRestartEnableEXT'
--
-- -   'cmdSetRasterizerDiscardEnableEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExtendedDynamicState2FeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_2_EXTENSION_NAME'
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_2_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2021-04-12 (Vikram Kushwaha)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceExtendedDynamicState2FeaturesEXT',
-- 'cmdSetDepthBiasEnableEXT', 'cmdSetLogicOpEXT',
-- 'cmdSetPatchControlPointsEXT', 'cmdSetPrimitiveRestartEnableEXT',
-- 'cmdSetRasterizerDiscardEnableEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_extended_dynamic_state2  (PhysicalDeviceExtendedDynamicState2FeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceExtendedDynamicState2FeaturesEXT

instance ToCStruct PhysicalDeviceExtendedDynamicState2FeaturesEXT
instance Show PhysicalDeviceExtendedDynamicState2FeaturesEXT

instance FromCStruct PhysicalDeviceExtendedDynamicState2FeaturesEXT

