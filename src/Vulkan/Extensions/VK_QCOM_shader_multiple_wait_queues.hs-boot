{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_shader_multiple_wait_queues - device extension
--
-- = VK_QCOM_shader_multiple_wait_queues
--
-- [__Name String__]
--     @VK_QCOM_shader_multiple_wait_queues@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     305
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
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_multiple_wait_queues.html SPV_QCOM_multiple_wait_queues>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_shader_multiple_wait_queues] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_shader_multiple_wait_queues extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_shader_multiple_wait_queues.adoc VK_QCOM_shader_multiple_wait_queues>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-05-04
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/qcom/GLSL_QCOM_multiple_wait_queues.txt GLSL_QCOM_multiple_wait_queues>
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc.
--
--     -   Elina Kamenetskaya, Qualcomm Technologies, Inc.
--
--     -   Wooyoung Kim, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension adds a new loop control hint to the SPIR-V execution
-- environment, instructing the compiler that it should use multiple wait
-- queues to optimize a loop.
--
-- This can improve performance for loops that have high latency
-- instructions such as for @VK_KHR_cooperative_matrix@ operations, by
-- allowing the compiler to issue instructions for future iterations while
-- waiting for the current iteration to complete.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_SHADER_MULTIPLE_WAIT_QUEUES_EXTENSION_NAME'
--
-- -   'QCOM_SHADER_MULTIPLE_WAIT_QUEUES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MULTIPLE_WAIT_QUEUES_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MULTIPLE_WAIT_QUEUES_PROPERTIES_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2026-05-04 (Matthew Netsch)
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_shader_multiple_wait_queues Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_shader_multiple_wait_queues  ( PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM
                                                              , PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM
                                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM

instance ToCStruct PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM
instance Show PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM

instance FromCStruct PhysicalDeviceShaderMultipleWaitQueuesFeaturesQCOM


data PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM

instance ToCStruct PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM
instance Show PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM

instance FromCStruct PhysicalDeviceShaderMultipleWaitQueuesPropertiesQCOM

