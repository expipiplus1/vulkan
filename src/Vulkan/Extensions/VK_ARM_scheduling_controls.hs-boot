{-# language CPP #-}
-- | = Name
--
-- VK_ARM_scheduling_controls - device extension
--
-- = VK_ARM_scheduling_controls
--
-- [__Name String__]
--     @VK_ARM_scheduling_controls@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     418
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_shader_core_builtins VK_ARM_shader_core_builtins>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_scheduling_controls] @kpet%0A*Here describe the issue or question you have about the VK_ARM_scheduling_controls extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-09-05
--
-- [__Interactions and External Dependencies__]
--     None
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Mikel Garai, Arm Ltd.
--
-- == Description
--
-- This extension exposes a collection of controls to modify the scheduling
-- behavior of Arm Mali devices.
--
-- == New Commands
--
-- -   'cmdSetDispatchParametersARM'
--
-- == New Structures
--
-- -   'DispatchParametersARM'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceQueueCreateInfo',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DeviceQueueShaderCoreControlCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSchedulingControlsFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM'
--
--     -   'PhysicalDeviceSchedulingControlsPropertiesARM'
--
-- == New Enums
--
-- -   'PhysicalDeviceSchedulingControlsFlagBitsARM'
--
-- == New Bitmasks
--
-- -   'PhysicalDeviceSchedulingControlsFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_SCHEDULING_CONTROLS_EXTENSION_NAME'
--
-- -   'ARM_SCHEDULING_CONTROLS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_SHADER_CORE_CONTROL_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPATCH_PARAMETERS_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SCHEDULING_CONTROLS_PROPERTIES_ARM'
--
-- == New SPIR-V Capabilities
--
-- None.
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 2, 2025-09-05 (Kévin Petit)
--
--     -   Add dispatch parameters controls
--
--         -   'PHYSICAL_DEVICE_SCHEDULING_CONTROLS_DISPATCH_PARAMETERS_ARM'
--
--         -   'cmdSetDispatchParametersARM'
--
--         -   'DispatchParametersARM'
--
--         -   'PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM'
--
-- -   Revision 1, 2023-08-23 (Kévin Petit)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_scheduling_controls Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_scheduling_controls  ( DeviceQueueShaderCoreControlCreateInfoARM
                                                     , DispatchParametersARM
                                                     , PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM
                                                     , PhysicalDeviceSchedulingControlsFeaturesARM
                                                     , PhysicalDeviceSchedulingControlsPropertiesARM
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceQueueShaderCoreControlCreateInfoARM

instance ToCStruct DeviceQueueShaderCoreControlCreateInfoARM
instance Show DeviceQueueShaderCoreControlCreateInfoARM

instance FromCStruct DeviceQueueShaderCoreControlCreateInfoARM


data DispatchParametersARM

instance ToCStruct DispatchParametersARM
instance Show DispatchParametersARM

instance FromCStruct DispatchParametersARM


data PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM

instance ToCStruct PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM
instance Show PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM

instance FromCStruct PhysicalDeviceSchedulingControlsDispatchParametersPropertiesARM


data PhysicalDeviceSchedulingControlsFeaturesARM

instance ToCStruct PhysicalDeviceSchedulingControlsFeaturesARM
instance Show PhysicalDeviceSchedulingControlsFeaturesARM

instance FromCStruct PhysicalDeviceSchedulingControlsFeaturesARM


data PhysicalDeviceSchedulingControlsPropertiesARM

instance ToCStruct PhysicalDeviceSchedulingControlsPropertiesARM
instance Show PhysicalDeviceSchedulingControlsPropertiesARM

instance FromCStruct PhysicalDeviceSchedulingControlsPropertiesARM

