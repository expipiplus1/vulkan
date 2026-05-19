{-# language CPP #-}
-- | = Name
--
-- VK_AMD_gpa_interface - device extension
--
-- = VK_AMD_gpa_interface
--
-- [__Name String__]
--     @VK_AMD_gpa_interface@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     134
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
-- [__Contact__]
--
--     -   Stu Smith
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_gpa_interface] @stu-s%0A*Here describe the issue or question you have about the VK_AMD_gpa_interface extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_AMD_gpa_interface.adoc VK_AMD_gpa_interface>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-05-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Noah Fredriks, AMD
--
--     -   Peter Lohrmann, AMD
--
--     -   Maciej Dziuban, AMD
--
-- == Description
--
-- This extension adds GPU Performance API (GPA) interface support for
-- accessing GPU global performance counters, streaming performance
-- monitors (SPM), and thread traces (SQTT), on AMD Radeon™ GPUs.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.GpaSessionAMD'
--
-- == New Commands
--
-- -   'cmdBeginGpaSampleAMD'
--
-- -   'cmdBeginGpaSessionAMD'
--
-- -   'cmdCopyGpaSessionResultsAMD'
--
-- -   'cmdEndGpaSampleAMD'
--
-- -   'cmdEndGpaSessionAMD'
--
-- -   'createGpaSessionAMD'
--
-- -   'destroyGpaSessionAMD'
--
-- -   'getGpaDeviceClockInfoAMD'
--
-- -   'getGpaSessionResultsAMD'
--
-- -   'getGpaSessionStatusAMD'
--
-- -   'resetGpaSessionAMD'
--
-- -   'setGpaDeviceClockModeAMD'
--
-- == New Structures
--
-- -   'GpaDeviceClockModeInfoAMD'
--
-- -   'GpaDeviceGetClockInfoAMD'
--
-- -   'GpaPerfBlockPropertiesAMD'
--
-- -   'GpaPerfCounterAMD'
--
-- -   'GpaSampleBeginInfoAMD'
--
-- -   'GpaSessionCreateInfoAMD'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceGpaFeaturesAMD'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceGpaProperties2AMD'
--
--     -   'PhysicalDeviceGpaPropertiesAMD'
--
-- == New Enums
--
-- -   'GpaDeviceClockModeAMD'
--
-- -   'GpaPerfBlockAMD'
--
-- -   'GpaSampleTypeAMD'
--
-- -   'GpaSqShaderStageFlagBitsAMD'
--
-- == New Bitmasks
--
-- -   'GpaPerfBlockPropertiesFlagsAMD'
--
-- -   'GpaSqShaderStageFlagsAMD'
--
-- -   'PhysicalDeviceGpaPropertiesFlagsAMD'
--
-- == New Enum Constants
--
-- -   'AMD_GPA_INTERFACE_EXTENSION_NAME'
--
-- -   'AMD_GPA_INTERFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_GPA_SESSION_AMD'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GPA_DEVICE_CLOCK_MODE_INFO_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GPA_DEVICE_GET_CLOCK_INFO_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GPA_SAMPLE_BEGIN_INFO_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GPA_SESSION_CREATE_INFO_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_FEATURES_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_PROPERTIES_2_AMD'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GPA_PROPERTIES_AMD'
--
-- == Version History
--
-- -   Revision 1, 2026-05-01 (Stu Smith)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_AMD_gpa_interface Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_gpa_interface  ( GpaDeviceClockModeInfoAMD
                                               , GpaDeviceGetClockInfoAMD
                                               , GpaPerfBlockPropertiesAMD
                                               , GpaPerfCounterAMD
                                               , GpaSampleBeginInfoAMD
                                               , GpaSessionCreateInfoAMD
                                               , PhysicalDeviceGpaFeaturesAMD
                                               , PhysicalDeviceGpaProperties2AMD
                                               , PhysicalDeviceGpaPropertiesAMD
                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data GpaDeviceClockModeInfoAMD

instance ToCStruct GpaDeviceClockModeInfoAMD
instance Show GpaDeviceClockModeInfoAMD

instance FromCStruct GpaDeviceClockModeInfoAMD


data GpaDeviceGetClockInfoAMD

instance ToCStruct GpaDeviceGetClockInfoAMD
instance Show GpaDeviceGetClockInfoAMD

instance FromCStruct GpaDeviceGetClockInfoAMD


data GpaPerfBlockPropertiesAMD

instance ToCStruct GpaPerfBlockPropertiesAMD
instance Show GpaPerfBlockPropertiesAMD

instance FromCStruct GpaPerfBlockPropertiesAMD


data GpaPerfCounterAMD

instance ToCStruct GpaPerfCounterAMD
instance Show GpaPerfCounterAMD

instance FromCStruct GpaPerfCounterAMD


data GpaSampleBeginInfoAMD

instance ToCStruct GpaSampleBeginInfoAMD
instance Show GpaSampleBeginInfoAMD

instance FromCStruct GpaSampleBeginInfoAMD


data GpaSessionCreateInfoAMD

instance ToCStruct GpaSessionCreateInfoAMD
instance Show GpaSessionCreateInfoAMD

instance FromCStruct GpaSessionCreateInfoAMD


data PhysicalDeviceGpaFeaturesAMD

instance ToCStruct PhysicalDeviceGpaFeaturesAMD
instance Show PhysicalDeviceGpaFeaturesAMD

instance FromCStruct PhysicalDeviceGpaFeaturesAMD


data PhysicalDeviceGpaProperties2AMD

instance ToCStruct PhysicalDeviceGpaProperties2AMD
instance Show PhysicalDeviceGpaProperties2AMD

instance FromCStruct PhysicalDeviceGpaProperties2AMD


data PhysicalDeviceGpaPropertiesAMD

instance ToCStruct PhysicalDeviceGpaPropertiesAMD
instance Show PhysicalDeviceGpaPropertiesAMD

instance FromCStruct PhysicalDeviceGpaPropertiesAMD

