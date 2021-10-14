{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance4 - device extension
--
-- == VK_KHR_maintenance4
--
-- [__Name String__]
--     @VK_KHR_maintenance4@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     414
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance4] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_KHR_maintenance4 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-08-18
--
-- [__Interactions and External Dependencies__]
--
--     -   Requires SPIR-V 1.2 for @LocalSizeId@
--
-- [__Contributors__]
--
--     -   Lionel Duc, NVIDIA
--
--     -   Jason Ekstrand, Intel
--
--     -   Spencer Fricke, Samsung
--
--     -   Tobias Hector, AMD
--
--     -   Lionel Landwerlin, Intel
--
--     -   Graeme Leese, Broadcom
--
--     -   Tom Olson, Arm
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- @VK_KHR_maintenance4@ adds a collection of minor features, none of which
-- would warrant an entire extension of their own.
--
-- The new features are as follows:
--
-- -   Allow the application to destroy their
--     'Vulkan.Core10.Handles.PipelineLayout' object immediately after it
--     was used to create another object. It is no longer necessary to keep
--     its handle valid while the created object is in use.
--
-- -   Add a new
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxBufferSize maxBufferSize>
--     implementation-defined limit for the maximum size
--     'Vulkan.Core10.Handles.Buffer' that /can/ be created.
--
-- -   Add support for the SPIR-V 1.2 @LocalSizeId@ execution mode, which
--     can be used as an alternative to @LocalSize@ to specify the local
--     workgroup size with specialization constants.
--
-- -   Add a guarantee that images created with identical creation
--     parameters will always have the same alignment requirements.
--
-- -   Add new 'getDeviceBufferMemoryRequirementsKHR',
--     'getDeviceImageMemoryRequirementsKHR', and
--     'getDeviceImageSparseMemoryRequirementsKHR' to allow the application
--     to query the image memory requirements without having to create an
--     image object and query it.
--
-- -   Relax the requirement that push constants must be initialized before
--     they are dynamically accessed.
--
-- -   Relax the interface matching rules to allow a larger output vector
--     to match with a smaller input vector, with additional values being
--     discarded.
--
-- == New Commands
--
-- -   'getDeviceBufferMemoryRequirementsKHR'
--
-- -   'getDeviceImageMemoryRequirementsKHR'
--
-- -   'getDeviceImageSparseMemoryRequirementsKHR'
--
-- == New Structures
--
-- -   'DeviceBufferMemoryRequirementsKHR'
--
-- -   'DeviceImageMemoryRequirementsKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance4FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMaintenance4PropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_4_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_4_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2021-08-18 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'DeviceBufferMemoryRequirementsKHR', 'DeviceImageMemoryRequirementsKHR',
-- 'PhysicalDeviceMaintenance4FeaturesKHR',
-- 'PhysicalDeviceMaintenance4PropertiesKHR',
-- 'getDeviceBufferMemoryRequirementsKHR',
-- 'getDeviceImageMemoryRequirementsKHR',
-- 'getDeviceImageSparseMemoryRequirementsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance4  ( DeviceBufferMemoryRequirementsKHR
                                              , DeviceImageMemoryRequirementsKHR
                                              , PhysicalDeviceMaintenance4FeaturesKHR
                                              , PhysicalDeviceMaintenance4PropertiesKHR
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceBufferMemoryRequirementsKHR

instance ToCStruct DeviceBufferMemoryRequirementsKHR
instance Show DeviceBufferMemoryRequirementsKHR

instance FromCStruct DeviceBufferMemoryRequirementsKHR


data DeviceImageMemoryRequirementsKHR

instance ToCStruct DeviceImageMemoryRequirementsKHR
instance Show DeviceImageMemoryRequirementsKHR

instance FromCStruct DeviceImageMemoryRequirementsKHR


data PhysicalDeviceMaintenance4FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance4FeaturesKHR
instance Show PhysicalDeviceMaintenance4FeaturesKHR

instance FromCStruct PhysicalDeviceMaintenance4FeaturesKHR


data PhysicalDeviceMaintenance4PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance4PropertiesKHR
instance Show PhysicalDeviceMaintenance4PropertiesKHR

instance FromCStruct PhysicalDeviceMaintenance4PropertiesKHR

