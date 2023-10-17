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
--     2
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance4] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_KHR_maintenance4 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-10-25
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
--     -   Requires SPIR-V 1.2 for @LocalSizeId@
--
-- [__Contributors__]
--
--     -   Lionel Duc, NVIDIA
--
--     -   Faith Ekstrand, Intel
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
--     -   Yiwei Zhang, Google
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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxBufferSize maxBufferSize>
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
-- -   Add a guarantee for buffer memory requirement that the size memory
--     requirement is never greater than the result of aligning create size
--     with the alignment memory requirement.
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
-- -   Extending
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.ImageAspectFlagBits':
--
--     -   'IMAGE_ASPECT_NONE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS_KHR'
--
--     -   'STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES_KHR'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- KHR suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
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
-- -   Revision 2, 2021-10-25 (Yiwei Zhang)
--
--     -   More guarantees on buffer memory requirements
--
-- == See Also
--
-- 'DeviceBufferMemoryRequirementsKHR', 'DeviceImageMemoryRequirementsKHR',
-- 'PhysicalDeviceMaintenance4FeaturesKHR',
-- 'PhysicalDeviceMaintenance4PropertiesKHR',
-- 'getDeviceBufferMemoryRequirementsKHR',
-- 'getDeviceImageMemoryRequirementsKHR',
-- 'getDeviceImageSparseMemoryRequirementsKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_maintenance4 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance4  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES_KHR
                                              , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES_KHR
                                              , pattern STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS_KHR
                                              , pattern STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS_KHR
                                              , pattern IMAGE_ASPECT_NONE_KHR
                                              , getDeviceBufferMemoryRequirementsKHR
                                              , getDeviceImageMemoryRequirementsKHR
                                              , getDeviceImageSparseMemoryRequirementsKHR
                                              , DeviceBufferMemoryRequirementsKHR
                                              , DeviceImageMemoryRequirementsKHR
                                              , PhysicalDeviceMaintenance4FeaturesKHR
                                              , PhysicalDeviceMaintenance4PropertiesKHR
                                              , KHR_MAINTENANCE_4_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE_4_SPEC_VERSION
                                              , KHR_MAINTENANCE_4_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE_4_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (getDeviceBufferMemoryRequirements)
import Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (getDeviceImageMemoryRequirements)
import Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (getDeviceImageSparseMemoryRequirements)
import Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (DeviceBufferMemoryRequirements)
import Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (DeviceImageMemoryRequirements)
import Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (PhysicalDeviceMaintenance4Features)
import Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (PhysicalDeviceMaintenance4Properties)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlags)
import Vulkan.Core10.Enums.ImageAspectFlagBits (ImageAspectFlagBits(IMAGE_ASPECT_NONE))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_4_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS_KHR"
pattern STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS_KHR = STRUCTURE_TYPE_DEVICE_BUFFER_MEMORY_REQUIREMENTS


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS_KHR"
pattern STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS_KHR = STRUCTURE_TYPE_DEVICE_IMAGE_MEMORY_REQUIREMENTS


-- No documentation found for TopLevel "VK_IMAGE_ASPECT_NONE_KHR"
pattern IMAGE_ASPECT_NONE_KHR = IMAGE_ASPECT_NONE


-- No documentation found for TopLevel "vkGetDeviceBufferMemoryRequirementsKHR"
getDeviceBufferMemoryRequirementsKHR = getDeviceBufferMemoryRequirements


-- No documentation found for TopLevel "vkGetDeviceImageMemoryRequirementsKHR"
getDeviceImageMemoryRequirementsKHR = getDeviceImageMemoryRequirements


-- No documentation found for TopLevel "vkGetDeviceImageSparseMemoryRequirementsKHR"
getDeviceImageSparseMemoryRequirementsKHR = getDeviceImageSparseMemoryRequirements


-- No documentation found for TopLevel "VkDeviceBufferMemoryRequirementsKHR"
type DeviceBufferMemoryRequirementsKHR = DeviceBufferMemoryRequirements


-- No documentation found for TopLevel "VkDeviceImageMemoryRequirementsKHR"
type DeviceImageMemoryRequirementsKHR = DeviceImageMemoryRequirements


-- No documentation found for TopLevel "VkPhysicalDeviceMaintenance4FeaturesKHR"
type PhysicalDeviceMaintenance4FeaturesKHR = PhysicalDeviceMaintenance4Features


-- No documentation found for TopLevel "VkPhysicalDeviceMaintenance4PropertiesKHR"
type PhysicalDeviceMaintenance4PropertiesKHR = PhysicalDeviceMaintenance4Properties


type KHR_MAINTENANCE_4_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_4_SPEC_VERSION"
pattern KHR_MAINTENANCE_4_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_4_SPEC_VERSION = 2


type KHR_MAINTENANCE_4_EXTENSION_NAME = "VK_KHR_maintenance4"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_4_EXTENSION_NAME"
pattern KHR_MAINTENANCE_4_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_4_EXTENSION_NAME = "VK_KHR_maintenance4"

