{-# language CPP #-}
-- | = Name
--
-- VK_KHR_driver_properties - device extension
--
-- == VK_KHR_driver_properties
--
-- [__Name String__]
--     @VK_KHR_driver_properties@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     197
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
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_driver_properties:%20&body=@drakos-amd%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-04-11
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Baldur Karlsson
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Piers Daniell, NVIDIA
--
--     -   Alexander Galazin, Arm
--
--     -   Jesse Hall, Google
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension provides a new physical device query which allows
-- retrieving information about the driver implementation, allowing
-- applications to determine which physical device corresponds to which
-- particular vendor’s driver, and which conformance test suite version the
-- driver implementation is compliant with.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   'ConformanceVersionKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDriverPropertiesKHR'
--
-- == New Enums
--
-- -   'DriverIdKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DRIVER_PROPERTIES_EXTENSION_NAME'
--
-- -   'KHR_DRIVER_PROPERTIES_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_DRIVER_INFO_SIZE_KHR'
--
-- -   'Vulkan.Core10.APIConstants.MAX_DRIVER_NAME_SIZE_KHR'
--
-- -   Extending 'Vulkan.Core12.Enums.DriverId.DriverId':
--
--     -   'DRIVER_ID_AMD_OPEN_SOURCE_KHR'
--
--     -   'DRIVER_ID_AMD_PROPRIETARY_KHR'
--
--     -   'DRIVER_ID_ARM_PROPRIETARY_KHR'
--
--     -   'DRIVER_ID_BROADCOM_PROPRIETARY_KHR'
--
--     -   'DRIVER_ID_GGP_PROPRIETARY_KHR'
--
--     -   'DRIVER_ID_GOOGLE_SWIFTSHADER_KHR'
--
--     -   'DRIVER_ID_IMAGINATION_PROPRIETARY_KHR'
--
--     -   'DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR'
--
--     -   'DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR'
--
--     -   'DRIVER_ID_MESA_RADV_KHR'
--
--     -   'DRIVER_ID_NVIDIA_PROPRIETARY_KHR'
--
--     -   'DRIVER_ID_QUALCOMM_PROPRIETARY_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2018-04-11 (Daniel Rakos)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'Vulkan.Core10.APIConstants.MAX_DRIVER_INFO_SIZE_KHR',
-- 'Vulkan.Core10.APIConstants.MAX_DRIVER_NAME_SIZE_KHR',
-- 'ConformanceVersionKHR', 'DriverIdKHR',
-- 'PhysicalDeviceDriverPropertiesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_driver_properties Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_driver_properties  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR
                                                   , pattern DRIVER_ID_AMD_PROPRIETARY_KHR
                                                   , pattern DRIVER_ID_AMD_OPEN_SOURCE_KHR
                                                   , pattern DRIVER_ID_MESA_RADV_KHR
                                                   , pattern DRIVER_ID_NVIDIA_PROPRIETARY_KHR
                                                   , pattern DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR
                                                   , pattern DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR
                                                   , pattern DRIVER_ID_IMAGINATION_PROPRIETARY_KHR
                                                   , pattern DRIVER_ID_QUALCOMM_PROPRIETARY_KHR
                                                   , pattern DRIVER_ID_ARM_PROPRIETARY_KHR
                                                   , pattern DRIVER_ID_GOOGLE_SWIFTSHADER_KHR
                                                   , pattern DRIVER_ID_GGP_PROPRIETARY_KHR
                                                   , pattern DRIVER_ID_BROADCOM_PROPRIETARY_KHR
                                                   , pattern MAX_DRIVER_NAME_SIZE_KHR
                                                   , pattern MAX_DRIVER_INFO_SIZE_KHR
                                                   , DriverIdKHR
                                                   , ConformanceVersionKHR
                                                   , PhysicalDeviceDriverPropertiesKHR
                                                   , KHR_DRIVER_PROPERTIES_SPEC_VERSION
                                                   , pattern KHR_DRIVER_PROPERTIES_SPEC_VERSION
                                                   , KHR_DRIVER_PROPERTIES_EXTENSION_NAME
                                                   , pattern KHR_DRIVER_PROPERTIES_EXTENSION_NAME
                                                   ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_driver_properties (ConformanceVersion)
import Vulkan.Core12.Enums.DriverId (DriverId)
import Vulkan.Core12.Promoted_From_VK_KHR_driver_properties (PhysicalDeviceDriverProperties)
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_AMD_OPEN_SOURCE))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_AMD_PROPRIETARY))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_ARM_PROPRIETARY))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_BROADCOM_PROPRIETARY))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_GGP_PROPRIETARY))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_GOOGLE_SWIFTSHADER))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_IMAGINATION_PROPRIETARY))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_INTEL_OPEN_SOURCE_MESA))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_INTEL_PROPRIETARY_WINDOWS))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_MESA_RADV))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_NVIDIA_PROPRIETARY))
import Vulkan.Core12.Enums.DriverId (DriverId(DRIVER_ID_QUALCOMM_PROPRIETARY))
import Vulkan.Core10.APIConstants (pattern MAX_DRIVER_INFO_SIZE)
import Vulkan.Core10.APIConstants (pattern MAX_DRIVER_NAME_SIZE)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES


-- No documentation found for TopLevel "VK_DRIVER_ID_AMD_PROPRIETARY_KHR"
pattern DRIVER_ID_AMD_PROPRIETARY_KHR = DRIVER_ID_AMD_PROPRIETARY


-- No documentation found for TopLevel "VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR"
pattern DRIVER_ID_AMD_OPEN_SOURCE_KHR = DRIVER_ID_AMD_OPEN_SOURCE


-- No documentation found for TopLevel "VK_DRIVER_ID_MESA_RADV_KHR"
pattern DRIVER_ID_MESA_RADV_KHR = DRIVER_ID_MESA_RADV


-- No documentation found for TopLevel "VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR"
pattern DRIVER_ID_NVIDIA_PROPRIETARY_KHR = DRIVER_ID_NVIDIA_PROPRIETARY


-- No documentation found for TopLevel "VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR"
pattern DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR = DRIVER_ID_INTEL_PROPRIETARY_WINDOWS


-- No documentation found for TopLevel "VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR"
pattern DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR = DRIVER_ID_INTEL_OPEN_SOURCE_MESA


-- No documentation found for TopLevel "VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR"
pattern DRIVER_ID_IMAGINATION_PROPRIETARY_KHR = DRIVER_ID_IMAGINATION_PROPRIETARY


-- No documentation found for TopLevel "VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR"
pattern DRIVER_ID_QUALCOMM_PROPRIETARY_KHR = DRIVER_ID_QUALCOMM_PROPRIETARY


-- No documentation found for TopLevel "VK_DRIVER_ID_ARM_PROPRIETARY_KHR"
pattern DRIVER_ID_ARM_PROPRIETARY_KHR = DRIVER_ID_ARM_PROPRIETARY


-- No documentation found for TopLevel "VK_DRIVER_ID_GOOGLE_SWIFTSHADER_KHR"
pattern DRIVER_ID_GOOGLE_SWIFTSHADER_KHR = DRIVER_ID_GOOGLE_SWIFTSHADER


-- No documentation found for TopLevel "VK_DRIVER_ID_GGP_PROPRIETARY_KHR"
pattern DRIVER_ID_GGP_PROPRIETARY_KHR = DRIVER_ID_GGP_PROPRIETARY


-- No documentation found for TopLevel "VK_DRIVER_ID_BROADCOM_PROPRIETARY_KHR"
pattern DRIVER_ID_BROADCOM_PROPRIETARY_KHR = DRIVER_ID_BROADCOM_PROPRIETARY


-- No documentation found for TopLevel "VK_MAX_DRIVER_NAME_SIZE_KHR"
pattern MAX_DRIVER_NAME_SIZE_KHR = MAX_DRIVER_NAME_SIZE


-- No documentation found for TopLevel "VK_MAX_DRIVER_INFO_SIZE_KHR"
pattern MAX_DRIVER_INFO_SIZE_KHR = MAX_DRIVER_INFO_SIZE


-- No documentation found for TopLevel "VkDriverIdKHR"
type DriverIdKHR = DriverId


-- No documentation found for TopLevel "VkConformanceVersionKHR"
type ConformanceVersionKHR = ConformanceVersion


-- No documentation found for TopLevel "VkPhysicalDeviceDriverPropertiesKHR"
type PhysicalDeviceDriverPropertiesKHR = PhysicalDeviceDriverProperties


type KHR_DRIVER_PROPERTIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION"
pattern KHR_DRIVER_PROPERTIES_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DRIVER_PROPERTIES_SPEC_VERSION = 1


type KHR_DRIVER_PROPERTIES_EXTENSION_NAME = "VK_KHR_driver_properties"

-- No documentation found for TopLevel "VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME"
pattern KHR_DRIVER_PROPERTIES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DRIVER_PROPERTIES_EXTENSION_NAME = "VK_KHR_driver_properties"

