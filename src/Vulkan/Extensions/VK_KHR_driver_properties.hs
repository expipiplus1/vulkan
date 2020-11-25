{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_driver_properties"
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

