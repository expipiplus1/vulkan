{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_KHR_driver_properties
  ( ConformanceVersionKHR(..)
  , DriverIdKHR
  , pattern DRIVER_ID_AMD_PROPRIETARY_KHR
  , pattern DRIVER_ID_AMD_OPEN_SOURCE_KHR
  , pattern DRIVER_ID_MESA_RADV_KHR
  , pattern DRIVER_ID_NVIDIA_PROPRIETARY_KHR
  , pattern DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR
  , pattern DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR
  , pattern DRIVER_ID_IMAGINATION_PROPRIETARY_KHR
  , pattern DRIVER_ID_QUALCOMM_PROPRIETARY_KHR
  , pattern DRIVER_ID_ARM_PROPRIETARY_KHR
  , pattern DRIVER_ID_GOOGLE_PASTEL_KHR
  , pattern DRIVER_ID_GGP_PROPRIETARY_KHR
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceDriverPropertiesKHR(..)
#endif
  , pattern KHR_DRIVER_PROPERTIES_EXTENSION_NAME
  , pattern KHR_DRIVER_PROPERTIES_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Data.ByteString
  ( ByteString
  )
#endif
import Data.String
  ( IsString
  )
import Data.Word
  ( Word8
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_driver_properties
  ( VkDriverIdKHR(..)
  , pattern VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR
  , pattern VK_DRIVER_ID_AMD_PROPRIETARY_KHR
  , pattern VK_DRIVER_ID_ARM_PROPRIETARY_KHR
  , pattern VK_DRIVER_ID_GGP_PROPRIETARY_KHR
  , pattern VK_DRIVER_ID_GOOGLE_PASTEL_KHR
  , pattern VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR
  , pattern VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR
  , pattern VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR
  , pattern VK_DRIVER_ID_MESA_RADV_KHR
  , pattern VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR
  , pattern VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR
  , pattern VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME
  , pattern VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR
  )



-- No documentation found for TopLevel "VkConformanceVersionKHR"
data ConformanceVersionKHR = ConformanceVersionKHR
  { -- No documentation found for Nested "ConformanceVersionKHR" "major"
  major :: Word8
  , -- No documentation found for Nested "ConformanceVersionKHR" "minor"
  minor :: Word8
  , -- No documentation found for Nested "ConformanceVersionKHR" "subminor"
  subminor :: Word8
  , -- No documentation found for Nested "ConformanceVersionKHR" "patch"
  patch :: Word8
  }
  deriving (Show, Eq)

instance Zero ConformanceVersionKHR where
  zero = ConformanceVersionKHR zero
                               zero
                               zero
                               zero


-- No documentation found for TopLevel "DriverIdKHR"
type DriverIdKHR = VkDriverIdKHR


{-# complete DRIVER_ID_AMD_PROPRIETARY_KHR, DRIVER_ID_AMD_OPEN_SOURCE_KHR, DRIVER_ID_MESA_RADV_KHR, DRIVER_ID_NVIDIA_PROPRIETARY_KHR, DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR, DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR, DRIVER_ID_IMAGINATION_PROPRIETARY_KHR, DRIVER_ID_QUALCOMM_PROPRIETARY_KHR, DRIVER_ID_ARM_PROPRIETARY_KHR, DRIVER_ID_GOOGLE_PASTEL_KHR, DRIVER_ID_GGP_PROPRIETARY_KHR :: DriverIdKHR #-}


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_AMD_PROPRIETARY_KHR"
pattern DRIVER_ID_AMD_PROPRIETARY_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_AMD_PROPRIETARY_KHR = VK_DRIVER_ID_AMD_PROPRIETARY_KHR


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_AMD_OPEN_SOURCE_KHR"
pattern DRIVER_ID_AMD_OPEN_SOURCE_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_AMD_OPEN_SOURCE_KHR = VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_MESA_RADV_KHR"
pattern DRIVER_ID_MESA_RADV_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_MESA_RADV_KHR = VK_DRIVER_ID_MESA_RADV_KHR


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_NVIDIA_PROPRIETARY_KHR"
pattern DRIVER_ID_NVIDIA_PROPRIETARY_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_NVIDIA_PROPRIETARY_KHR = VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR"
pattern DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR = VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR"
pattern DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR = VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_IMAGINATION_PROPRIETARY_KHR"
pattern DRIVER_ID_IMAGINATION_PROPRIETARY_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_IMAGINATION_PROPRIETARY_KHR = VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_QUALCOMM_PROPRIETARY_KHR"
pattern DRIVER_ID_QUALCOMM_PROPRIETARY_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_QUALCOMM_PROPRIETARY_KHR = VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_ARM_PROPRIETARY_KHR"
pattern DRIVER_ID_ARM_PROPRIETARY_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_ARM_PROPRIETARY_KHR = VK_DRIVER_ID_ARM_PROPRIETARY_KHR


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_GOOGLE_PASTEL_KHR"
pattern DRIVER_ID_GOOGLE_PASTEL_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_GOOGLE_PASTEL_KHR = VK_DRIVER_ID_GOOGLE_PASTEL_KHR


-- No documentation found for Nested "DriverIdKHR" "DRIVER_ID_GGP_PROPRIETARY_KHR"
pattern DRIVER_ID_GGP_PROPRIETARY_KHR :: (a ~ DriverIdKHR) => a
pattern DRIVER_ID_GGP_PROPRIETARY_KHR = VK_DRIVER_ID_GGP_PROPRIETARY_KHR


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceDriverPropertiesKHR"
data PhysicalDeviceDriverPropertiesKHR = PhysicalDeviceDriverPropertiesKHR
  { -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "driverID"
  driverID :: DriverIdKHR
  , -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "driverName"
  driverName :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "driverInfo"
  driverInfo :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "conformanceVersion"
  conformanceVersion :: ConformanceVersionKHR
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceDriverPropertiesKHR where
  zero = PhysicalDeviceDriverPropertiesKHR Nothing
                                           zero
                                           mempty
                                           mempty
                                           zero

#endif

-- No documentation found for TopLevel "VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME"
pattern KHR_DRIVER_PROPERTIES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_DRIVER_PROPERTIES_EXTENSION_NAME = VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION"
pattern KHR_DRIVER_PROPERTIES_SPEC_VERSION :: Integral a => a
pattern KHR_DRIVER_PROPERTIES_SPEC_VERSION = VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION
