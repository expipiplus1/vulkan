{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_KHR_driver_properties
  ( withCStructConformanceVersionKHR
  , fromCStructConformanceVersionKHR
  , ConformanceVersionKHR(..)
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
  , withCStructPhysicalDeviceDriverPropertiesKHR
  , fromCStructPhysicalDeviceDriverPropertiesKHR
  , PhysicalDeviceDriverPropertiesKHR(..)
  , pattern VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION
  , pattern VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR
  , pattern VK_MAX_DRIVER_NAME_SIZE_KHR
  , pattern VK_MAX_DRIVER_INFO_SIZE_KHR
  ) where

import Data.ByteString
  ( ByteString
  , packCString
  )
import qualified Data.ByteString
  ( empty
  )
import qualified Data.Vector.Storable
  ( unsafeWith
  )
import qualified Data.Vector.Storable.Sized
  ( fromSized
  )
import Data.Word
  ( Word8
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_driver_properties
  ( VkConformanceVersionKHR(..)
  , VkDriverIdKHR(..)
  , VkPhysicalDeviceDriverPropertiesKHR(..)
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
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR
  )
import Graphics.Vulkan.Marshal.Utils
  ( byteStringToNullTerminatedSizedVector
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_driver_properties
  ( pattern VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME
  , pattern VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION
  , pattern VK_MAX_DRIVER_INFO_SIZE_KHR
  , pattern VK_MAX_DRIVER_NAME_SIZE_KHR
  )



-- | VkConformanceVersionKHR - Structure containing the conformance test
-- suite version the implementation is compliant with
--
-- = Description
--
-- Unresolved directive in VkConformanceVersionKHR.txt -
-- include::{generated}\/validity\/structs\/VkConformanceVersionKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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

-- | A function to temporarily allocate memory for a 'VkConformanceVersionKHR' and
-- marshal a 'ConformanceVersionKHR' into it. The 'VkConformanceVersionKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructConformanceVersionKHR :: ConformanceVersionKHR -> (VkConformanceVersionKHR -> IO a) -> IO a
withCStructConformanceVersionKHR marshalled cont = cont (VkConformanceVersionKHR (major (marshalled :: ConformanceVersionKHR)) (minor (marshalled :: ConformanceVersionKHR)) (subminor (marshalled :: ConformanceVersionKHR)) (patch (marshalled :: ConformanceVersionKHR)))

-- | A function to read a 'VkConformanceVersionKHR' and all additional
-- structures in the pointer chain into a 'ConformanceVersionKHR'.
fromCStructConformanceVersionKHR :: VkConformanceVersionKHR -> IO ConformanceVersionKHR
fromCStructConformanceVersionKHR c = ConformanceVersionKHR <$> pure (vkMajor (c :: VkConformanceVersionKHR))
                                                           <*> pure (vkMinor (c :: VkConformanceVersionKHR))
                                                           <*> pure (vkSubminor (c :: VkConformanceVersionKHR))
                                                           <*> pure (vkPatch (c :: VkConformanceVersionKHR))

instance Zero ConformanceVersionKHR where
  zero = ConformanceVersionKHR zero
                               zero
                               zero
                               zero


-- | VkDriverIdKHR - Khronos driver IDs
--
-- = Description
--
-- __Note__
--
-- Khronos driver IDs may be allocated by vendors at any time. There may be
-- multiple driver IDs for the same vendor, representing different drivers
-- (for e.g. different platforms, proprietary or open source, etc.). Only
-- the latest canonical versions of this Specification, of the
-- corresponding @vk.xml@ API Registry, and of the corresponding
-- @vulkan_core.h@ header file /must/ contain all reserved Khronos driver
-- IDs.
--
-- Only driver IDs registered with Khronos are given symbolic names. There
-- /may/ be unregistered driver IDs returned.
--
-- = See Also
--
-- No cross-references are available
type DriverIdKHR = VkDriverIdKHR


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


-- | VkPhysicalDeviceDriverPropertiesKHR - Structure containing driver
-- identification information
--
-- = Description
--
-- @driverID@ /must/ be immutable for a given driver across instances,
-- processes, driver versions, and system reboots.
--
-- Unresolved directive in VkPhysicalDeviceDriverPropertiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkPhysicalDeviceDriverPropertiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data PhysicalDeviceDriverPropertiesKHR = PhysicalDeviceDriverPropertiesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceDriverPropertiesKHR' and
-- marshal a 'PhysicalDeviceDriverPropertiesKHR' into it. The 'VkPhysicalDeviceDriverPropertiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceDriverPropertiesKHR :: PhysicalDeviceDriverPropertiesKHR -> (VkPhysicalDeviceDriverPropertiesKHR -> IO a) -> IO a
withCStructPhysicalDeviceDriverPropertiesKHR marshalled cont = withCStructConformanceVersionKHR (conformanceVersion (marshalled :: PhysicalDeviceDriverPropertiesKHR)) (\conformanceVersion'' -> maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceDriverPropertiesKHR)) (\pPNext -> cont (VkPhysicalDeviceDriverPropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR pPNext (driverID (marshalled :: PhysicalDeviceDriverPropertiesKHR)) (byteStringToNullTerminatedSizedVector (driverName (marshalled :: PhysicalDeviceDriverPropertiesKHR))) (byteStringToNullTerminatedSizedVector (driverInfo (marshalled :: PhysicalDeviceDriverPropertiesKHR))) conformanceVersion'')))

-- | A function to read a 'VkPhysicalDeviceDriverPropertiesKHR' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceDriverPropertiesKHR'.
fromCStructPhysicalDeviceDriverPropertiesKHR :: VkPhysicalDeviceDriverPropertiesKHR -> IO PhysicalDeviceDriverPropertiesKHR
fromCStructPhysicalDeviceDriverPropertiesKHR c = PhysicalDeviceDriverPropertiesKHR <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceDriverPropertiesKHR)))
                                                                                   <*> pure (vkDriverID (c :: VkPhysicalDeviceDriverPropertiesKHR))
                                                                                   <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDriverName (c :: VkPhysicalDeviceDriverPropertiesKHR))) packCString
                                                                                   <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDriverInfo (c :: VkPhysicalDeviceDriverPropertiesKHR))) packCString
                                                                                   <*> (fromCStructConformanceVersionKHR (vkConformanceVersion (c :: VkPhysicalDeviceDriverPropertiesKHR)))

instance Zero PhysicalDeviceDriverPropertiesKHR where
  zero = PhysicalDeviceDriverPropertiesKHR Nothing
                                           zero
                                           Data.ByteString.empty
                                           Data.ByteString.empty
                                           zero

