{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_driver_properties
  ( VkConformanceVersionKHR(..)
  , VkDriverIdKHR(..)
  , pattern VK_DRIVER_ID_AMD_PROPRIETARY_KHR
  , pattern VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR
  , pattern VK_DRIVER_ID_MESA_RADV_KHR
  , pattern VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR
  , pattern VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR
  , pattern VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR
  , pattern VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR
  , pattern VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR
  , pattern VK_DRIVER_ID_ARM_PROPRIETARY_KHR
  , pattern VK_DRIVER_ID_GOOGLE_PASTEL_KHR
  , pattern VK_DRIVER_ID_GGP_PROPRIETARY_KHR
  , VkPhysicalDeviceDriverPropertiesKHR(..)
  , pattern VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME
  , pattern VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION
  , VK_MAX_DRIVER_INFO_SIZE_KHR
  , pattern VK_MAX_DRIVER_INFO_SIZE_KHR
  , VK_MAX_DRIVER_NAME_SIZE_KHR
  , pattern VK_MAX_DRIVER_NAME_SIZE_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR
  ) where

import Data.Int
  ( Int32
  )
import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word8
  )
import Foreign.C.Types
  ( CChar(..)
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
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
data VkConformanceVersionKHR = VkConformanceVersionKHR
  { -- | @major@ is the major version number of the conformance test suite.
  vkMajor :: Word8
  , -- | @minor@ is the minor version number of the conformance test suite.
  vkMinor :: Word8
  , -- | @subminor@ is the subminor version number of the conformance test suite.
  vkSubminor :: Word8
  , -- | @patch@ is the patch version number of the conformance test suite.
  vkPatch :: Word8
  }
  deriving (Eq, Show)

instance Storable VkConformanceVersionKHR where
  sizeOf ~_ = 4
  alignment ~_ = 1
  peek ptr = VkConformanceVersionKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 1)
                                     <*> peek (ptr `plusPtr` 2)
                                     <*> peek (ptr `plusPtr` 3)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMajor (poked :: VkConformanceVersionKHR))
                *> poke (ptr `plusPtr` 1) (vkMinor (poked :: VkConformanceVersionKHR))
                *> poke (ptr `plusPtr` 2) (vkSubminor (poked :: VkConformanceVersionKHR))
                *> poke (ptr `plusPtr` 3) (vkPatch (poked :: VkConformanceVersionKHR))

instance Zero VkConformanceVersionKHR where
  zero = VkConformanceVersionKHR zero
                                 zero
                                 zero
                                 zero

-- ** VkDriverIdKHR

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
newtype VkDriverIdKHR = VkDriverIdKHR Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkDriverIdKHR where
  showsPrec _ VK_DRIVER_ID_AMD_PROPRIETARY_KHR = showString "VK_DRIVER_ID_AMD_PROPRIETARY_KHR"
  showsPrec _ VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR = showString "VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR"
  showsPrec _ VK_DRIVER_ID_MESA_RADV_KHR = showString "VK_DRIVER_ID_MESA_RADV_KHR"
  showsPrec _ VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR = showString "VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR"
  showsPrec _ VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR = showString "VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR"
  showsPrec _ VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR = showString "VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR"
  showsPrec _ VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR = showString "VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR"
  showsPrec _ VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR = showString "VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR"
  showsPrec _ VK_DRIVER_ID_ARM_PROPRIETARY_KHR = showString "VK_DRIVER_ID_ARM_PROPRIETARY_KHR"
  showsPrec _ VK_DRIVER_ID_GOOGLE_PASTEL_KHR = showString "VK_DRIVER_ID_GOOGLE_PASTEL_KHR"
  showsPrec _ VK_DRIVER_ID_GGP_PROPRIETARY_KHR = showString "VK_DRIVER_ID_GGP_PROPRIETARY_KHR"
  showsPrec p (VkDriverIdKHR x) = showParen (p >= 11) (showString "VkDriverIdKHR " . showsPrec 11 x)

instance Read VkDriverIdKHR where
  readPrec = parens ( choose [ ("VK_DRIVER_ID_AMD_PROPRIETARY_KHR",           pure VK_DRIVER_ID_AMD_PROPRIETARY_KHR)
                             , ("VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR",           pure VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR)
                             , ("VK_DRIVER_ID_MESA_RADV_KHR",                 pure VK_DRIVER_ID_MESA_RADV_KHR)
                             , ("VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR",        pure VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR)
                             , ("VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR", pure VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR)
                             , ("VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR",    pure VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR)
                             , ("VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR",   pure VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR)
                             , ("VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR",      pure VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR)
                             , ("VK_DRIVER_ID_ARM_PROPRIETARY_KHR",           pure VK_DRIVER_ID_ARM_PROPRIETARY_KHR)
                             , ("VK_DRIVER_ID_GOOGLE_PASTEL_KHR",             pure VK_DRIVER_ID_GOOGLE_PASTEL_KHR)
                             , ("VK_DRIVER_ID_GGP_PROPRIETARY_KHR",           pure VK_DRIVER_ID_GGP_PROPRIETARY_KHR)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDriverIdKHR")
                        v <- step readPrec
                        pure (VkDriverIdKHR v)
                        )
                    )

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_AMD_PROPRIETARY_KHR"
pattern VK_DRIVER_ID_AMD_PROPRIETARY_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_AMD_PROPRIETARY_KHR = VkDriverIdKHR 1

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR"
pattern VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_AMD_OPEN_SOURCE_KHR = VkDriverIdKHR 2

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_MESA_RADV_KHR"
pattern VK_DRIVER_ID_MESA_RADV_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_MESA_RADV_KHR = VkDriverIdKHR 3

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR"
pattern VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_NVIDIA_PROPRIETARY_KHR = VkDriverIdKHR 4

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR"
pattern VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_INTEL_PROPRIETARY_WINDOWS_KHR = VkDriverIdKHR 5

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR"
pattern VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_INTEL_OPEN_SOURCE_MESA_KHR = VkDriverIdKHR 6

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR"
pattern VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_IMAGINATION_PROPRIETARY_KHR = VkDriverIdKHR 7

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR"
pattern VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_QUALCOMM_PROPRIETARY_KHR = VkDriverIdKHR 8

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_ARM_PROPRIETARY_KHR"
pattern VK_DRIVER_ID_ARM_PROPRIETARY_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_ARM_PROPRIETARY_KHR = VkDriverIdKHR 9

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_GOOGLE_PASTEL_KHR"
pattern VK_DRIVER_ID_GOOGLE_PASTEL_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_GOOGLE_PASTEL_KHR = VkDriverIdKHR 10

-- No documentation found for Nested "VkDriverIdKHR" "VK_DRIVER_ID_GGP_PROPRIETARY_KHR"
pattern VK_DRIVER_ID_GGP_PROPRIETARY_KHR :: VkDriverIdKHR
pattern VK_DRIVER_ID_GGP_PROPRIETARY_KHR = VkDriverIdKHR 11

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
data VkPhysicalDeviceDriverPropertiesKHR = VkPhysicalDeviceDriverPropertiesKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension specific structure.
  vkPNext :: Ptr ()
  , -- | @driverID@ is a unique identifier for the driver of the physical device.
  vkDriverID :: VkDriverIdKHR
  , -- | @driverName@ is a null-terminated UTF-8 string containing the name of
  -- the driver.
  vkDriverName :: Vector VK_MAX_DRIVER_NAME_SIZE_KHR CChar
  , -- | @driverInfo@ is a null-terminated UTF-8 string containing additional
  -- information about the driver.
  vkDriverInfo :: Vector VK_MAX_DRIVER_INFO_SIZE_KHR CChar
  , -- | @conformanceVersion@ is the version of the Vulkan conformance test this
  -- driver is conformant against (see 'VkConformanceVersionKHR').
  vkConformanceVersion :: VkConformanceVersionKHR
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDriverPropertiesKHR where
  sizeOf ~_ = 536
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDriverPropertiesKHR <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
                                                 <*> peek (ptr `plusPtr` 20)
                                                 <*> peek (ptr `plusPtr` 276)
                                                 <*> peek (ptr `plusPtr` 532)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDriverPropertiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDriverPropertiesKHR))
                *> poke (ptr `plusPtr` 16) (vkDriverID (poked :: VkPhysicalDeviceDriverPropertiesKHR))
                *> poke (ptr `plusPtr` 20) (vkDriverName (poked :: VkPhysicalDeviceDriverPropertiesKHR))
                *> poke (ptr `plusPtr` 276) (vkDriverInfo (poked :: VkPhysicalDeviceDriverPropertiesKHR))
                *> poke (ptr `plusPtr` 532) (vkConformanceVersion (poked :: VkPhysicalDeviceDriverPropertiesKHR))

instance Zero VkPhysicalDeviceDriverPropertiesKHR where
  zero = VkPhysicalDeviceDriverPropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR
                                             zero
                                             zero
                                             zero
                                             zero
                                             zero

-- No documentation found for TopLevel "VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME"
pattern VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DRIVER_PROPERTIES_EXTENSION_NAME = "VK_KHR_driver_properties"

-- No documentation found for TopLevel "VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION"
pattern VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DRIVER_PROPERTIES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_MAX_DRIVER_INFO_SIZE_KHR"
type VK_MAX_DRIVER_INFO_SIZE_KHR = 256
-- No documentation found for Nested "Integral a => a" "VK_MAX_DRIVER_INFO_SIZE_KHR"
pattern VK_MAX_DRIVER_INFO_SIZE_KHR :: Integral a => a
pattern VK_MAX_DRIVER_INFO_SIZE_KHR = 256

-- No documentation found for TopLevel "VK_MAX_DRIVER_NAME_SIZE_KHR"
type VK_MAX_DRIVER_NAME_SIZE_KHR = 256
-- No documentation found for Nested "Integral a => a" "VK_MAX_DRIVER_NAME_SIZE_KHR"
pattern VK_MAX_DRIVER_NAME_SIZE_KHR :: Integral a => a
pattern VK_MAX_DRIVER_NAME_SIZE_KHR = 256

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR = VkStructureType 1000196000
