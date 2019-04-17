{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_driver_properties
  ( withCStructConformanceVersionKHR
  , fromCStructConformanceVersionKHR
  , ConformanceVersionKHR(..)
  , DriverIdKHR
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


-- No documentation found for TopLevel "ConformanceVersionKHR"
data ConformanceVersionKHR = ConformanceVersionKHR
  { -- No documentation found for Nested "ConformanceVersionKHR" "major"
  vkMajor :: Word8
  , -- No documentation found for Nested "ConformanceVersionKHR" "minor"
  vkMinor :: Word8
  , -- No documentation found for Nested "ConformanceVersionKHR" "subminor"
  vkSubminor :: Word8
  , -- No documentation found for Nested "ConformanceVersionKHR" "patch"
  vkPatch :: Word8
  }
  deriving (Show, Eq)
withCStructConformanceVersionKHR :: ConformanceVersionKHR -> (VkConformanceVersionKHR -> IO a) -> IO a
withCStructConformanceVersionKHR from cont = cont (VkConformanceVersionKHR (vkMajor (from :: ConformanceVersionKHR)) (vkMinor (from :: ConformanceVersionKHR)) (vkSubminor (from :: ConformanceVersionKHR)) (vkPatch (from :: ConformanceVersionKHR)))
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
-- No documentation found for TopLevel "DriverIdKHR"
type DriverIdKHR = VkDriverIdKHR
-- No documentation found for TopLevel "PhysicalDeviceDriverPropertiesKHR"
data PhysicalDeviceDriverPropertiesKHR = PhysicalDeviceDriverPropertiesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "driverID"
  vkDriverID :: DriverIdKHR
  , -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "driverName"
  vkDriverName :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "driverInfo"
  vkDriverInfo :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceDriverPropertiesKHR" "conformanceVersion"
  vkConformanceVersion :: ConformanceVersionKHR
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceDriverPropertiesKHR :: PhysicalDeviceDriverPropertiesKHR -> (VkPhysicalDeviceDriverPropertiesKHR -> IO a) -> IO a
withCStructPhysicalDeviceDriverPropertiesKHR from cont = withCStructConformanceVersionKHR (vkConformanceVersion (from :: PhysicalDeviceDriverPropertiesKHR)) (\conformanceVersion -> maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceDriverPropertiesKHR)) (\pPNext -> cont (VkPhysicalDeviceDriverPropertiesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR pPNext (vkDriverID (from :: PhysicalDeviceDriverPropertiesKHR)) (byteStringToNullTerminatedSizedVector (vkDriverName (from :: PhysicalDeviceDriverPropertiesKHR))) (byteStringToNullTerminatedSizedVector (vkDriverInfo (from :: PhysicalDeviceDriverPropertiesKHR))) conformanceVersion)))
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
