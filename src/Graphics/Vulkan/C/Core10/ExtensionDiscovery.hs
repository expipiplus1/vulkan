{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language TypeOperators #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VK_MAX_EXTENSION_NAME_SIZE
  , pattern VK_MAX_EXTENSION_NAME_SIZE
  , VkExtensionProperties(..)
  , FN_vkEnumerateDeviceExtensionProperties
  , PFN_vkEnumerateDeviceExtensionProperties
  , vkEnumerateDeviceExtensionProperties
  , FN_vkEnumerateInstanceExtensionProperties
  , PFN_vkEnumerateInstanceExtensionProperties
  , vkEnumerateInstanceExtensionProperties
  ) where

import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CChar(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , castPtrToFunPtr
  , nullPtr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import qualified GHC.Ptr
  ( Ptr(Ptr)
  )
import System.IO.Unsafe
  ( unsafeDupablePerformIO
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  , vkGetInstanceProcAddr'
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VK_MAX_EXTENSION_NAME_SIZE"
type VK_MAX_EXTENSION_NAME_SIZE = 256
-- No documentation found for Nested "Integral a => a" "VK_MAX_EXTENSION_NAME_SIZE"
pattern VK_MAX_EXTENSION_NAME_SIZE :: Integral a => a
pattern VK_MAX_EXTENSION_NAME_SIZE = 256

-- No documentation found for TopLevel "VkExtensionProperties"
data VkExtensionProperties = VkExtensionProperties
  { -- No documentation found for Nested "VkExtensionProperties" "extensionName"
  vkExtensionName :: Vector VK_MAX_EXTENSION_NAME_SIZE CChar
  , -- No documentation found for Nested "VkExtensionProperties" "specVersion"
  vkSpecVersion :: Word32
  }
  deriving (Eq, Show)

instance Storable VkExtensionProperties where
  sizeOf ~_ = 260
  alignment ~_ = 4
  peek ptr = VkExtensionProperties <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 256)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkExtensionName (poked :: VkExtensionProperties))
                *> poke (ptr `plusPtr` 256) (vkSpecVersion (poked :: VkExtensionProperties))

instance Zero VkExtensionProperties where
  zero = VkExtensionProperties zero
                               zero

-- No documentation found for TopLevel "vkEnumerateDeviceExtensionProperties"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateDeviceExtensionProperties" vkEnumerateDeviceExtensionProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
#else
vkEnumerateDeviceExtensionProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
vkEnumerateDeviceExtensionProperties deviceCmds = mkVkEnumerateDeviceExtensionProperties (pVkEnumerateDeviceExtensionProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateDeviceExtensionProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult)
#endif

type FN_vkEnumerateDeviceExtensionProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
type PFN_vkEnumerateDeviceExtensionProperties = FunPtr FN_vkEnumerateDeviceExtensionProperties

-- No documentation found for TopLevel "vkEnumerateInstanceExtensionProperties"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateInstanceExtensionProperties" vkEnumerateInstanceExtensionProperties :: ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
#else
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceExtensionProperties
  :: FunPtr (("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult) -> (("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult)

vkEnumerateInstanceExtensionProperties :: ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
vkEnumerateInstanceExtensionProperties = mkVkEnumerateInstanceExtensionProperties procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkEnumerateInstanceExtensionProperties $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr' nullPtr (GHC.Ptr.Ptr "vkEnumerateInstanceExtensionProperties\NUL"#)
#endif

type FN_vkEnumerateInstanceExtensionProperties = ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
type PFN_vkEnumerateInstanceExtensionProperties = FunPtr FN_vkEnumerateInstanceExtensionProperties
