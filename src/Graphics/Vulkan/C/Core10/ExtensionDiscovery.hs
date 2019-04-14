{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VK_MAX_EXTENSION_NAME_SIZE
  , pattern VK_MAX_EXTENSION_NAME_SIZE
  , VkExtensionProperties(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkEnumerateDeviceExtensionProperties
#endif
  , FN_vkEnumerateDeviceExtensionProperties
  , PFN_vkEnumerateDeviceExtensionProperties
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkEnumerateInstanceExtensionProperties
#endif
  , FN_vkEnumerateInstanceExtensionProperties
  , PFN_vkEnumerateInstanceExtensionProperties
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
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
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
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkEnumerateDeviceExtensionProperties"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateDeviceExtensionProperties" vkEnumerateDeviceExtensionProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult

#endif
type FN_vkEnumerateDeviceExtensionProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
type PFN_vkEnumerateDeviceExtensionProperties = FunPtr FN_vkEnumerateDeviceExtensionProperties
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkEnumerateInstanceExtensionProperties"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateInstanceExtensionProperties" vkEnumerateInstanceExtensionProperties :: ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult

#endif
type FN_vkEnumerateInstanceExtensionProperties = ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
type PFN_vkEnumerateInstanceExtensionProperties = FunPtr FN_vkEnumerateInstanceExtensionProperties
