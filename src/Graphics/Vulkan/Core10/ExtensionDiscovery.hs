{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.ExtensionDiscovery
  ( VK_MAX_EXTENSION_NAME_SIZE
  , pattern VK_MAX_EXTENSION_NAME_SIZE
  , vkEnumerateInstanceExtensionProperties
  , vkEnumerateDeviceExtensionProperties
  , VkExtensionProperties(..)
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
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )


type VK_MAX_EXTENSION_NAME_SIZE = 256
pattern VK_MAX_EXTENSION_NAME_SIZE :: Integral a => a
pattern VK_MAX_EXTENSION_NAME_SIZE = 256
-- | 
foreign import ccall "vkEnumerateInstanceExtensionProperties" vkEnumerateInstanceExtensionProperties :: ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
-- | 
foreign import ccall "vkEnumerateDeviceExtensionProperties" vkEnumerateDeviceExtensionProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
-- | TODO: Struct comments
data VkExtensionProperties = VkExtensionProperties
  { vkExtensionName :: Vector VK_MAX_EXTENSION_NAME_SIZE CChar
  , vkSpecVersion :: Word32
  }
  deriving (Eq, Show)

instance Storable VkExtensionProperties where
  sizeOf ~_ = 260
  alignment ~_ = 4
  peek ptr = VkExtensionProperties <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 256)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkExtensionName (poked :: VkExtensionProperties))
                *> poke (ptr `plusPtr` 256) (vkSpecVersion (poked :: VkExtensionProperties))
