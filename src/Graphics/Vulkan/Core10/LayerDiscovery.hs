{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.LayerDiscovery
  ( VK_MAX_DESCRIPTION_SIZE
  , pattern VK_MAX_DESCRIPTION_SIZE
  , vkEnumerateInstanceLayerProperties
  , vkEnumerateDeviceLayerProperties
  , VkLayerProperties(..)
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
import Graphics.Vulkan.Core10.ExtensionDiscovery
  ( VK_MAX_EXTENSION_NAME_SIZE
  )


type VK_MAX_DESCRIPTION_SIZE = 256
pattern VK_MAX_DESCRIPTION_SIZE :: Integral a => a
pattern VK_MAX_DESCRIPTION_SIZE = 256
-- | 
foreign import ccall "vkEnumerateInstanceLayerProperties" vkEnumerateInstanceLayerProperties :: ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
-- | 
foreign import ccall "vkEnumerateDeviceLayerProperties" vkEnumerateDeviceLayerProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
-- | TODO: Struct comments
data VkLayerProperties = VkLayerProperties
  { vkLayerName :: Vector VK_MAX_EXTENSION_NAME_SIZE CChar
  , vkSpecVersion :: Word32
  , vkImplementationVersion :: Word32
  , vkDescription :: Vector VK_MAX_DESCRIPTION_SIZE CChar
  }
  deriving (Eq, Show)

instance Storable VkLayerProperties where
  sizeOf ~_ = 520
  alignment ~_ = 4
  peek ptr = VkLayerProperties <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 256)
                               <*> peek (ptr `plusPtr` 260)
                               <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLayerName (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 256) (vkSpecVersion (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 260) (vkImplementationVersion (poked :: VkLayerProperties))
                *> poke (ptr `plusPtr` 264) (vkDescription (poked :: VkLayerProperties))
