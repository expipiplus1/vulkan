{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language DuplicateRecordFields #-}
{-# language TypeOperators #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Graphics.Vulkan.C.Core10.LayerDiscovery
  ( VK_MAX_DESCRIPTION_SIZE
  , pattern VK_MAX_DESCRIPTION_SIZE
  , VkLayerProperties(..)
  , FN_vkEnumerateDeviceLayerProperties
  , PFN_vkEnumerateDeviceLayerProperties
  , vkEnumerateDeviceLayerProperties
  , FN_vkEnumerateInstanceLayerProperties
  , PFN_vkEnumerateInstanceLayerProperties
  , vkEnumerateInstanceLayerProperties
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
import Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VK_MAX_EXTENSION_NAME_SIZE
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VK_MAX_DESCRIPTION_SIZE"
type VK_MAX_DESCRIPTION_SIZE = 256
-- No documentation found for Nested "Integral a => a" "VK_MAX_DESCRIPTION_SIZE"
pattern VK_MAX_DESCRIPTION_SIZE :: Integral a => a
pattern VK_MAX_DESCRIPTION_SIZE = 256

-- No documentation found for TopLevel "VkLayerProperties"
data VkLayerProperties = VkLayerProperties
  { -- No documentation found for Nested "VkLayerProperties" "layerName"
  vkLayerName :: Vector VK_MAX_EXTENSION_NAME_SIZE CChar
  , -- No documentation found for Nested "VkLayerProperties" "specVersion"
  vkSpecVersion :: Word32
  , -- No documentation found for Nested "VkLayerProperties" "implementationVersion"
  vkImplementationVersion :: Word32
  , -- No documentation found for Nested "VkLayerProperties" "description"
  vkDescription :: Vector VK_MAX_DESCRIPTION_SIZE CChar
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

instance Zero VkLayerProperties where
  zero = VkLayerProperties zero
                           zero
                           zero
                           zero

-- No documentation found for TopLevel "vkEnumerateDeviceLayerProperties"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateDeviceLayerProperties" vkEnumerateDeviceLayerProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
#else
vkEnumerateDeviceLayerProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
vkEnumerateDeviceLayerProperties deviceCmds = mkVkEnumerateDeviceLayerProperties (pVkEnumerateDeviceLayerProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateDeviceLayerProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult)
#endif

type FN_vkEnumerateDeviceLayerProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
type PFN_vkEnumerateDeviceLayerProperties = FunPtr FN_vkEnumerateDeviceLayerProperties

-- No documentation found for TopLevel "vkEnumerateInstanceLayerProperties"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumerateInstanceLayerProperties" vkEnumerateInstanceLayerProperties :: ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
#else
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceLayerProperties
  :: FunPtr (("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult) -> (("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult)

vkEnumerateInstanceLayerProperties :: ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
vkEnumerateInstanceLayerProperties = mkVkEnumerateInstanceLayerProperties procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkEnumerateInstanceLayerProperties $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr' nullPtr (GHC.Ptr.Ptr "vkEnumerateInstanceLayerProperties\NUL"#)
#endif

type FN_vkEnumerateInstanceLayerProperties = ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
type PFN_vkEnumerateInstanceLayerProperties = FunPtr FN_vkEnumerateInstanceLayerProperties
