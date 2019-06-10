{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.ExtensionDiscovery
  ( ExtensionProperties(..)
#if defined(VK_USE_PLATFORM_GGP)
  , getNumDeviceExtensionProperties
  , enumerateDeviceExtensionProperties
  , enumerateAllDeviceExtensionProperties
  , getNumInstanceExtensionProperties
  , enumerateInstanceExtensionProperties
  , enumerateAllInstanceExtensionProperties
#endif
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.ByteString
  ( ByteString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.ByteString
  ( useAsCString
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import qualified Data.Vector
  ( generateM
  )
#endif
import Data.Word
  ( Word32
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Alloc
  ( alloca
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Array
  ( allocaArray
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( with
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Marshal.Utils
  ( maybeWith
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peekElemOff
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Storable
  ( peek
  )
#endif


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( vkEnumerateDeviceExtensionProperties
  , vkEnumerateInstanceExtensionProperties
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif



-- No documentation found for TopLevel "VkExtensionProperties"
data ExtensionProperties = ExtensionProperties
  { -- No documentation found for Nested "ExtensionProperties" "extensionName"
  extensionName :: ByteString
  , -- No documentation found for Nested "ExtensionProperties" "specVersion"
  specVersion :: Word32
  }
  deriving (Show, Eq)

instance Zero ExtensionProperties where
  zero = ExtensionProperties mempty
                             zero



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkEnumerateDeviceExtensionProperties"
getNumDeviceExtensionProperties :: PhysicalDevice ->  Maybe ByteString ->  IO (VkResult, Word32)
getNumDeviceExtensionProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkEnumerateDeviceExtensionProperties"
enumerateDeviceExtensionProperties :: PhysicalDevice ->  Maybe ByteString ->  Word32 ->  IO (VkResult, Vector ExtensionProperties)
enumerateDeviceExtensionProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'enumerateDeviceExtensionProperties'.
enumerateAllDeviceExtensionProperties :: PhysicalDevice ->  Maybe ByteString ->  IO (Vector ExtensionProperties)
enumerateAllDeviceExtensionProperties physicalDevice' pLayerName' =
  snd <$> getNumDeviceExtensionProperties physicalDevice' pLayerName'
    >>= \num -> snd <$> enumerateDeviceExtensionProperties physicalDevice' pLayerName' num

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkEnumerateInstanceExtensionProperties"
getNumInstanceExtensionProperties :: Maybe ByteString ->  IO (VkResult, Word32)
getNumInstanceExtensionProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkEnumerateInstanceExtensionProperties"
enumerateInstanceExtensionProperties :: Maybe ByteString ->  Word32 ->  IO (VkResult, Vector ExtensionProperties)
enumerateInstanceExtensionProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'enumerateInstanceExtensionProperties'.
enumerateAllInstanceExtensionProperties :: Maybe ByteString ->  IO (Vector ExtensionProperties)
enumerateAllInstanceExtensionProperties pLayerName' =
  snd <$> getNumInstanceExtensionProperties pLayerName'
    >>= \num -> snd <$> enumerateInstanceExtensionProperties pLayerName' num

#endif
