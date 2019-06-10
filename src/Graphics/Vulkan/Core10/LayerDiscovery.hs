{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.LayerDiscovery
  ( LayerProperties(..)
#if defined(VK_USE_PLATFORM_GGP)
  , getNumDeviceLayerProperties
  , enumerateDeviceLayerProperties
  , enumerateAllDeviceLayerProperties
  , getNumInstanceLayerProperties
  , enumerateInstanceLayerProperties
  , enumerateAllInstanceLayerProperties
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
import Graphics.Vulkan.C.Core10.LayerDiscovery
  ( vkEnumerateDeviceLayerProperties
  , vkEnumerateInstanceLayerProperties
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



-- No documentation found for TopLevel "VkLayerProperties"
data LayerProperties = LayerProperties
  { -- No documentation found for Nested "LayerProperties" "layerName"
  layerName :: ByteString
  , -- No documentation found for Nested "LayerProperties" "specVersion"
  specVersion :: Word32
  , -- No documentation found for Nested "LayerProperties" "implementationVersion"
  implementationVersion :: Word32
  , -- No documentation found for Nested "LayerProperties" "description"
  description :: ByteString
  }
  deriving (Show, Eq)

instance Zero LayerProperties where
  zero = LayerProperties mempty
                         zero
                         zero
                         mempty



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkEnumerateDeviceLayerProperties"
getNumDeviceLayerProperties :: PhysicalDevice ->  IO (VkResult, Word32)
getNumDeviceLayerProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkEnumerateDeviceLayerProperties"
enumerateDeviceLayerProperties :: PhysicalDevice ->  Word32 ->  IO (VkResult, Vector LayerProperties)
enumerateDeviceLayerProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'enumerateDeviceLayerProperties'.
enumerateAllDeviceLayerProperties :: PhysicalDevice ->  IO (Vector LayerProperties)
enumerateAllDeviceLayerProperties physicalDevice' =
  snd <$> getNumDeviceLayerProperties physicalDevice'
    >>= \num -> snd <$> enumerateDeviceLayerProperties physicalDevice' num

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkEnumerateInstanceLayerProperties"
getNumInstanceLayerProperties :: IO (VkResult, Word32)
getNumInstanceLayerProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkEnumerateInstanceLayerProperties"
enumerateInstanceLayerProperties :: Word32 ->  IO (VkResult, Vector LayerProperties)
enumerateInstanceLayerProperties = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'enumerateInstanceLayerProperties'.
enumerateAllInstanceLayerProperties :: IO (Vector LayerProperties)
enumerateAllInstanceLayerProperties  =
  snd <$> getNumInstanceLayerProperties 
    >>= \num -> snd <$> enumerateInstanceLayerProperties  num

#endif
