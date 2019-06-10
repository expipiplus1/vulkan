{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceSurfaceInfo2KHR(..)
  , 
  SurfaceCapabilities2KHR(..)
  , SurfaceFormat2KHR(..)
  , getPhysicalDeviceSurfaceCapabilities2KHR
  , getNumPhysicalDeviceSurfaceFormats2KHR
  , getPhysicalDeviceSurfaceFormats2KHR
  , getAllPhysicalDeviceSurfaceFormats2KHR
#endif
  , pattern KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  , pattern KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
  , pattern STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
  ) where


#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
import Data.String
  ( IsString
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

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

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



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  , pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( vkGetPhysicalDeviceSurfaceCapabilities2KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( vkGetPhysicalDeviceSurfaceFormats2KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceCapabilitiesKHR(..)
  , SurfaceFormatKHR(..)
  , SurfaceKHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( FromCStruct(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
  , pattern STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceSurfaceInfo2KHR"
data PhysicalDeviceSurfaceInfo2KHR = PhysicalDeviceSurfaceInfo2KHR
  { -- No documentation found for Nested "PhysicalDeviceSurfaceInfo2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSurfaceInfo2KHR" "surface"
  surface :: SurfaceKHR
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceSurfaceInfo2KHR where
  zero = PhysicalDeviceSurfaceInfo2KHR Nothing
                                       zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSurfaceCapabilities2KHR"
data SurfaceCapabilities2KHR = SurfaceCapabilities2KHR
  { -- No documentation found for Nested "SurfaceCapabilities2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceCapabilities2KHR" "surfaceCapabilities"
  surfaceCapabilities :: SurfaceCapabilitiesKHR
  }
  deriving (Show, Eq)

instance Zero SurfaceCapabilities2KHR where
  zero = SurfaceCapabilities2KHR Nothing
                                 zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSurfaceFormat2KHR"
data SurfaceFormat2KHR = SurfaceFormat2KHR
  { -- No documentation found for Nested "SurfaceFormat2KHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceFormat2KHR" "surfaceFormat"
  surfaceFormat :: SurfaceFormatKHR
  }
  deriving (Show, Eq)

instance Zero SurfaceFormat2KHR where
  zero = SurfaceFormat2KHR Nothing
                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceCapabilities2KHR"
getPhysicalDeviceSurfaceCapabilities2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (SurfaceCapabilities2KHR)
getPhysicalDeviceSurfaceCapabilities2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceFormats2KHR"
getNumPhysicalDeviceSurfaceFormats2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfaceFormats2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfaceFormats2KHR"
getPhysicalDeviceSurfaceFormats2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  Word32 ->  IO (VkResult, Vector SurfaceFormat2KHR)
getPhysicalDeviceSurfaceFormats2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceSurfaceFormats2KHR'.
getAllPhysicalDeviceSurfaceFormats2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (Vector SurfaceFormat2KHR)
getAllPhysicalDeviceSurfaceFormats2KHR physicalDevice' pSurfaceInfo' =
  snd <$> getNumPhysicalDeviceSurfaceFormats2KHR physicalDevice' pSurfaceInfo'
    >>= \num -> snd <$> getPhysicalDeviceSurfaceFormats2KHR physicalDevice' pSurfaceInfo' num

#endif

-- No documentation found for TopLevel "VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME"
pattern KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME = VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION"
pattern KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION :: Integral a => a
pattern KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION = VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
