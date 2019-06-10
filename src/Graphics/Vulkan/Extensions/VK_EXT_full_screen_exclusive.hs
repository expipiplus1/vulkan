{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive
  ( FullScreenExclusiveEXT
  , pattern FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT
  , pattern FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT
  , pattern FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT
  , pattern FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT
#if defined(VK_USE_PLATFORM_GGP)
  , SurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , SurfaceFullScreenExclusiveInfoEXT(..)
  , SurfaceFullScreenExclusiveWin32InfoEXT(..)
#endif
  , acquireFullScreenExclusiveModeEXT
  , getNumPhysicalDeviceSurfacePresentModes2EXT
  , getPhysicalDeviceSurfacePresentModes2EXT
  , getAllPhysicalDeviceSurfacePresentModes2EXT
  , releaseFullScreenExclusiveModeEXT
  , pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  , pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
#if defined(VK_USE_PLATFORM_WIN32)
  , getDeviceGroupSurfacePresentModes2EXT
#endif
  , pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
  ) where

import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( with
  )
import Foreign.Ptr
  ( nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( VkFullScreenExclusiveEXT(..)
  , vkAcquireFullScreenExclusiveModeEXT
  , vkGetPhysicalDeviceSurfacePresentModes2EXT
  , vkReleaseFullScreenExclusiveModeEXT
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
  , pattern VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT
  , pattern VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( HMONITOR
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , PhysicalDevice(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( PhysicalDeviceSurfaceInfo2KHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( PresentModeKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
  )

#if defined(VK_USE_PLATFORM_WIN32)
import Graphics.Vulkan.Extensions.VK_KHR_device_group
  ( getDeviceGroupSurfacePresentModes2EXT
  )
#endif


-- No documentation found for TopLevel "FullScreenExclusiveEXT"
type FullScreenExclusiveEXT = VkFullScreenExclusiveEXT


{-# complete FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT, FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT, FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT, FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT :: FullScreenExclusiveEXT #-}


-- No documentation found for Nested "FullScreenExclusiveEXT" "FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT"
pattern FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT :: (a ~ FullScreenExclusiveEXT) => a
pattern FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT = VK_FULL_SCREEN_EXCLUSIVE_DEFAULT_EXT


-- No documentation found for Nested "FullScreenExclusiveEXT" "FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT"
pattern FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT :: (a ~ FullScreenExclusiveEXT) => a
pattern FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT = VK_FULL_SCREEN_EXCLUSIVE_ALLOWED_EXT


-- No documentation found for Nested "FullScreenExclusiveEXT" "FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT"
pattern FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT :: (a ~ FullScreenExclusiveEXT) => a
pattern FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT = VK_FULL_SCREEN_EXCLUSIVE_DISALLOWED_EXT


-- No documentation found for Nested "FullScreenExclusiveEXT" "FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT"
pattern FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT :: (a ~ FullScreenExclusiveEXT) => a
pattern FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT = VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSurfaceCapabilitiesFullScreenExclusiveEXT"
data SurfaceCapabilitiesFullScreenExclusiveEXT = SurfaceCapabilitiesFullScreenExclusiveEXT
  { -- No documentation found for Nested "SurfaceCapabilitiesFullScreenExclusiveEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceCapabilitiesFullScreenExclusiveEXT" "fullScreenExclusiveSupported"
  fullScreenExclusiveSupported :: Bool
  }
  deriving (Show, Eq)

instance Zero SurfaceCapabilitiesFullScreenExclusiveEXT where
  zero = SurfaceCapabilitiesFullScreenExclusiveEXT Nothing
                                                   False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSurfaceFullScreenExclusiveInfoEXT"
data SurfaceFullScreenExclusiveInfoEXT = SurfaceFullScreenExclusiveInfoEXT
  { -- No documentation found for Nested "SurfaceFullScreenExclusiveInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceFullScreenExclusiveInfoEXT" "fullScreenExclusive"
  fullScreenExclusive :: FullScreenExclusiveEXT
  }
  deriving (Show, Eq)

instance Zero SurfaceFullScreenExclusiveInfoEXT where
  zero = SurfaceFullScreenExclusiveInfoEXT Nothing
                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSurfaceFullScreenExclusiveWin32InfoEXT"
data SurfaceFullScreenExclusiveWin32InfoEXT = SurfaceFullScreenExclusiveWin32InfoEXT
  { -- No documentation found for Nested "SurfaceFullScreenExclusiveWin32InfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceFullScreenExclusiveWin32InfoEXT" "hmonitor"
  hmonitor :: HMONITOR
  }
  deriving (Show, Eq)

instance Zero SurfaceFullScreenExclusiveWin32InfoEXT where
  zero = SurfaceFullScreenExclusiveWin32InfoEXT Nothing
                                                zero

#endif


-- No documentation found for TopLevel "vkAcquireFullScreenExclusiveModeEXT"
acquireFullScreenExclusiveModeEXT :: Device ->  SwapchainKHR ->  IO ()
acquireFullScreenExclusiveModeEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfacePresentModes2EXT"
getNumPhysicalDeviceSurfacePresentModes2EXT :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfacePresentModes2EXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDeviceSurfacePresentModes2EXT"
getPhysicalDeviceSurfacePresentModes2EXT :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  Word32 ->  IO (VkResult, Vector PresentModeKHR)
getPhysicalDeviceSurfacePresentModes2EXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDeviceSurfacePresentModes2EXT'.
getAllPhysicalDeviceSurfacePresentModes2EXT :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (Vector PresentModeKHR)
getAllPhysicalDeviceSurfacePresentModes2EXT physicalDevice' pSurfaceInfo' =
  snd <$> getNumPhysicalDeviceSurfacePresentModes2EXT physicalDevice' pSurfaceInfo'
    >>= \num -> snd <$> getPhysicalDeviceSurfacePresentModes2EXT physicalDevice' pSurfaceInfo' num



-- No documentation found for TopLevel "vkReleaseFullScreenExclusiveModeEXT"
releaseFullScreenExclusiveModeEXT :: Device ->  SwapchainKHR ->  IO ()
releaseFullScreenExclusiveModeEXT = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME"
pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME = VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION"
pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION :: Integral a => a
pattern EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION = VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
