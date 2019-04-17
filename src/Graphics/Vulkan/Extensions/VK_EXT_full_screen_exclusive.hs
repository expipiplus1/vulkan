{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive
  ( FullScreenExclusiveEXT
  , withCStructSurfaceCapabilitiesFullScreenExclusiveEXT
  , fromCStructSurfaceCapabilitiesFullScreenExclusiveEXT
  , SurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , withCStructSurfaceFullScreenExclusiveInfoEXT
  , fromCStructSurfaceFullScreenExclusiveInfoEXT
  , SurfaceFullScreenExclusiveInfoEXT(..)
  , withCStructSurfaceFullScreenExclusiveWin32InfoEXT
  , fromCStructSurfaceFullScreenExclusiveWin32InfoEXT
  , SurfaceFullScreenExclusiveWin32InfoEXT(..)
  , acquireFullScreenExclusiveModeEXT
  , getNumPhysicalDeviceSurfacePresentModes2EXT
  , getPhysicalDeviceSurfacePresentModes2EXT
  , getAllPhysicalDeviceSurfacePresentModes2EXT
  , releaseFullScreenExclusiveModeEXT
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
#if defined(VK_USE_PLATFORM_WIN32)
  , getDeviceGroupSurfacePresentModes2EXT
#endif
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
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
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( acquireFullScreenExclusiveModeEXT
  , getPhysicalDeviceSurfacePresentModes2EXT
  , releaseFullScreenExclusiveModeEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( VkFullScreenExclusiveEXT(..)
  , VkSurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , VkSurfaceFullScreenExclusiveInfoEXT(..)
  , VkSurfaceFullScreenExclusiveWin32InfoEXT(..)
  , HMONITOR
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , PhysicalDevice(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( PhysicalDeviceSurfaceInfo2KHR(..)
  , withCStructPhysicalDeviceSurfaceInfo2KHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( PresentModeKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_EXTENSION_NAME
  , pattern VK_EXT_FULL_SCREEN_EXCLUSIVE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_WIN32)
import Graphics.Vulkan.Extensions.VK_KHR_device_group
  ( getDeviceGroupSurfacePresentModes2EXT
  )
#endif


-- No documentation found for TopLevel "FullScreenExclusiveEXT"
type FullScreenExclusiveEXT = VkFullScreenExclusiveEXT
-- No documentation found for TopLevel "SurfaceCapabilitiesFullScreenExclusiveEXT"
data SurfaceCapabilitiesFullScreenExclusiveEXT = SurfaceCapabilitiesFullScreenExclusiveEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "SurfaceCapabilitiesFullScreenExclusiveEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceCapabilitiesFullScreenExclusiveEXT" "fullScreenExclusiveSupported"
  vkFullScreenExclusiveSupported :: Bool
  }
  deriving (Show, Eq)
withCStructSurfaceCapabilitiesFullScreenExclusiveEXT :: SurfaceCapabilitiesFullScreenExclusiveEXT -> (VkSurfaceCapabilitiesFullScreenExclusiveEXT -> IO a) -> IO a
withCStructSurfaceCapabilitiesFullScreenExclusiveEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: SurfaceCapabilitiesFullScreenExclusiveEXT)) (\pPNext -> cont (VkSurfaceCapabilitiesFullScreenExclusiveEXT VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT pPNext (boolToBool32 (vkFullScreenExclusiveSupported (from :: SurfaceCapabilitiesFullScreenExclusiveEXT)))))
fromCStructSurfaceCapabilitiesFullScreenExclusiveEXT :: VkSurfaceCapabilitiesFullScreenExclusiveEXT -> IO SurfaceCapabilitiesFullScreenExclusiveEXT
fromCStructSurfaceCapabilitiesFullScreenExclusiveEXT c = SurfaceCapabilitiesFullScreenExclusiveEXT <$> -- Univalued Member elided
                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceCapabilitiesFullScreenExclusiveEXT)))
                                                                                                   <*> pure (bool32ToBool (vkFullScreenExclusiveSupported (c :: VkSurfaceCapabilitiesFullScreenExclusiveEXT)))
instance Zero SurfaceCapabilitiesFullScreenExclusiveEXT where
  zero = SurfaceCapabilitiesFullScreenExclusiveEXT Nothing
                                                   False
-- No documentation found for TopLevel "SurfaceFullScreenExclusiveInfoEXT"
data SurfaceFullScreenExclusiveInfoEXT = SurfaceFullScreenExclusiveInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "SurfaceFullScreenExclusiveInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceFullScreenExclusiveInfoEXT" "fullScreenExclusive"
  vkFullScreenExclusive :: FullScreenExclusiveEXT
  }
  deriving (Show, Eq)
withCStructSurfaceFullScreenExclusiveInfoEXT :: SurfaceFullScreenExclusiveInfoEXT -> (VkSurfaceFullScreenExclusiveInfoEXT -> IO a) -> IO a
withCStructSurfaceFullScreenExclusiveInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: SurfaceFullScreenExclusiveInfoEXT)) (\pPNext -> cont (VkSurfaceFullScreenExclusiveInfoEXT VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT pPNext (vkFullScreenExclusive (from :: SurfaceFullScreenExclusiveInfoEXT))))
fromCStructSurfaceFullScreenExclusiveInfoEXT :: VkSurfaceFullScreenExclusiveInfoEXT -> IO SurfaceFullScreenExclusiveInfoEXT
fromCStructSurfaceFullScreenExclusiveInfoEXT c = SurfaceFullScreenExclusiveInfoEXT <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceFullScreenExclusiveInfoEXT)))
                                                                                   <*> pure (vkFullScreenExclusive (c :: VkSurfaceFullScreenExclusiveInfoEXT))
instance Zero SurfaceFullScreenExclusiveInfoEXT where
  zero = SurfaceFullScreenExclusiveInfoEXT Nothing
                                           zero
-- No documentation found for TopLevel "SurfaceFullScreenExclusiveWin32InfoEXT"
data SurfaceFullScreenExclusiveWin32InfoEXT = SurfaceFullScreenExclusiveWin32InfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "SurfaceFullScreenExclusiveWin32InfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceFullScreenExclusiveWin32InfoEXT" "hmonitor"
  vkHmonitor :: HMONITOR
  }
  deriving (Show, Eq)
withCStructSurfaceFullScreenExclusiveWin32InfoEXT :: SurfaceFullScreenExclusiveWin32InfoEXT -> (VkSurfaceFullScreenExclusiveWin32InfoEXT -> IO a) -> IO a
withCStructSurfaceFullScreenExclusiveWin32InfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: SurfaceFullScreenExclusiveWin32InfoEXT)) (\pPNext -> cont (VkSurfaceFullScreenExclusiveWin32InfoEXT VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT pPNext (vkHmonitor (from :: SurfaceFullScreenExclusiveWin32InfoEXT))))
fromCStructSurfaceFullScreenExclusiveWin32InfoEXT :: VkSurfaceFullScreenExclusiveWin32InfoEXT -> IO SurfaceFullScreenExclusiveWin32InfoEXT
fromCStructSurfaceFullScreenExclusiveWin32InfoEXT c = SurfaceFullScreenExclusiveWin32InfoEXT <$> -- Univalued Member elided
                                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceFullScreenExclusiveWin32InfoEXT)))
                                                                                             <*> pure (vkHmonitor (c :: VkSurfaceFullScreenExclusiveWin32InfoEXT))
instance Zero SurfaceFullScreenExclusiveWin32InfoEXT where
  zero = SurfaceFullScreenExclusiveWin32InfoEXT Nothing
                                                zero

-- | Wrapper for 'vkAcquireFullScreenExclusiveModeEXT'
acquireFullScreenExclusiveModeEXT :: Device ->  SwapchainKHR ->  IO ()
acquireFullScreenExclusiveModeEXT = \(Device device commandTable) -> \swapchain -> Graphics.Vulkan.C.Dynamic.acquireFullScreenExclusiveModeEXT commandTable device swapchain >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))

-- | Wrapper for 'vkGetPhysicalDeviceSurfacePresentModes2EXT'
getNumPhysicalDeviceSurfacePresentModes2EXT :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfacePresentModes2EXT = \(PhysicalDevice physicalDevice commandTable) -> \surfaceInfo -> alloca (\pPresentModeCount -> (\a -> withCStructPhysicalDeviceSurfaceInfo2KHR a . flip with) surfaceInfo (\pSurfaceInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfacePresentModes2EXT commandTable physicalDevice pSurfaceInfo pPresentModeCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPresentModeCount))))

-- | Wrapper for 'vkGetPhysicalDeviceSurfacePresentModes2EXT'
getPhysicalDeviceSurfacePresentModes2EXT :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  Word32 ->  IO (VkResult, Vector PresentModeKHR)
getPhysicalDeviceSurfacePresentModes2EXT = \(PhysicalDevice physicalDevice commandTable) -> \surfaceInfo -> \presentModeCount -> allocaArray (fromIntegral presentModeCount) (\pPresentModes -> with presentModeCount (\pPresentModeCount -> (\a -> withCStructPhysicalDeviceSurfaceInfo2KHR a . flip with) surfaceInfo (\pSurfaceInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfacePresentModes2EXT commandTable physicalDevice pSurfaceInfo pPresentModeCount pPresentModes >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM (peekElemOff pPresentModes) =<< (fromIntegral <$> (peek pPresentModeCount))))))))
-- | Call 'getNumPhysicalDeviceSurfacePresentModes2EXT' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceSurfacePresentModes2EXT' to get all the values.
getAllPhysicalDeviceSurfacePresentModes2EXT :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (Vector PresentModeKHR)
getAllPhysicalDeviceSurfacePresentModes2EXT physicalDevice pSurfaceInfo =
  snd <$> getNumPhysicalDeviceSurfacePresentModes2EXT physicalDevice pSurfaceInfo
    >>= \num -> snd <$> getPhysicalDeviceSurfacePresentModes2EXT physicalDevice pSurfaceInfo num


-- | Wrapper for 'vkReleaseFullScreenExclusiveModeEXT'
releaseFullScreenExclusiveModeEXT :: Device ->  SwapchainKHR ->  IO ()
releaseFullScreenExclusiveModeEXT = \(Device device commandTable) -> \swapchain -> Graphics.Vulkan.C.Dynamic.releaseFullScreenExclusiveModeEXT commandTable device swapchain >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))
