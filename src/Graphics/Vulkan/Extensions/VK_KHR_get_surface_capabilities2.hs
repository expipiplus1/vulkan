{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( withCStructPhysicalDeviceSurfaceInfo2KHR
  , fromCStructPhysicalDeviceSurfaceInfo2KHR
  , PhysicalDeviceSurfaceInfo2KHR(..)
  , withCStructSurfaceCapabilities2KHR
  , fromCStructSurfaceCapabilities2KHR
  , SurfaceCapabilities2KHR(..)
  , withCStructSurfaceFormat2KHR
  , fromCStructSurfaceFormat2KHR
  , SurfaceFormat2KHR(..)
  , getPhysicalDeviceSurfaceCapabilities2KHR
  , getNumPhysicalDeviceSurfaceFormats2KHR
  , getPhysicalDeviceSurfaceFormats2KHR
  , getAllPhysicalDeviceSurfaceFormats2KHR
  , pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
  , pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
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
  ( getPhysicalDeviceSurfaceCapabilities2KHR
  , getPhysicalDeviceSurfaceFormats2KHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  , VkSurfaceCapabilities2KHR(..)
  , VkSurfaceFormat2KHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceCapabilitiesKHR(..)
  , SurfaceFormatKHR(..)
  , SurfaceKHR
  , fromCStructSurfaceCapabilitiesKHR
  , fromCStructSurfaceFormatKHR
  , withCStructSurfaceCapabilitiesKHR
  , withCStructSurfaceFormatKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME
  , pattern VK_KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceSurfaceInfo2KHR"
data PhysicalDeviceSurfaceInfo2KHR = PhysicalDeviceSurfaceInfo2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceSurfaceInfo2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceSurfaceInfo2KHR" "surface"
  vkSurface :: SurfaceKHR
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceSurfaceInfo2KHR :: PhysicalDeviceSurfaceInfo2KHR -> (VkPhysicalDeviceSurfaceInfo2KHR -> IO a) -> IO a
withCStructPhysicalDeviceSurfaceInfo2KHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceSurfaceInfo2KHR)) (\pPNext -> cont (VkPhysicalDeviceSurfaceInfo2KHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR pPNext (vkSurface (from :: PhysicalDeviceSurfaceInfo2KHR))))
fromCStructPhysicalDeviceSurfaceInfo2KHR :: VkPhysicalDeviceSurfaceInfo2KHR -> IO PhysicalDeviceSurfaceInfo2KHR
fromCStructPhysicalDeviceSurfaceInfo2KHR c = PhysicalDeviceSurfaceInfo2KHR <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceSurfaceInfo2KHR)))
                                                                           <*> pure (vkSurface (c :: VkPhysicalDeviceSurfaceInfo2KHR))
-- No documentation found for TopLevel "SurfaceCapabilities2KHR"
data SurfaceCapabilities2KHR = SurfaceCapabilities2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SurfaceCapabilities2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceCapabilities2KHR" "surfaceCapabilities"
  vkSurfaceCapabilities :: SurfaceCapabilitiesKHR
  }
  deriving (Show, Eq)
withCStructSurfaceCapabilities2KHR :: SurfaceCapabilities2KHR -> (VkSurfaceCapabilities2KHR -> IO a) -> IO a
withCStructSurfaceCapabilities2KHR from cont = withCStructSurfaceCapabilitiesKHR (vkSurfaceCapabilities (from :: SurfaceCapabilities2KHR)) (\surfaceCapabilities -> maybeWith withSomeVkStruct (vkPNext (from :: SurfaceCapabilities2KHR)) (\pPNext -> cont (VkSurfaceCapabilities2KHR VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR pPNext surfaceCapabilities)))
fromCStructSurfaceCapabilities2KHR :: VkSurfaceCapabilities2KHR -> IO SurfaceCapabilities2KHR
fromCStructSurfaceCapabilities2KHR c = SurfaceCapabilities2KHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceCapabilities2KHR)))
                                                               <*> (fromCStructSurfaceCapabilitiesKHR (vkSurfaceCapabilities (c :: VkSurfaceCapabilities2KHR)))
-- No documentation found for TopLevel "SurfaceFormat2KHR"
data SurfaceFormat2KHR = SurfaceFormat2KHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SurfaceFormat2KHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SurfaceFormat2KHR" "surfaceFormat"
  vkSurfaceFormat :: SurfaceFormatKHR
  }
  deriving (Show, Eq)
withCStructSurfaceFormat2KHR :: SurfaceFormat2KHR -> (VkSurfaceFormat2KHR -> IO a) -> IO a
withCStructSurfaceFormat2KHR from cont = withCStructSurfaceFormatKHR (vkSurfaceFormat (from :: SurfaceFormat2KHR)) (\surfaceFormat -> maybeWith withSomeVkStruct (vkPNext (from :: SurfaceFormat2KHR)) (\pPNext -> cont (VkSurfaceFormat2KHR VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR pPNext surfaceFormat)))
fromCStructSurfaceFormat2KHR :: VkSurfaceFormat2KHR -> IO SurfaceFormat2KHR
fromCStructSurfaceFormat2KHR c = SurfaceFormat2KHR <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSurfaceFormat2KHR)))
                                                   <*> (fromCStructSurfaceFormatKHR (vkSurfaceFormat (c :: VkSurfaceFormat2KHR)))

-- | Wrapper for 'vkGetPhysicalDeviceSurfaceCapabilities2KHR'
getPhysicalDeviceSurfaceCapabilities2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO ( SurfaceCapabilities2KHR )
getPhysicalDeviceSurfaceCapabilities2KHR = \(PhysicalDevice physicalDevice commandTable) -> \surfaceInfo -> alloca (\pSurfaceCapabilities -> (\a -> withCStructPhysicalDeviceSurfaceInfo2KHR a . flip with) surfaceInfo (\pSurfaceInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfaceCapabilities2KHR commandTable physicalDevice pSurfaceInfo pSurfaceCapabilities >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructSurfaceCapabilities2KHR <=< peek) pSurfaceCapabilities))))

-- | Wrapper for 'vkGetPhysicalDeviceSurfaceFormats2KHR'
getNumPhysicalDeviceSurfaceFormats2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfaceFormats2KHR = \(PhysicalDevice physicalDevice commandTable) -> \surfaceInfo -> alloca (\pSurfaceFormatCount -> (\a -> withCStructPhysicalDeviceSurfaceInfo2KHR a . flip with) surfaceInfo (\pSurfaceInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfaceFormats2KHR commandTable physicalDevice pSurfaceInfo pSurfaceFormatCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pSurfaceFormatCount))))

-- | Wrapper for 'vkGetPhysicalDeviceSurfaceFormats2KHR'
getPhysicalDeviceSurfaceFormats2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  Word32 ->  IO ( VkResult
, Vector SurfaceFormat2KHR )
getPhysicalDeviceSurfaceFormats2KHR = \(PhysicalDevice physicalDevice commandTable) -> \surfaceInfo -> \surfaceFormatCount -> allocaArray (fromIntegral surfaceFormatCount) (\pSurfaceFormats -> with surfaceFormatCount (\pSurfaceFormatCount -> (\a -> withCStructPhysicalDeviceSurfaceInfo2KHR a . flip with) surfaceInfo (\pSurfaceInfo -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfaceFormats2KHR commandTable physicalDevice pSurfaceInfo pSurfaceFormatCount pSurfaceFormats >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructSurfaceFormat2KHR <=< peekElemOff p) pSurfaceFormats) =<< (fromIntegral <$> (peek pSurfaceFormatCount))))))))
-- | Call 'getNumPhysicalDeviceSurfaceFormats2KHR' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceSurfaceFormats2KHR' to get all the values.
getAllPhysicalDeviceSurfaceFormats2KHR :: PhysicalDevice ->  PhysicalDeviceSurfaceInfo2KHR ->  IO (Vector SurfaceFormat2KHR)
getAllPhysicalDeviceSurfaceFormats2KHR physicalDevice pSurfaceInfo =
  snd <$> getNumPhysicalDeviceSurfaceFormats2KHR physicalDevice pSurfaceInfo
    >>= \num -> snd <$> getPhysicalDeviceSurfaceFormats2KHR physicalDevice pSurfaceInfo num

