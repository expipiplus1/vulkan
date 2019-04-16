{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_surface
  ( ColorSpaceKHR
  , CompositeAlphaFlagBitsKHR
  , CompositeAlphaFlagsKHR
  , PresentModeKHR
  , withCStructSurfaceCapabilitiesKHR
  , fromCStructSurfaceCapabilitiesKHR
  , SurfaceCapabilitiesKHR(..)
  , withCStructSurfaceFormatKHR
  , fromCStructSurfaceFormatKHR
  , SurfaceFormatKHR(..)
  , SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  , SurfaceTransformFlagsKHR
  , destroySurfaceKHR
  , getPhysicalDeviceSurfaceCapabilitiesKHR
  , getNumPhysicalDeviceSurfaceFormatsKHR
  , getPhysicalDeviceSurfaceFormatsKHR
  , getAllPhysicalDeviceSurfaceFormatsKHR
  , getNumPhysicalDeviceSurfacePresentModesKHR
  , getPhysicalDeviceSurfacePresentModesKHR
  , getAllPhysicalDeviceSurfacePresentModesKHR
  , getPhysicalDeviceSurfaceSupportKHR
  , pattern VK_KHR_SURFACE_SPEC_VERSION
  , pattern VK_KHR_SURFACE_EXTENSION_NAME
  , pattern VK_ERROR_SURFACE_LOST_KHR
  , pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern VK_OBJECT_TYPE_SURFACE_KHR
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
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( destroySurfaceKHR
  , getPhysicalDeviceSurfaceCapabilitiesKHR
  , getPhysicalDeviceSurfaceFormatsKHR
  , getPhysicalDeviceSurfacePresentModesKHR
  , getPhysicalDeviceSurfaceSupportKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR(..)
  , VkCompositeAlphaFlagBitsKHR(..)
  , VkPresentModeKHR(..)
  , VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceFormatKHR(..)
  , VkSurfaceTransformFlagBitsKHR(..)
  , VkSurfaceKHR
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  , bool32ToBool
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , PhysicalDevice(..)
  , ImageUsageFlags
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , fromCStructExtent2D
  , withCStructExtent2D
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern VK_ERROR_SURFACE_LOST_KHR
  , pattern VK_KHR_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_SURFACE_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_SURFACE_KHR
  )


-- No documentation found for TopLevel "ColorSpaceKHR"
type ColorSpaceKHR = VkColorSpaceKHR
-- No documentation found for TopLevel "CompositeAlphaFlagBitsKHR"
type CompositeAlphaFlagBitsKHR = VkCompositeAlphaFlagBitsKHR
-- No documentation found for TopLevel "CompositeAlphaFlagsKHR"
type CompositeAlphaFlagsKHR = CompositeAlphaFlagBitsKHR
-- No documentation found for TopLevel "PresentModeKHR"
type PresentModeKHR = VkPresentModeKHR
-- No documentation found for TopLevel "SurfaceCapabilitiesKHR"
data SurfaceCapabilitiesKHR = SurfaceCapabilitiesKHR
  { -- No documentation found for Nested "SurfaceCapabilitiesKHR" "minImageCount"
  vkMinImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "maxImageCount"
  vkMaxImageCount :: Word32
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "currentExtent"
  vkCurrentExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "minImageExtent"
  vkMinImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "maxImageExtent"
  vkMaxImageExtent :: Extent2D
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "maxImageArrayLayers"
  vkMaxImageArrayLayers :: Word32
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "supportedTransforms"
  vkSupportedTransforms :: SurfaceTransformFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "currentTransform"
  vkCurrentTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "supportedCompositeAlpha"
  vkSupportedCompositeAlpha :: CompositeAlphaFlagsKHR
  , -- No documentation found for Nested "SurfaceCapabilitiesKHR" "supportedUsageFlags"
  vkSupportedUsageFlags :: ImageUsageFlags
  }
  deriving (Show, Eq)
withCStructSurfaceCapabilitiesKHR :: SurfaceCapabilitiesKHR -> (VkSurfaceCapabilitiesKHR -> IO a) -> IO a
withCStructSurfaceCapabilitiesKHR from cont = withCStructExtent2D (vkMaxImageExtent (from :: SurfaceCapabilitiesKHR)) (\maxImageExtent -> withCStructExtent2D (vkMinImageExtent (from :: SurfaceCapabilitiesKHR)) (\minImageExtent -> withCStructExtent2D (vkCurrentExtent (from :: SurfaceCapabilitiesKHR)) (\currentExtent -> cont (VkSurfaceCapabilitiesKHR (vkMinImageCount (from :: SurfaceCapabilitiesKHR)) (vkMaxImageCount (from :: SurfaceCapabilitiesKHR)) currentExtent minImageExtent maxImageExtent (vkMaxImageArrayLayers (from :: SurfaceCapabilitiesKHR)) (vkSupportedTransforms (from :: SurfaceCapabilitiesKHR)) (vkCurrentTransform (from :: SurfaceCapabilitiesKHR)) (vkSupportedCompositeAlpha (from :: SurfaceCapabilitiesKHR)) (vkSupportedUsageFlags (from :: SurfaceCapabilitiesKHR))))))
fromCStructSurfaceCapabilitiesKHR :: VkSurfaceCapabilitiesKHR -> IO SurfaceCapabilitiesKHR
fromCStructSurfaceCapabilitiesKHR c = SurfaceCapabilitiesKHR <$> pure (vkMinImageCount (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> pure (vkMaxImageCount (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> (fromCStructExtent2D (vkCurrentExtent (c :: VkSurfaceCapabilitiesKHR)))
                                                             <*> (fromCStructExtent2D (vkMinImageExtent (c :: VkSurfaceCapabilitiesKHR)))
                                                             <*> (fromCStructExtent2D (vkMaxImageExtent (c :: VkSurfaceCapabilitiesKHR)))
                                                             <*> pure (vkMaxImageArrayLayers (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> pure (vkSupportedTransforms (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> pure (vkCurrentTransform (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> pure (vkSupportedCompositeAlpha (c :: VkSurfaceCapabilitiesKHR))
                                                             <*> pure (vkSupportedUsageFlags (c :: VkSurfaceCapabilitiesKHR))
-- No documentation found for TopLevel "SurfaceFormatKHR"
data SurfaceFormatKHR = SurfaceFormatKHR
  { -- No documentation found for Nested "SurfaceFormatKHR" "format"
  vkFormat :: Format
  , -- No documentation found for Nested "SurfaceFormatKHR" "colorSpace"
  vkColorSpace :: ColorSpaceKHR
  }
  deriving (Show, Eq)
withCStructSurfaceFormatKHR :: SurfaceFormatKHR -> (VkSurfaceFormatKHR -> IO a) -> IO a
withCStructSurfaceFormatKHR from cont = cont (VkSurfaceFormatKHR (vkFormat (from :: SurfaceFormatKHR)) (vkColorSpace (from :: SurfaceFormatKHR)))
fromCStructSurfaceFormatKHR :: VkSurfaceFormatKHR -> IO SurfaceFormatKHR
fromCStructSurfaceFormatKHR c = SurfaceFormatKHR <$> pure (vkFormat (c :: VkSurfaceFormatKHR))
                                                 <*> pure (vkColorSpace (c :: VkSurfaceFormatKHR))
-- No documentation found for TopLevel "SurfaceKHR"
type SurfaceKHR = VkSurfaceKHR
-- No documentation found for TopLevel "SurfaceTransformFlagBitsKHR"
type SurfaceTransformFlagBitsKHR = VkSurfaceTransformFlagBitsKHR
-- No documentation found for TopLevel "SurfaceTransformFlagsKHR"
type SurfaceTransformFlagsKHR = SurfaceTransformFlagBitsKHR

-- | Wrapper for vkDestroySurfaceKHR
destroySurfaceKHR :: Instance ->  SurfaceKHR ->  Maybe AllocationCallbacks ->  IO ()
destroySurfaceKHR = \(Instance instance' commandTable) -> \surface -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroySurfaceKHR commandTable instance' surface pAllocator *> (pure ()))

-- | Wrapper for vkGetPhysicalDeviceSurfaceCapabilitiesKHR
getPhysicalDeviceSurfaceCapabilitiesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (SurfaceCapabilitiesKHR)
getPhysicalDeviceSurfaceCapabilitiesKHR = \(PhysicalDevice physicalDevice commandTable) -> \surface -> alloca (\pSurfaceCapabilities -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfaceCapabilitiesKHR commandTable physicalDevice surface pSurfaceCapabilities >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructSurfaceCapabilitiesKHR <=< peek) pSurfaceCapabilities)))

-- | Wrapper for vkGetPhysicalDeviceSurfaceFormatsKHR
getNumPhysicalDeviceSurfaceFormatsKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfaceFormatsKHR = \(PhysicalDevice physicalDevice commandTable) -> \surface -> alloca (\pSurfaceFormatCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfaceFormatsKHR commandTable physicalDevice surface pSurfaceFormatCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pSurfaceFormatCount)))

-- | Wrapper for vkGetPhysicalDeviceSurfaceFormatsKHR
getPhysicalDeviceSurfaceFormatsKHR :: PhysicalDevice ->  SurfaceKHR ->  Word32 ->  IO (VkResult, Vector SurfaceFormatKHR)
getPhysicalDeviceSurfaceFormatsKHR = \(PhysicalDevice physicalDevice commandTable) -> \surface -> \surfaceFormatCount -> allocaArray (fromIntegral surfaceFormatCount) (\pSurfaceFormats -> with surfaceFormatCount (\pSurfaceFormatCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfaceFormatsKHR commandTable physicalDevice surface pSurfaceFormatCount pSurfaceFormats >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructSurfaceFormatKHR <=< peekElemOff p) pSurfaceFormats) =<< (fromIntegral <$> (peek pSurfaceFormatCount)))))))
-- | Call 'getNumPhysicalDeviceSurfaceFormatsKHR' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceSurfaceFormatsKHR' to get all the values.
getAllPhysicalDeviceSurfaceFormatsKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (Vector SurfaceFormatKHR)
getAllPhysicalDeviceSurfaceFormatsKHR physicalDevice surface =
  snd <$> getNumPhysicalDeviceSurfaceFormatsKHR physicalDevice surface
    >>= \num -> snd <$> getPhysicalDeviceSurfaceFormatsKHR physicalDevice surface num


-- | Wrapper for vkGetPhysicalDeviceSurfacePresentModesKHR
getNumPhysicalDeviceSurfacePresentModesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (VkResult, Word32)
getNumPhysicalDeviceSurfacePresentModesKHR = \(PhysicalDevice physicalDevice commandTable) -> \surface -> alloca (\pPresentModeCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfacePresentModesKHR commandTable physicalDevice surface pPresentModeCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pPresentModeCount)))

-- | Wrapper for vkGetPhysicalDeviceSurfacePresentModesKHR
getPhysicalDeviceSurfacePresentModesKHR :: PhysicalDevice ->  SurfaceKHR ->  Word32 ->  IO (VkResult, Vector PresentModeKHR)
getPhysicalDeviceSurfacePresentModesKHR = \(PhysicalDevice physicalDevice commandTable) -> \surface -> \presentModeCount -> allocaArray (fromIntegral presentModeCount) (\pPresentModes -> with presentModeCount (\pPresentModeCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfacePresentModesKHR commandTable physicalDevice surface pPresentModeCount pPresentModes >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM (peekElemOff pPresentModes) =<< (fromIntegral <$> (peek pPresentModeCount)))))))
-- | Call 'getNumPhysicalDeviceSurfacePresentModesKHR' to get the number of return values, then use that
-- number to call 'getPhysicalDeviceSurfacePresentModesKHR' to get all the values.
getAllPhysicalDeviceSurfacePresentModesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (Vector PresentModeKHR)
getAllPhysicalDeviceSurfacePresentModesKHR physicalDevice surface =
  snd <$> getNumPhysicalDeviceSurfacePresentModesKHR physicalDevice surface
    >>= \num -> snd <$> getPhysicalDeviceSurfacePresentModesKHR physicalDevice surface num


-- | Wrapper for vkGetPhysicalDeviceSurfaceSupportKHR
getPhysicalDeviceSurfaceSupportKHR :: PhysicalDevice ->  Word32 ->  SurfaceKHR ->  IO (Bool)
getPhysicalDeviceSurfaceSupportKHR = \(PhysicalDevice physicalDevice commandTable) -> \queueFamilyIndex -> \surface -> alloca (\pSupported -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceSurfaceSupportKHR commandTable physicalDevice queueFamilyIndex surface pSupported >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (bool32ToBool <$> (peek pSupported))))
