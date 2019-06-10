{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  AcquireNextImageInfoKHR(..)
  , 
  BindImageMemorySwapchainInfoKHR(..)
  , DeviceGroupPresentCapabilitiesKHR(..)
  , DeviceGroupPresentInfoKHR(..)
#endif
  , DeviceGroupPresentModeFlagBitsKHR
  , pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR
  , pattern DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR
  , pattern DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR
  , pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR
  , DeviceGroupPresentModeFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , DeviceGroupSwapchainCreateInfoKHR(..)
  , ImageSwapchainCreateInfoKHR(..)
  , PresentInfoKHR(..)
#endif
  , SwapchainCreateFlagBitsKHR
  , pattern SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  , pattern SWAPCHAIN_CREATE_PROTECTED_BIT_KHR
  , pattern SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR
  , SwapchainCreateFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , SwapchainCreateInfoKHR(..)
#endif
  , SwapchainKHR
  , acquireNextImage2KHR
  , acquireNextImageKHR
  , createSwapchainKHR
  , destroySwapchainKHR
#if defined(VK_USE_PLATFORM_GGP)
  , getDeviceGroupPresentCapabilitiesKHR
#endif
  , getDeviceGroupSurfacePresentModesKHR
#if defined(VK_USE_PLATFORM_GGP)
  , getNumPhysicalDevicePresentRectanglesKHR
  , getPhysicalDevicePresentRectanglesKHR
  , getAllPhysicalDevicePresentRectanglesKHR
#endif
  , getNumSwapchainImagesKHR
  , getSwapchainImagesKHR
  , getAllSwapchainImagesKHR
  , queuePresentKHR
  , withSwapchainKHR
  , pattern KHR_SWAPCHAIN_EXTENSION_NAME
  , pattern KHR_SWAPCHAIN_SPEC_VERSION
  , pattern STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_PRESENT_INFO_KHR
  , pattern IMAGE_LAYOUT_PRESENT_SRC_KHR
  , pattern SUBOPTIMAL_KHR
  , pattern ERROR_OUT_OF_DATE_KHR
  , pattern OBJECT_TYPE_SWAPCHAIN_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Control.Monad
  ( (<=<)
  )
#endif
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
  , Word64
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

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( Ptr
  )
#endif
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
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkDeviceGroupPresentModeFlagBitsKHR(..)
  , VkSwapchainCreateFlagBitsKHR(..)
  , VkSwapchainKHR
  , vkAcquireNextImage2KHR
  , vkAcquireNextImageKHR
  , vkCreateSwapchainKHR
  , vkDestroySwapchainKHR
  , vkGetDeviceGroupSurfacePresentModesKHR
  , vkGetSwapchainImagesKHR
  , vkQueuePresentKHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR
  , pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_KHR_SWAPCHAIN_SPEC_VERSION
  , pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR
  , pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( vkGetDeviceGroupPresentCapabilitiesKHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( vkGetPhysicalDevicePresentRectanglesKHR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain_mutable_format
  ( pattern VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Buffer
  ( SharingMode
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Core
  ( Format
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageUsageFlags
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
#endif
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  )
#endif
import Graphics.Vulkan.Core10.Queue
  ( Queue(..)
  , Fence
  , Semaphore
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( ColorSpaceKHR
  , CompositeAlphaFlagBitsKHR
  , PresentModeKHR
  , SurfaceTransformFlagBitsKHR
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
  ( pattern ERROR_OUT_OF_DATE_KHR
  , pattern OBJECT_TYPE_SWAPCHAIN_KHR
  , pattern STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_PRESENT_INFO_KHR
  , pattern STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern SUBOPTIMAL_KHR
  )
import Graphics.Vulkan.Core10.Image
  ( pattern IMAGE_LAYOUT_PRESENT_SRC_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkAcquireNextImageInfoKHR"
data AcquireNextImageInfoKHR = AcquireNextImageInfoKHR
  { -- No documentation found for Nested "AcquireNextImageInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AcquireNextImageInfoKHR" "swapchain"
  swapchain :: SwapchainKHR
  , -- No documentation found for Nested "AcquireNextImageInfoKHR" "timeout"
  timeout :: Word64
  , -- No documentation found for Nested "AcquireNextImageInfoKHR" "semaphore"
  semaphore :: Semaphore
  , -- No documentation found for Nested "AcquireNextImageInfoKHR" "fence"
  fence :: Fence
  , -- No documentation found for Nested "AcquireNextImageInfoKHR" "deviceMask"
  deviceMask :: Word32
  }
  deriving (Show, Eq)

instance Zero AcquireNextImageInfoKHR where
  zero = AcquireNextImageInfoKHR Nothing
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBindImageMemorySwapchainInfoKHR"
data BindImageMemorySwapchainInfoKHR = BindImageMemorySwapchainInfoKHR
  { -- No documentation found for Nested "BindImageMemorySwapchainInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImageMemorySwapchainInfoKHR" "swapchain"
  swapchain :: SwapchainKHR
  , -- No documentation found for Nested "BindImageMemorySwapchainInfoKHR" "imageIndex"
  imageIndex :: Word32
  }
  deriving (Show, Eq)

instance Zero BindImageMemorySwapchainInfoKHR where
  zero = BindImageMemorySwapchainInfoKHR Nothing
                                         zero
                                         zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceGroupPresentCapabilitiesKHR"
data DeviceGroupPresentCapabilitiesKHR = DeviceGroupPresentCapabilitiesKHR
  { -- No documentation found for Nested "DeviceGroupPresentCapabilitiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupPresentCapabilitiesKHR" "presentMask"
  presentMask :: Vector Word32
  , -- No documentation found for Nested "DeviceGroupPresentCapabilitiesKHR" "modes"
  modes :: DeviceGroupPresentModeFlagsKHR
  }
  deriving (Show, Eq)

instance Zero DeviceGroupPresentCapabilitiesKHR where
  zero = DeviceGroupPresentCapabilitiesKHR Nothing
                                           mempty
                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceGroupPresentInfoKHR"
data DeviceGroupPresentInfoKHR = DeviceGroupPresentInfoKHR
  { -- No documentation found for Nested "DeviceGroupPresentInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupPresentInfoKHR" "pDeviceMasks"
  deviceMasks :: Vector Word32
  , -- No documentation found for Nested "DeviceGroupPresentInfoKHR" "mode"
  mode :: DeviceGroupPresentModeFlagBitsKHR
  }
  deriving (Show, Eq)

instance Zero DeviceGroupPresentInfoKHR where
  zero = DeviceGroupPresentInfoKHR Nothing
                                   mempty
                                   zero

#endif

-- No documentation found for TopLevel "DeviceGroupPresentModeFlagBitsKHR"
type DeviceGroupPresentModeFlagBitsKHR = VkDeviceGroupPresentModeFlagBitsKHR


{-# complete DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR, DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR, DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR, DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR :: DeviceGroupPresentModeFlagBitsKHR #-}


-- No documentation found for Nested "DeviceGroupPresentModeFlagBitsKHR" "DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR"
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR :: (a ~ DeviceGroupPresentModeFlagBitsKHR) => a
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR = VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR


-- No documentation found for Nested "DeviceGroupPresentModeFlagBitsKHR" "DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR"
pattern DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR :: (a ~ DeviceGroupPresentModeFlagBitsKHR) => a
pattern DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR = VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR


-- No documentation found for Nested "DeviceGroupPresentModeFlagBitsKHR" "DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR"
pattern DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR :: (a ~ DeviceGroupPresentModeFlagBitsKHR) => a
pattern DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR = VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR


-- No documentation found for Nested "DeviceGroupPresentModeFlagBitsKHR" "DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR"
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR :: (a ~ DeviceGroupPresentModeFlagBitsKHR) => a
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR = VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR

-- No documentation found for TopLevel "DeviceGroupPresentModeFlagsKHR"
type DeviceGroupPresentModeFlagsKHR = DeviceGroupPresentModeFlagBitsKHR


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceGroupSwapchainCreateInfoKHR"
data DeviceGroupSwapchainCreateInfoKHR = DeviceGroupSwapchainCreateInfoKHR
  { -- No documentation found for Nested "DeviceGroupSwapchainCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupSwapchainCreateInfoKHR" "modes"
  modes :: DeviceGroupPresentModeFlagsKHR
  }
  deriving (Show, Eq)

instance Zero DeviceGroupSwapchainCreateInfoKHR where
  zero = DeviceGroupSwapchainCreateInfoKHR Nothing
                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImageSwapchainCreateInfoKHR"
data ImageSwapchainCreateInfoKHR = ImageSwapchainCreateInfoKHR
  { -- No documentation found for Nested "ImageSwapchainCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageSwapchainCreateInfoKHR" "swapchain"
  swapchain :: SwapchainKHR
  }
  deriving (Show, Eq)

instance Zero ImageSwapchainCreateInfoKHR where
  zero = ImageSwapchainCreateInfoKHR Nothing
                                     zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPresentInfoKHR"
data PresentInfoKHR = PresentInfoKHR
  { -- No documentation found for Nested "PresentInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PresentInfoKHR" "pWaitSemaphores"
  waitSemaphores :: Vector Semaphore
  , -- No documentation found for Nested "PresentInfoKHR" "pSwapchains"
  swapchains :: Vector SwapchainKHR
  , -- No documentation found for Nested "PresentInfoKHR" "pImageIndices"
  imageIndices :: Vector Word32
  , -- No documentation found for Nested "PresentInfoKHR" "pResults"
  results :: Ptr VkResult
  }
  deriving (Show, Eq)

instance Zero PresentInfoKHR where
  zero = PresentInfoKHR Nothing
                        mempty
                        mempty
                        mempty
                        nullPtr

#endif

-- No documentation found for TopLevel "SwapchainCreateFlagBitsKHR"
type SwapchainCreateFlagBitsKHR = VkSwapchainCreateFlagBitsKHR


{-# complete SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR, SWAPCHAIN_CREATE_PROTECTED_BIT_KHR, SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR, SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR :: SwapchainCreateFlagBitsKHR #-}


-- No documentation found for Nested "SwapchainCreateFlagBitsKHR" "SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR"
pattern SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR :: (a ~ SwapchainCreateFlagBitsKHR) => a
pattern SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR


-- No documentation found for Nested "SwapchainCreateFlagBitsKHR" "SWAPCHAIN_CREATE_PROTECTED_BIT_KHR"
pattern SWAPCHAIN_CREATE_PROTECTED_BIT_KHR :: (a ~ SwapchainCreateFlagBitsKHR) => a
pattern SWAPCHAIN_CREATE_PROTECTED_BIT_KHR = VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR


-- No documentation found for Nested "SwapchainCreateFlagBitsKHR" "SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR"
pattern SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR :: (a ~ SwapchainCreateFlagBitsKHR) => a
pattern SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR = VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR

-- No documentation found for TopLevel "SwapchainCreateFlagsKHR"
type SwapchainCreateFlagsKHR = SwapchainCreateFlagBitsKHR


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSwapchainCreateInfoKHR"
data SwapchainCreateInfoKHR = SwapchainCreateInfoKHR
  { -- No documentation found for Nested "SwapchainCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "flags"
  flags :: SwapchainCreateFlagsKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "surface"
  surface :: SurfaceKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "minImageCount"
  minImageCount :: Word32
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageFormat"
  imageFormat :: Format
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageColorSpace"
  imageColorSpace :: ColorSpaceKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageExtent"
  imageExtent :: Extent2D
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageArrayLayers"
  imageArrayLayers :: Word32
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageUsage"
  imageUsage :: ImageUsageFlags
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageSharingMode"
  imageSharingMode :: SharingMode
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "pQueueFamilyIndices"
  queueFamilyIndices :: Vector Word32
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "preTransform"
  preTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "compositeAlpha"
  compositeAlpha :: CompositeAlphaFlagBitsKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "presentMode"
  presentMode :: PresentModeKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "clipped"
  clipped :: Bool
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "oldSwapchain"
  oldSwapchain :: SwapchainKHR
  }
  deriving (Show, Eq)

instance Zero SwapchainCreateInfoKHR where
  zero = SwapchainCreateInfoKHR Nothing
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                zero
                                mempty
                                zero
                                zero
                                zero
                                False
                                zero

#endif

-- No documentation found for TopLevel "SwapchainKHR"
type SwapchainKHR = VkSwapchainKHR


-- No documentation found for TopLevel "vkAcquireNextImage2KHR"
acquireNextImage2KHR :: Device ->  AcquireNextImageInfoKHR ->  IO (VkResult, Word32)
acquireNextImage2KHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkAcquireNextImageKHR"
acquireNextImageKHR :: Device ->  SwapchainKHR ->  Word64 ->  Semaphore ->  Fence ->  IO (VkResult, Word32)
acquireNextImageKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateSwapchainKHR"
createSwapchainKHR :: Device ->  SwapchainCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SwapchainKHR)
createSwapchainKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroySwapchainKHR"
destroySwapchainKHR :: Device ->  SwapchainKHR ->  Maybe AllocationCallbacks ->  IO ()
destroySwapchainKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetDeviceGroupPresentCapabilitiesKHR"
getDeviceGroupPresentCapabilitiesKHR :: Device ->  IO (DeviceGroupPresentCapabilitiesKHR)
getDeviceGroupPresentCapabilitiesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
#endif


-- No documentation found for TopLevel "vkGetDeviceGroupSurfacePresentModesKHR"
getDeviceGroupSurfacePresentModesKHR :: Device ->  SurfaceKHR ->  IO (DeviceGroupPresentModeFlagsKHR)
getDeviceGroupSurfacePresentModesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "vkGetPhysicalDevicePresentRectanglesKHR"
getNumPhysicalDevicePresentRectanglesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (VkResult, Word32)
getNumPhysicalDevicePresentRectanglesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetPhysicalDevicePresentRectanglesKHR"
getPhysicalDevicePresentRectanglesKHR :: PhysicalDevice ->  SurfaceKHR ->  Word32 ->  IO (VkResult, Vector Rect2D)
getPhysicalDevicePresentRectanglesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getPhysicalDevicePresentRectanglesKHR'.
getAllPhysicalDevicePresentRectanglesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (Vector Rect2D)
getAllPhysicalDevicePresentRectanglesKHR physicalDevice' surface' =
  snd <$> getNumPhysicalDevicePresentRectanglesKHR physicalDevice' surface'
    >>= \num -> snd <$> getPhysicalDevicePresentRectanglesKHR physicalDevice' surface' num

#endif


-- No documentation found for TopLevel "vkGetSwapchainImagesKHR"
getNumSwapchainImagesKHR :: Device ->  SwapchainKHR ->  IO (VkResult, Word32)
getNumSwapchainImagesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "vkGetSwapchainImagesKHR"
getSwapchainImagesKHR :: Device ->  SwapchainKHR ->  Word32 ->  IO (VkResult, Vector Image)
getSwapchainImagesKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}
-- | Returns all the values available from 'getSwapchainImagesKHR'.
getAllSwapchainImagesKHR :: Device ->  SwapchainKHR ->  IO (Vector Image)
getAllSwapchainImagesKHR device' swapchain' =
  snd <$> getNumSwapchainImagesKHR device' swapchain'
    >>= \num -> snd <$> getSwapchainImagesKHR device' swapchain' num



-- No documentation found for TopLevel "vkQueuePresentKHR"
queuePresentKHR :: Queue ->  PresentInfoKHR ->  IO (VkResult)
queuePresentKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createSwapchainKHR' and 'destroySwapchainKHR' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withSwapchainKHR
  :: Device -> SwapchainCreateInfoKHR -> Maybe AllocationCallbacks -> (SwapchainKHR -> IO a) -> IO a
withSwapchainKHR device swapchainCreateInfoKHR allocationCallbacks = bracket
  (createSwapchainKHR device swapchainCreateInfoKHR allocationCallbacks)
  (\o -> destroySwapchainKHR device o allocationCallbacks)

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_EXTENSION_NAME"
pattern KHR_SWAPCHAIN_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SWAPCHAIN_EXTENSION_NAME = VK_KHR_SWAPCHAIN_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_SPEC_VERSION"
pattern KHR_SWAPCHAIN_SPEC_VERSION :: Integral a => a
pattern KHR_SWAPCHAIN_SPEC_VERSION = VK_KHR_SWAPCHAIN_SPEC_VERSION
