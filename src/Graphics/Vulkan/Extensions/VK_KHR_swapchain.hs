{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( withCStructAcquireNextImageInfoKHR
  , fromCStructAcquireNextImageInfoKHR
  , AcquireNextImageInfoKHR(..)
  , withCStructBindImageMemorySwapchainInfoKHR
  , fromCStructBindImageMemorySwapchainInfoKHR
  , BindImageMemorySwapchainInfoKHR(..)
  , withCStructDeviceGroupPresentCapabilitiesKHR
  , fromCStructDeviceGroupPresentCapabilitiesKHR
  , DeviceGroupPresentCapabilitiesKHR(..)
  , withCStructDeviceGroupPresentInfoKHR
  , fromCStructDeviceGroupPresentInfoKHR
  , DeviceGroupPresentInfoKHR(..)
  , DeviceGroupPresentModeFlagBitsKHR
  , DeviceGroupPresentModeFlagsKHR
  , withCStructDeviceGroupSwapchainCreateInfoKHR
  , fromCStructDeviceGroupSwapchainCreateInfoKHR
  , DeviceGroupSwapchainCreateInfoKHR(..)
  , withCStructImageSwapchainCreateInfoKHR
  , fromCStructImageSwapchainCreateInfoKHR
  , ImageSwapchainCreateInfoKHR(..)
  , withCStructPresentInfoKHR
  , fromCStructPresentInfoKHR
  , PresentInfoKHR(..)
  , SwapchainCreateFlagBitsKHR
  , SwapchainCreateFlagsKHR
  , withCStructSwapchainCreateInfoKHR
  , fromCStructSwapchainCreateInfoKHR
  , SwapchainCreateInfoKHR(..)
  , SwapchainKHR
  , acquireNextImage2KHR
  , acquireNextImageKHR
  , createSwapchainKHR
  , destroySwapchainKHR
  , getDeviceGroupPresentCapabilitiesKHR
  , getDeviceGroupSurfacePresentModesKHR
  , getNumPhysicalDevicePresentRectanglesKHR
  , getPhysicalDevicePresentRectanglesKHR
  , getAllPhysicalDevicePresentRectanglesKHR
  , getNumSwapchainImagesKHR
  , getSwapchainImagesKHR
  , getAllSwapchainImagesKHR
  , queuePresentKHR
  , withSwapchainKHR
  , pattern VK_KHR_SWAPCHAIN_SPEC_VERSION
  , pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
  , pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
  , pattern VK_SUBOPTIMAL_KHR
  , pattern VK_ERROR_OUT_OF_DATE_KHR
  , pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  , pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
  )
import Data.List
  ( minimum
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
  )
import qualified Data.Vector.Generic
  ( convert
  )
import qualified Data.Vector.Generic.Sized
  ( convert
  )
import qualified Data.Vector.Storable.Sized
  ( fromSized
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
  ( acquireNextImage2KHR
  , acquireNextImageKHR
  , createSwapchainKHR
  , destroySwapchainKHR
  , getDeviceGroupPresentCapabilitiesKHR
  , getDeviceGroupSurfacePresentModesKHR
  , getPhysicalDevicePresentRectanglesKHR
  , getSwapchainImagesKHR
  , queuePresentKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkAcquireNextImageInfoKHR(..)
  , VkBindImageMemorySwapchainInfoKHR(..)
  , VkDeviceGroupPresentCapabilitiesKHR(..)
  , VkDeviceGroupPresentInfoKHR(..)
  , VkDeviceGroupPresentModeFlagBitsKHR(..)
  , VkDeviceGroupSwapchainCreateInfoKHR(..)
  , VkImageSwapchainCreateInfoKHR(..)
  , VkPresentInfoKHR(..)
  , VkSwapchainCreateFlagBitsKHR(..)
  , VkSwapchainCreateInfoKHR(..)
  , VkSwapchainKHR
  , pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core10.Buffer
  ( SharingMode
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  , Result
  , bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , PhysicalDevice(..)
  , ImageUsageFlags
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Image
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Extent2D(..)
  , Rect2D(..)
  , fromCStructExtent2D
  , fromCStructRect2D
  , withCStructExtent2D
  )
import Graphics.Vulkan.Core10.Queue
  ( Queue(..)
  , Fence
  , Semaphore
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( ColorSpaceKHR
  , CompositeAlphaFlagBitsKHR
  , PresentModeKHR
  , SurfaceKHR
  , SurfaceTransformFlagBitsKHR
  )
import Graphics.Vulkan.Marshal.Utils
  ( padSized
  , withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( pattern VK_ERROR_OUT_OF_DATE_KHR
  , pattern VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
  , pattern VK_KHR_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_KHR_SWAPCHAIN_SPEC_VERSION
  , pattern VK_OBJECT_TYPE_SWAPCHAIN_KHR
  , pattern VK_SUBOPTIMAL_KHR
  , pattern VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR
  , pattern VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
  )


-- No documentation found for TopLevel "AcquireNextImageInfoKHR"
data AcquireNextImageInfoKHR = AcquireNextImageInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "AcquireNextImageInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AcquireNextImageInfoKHR" "swapchain"
  vkSwapchain :: SwapchainKHR
  , -- No documentation found for Nested "AcquireNextImageInfoKHR" "timeout"
  vkTimeout :: Word64
  , -- No documentation found for Nested "AcquireNextImageInfoKHR" "semaphore"
  vkSemaphore :: Semaphore
  , -- No documentation found for Nested "AcquireNextImageInfoKHR" "fence"
  vkFence :: Fence
  , -- No documentation found for Nested "AcquireNextImageInfoKHR" "deviceMask"
  vkDeviceMask :: Word32
  }
  deriving (Show, Eq)
withCStructAcquireNextImageInfoKHR :: AcquireNextImageInfoKHR -> (VkAcquireNextImageInfoKHR -> IO a) -> IO a
withCStructAcquireNextImageInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: AcquireNextImageInfoKHR)) (\pPNext -> cont (VkAcquireNextImageInfoKHR VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR pPNext (vkSwapchain (from :: AcquireNextImageInfoKHR)) (vkTimeout (from :: AcquireNextImageInfoKHR)) (vkSemaphore (from :: AcquireNextImageInfoKHR)) (vkFence (from :: AcquireNextImageInfoKHR)) (vkDeviceMask (from :: AcquireNextImageInfoKHR))))
fromCStructAcquireNextImageInfoKHR :: VkAcquireNextImageInfoKHR -> IO AcquireNextImageInfoKHR
fromCStructAcquireNextImageInfoKHR c = AcquireNextImageInfoKHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAcquireNextImageInfoKHR)))
                                                               <*> pure (vkSwapchain (c :: VkAcquireNextImageInfoKHR))
                                                               <*> pure (vkTimeout (c :: VkAcquireNextImageInfoKHR))
                                                               <*> pure (vkSemaphore (c :: VkAcquireNextImageInfoKHR))
                                                               <*> pure (vkFence (c :: VkAcquireNextImageInfoKHR))
                                                               <*> pure (vkDeviceMask (c :: VkAcquireNextImageInfoKHR))
-- No documentation found for TopLevel "BindImageMemorySwapchainInfoKHR"
data BindImageMemorySwapchainInfoKHR = BindImageMemorySwapchainInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "BindImageMemorySwapchainInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImageMemorySwapchainInfoKHR" "swapchain"
  vkSwapchain :: SwapchainKHR
  , -- No documentation found for Nested "BindImageMemorySwapchainInfoKHR" "imageIndex"
  vkImageIndex :: Word32
  }
  deriving (Show, Eq)
withCStructBindImageMemorySwapchainInfoKHR :: BindImageMemorySwapchainInfoKHR -> (VkBindImageMemorySwapchainInfoKHR -> IO a) -> IO a
withCStructBindImageMemorySwapchainInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: BindImageMemorySwapchainInfoKHR)) (\pPNext -> cont (VkBindImageMemorySwapchainInfoKHR VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR pPNext (vkSwapchain (from :: BindImageMemorySwapchainInfoKHR)) (vkImageIndex (from :: BindImageMemorySwapchainInfoKHR))))
fromCStructBindImageMemorySwapchainInfoKHR :: VkBindImageMemorySwapchainInfoKHR -> IO BindImageMemorySwapchainInfoKHR
fromCStructBindImageMemorySwapchainInfoKHR c = BindImageMemorySwapchainInfoKHR <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindImageMemorySwapchainInfoKHR)))
                                                                               <*> pure (vkSwapchain (c :: VkBindImageMemorySwapchainInfoKHR))
                                                                               <*> pure (vkImageIndex (c :: VkBindImageMemorySwapchainInfoKHR))
-- No documentation found for TopLevel "DeviceGroupPresentCapabilitiesKHR"
data DeviceGroupPresentCapabilitiesKHR = DeviceGroupPresentCapabilitiesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceGroupPresentCapabilitiesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupPresentCapabilitiesKHR" "presentMask"
  vkPresentMask :: Vector Word32
  , -- No documentation found for Nested "DeviceGroupPresentCapabilitiesKHR" "modes"
  vkModes :: DeviceGroupPresentModeFlagsKHR
  }
  deriving (Show, Eq)
withCStructDeviceGroupPresentCapabilitiesKHR :: DeviceGroupPresentCapabilitiesKHR -> (VkDeviceGroupPresentCapabilitiesKHR -> IO a) -> IO a
withCStructDeviceGroupPresentCapabilitiesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: DeviceGroupPresentCapabilitiesKHR)) (\pPNext -> cont (VkDeviceGroupPresentCapabilitiesKHR VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR pPNext (Data.Vector.Generic.Sized.convert (padSized 0 (vkPresentMask (from :: DeviceGroupPresentCapabilitiesKHR)))) (vkModes (from :: DeviceGroupPresentCapabilitiesKHR))))
fromCStructDeviceGroupPresentCapabilitiesKHR :: VkDeviceGroupPresentCapabilitiesKHR -> IO DeviceGroupPresentCapabilitiesKHR
fromCStructDeviceGroupPresentCapabilitiesKHR c = DeviceGroupPresentCapabilitiesKHR <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupPresentCapabilitiesKHR)))
                                                                                   <*> pure (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkPresentMask (c :: VkDeviceGroupPresentCapabilitiesKHR))))
                                                                                   <*> pure (vkModes (c :: VkDeviceGroupPresentCapabilitiesKHR))
-- No documentation found for TopLevel "DeviceGroupPresentInfoKHR"
data DeviceGroupPresentInfoKHR = DeviceGroupPresentInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceGroupPresentInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupPresentInfoKHR" "pDeviceMasks"
  vkPDeviceMasks :: Vector Word32
  , -- No documentation found for Nested "DeviceGroupPresentInfoKHR" "mode"
  vkMode :: DeviceGroupPresentModeFlagBitsKHR
  }
  deriving (Show, Eq)
withCStructDeviceGroupPresentInfoKHR :: DeviceGroupPresentInfoKHR -> (VkDeviceGroupPresentInfoKHR -> IO a) -> IO a
withCStructDeviceGroupPresentInfoKHR from cont = withVec (&) (vkPDeviceMasks (from :: DeviceGroupPresentInfoKHR)) (\pDeviceMasks -> maybeWith withSomeVkStruct (vkPNext (from :: DeviceGroupPresentInfoKHR)) (\pPNext -> cont (VkDeviceGroupPresentInfoKHR VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR pPNext (fromIntegral (Data.Vector.length (vkPDeviceMasks (from :: DeviceGroupPresentInfoKHR)))) pDeviceMasks (vkMode (from :: DeviceGroupPresentInfoKHR)))))
fromCStructDeviceGroupPresentInfoKHR :: VkDeviceGroupPresentInfoKHR -> IO DeviceGroupPresentInfoKHR
fromCStructDeviceGroupPresentInfoKHR c = DeviceGroupPresentInfoKHR <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupPresentInfoKHR)))
                                                                   -- Length valued member elided
                                                                   <*> (Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkDeviceGroupPresentInfoKHR))) (peekElemOff (vkPDeviceMasks (c :: VkDeviceGroupPresentInfoKHR))))
                                                                   <*> pure (vkMode (c :: VkDeviceGroupPresentInfoKHR))
-- No documentation found for TopLevel "DeviceGroupPresentModeFlagBitsKHR"
type DeviceGroupPresentModeFlagBitsKHR = VkDeviceGroupPresentModeFlagBitsKHR
-- No documentation found for TopLevel "DeviceGroupPresentModeFlagsKHR"
type DeviceGroupPresentModeFlagsKHR = DeviceGroupPresentModeFlagBitsKHR
-- No documentation found for TopLevel "DeviceGroupSwapchainCreateInfoKHR"
data DeviceGroupSwapchainCreateInfoKHR = DeviceGroupSwapchainCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceGroupSwapchainCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupSwapchainCreateInfoKHR" "modes"
  vkModes :: DeviceGroupPresentModeFlagsKHR
  }
  deriving (Show, Eq)
withCStructDeviceGroupSwapchainCreateInfoKHR :: DeviceGroupSwapchainCreateInfoKHR -> (VkDeviceGroupSwapchainCreateInfoKHR -> IO a) -> IO a
withCStructDeviceGroupSwapchainCreateInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: DeviceGroupSwapchainCreateInfoKHR)) (\pPNext -> cont (VkDeviceGroupSwapchainCreateInfoKHR VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR pPNext (vkModes (from :: DeviceGroupSwapchainCreateInfoKHR))))
fromCStructDeviceGroupSwapchainCreateInfoKHR :: VkDeviceGroupSwapchainCreateInfoKHR -> IO DeviceGroupSwapchainCreateInfoKHR
fromCStructDeviceGroupSwapchainCreateInfoKHR c = DeviceGroupSwapchainCreateInfoKHR <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupSwapchainCreateInfoKHR)))
                                                                                   <*> pure (vkModes (c :: VkDeviceGroupSwapchainCreateInfoKHR))
-- No documentation found for TopLevel "ImageSwapchainCreateInfoKHR"
data ImageSwapchainCreateInfoKHR = ImageSwapchainCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "ImageSwapchainCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageSwapchainCreateInfoKHR" "swapchain"
  vkSwapchain :: SwapchainKHR
  }
  deriving (Show, Eq)
withCStructImageSwapchainCreateInfoKHR :: ImageSwapchainCreateInfoKHR -> (VkImageSwapchainCreateInfoKHR -> IO a) -> IO a
withCStructImageSwapchainCreateInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImageSwapchainCreateInfoKHR)) (\pPNext -> cont (VkImageSwapchainCreateInfoKHR VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR pPNext (vkSwapchain (from :: ImageSwapchainCreateInfoKHR))))
fromCStructImageSwapchainCreateInfoKHR :: VkImageSwapchainCreateInfoKHR -> IO ImageSwapchainCreateInfoKHR
fromCStructImageSwapchainCreateInfoKHR c = ImageSwapchainCreateInfoKHR <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageSwapchainCreateInfoKHR)))
                                                                       <*> pure (vkSwapchain (c :: VkImageSwapchainCreateInfoKHR))
-- No documentation found for TopLevel "PresentInfoKHR"
data PresentInfoKHR = PresentInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PresentInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "PresentInfoKHR" "pWaitSemaphores"
  vkPWaitSemaphores :: Vector Semaphore
  -- Length valued member elided
  , -- No documentation found for Nested "PresentInfoKHR" "pSwapchains"
  vkPSwapchains :: Vector SwapchainKHR
  , -- No documentation found for Nested "PresentInfoKHR" "pImageIndices"
  vkPImageIndices :: Vector Word32
  , -- No documentation found for Nested "PresentInfoKHR" "pResults"
  vkPResults :: Maybe (Vector Result)
  }
  deriving (Show, Eq)
withCStructPresentInfoKHR :: PresentInfoKHR -> (VkPresentInfoKHR -> IO a) -> IO a
withCStructPresentInfoKHR from cont = maybeWith (withVec (&)) (vkPResults (from :: PresentInfoKHR)) (\pResults -> withVec (&) (vkPImageIndices (from :: PresentInfoKHR)) (\pImageIndices -> withVec (&) (vkPSwapchains (from :: PresentInfoKHR)) (\pSwapchains -> withVec (&) (vkPWaitSemaphores (from :: PresentInfoKHR)) (\pWaitSemaphores -> maybeWith withSomeVkStruct (vkPNext (from :: PresentInfoKHR)) (\pPNext -> cont (VkPresentInfoKHR VK_STRUCTURE_TYPE_PRESENT_INFO_KHR pPNext (fromIntegral (Data.Vector.length (vkPWaitSemaphores (from :: PresentInfoKHR)))) pWaitSemaphores (fromIntegral (minimum ([ Data.Vector.length (vkPSwapchains (from :: PresentInfoKHR))
, Data.Vector.length (vkPImageIndices (from :: PresentInfoKHR)) ] ++ [Data.Vector.length v | Just v <- [ (vkPResults (from :: PresentInfoKHR)) ]]))) pSwapchains pImageIndices pResults))))))
fromCStructPresentInfoKHR :: VkPresentInfoKHR -> IO PresentInfoKHR
fromCStructPresentInfoKHR c = PresentInfoKHR <$> -- Univalued Member elided
                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPresentInfoKHR)))
                                             -- Length valued member elided
                                             <*> (Data.Vector.generateM (fromIntegral (vkWaitSemaphoreCount (c :: VkPresentInfoKHR))) (peekElemOff (vkPWaitSemaphores (c :: VkPresentInfoKHR))))
                                             -- Length valued member elided
                                             <*> (Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkPresentInfoKHR))) (peekElemOff (vkPSwapchains (c :: VkPresentInfoKHR))))
                                             <*> (Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkPresentInfoKHR))) (peekElemOff (vkPImageIndices (c :: VkPresentInfoKHR))))
                                             <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkPresentInfoKHR))) (peekElemOff p)) (vkPResults (c :: VkPresentInfoKHR))
-- No documentation found for TopLevel "SwapchainCreateFlagBitsKHR"
type SwapchainCreateFlagBitsKHR = VkSwapchainCreateFlagBitsKHR
-- No documentation found for TopLevel "SwapchainCreateFlagsKHR"
type SwapchainCreateFlagsKHR = SwapchainCreateFlagBitsKHR
-- No documentation found for TopLevel "SwapchainCreateInfoKHR"
data SwapchainCreateInfoKHR = SwapchainCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SwapchainCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "flags"
  vkFlags :: SwapchainCreateFlagsKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "surface"
  vkSurface :: SurfaceKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "minImageCount"
  vkMinImageCount :: Word32
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageFormat"
  vkImageFormat :: Format
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageColorSpace"
  vkImageColorSpace :: ColorSpaceKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageExtent"
  vkImageExtent :: Extent2D
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageArrayLayers"
  vkImageArrayLayers :: Word32
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageUsage"
  vkImageUsage :: ImageUsageFlags
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "imageSharingMode"
  vkImageSharingMode :: SharingMode
  -- Length valued member elided
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "pQueueFamilyIndices"
  vkPQueueFamilyIndices :: Vector Word32
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "preTransform"
  vkPreTransform :: SurfaceTransformFlagBitsKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "compositeAlpha"
  vkCompositeAlpha :: CompositeAlphaFlagBitsKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "presentMode"
  vkPresentMode :: PresentModeKHR
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "clipped"
  vkClipped :: Bool
  , -- No documentation found for Nested "SwapchainCreateInfoKHR" "oldSwapchain"
  vkOldSwapchain :: SwapchainKHR
  }
  deriving (Show, Eq)
withCStructSwapchainCreateInfoKHR :: SwapchainCreateInfoKHR -> (VkSwapchainCreateInfoKHR -> IO a) -> IO a
withCStructSwapchainCreateInfoKHR from cont = withVec (&) (vkPQueueFamilyIndices (from :: SwapchainCreateInfoKHR)) (\pQueueFamilyIndices -> withCStructExtent2D (vkImageExtent (from :: SwapchainCreateInfoKHR)) (\imageExtent -> maybeWith withSomeVkStruct (vkPNext (from :: SwapchainCreateInfoKHR)) (\pPNext -> cont (VkSwapchainCreateInfoKHR VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR pPNext (vkFlags (from :: SwapchainCreateInfoKHR)) (vkSurface (from :: SwapchainCreateInfoKHR)) (vkMinImageCount (from :: SwapchainCreateInfoKHR)) (vkImageFormat (from :: SwapchainCreateInfoKHR)) (vkImageColorSpace (from :: SwapchainCreateInfoKHR)) imageExtent (vkImageArrayLayers (from :: SwapchainCreateInfoKHR)) (vkImageUsage (from :: SwapchainCreateInfoKHR)) (vkImageSharingMode (from :: SwapchainCreateInfoKHR)) (fromIntegral (Data.Vector.length (vkPQueueFamilyIndices (from :: SwapchainCreateInfoKHR)))) pQueueFamilyIndices (vkPreTransform (from :: SwapchainCreateInfoKHR)) (vkCompositeAlpha (from :: SwapchainCreateInfoKHR)) (vkPresentMode (from :: SwapchainCreateInfoKHR)) (boolToBool32 (vkClipped (from :: SwapchainCreateInfoKHR))) (vkOldSwapchain (from :: SwapchainCreateInfoKHR))))))
fromCStructSwapchainCreateInfoKHR :: VkSwapchainCreateInfoKHR -> IO SwapchainCreateInfoKHR
fromCStructSwapchainCreateInfoKHR c = SwapchainCreateInfoKHR <$> -- Univalued Member elided
                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSwapchainCreateInfoKHR)))
                                                             <*> pure (vkFlags (c :: VkSwapchainCreateInfoKHR))
                                                             <*> pure (vkSurface (c :: VkSwapchainCreateInfoKHR))
                                                             <*> pure (vkMinImageCount (c :: VkSwapchainCreateInfoKHR))
                                                             <*> pure (vkImageFormat (c :: VkSwapchainCreateInfoKHR))
                                                             <*> pure (vkImageColorSpace (c :: VkSwapchainCreateInfoKHR))
                                                             <*> (fromCStructExtent2D (vkImageExtent (c :: VkSwapchainCreateInfoKHR)))
                                                             <*> pure (vkImageArrayLayers (c :: VkSwapchainCreateInfoKHR))
                                                             <*> pure (vkImageUsage (c :: VkSwapchainCreateInfoKHR))
                                                             <*> pure (vkImageSharingMode (c :: VkSwapchainCreateInfoKHR))
                                                             -- Length valued member elided
                                                             <*> (Data.Vector.generateM (fromIntegral (vkQueueFamilyIndexCount (c :: VkSwapchainCreateInfoKHR))) (peekElemOff (vkPQueueFamilyIndices (c :: VkSwapchainCreateInfoKHR))))
                                                             <*> pure (vkPreTransform (c :: VkSwapchainCreateInfoKHR))
                                                             <*> pure (vkCompositeAlpha (c :: VkSwapchainCreateInfoKHR))
                                                             <*> pure (vkPresentMode (c :: VkSwapchainCreateInfoKHR))
                                                             <*> pure (bool32ToBool (vkClipped (c :: VkSwapchainCreateInfoKHR)))
                                                             <*> pure (vkOldSwapchain (c :: VkSwapchainCreateInfoKHR))
-- No documentation found for TopLevel "SwapchainKHR"
type SwapchainKHR = VkSwapchainKHR

-- | Wrapper for 'vkAcquireNextImage2KHR'
acquireNextImage2KHR :: Device ->  AcquireNextImageInfoKHR ->  IO (VkResult, Word32)
acquireNextImage2KHR = \(Device device commandTable) -> \acquireInfo -> alloca (\pImageIndex -> (\a -> withCStructAcquireNextImageInfoKHR a . flip with) acquireInfo (\pAcquireInfo -> Graphics.Vulkan.C.Dynamic.acquireNextImage2KHR commandTable device pAcquireInfo pImageIndex >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pImageIndex))))

-- | Wrapper for 'vkAcquireNextImageKHR'
acquireNextImageKHR :: Device ->  SwapchainKHR ->  Word64 ->  Semaphore ->  Fence ->  IO ( VkResult
, Word32 )
acquireNextImageKHR = \(Device device commandTable) -> \swapchain -> \timeout -> \semaphore -> \fence -> alloca (\pImageIndex -> Graphics.Vulkan.C.Dynamic.acquireNextImageKHR commandTable device swapchain timeout semaphore fence pImageIndex >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pImageIndex)))

-- | Wrapper for 'vkCreateSwapchainKHR'
createSwapchainKHR :: Device ->  SwapchainCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO ( SwapchainKHR )
createSwapchainKHR = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pSwapchain -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructSwapchainCreateInfoKHR a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createSwapchainKHR commandTable device pCreateInfo pAllocator pSwapchain >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSwapchain)))))

-- | Wrapper for 'vkDestroySwapchainKHR'
destroySwapchainKHR :: Device ->  SwapchainKHR ->  Maybe AllocationCallbacks ->  IO ()
destroySwapchainKHR = \(Device device commandTable) -> \swapchain -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroySwapchainKHR commandTable device swapchain pAllocator *> (pure ()))

-- | Wrapper for 'vkGetDeviceGroupPresentCapabilitiesKHR'
getDeviceGroupPresentCapabilitiesKHR :: Device ->  IO (DeviceGroupPresentCapabilitiesKHR)
getDeviceGroupPresentCapabilitiesKHR = \(Device device commandTable) -> alloca (\pDeviceGroupPresentCapabilities -> Graphics.Vulkan.C.Dynamic.getDeviceGroupPresentCapabilitiesKHR commandTable device pDeviceGroupPresentCapabilities >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((fromCStructDeviceGroupPresentCapabilitiesKHR <=< peek) pDeviceGroupPresentCapabilities)))

-- | Wrapper for 'vkGetDeviceGroupSurfacePresentModesKHR'
getDeviceGroupSurfacePresentModesKHR :: Device ->  SurfaceKHR ->  IO (DeviceGroupPresentModeFlagsKHR)
getDeviceGroupSurfacePresentModesKHR = \(Device device commandTable) -> \surface -> alloca (\pModes -> Graphics.Vulkan.C.Dynamic.getDeviceGroupSurfacePresentModesKHR commandTable device surface pModes >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pModes)))

-- | Wrapper for 'vkGetPhysicalDevicePresentRectanglesKHR'
getNumPhysicalDevicePresentRectanglesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (VkResult, Word32)
getNumPhysicalDevicePresentRectanglesKHR = \(PhysicalDevice physicalDevice commandTable) -> \surface -> alloca (\pRectCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDevicePresentRectanglesKHR commandTable physicalDevice surface pRectCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pRectCount)))

-- | Wrapper for 'vkGetPhysicalDevicePresentRectanglesKHR'
getPhysicalDevicePresentRectanglesKHR :: PhysicalDevice ->  SurfaceKHR ->  Word32 ->  IO (VkResult, Vector Rect2D)
getPhysicalDevicePresentRectanglesKHR = \(PhysicalDevice physicalDevice commandTable) -> \surface -> \rectCount -> allocaArray (fromIntegral rectCount) (\pRects -> with rectCount (\pRectCount -> Graphics.Vulkan.C.Dynamic.getPhysicalDevicePresentRectanglesKHR commandTable physicalDevice surface pRectCount pRects >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM ((\p -> fromCStructRect2D <=< peekElemOff p) pRects) =<< (fromIntegral <$> (peek pRectCount)))))))
-- | Call 'getNumPhysicalDevicePresentRectanglesKHR' to get the number of return values, then use that
-- number to call 'getPhysicalDevicePresentRectanglesKHR' to get all the values.
getAllPhysicalDevicePresentRectanglesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (Vector Rect2D)
getAllPhysicalDevicePresentRectanglesKHR physicalDevice surface =
  snd <$> getNumPhysicalDevicePresentRectanglesKHR physicalDevice surface
    >>= \num -> snd <$> getPhysicalDevicePresentRectanglesKHR physicalDevice surface num


-- | Wrapper for 'vkGetSwapchainImagesKHR'
getNumSwapchainImagesKHR :: Device ->  SwapchainKHR ->  IO (VkResult, Word32)
getNumSwapchainImagesKHR = \(Device device commandTable) -> \swapchain -> alloca (\pSwapchainImageCount -> Graphics.Vulkan.C.Dynamic.getSwapchainImagesKHR commandTable device swapchain pSwapchainImageCount nullPtr >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>peek pSwapchainImageCount)))

-- | Wrapper for 'vkGetSwapchainImagesKHR'
getSwapchainImagesKHR :: Device ->  SwapchainKHR ->  Word32 ->  IO (VkResult, Vector Image)
getSwapchainImagesKHR = \(Device device commandTable) -> \swapchain -> \swapchainImageCount -> allocaArray (fromIntegral swapchainImageCount) (\pSwapchainImages -> with swapchainImageCount (\pSwapchainImageCount -> Graphics.Vulkan.C.Dynamic.getSwapchainImagesKHR commandTable device swapchain pSwapchainImageCount pSwapchainImages >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(flip Data.Vector.generateM (peekElemOff pSwapchainImages) =<< (fromIntegral <$> (peek pSwapchainImageCount)))))))
-- | Call 'getNumSwapchainImagesKHR' to get the number of return values, then use that
-- number to call 'getSwapchainImagesKHR' to get all the values.
getAllSwapchainImagesKHR :: Device ->  SwapchainKHR ->  IO (Vector Image)
getAllSwapchainImagesKHR device swapchain =
  snd <$> getNumSwapchainImagesKHR device swapchain
    >>= \num -> snd <$> getSwapchainImagesKHR device swapchain num


-- | Wrapper for 'vkQueuePresentKHR'
queuePresentKHR :: Queue ->  PresentInfoKHR ->  IO (VkResult)
queuePresentKHR = \(Queue queue commandTable) -> \presentInfo -> (\a -> withCStructPresentInfoKHR a . flip with) presentInfo (\pPresentInfo -> Graphics.Vulkan.C.Dynamic.queuePresentKHR commandTable queue pPresentInfo >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure r)))
withSwapchainKHR :: CreateInfo -> Maybe AllocationCallbacks -> (t -> IO a) -> IO a
withSwapchainKHR createInfo allocationCallbacks =
  bracket
    (vkCreateSwapchainKHR createInfo allocationCallbacks)
    (`vkDestroySwapchainKHR` allocationCallbacks)
