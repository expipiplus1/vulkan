{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

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
  , pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR
  , pattern DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR
  , pattern DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR
  , pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR
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
  ( bracket
  , throwIO
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
  ( empty
  , generateM
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
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
  , vkAcquireNextImage2KHR
  , vkAcquireNextImageKHR
  , vkCreateSwapchainKHR
  , vkDestroySwapchainKHR
  , vkGetDeviceGroupPresentCapabilitiesKHR
  , vkGetDeviceGroupSurfacePresentModesKHR
  , vkGetPhysicalDevicePresentRectanglesKHR
  , vkGetSwapchainImagesKHR
  , vkQueuePresentKHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR
  , pattern VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR
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



-- | VkAcquireNextImageInfoKHR - Structure specifying parameters of the
-- acquire
--
-- = Description
--
-- If 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR'
-- is used, the device mask is considered to include all physical devices
-- in the logical device.
--
-- __Note__
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImage2KHR'
-- signals at most one semaphore, even if the application requests waiting
-- for multiple physical devices to be ready via the @deviceMask@. However,
-- only a single physical device /can/ wait on that semaphore, since the
-- semaphore becomes unsignaled when the wait succeeds. For other physical
-- devices to wait for the image to be ready, it is necessary for the
-- application to submit semaphore signal operation(s) to that first
-- physical device to signal additional semaphore(s) after the wait
-- succeeds, which the other physical device(s) /can/ wait upon.
--
-- == Valid Usage
--
-- -   @swapchain@ /must/ not be in the retired state
--
-- -   If @semaphore@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/ be
--     unsignaled
--
-- -   If @semaphore@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/ not
--     have any uncompleted signal or wait operations pending
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/ be
--     unsignaled and /must/ not be associated with any other queue command
--     that has not yet completed execution on that queue
--
-- -   @semaphore@ and @fence@ /must/ not both be equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   @deviceMask@ /must/ be a valid device mask
--
-- -   @deviceMask@ /must/ not be zero
--
-- Unresolved directive in VkAcquireNextImageInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkAcquireNextImageInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data AcquireNextImageInfoKHR = AcquireNextImageInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "AcquireNextImageInfoKHR" "pNext"
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

-- | A function to temporarily allocate memory for a 'VkAcquireNextImageInfoKHR' and
-- marshal a 'AcquireNextImageInfoKHR' into it. The 'VkAcquireNextImageInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAcquireNextImageInfoKHR :: AcquireNextImageInfoKHR -> (VkAcquireNextImageInfoKHR -> IO a) -> IO a
withCStructAcquireNextImageInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: AcquireNextImageInfoKHR)) (\pPNext -> cont (VkAcquireNextImageInfoKHR VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR pPNext (swapchain (marshalled :: AcquireNextImageInfoKHR)) (timeout (marshalled :: AcquireNextImageInfoKHR)) (semaphore (marshalled :: AcquireNextImageInfoKHR)) (fence (marshalled :: AcquireNextImageInfoKHR)) (deviceMask (marshalled :: AcquireNextImageInfoKHR))))

-- | A function to read a 'VkAcquireNextImageInfoKHR' and all additional
-- structures in the pointer chain into a 'AcquireNextImageInfoKHR'.
fromCStructAcquireNextImageInfoKHR :: VkAcquireNextImageInfoKHR -> IO AcquireNextImageInfoKHR
fromCStructAcquireNextImageInfoKHR c = AcquireNextImageInfoKHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAcquireNextImageInfoKHR)))
                                                               <*> pure (vkSwapchain (c :: VkAcquireNextImageInfoKHR))
                                                               <*> pure (vkTimeout (c :: VkAcquireNextImageInfoKHR))
                                                               <*> pure (vkSemaphore (c :: VkAcquireNextImageInfoKHR))
                                                               <*> pure (vkFence (c :: VkAcquireNextImageInfoKHR))
                                                               <*> pure (vkDeviceMask (c :: VkAcquireNextImageInfoKHR))

instance Zero AcquireNextImageInfoKHR where
  zero = AcquireNextImageInfoKHR Nothing
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero



-- | VkBindImageMemorySwapchainInfoKHR - Structure specifying swapchain image
-- memory to bind to
--
-- = Description
--
-- If @swapchain@ is not @NULL@, the @swapchain@ and @imageIndex@ are used
-- to determine the memory that the image is bound to, instead of @memory@
-- and @memoryOffset@.
--
-- Memory /can/ be bound to a swapchain and use the @pDeviceIndices@ or
-- @pSplitInstanceBindRegions@ members of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'.
--
-- == Valid Usage
--
-- Unresolved directive in VkBindImageMemorySwapchainInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkBindImageMemorySwapchainInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data BindImageMemorySwapchainInfoKHR = BindImageMemorySwapchainInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "BindImageMemorySwapchainInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImageMemorySwapchainInfoKHR" "swapchain"
  swapchain :: SwapchainKHR
  , -- No documentation found for Nested "BindImageMemorySwapchainInfoKHR" "imageIndex"
  imageIndex :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBindImageMemorySwapchainInfoKHR' and
-- marshal a 'BindImageMemorySwapchainInfoKHR' into it. The 'VkBindImageMemorySwapchainInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBindImageMemorySwapchainInfoKHR :: BindImageMemorySwapchainInfoKHR -> (VkBindImageMemorySwapchainInfoKHR -> IO a) -> IO a
withCStructBindImageMemorySwapchainInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BindImageMemorySwapchainInfoKHR)) (\pPNext -> cont (VkBindImageMemorySwapchainInfoKHR VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR pPNext (swapchain (marshalled :: BindImageMemorySwapchainInfoKHR)) (imageIndex (marshalled :: BindImageMemorySwapchainInfoKHR))))

-- | A function to read a 'VkBindImageMemorySwapchainInfoKHR' and all additional
-- structures in the pointer chain into a 'BindImageMemorySwapchainInfoKHR'.
fromCStructBindImageMemorySwapchainInfoKHR :: VkBindImageMemorySwapchainInfoKHR -> IO BindImageMemorySwapchainInfoKHR
fromCStructBindImageMemorySwapchainInfoKHR c = BindImageMemorySwapchainInfoKHR <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindImageMemorySwapchainInfoKHR)))
                                                                               <*> pure (vkSwapchain (c :: VkBindImageMemorySwapchainInfoKHR))
                                                                               <*> pure (vkImageIndex (c :: VkBindImageMemorySwapchainInfoKHR))

instance Zero BindImageMemorySwapchainInfoKHR where
  zero = BindImageMemorySwapchainInfoKHR Nothing
                                         zero
                                         zero



-- | VkDeviceGroupPresentCapabilitiesKHR - Present capabilities from other
-- physical devices
--
-- = Description
--
-- @modes@ always has
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'
-- set.
--
-- The present mode flags are also used when presenting an image, in
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentInfoKHR'::@mode@.
--
-- If a device group only includes a single physical device, then @modes@
-- /must/ equal
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'.
--
-- Unresolved directive in VkDeviceGroupPresentCapabilitiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkDeviceGroupPresentCapabilitiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data DeviceGroupPresentCapabilitiesKHR = DeviceGroupPresentCapabilitiesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceGroupPresentCapabilitiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupPresentCapabilitiesKHR" "presentMask"
  presentMask :: Vector Word32
  , -- No documentation found for Nested "DeviceGroupPresentCapabilitiesKHR" "modes"
  modes :: DeviceGroupPresentModeFlagsKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceGroupPresentCapabilitiesKHR' and
-- marshal a 'DeviceGroupPresentCapabilitiesKHR' into it. The 'VkDeviceGroupPresentCapabilitiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceGroupPresentCapabilitiesKHR :: DeviceGroupPresentCapabilitiesKHR -> (VkDeviceGroupPresentCapabilitiesKHR -> IO a) -> IO a
withCStructDeviceGroupPresentCapabilitiesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DeviceGroupPresentCapabilitiesKHR)) (\pPNext -> cont (VkDeviceGroupPresentCapabilitiesKHR VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR pPNext (Data.Vector.Generic.Sized.convert (padSized 0 (presentMask (marshalled :: DeviceGroupPresentCapabilitiesKHR)))) (modes (marshalled :: DeviceGroupPresentCapabilitiesKHR))))

-- | A function to read a 'VkDeviceGroupPresentCapabilitiesKHR' and all additional
-- structures in the pointer chain into a 'DeviceGroupPresentCapabilitiesKHR'.
fromCStructDeviceGroupPresentCapabilitiesKHR :: VkDeviceGroupPresentCapabilitiesKHR -> IO DeviceGroupPresentCapabilitiesKHR
fromCStructDeviceGroupPresentCapabilitiesKHR c = DeviceGroupPresentCapabilitiesKHR <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupPresentCapabilitiesKHR)))
                                                                                   <*> pure (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkPresentMask (c :: VkDeviceGroupPresentCapabilitiesKHR))))
                                                                                   <*> pure (vkModes (c :: VkDeviceGroupPresentCapabilitiesKHR))

instance Zero DeviceGroupPresentCapabilitiesKHR where
  zero = DeviceGroupPresentCapabilitiesKHR Nothing
                                           Data.Vector.empty
                                           zero



-- | VkDeviceGroupPresentInfoKHR - Mode and mask controlling which physical
-- devices\' images are presented
--
-- = Description
--
-- If @mode@ is
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR',
-- then each element of @pDeviceMasks@ selects which instance of the
-- swapchain image is presented. Each element of @pDeviceMasks@ /must/ have
-- exactly one bit set, and the corresponding physical device /must/ have a
-- presentation engine as reported by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR'.
--
-- If @mode@ is
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR',
-- then each element of @pDeviceMasks@ selects which instance of the
-- swapchain image is presented. Each element of @pDeviceMasks@ /must/ have
-- exactly one bit set, and some physical device in the logical device
-- /must/ include that bit in its
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR'::@presentMask@.
--
-- If @mode@ is
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR',
-- then each element of @pDeviceMasks@ selects which instances of the
-- swapchain image are component-wise summed and the sum of those images is
-- presented. If the sum in any component is outside the representable
-- range, the value of that component is undefined. Each element of
-- @pDeviceMasks@ /must/ have a value for which all set bits are set in one
-- of the elements of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR'::@presentMask@.
--
-- If @mode@ is
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR',
-- then each element of @pDeviceMasks@ selects which instance(s) of the
-- swapchain images are presented. For each bit set in each element of
-- @pDeviceMasks@, the corresponding physical device /must/ have a
-- presentation engine as reported by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR'.
--
-- If
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentInfoKHR'
-- is not provided or @swapchainCount@ is zero then the masks are
-- considered to be @1@. If
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentInfoKHR'
-- is not provided, @mode@ is considered to be
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'.
--
-- == Valid Usage
--
-- -   @swapchainCount@ /must/ equal @0@ or
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR'::@swapchainCount@
--
-- -   If @mode@ is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR',
--     then each element of @pDeviceMasks@ /must/ have exactly one bit set,
--     and the corresponding element of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR'::@presentMask@
--     /must/ be non-zero
--
-- -   If @mode@ is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR',
--     then each element of @pDeviceMasks@ /must/ have exactly one bit set,
--     and some physical device in the logical device /must/ include that
--     bit in its
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR'::@presentMask@.
--
-- -   If @mode@ is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR',
--     then each element of @pDeviceMasks@ /must/ have a value for which
--     all set bits are set in one of the elements of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR'::@presentMask@
--
-- -   If @mode@ is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR',
--     then for each bit set in each element of @pDeviceMasks@, the
--     corresponding element of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR'::@presentMask@
--     /must/ be non-zero
--
-- -   The value of each element of @pDeviceMasks@ /must/ be equal to the
--     device mask passed in
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkAcquireNextImageInfoKHR'::@deviceMask@
--     when the image index was last acquired
--
-- -   @mode@ /must/ have exactly one bit set, and that bit /must/ have
--     been included in
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupSwapchainCreateInfoKHR'::@modes@
--
-- Unresolved directive in VkDeviceGroupPresentInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkDeviceGroupPresentInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data DeviceGroupPresentInfoKHR = DeviceGroupPresentInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceGroupPresentInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupPresentInfoKHR" "pDeviceMasks"
  deviceMasks :: Vector Word32
  , -- No documentation found for Nested "DeviceGroupPresentInfoKHR" "mode"
  mode :: DeviceGroupPresentModeFlagBitsKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceGroupPresentInfoKHR' and
-- marshal a 'DeviceGroupPresentInfoKHR' into it. The 'VkDeviceGroupPresentInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceGroupPresentInfoKHR :: DeviceGroupPresentInfoKHR -> (VkDeviceGroupPresentInfoKHR -> IO a) -> IO a
withCStructDeviceGroupPresentInfoKHR marshalled cont = withVec (&) (deviceMasks (marshalled :: DeviceGroupPresentInfoKHR)) (\pPDeviceMasks -> maybeWith withSomeVkStruct (next (marshalled :: DeviceGroupPresentInfoKHR)) (\pPNext -> cont (VkDeviceGroupPresentInfoKHR VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR pPNext (fromIntegral (Data.Vector.length (deviceMasks (marshalled :: DeviceGroupPresentInfoKHR)))) pPDeviceMasks (mode (marshalled :: DeviceGroupPresentInfoKHR)))))

-- | A function to read a 'VkDeviceGroupPresentInfoKHR' and all additional
-- structures in the pointer chain into a 'DeviceGroupPresentInfoKHR'.
fromCStructDeviceGroupPresentInfoKHR :: VkDeviceGroupPresentInfoKHR -> IO DeviceGroupPresentInfoKHR
fromCStructDeviceGroupPresentInfoKHR c = DeviceGroupPresentInfoKHR <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupPresentInfoKHR)))
                                                                   -- Length valued member elided
                                                                   <*> (Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkDeviceGroupPresentInfoKHR))) (peekElemOff (vkPDeviceMasks (c :: VkDeviceGroupPresentInfoKHR))))
                                                                   <*> pure (vkMode (c :: VkDeviceGroupPresentInfoKHR))

instance Zero DeviceGroupPresentInfoKHR where
  zero = DeviceGroupPresentInfoKHR Nothing
                                   Data.Vector.empty
                                   zero


-- | VkDeviceGroupPresentModeFlagBitsKHR - Bitmask specifying supported
-- device group present modes
--
-- = See Also
--
-- No cross-references are available
type DeviceGroupPresentModeFlagBitsKHR = VkDeviceGroupPresentModeFlagBitsKHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'
-- specifies that any physical device with a presentation engine /can/
-- present its own swapchain images.
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR :: (a ~ DeviceGroupPresentModeFlagBitsKHR) => a
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR = VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR'
-- specifies that any physical device with a presentation engine /can/
-- present swapchain images from any physical device in its @presentMask@.
pattern DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR :: (a ~ DeviceGroupPresentModeFlagBitsKHR) => a
pattern DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR = VK_DEVICE_GROUP_PRESENT_MODE_REMOTE_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR'
-- specifies that any physical device with a presentation engine /can/
-- present the sum of swapchain images from any physical devices in its
-- @presentMask@.
pattern DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR :: (a ~ DeviceGroupPresentModeFlagBitsKHR) => a
pattern DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR = VK_DEVICE_GROUP_PRESENT_MODE_SUM_BIT_KHR


-- | 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR'
-- specifies that multiple physical devices with a presentation engine
-- /can/ each present their own swapchain images.
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR :: (a ~ DeviceGroupPresentModeFlagBitsKHR) => a
pattern DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR = VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_MULTI_DEVICE_BIT_KHR

-- | VkDeviceGroupPresentModeFlagsKHR - Bitmask of
-- VkDeviceGroupPresentModeFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagsKHR'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagBitsKHR'.
--
-- = See Also
--
-- No cross-references are available
type DeviceGroupPresentModeFlagsKHR = DeviceGroupPresentModeFlagBitsKHR


-- | VkDeviceGroupSwapchainCreateInfoKHR - Structure specifying parameters of
-- a newly created swapchain object
--
-- = Description
--
-- If this structure is not present, @modes@ is considered to be
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_DEVICE_GROUP_PRESENT_MODE_LOCAL_BIT_KHR'.
--
-- Unresolved directive in VkDeviceGroupSwapchainCreateInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkDeviceGroupSwapchainCreateInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data DeviceGroupSwapchainCreateInfoKHR = DeviceGroupSwapchainCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceGroupSwapchainCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupSwapchainCreateInfoKHR" "modes"
  modes :: DeviceGroupPresentModeFlagsKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceGroupSwapchainCreateInfoKHR' and
-- marshal a 'DeviceGroupSwapchainCreateInfoKHR' into it. The 'VkDeviceGroupSwapchainCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceGroupSwapchainCreateInfoKHR :: DeviceGroupSwapchainCreateInfoKHR -> (VkDeviceGroupSwapchainCreateInfoKHR -> IO a) -> IO a
withCStructDeviceGroupSwapchainCreateInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DeviceGroupSwapchainCreateInfoKHR)) (\pPNext -> cont (VkDeviceGroupSwapchainCreateInfoKHR VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR pPNext (modes (marshalled :: DeviceGroupSwapchainCreateInfoKHR))))

-- | A function to read a 'VkDeviceGroupSwapchainCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'DeviceGroupSwapchainCreateInfoKHR'.
fromCStructDeviceGroupSwapchainCreateInfoKHR :: VkDeviceGroupSwapchainCreateInfoKHR -> IO DeviceGroupSwapchainCreateInfoKHR
fromCStructDeviceGroupSwapchainCreateInfoKHR c = DeviceGroupSwapchainCreateInfoKHR <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupSwapchainCreateInfoKHR)))
                                                                                   <*> pure (vkModes (c :: VkDeviceGroupSwapchainCreateInfoKHR))

instance Zero DeviceGroupSwapchainCreateInfoKHR where
  zero = DeviceGroupSwapchainCreateInfoKHR Nothing
                                           zero



-- | VkImageSwapchainCreateInfoKHR - Specify that an image will be bound to
-- swapchain memory
--
-- == Valid Usage
--
-- -   If @swapchain@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the fields of
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' /must/ match the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#swapchain-wsi-image-create-info implied image creation parameters>
--     of the swapchain
--
-- Unresolved directive in VkImageSwapchainCreateInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkImageSwapchainCreateInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data ImageSwapchainCreateInfoKHR = ImageSwapchainCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "ImageSwapchainCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageSwapchainCreateInfoKHR" "swapchain"
  swapchain :: SwapchainKHR
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageSwapchainCreateInfoKHR' and
-- marshal a 'ImageSwapchainCreateInfoKHR' into it. The 'VkImageSwapchainCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageSwapchainCreateInfoKHR :: ImageSwapchainCreateInfoKHR -> (VkImageSwapchainCreateInfoKHR -> IO a) -> IO a
withCStructImageSwapchainCreateInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImageSwapchainCreateInfoKHR)) (\pPNext -> cont (VkImageSwapchainCreateInfoKHR VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR pPNext (swapchain (marshalled :: ImageSwapchainCreateInfoKHR))))

-- | A function to read a 'VkImageSwapchainCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'ImageSwapchainCreateInfoKHR'.
fromCStructImageSwapchainCreateInfoKHR :: VkImageSwapchainCreateInfoKHR -> IO ImageSwapchainCreateInfoKHR
fromCStructImageSwapchainCreateInfoKHR c = ImageSwapchainCreateInfoKHR <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageSwapchainCreateInfoKHR)))
                                                                       <*> pure (vkSwapchain (c :: VkImageSwapchainCreateInfoKHR))

instance Zero ImageSwapchainCreateInfoKHR where
  zero = ImageSwapchainCreateInfoKHR Nothing
                                     zero



-- | VkPresentInfoKHR - Structure describing parameters of a queue
-- presentation
--
-- = Description
--
-- Before an application /can/ present an image, the image’s layout /must/
-- be transitioned to the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR'
-- layout, or for a shared presentable image the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR'
-- layout.
--
-- __Note__
--
-- When transitioning the image to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR'
-- or
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR',
-- there is no need to delay subsequent processing, or perform any
-- visibility operations (as
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR'
-- performs automatic visibility operations). To achieve this, the
-- @dstAccessMask@ member of the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier'
-- /should/ be set to @0@, and the @dstStageMask@ parameter /should/ be set
-- to
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT'.
--
-- == Valid Usage
--
-- -   Each element of @pImageIndices@ /must/ be the index of a presentable
--     image acquired from the swapchain specified by the corresponding
--     element of the @pSwapchains@ array, and the presented image
--     subresource /must/ be in the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR'
--     layout at the time the operation is executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- Unresolved directive in VkPresentInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkPresentInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data PresentInfoKHR = PresentInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "PresentInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "PresentInfoKHR" "pWaitSemaphores"
  waitSemaphores :: Vector Semaphore
  -- Length valued member elided
  , -- No documentation found for Nested "PresentInfoKHR" "pSwapchains"
  swapchains :: Vector SwapchainKHR
  , -- No documentation found for Nested "PresentInfoKHR" "pImageIndices"
  imageIndices :: Vector Word32
  , -- No documentation found for Nested "PresentInfoKHR" "pResults"
  results :: Maybe (Vector Result)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPresentInfoKHR' and
-- marshal a 'PresentInfoKHR' into it. The 'VkPresentInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPresentInfoKHR :: PresentInfoKHR -> (VkPresentInfoKHR -> IO a) -> IO a
withCStructPresentInfoKHR marshalled cont = maybeWith (withVec (&)) (results (marshalled :: PresentInfoKHR)) (\pPResults -> withVec (&) (imageIndices (marshalled :: PresentInfoKHR)) (\pPImageIndices -> withVec (&) (swapchains (marshalled :: PresentInfoKHR)) (\pPSwapchains -> withVec (&) (waitSemaphores (marshalled :: PresentInfoKHR)) (\pPWaitSemaphores -> maybeWith withSomeVkStruct (next (marshalled :: PresentInfoKHR)) (\pPNext -> cont (VkPresentInfoKHR VK_STRUCTURE_TYPE_PRESENT_INFO_KHR pPNext (fromIntegral (Data.Vector.length (waitSemaphores (marshalled :: PresentInfoKHR)))) pPWaitSemaphores (fromIntegral (minimum ([Data.Vector.length (swapchains (marshalled :: PresentInfoKHR)), Data.Vector.length (imageIndices (marshalled :: PresentInfoKHR))] ++ [Data.Vector.length v | Just v <- [(results (marshalled :: PresentInfoKHR))]]))) pPSwapchains pPImageIndices pPResults))))))

-- | A function to read a 'VkPresentInfoKHR' and all additional
-- structures in the pointer chain into a 'PresentInfoKHR'.
fromCStructPresentInfoKHR :: VkPresentInfoKHR -> IO PresentInfoKHR
fromCStructPresentInfoKHR c = PresentInfoKHR <$> -- Univalued Member elided
                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPresentInfoKHR)))
                                             -- Length valued member elided
                                             <*> (Data.Vector.generateM (fromIntegral (vkWaitSemaphoreCount (c :: VkPresentInfoKHR))) (peekElemOff (vkPWaitSemaphores (c :: VkPresentInfoKHR))))
                                             -- Length valued member elided
                                             <*> (Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkPresentInfoKHR))) (peekElemOff (vkPSwapchains (c :: VkPresentInfoKHR))))
                                             <*> (Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkPresentInfoKHR))) (peekElemOff (vkPImageIndices (c :: VkPresentInfoKHR))))
                                             <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkSwapchainCount (c :: VkPresentInfoKHR))) (peekElemOff p)) (vkPResults (c :: VkPresentInfoKHR))

instance Zero PresentInfoKHR where
  zero = PresentInfoKHR Nothing
                        Data.Vector.empty
                        Data.Vector.empty
                        Data.Vector.empty
                        Nothing


-- | VkSwapchainCreateFlagBitsKHR - Bitmask controlling swapchain creation
--
-- = See Also
--
-- No cross-references are available
type SwapchainCreateFlagBitsKHR = VkSwapchainCreateFlagBitsKHR

-- | VkSwapchainCreateFlagsKHR - Bitmask of VkSwapchainCreateFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateFlagsKHR'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateFlagBitsKHR'.
--
-- = See Also
--
-- No cross-references are available
type SwapchainCreateFlagsKHR = SwapchainCreateFlagBitsKHR


-- | VkSwapchainCreateInfoKHR - Structure specifying parameters of a newly
-- created swapchain object
--
-- = Description
--
-- __Note__
--
-- On some platforms, it is normal that @maxImageExtent@ /may/ become @(0,
-- 0)@, for example when the window is minimized. In such a case, it is not
-- possible to create a swapchain due to the Valid Usage requirements.
--
-- -   @imageArrayLayers@ is the number of views in a multiview\/stereo
--     surface. For non-stereoscopic-3D applications, this value is 1.
--
-- -   @imageUsage@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'
--     describing the intended usage of the (acquired) swapchain images.
--
-- -   @imageSharingMode@ is the sharing mode used for the image(s) of the
--     swapchain.
--
-- -   @queueFamilyIndexCount@ is the number of queue families having
--     access to the image(s) of the swapchain when @imageSharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT'.
--
-- -   @pQueueFamilyIndices@ is an array of queue family indices having
--     access to the images(s) of the swapchain when @imageSharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT'.
--
-- -   @preTransform@ is a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceTransformFlagBitsKHR'
--     value describing the transform, relative to the presentation
--     engine’s natural orientation, applied to the image content prior to
--     presentation. If it does not match the @currentTransform@ value
--     returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR',
--     the presentation engine will transform the image content as part of
--     the presentation operation.
--
-- -   @compositeAlpha@ is a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkCompositeAlphaFlagBitsKHR'
--     value indicating the alpha compositing mode to use when this surface
--     is composited together with other surfaces on certain window
--     systems.
--
-- -   @presentMode@ is the presentation mode the swapchain will use. A
--     swapchain’s present mode determines how incoming present requests
--     will be processed and queued internally.
--
-- -   @clipped@ specifies whether the Vulkan implementation is allowed to
--     discard rendering operations that affect regions of the surface that
--     are not visible.
--
--     -   If set to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the
--         presentable images associated with the swapchain /may/ not own
--         all of their pixels. Pixels in the presentable images that
--         correspond to regions of the target surface obscured by another
--         window on the desktop, or subject to some other clipping
--         mechanism will have undefined content when read back. Pixel
--         shaders /may/ not execute for these pixels, and thus any side
--         effects they would have had will not occur.
--         'Graphics.Vulkan.C.Core10.Core.VK_TRUE' value does not guarantee
--         any clipping will occur, but allows more optimal presentation
--         methods to be used on some platforms.
--
--     -   If set to 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', presentable
--         images associated with the swapchain will own all of the pixels
--         they contain.
--
-- __Note__
--
-- Applications /should/ set this value to
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' if they do not expect to read
-- back the content of presentable images before presenting them or after
-- reacquiring them, and if their pixel shaders do not have any side
-- effects that require them to run for all pixels in the presentable
-- image.
--
-- -   @oldSwapchain@ is
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', or the existing
--     non-retired swapchain currently associated with @surface@. Providing
--     a valid @oldSwapchain@ /may/ aid in the resource reuse, and also
--     allows the application to still present any images that are already
--     acquired from it.
--
-- Upon calling
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'
-- with an @oldSwapchain@ that is not
-- 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @oldSwapchain@ is
-- retired — even if creation of the new swapchain fails. The new swapchain
-- is created in the non-retired state whether or not @oldSwapchain@ is
-- 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'.
--
-- Upon calling
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR'
-- with an @oldSwapchain@ that is not
-- 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', any images from
-- @oldSwapchain@ that are not acquired by the application /may/ be freed
-- by the implementation, which /may/ occur even if creation of the new
-- swapchain fails. The application /can/ destroy @oldSwapchain@ to free
-- all memory associated with @oldSwapchain@.
--
-- __Note__
--
-- Multiple retired swapchains /can/ be associated with the same
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' through
-- multiple uses of @oldSwapchain@ that outnumber calls to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkDestroySwapchainKHR'.
--
-- After @oldSwapchain@ is retired, the application /can/ pass to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR' any
-- images it had already acquired from @oldSwapchain@. E.g., an application
-- may present an image from the old swapchain before an image from the new
-- swapchain is ready to be presented. As usual,
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR' /may/
-- fail if @oldSwapchain@ has entered a state that causes
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_ERROR_OUT_OF_DATE_KHR'
-- to be returned.
--
-- The application /can/ continue to use a shared presentable image
-- obtained from @oldSwapchain@ until a presentable image is acquired from
-- the new swapchain, as long as it has not entered a state that causes it
-- to return
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_ERROR_OUT_OF_DATE_KHR'.
--
-- == Valid Usage
--
-- -   @surface@ /must/ be a surface that is supported by the device as
--     determined using
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceSupportKHR'
--
-- -   @minImageCount@ /must/ be greater than or equal to the value
--     returned in the @minImageCount@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   @minImageCount@ /must/ be less than or equal to the value returned
--     in the @maxImageCount@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface if the returned @maxImageCount@ is not zero
--
-- -   @minImageCount@ /must/ be @1@ if @presentMode@ is either
--     'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR'
--
-- -   @imageFormat@ and @imageColorSpace@ /must/ match the @format@ and
--     @colorSpace@ members, respectively, of one of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceFormatKHR'
--     structures returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceFormatsKHR'
--     for the surface
--
-- -   @imageExtent@ /must/ be between @minImageExtent@ and
--     @maxImageExtent@, inclusive, where @minImageExtent@ and
--     @maxImageExtent@ are members of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   @imageExtent@ members @width@ and @height@ /must/ both be non-zero
--
-- -   @imageArrayLayers@ /must/ be greater than @0@ and less than or equal
--     to the @maxImageArrayLayers@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   If @presentMode@ is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_PRESENT_MODE_IMMEDIATE_KHR',
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_PRESENT_MODE_MAILBOX_KHR',
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_PRESENT_MODE_FIFO_KHR'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_PRESENT_MODE_FIFO_RELAXED_KHR',
--     @imageUsage@ /must/ be a subset of the supported usage flags present
--     in the @supportedUsageFlags@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR'
--     for @surface@
--
-- -   If @presentMode@ is
--     'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR',
--     @imageUsage@ /must/ be a subset of the supported usage flags present
--     in the @sharedPresentSupportedUsageFlags@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VkSharedPresentSurfaceCapabilitiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.vkGetPhysicalDeviceSurfaceCapabilities2KHR'
--     for @surface@
--
-- -   If @imageSharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT',
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   If @imageSharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT',
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   If @imageSharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT', each
--     element of @pQueueFamilyIndices@ /must/ be unique and /must/ be less
--     than @pQueueFamilyPropertyCount@ returned by either
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   @preTransform@ /must/ be one of the bits present in the
--     @supportedTransforms@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   @compositeAlpha@ /must/ be one of the bits present in the
--     @supportedCompositeAlpha@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR'
--     for the surface
--
-- -   @presentMode@ /must/ be one of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR'
--     values returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfacePresentModesKHR'
--     for the surface
--
-- -   If the logical device was created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkDeviceGroupDeviceCreateInfo'::@physicalDeviceCount@
--     equal to 1, @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_SWAPCHAIN_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR'
--
-- -   If @oldSwapchain@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @oldSwapchain@
--     /must/ be a non-retired swapchain associated with native window
--     referred to by @surface@
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#swapchain-wsi-image-create-info implied image creation parameters>
--     of the swapchain /must/ be supported as reported by
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain_mutable_format.VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR'
--     then the @pNext@ chain /must/ contain an instance of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR'
--     with a @viewFormatCount@ greater than zero and @pViewFormats@ /must/
--     have an element equal to @imageFormat@
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_SWAPCHAIN_CREATE_PROTECTED_BIT_KHR',
--     then
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface_protected_capabilities.VkSurfaceProtectedCapabilitiesKHR'::@supportsProtected@
--     /must/ be 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' in the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface_protected_capabilities.VkSurfaceProtectedCapabilitiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.vkGetPhysicalDeviceSurfaceCapabilities2KHR'
--     for @surface@
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceFullScreenExclusiveInfoEXT'
--     with its @fullScreenExclusive@ member set to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT',
--     and @surface@ was created using
--     'Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface.vkCreateWin32SurfaceKHR',
--     an instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceFullScreenExclusiveWin32InfoEXT'
--     /must/ be present in the @pNext@ chain
--
-- Unresolved directive in VkSwapchainCreateInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkSwapchainCreateInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data SwapchainCreateInfoKHR = SwapchainCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "SwapchainCreateInfoKHR" "pNext"
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
  -- Length valued member elided
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

-- | A function to temporarily allocate memory for a 'VkSwapchainCreateInfoKHR' and
-- marshal a 'SwapchainCreateInfoKHR' into it. The 'VkSwapchainCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSwapchainCreateInfoKHR :: SwapchainCreateInfoKHR -> (VkSwapchainCreateInfoKHR -> IO a) -> IO a
withCStructSwapchainCreateInfoKHR marshalled cont = withVec (&) (queueFamilyIndices (marshalled :: SwapchainCreateInfoKHR)) (\pPQueueFamilyIndices -> withCStructExtent2D (imageExtent (marshalled :: SwapchainCreateInfoKHR)) (\imageExtent'' -> maybeWith withSomeVkStruct (next (marshalled :: SwapchainCreateInfoKHR)) (\pPNext -> cont (VkSwapchainCreateInfoKHR VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR pPNext (flags (marshalled :: SwapchainCreateInfoKHR)) (surface (marshalled :: SwapchainCreateInfoKHR)) (minImageCount (marshalled :: SwapchainCreateInfoKHR)) (imageFormat (marshalled :: SwapchainCreateInfoKHR)) (imageColorSpace (marshalled :: SwapchainCreateInfoKHR)) imageExtent'' (imageArrayLayers (marshalled :: SwapchainCreateInfoKHR)) (imageUsage (marshalled :: SwapchainCreateInfoKHR)) (imageSharingMode (marshalled :: SwapchainCreateInfoKHR)) (fromIntegral (Data.Vector.length (queueFamilyIndices (marshalled :: SwapchainCreateInfoKHR)))) pPQueueFamilyIndices (preTransform (marshalled :: SwapchainCreateInfoKHR)) (compositeAlpha (marshalled :: SwapchainCreateInfoKHR)) (presentMode (marshalled :: SwapchainCreateInfoKHR)) (boolToBool32 (clipped (marshalled :: SwapchainCreateInfoKHR))) (oldSwapchain (marshalled :: SwapchainCreateInfoKHR))))))

-- | A function to read a 'VkSwapchainCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'SwapchainCreateInfoKHR'.
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
                                Data.Vector.empty
                                zero
                                zero
                                zero
                                False
                                zero


-- | VkSwapchainKHR - Opaque handle to a swapchain object
--
-- = Description
--
-- A swapchain is an abstraction for an array of presentable images that
-- are associated with a surface. The presentable images are represented by
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' objects created by
-- the platform. One image (which /can/ be an array image for
-- multiview\/stereoscopic-3D surfaces) is displayed at a time, but
-- multiple images /can/ be queued for presentation. An application renders
-- to the image, and then queues the image for presentation to the surface.
--
-- A native window /cannot/ be associated with more than one non-retired
-- swapchain at a time. Further, swapchains /cannot/ be created for native
-- windows that have a non-Vulkan graphics API surface associated with
-- them.
--
-- __Note__
--
-- The presentation engine is an abstraction for the platform’s compositor
-- or display engine.
--
-- The presentation engine /may/ be synchronous or asynchronous with
-- respect to the application and\/or logical device.
--
-- Some implementations /may/ use the device’s graphics queue or dedicated
-- presentation hardware to perform presentation.
--
-- The presentable images of a swapchain are owned by the presentation
-- engine. An application /can/ acquire use of a presentable image from the
-- presentation engine. Use of a presentable image /must/ occur only after
-- the image is returned by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR',
-- and before it is presented by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR'. This
-- includes transitioning the image layout and rendering commands.
--
-- An application /can/ acquire use of a presentable image with
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR'.
-- After acquiring a presentable image and before modifying it, the
-- application /must/ use a synchronization primitive to ensure that the
-- presentation engine has finished reading from the image. The application
-- /can/ then transition the image’s layout, queue rendering commands to
-- it, etc. Finally, the application presents the image with
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR', which
-- releases the acquisition of the image.
--
-- The presentation engine controls the order in which presentable images
-- are acquired for use by the application.
--
-- __Note__
--
-- This allows the platform to handle situations which require out-of-order
-- return of images after presentation. At the same time, it allows the
-- application to generate command buffers referencing all of the images in
-- the swapchain at initialization time, rather than in its main loop.
--
-- = See Also
--
-- No cross-references are available
type SwapchainKHR = VkSwapchainKHR


-- | vkAcquireNextImage2KHR - Retrieve the index of the next available
-- presentable image
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @pAcquireInfo@ is a pointer to a structure of type
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkAcquireNextImageInfoKHR'
--     containing parameters of the acquire.
--
-- -   @pImageIndex@ is a pointer to a @uint32_t@ that is set to the index
--     of the next image to use.
--
-- == Valid Usage
--
-- -   If the number of currently acquired images is greater than the
--     difference between the number of images in the @swapchain@ member of
--     @pAcquireInfo@ and the value of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'::@minImageCount@
--     as returned by a call to
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.vkGetPhysicalDeviceSurfaceCapabilities2KHR'
--     with the @surface@ used to create @swapchain@, the @timeout@ member
--     of @pAcquireInfo@ /must/ not be @UINT64_MAX@
--
-- Unresolved directive in vkAcquireNextImage2KHR.txt -
-- include::{generated}\/validity\/protos\/vkAcquireNextImage2KHR.txt[]
--
-- = See Also
--
-- No cross-references are available
acquireNextImage2KHR :: Device ->  AcquireNextImageInfoKHR ->  IO (VkResult, Word32)
acquireNextImage2KHR = \(Device device' commandTable) -> \acquireInfo' -> alloca (\pImageIndex' -> (\marshalled -> withCStructAcquireNextImageInfoKHR marshalled . flip with) acquireInfo' (\pAcquireInfo' -> vkAcquireNextImage2KHR commandTable device' pAcquireInfo' pImageIndex' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pImageIndex'))))


-- | vkAcquireNextImageKHR - Retrieve the index of the next available
-- presentable image
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the non-retired swapchain from which an image is
--     being acquired.
--
-- -   @timeout@ specifies how long the function waits, in nanoseconds, if
--     no image is available.
--
-- -   @semaphore@ is 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--     or a semaphore to signal.
--
-- -   @fence@ is 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' or a
--     fence to signal.
--
-- -   @pImageIndex@ is a pointer to a @uint32_t@ that is set to the index
--     of the next image to use (i.e. an index into the array of images
--     returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetSwapchainImagesKHR').
--
-- == Valid Usage
--
-- -   @swapchain@ /must/ not be in the retired state
--
-- -   If @semaphore@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/ be
--     unsignaled
--
-- -   If @semaphore@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/ not
--     have any uncompleted signal or wait operations pending
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' it /must/ be
--     unsignaled and /must/ not be associated with any other queue command
--     that has not yet completed execution on that queue
--
-- -   @semaphore@ and @fence@ /must/ not both be equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If the number of currently acquired images is greater than the
--     difference between the number of images in @swapchain@ and the value
--     of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR'::@minImageCount@
--     as returned by a call to
--     'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.vkGetPhysicalDeviceSurfaceCapabilities2KHR'
--     with the @surface@ used to create @swapchain@, @timeout@ /must/ not
--     be @UINT64_MAX@
--
-- Unresolved directive in vkAcquireNextImageKHR.txt -
-- include::{generated}\/validity\/protos\/vkAcquireNextImageKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
acquireNextImageKHR :: Device ->  SwapchainKHR ->  Word64 ->  Semaphore ->  Fence ->  IO (VkResult, Word32)
acquireNextImageKHR = \(Device device' commandTable) -> \swapchain' -> \timeout' -> \semaphore' -> \fence' -> alloca (\pImageIndex' -> vkAcquireNextImageKHR commandTable device' swapchain' timeout' semaphore' fence' pImageIndex' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pImageIndex')))


-- | vkCreateSwapchainKHR - Create a swapchain
--
-- = Parameters
--
-- -   @device@ is the device to create the swapchain for.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
--     structure specifying the parameters of the created swapchain.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     swapchain object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pSwapchain@ is a pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     handle in which the created swapchain object will be returned.
--
-- = Description
--
-- If the @oldSwapchain@ parameter of @pCreateInfo@ is a valid swapchain,
-- which has exclusive full-screen access, that access is released from
-- @oldSwapchain@. If the command succeeds in this case, the newly created
-- swapchain will automatically acquire exclusive full-screen access from
-- @oldSwapchain@.
--
-- __Note__
--
-- This implicit transfer is intended to avoid exiting and entering
-- full-screen exclusive mode, which may otherwise cause unwanted visual
-- updates to the display.
--
-- In some cases, swapchain creation /may/ fail if exclusive full-screen
-- mode is requested for application control, but for some
-- implementation-specific reason exclusive full-screen access is
-- unavailable for the particular combination of parameters provided. If
-- this occurs,
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED' will be
-- returned.
--
-- __Note__
--
-- In particular, it will fail if the @imageExtent@ member of @pCreateInfo@
-- does not match the extents of the monitor. Other reasons for failure may
-- include the app not being set as high-dpi aware, or if the physical
-- device and monitor are not compatible in this mode.
--
-- Unresolved directive in vkCreateSwapchainKHR.txt -
-- include::{generated}\/validity\/protos\/vkCreateSwapchainKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
createSwapchainKHR :: Device ->  SwapchainCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SwapchainKHR)
createSwapchainKHR = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pSwapchain' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructSwapchainCreateInfoKHR marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateSwapchainKHR commandTable device' pCreateInfo' pAllocator pSwapchain' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSwapchain')))))


-- | vkDestroySwapchainKHR - Destroy a swapchain object
--
-- = Parameters
--
-- -   @device@ is the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' associated
--     with @swapchain@.
--
-- -   @swapchain@ is the swapchain to destroy.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     swapchain object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- = Description
--
-- The application /must/ not destroy a swapchain until after completion of
-- all outstanding operations on images that were acquired from the
-- swapchain. @swapchain@ and all associated
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handles are
-- destroyed, and /must/ not be acquired or used any more by the
-- application. The memory of each
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' will only be freed
-- after that image is no longer used by the presentation engine. For
-- example, if one image of the swapchain is being displayed in a window,
-- the memory for that image /may/ not be freed until the window is
-- destroyed, or another swapchain is created for the window. Destroying
-- the swapchain does not invalidate the parent
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR', and a new
-- swapchain /can/ be created with it.
--
-- When a swapchain associated with a display surface is destroyed, if the
-- image most recently presented to the display surface is from the
-- swapchain being destroyed, then either any display resources modified by
-- presenting images from any swapchain associated with the display surface
-- /must/ be reverted by the implementation to their state prior to the
-- first present performed on one of these swapchains, or such resources
-- /must/ be left in their current state.
--
-- If @swapchain@ has exclusive full-screen access, it is released before
-- the swapchain is destroyed.
--
-- == Valid Usage
--
-- -   All uses of presentable images acquired from @swapchain@ /must/ have
--     completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @swapchain@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @swapchain@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroySwapchainKHR.txt -
-- include::{generated}\/validity\/protos\/vkDestroySwapchainKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
destroySwapchainKHR :: Device ->  SwapchainKHR ->  Maybe AllocationCallbacks ->  IO ()
destroySwapchainKHR = \(Device device' commandTable) -> \swapchain' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroySwapchainKHR commandTable device' swapchain' pAllocator *> (pure ()))


-- | vkGetDeviceGroupPresentCapabilitiesKHR - Query present capabilities from
-- other physical devices
--
-- = Parameters
--
-- -   @device@ is the logical device.
--
-- -   @pDeviceGroupPresentCapabilities@ is a pointer to a structure of
--     type
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR'
--     that is filled with the logical device’s capabilities.
--
-- = Description
--
-- Unresolved directive in vkGetDeviceGroupPresentCapabilitiesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetDeviceGroupPresentCapabilitiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getDeviceGroupPresentCapabilitiesKHR :: Device ->  IO (DeviceGroupPresentCapabilitiesKHR)
getDeviceGroupPresentCapabilitiesKHR = \(Device device' commandTable) -> alloca (\pDeviceGroupPresentCapabilities' -> vkGetDeviceGroupPresentCapabilitiesKHR commandTable device' pDeviceGroupPresentCapabilities' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructDeviceGroupPresentCapabilitiesKHR <=< peek) pDeviceGroupPresentCapabilities')))


-- | vkGetDeviceGroupSurfacePresentModesKHR - Query present capabilities for
-- a surface
--
-- = Parameters
--
-- -   @device@ is the logical device.
--
-- -   @surface@ is the surface.
--
-- -   @pModes@ is a pointer to a value of type
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagsKHR'
--     that is filled with the supported device group present modes for the
--     surface.
--
-- = Description
--
-- The modes returned by this command are not invariant, and /may/ change
-- in response to the surface being moved, resized, or occluded. These
-- modes /must/ be a subset of the modes returned by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetDeviceGroupPresentCapabilitiesKHR'.
--
-- Unresolved directive in vkGetDeviceGroupSurfacePresentModesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetDeviceGroupSurfacePresentModesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getDeviceGroupSurfacePresentModesKHR :: Device ->  SurfaceKHR ->  IO (DeviceGroupPresentModeFlagsKHR)
getDeviceGroupSurfacePresentModesKHR = \(Device device' commandTable) -> \surface' -> alloca (\pModes' -> vkGetDeviceGroupSurfacePresentModesKHR commandTable device' surface' pModes' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pModes')))


-- | vkGetPhysicalDevicePresentRectanglesKHR - Query present rectangles for a
-- surface on a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @surface@ is the surface.
--
-- -   @pRectCount@ is a pointer to an integer related to the number of
--     rectangles available or queried, as described below.
--
-- -   @pRects@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures.
--
-- = Description
--
-- If @pRects@ is @NULL@, then the number of rectangles used when
-- presenting the given @surface@ is returned in @pRectCount@. Otherwise,
-- @pRectCount@ /must/ point to a variable set by the user to the number of
-- elements in the @pRects@ array, and on return the variable is
-- overwritten with the number of structures actually written to @pRects@.
-- If the value of @pRectCount@ is less than the number of rectangles, at
-- most @pRectCount@ structures will be written. If @pRectCount@ is smaller
-- than the number of rectangles used for the given @surface@,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- The values returned by this command are not invariant, and /may/ change
-- in response to the surface being moved, resized, or occluded.
--
-- The rectangles returned by this command /must/ not overlap.
--
-- Unresolved directive in vkGetPhysicalDevicePresentRectanglesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDevicePresentRectanglesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getNumPhysicalDevicePresentRectanglesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (VkResult, Word32)
getNumPhysicalDevicePresentRectanglesKHR = \(PhysicalDevice physicalDevice' commandTable) -> \surface' -> alloca (\pRectCount' -> vkGetPhysicalDevicePresentRectanglesKHR commandTable physicalDevice' surface' pRectCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pRectCount')))

-- | vkGetPhysicalDevicePresentRectanglesKHR - Query present rectangles for a
-- surface on a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @surface@ is the surface.
--
-- -   @pRectCount@ is a pointer to an integer related to the number of
--     rectangles available or queried, as described below.
--
-- -   @pRects@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures.
--
-- = Description
--
-- If @pRects@ is @NULL@, then the number of rectangles used when
-- presenting the given @surface@ is returned in @pRectCount@. Otherwise,
-- @pRectCount@ /must/ point to a variable set by the user to the number of
-- elements in the @pRects@ array, and on return the variable is
-- overwritten with the number of structures actually written to @pRects@.
-- If the value of @pRectCount@ is less than the number of rectangles, at
-- most @pRectCount@ structures will be written. If @pRectCount@ is smaller
-- than the number of rectangles used for the given @surface@,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to indicate that not all
-- the available values were returned.
--
-- The values returned by this command are not invariant, and /may/ change
-- in response to the surface being moved, resized, or occluded.
--
-- The rectangles returned by this command /must/ not overlap.
--
-- Unresolved directive in vkGetPhysicalDevicePresentRectanglesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDevicePresentRectanglesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDevicePresentRectanglesKHR :: PhysicalDevice ->  SurfaceKHR ->  Word32 ->  IO (VkResult, Vector Rect2D)
getPhysicalDevicePresentRectanglesKHR = \(PhysicalDevice physicalDevice' commandTable) -> \surface' -> \rectCount' -> allocaArray (fromIntegral rectCount') (\pRects' -> with rectCount' (\pRectCount' -> vkGetPhysicalDevicePresentRectanglesKHR commandTable physicalDevice' surface' pRectCount' pRects' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p -> fromCStructRect2D <=< peekElemOff p) pRects') =<< (fromIntegral <$> (peek pRectCount')))))))
-- | Returns all the values available from 'getPhysicalDevicePresentRectanglesKHR'.
getAllPhysicalDevicePresentRectanglesKHR :: PhysicalDevice ->  SurfaceKHR ->  IO (Vector Rect2D)
getAllPhysicalDevicePresentRectanglesKHR physicalDevice' surface' =
  snd <$> getNumPhysicalDevicePresentRectanglesKHR physicalDevice' surface'
    >>= \num -> snd <$> getPhysicalDevicePresentRectanglesKHR physicalDevice' surface' num



-- | vkGetSwapchainImagesKHR - Obtain the array of presentable images
-- associated with a swapchain
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to query.
--
-- -   @pSwapchainImageCount@ is a pointer to an integer related to the
--     number of presentable images available or queried, as described
--     below.
--
-- -   @pSwapchainImages@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handles.
--
-- = Description
--
-- If @pSwapchainImages@ is @NULL@, then the number of presentable images
-- for @swapchain@ is returned in @pSwapchainImageCount@. Otherwise,
-- @pSwapchainImageCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pSwapchainImages@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pSwapchainImages@. If the value of @pSwapchainImageCount@ is less
-- than the number of presentable images for @swapchain@, at most
-- @pSwapchainImageCount@ structures will be written. If
-- @pSwapchainImageCount@ is smaller than the number of presentable images
-- for @swapchain@, 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be
-- returned instead of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to
-- indicate that not all the available values were returned.
--
-- Unresolved directive in vkGetSwapchainImagesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetSwapchainImagesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getNumSwapchainImagesKHR :: Device ->  SwapchainKHR ->  IO (VkResult, Word32)
getNumSwapchainImagesKHR = \(Device device' commandTable) -> \swapchain' -> alloca (\pSwapchainImageCount' -> vkGetSwapchainImagesKHR commandTable device' swapchain' pSwapchainImageCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pSwapchainImageCount')))

-- | vkGetSwapchainImagesKHR - Obtain the array of presentable images
-- associated with a swapchain
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to query.
--
-- -   @pSwapchainImageCount@ is a pointer to an integer related to the
--     number of presentable images available or queried, as described
--     below.
--
-- -   @pSwapchainImages@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handles.
--
-- = Description
--
-- If @pSwapchainImages@ is @NULL@, then the number of presentable images
-- for @swapchain@ is returned in @pSwapchainImageCount@. Otherwise,
-- @pSwapchainImageCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pSwapchainImages@ array, and on return the
-- variable is overwritten with the number of structures actually written
-- to @pSwapchainImages@. If the value of @pSwapchainImageCount@ is less
-- than the number of presentable images for @swapchain@, at most
-- @pSwapchainImageCount@ structures will be written. If
-- @pSwapchainImageCount@ is smaller than the number of presentable images
-- for @swapchain@, 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be
-- returned instead of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' to
-- indicate that not all the available values were returned.
--
-- Unresolved directive in vkGetSwapchainImagesKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetSwapchainImagesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getSwapchainImagesKHR :: Device ->  SwapchainKHR ->  Word32 ->  IO (VkResult, Vector Image)
getSwapchainImagesKHR = \(Device device' commandTable) -> \swapchain' -> \swapchainImageCount' -> allocaArray (fromIntegral swapchainImageCount') (\pSwapchainImages' -> with swapchainImageCount' (\pSwapchainImageCount' -> vkGetSwapchainImagesKHR commandTable device' swapchain' pSwapchainImageCount' pSwapchainImages' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM (peekElemOff pSwapchainImages') =<< (fromIntegral <$> (peek pSwapchainImageCount')))))))
-- | Returns all the values available from 'getSwapchainImagesKHR'.
getAllSwapchainImagesKHR :: Device ->  SwapchainKHR ->  IO (Vector Image)
getAllSwapchainImagesKHR device' swapchain' =
  snd <$> getNumSwapchainImagesKHR device' swapchain'
    >>= \num -> snd <$> getSwapchainImagesKHR device' swapchain' num



-- | vkQueuePresentKHR - Queue an image for presentation
--
-- = Parameters
--
-- -   @queue@ is a queue that is capable of presentation to the target
--     surface’s platform on the same device as the image’s swapchain.
--
-- -   @pPresentInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR'
--     structure specifying the parameters of the presentation.
--
-- = Description
--
-- __Note__
--
-- There is no requirement for an application to present images in the same
-- order that they were acquired - applications can arbitrarily present any
-- image that is currently acquired.
--
-- == Valid Usage
--
-- -   Each element of @pSwapchains@ member of @pPresentInfo@ /must/ be a
--     swapchain that is created for a surface for which presentation is
--     supported from @queue@ as determined using a call to
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceSupportKHR'
--
-- -   If more than one member of @pSwapchains@ was created from a display
--     surface, all display surfaces referenced that refer to the same
--     display /must/ use the same display mode
--
-- -   When a semaphore unsignal operation defined by the elements of the
--     @pWaitSemaphores@ member of @pPresentInfo@ executes on @queue@, no
--     other queue /must/ be waiting on the same semaphore.
--
-- -   All elements of the @pWaitSemaphores@ member of @pPresentInfo@
--     /must/ be semaphores that are signaled, or have
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>
--     previously submitted for execution.
--
-- Any writes to memory backing the images referenced by the
-- @pImageIndices@ and @pSwapchains@ members of @pPresentInfo@, that are
-- available before
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR' is
-- executed, are automatically made visible to the read access performed by
-- the presentation engine. This automatic visibility operation for an
-- image happens-after the semaphore signal operation, and happens-before
-- the presentation engine accesses the image.
--
-- Queueing an image for presentation defines a set of /queue operations/,
-- including waiting on the semaphores and submitting a presentation
-- request to the presentation engine. However, the scope of this set of
-- queue operations does not include the actual processing of the image by
-- the presentation engine.
--
-- If 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR'
-- fails to enqueue the corresponding set of queue operations, it /may/
-- return 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY' or
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'. If it
-- does, the implementation /must/ ensure that the state and contents of
-- any resources or synchronization primitives referenced is unaffected by
-- the call or its failure.
--
-- If 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR'
-- fails in such a way that the implementation is unable to make that
-- guarantee, the implementation /must/ return
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'.
--
-- However, if the presentation request is rejected by the presentation
-- engine with an error
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_ERROR_OUT_OF_DATE_KHR'
-- or
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR',
-- the set of queue operations are still considered to be enqueued and thus
-- any semaphore to be waited on gets unsignaled when the corresponding
-- queue operation is complete.
--
-- If any @swapchain@ member of @pPresentInfo@ was created with
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
-- will be returned if that swapchain does not have exclusive full-screen
-- access, possibly for implementation-specific reasons outside of the
-- application’s control.
--
-- Unresolved directive in vkQueuePresentKHR.txt -
-- include::{generated}\/validity\/protos\/vkQueuePresentKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
queuePresentKHR :: Queue ->  PresentInfoKHR ->  IO (VkResult)
queuePresentKHR = \(Queue queue' commandTable) -> \presentInfo' -> (\marshalled -> withCStructPresentInfoKHR marshalled . flip with) presentInfo' (\pPresentInfo' -> vkQueuePresentKHR commandTable queue' pPresentInfo' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ret)))

-- | A safe wrapper for 'createSwapchainKHR' and 'destroySwapchainKHR' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withSwapchainKHR
  :: Device -> SwapchainCreateInfoKHR -> Maybe (AllocationCallbacks) -> (SwapchainKHR -> IO a) -> IO a
withSwapchainKHR device swapchainCreateInfoKHR allocationCallbacks = bracket
  (createSwapchainKHR device swapchainCreateInfoKHR allocationCallbacks)
  (\o -> destroySwapchainKHR device o allocationCallbacks)
