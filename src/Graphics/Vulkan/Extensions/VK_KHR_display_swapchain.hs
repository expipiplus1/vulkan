{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_display_swapchain
  ( withCStructDisplayPresentInfoKHR
  , fromCStructDisplayPresentInfoKHR
  , DisplayPresentInfoKHR(..)
  , createSharedSwapchainsKHR
  , pattern KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  , pattern KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
  , pattern STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  , pattern ERROR_INCOMPATIBLE_DISPLAY_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( generateM
  , length
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
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( VkDisplayPresentInfoKHR(..)
  , vkCreateSharedSwapchainsKHR
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , fromCStructRect2D
  , withCStructRect2D
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainCreateInfoKHR(..)
  , SwapchainKHR
  , withCStructSwapchainCreateInfoKHR
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_INCOMPATIBLE_DISPLAY_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  )



-- | VkDisplayPresentInfoKHR - Structure describing parameters of a queue
-- presentation to a swapchain
--
-- = Description
--
-- If the extent of the @srcRect@ and @dstRect@ are not equal, the
-- presented pixels will be scaled accordingly.
--
-- == Valid Usage
--
-- -   @srcRect@ /must/ specify a rectangular region that is a subset of
--     the image being presented
--
-- -   @dstRect@ /must/ specify a rectangular region that is a subset of
--     the @visibleRegion@ parameter of the display mode the swapchain
--     being presented uses
--
-- -   If the @persistentContent@ member of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPropertiesKHR'
--     structure returned by
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPropertiesKHR'
--     for the display the present operation targets then @persistent@
--     /must/ be 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain.VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DisplayPresentInfoKHR = DisplayPresentInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "DisplayPresentInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DisplayPresentInfoKHR" "srcRect"
  srcRect :: Rect2D
  , -- No documentation found for Nested "DisplayPresentInfoKHR" "dstRect"
  dstRect :: Rect2D
  , -- No documentation found for Nested "DisplayPresentInfoKHR" "persistent"
  persistent :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDisplayPresentInfoKHR' and
-- marshal a 'DisplayPresentInfoKHR' into it. The 'VkDisplayPresentInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDisplayPresentInfoKHR :: DisplayPresentInfoKHR -> (VkDisplayPresentInfoKHR -> IO a) -> IO a
withCStructDisplayPresentInfoKHR marshalled cont = withCStructRect2D (dstRect (marshalled :: DisplayPresentInfoKHR)) (\dstRect'' -> withCStructRect2D (srcRect (marshalled :: DisplayPresentInfoKHR)) (\srcRect'' -> maybeWith withSomeVkStruct (next (marshalled :: DisplayPresentInfoKHR)) (\pPNext -> cont (VkDisplayPresentInfoKHR VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR pPNext srcRect'' dstRect'' (boolToBool32 (persistent (marshalled :: DisplayPresentInfoKHR)))))))

-- | A function to read a 'VkDisplayPresentInfoKHR' and all additional
-- structures in the pointer chain into a 'DisplayPresentInfoKHR'.
fromCStructDisplayPresentInfoKHR :: VkDisplayPresentInfoKHR -> IO DisplayPresentInfoKHR
fromCStructDisplayPresentInfoKHR c = DisplayPresentInfoKHR <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDisplayPresentInfoKHR)))
                                                           <*> (fromCStructRect2D (vkSrcRect (c :: VkDisplayPresentInfoKHR)))
                                                           <*> (fromCStructRect2D (vkDstRect (c :: VkDisplayPresentInfoKHR)))
                                                           <*> pure (bool32ToBool (vkPersistent (c :: VkDisplayPresentInfoKHR)))

instance Zero DisplayPresentInfoKHR where
  zero = DisplayPresentInfoKHR Nothing
                               zero
                               zero
                               False



-- | vkCreateSharedSwapchainsKHR - Create multiple swapchains that share
-- presentable images
--
-- = Parameters
--
-- -   @device@ is the device to create the swapchains for.
--
-- -   @swapchainCount@ is the number of swapchains to create.
--
-- -   @pCreateInfos@ is a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
--     structures specifying the parameters of the created swapchains.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     swapchain objects when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pSwapchains@ is a pointer to an array of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     handles in which the created swapchain objects will be returned.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain.vkCreateSharedSwapchainsKHR'
-- is similar to
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR',
-- except that it takes an array of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
-- structures, and returns an array of swapchain objects.
--
-- The swapchain creation parameters that affect the properties and number
-- of presentable images /must/ match between all the swapchains. If the
-- displays used by any of the swapchains do not use the same presentable
-- image layout or are incompatible in a way that prevents sharing images,
-- swapchain creation will fail with the result code
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain.VK_ERROR_INCOMPATIBLE_DISPLAY_KHR'.
-- If any error occurs, no swapchains will be created. Images presented to
-- multiple swapchains /must/ be re-acquired from all of them before
-- transitioning away from
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_IMAGE_LAYOUT_PRESENT_SRC_KHR'.
-- After destroying one or more of the swapchains, the remaining swapchains
-- and the presentable images /can/ continue to be used.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
--     structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pSwapchains@ /must/ be a valid pointer to an array of
--     @swapchainCount@
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     handles
--
-- -   @swapchainCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @pCreateInfos@[].surface /must/ be externally
--     synchronized
--
-- -   Host access to @pCreateInfos@[].oldSwapchain /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain.VK_ERROR_INCOMPATIBLE_DISPLAY_KHR'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
createSharedSwapchainsKHR :: Device ->  Vector SwapchainCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (Vector SwapchainKHR)
createSharedSwapchainsKHR = \(Device device' commandTable) -> \createInfos' -> \allocator -> allocaArray ((Data.Vector.length createInfos')) (\pSwapchains' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> withVec withCStructSwapchainCreateInfoKHR createInfos' (\pCreateInfos' -> vkCreateSharedSwapchainsKHR commandTable device' (fromIntegral $ Data.Vector.length createInfos') pCreateInfos' pAllocator pSwapchains' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((Data.Vector.generateM ((Data.Vector.length createInfos')) (peekElemOff pSwapchains')))))))

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME"
pattern KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME = VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION"
pattern KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION :: Integral a => a
pattern KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
