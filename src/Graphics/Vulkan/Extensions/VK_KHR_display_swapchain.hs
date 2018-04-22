{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_display_swapchain
  ( pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION
  , pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME
  , vkCreateSharedSwapchainsKHR
  , VkDisplayPresentInfoKHR(..)
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkRect2D(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( VkSwapchainCreateInfoKHR(..)
  , VkSwapchainKHR
  )


-- No documentation found for Nested "VkResult" "VK_ERROR_INCOMPATIBLE_DISPLAY_KHR"
pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR :: VkResult
pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR = VkResult (-1000003001)
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR = VkStructureType 1000003000
-- No documentation found for TopLevel "VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION"
pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 9
-- No documentation found for TopLevel "VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME"
pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_display_swapchain"
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
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
--     structures specifying the parameters of the created swapchains.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     swapchain objects when there is no more specific allocator available
--     (see [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)).
--
-- -   @pSwapchains@ is a pointer to an array of @VkSwapchainKHR@ handles
--     in which the created swapchain objects will be returned.
--
-- = Description
--
-- @vkCreateSharedSwapchains@ is similar to
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR',
-- except that it takes an array of @VkSwapchainCreateInfoKHR@ structures,
-- and returns an array of swapchain objects.
--
-- The swapchain creation parameters that affect the properties and number
-- of presentable images /must/ match between all the swapchains. If the
-- displays used by any of the swapchains do not use the same presentable
-- image layout or are incompatible in a way that prevents sharing images,
-- swapchain creation will fail with the result code
-- @VK_ERROR_INCOMPATIBLE_DISPLAY_KHR@. If any error occurs, no swapchains
-- will be created. Images presented to multiple swapchains /must/ be
-- re-acquired from all of them before transitioning away from
-- @VK_IMAGE_LAYOUT_PRESENT_SRC_KHR@. After destroying one or more of the
-- swapchains, the remaining swapchains and the presentable images /can/
-- continue to be used.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @swapchainCount@ valid @VkSwapchainCreateInfoKHR@ structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pSwapchains@ /must/ be a valid pointer to an array of
--     @swapchainCount@ @VkSwapchainKHR@ handles
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
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_INCOMPATIBLE_DISPLAY_KHR@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
--     -   @VK_ERROR_SURFACE_LOST_KHR@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateSharedSwapchainsKHR" vkCreateSharedSwapchainsKHR :: ("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult
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
-- -   If the @persistentContent@ member of the @VkDisplayPropertiesKHR@
--     structure returned by @vkGetPhysicalDeviceDisplayPropertiesKHR@ for
--     the display the present operation targets then @persistent@ /must/
--     be @VK_FALSE@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkDisplayPresentInfoKHR = VkDisplayPresentInfoKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @srcRect@ is a rectangular region of pixels to present. It /must/ be a
  -- subset of the image being presented. If @VkDisplayPresentInfoKHR@ is not
  -- specified, this region will be assumed to be the entire presentable
  -- image.
  vkSrcRect :: VkRect2D
  , -- | @dstRect@ is a rectangular region within the visible region of the
  -- swapchain’s display mode. If @VkDisplayPresentInfoKHR@ is not specified,
  -- this region will be assumed to be the entire visible region of the
  -- visible region of the swapchain’s mode. If the specified rectangle is a
  -- subset of the display mode’s visible region, content from display planes
  -- below the swapchain’s plane will be visible outside the rectangle. If
  -- there are no planes below the swapchain’s, the area outside the
  -- specified rectangle will be black. If portions of the specified
  -- rectangle are outside of the display’s visible region, pixels mapping
  -- only to those portions of the rectangle will be discarded.
  vkDstRect :: VkRect2D
  , -- | @persistent@: If this is @VK_TRUE@, the display engine will enable
  -- buffered mode on displays that support it. This allows the display
  -- engine to stop sending content to the display until a new image is
  -- presented. The display will instead maintain a copy of the last
  -- presented image. This allows less power to be used, but /may/ increase
  -- presentation latency. If @VkDisplayPresentInfoKHR@ is not specified,
  -- persistent mode will not be used.
  vkPersistent :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDisplayPresentInfoKHR where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkDisplayPresentInfoKHR <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 16) (vkSrcRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 32) (vkDstRect (poked :: VkDisplayPresentInfoKHR))
                *> poke (ptr `plusPtr` 48) (vkPersistent (poked :: VkDisplayPresentInfoKHR))
