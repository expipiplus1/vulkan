{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( VkSharedPresentSurfaceCapabilitiesKHR(..)
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
  , vkGetSwapchainStatusKHR
#endif
  , FN_vkGetSwapchainStatusKHR
  , PFN_vkGetSwapchainStatusKHR
  , pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
  , pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  , pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  , VkImageUsageFlags
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkPresentModeKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkSharedPresentSurfaceCapabilitiesKHR - structure describing
-- capabilities of a surface for shared presentation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkSharedPresentSurfaceCapabilitiesKHR = VkSharedPresentSurfaceCapabilitiesKHR
  { -- | @sType@ /must/ be
  -- @VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR@
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @sharedPresentSupportedUsageFlags@ is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'
  -- representing the ways the application /can/ use the shared presentable
  -- image from a swapchain created with
  -- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkPresentModeKHR' set to
  -- @VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR@ or
  -- @VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR@ for the surface on the
  -- specified device. @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@ /must/ be
  -- included in the set but implementations /may/ support additional usages.
  vkSharedPresentSupportedUsageFlags :: VkImageUsageFlags
  }
  deriving (Eq, Show)

instance Storable VkSharedPresentSurfaceCapabilitiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkSharedPresentSurfaceCapabilitiesKHR <$> peek (ptr `plusPtr` 0)
                                                   <*> peek (ptr `plusPtr` 8)
                                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkSharedPresentSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkSharedPresentSurfaceCapabilitiesKHR))
                *> poke (ptr `plusPtr` 16) (vkSharedPresentSupportedUsageFlags (poked :: VkSharedPresentSurfaceCapabilitiesKHR))

instance Zero VkSharedPresentSurfaceCapabilitiesKHR where
  zero = VkSharedPresentSurfaceCapabilitiesKHR zero
                                               zero
                                               zero
#if defined(EXPOSE_STATIC_EXTENSION_COMMANDS)
-- | vkGetSwapchainStatusKHR - Get a swapchain’s status
--
-- = Parameters
--
-- -   @device@ is the device associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain to query.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @swapchain@ /must/ be a valid @VkSwapchainKHR@ handle
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same @VkInstance@
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
--     -   @VK_SUBOPTIMAL_KHR@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
--     -   @VK_ERROR_OUT_OF_DATE_KHR@
--
--     -   @VK_ERROR_SURFACE_LOST_KHR@
--
-- = See Also
--
-- No cross-references are available
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetSwapchainStatusKHR" vkGetSwapchainStatusKHR :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult

#endif
type FN_vkGetSwapchainStatusKHR = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
type PFN_vkGetSwapchainStatusKHR = FunPtr FN_vkGetSwapchainStatusKHR
-- | @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@ is valid only for shared
-- presentable images, and /must/ be used for any usage the image supports.
pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR :: VkImageLayout
pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR = VkImageLayout 1000111000
-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME"
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME = "VK_KHR_shared_presentable_image"
-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION"
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1
-- | @VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR@ specifies that the
-- presentation engine and application have concurrent access to a single
-- image, which is referred to as a /shared presentable image/. The
-- presentation engine periodically updates the current image on its
-- regular refresh cycle. The application is only required to make one
-- initial presentation request, after which the presentation engine /must/
-- update the current image without any need for further presentation
-- requests. The application /can/ indicate the image contents have been
-- updated by making a presentation request, but this does not guarantee
-- the timing of when it will be updated. This mode /may/ result in visible
-- tearing if rendering to the image is not timed correctly.
pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR = VkPresentModeKHR 1000111001
-- | @VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR@ specifies that the
-- presentation engine and application have concurrent access to a single
-- image, which is referred to as a /shared presentable image/. The
-- presentation engine is only required to update the current image after a
-- new presentation request is received. Therefore the application /must/
-- make a presentation request whenever an update is required. However, the
-- presentation engine /may/ update the current image at any point, meaning
-- this mode /may/ result in visible tearing.
pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR = VkPresentModeKHR 1000111000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR"
pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR = VkStructureType 1000111000
