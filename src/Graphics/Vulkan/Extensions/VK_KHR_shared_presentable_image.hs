{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
  ( withCStructSharedPresentSurfaceCapabilitiesKHR
  , fromCStructSharedPresentSurfaceCapabilitiesKHR
  , SharedPresentSurfaceCapabilitiesKHR(..)
  , getSwapchainStatusKHR
  , pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  , pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  , pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  , pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR
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
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( VkSharedPresentSurfaceCapabilitiesKHR(..)
  , vkGetSwapchainStatusKHR
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , ImageUsageFlags
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  )
import Graphics.Vulkan.Core10.Image
  ( pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  )



-- | VkSharedPresentSurfaceCapabilitiesKHR - structure describing
-- capabilities of a surface for shared presentation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data SharedPresentSurfaceCapabilitiesKHR = SharedPresentSurfaceCapabilitiesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "SharedPresentSurfaceCapabilitiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SharedPresentSurfaceCapabilitiesKHR" "sharedPresentSupportedUsageFlags"
  sharedPresentSupportedUsageFlags :: ImageUsageFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSharedPresentSurfaceCapabilitiesKHR' and
-- marshal a 'SharedPresentSurfaceCapabilitiesKHR' into it. The 'VkSharedPresentSurfaceCapabilitiesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSharedPresentSurfaceCapabilitiesKHR :: SharedPresentSurfaceCapabilitiesKHR -> (VkSharedPresentSurfaceCapabilitiesKHR -> IO a) -> IO a
withCStructSharedPresentSurfaceCapabilitiesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: SharedPresentSurfaceCapabilitiesKHR)) (\pPNext -> cont (VkSharedPresentSurfaceCapabilitiesKHR VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR pPNext (sharedPresentSupportedUsageFlags (marshalled :: SharedPresentSurfaceCapabilitiesKHR))))

-- | A function to read a 'VkSharedPresentSurfaceCapabilitiesKHR' and all additional
-- structures in the pointer chain into a 'SharedPresentSurfaceCapabilitiesKHR'.
fromCStructSharedPresentSurfaceCapabilitiesKHR :: VkSharedPresentSurfaceCapabilitiesKHR -> IO SharedPresentSurfaceCapabilitiesKHR
fromCStructSharedPresentSurfaceCapabilitiesKHR c = SharedPresentSurfaceCapabilitiesKHR <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSharedPresentSurfaceCapabilitiesKHR)))
                                                                                       <*> pure (vkSharedPresentSupportedUsageFlags (c :: VkSharedPresentSurfaceCapabilitiesKHR))

instance Zero SharedPresentSurfaceCapabilitiesKHR where
  zero = SharedPresentSurfaceCapabilitiesKHR Nothing
                                             zero



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
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @swapchain@ /must/ be a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
--     handle
--
-- -   Both of @device@, and @swapchain@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_SUBOPTIMAL_KHR'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VK_ERROR_OUT_OF_DATE_KHR'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_SURFACE_LOST_KHR'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainKHR'
getSwapchainStatusKHR :: Device ->  SwapchainKHR ->  IO (VkResult)
getSwapchainStatusKHR = \(Device device' commandTable) -> \swapchain' -> vkGetSwapchainStatusKHR commandTable device' swapchain' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ret))

-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME"
pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME = VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION"
pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION :: Integral a => a
pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
