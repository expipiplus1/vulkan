{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
  ( withCStructSharedPresentSurfaceCapabilitiesKHR
  , fromCStructSharedPresentSurfaceCapabilitiesKHR
  , SharedPresentSurfaceCapabilitiesKHR(..)
  , getSwapchainStatusKHR
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  , pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  , pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
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
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
  , pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  )



-- | VkSharedPresentSurfaceCapabilitiesKHR - structure describing
-- capabilities of a surface for shared presentation
--
-- = Description
--
-- Unresolved directive in VkSharedPresentSurfaceCapabilitiesKHR.txt -
-- include::{generated}\/validity\/structs\/VkSharedPresentSurfaceCapabilitiesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
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
-- = Description
--
-- Unresolved directive in vkGetSwapchainStatusKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetSwapchainStatusKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getSwapchainStatusKHR :: Device ->  SwapchainKHR ->  IO (VkResult)
getSwapchainStatusKHR = \(Device device' commandTable) -> \swapchain' -> vkGetSwapchainStatusKHR commandTable device' swapchain' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ret))
