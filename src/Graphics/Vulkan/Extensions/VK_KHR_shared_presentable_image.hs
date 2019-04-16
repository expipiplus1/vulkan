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
import qualified Graphics.Vulkan.C.Dynamic
  ( getSwapchainStatusKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( VkSharedPresentSurfaceCapabilitiesKHR(..)
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


-- No documentation found for TopLevel "SharedPresentSurfaceCapabilitiesKHR"
data SharedPresentSurfaceCapabilitiesKHR = SharedPresentSurfaceCapabilitiesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "SharedPresentSurfaceCapabilitiesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SharedPresentSurfaceCapabilitiesKHR" "sharedPresentSupportedUsageFlags"
  vkSharedPresentSupportedUsageFlags :: ImageUsageFlags
  }
  deriving (Show, Eq)
withCStructSharedPresentSurfaceCapabilitiesKHR :: SharedPresentSurfaceCapabilitiesKHR -> (VkSharedPresentSurfaceCapabilitiesKHR -> IO a) -> IO a
withCStructSharedPresentSurfaceCapabilitiesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: SharedPresentSurfaceCapabilitiesKHR)) (\pPNext -> cont (VkSharedPresentSurfaceCapabilitiesKHR VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR pPNext (vkSharedPresentSupportedUsageFlags (from :: SharedPresentSurfaceCapabilitiesKHR))))
fromCStructSharedPresentSurfaceCapabilitiesKHR :: VkSharedPresentSurfaceCapabilitiesKHR -> IO SharedPresentSurfaceCapabilitiesKHR
fromCStructSharedPresentSurfaceCapabilitiesKHR c = SharedPresentSurfaceCapabilitiesKHR <$> -- Univalued Member elided
                                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSharedPresentSurfaceCapabilitiesKHR)))
                                                                                       <*> pure (vkSharedPresentSupportedUsageFlags (c :: VkSharedPresentSurfaceCapabilitiesKHR))

-- | Wrapper for 'vkGetSwapchainStatusKHR'
getSwapchainStatusKHR :: Device ->  SwapchainKHR ->  IO (VkResult)
getSwapchainStatusKHR = \(Device device commandTable) -> \swapchain -> Graphics.Vulkan.C.Dynamic.getSwapchainStatusKHR commandTable device swapchain >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure r))
