{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  SharedPresentSurfaceCapabilitiesKHR(..)
  , 
#endif
  getSwapchainStatusKHR
  , pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  , pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  , pattern PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  , pattern PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( vkGetSwapchainStatusKHR
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ImageUsageFlags
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( SwapchainKHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
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



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSharedPresentSurfaceCapabilitiesKHR"
data SharedPresentSurfaceCapabilitiesKHR = SharedPresentSurfaceCapabilitiesKHR
  { -- No documentation found for Nested "SharedPresentSurfaceCapabilitiesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SharedPresentSurfaceCapabilitiesKHR" "sharedPresentSupportedUsageFlags"
  sharedPresentSupportedUsageFlags :: ImageUsageFlags
  }
  deriving (Show, Eq)

instance Zero SharedPresentSurfaceCapabilitiesKHR where
  zero = SharedPresentSurfaceCapabilitiesKHR Nothing
                                             zero

#endif


-- No documentation found for TopLevel "vkGetSwapchainStatusKHR"
getSwapchainStatusKHR :: Device ->  SwapchainKHR ->  IO (VkResult)
getSwapchainStatusKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME"
pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME = VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION"
pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION :: Integral a => a
pattern KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
