{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
  ( pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR
  , pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  , pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR
  , pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION
  , pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME
  , vkGetSwapchainStatusKHR
  , VkSharedPresentSurfaceCapabilitiesKHR(..)
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageUsageFlags
  , VkDevice
  )
import Graphics.Vulkan.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkPresentModeKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )


-- | Nothing
pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR :: VkImageLayout
pattern VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR = VkImageLayout 1000111000
-- | Nothing
pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR = VkStructureType 1000111000
-- | Nothing
pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_SHARED_DEMAND_REFRESH_KHR = VkPresentModeKHR 1000111000
-- | Nothing
pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR :: VkPresentModeKHR
pattern VK_PRESENT_MODE_SHARED_CONTINUOUS_REFRESH_KHR = VkPresentModeKHR 1000111001
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_SPEC_VERSION = 1
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SHARED_PRESENTABLE_IMAGE_EXTENSION_NAME = "VK_KHR_shared_presentable_image"
-- | 
foreign import ccall "vkGetSwapchainStatusKHR" vkGetSwapchainStatusKHR :: ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
-- | TODO: Struct comments
data VkSharedPresentSurfaceCapabilitiesKHR = VkSharedPresentSurfaceCapabilitiesKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkSharedPresentSupportedUsageFlags :: VkImageUsageFlags
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
