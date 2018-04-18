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
  ( VkBool32(..)
  , VkStructureType(..)
  , VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkRect2D(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  , VkSwapchainCreateInfoKHR(..)
  )


-- | Nothing
pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR :: VkResult
pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR = VkResult (-1000003001)
-- | Nothing
pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR = VkStructureType 1000003000
pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DISPLAY_SWAPCHAIN_SPEC_VERSION = 9
pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DISPLAY_SWAPCHAIN_EXTENSION_NAME = "VK_KHR_display_swapchain"
-- | 
foreign import ccall "vkCreateSharedSwapchainsKHR" vkCreateSharedSwapchainsKHR :: ("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult
-- | TODO: Struct comments
data VkDisplayPresentInfoKHR = VkDisplayPresentInfoKHR
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkSrcRect :: VkRect2D
  , vkDstRect :: VkRect2D
  , vkPersistent :: VkBool32
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
