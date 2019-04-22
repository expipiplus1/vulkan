{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_swapchain_mutable_format
  ( pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME
  , pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION
  , pattern VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainCreateFlagBitsKHR(..)
  )


-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME"
pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME = "VK_KHR_swapchain_mutable_format"

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION"
pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION = 1

-- No documentation found for Nested "VkSwapchainCreateFlagBitsKHR" "VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR"
pattern VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR :: VkSwapchainCreateFlagBitsKHR
pattern VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR = VkSwapchainCreateFlagBitsKHR 0x00000004
