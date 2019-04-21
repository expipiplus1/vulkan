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
pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME = "VK_KHR_swapchain_mutable_format"

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION"
pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION = 1

-- | 'VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR' specifies that the images
-- of the swapchain /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' with a different format
-- than what the swapchain was created with. The list of allowed image view
-- formats are specified by chaining an instance of the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR'
-- structure to the @pNext@ chain of
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'.
-- In addition, this flag also specifies that the swapchain /can/ be
-- created with usage flags that are not supported for the format the
-- swapchain is created with but are supported for at least one of the
-- allowed image view formats.
pattern VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR :: VkSwapchainCreateFlagBitsKHR
pattern VK_SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR = VkSwapchainCreateFlagBitsKHR 0x00000004
