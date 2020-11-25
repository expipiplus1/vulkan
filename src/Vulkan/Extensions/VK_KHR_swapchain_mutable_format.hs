{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_swapchain_mutable_format"
module Vulkan.Extensions.VK_KHR_swapchain_mutable_format  ( KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION
                                                          , pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION
                                                          , KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME
                                                          , pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME
                                                          , SwapchainCreateFlagBitsKHR(..)
                                                          , SwapchainCreateFlagsKHR
                                                          ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
type KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION"
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION = 1


type KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME = "VK_KHR_swapchain_mutable_format"

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME"
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME = "VK_KHR_swapchain_mutable_format"

