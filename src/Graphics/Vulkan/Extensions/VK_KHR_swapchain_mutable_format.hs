{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_swapchain_mutable_format
  ( pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME
  , pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION
  , pattern SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain_mutable_format
  ( pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME
  , pattern VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( pattern SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR
  )


-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME"
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME = VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION"
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION :: Integral a => a
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION = VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION
