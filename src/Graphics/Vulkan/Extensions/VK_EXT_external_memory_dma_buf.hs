{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_EXT_external_memory_dma_buf
  ( pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT
  , pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION
  , pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlagBits(..)
  )


-- No documentation found for Nested "VkExternalMemoryHandleTypeFlagBits" "VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT"
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT :: VkExternalMemoryHandleTypeFlagBits
pattern VK_EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT = VkExternalMemoryHandleTypeFlagBits 0x00000200
-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION"
pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION :: Integral a => a
pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME"
pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME = "VK_EXT_external_memory_dma_buf"
