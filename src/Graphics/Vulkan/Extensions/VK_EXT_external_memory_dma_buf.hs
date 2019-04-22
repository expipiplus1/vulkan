{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_external_memory_dma_buf
  ( pattern EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME
  , pattern EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION
  , pattern EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_dma_buf
  ( pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME
  , pattern VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( pattern EXTERNAL_MEMORY_HANDLE_TYPE_DMA_BUF_BIT_EXT
  )


-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME"
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME = VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION"
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION :: Integral a => a
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION = VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION
