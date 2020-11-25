{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_external_memory_dma_buf"
module Vulkan.Extensions.VK_EXT_external_memory_dma_buf  ( EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION
                                                         , pattern EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION
                                                         , EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME
                                                         , pattern EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME
                                                         ) where

import Data.String (IsString)

type EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION"
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_SPEC_VERSION = 1


type EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME = "VK_EXT_external_memory_dma_buf"

-- No documentation found for TopLevel "VK_EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME"
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EXTERNAL_MEMORY_DMA_BUF_EXTENSION_NAME = "VK_EXT_external_memory_dma_buf"

