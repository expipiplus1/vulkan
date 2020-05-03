{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_relaxed_block_layout  ( KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION
                                                      , pattern KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION
                                                      , KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME
                                                      , pattern KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME
                                                      ) where

import Data.String (IsString)

type KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION"
pattern KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION = 1


type KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME = "VK_KHR_relaxed_block_layout"

-- No documentation found for TopLevel "VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME"
pattern KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME = "VK_KHR_relaxed_block_layout"

