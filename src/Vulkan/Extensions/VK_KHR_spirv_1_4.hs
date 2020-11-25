{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_spirv_1_4"
module Vulkan.Extensions.VK_KHR_spirv_1_4  ( KHR_SPIRV_1_4_SPEC_VERSION
                                           , pattern KHR_SPIRV_1_4_SPEC_VERSION
                                           , KHR_SPIRV_1_4_EXTENSION_NAME
                                           , pattern KHR_SPIRV_1_4_EXTENSION_NAME
                                           ) where

import Data.String (IsString)

type KHR_SPIRV_1_4_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SPIRV_1_4_SPEC_VERSION"
pattern KHR_SPIRV_1_4_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SPIRV_1_4_SPEC_VERSION = 1


type KHR_SPIRV_1_4_EXTENSION_NAME = "VK_KHR_spirv_1_4"

-- No documentation found for TopLevel "VK_KHR_SPIRV_1_4_EXTENSION_NAME"
pattern KHR_SPIRV_1_4_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SPIRV_1_4_EXTENSION_NAME = "VK_KHR_spirv_1_4"

