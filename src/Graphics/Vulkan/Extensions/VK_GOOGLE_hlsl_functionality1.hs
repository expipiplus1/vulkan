{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_GOOGLE_hlsl_functionality1  ( GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION
                                                                 , pattern GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION
                                                                 , GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME
                                                                 , pattern GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME
                                                                 ) where

import Data.String (IsString)

type GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION"
pattern GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION :: forall a . Integral a => a
pattern GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION = 1


type GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME = "VK_GOOGLE_hlsl_functionality1"

-- No documentation found for TopLevel "VK_GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME"
pattern GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME = "VK_GOOGLE_hlsl_functionality1"

