{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_IMG_format_pvrtc  ( IMG_FORMAT_PVRTC_SPEC_VERSION
                                                       , pattern IMG_FORMAT_PVRTC_SPEC_VERSION
                                                       , IMG_FORMAT_PVRTC_EXTENSION_NAME
                                                       , pattern IMG_FORMAT_PVRTC_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)

type IMG_FORMAT_PVRTC_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_IMG_FORMAT_PVRTC_SPEC_VERSION"
pattern IMG_FORMAT_PVRTC_SPEC_VERSION :: forall a . Integral a => a
pattern IMG_FORMAT_PVRTC_SPEC_VERSION = 1


type IMG_FORMAT_PVRTC_EXTENSION_NAME = "VK_IMG_format_pvrtc"

-- No documentation found for TopLevel "VK_IMG_FORMAT_PVRTC_EXTENSION_NAME"
pattern IMG_FORMAT_PVRTC_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern IMG_FORMAT_PVRTC_EXTENSION_NAME = "VK_IMG_format_pvrtc"

