{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_private_vendor_info"
module Vulkan.Extensions.VK_NV_private_vendor_info  ( NV_PRIVATE_VENDOR_INFO_SPEC_VERSION
                                                    , pattern NV_PRIVATE_VENDOR_INFO_SPEC_VERSION
                                                    , NV_PRIVATE_VENDOR_INFO_EXTENSION_NAME
                                                    , pattern NV_PRIVATE_VENDOR_INFO_EXTENSION_NAME
                                                    ) where

import Data.String (IsString)

type NV_PRIVATE_VENDOR_INFO_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_PRIVATE_VENDOR_INFO_SPEC_VERSION"
pattern NV_PRIVATE_VENDOR_INFO_SPEC_VERSION :: forall a . Integral a => a
pattern NV_PRIVATE_VENDOR_INFO_SPEC_VERSION = 2


type NV_PRIVATE_VENDOR_INFO_EXTENSION_NAME = "VK_NV_private_vendor_info"

-- No documentation found for TopLevel "VK_NV_PRIVATE_VENDOR_INFO_EXTENSION_NAME"
pattern NV_PRIVATE_VENDOR_INFO_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_PRIVATE_VENDOR_INFO_EXTENSION_NAME = "VK_NV_private_vendor_info"

