{-# language CPP #-}
module Vulkan.Extensions.VK_NVX_image_view_handle  ( ImageViewAddressPropertiesNVX
                                                   , ImageViewHandleInfoNVX
                                                   ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ImageViewAddressPropertiesNVX

instance ToCStruct ImageViewAddressPropertiesNVX
instance Show ImageViewAddressPropertiesNVX

instance FromCStruct ImageViewAddressPropertiesNVX


data ImageViewHandleInfoNVX

instance ToCStruct ImageViewHandleInfoNVX
instance Show ImageViewHandleInfoNVX

instance FromCStruct ImageViewHandleInfoNVX

