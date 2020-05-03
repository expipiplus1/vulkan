{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_hdr_metadata  ( HdrMetadataEXT
                                              , XYColorEXT
                                              ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data HdrMetadataEXT

instance ToCStruct HdrMetadataEXT
instance Show HdrMetadataEXT

instance FromCStruct HdrMetadataEXT


data XYColorEXT

instance ToCStruct XYColorEXT
instance Show XYColorEXT

instance FromCStruct XYColorEXT

