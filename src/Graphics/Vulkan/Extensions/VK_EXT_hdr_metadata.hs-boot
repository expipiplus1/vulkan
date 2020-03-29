{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata  ( HdrMetadataEXT
                                                       , XYColorEXT
                                                       ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data HdrMetadataEXT

instance ToCStruct HdrMetadataEXT
instance Show HdrMetadataEXT

instance FromCStruct HdrMetadataEXT


data XYColorEXT

instance ToCStruct XYColorEXT
instance Show XYColorEXT

instance FromCStruct XYColorEXT

