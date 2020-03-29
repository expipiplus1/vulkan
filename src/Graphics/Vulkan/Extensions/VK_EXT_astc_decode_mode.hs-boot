{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode  ( ImageViewASTCDecodeModeEXT
                                                           , PhysicalDeviceASTCDecodeFeaturesEXT
                                                           ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data ImageViewASTCDecodeModeEXT

instance ToCStruct ImageViewASTCDecodeModeEXT
instance Show ImageViewASTCDecodeModeEXT

instance FromCStruct ImageViewASTCDecodeModeEXT


data PhysicalDeviceASTCDecodeFeaturesEXT

instance ToCStruct PhysicalDeviceASTCDecodeFeaturesEXT
instance Show PhysicalDeviceASTCDecodeFeaturesEXT

instance FromCStruct PhysicalDeviceASTCDecodeFeaturesEXT

