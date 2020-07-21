{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_image_robustness  (PhysicalDeviceImageRobustnessFeaturesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceImageRobustnessFeaturesEXT

instance ToCStruct PhysicalDeviceImageRobustnessFeaturesEXT
instance Show PhysicalDeviceImageRobustnessFeaturesEXT

instance FromCStruct PhysicalDeviceImageRobustnessFeaturesEXT

