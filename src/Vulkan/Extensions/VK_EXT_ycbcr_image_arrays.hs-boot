{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_ycbcr_image_arrays  (PhysicalDeviceYcbcrImageArraysFeaturesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceYcbcrImageArraysFeaturesEXT

instance ToCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT
instance Show PhysicalDeviceYcbcrImageArraysFeaturesEXT

instance FromCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT

