{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_extended_dynamic_state  (PhysicalDeviceExtendedDynamicStateFeaturesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceExtendedDynamicStateFeaturesEXT

instance ToCStruct PhysicalDeviceExtendedDynamicStateFeaturesEXT
instance Show PhysicalDeviceExtendedDynamicStateFeaturesEXT

instance FromCStruct PhysicalDeviceExtendedDynamicStateFeaturesEXT

