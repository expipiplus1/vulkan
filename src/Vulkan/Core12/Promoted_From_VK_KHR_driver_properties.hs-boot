{-# language CPP #-}
module Vulkan.Core12.Promoted_From_VK_KHR_driver_properties  ( ConformanceVersion
                                                             , PhysicalDeviceDriverProperties
                                                             ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ConformanceVersion

instance ToCStruct ConformanceVersion
instance Show ConformanceVersion

instance FromCStruct ConformanceVersion


data PhysicalDeviceDriverProperties

instance ToCStruct PhysicalDeviceDriverProperties
instance Show PhysicalDeviceDriverProperties

instance FromCStruct PhysicalDeviceDriverProperties

