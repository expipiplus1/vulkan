{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_driver_properties"
module Vulkan.Core12.Promoted_From_VK_KHR_driver_properties  ( ConformanceVersion
                                                             , PhysicalDeviceDriverProperties
                                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ConformanceVersion

instance ToCStruct ConformanceVersion
instance Show ConformanceVersion

instance FromCStruct ConformanceVersion


data PhysicalDeviceDriverProperties

instance ToCStruct PhysicalDeviceDriverProperties
instance Show PhysicalDeviceDriverProperties

instance FromCStruct PhysicalDeviceDriverProperties

