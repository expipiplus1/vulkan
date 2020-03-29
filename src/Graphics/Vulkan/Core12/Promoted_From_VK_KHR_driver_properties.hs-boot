{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_driver_properties  ( ConformanceVersion
                                                                      , PhysicalDeviceDriverProperties
                                                                      ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data ConformanceVersion

instance ToCStruct ConformanceVersion
instance Show ConformanceVersion

instance FromCStruct ConformanceVersion


data PhysicalDeviceDriverProperties

instance ToCStruct PhysicalDeviceDriverProperties
instance Show PhysicalDeviceDriverProperties

instance FromCStruct PhysicalDeviceDriverProperties

