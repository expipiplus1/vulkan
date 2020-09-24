{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_portability_subset  ( PhysicalDevicePortabilitySubsetFeaturesKHR
                                                    , PhysicalDevicePortabilitySubsetPropertiesKHR
                                                    ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDevicePortabilitySubsetFeaturesKHR

instance ToCStruct PhysicalDevicePortabilitySubsetFeaturesKHR
instance Show PhysicalDevicePortabilitySubsetFeaturesKHR

instance FromCStruct PhysicalDevicePortabilitySubsetFeaturesKHR


data PhysicalDevicePortabilitySubsetPropertiesKHR

instance ToCStruct PhysicalDevicePortabilitySubsetPropertiesKHR
instance Show PhysicalDevicePortabilitySubsetPropertiesKHR

instance FromCStruct PhysicalDevicePortabilitySubsetPropertiesKHR

