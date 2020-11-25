{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_device_coherent_memory"
module Vulkan.Extensions.VK_AMD_device_coherent_memory  (PhysicalDeviceCoherentMemoryFeaturesAMD) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceCoherentMemoryFeaturesAMD

instance ToCStruct PhysicalDeviceCoherentMemoryFeaturesAMD
instance Show PhysicalDeviceCoherentMemoryFeaturesAMD

instance FromCStruct PhysicalDeviceCoherentMemoryFeaturesAMD

