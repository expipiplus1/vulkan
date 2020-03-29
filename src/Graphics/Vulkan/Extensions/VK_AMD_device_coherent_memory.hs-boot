{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_AMD_device_coherent_memory  (PhysicalDeviceCoherentMemoryFeaturesAMD) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceCoherentMemoryFeaturesAMD

instance ToCStruct PhysicalDeviceCoherentMemoryFeaturesAMD
instance Show PhysicalDeviceCoherentMemoryFeaturesAMD

instance FromCStruct PhysicalDeviceCoherentMemoryFeaturesAMD

