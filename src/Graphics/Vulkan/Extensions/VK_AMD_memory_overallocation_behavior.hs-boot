{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_AMD_memory_overallocation_behavior  (DeviceMemoryOverallocationCreateInfoAMD) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data DeviceMemoryOverallocationCreateInfoAMD

instance ToCStruct DeviceMemoryOverallocationCreateInfoAMD
instance Show DeviceMemoryOverallocationCreateInfoAMD

instance FromCStruct DeviceMemoryOverallocationCreateInfoAMD

