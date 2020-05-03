{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_memory_overallocation_behavior  (DeviceMemoryOverallocationCreateInfoAMD) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DeviceMemoryOverallocationCreateInfoAMD

instance ToCStruct DeviceMemoryOverallocationCreateInfoAMD
instance Show DeviceMemoryOverallocationCreateInfoAMD

instance FromCStruct DeviceMemoryOverallocationCreateInfoAMD

