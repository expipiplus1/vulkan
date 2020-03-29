{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_memory_budget  (PhysicalDeviceMemoryBudgetPropertiesEXT) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceMemoryBudgetPropertiesEXT

instance ToCStruct PhysicalDeviceMemoryBudgetPropertiesEXT
instance Show PhysicalDeviceMemoryBudgetPropertiesEXT

instance FromCStruct PhysicalDeviceMemoryBudgetPropertiesEXT

