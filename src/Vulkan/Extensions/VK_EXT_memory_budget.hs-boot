{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_memory_budget  (PhysicalDeviceMemoryBudgetPropertiesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceMemoryBudgetPropertiesEXT

instance ToCStruct PhysicalDeviceMemoryBudgetPropertiesEXT
instance Show PhysicalDeviceMemoryBudgetPropertiesEXT

instance FromCStruct PhysicalDeviceMemoryBudgetPropertiesEXT

