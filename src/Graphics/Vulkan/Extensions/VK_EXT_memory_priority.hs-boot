{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_memory_priority  ( MemoryPriorityAllocateInfoEXT
                                                          , PhysicalDeviceMemoryPriorityFeaturesEXT
                                                          ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data MemoryPriorityAllocateInfoEXT

instance ToCStruct MemoryPriorityAllocateInfoEXT
instance Show MemoryPriorityAllocateInfoEXT

instance FromCStruct MemoryPriorityAllocateInfoEXT


data PhysicalDeviceMemoryPriorityFeaturesEXT

instance ToCStruct PhysicalDeviceMemoryPriorityFeaturesEXT
instance Show PhysicalDeviceMemoryPriorityFeaturesEXT

instance FromCStruct PhysicalDeviceMemoryPriorityFeaturesEXT

