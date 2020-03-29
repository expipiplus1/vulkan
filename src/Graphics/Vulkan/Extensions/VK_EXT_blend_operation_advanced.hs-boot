{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced  ( PhysicalDeviceBlendOperationAdvancedFeaturesEXT
                                                                   , PhysicalDeviceBlendOperationAdvancedPropertiesEXT
                                                                   , PipelineColorBlendAdvancedStateCreateInfoEXT
                                                                   ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceBlendOperationAdvancedFeaturesEXT

instance ToCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT
instance Show PhysicalDeviceBlendOperationAdvancedFeaturesEXT

instance FromCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT


data PhysicalDeviceBlendOperationAdvancedPropertiesEXT

instance ToCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT
instance Show PhysicalDeviceBlendOperationAdvancedPropertiesEXT

instance FromCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT


data PipelineColorBlendAdvancedStateCreateInfoEXT

instance ToCStruct PipelineColorBlendAdvancedStateCreateInfoEXT
instance Show PipelineColorBlendAdvancedStateCreateInfoEXT

instance FromCStruct PipelineColorBlendAdvancedStateCreateInfoEXT

