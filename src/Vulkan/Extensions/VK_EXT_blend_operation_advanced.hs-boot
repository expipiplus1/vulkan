{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_blend_operation_advanced"
module Vulkan.Extensions.VK_EXT_blend_operation_advanced  ( PhysicalDeviceBlendOperationAdvancedFeaturesEXT
                                                          , PhysicalDeviceBlendOperationAdvancedPropertiesEXT
                                                          , PipelineColorBlendAdvancedStateCreateInfoEXT
                                                          ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
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

