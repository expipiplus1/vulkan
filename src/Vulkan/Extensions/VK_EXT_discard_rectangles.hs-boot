{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_discard_rectangles"
module Vulkan.Extensions.VK_EXT_discard_rectangles  ( PhysicalDeviceDiscardRectanglePropertiesEXT
                                                    , PipelineDiscardRectangleStateCreateInfoEXT
                                                    ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceDiscardRectanglePropertiesEXT

instance ToCStruct PhysicalDeviceDiscardRectanglePropertiesEXT
instance Show PhysicalDeviceDiscardRectanglePropertiesEXT

instance FromCStruct PhysicalDeviceDiscardRectanglePropertiesEXT


data PipelineDiscardRectangleStateCreateInfoEXT

instance ToCStruct PipelineDiscardRectangleStateCreateInfoEXT
instance Show PipelineDiscardRectangleStateCreateInfoEXT

instance FromCStruct PipelineDiscardRectangleStateCreateInfoEXT

