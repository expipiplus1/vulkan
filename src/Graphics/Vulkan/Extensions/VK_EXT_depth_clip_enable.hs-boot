{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable  ( PhysicalDeviceDepthClipEnableFeaturesEXT
                                                            , PipelineRasterizationDepthClipStateCreateInfoEXT
                                                            ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceDepthClipEnableFeaturesEXT

instance ToCStruct PhysicalDeviceDepthClipEnableFeaturesEXT
instance Show PhysicalDeviceDepthClipEnableFeaturesEXT

instance FromCStruct PhysicalDeviceDepthClipEnableFeaturesEXT


data PipelineRasterizationDepthClipStateCreateInfoEXT

instance ToCStruct PipelineRasterizationDepthClipStateCreateInfoEXT
instance Show PipelineRasterizationDepthClipStateCreateInfoEXT

instance FromCStruct PipelineRasterizationDepthClipStateCreateInfoEXT

