{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_sample_locations  ( AttachmentSampleLocationsEXT
                                                           , MultisamplePropertiesEXT
                                                           , PhysicalDeviceSampleLocationsPropertiesEXT
                                                           , PipelineSampleLocationsStateCreateInfoEXT
                                                           , RenderPassSampleLocationsBeginInfoEXT
                                                           , SampleLocationEXT
                                                           , SampleLocationsInfoEXT
                                                           , SubpassSampleLocationsEXT
                                                           ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data AttachmentSampleLocationsEXT

instance ToCStruct AttachmentSampleLocationsEXT
instance Show AttachmentSampleLocationsEXT

instance FromCStruct AttachmentSampleLocationsEXT


data MultisamplePropertiesEXT

instance ToCStruct MultisamplePropertiesEXT
instance Show MultisamplePropertiesEXT

instance FromCStruct MultisamplePropertiesEXT


data PhysicalDeviceSampleLocationsPropertiesEXT

instance ToCStruct PhysicalDeviceSampleLocationsPropertiesEXT
instance Show PhysicalDeviceSampleLocationsPropertiesEXT

instance FromCStruct PhysicalDeviceSampleLocationsPropertiesEXT


data PipelineSampleLocationsStateCreateInfoEXT

instance ToCStruct PipelineSampleLocationsStateCreateInfoEXT
instance Show PipelineSampleLocationsStateCreateInfoEXT

instance FromCStruct PipelineSampleLocationsStateCreateInfoEXT


data RenderPassSampleLocationsBeginInfoEXT

instance ToCStruct RenderPassSampleLocationsBeginInfoEXT
instance Show RenderPassSampleLocationsBeginInfoEXT

instance FromCStruct RenderPassSampleLocationsBeginInfoEXT


data SampleLocationEXT

instance ToCStruct SampleLocationEXT
instance Show SampleLocationEXT

instance FromCStruct SampleLocationEXT


data SampleLocationsInfoEXT

instance ToCStruct SampleLocationsInfoEXT
instance Show SampleLocationsInfoEXT

instance FromCStruct SampleLocationsInfoEXT


data SubpassSampleLocationsEXT

instance ToCStruct SubpassSampleLocationsEXT
instance Show SubpassSampleLocationsEXT

instance FromCStruct SubpassSampleLocationsEXT

