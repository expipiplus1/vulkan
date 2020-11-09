{-# language CPP #-}
module Vulkan.Extensions.VK_NV_fragment_shading_rate_enums  ( PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
                                                            , PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
                                                            , PipelineFragmentShadingRateEnumStateCreateInfoNV
                                                            , FragmentShadingRateNV
                                                            ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceFragmentShadingRateEnumsFeaturesNV

instance ToCStruct PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
instance Show PhysicalDeviceFragmentShadingRateEnumsFeaturesNV

instance FromCStruct PhysicalDeviceFragmentShadingRateEnumsFeaturesNV


data PhysicalDeviceFragmentShadingRateEnumsPropertiesNV

instance ToCStruct PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
instance Show PhysicalDeviceFragmentShadingRateEnumsPropertiesNV

instance FromCStruct PhysicalDeviceFragmentShadingRateEnumsPropertiesNV


data PipelineFragmentShadingRateEnumStateCreateInfoNV

instance ToCStruct PipelineFragmentShadingRateEnumStateCreateInfoNV
instance Show PipelineFragmentShadingRateEnumStateCreateInfoNV

instance FromCStruct PipelineFragmentShadingRateEnumStateCreateInfoNV


data FragmentShadingRateNV

