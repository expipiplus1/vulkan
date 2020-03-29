{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax  ( PhysicalDeviceSamplerFilterMinmaxProperties
                                                                          , SamplerReductionModeCreateInfo
                                                                          ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceSamplerFilterMinmaxProperties

instance ToCStruct PhysicalDeviceSamplerFilterMinmaxProperties
instance Show PhysicalDeviceSamplerFilterMinmaxProperties

instance FromCStruct PhysicalDeviceSamplerFilterMinmaxProperties


data SamplerReductionModeCreateInfo

instance ToCStruct SamplerReductionModeCreateInfo
instance Show SamplerReductionModeCreateInfo

instance FromCStruct SamplerReductionModeCreateInfo

