{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion  ( BindImagePlaneMemoryInfo
                                                                             , ImagePlaneMemoryRequirementsInfo
                                                                             , PhysicalDeviceSamplerYcbcrConversionFeatures
                                                                             , SamplerYcbcrConversionCreateInfo
                                                                             , SamplerYcbcrConversionImageFormatProperties
                                                                             , SamplerYcbcrConversionInfo
                                                                             ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data BindImagePlaneMemoryInfo

instance ToCStruct BindImagePlaneMemoryInfo
instance Show BindImagePlaneMemoryInfo

instance FromCStruct BindImagePlaneMemoryInfo


data ImagePlaneMemoryRequirementsInfo

instance ToCStruct ImagePlaneMemoryRequirementsInfo
instance Show ImagePlaneMemoryRequirementsInfo

instance FromCStruct ImagePlaneMemoryRequirementsInfo


data PhysicalDeviceSamplerYcbcrConversionFeatures

instance ToCStruct PhysicalDeviceSamplerYcbcrConversionFeatures
instance Show PhysicalDeviceSamplerYcbcrConversionFeatures

instance FromCStruct PhysicalDeviceSamplerYcbcrConversionFeatures


type role SamplerYcbcrConversionCreateInfo nominal
data SamplerYcbcrConversionCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (SamplerYcbcrConversionCreateInfo es)
instance Show (Chain es) => Show (SamplerYcbcrConversionCreateInfo es)

instance PeekChain es => FromCStruct (SamplerYcbcrConversionCreateInfo es)


data SamplerYcbcrConversionImageFormatProperties

instance ToCStruct SamplerYcbcrConversionImageFormatProperties
instance Show SamplerYcbcrConversionImageFormatProperties

instance FromCStruct SamplerYcbcrConversionImageFormatProperties


data SamplerYcbcrConversionInfo

instance ToCStruct SamplerYcbcrConversionInfo
instance Show SamplerYcbcrConversionInfo

instance FromCStruct SamplerYcbcrConversionInfo

