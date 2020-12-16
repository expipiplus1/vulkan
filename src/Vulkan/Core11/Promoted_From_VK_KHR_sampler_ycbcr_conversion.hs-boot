{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_sampler_ycbcr_conversion"
module Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion  ( BindImagePlaneMemoryInfo
                                                                    , ImagePlaneMemoryRequirementsInfo
                                                                    , PhysicalDeviceSamplerYcbcrConversionFeatures
                                                                    , SamplerYcbcrConversionCreateInfo
                                                                    , SamplerYcbcrConversionImageFormatProperties
                                                                    , SamplerYcbcrConversionInfo
                                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
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

instance (Extendss SamplerYcbcrConversionCreateInfo es, PokeChain es) => ToCStruct (SamplerYcbcrConversionCreateInfo es)
instance Show (Chain es) => Show (SamplerYcbcrConversionCreateInfo es)

instance (Extendss SamplerYcbcrConversionCreateInfo es, PeekChain es) => FromCStruct (SamplerYcbcrConversionCreateInfo es)


data SamplerYcbcrConversionImageFormatProperties

instance ToCStruct SamplerYcbcrConversionImageFormatProperties
instance Show SamplerYcbcrConversionImageFormatProperties

instance FromCStruct SamplerYcbcrConversionImageFormatProperties


data SamplerYcbcrConversionInfo

instance ToCStruct SamplerYcbcrConversionInfo
instance Show SamplerYcbcrConversionInfo

instance FromCStruct SamplerYcbcrConversionInfo

