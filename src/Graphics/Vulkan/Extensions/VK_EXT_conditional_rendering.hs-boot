{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering  ( CommandBufferInheritanceConditionalRenderingInfoEXT
                                                                , ConditionalRenderingBeginInfoEXT
                                                                , PhysicalDeviceConditionalRenderingFeaturesEXT
                                                                ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data CommandBufferInheritanceConditionalRenderingInfoEXT

instance ToCStruct CommandBufferInheritanceConditionalRenderingInfoEXT
instance Show CommandBufferInheritanceConditionalRenderingInfoEXT

instance FromCStruct CommandBufferInheritanceConditionalRenderingInfoEXT


data ConditionalRenderingBeginInfoEXT

instance ToCStruct ConditionalRenderingBeginInfoEXT
instance Show ConditionalRenderingBeginInfoEXT

instance FromCStruct ConditionalRenderingBeginInfoEXT


data PhysicalDeviceConditionalRenderingFeaturesEXT

instance ToCStruct PhysicalDeviceConditionalRenderingFeaturesEXT
instance Show PhysicalDeviceConditionalRenderingFeaturesEXT

instance FromCStruct PhysicalDeviceConditionalRenderingFeaturesEXT

