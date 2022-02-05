{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_dynamic_rendering"
module Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering  ( CommandBufferInheritanceRenderingInfo
                                                             , PhysicalDeviceDynamicRenderingFeatures
                                                             , PipelineRenderingCreateInfo
                                                             , RenderingAttachmentInfo
                                                             , RenderingInfo
                                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data CommandBufferInheritanceRenderingInfo

instance ToCStruct CommandBufferInheritanceRenderingInfo
instance Show CommandBufferInheritanceRenderingInfo

instance FromCStruct CommandBufferInheritanceRenderingInfo


data PhysicalDeviceDynamicRenderingFeatures

instance ToCStruct PhysicalDeviceDynamicRenderingFeatures
instance Show PhysicalDeviceDynamicRenderingFeatures

instance FromCStruct PhysicalDeviceDynamicRenderingFeatures


data PipelineRenderingCreateInfo

instance ToCStruct PipelineRenderingCreateInfo
instance Show PipelineRenderingCreateInfo

instance FromCStruct PipelineRenderingCreateInfo


data RenderingAttachmentInfo

instance ToCStruct RenderingAttachmentInfo
instance Show RenderingAttachmentInfo


type role RenderingInfo nominal
data RenderingInfo (es :: [Type])

instance (Extendss RenderingInfo es, PokeChain es) => ToCStruct (RenderingInfo es)
instance Show (Chain es) => Show (RenderingInfo es)

