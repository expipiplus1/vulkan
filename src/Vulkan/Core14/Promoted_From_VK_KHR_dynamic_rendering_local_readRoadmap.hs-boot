{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap  ( PhysicalDeviceDynamicRenderingLocalReadFeatures
                                                                               , RenderingAttachmentLocationInfo
                                                                               , RenderingInputAttachmentIndexInfo
                                                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceDynamicRenderingLocalReadFeatures

instance ToCStruct PhysicalDeviceDynamicRenderingLocalReadFeatures
instance Show PhysicalDeviceDynamicRenderingLocalReadFeatures

instance FromCStruct PhysicalDeviceDynamicRenderingLocalReadFeatures


data RenderingAttachmentLocationInfo

instance ToCStruct RenderingAttachmentLocationInfo
instance Show RenderingAttachmentLocationInfo

instance FromCStruct RenderingAttachmentLocationInfo


data RenderingInputAttachmentIndexInfo

instance ToCStruct RenderingInputAttachmentIndexInfo
instance Show RenderingInputAttachmentIndexInfo

instance FromCStruct RenderingInputAttachmentIndexInfo

