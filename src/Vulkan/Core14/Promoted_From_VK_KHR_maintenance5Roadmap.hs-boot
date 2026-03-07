{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance5Roadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap  ( BufferUsageFlags2CreateInfo
                                                               , DeviceImageSubresourceInfo
                                                               , ImageSubresource2
                                                               , PhysicalDeviceMaintenance5Features
                                                               , PhysicalDeviceMaintenance5Properties
                                                               , PipelineCreateFlags2CreateInfo
                                                               , RenderingAreaInfo
                                                               , SubresourceLayout2
                                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BufferUsageFlags2CreateInfo

instance ToCStruct BufferUsageFlags2CreateInfo
instance Show BufferUsageFlags2CreateInfo

instance FromCStruct BufferUsageFlags2CreateInfo


data DeviceImageSubresourceInfo

instance ToCStruct DeviceImageSubresourceInfo
instance Show DeviceImageSubresourceInfo

instance FromCStruct DeviceImageSubresourceInfo


data ImageSubresource2

instance ToCStruct ImageSubresource2
instance Show ImageSubresource2

instance FromCStruct ImageSubresource2


data PhysicalDeviceMaintenance5Features

instance ToCStruct PhysicalDeviceMaintenance5Features
instance Show PhysicalDeviceMaintenance5Features

instance FromCStruct PhysicalDeviceMaintenance5Features


data PhysicalDeviceMaintenance5Properties

instance ToCStruct PhysicalDeviceMaintenance5Properties
instance Show PhysicalDeviceMaintenance5Properties

instance FromCStruct PhysicalDeviceMaintenance5Properties


data PipelineCreateFlags2CreateInfo

instance ToCStruct PipelineCreateFlags2CreateInfo
instance Show PipelineCreateFlags2CreateInfo

instance FromCStruct PipelineCreateFlags2CreateInfo


data RenderingAreaInfo

instance ToCStruct RenderingAreaInfo
instance Show RenderingAreaInfo

instance FromCStruct RenderingAreaInfo


type role SubresourceLayout2 nominal
data SubresourceLayout2 (es :: [Type])

instance ( Extendss SubresourceLayout2 es
         , PokeChain es ) => ToCStruct (SubresourceLayout2 es)
instance Show (Chain es) => Show (SubresourceLayout2 es)

instance ( Extendss SubresourceLayout2 es
         , PeekChain es ) => FromCStruct (SubresourceLayout2 es)

