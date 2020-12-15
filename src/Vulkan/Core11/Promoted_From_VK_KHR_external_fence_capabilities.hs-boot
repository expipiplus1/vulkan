{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_fence_capabilities"
module Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities  ( ExternalFenceProperties
                                                                       , PhysicalDeviceExternalFenceInfo
                                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ExternalFenceProperties

instance ToCStruct ExternalFenceProperties
instance Show ExternalFenceProperties

instance FromCStruct ExternalFenceProperties


data PhysicalDeviceExternalFenceInfo

instance ToCStruct PhysicalDeviceExternalFenceInfo
instance Show PhysicalDeviceExternalFenceInfo

instance FromCStruct PhysicalDeviceExternalFenceInfo

