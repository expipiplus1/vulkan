{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_multiview"
module Vulkan.Core11.Promoted_From_VK_KHR_multiview  ( PhysicalDeviceMultiviewFeatures
                                                     , PhysicalDeviceMultiviewProperties
                                                     , RenderPassMultiviewCreateInfo
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceMultiviewFeatures

instance ToCStruct PhysicalDeviceMultiviewFeatures
instance Show PhysicalDeviceMultiviewFeatures

instance FromCStruct PhysicalDeviceMultiviewFeatures


data PhysicalDeviceMultiviewProperties

instance ToCStruct PhysicalDeviceMultiviewProperties
instance Show PhysicalDeviceMultiviewProperties

instance FromCStruct PhysicalDeviceMultiviewProperties


data RenderPassMultiviewCreateInfo

instance ToCStruct RenderPassMultiviewCreateInfo
instance Show RenderPassMultiviewCreateInfo

instance FromCStruct RenderPassMultiviewCreateInfo

