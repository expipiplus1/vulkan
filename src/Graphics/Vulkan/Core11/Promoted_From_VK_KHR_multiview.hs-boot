{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview  ( PhysicalDeviceMultiviewFeatures
                                                              , PhysicalDeviceMultiviewProperties
                                                              , RenderPassMultiviewCreateInfo
                                                              ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
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

