{-# language CPP #-}
-- No documentation found for Chapter "Core10"
module Vulkan.Core10  ( PhysicalDeviceVulkanSC10Features
                      , PhysicalDeviceVulkanSC10Properties
                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceVulkanSC10Features

instance ToCStruct PhysicalDeviceVulkanSC10Features
instance Show PhysicalDeviceVulkanSC10Features

instance FromCStruct PhysicalDeviceVulkanSC10Features


data PhysicalDeviceVulkanSC10Properties

instance ToCStruct PhysicalDeviceVulkanSC10Properties
instance Show PhysicalDeviceVulkanSC10Properties

instance FromCStruct PhysicalDeviceVulkanSC10Properties

