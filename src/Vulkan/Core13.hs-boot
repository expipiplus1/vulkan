{-# language CPP #-}
-- No documentation found for Chapter "Core13"
module Vulkan.Core13  ( PhysicalDeviceVulkan13Features
                      , PhysicalDeviceVulkan13Properties
                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceVulkan13Features

instance ToCStruct PhysicalDeviceVulkan13Features
instance Show PhysicalDeviceVulkan13Features

instance FromCStruct PhysicalDeviceVulkan13Features


data PhysicalDeviceVulkan13Properties

instance ToCStruct PhysicalDeviceVulkan13Properties
instance Show PhysicalDeviceVulkan13Properties

instance FromCStruct PhysicalDeviceVulkan13Properties

