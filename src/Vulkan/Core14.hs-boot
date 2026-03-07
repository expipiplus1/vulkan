{-# language CPP #-}
-- No documentation found for Chapter "Core14"
module Vulkan.Core14  ( PhysicalDeviceVulkan14Features
                      , PhysicalDeviceVulkan14Properties
                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceVulkan14Features

instance ToCStruct PhysicalDeviceVulkan14Features
instance Show PhysicalDeviceVulkan14Features

instance FromCStruct PhysicalDeviceVulkan14Features


data PhysicalDeviceVulkan14Properties

instance ToCStruct PhysicalDeviceVulkan14Properties
instance Show PhysicalDeviceVulkan14Properties

instance FromCStruct PhysicalDeviceVulkan14Properties

