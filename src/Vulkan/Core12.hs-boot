{-# language CPP #-}
module Vulkan.Core12  ( PhysicalDeviceVulkan11Features
                      , PhysicalDeviceVulkan11Properties
                      , PhysicalDeviceVulkan12Features
                      , PhysicalDeviceVulkan12Properties
                      ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceVulkan11Features

instance ToCStruct PhysicalDeviceVulkan11Features
instance Show PhysicalDeviceVulkan11Features

instance FromCStruct PhysicalDeviceVulkan11Features


data PhysicalDeviceVulkan11Properties

instance ToCStruct PhysicalDeviceVulkan11Properties
instance Show PhysicalDeviceVulkan11Properties

instance FromCStruct PhysicalDeviceVulkan11Properties


data PhysicalDeviceVulkan12Features

instance ToCStruct PhysicalDeviceVulkan12Features
instance Show PhysicalDeviceVulkan12Features

instance FromCStruct PhysicalDeviceVulkan12Features


data PhysicalDeviceVulkan12Properties

instance ToCStruct PhysicalDeviceVulkan12Properties
instance Show PhysicalDeviceVulkan12Properties

instance FromCStruct PhysicalDeviceVulkan12Properties

