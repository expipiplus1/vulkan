{-# language CPP #-}
module Graphics.Vulkan.Core10.DeviceInitialization  ( ApplicationInfo
                                                    , FormatProperties
                                                    , ImageFormatProperties
                                                    , InstanceCreateInfo
                                                    , MemoryHeap
                                                    , MemoryType
                                                    , PhysicalDeviceFeatures
                                                    , PhysicalDeviceLimits
                                                    , PhysicalDeviceMemoryProperties
                                                    , PhysicalDeviceProperties
                                                    , PhysicalDeviceSparseProperties
                                                    , QueueFamilyProperties
                                                    ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data ApplicationInfo

instance ToCStruct ApplicationInfo
instance Show ApplicationInfo

instance FromCStruct ApplicationInfo


data FormatProperties

instance ToCStruct FormatProperties
instance Show FormatProperties

instance FromCStruct FormatProperties


data ImageFormatProperties

instance ToCStruct ImageFormatProperties
instance Show ImageFormatProperties

instance FromCStruct ImageFormatProperties


type role InstanceCreateInfo nominal
data InstanceCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (InstanceCreateInfo es)
instance Show (Chain es) => Show (InstanceCreateInfo es)

instance PeekChain es => FromCStruct (InstanceCreateInfo es)


data MemoryHeap

instance ToCStruct MemoryHeap
instance Show MemoryHeap

instance FromCStruct MemoryHeap


data MemoryType

instance ToCStruct MemoryType
instance Show MemoryType

instance FromCStruct MemoryType


data PhysicalDeviceFeatures

instance ToCStruct PhysicalDeviceFeatures
instance Show PhysicalDeviceFeatures

instance FromCStruct PhysicalDeviceFeatures


data PhysicalDeviceLimits

instance ToCStruct PhysicalDeviceLimits
instance Show PhysicalDeviceLimits

instance FromCStruct PhysicalDeviceLimits


data PhysicalDeviceMemoryProperties

instance ToCStruct PhysicalDeviceMemoryProperties
instance Show PhysicalDeviceMemoryProperties

instance FromCStruct PhysicalDeviceMemoryProperties


data PhysicalDeviceProperties

instance ToCStruct PhysicalDeviceProperties
instance Show PhysicalDeviceProperties

instance FromCStruct PhysicalDeviceProperties


data PhysicalDeviceSparseProperties

instance ToCStruct PhysicalDeviceSparseProperties
instance Show PhysicalDeviceSparseProperties

instance FromCStruct PhysicalDeviceSparseProperties


data QueueFamilyProperties

instance ToCStruct QueueFamilyProperties
instance Show QueueFamilyProperties

instance FromCStruct QueueFamilyProperties

