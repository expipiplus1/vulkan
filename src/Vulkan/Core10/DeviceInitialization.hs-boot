{-# language CPP #-}
-- No documentation found for Chapter "DeviceInitialization"
module Vulkan.Core10.DeviceInitialization  ( ApplicationInfo
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

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
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

instance ( Extendss InstanceCreateInfo es
         , PokeChain es ) => ToCStruct (InstanceCreateInfo es)
instance Show (Chain es) => Show (InstanceCreateInfo es)

instance ( Extendss InstanceCreateInfo es
         , PeekChain es ) => FromCStruct (InstanceCreateInfo es)


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

