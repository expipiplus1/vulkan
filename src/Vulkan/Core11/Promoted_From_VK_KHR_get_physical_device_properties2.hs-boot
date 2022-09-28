{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_get_physical_device_properties2"
module Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2  ( FormatProperties2
                                                                           , ImageFormatProperties2
                                                                           , PhysicalDeviceFeatures2
                                                                           , PhysicalDeviceImageFormatInfo2
                                                                           , PhysicalDeviceMemoryProperties2
                                                                           , PhysicalDeviceProperties2
                                                                           , PhysicalDeviceSparseImageFormatInfo2
                                                                           , QueueFamilyProperties2
                                                                           , SparseImageFormatProperties2
                                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role FormatProperties2 nominal
data FormatProperties2 (es :: [Type])

instance ( Extendss FormatProperties2 es
         , PokeChain es ) => ToCStruct (FormatProperties2 es)
instance Show (Chain es) => Show (FormatProperties2 es)

instance ( Extendss FormatProperties2 es
         , PeekChain es ) => FromCStruct (FormatProperties2 es)


type role ImageFormatProperties2 nominal
data ImageFormatProperties2 (es :: [Type])

instance ( Extendss ImageFormatProperties2 es
         , PokeChain es ) => ToCStruct (ImageFormatProperties2 es)
instance Show (Chain es) => Show (ImageFormatProperties2 es)

instance ( Extendss ImageFormatProperties2 es
         , PeekChain es ) => FromCStruct (ImageFormatProperties2 es)


type role PhysicalDeviceFeatures2 nominal
data PhysicalDeviceFeatures2 (es :: [Type])

instance ( Extendss PhysicalDeviceFeatures2 es
         , PokeChain es ) => ToCStruct (PhysicalDeviceFeatures2 es)
instance Show (Chain es) => Show (PhysicalDeviceFeatures2 es)

instance ( Extendss PhysicalDeviceFeatures2 es
         , PeekChain es ) => FromCStruct (PhysicalDeviceFeatures2 es)


type role PhysicalDeviceImageFormatInfo2 nominal
data PhysicalDeviceImageFormatInfo2 (es :: [Type])

instance ( Extendss PhysicalDeviceImageFormatInfo2 es
         , PokeChain es ) => ToCStruct (PhysicalDeviceImageFormatInfo2 es)
instance Show (Chain es) => Show (PhysicalDeviceImageFormatInfo2 es)

instance ( Extendss PhysicalDeviceImageFormatInfo2 es
         , PeekChain es ) => FromCStruct (PhysicalDeviceImageFormatInfo2 es)


type role PhysicalDeviceMemoryProperties2 nominal
data PhysicalDeviceMemoryProperties2 (es :: [Type])

instance ( Extendss PhysicalDeviceMemoryProperties2 es
         , PokeChain es ) => ToCStruct (PhysicalDeviceMemoryProperties2 es)
instance Show (Chain es) => Show (PhysicalDeviceMemoryProperties2 es)

instance ( Extendss PhysicalDeviceMemoryProperties2 es
         , PeekChain es ) => FromCStruct (PhysicalDeviceMemoryProperties2 es)


type role PhysicalDeviceProperties2 nominal
data PhysicalDeviceProperties2 (es :: [Type])

instance ( Extendss PhysicalDeviceProperties2 es
         , PokeChain es ) => ToCStruct (PhysicalDeviceProperties2 es)
instance Show (Chain es) => Show (PhysicalDeviceProperties2 es)

instance ( Extendss PhysicalDeviceProperties2 es
         , PeekChain es ) => FromCStruct (PhysicalDeviceProperties2 es)


data PhysicalDeviceSparseImageFormatInfo2

instance ToCStruct PhysicalDeviceSparseImageFormatInfo2
instance Show PhysicalDeviceSparseImageFormatInfo2

instance FromCStruct PhysicalDeviceSparseImageFormatInfo2


type role QueueFamilyProperties2 nominal
data QueueFamilyProperties2 (es :: [Type])

instance ( Extendss QueueFamilyProperties2 es
         , PokeChain es ) => ToCStruct (QueueFamilyProperties2 es)
instance Show (Chain es) => Show (QueueFamilyProperties2 es)

instance ( Extendss QueueFamilyProperties2 es
         , PeekChain es ) => FromCStruct (QueueFamilyProperties2 es)


data SparseImageFormatProperties2

instance ToCStruct SparseImageFormatProperties2
instance Show SparseImageFormatProperties2

instance FromCStruct SparseImageFormatProperties2

