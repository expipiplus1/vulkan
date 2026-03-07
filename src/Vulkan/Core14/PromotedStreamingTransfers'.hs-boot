{-# language CPP #-}
-- No documentation found for Chapter "PromotedStreamingTransfers'"
module Vulkan.Core14.PromotedStreamingTransfers'  ( CopyImageToImageInfo
                                                  , CopyImageToMemoryInfo
                                                  , CopyMemoryToImageInfo
                                                  , HostImageCopyDevicePerformanceQuery
                                                  , HostImageLayoutTransitionInfo
                                                  , ImageToMemoryCopy
                                                  , MemoryToImageCopy
                                                  , PhysicalDeviceHostImageCopyFeatures
                                                  , PhysicalDeviceHostImageCopyProperties
                                                  , SubresourceHostMemcpySize
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CopyImageToImageInfo

instance ToCStruct CopyImageToImageInfo
instance Show CopyImageToImageInfo

instance FromCStruct CopyImageToImageInfo


data CopyImageToMemoryInfo

instance ToCStruct CopyImageToMemoryInfo
instance Show CopyImageToMemoryInfo

instance FromCStruct CopyImageToMemoryInfo


data CopyMemoryToImageInfo

instance ToCStruct CopyMemoryToImageInfo
instance Show CopyMemoryToImageInfo

instance FromCStruct CopyMemoryToImageInfo


data HostImageCopyDevicePerformanceQuery

instance ToCStruct HostImageCopyDevicePerformanceQuery
instance Show HostImageCopyDevicePerformanceQuery

instance FromCStruct HostImageCopyDevicePerformanceQuery


data HostImageLayoutTransitionInfo

instance ToCStruct HostImageLayoutTransitionInfo
instance Show HostImageLayoutTransitionInfo

instance FromCStruct HostImageLayoutTransitionInfo


data ImageToMemoryCopy

instance ToCStruct ImageToMemoryCopy
instance Show ImageToMemoryCopy

instance FromCStruct ImageToMemoryCopy


data MemoryToImageCopy

instance ToCStruct MemoryToImageCopy
instance Show MemoryToImageCopy

instance FromCStruct MemoryToImageCopy


data PhysicalDeviceHostImageCopyFeatures

instance ToCStruct PhysicalDeviceHostImageCopyFeatures
instance Show PhysicalDeviceHostImageCopyFeatures

instance FromCStruct PhysicalDeviceHostImageCopyFeatures


data PhysicalDeviceHostImageCopyProperties

instance ToCStruct PhysicalDeviceHostImageCopyProperties
instance Show PhysicalDeviceHostImageCopyProperties

instance FromCStruct PhysicalDeviceHostImageCopyProperties


data SubresourceHostMemcpySize

instance ToCStruct SubresourceHostMemcpySize
instance Show SubresourceHostMemcpySize

instance FromCStruct SubresourceHostMemcpySize

