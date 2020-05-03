{-# language CPP #-}
module Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities  ( ExternalBufferProperties
                                                                        , ExternalImageFormatProperties
                                                                        , ExternalMemoryProperties
                                                                        , PhysicalDeviceExternalBufferInfo
                                                                        , PhysicalDeviceExternalImageFormatInfo
                                                                        , PhysicalDeviceIDProperties
                                                                        ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ExternalBufferProperties

instance ToCStruct ExternalBufferProperties
instance Show ExternalBufferProperties

instance FromCStruct ExternalBufferProperties


data ExternalImageFormatProperties

instance ToCStruct ExternalImageFormatProperties
instance Show ExternalImageFormatProperties

instance FromCStruct ExternalImageFormatProperties


data ExternalMemoryProperties

instance ToCStruct ExternalMemoryProperties
instance Show ExternalMemoryProperties

instance FromCStruct ExternalMemoryProperties


data PhysicalDeviceExternalBufferInfo

instance ToCStruct PhysicalDeviceExternalBufferInfo
instance Show PhysicalDeviceExternalBufferInfo

instance FromCStruct PhysicalDeviceExternalBufferInfo


data PhysicalDeviceExternalImageFormatInfo

instance ToCStruct PhysicalDeviceExternalImageFormatInfo
instance Show PhysicalDeviceExternalImageFormatInfo

instance FromCStruct PhysicalDeviceExternalImageFormatInfo


data PhysicalDeviceIDProperties

instance ToCStruct PhysicalDeviceIDProperties
instance Show PhysicalDeviceIDProperties

instance FromCStruct PhysicalDeviceIDProperties

