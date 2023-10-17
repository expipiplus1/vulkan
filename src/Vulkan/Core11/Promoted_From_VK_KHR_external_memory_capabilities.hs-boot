{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_memory_capabilities"
module Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities  ( ExternalBufferProperties
                                                                        , ExternalImageFormatProperties
                                                                        , ExternalMemoryProperties
                                                                        , PhysicalDeviceExternalBufferInfo
                                                                        , PhysicalDeviceExternalImageFormatInfo
                                                                        , PhysicalDeviceIDProperties
                                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
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


type role PhysicalDeviceExternalBufferInfo nominal
data PhysicalDeviceExternalBufferInfo (es :: [Type])

instance ( Extendss PhysicalDeviceExternalBufferInfo es
         , PokeChain es ) => ToCStruct (PhysicalDeviceExternalBufferInfo es)
instance Show (Chain es) => Show (PhysicalDeviceExternalBufferInfo es)

instance ( Extendss PhysicalDeviceExternalBufferInfo es
         , PeekChain es ) => FromCStruct (PhysicalDeviceExternalBufferInfo es)


data PhysicalDeviceExternalImageFormatInfo

instance ToCStruct PhysicalDeviceExternalImageFormatInfo
instance Show PhysicalDeviceExternalImageFormatInfo

instance FromCStruct PhysicalDeviceExternalImageFormatInfo


data PhysicalDeviceIDProperties

instance ToCStruct PhysicalDeviceIDProperties
instance Show PhysicalDeviceIDProperties

instance FromCStruct PhysicalDeviceIDProperties

