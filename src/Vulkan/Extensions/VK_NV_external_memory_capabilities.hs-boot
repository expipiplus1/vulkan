{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_external_memory_capabilities"
module Vulkan.Extensions.VK_NV_external_memory_capabilities  ( ExternalImageFormatPropertiesNV
                                                             , ExternalMemoryHandleTypeFlagsNV
                                                             , ExternalMemoryHandleTypeFlagBitsNV
                                                             ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ExternalImageFormatPropertiesNV

instance ToCStruct ExternalImageFormatPropertiesNV
instance Show ExternalImageFormatPropertiesNV

instance FromCStruct ExternalImageFormatPropertiesNV


type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV

data ExternalMemoryHandleTypeFlagBitsNV

