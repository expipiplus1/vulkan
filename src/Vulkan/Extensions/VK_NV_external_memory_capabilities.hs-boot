{-# language CPP #-}
module Vulkan.Extensions.VK_NV_external_memory_capabilities  ( ExternalImageFormatPropertiesNV
                                                             , ExternalMemoryHandleTypeFlagBitsNV
                                                             , ExternalMemoryHandleTypeFlagsNV
                                                             ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ExternalImageFormatPropertiesNV

instance ToCStruct ExternalImageFormatPropertiesNV
instance Show ExternalImageFormatPropertiesNV

instance FromCStruct ExternalImageFormatPropertiesNV


data ExternalMemoryHandleTypeFlagBitsNV

type ExternalMemoryHandleTypeFlagsNV = ExternalMemoryHandleTypeFlagBitsNV

