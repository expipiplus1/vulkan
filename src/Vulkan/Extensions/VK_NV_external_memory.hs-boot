{-# language CPP #-}
module Vulkan.Extensions.VK_NV_external_memory  ( ExportMemoryAllocateInfoNV
                                                , ExternalMemoryImageCreateInfoNV
                                                ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ExportMemoryAllocateInfoNV

instance ToCStruct ExportMemoryAllocateInfoNV
instance Show ExportMemoryAllocateInfoNV

instance FromCStruct ExportMemoryAllocateInfoNV


data ExternalMemoryImageCreateInfoNV

instance ToCStruct ExternalMemoryImageCreateInfoNV
instance Show ExternalMemoryImageCreateInfoNV

instance FromCStruct ExternalMemoryImageCreateInfoNV

