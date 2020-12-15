{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_external_memory"
module Vulkan.Core11.Promoted_From_VK_KHR_external_memory  ( ExportMemoryAllocateInfo
                                                           , ExternalMemoryBufferCreateInfo
                                                           , ExternalMemoryImageCreateInfo
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ExportMemoryAllocateInfo

instance ToCStruct ExportMemoryAllocateInfo
instance Show ExportMemoryAllocateInfo

instance FromCStruct ExportMemoryAllocateInfo


data ExternalMemoryBufferCreateInfo

instance ToCStruct ExternalMemoryBufferCreateInfo
instance Show ExternalMemoryBufferCreateInfo

instance FromCStruct ExternalMemoryBufferCreateInfo


data ExternalMemoryImageCreateInfo

instance ToCStruct ExternalMemoryImageCreateInfo
instance Show ExternalMemoryImageCreateInfo

instance FromCStruct ExternalMemoryImageCreateInfo

