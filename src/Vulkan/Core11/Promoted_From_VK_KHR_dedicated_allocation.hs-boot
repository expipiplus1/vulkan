{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_dedicated_allocation"
module Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation  ( MemoryDedicatedAllocateInfo
                                                                , MemoryDedicatedRequirements
                                                                ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data MemoryDedicatedAllocateInfo

instance ToCStruct MemoryDedicatedAllocateInfo
instance Show MemoryDedicatedAllocateInfo

instance FromCStruct MemoryDedicatedAllocateInfo


data MemoryDedicatedRequirements

instance ToCStruct MemoryDedicatedRequirements
instance Show MemoryDedicatedRequirements

instance FromCStruct MemoryDedicatedRequirements

