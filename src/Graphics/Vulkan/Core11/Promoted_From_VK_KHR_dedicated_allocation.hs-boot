{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation  ( MemoryDedicatedAllocateInfo
                                                                         , MemoryDedicatedRequirements
                                                                         ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data MemoryDedicatedAllocateInfo

instance ToCStruct MemoryDedicatedAllocateInfo
instance Show MemoryDedicatedAllocateInfo

instance FromCStruct MemoryDedicatedAllocateInfo


data MemoryDedicatedRequirements

instance ToCStruct MemoryDedicatedRequirements
instance Show MemoryDedicatedRequirements

instance FromCStruct MemoryDedicatedRequirements

