{-# language CPP #-}
module Vulkan.Extensions.VK_NV_dedicated_allocation  ( DedicatedAllocationBufferCreateInfoNV
                                                     , DedicatedAllocationImageCreateInfoNV
                                                     , DedicatedAllocationMemoryAllocateInfoNV
                                                     ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DedicatedAllocationBufferCreateInfoNV

instance ToCStruct DedicatedAllocationBufferCreateInfoNV
instance Show DedicatedAllocationBufferCreateInfoNV

instance FromCStruct DedicatedAllocationBufferCreateInfoNV


data DedicatedAllocationImageCreateInfoNV

instance ToCStruct DedicatedAllocationImageCreateInfoNV
instance Show DedicatedAllocationImageCreateInfoNV

instance FromCStruct DedicatedAllocationImageCreateInfoNV


data DedicatedAllocationMemoryAllocateInfoNV

instance ToCStruct DedicatedAllocationMemoryAllocateInfoNV
instance Show DedicatedAllocationMemoryAllocateInfoNV

instance FromCStruct DedicatedAllocationMemoryAllocateInfoNV

