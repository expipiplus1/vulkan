{-# language CPP #-}
-- No documentation found for Chapter "AllocationCallbacks"
module Vulkan.Core10.AllocationCallbacks  (AllocationCallbacks) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AllocationCallbacks

instance ToCStruct AllocationCallbacks
instance Show AllocationCallbacks

instance FromCStruct AllocationCallbacks

