{-# language CPP #-}
module Vulkan.Core10.AllocationCallbacks  (AllocationCallbacks) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data AllocationCallbacks

instance ToCStruct AllocationCallbacks
instance Show AllocationCallbacks

instance FromCStruct AllocationCallbacks

