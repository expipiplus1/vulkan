{-# language CPP #-}
module Graphics.Vulkan.Core10.AllocationCallbacks  (AllocationCallbacks) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data AllocationCallbacks

instance ToCStruct AllocationCallbacks
instance Show AllocationCallbacks

instance FromCStruct AllocationCallbacks

