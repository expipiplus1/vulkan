{-# language CPP #-}
module Vulkan.Core10.MemoryManagement  (MemoryRequirements) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data MemoryRequirements

instance ToCStruct MemoryRequirements
instance Show MemoryRequirements

instance FromCStruct MemoryRequirements

