{-# language CPP #-}
-- No documentation found for Chapter "MemoryManagement"
module Vulkan.Core10.MemoryManagement  (MemoryRequirements) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data MemoryRequirements

instance ToCStruct MemoryRequirements
instance Show MemoryRequirements

instance FromCStruct MemoryRequirements

