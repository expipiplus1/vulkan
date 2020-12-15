{-# language CPP #-}
-- No documentation found for Chapter "MemoryManagement"
module Vulkan.Core10.MemoryManagement  (MemoryRequirements) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MemoryRequirements

instance ToCStruct MemoryRequirements
instance Show MemoryRequirements

instance FromCStruct MemoryRequirements

