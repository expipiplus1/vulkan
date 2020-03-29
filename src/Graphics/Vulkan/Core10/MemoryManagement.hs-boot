{-# language CPP #-}
module Graphics.Vulkan.Core10.MemoryManagement  (MemoryRequirements) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data MemoryRequirements

instance ToCStruct MemoryRequirements
instance Show MemoryRequirements

instance FromCStruct MemoryRequirements

