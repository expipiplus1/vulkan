{-# language CPP #-}
module Graphics.Vulkan.Core10.BufferView  (BufferViewCreateInfo) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data BufferViewCreateInfo

instance ToCStruct BufferViewCreateInfo
instance Show BufferViewCreateInfo

instance FromCStruct BufferViewCreateInfo

