{-# language CPP #-}
-- No documentation found for Chapter "BufferView"
module Vulkan.Core10.BufferView  (BufferViewCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data BufferViewCreateInfo

instance ToCStruct BufferViewCreateInfo
instance Show BufferViewCreateInfo

instance FromCStruct BufferViewCreateInfo

