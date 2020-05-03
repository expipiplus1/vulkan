{-# language CPP #-}
module Vulkan.Core10.BufferView  (BufferViewCreateInfo) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data BufferViewCreateInfo

instance ToCStruct BufferViewCreateInfo
instance Show BufferViewCreateInfo

instance FromCStruct BufferViewCreateInfo

