{-# language CPP #-}
module Vulkan.Core10.CommandPool  (CommandPoolCreateInfo) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data CommandPoolCreateInfo

instance ToCStruct CommandPoolCreateInfo
instance Show CommandPoolCreateInfo

instance FromCStruct CommandPoolCreateInfo

