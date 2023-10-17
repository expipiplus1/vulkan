{-# language CPP #-}
-- No documentation found for Chapter "CommandPool"
module Vulkan.Core10.CommandPool  (CommandPoolCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data CommandPoolCreateInfo

instance ToCStruct CommandPoolCreateInfo
instance Show CommandPoolCreateInfo

instance FromCStruct CommandPoolCreateInfo

