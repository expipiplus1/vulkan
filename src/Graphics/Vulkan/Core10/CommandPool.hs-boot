{-# language CPP #-}
module Graphics.Vulkan.Core10.CommandPool  (CommandPoolCreateInfo) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data CommandPoolCreateInfo

instance ToCStruct CommandPoolCreateInfo
instance Show CommandPoolCreateInfo

instance FromCStruct CommandPoolCreateInfo

