{-# language CPP #-}
module Vulkan.Core10.Event  (EventCreateInfo) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data EventCreateInfo

instance ToCStruct EventCreateInfo
instance Show EventCreateInfo

instance FromCStruct EventCreateInfo

