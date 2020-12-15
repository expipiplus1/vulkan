{-# language CPP #-}
-- No documentation found for Chapter "Event"
module Vulkan.Core10.Event  (EventCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data EventCreateInfo

instance ToCStruct EventCreateInfo
instance Show EventCreateInfo

instance FromCStruct EventCreateInfo

