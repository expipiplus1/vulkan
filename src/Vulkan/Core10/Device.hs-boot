{-# language CPP #-}
-- No documentation found for Chapter "Device"
module Vulkan.Core10.Device  ( DeviceCreateInfo
                             , DeviceQueueCreateInfo
                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role DeviceCreateInfo nominal
data DeviceCreateInfo (es :: [Type])

instance (Extendss DeviceCreateInfo es, PokeChain es) => ToCStruct (DeviceCreateInfo es)
instance Show (Chain es) => Show (DeviceCreateInfo es)

instance (Extendss DeviceCreateInfo es, PeekChain es) => FromCStruct (DeviceCreateInfo es)


type role DeviceQueueCreateInfo nominal
data DeviceQueueCreateInfo (es :: [Type])

instance (Extendss DeviceQueueCreateInfo es, PokeChain es) => ToCStruct (DeviceQueueCreateInfo es)
instance Show (Chain es) => Show (DeviceQueueCreateInfo es)

instance (Extendss DeviceQueueCreateInfo es, PeekChain es) => FromCStruct (DeviceQueueCreateInfo es)

