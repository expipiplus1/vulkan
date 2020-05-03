{-# language CPP #-}
module Vulkan.Core10.Device  ( DeviceCreateInfo
                             , DeviceQueueCreateInfo
                             ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role DeviceCreateInfo nominal
data DeviceCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (DeviceCreateInfo es)
instance Show (Chain es) => Show (DeviceCreateInfo es)

instance PeekChain es => FromCStruct (DeviceCreateInfo es)


type role DeviceQueueCreateInfo nominal
data DeviceQueueCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (DeviceQueueCreateInfo es)
instance Show (Chain es) => Show (DeviceQueueCreateInfo es)

instance PeekChain es => FromCStruct (DeviceQueueCreateInfo es)

