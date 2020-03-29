{-# language CPP #-}
module Graphics.Vulkan.Core10.Device  ( DeviceCreateInfo
                                      , DeviceQueueCreateInfo
                                      ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
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

