{-# language CPP #-}
module Graphics.Vulkan.Core10.QueueSemaphore  (SemaphoreCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role SemaphoreCreateInfo nominal
data SemaphoreCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (SemaphoreCreateInfo es)
instance Show (Chain es) => Show (SemaphoreCreateInfo es)

instance PeekChain es => FromCStruct (SemaphoreCreateInfo es)

