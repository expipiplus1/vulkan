{-# language CPP #-}
module Vulkan.Core10.QueueSemaphore  (SemaphoreCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role SemaphoreCreateInfo nominal
data SemaphoreCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (SemaphoreCreateInfo es)
instance Show (Chain es) => Show (SemaphoreCreateInfo es)

instance PeekChain es => FromCStruct (SemaphoreCreateInfo es)

