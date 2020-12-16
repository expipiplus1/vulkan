{-# language CPP #-}
-- No documentation found for Chapter "QueueSemaphore"
module Vulkan.Core10.QueueSemaphore  (SemaphoreCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role SemaphoreCreateInfo nominal
data SemaphoreCreateInfo (es :: [Type])

instance (Extendss SemaphoreCreateInfo es, PokeChain es) => ToCStruct (SemaphoreCreateInfo es)
instance Show (Chain es) => Show (SemaphoreCreateInfo es)

instance (Extendss SemaphoreCreateInfo es, PeekChain es) => FromCStruct (SemaphoreCreateInfo es)

