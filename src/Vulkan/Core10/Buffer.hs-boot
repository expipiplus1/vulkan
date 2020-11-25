{-# language CPP #-}
-- No documentation found for Chapter "Buffer"
module Vulkan.Core10.Buffer  (BufferCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role BufferCreateInfo nominal
data BufferCreateInfo (es :: [Type])

instance (Extendss BufferCreateInfo es, PokeChain es) => ToCStruct (BufferCreateInfo es)
instance Show (Chain es) => Show (BufferCreateInfo es)

instance (Extendss BufferCreateInfo es, PeekChain es) => FromCStruct (BufferCreateInfo es)

