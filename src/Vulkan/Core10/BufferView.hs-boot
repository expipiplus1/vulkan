{-# language CPP #-}
-- No documentation found for Chapter "BufferView"
module Vulkan.Core10.BufferView  (BufferViewCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role BufferViewCreateInfo nominal
data BufferViewCreateInfo (es :: [Type])

instance ( Extendss BufferViewCreateInfo es
         , PokeChain es ) => ToCStruct (BufferViewCreateInfo es)
instance Show (Chain es) => Show (BufferViewCreateInfo es)

instance ( Extendss BufferViewCreateInfo es
         , PeekChain es ) => FromCStruct (BufferViewCreateInfo es)

