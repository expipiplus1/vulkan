{-# language CPP #-}
module Vulkan.Core10.Buffer  (BufferCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role BufferCreateInfo nominal
data BufferCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (BufferCreateInfo es)
instance Show (Chain es) => Show (BufferCreateInfo es)

instance PeekChain es => FromCStruct (BufferCreateInfo es)

