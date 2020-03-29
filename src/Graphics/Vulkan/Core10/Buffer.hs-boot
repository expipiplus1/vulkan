{-# language CPP #-}
module Graphics.Vulkan.Core10.Buffer  (BufferCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role BufferCreateInfo nominal
data BufferCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (BufferCreateInfo es)
instance Show (Chain es) => Show (BufferCreateInfo es)

instance PeekChain es => FromCStruct (BufferCreateInfo es)

