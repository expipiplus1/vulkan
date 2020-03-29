{-# language CPP #-}
module Graphics.Vulkan.Core10.Fence  (FenceCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role FenceCreateInfo nominal
data FenceCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (FenceCreateInfo es)
instance Show (Chain es) => Show (FenceCreateInfo es)

instance PeekChain es => FromCStruct (FenceCreateInfo es)

