{-# language CPP #-}
module Vulkan.Core10.Fence  (FenceCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role FenceCreateInfo nominal
data FenceCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (FenceCreateInfo es)
instance Show (Chain es) => Show (FenceCreateInfo es)

instance PeekChain es => FromCStruct (FenceCreateInfo es)

