{-# language CPP #-}
-- No documentation found for Chapter "Fence"
module Vulkan.Core10.Fence  (FenceCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role FenceCreateInfo nominal
data FenceCreateInfo (es :: [Type])

instance ( Extendss FenceCreateInfo es
         , PokeChain es ) => ToCStruct (FenceCreateInfo es)
instance Show (Chain es) => Show (FenceCreateInfo es)

instance ( Extendss FenceCreateInfo es
         , PeekChain es ) => FromCStruct (FenceCreateInfo es)

