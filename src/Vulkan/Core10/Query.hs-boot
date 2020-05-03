{-# language CPP #-}
module Vulkan.Core10.Query  (QueryPoolCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role QueryPoolCreateInfo nominal
data QueryPoolCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (QueryPoolCreateInfo es)
instance Show (Chain es) => Show (QueryPoolCreateInfo es)

instance PeekChain es => FromCStruct (QueryPoolCreateInfo es)

