{-# language CPP #-}
-- No documentation found for Chapter "Query"
module Vulkan.Core10.Query  (QueryPoolCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role QueryPoolCreateInfo nominal
data QueryPoolCreateInfo (es :: [Type])

instance (Extendss QueryPoolCreateInfo es, PokeChain es) => ToCStruct (QueryPoolCreateInfo es)
instance Show (Chain es) => Show (QueryPoolCreateInfo es)

instance (Extendss QueryPoolCreateInfo es, PeekChain es) => FromCStruct (QueryPoolCreateInfo es)

