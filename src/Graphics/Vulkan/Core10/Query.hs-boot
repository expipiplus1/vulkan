{-# language CPP #-}
module Graphics.Vulkan.Core10.Query  (QueryPoolCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role QueryPoolCreateInfo nominal
data QueryPoolCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (QueryPoolCreateInfo es)
instance Show (Chain es) => Show (QueryPoolCreateInfo es)

instance PeekChain es => FromCStruct (QueryPoolCreateInfo es)

