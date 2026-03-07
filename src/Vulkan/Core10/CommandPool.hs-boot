{-# language CPP #-}
-- No documentation found for Chapter "CommandPool"
module Vulkan.Core10.CommandPool  (CommandPoolCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role CommandPoolCreateInfo nominal
data CommandPoolCreateInfo (es :: [Type])

instance ( Extendss CommandPoolCreateInfo es
         , PokeChain es ) => ToCStruct (CommandPoolCreateInfo es)
instance Show (Chain es) => Show (CommandPoolCreateInfo es)

instance ( Extendss CommandPoolCreateInfo es
         , PeekChain es ) => FromCStruct (CommandPoolCreateInfo es)

