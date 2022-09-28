{-# language CPP #-}
-- No documentation found for Chapter "Event"
module Vulkan.Core10.Event  (EventCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role EventCreateInfo nominal
data EventCreateInfo (es :: [Type])

instance ( Extendss EventCreateInfo es
         , PokeChain es ) => ToCStruct (EventCreateInfo es)
instance Show (Chain es) => Show (EventCreateInfo es)

instance ( Extendss EventCreateInfo es
         , PeekChain es ) => FromCStruct (EventCreateInfo es)

