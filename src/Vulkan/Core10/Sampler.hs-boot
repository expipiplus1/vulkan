{-# language CPP #-}
module Vulkan.Core10.Sampler  (SamplerCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role SamplerCreateInfo nominal
data SamplerCreateInfo (es :: [Type])

instance (Extendss SamplerCreateInfo es, PokeChain es) => ToCStruct (SamplerCreateInfo es)
instance Show (Chain es) => Show (SamplerCreateInfo es)

instance (Extendss SamplerCreateInfo es, PeekChain es) => FromCStruct (SamplerCreateInfo es)

