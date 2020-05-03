{-# language CPP #-}
module Vulkan.Core10.Sampler  (SamplerCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role SamplerCreateInfo nominal
data SamplerCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (SamplerCreateInfo es)
instance Show (Chain es) => Show (SamplerCreateInfo es)

instance PeekChain es => FromCStruct (SamplerCreateInfo es)

