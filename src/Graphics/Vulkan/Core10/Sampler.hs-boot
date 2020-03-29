{-# language CPP #-}
module Graphics.Vulkan.Core10.Sampler  (SamplerCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role SamplerCreateInfo nominal
data SamplerCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (SamplerCreateInfo es)
instance Show (Chain es) => Show (SamplerCreateInfo es)

instance PeekChain es => FromCStruct (SamplerCreateInfo es)

