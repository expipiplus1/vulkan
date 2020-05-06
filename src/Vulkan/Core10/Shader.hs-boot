{-# language CPP #-}
module Vulkan.Core10.Shader  (ShaderModuleCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role ShaderModuleCreateInfo nominal
data ShaderModuleCreateInfo (es :: [Type])

instance (Extendss ShaderModuleCreateInfo es, PokeChain es) => ToCStruct (ShaderModuleCreateInfo es)
instance Show (Chain es) => Show (ShaderModuleCreateInfo es)

instance (Extendss ShaderModuleCreateInfo es, PeekChain es) => FromCStruct (ShaderModuleCreateInfo es)

