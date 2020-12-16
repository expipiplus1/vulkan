{-# language CPP #-}
-- No documentation found for Chapter "Shader"
module Vulkan.Core10.Shader  (ShaderModuleCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role ShaderModuleCreateInfo nominal
data ShaderModuleCreateInfo (es :: [Type])

instance (Extendss ShaderModuleCreateInfo es, PokeChain es) => ToCStruct (ShaderModuleCreateInfo es)
instance Show (Chain es) => Show (ShaderModuleCreateInfo es)

instance (Extendss ShaderModuleCreateInfo es, PeekChain es) => FromCStruct (ShaderModuleCreateInfo es)

