{-# language CPP #-}
module Vulkan.Core10.Shader  (ShaderModuleCreateInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role ShaderModuleCreateInfo nominal
data ShaderModuleCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (ShaderModuleCreateInfo es)
instance Show (Chain es) => Show (ShaderModuleCreateInfo es)

instance PeekChain es => FromCStruct (ShaderModuleCreateInfo es)

