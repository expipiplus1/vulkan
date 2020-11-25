{-# language CPP #-}
-- No documentation found for Chapter "Image"
module Vulkan.Core10.Image  ( ImageCreateInfo
                            , SubresourceLayout
                            ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role ImageCreateInfo nominal
data ImageCreateInfo (es :: [Type])

instance (Extendss ImageCreateInfo es, PokeChain es) => ToCStruct (ImageCreateInfo es)
instance Show (Chain es) => Show (ImageCreateInfo es)

instance (Extendss ImageCreateInfo es, PeekChain es) => FromCStruct (ImageCreateInfo es)


data SubresourceLayout

instance ToCStruct SubresourceLayout
instance Show SubresourceLayout

instance FromCStruct SubresourceLayout

