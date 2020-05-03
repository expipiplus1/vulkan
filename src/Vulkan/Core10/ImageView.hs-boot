{-# language CPP #-}
module Vulkan.Core10.ImageView  ( ComponentMapping
                                , ImageViewCreateInfo
                                ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
data ComponentMapping

instance ToCStruct ComponentMapping
instance Show ComponentMapping

instance FromCStruct ComponentMapping


type role ImageViewCreateInfo nominal
data ImageViewCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (ImageViewCreateInfo es)
instance Show (Chain es) => Show (ImageViewCreateInfo es)

instance PeekChain es => FromCStruct (ImageViewCreateInfo es)

