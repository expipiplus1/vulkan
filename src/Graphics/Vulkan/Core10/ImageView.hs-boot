{-# language CPP #-}
module Graphics.Vulkan.Core10.ImageView  ( ComponentMapping
                                         , ImageViewCreateInfo
                                         ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data ComponentMapping

instance ToCStruct ComponentMapping
instance Show ComponentMapping

instance FromCStruct ComponentMapping


type role ImageViewCreateInfo nominal
data ImageViewCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (ImageViewCreateInfo es)
instance Show (Chain es) => Show (ImageViewCreateInfo es)

instance PeekChain es => FromCStruct (ImageViewCreateInfo es)

