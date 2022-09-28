{-# language CPP #-}
-- No documentation found for Chapter "ImageView"
module Vulkan.Core10.ImageView  ( ComponentMapping
                                , ImageSubresourceRange
                                , ImageViewCreateInfo
                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data ComponentMapping

instance ToCStruct ComponentMapping
instance Show ComponentMapping

instance FromCStruct ComponentMapping


data ImageSubresourceRange

instance ToCStruct ImageSubresourceRange
instance Show ImageSubresourceRange

instance FromCStruct ImageSubresourceRange


type role ImageViewCreateInfo nominal
data ImageViewCreateInfo (es :: [Type])

instance ( Extendss ImageViewCreateInfo es
         , PokeChain es ) => ToCStruct (ImageViewCreateInfo es)
instance Show (Chain es) => Show (ImageViewCreateInfo es)

instance ( Extendss ImageViewCreateInfo es
         , PeekChain es ) => FromCStruct (ImageViewCreateInfo es)

