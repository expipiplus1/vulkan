{-# language CPP #-}
module Graphics.Vulkan.Core10.Image  ( ImageCreateInfo
                                     , ImageSubresource
                                     , SubresourceLayout
                                     ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role ImageCreateInfo nominal
data ImageCreateInfo (es :: [Type])

instance PokeChain es => ToCStruct (ImageCreateInfo es)
instance Show (Chain es) => Show (ImageCreateInfo es)

instance PeekChain es => FromCStruct (ImageCreateInfo es)


data ImageSubresource

instance ToCStruct ImageSubresource
instance Show ImageSubresource

instance FromCStruct ImageSubresource


data SubresourceLayout

instance ToCStruct SubresourceLayout
instance Show SubresourceLayout

instance FromCStruct SubresourceLayout

