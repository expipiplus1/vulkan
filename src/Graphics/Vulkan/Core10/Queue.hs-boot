{-# language CPP #-}
module Graphics.Vulkan.Core10.Queue  (SubmitInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role SubmitInfo nominal
data SubmitInfo (es :: [Type])

instance PokeChain es => ToCStruct (SubmitInfo es)
instance Show (Chain es) => Show (SubmitInfo es)

instance PeekChain es => FromCStruct (SubmitInfo es)

