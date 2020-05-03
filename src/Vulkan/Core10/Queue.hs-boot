{-# language CPP #-}
module Vulkan.Core10.Queue  (SubmitInfo) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role SubmitInfo nominal
data SubmitInfo (es :: [Type])

instance PokeChain es => ToCStruct (SubmitInfo es)
instance Show (Chain es) => Show (SubmitInfo es)

instance PeekChain es => FromCStruct (SubmitInfo es)

