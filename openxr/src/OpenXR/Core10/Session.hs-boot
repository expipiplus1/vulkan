{-# language CPP #-}
-- No documentation found for Chapter "Session"
module OpenXR.Core10.Session  (SessionBeginInfo) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Extendss)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PeekChain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PokeChain)
type role SessionBeginInfo nominal
data SessionBeginInfo (es :: [Type])

instance ( Extendss SessionBeginInfo es
         , PokeChain es ) => ToCStruct (SessionBeginInfo es)
instance Show (Chain es) => Show (SessionBeginInfo es)

instance ( Extendss SessionBeginInfo es
         , PeekChain es ) => FromCStruct (SessionBeginInfo es)

