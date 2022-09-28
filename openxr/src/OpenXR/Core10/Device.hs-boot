{-# language CPP #-}
-- No documentation found for Chapter "Device"
module OpenXR.Core10.Device  ( SessionCreateInfo
                             , SystemGetInfo
                             , SystemGraphicsProperties
                             , SystemProperties
                             , SystemTrackingProperties
                             , SystemId
                             ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Extendss)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PeekChain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PokeChain)
type role SessionCreateInfo nominal
data SessionCreateInfo (es :: [Type])

instance ( Extendss SessionCreateInfo es
         , PokeChain es ) => ToCStruct (SessionCreateInfo es)
instance Show (Chain es) => Show (SessionCreateInfo es)

instance ( Extendss SessionCreateInfo es
         , PeekChain es ) => FromCStruct (SessionCreateInfo es)


data SystemGetInfo

instance ToCStruct SystemGetInfo
instance Show SystemGetInfo

instance FromCStruct SystemGetInfo


data SystemGraphicsProperties

instance ToCStruct SystemGraphicsProperties
instance Show SystemGraphicsProperties

instance FromCStruct SystemGraphicsProperties


type role SystemProperties nominal
data SystemProperties (es :: [Type])

instance ( Extendss SystemProperties es
         , PokeChain es ) => ToCStruct (SystemProperties es)
instance Show (Chain es) => Show (SystemProperties es)

instance ( Extendss SystemProperties es
         , PeekChain es ) => FromCStruct (SystemProperties es)


data SystemTrackingProperties

instance ToCStruct SystemTrackingProperties
instance Show SystemTrackingProperties

instance FromCStruct SystemTrackingProperties


data SystemId

