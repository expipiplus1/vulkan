{-# language CPP #-}
-- No documentation found for Chapter "Haptics"
module OpenXR.Core10.Haptics  ( HapticActionInfo
                              , HapticBaseHeader
                              , SomeHapticBaseHeader
                              ) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data HapticActionInfo

instance ToCStruct HapticActionInfo
instance Show HapticActionInfo

instance FromCStruct HapticActionInfo


data HapticBaseHeader

data SomeHapticBaseHeader

instance ToCStruct HapticBaseHeader
instance Show HapticBaseHeader

instance FromCStruct HapticBaseHeader

