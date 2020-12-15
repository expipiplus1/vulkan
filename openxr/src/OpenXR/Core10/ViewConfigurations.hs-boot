{-# language CPP #-}
-- No documentation found for Chapter "ViewConfigurations"
module OpenXR.Core10.ViewConfigurations  ( ViewConfigurationProperties
                                         , ViewConfigurationView
                                         ) where

import Data.Kind (Type)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct (FromCStruct)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PeekChain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct (ToCStruct)
data ViewConfigurationProperties

instance ToCStruct ViewConfigurationProperties
instance Show ViewConfigurationProperties

instance FromCStruct ViewConfigurationProperties


type role ViewConfigurationView nominal
data ViewConfigurationView (es :: [Type])

instance (Extendss ViewConfigurationView es, PokeChain es) => ToCStruct (ViewConfigurationView es)
instance Show (Chain es) => Show (ViewConfigurationView es)

instance (Extendss ViewConfigurationView es, PeekChain es) => FromCStruct (ViewConfigurationView es)

