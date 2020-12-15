{-# language CPP #-}
-- No documentation found for Chapter "Instance"
module OpenXR.Core10.Instance  ( ApiLayerProperties
                               , ApplicationInfo
                               , EventDataBuffer
                               , ExtensionProperties
                               , InstanceCreateInfo
                               , InstanceProperties
                               ) where

import Data.Kind (Type)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Chain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct (FromCStruct)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PeekChain)
import {-# SOURCE #-} OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct (ToCStruct)
data ApiLayerProperties

instance ToCStruct ApiLayerProperties
instance Show ApiLayerProperties

instance FromCStruct ApiLayerProperties


data ApplicationInfo

instance ToCStruct ApplicationInfo
instance Show ApplicationInfo

instance FromCStruct ApplicationInfo


data EventDataBuffer

instance ToCStruct EventDataBuffer
instance Show EventDataBuffer

instance FromCStruct EventDataBuffer


data ExtensionProperties

instance ToCStruct ExtensionProperties
instance Show ExtensionProperties

instance FromCStruct ExtensionProperties


type role InstanceCreateInfo nominal
data InstanceCreateInfo (es :: [Type])

instance (Extendss InstanceCreateInfo es, PokeChain es) => ToCStruct (InstanceCreateInfo es)
instance Show (Chain es) => Show (InstanceCreateInfo es)

instance (Extendss InstanceCreateInfo es, PeekChain es) => FromCStruct (InstanceCreateInfo es)


data InstanceProperties

instance ToCStruct InstanceProperties
instance Show InstanceProperties

instance FromCStruct InstanceProperties

