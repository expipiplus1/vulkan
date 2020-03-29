{-# language CPP #-}
module Graphics.Vulkan.Core10.LayerDiscovery  (LayerProperties) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data LayerProperties

instance ToCStruct LayerProperties
instance Show LayerProperties

instance FromCStruct LayerProperties

