{-# language CPP #-}
-- No documentation found for Chapter "LayerDiscovery"
module Vulkan.Core10.LayerDiscovery  (LayerProperties) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data LayerProperties

instance ToCStruct LayerProperties
instance Show LayerProperties

instance FromCStruct LayerProperties

