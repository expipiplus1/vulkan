{-# language CPP #-}
module Vulkan.Core10.LayerDiscovery  (LayerProperties) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data LayerProperties

instance ToCStruct LayerProperties
instance Show LayerProperties

instance FromCStruct LayerProperties

