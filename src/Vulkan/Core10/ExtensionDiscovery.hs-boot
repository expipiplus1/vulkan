{-# language CPP #-}
-- No documentation found for Chapter "ExtensionDiscovery"
module Vulkan.Core10.ExtensionDiscovery  (ExtensionProperties) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ExtensionProperties

instance ToCStruct ExtensionProperties
instance Show ExtensionProperties

instance FromCStruct ExtensionProperties

