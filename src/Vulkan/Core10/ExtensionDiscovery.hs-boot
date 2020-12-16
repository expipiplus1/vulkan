{-# language CPP #-}
-- No documentation found for Chapter "ExtensionDiscovery"
module Vulkan.Core10.ExtensionDiscovery  (ExtensionProperties) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ExtensionProperties

instance ToCStruct ExtensionProperties
instance Show ExtensionProperties

instance FromCStruct ExtensionProperties

