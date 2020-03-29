{-# language CPP #-}
module Graphics.Vulkan.Core10.ExtensionDiscovery  (ExtensionProperties) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data ExtensionProperties

instance ToCStruct ExtensionProperties
instance Show ExtensionProperties

instance FromCStruct ExtensionProperties

