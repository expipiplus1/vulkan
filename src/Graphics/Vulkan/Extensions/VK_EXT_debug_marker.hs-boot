{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_debug_marker  ( DebugMarkerMarkerInfoEXT
                                                       , DebugMarkerObjectNameInfoEXT
                                                       , DebugMarkerObjectTagInfoEXT
                                                       ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data DebugMarkerMarkerInfoEXT

instance ToCStruct DebugMarkerMarkerInfoEXT
instance Show DebugMarkerMarkerInfoEXT

instance FromCStruct DebugMarkerMarkerInfoEXT


data DebugMarkerObjectNameInfoEXT

instance ToCStruct DebugMarkerObjectNameInfoEXT
instance Show DebugMarkerObjectNameInfoEXT

instance FromCStruct DebugMarkerObjectNameInfoEXT


data DebugMarkerObjectTagInfoEXT

instance ToCStruct DebugMarkerObjectTagInfoEXT
instance Show DebugMarkerObjectTagInfoEXT

instance FromCStruct DebugMarkerObjectTagInfoEXT

