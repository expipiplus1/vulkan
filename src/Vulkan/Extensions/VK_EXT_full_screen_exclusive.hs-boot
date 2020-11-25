{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_full_screen_exclusive"
module Vulkan.Extensions.VK_EXT_full_screen_exclusive  ( SurfaceCapabilitiesFullScreenExclusiveEXT
                                                       , SurfaceFullScreenExclusiveInfoEXT
                                                       , SurfaceFullScreenExclusiveWin32InfoEXT
                                                       ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data SurfaceCapabilitiesFullScreenExclusiveEXT

instance ToCStruct SurfaceCapabilitiesFullScreenExclusiveEXT
instance Show SurfaceCapabilitiesFullScreenExclusiveEXT

instance FromCStruct SurfaceCapabilitiesFullScreenExclusiveEXT


data SurfaceFullScreenExclusiveInfoEXT

instance ToCStruct SurfaceFullScreenExclusiveInfoEXT
instance Show SurfaceFullScreenExclusiveInfoEXT

instance FromCStruct SurfaceFullScreenExclusiveInfoEXT


data SurfaceFullScreenExclusiveWin32InfoEXT

instance ToCStruct SurfaceFullScreenExclusiveWin32InfoEXT
instance Show SurfaceFullScreenExclusiveWin32InfoEXT

instance FromCStruct SurfaceFullScreenExclusiveWin32InfoEXT

