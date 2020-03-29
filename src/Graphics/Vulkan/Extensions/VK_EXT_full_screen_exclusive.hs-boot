{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive  ( SurfaceCapabilitiesFullScreenExclusiveEXT
                                                                , SurfaceFullScreenExclusiveInfoEXT
                                                                , SurfaceFullScreenExclusiveWin32InfoEXT
                                                                ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
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

