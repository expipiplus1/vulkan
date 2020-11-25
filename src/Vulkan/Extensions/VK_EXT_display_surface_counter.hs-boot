{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_display_surface_counter"
module Vulkan.Extensions.VK_EXT_display_surface_counter  ( SurfaceCapabilities2EXT
                                                         , SurfaceCounterFlagsEXT
                                                         , SurfaceCounterFlagBitsEXT
                                                         ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data SurfaceCapabilities2EXT

instance ToCStruct SurfaceCapabilities2EXT
instance Show SurfaceCapabilities2EXT

instance FromCStruct SurfaceCapabilities2EXT


type SurfaceCounterFlagsEXT = SurfaceCounterFlagBitsEXT

data SurfaceCounterFlagBitsEXT

