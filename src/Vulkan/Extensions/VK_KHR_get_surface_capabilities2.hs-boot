{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_get_surface_capabilities2"
module Vulkan.Extensions.VK_KHR_get_surface_capabilities2  ( PhysicalDeviceSurfaceInfo2KHR
                                                           , SurfaceCapabilities2KHR
                                                           , SurfaceFormat2KHR
                                                           ) where

import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct (ToCStruct)
type role PhysicalDeviceSurfaceInfo2KHR nominal
data PhysicalDeviceSurfaceInfo2KHR (es :: [Type])

instance (Extendss PhysicalDeviceSurfaceInfo2KHR es, PokeChain es) => ToCStruct (PhysicalDeviceSurfaceInfo2KHR es)
instance Show (Chain es) => Show (PhysicalDeviceSurfaceInfo2KHR es)

instance (Extendss PhysicalDeviceSurfaceInfo2KHR es, PeekChain es) => FromCStruct (PhysicalDeviceSurfaceInfo2KHR es)


type role SurfaceCapabilities2KHR nominal
data SurfaceCapabilities2KHR (es :: [Type])

instance (Extendss SurfaceCapabilities2KHR es, PokeChain es) => ToCStruct (SurfaceCapabilities2KHR es)
instance Show (Chain es) => Show (SurfaceCapabilities2KHR es)

instance (Extendss SurfaceCapabilities2KHR es, PeekChain es) => FromCStruct (SurfaceCapabilities2KHR es)


data SurfaceFormat2KHR

instance ToCStruct SurfaceFormat2KHR
instance Show SurfaceFormat2KHR

instance FromCStruct SurfaceFormat2KHR

