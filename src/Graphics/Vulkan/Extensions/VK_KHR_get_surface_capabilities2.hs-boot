{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2  ( PhysicalDeviceSurfaceInfo2KHR
                                                                    , SurfaceCapabilities2KHR
                                                                    , SurfaceFormat2KHR
                                                                    ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
type role PhysicalDeviceSurfaceInfo2KHR nominal
data PhysicalDeviceSurfaceInfo2KHR (es :: [Type])

instance PokeChain es => ToCStruct (PhysicalDeviceSurfaceInfo2KHR es)
instance Show (Chain es) => Show (PhysicalDeviceSurfaceInfo2KHR es)

instance PeekChain es => FromCStruct (PhysicalDeviceSurfaceInfo2KHR es)


type role SurfaceCapabilities2KHR nominal
data SurfaceCapabilities2KHR (es :: [Type])

instance PokeChain es => ToCStruct (SurfaceCapabilities2KHR es)
instance Show (Chain es) => Show (SurfaceCapabilities2KHR es)

instance PeekChain es => FromCStruct (SurfaceCapabilities2KHR es)


data SurfaceFormat2KHR

instance ToCStruct SurfaceFormat2KHR
instance Show SurfaceFormat2KHR

instance FromCStruct SurfaceFormat2KHR

