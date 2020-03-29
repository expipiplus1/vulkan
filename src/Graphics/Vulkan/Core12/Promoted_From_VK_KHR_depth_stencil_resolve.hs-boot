{-# language CPP #-}
module Graphics.Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve  ( PhysicalDeviceDepthStencilResolveProperties
                                                                          , SubpassDescriptionDepthStencilResolve
                                                                          ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceDepthStencilResolveProperties

instance ToCStruct PhysicalDeviceDepthStencilResolveProperties
instance Show PhysicalDeviceDepthStencilResolveProperties

instance FromCStruct PhysicalDeviceDepthStencilResolveProperties


data SubpassDescriptionDepthStencilResolve

instance ToCStruct SubpassDescriptionDepthStencilResolve
instance Show SubpassDescriptionDepthStencilResolve

instance FromCStruct SubpassDescriptionDepthStencilResolve

