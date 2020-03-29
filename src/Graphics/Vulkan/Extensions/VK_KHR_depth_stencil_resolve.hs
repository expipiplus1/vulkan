{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_depth_stencil_resolve  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
                                                                , pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
                                                                , pattern RESOLVE_MODE_NONE_KHR
                                                                , pattern RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR
                                                                , pattern RESOLVE_MODE_AVERAGE_BIT_KHR
                                                                , pattern RESOLVE_MODE_MIN_BIT_KHR
                                                                , pattern RESOLVE_MODE_MAX_BIT_KHR
                                                                , ResolveModeFlagsKHR
                                                                , ResolveModeFlagBitsKHR
                                                                , PhysicalDeviceDepthStencilResolvePropertiesKHR
                                                                , SubpassDescriptionDepthStencilResolveKHR
                                                                , KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION
                                                                , pattern KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION
                                                                , KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
                                                                , pattern KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME
                                                                ) where

import Data.String (IsString)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (PhysicalDeviceDepthStencilResolveProperties)
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits)
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Graphics.Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (SubpassDescriptionDepthStencilResolve)
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(RESOLVE_MODE_AVERAGE_BIT))
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(RESOLVE_MODE_MAX_BIT))
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(RESOLVE_MODE_MIN_BIT))
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(RESOLVE_MODE_NONE))
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Graphics.Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(RESOLVE_MODE_SAMPLE_ZERO_BIT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR"
pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR = STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE


-- No documentation found for TopLevel "VK_RESOLVE_MODE_NONE_KHR"
pattern RESOLVE_MODE_NONE_KHR = RESOLVE_MODE_NONE


-- No documentation found for TopLevel "VK_RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR"
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR = RESOLVE_MODE_SAMPLE_ZERO_BIT


-- No documentation found for TopLevel "VK_RESOLVE_MODE_AVERAGE_BIT_KHR"
pattern RESOLVE_MODE_AVERAGE_BIT_KHR = RESOLVE_MODE_AVERAGE_BIT


-- No documentation found for TopLevel "VK_RESOLVE_MODE_MIN_BIT_KHR"
pattern RESOLVE_MODE_MIN_BIT_KHR = RESOLVE_MODE_MIN_BIT


-- No documentation found for TopLevel "VK_RESOLVE_MODE_MAX_BIT_KHR"
pattern RESOLVE_MODE_MAX_BIT_KHR = RESOLVE_MODE_MAX_BIT


-- No documentation found for TopLevel "VkResolveModeFlagsKHR"
type ResolveModeFlagsKHR = ResolveModeFlags


-- No documentation found for TopLevel "VkResolveModeFlagBitsKHR"
type ResolveModeFlagBitsKHR = ResolveModeFlagBits


-- No documentation found for TopLevel "VkPhysicalDeviceDepthStencilResolvePropertiesKHR"
type PhysicalDeviceDepthStencilResolvePropertiesKHR = PhysicalDeviceDepthStencilResolveProperties


-- No documentation found for TopLevel "VkSubpassDescriptionDepthStencilResolveKHR"
type SubpassDescriptionDepthStencilResolveKHR = SubpassDescriptionDepthStencilResolve


type KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION"
pattern KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION = 1


type KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME = "VK_KHR_depth_stencil_resolve"

-- No documentation found for TopLevel "VK_KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME"
pattern KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME = "VK_KHR_depth_stencil_resolve"

