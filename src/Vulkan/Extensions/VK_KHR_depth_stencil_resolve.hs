{-# language CPP #-}
-- | = Name
--
-- VK_KHR_depth_stencil_resolve - device extension
--
-- == VK_KHR_depth_stencil_resolve
--
-- [__Name String__]
--     @VK_KHR_depth_stencil_resolve@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     200
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_create_renderpass2@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_depth_stencil_resolve] @janharald%0A<<Here describe the issue or question you have about the VK_KHR_depth_stencil_resolve extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-04-09
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Andrew Garrard, Samsung Electronics
--
--     -   Soowan Park, Samsung Electronics
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension adds support for automatically resolving multisampled
-- depth\/stencil attachments in a subpass in a similar manner as for color
-- attachments.
--
-- Multisampled color attachments can be resolved at the end of a subpass
-- by specifying @pResolveAttachments@ entries corresponding to the
-- @pColorAttachments@ array entries. This does not allow for a way to map
-- the resolve attachments to the depth\/stencil attachment. The
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResolveImage' command does not
-- allow for depth\/stencil images. While there are other ways to resolve
-- the depth\/stencil attachment, they can give sub-optimal performance.
-- Extending the
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2'
-- in this extension allows an application to add a
-- @pDepthStencilResolveAttachment@, that is similar to the color
-- @pResolveAttachments@, that the @pDepthStencilAttachment@ can be
-- resolved into.
--
-- Depth and stencil samples are resolved to a single value based on the
-- resolve mode. The set of possible resolve modes is defined in the
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits' enum. The
-- 'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_SAMPLE_ZERO_BIT'
-- mode is the only mode that is required of all implementations (that
-- support the extension or support Vulkan 1.2 or higher). Some
-- implementations may also support averaging (the same as color sample
-- resolve) or taking the minimum or maximum sample, which may be more
-- suitable for depth\/stencil resolve.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDepthStencilResolvePropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2':
--
--     -   'SubpassDescriptionDepthStencilResolveKHR'
--
-- == New Enums
--
-- -   'ResolveModeFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'ResolveModeFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DEPTH_STENCIL_RESOLVE_EXTENSION_NAME'
--
-- -   'KHR_DEPTH_STENCIL_RESOLVE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits':
--
--     -   'RESOLVE_MODE_AVERAGE_BIT_KHR'
--
--     -   'RESOLVE_MODE_MAX_BIT_KHR'
--
--     -   'RESOLVE_MODE_MIN_BIT_KHR'
--
--     -   'RESOLVE_MODE_NONE_KHR'
--
--     -   'RESOLVE_MODE_SAMPLE_ZERO_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR'
--
--     -   'STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR'
--
-- == Version History
--
-- -   Revision 1, 2018-04-09 (Jan-Harald Fredriksen)
--
--     -   Initial revision
--
-- == See Also
--
-- 'PhysicalDeviceDepthStencilResolvePropertiesKHR',
-- 'ResolveModeFlagBitsKHR', 'ResolveModeFlagsKHR',
-- 'SubpassDescriptionDepthStencilResolveKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_depth_stencil_resolve Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_depth_stencil_resolve  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
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
import Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (PhysicalDeviceDepthStencilResolveProperties)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (SubpassDescriptionDepthStencilResolve)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(RESOLVE_MODE_AVERAGE_BIT))
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(RESOLVE_MODE_MAX_BIT))
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(RESOLVE_MODE_MIN_BIT))
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(RESOLVE_MODE_NONE))
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlags)
import Vulkan.Core12.Enums.ResolveModeFlagBits (ResolveModeFlagBits(RESOLVE_MODE_SAMPLE_ZERO_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE))
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

