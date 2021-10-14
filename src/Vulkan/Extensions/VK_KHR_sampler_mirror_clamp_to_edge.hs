{-# language CPP #-}
-- | = Name
--
-- VK_KHR_sampler_mirror_clamp_to_edge - device extension
--
-- == VK_KHR_sampler_mirror_clamp_to_edge
--
-- [__Name String__]
--     @VK_KHR_sampler_mirror_clamp_to_edge@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     15
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_sampler_mirror_clamp_to_edge] @tobski%0A<<Here describe the issue or question you have about the VK_KHR_sampler_mirror_clamp_to_edge extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-08-17
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__Contributors__]
--
--     -   Tobias Hector, Imagination Technologies
--
--     -   Jon Leech, Khronos
--
-- == Description
--
-- @VK_KHR_sampler_mirror_clamp_to_edge@ extends the set of sampler address
-- modes to include an additional mode
-- ('Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE')
-- that effectively uses a texture map twice as large as the original image
-- in which the additional half of the new image is a mirror image of the
-- original image.
--
-- This new mode relaxes the need to generate images whose opposite edges
-- match by using the original image to generate a matching “mirror image”.
-- This mode allows the texture to be mirrored only once in the negative s,
-- t, and r directions.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2.
-- However, if Vulkan 1.2 is supported and this extension is not, the
-- 'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode'
-- 'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE'
-- is optional. Since the original extension did not use an author suffix
-- on the enum
-- 'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE',
-- it is used by both core and extension implementations.
--
-- == New Enum Constants
--
-- -   'KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME'
--
-- -   'KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode':
--
--     -   'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE'
--
-- == Example
--
-- Creating a sampler with the new address mode in each dimension
--
-- >     VkSamplerCreateInfo createInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO // sType
-- >         // Other members set to application-desired values
-- >     };
-- >
-- >     createInfo.addressModeU = VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE;
-- >     createInfo.addressModeV = VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE;
-- >     createInfo.addressModeW = VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE;
-- >
-- >     VkSampler sampler;
-- >     VkResult result = vkCreateSampler(
-- >         device,
-- >         &createInfo,
-- >         &sampler);
--
-- == Issues
--
-- 1) Why are both KHR and core versions of the
-- 'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE'
-- token present?
--
-- __RESOLVED__: This functionality was intended to be required in Vulkan
-- 1.0. We realized shortly before public release that not all
-- implementations could support it, and moved the functionality into an
-- optional extension, but did not apply the KHR extension suffix. Adding a
-- KHR-suffixed alias of the non-suffixed enum has been done to comply with
-- our own naming rules.
--
-- In a related change, before spec revision 1.1.121 this extension was
-- hardwiring into the spec Makefile so it was always included with the
-- Specification, even in the core-only versions. This has now been
-- reverted, and it is treated as any other extension.
--
-- == Version History
--
-- -   Revision 1, 2016-02-16 (Tobias Hector)
--
--     -   Initial draft
--
-- -   Revision 2, 2019-08-14 (Jon Leech)
--
--     -   Add KHR-suffixed alias of non-suffixed enum.
--
-- -   Revision 3, 2019-08-17 (Jon Leech)
--
--     -   Add an issue explaining the reason for the extension API not
--         being suffixed with KHR.
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_sampler_mirror_clamp_to_edge Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_sampler_mirror_clamp_to_edge  ( pattern SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE_KHR
                                                              , KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION
                                                              , pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION
                                                              , KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
                                                              , pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
                                                              ) where

import Data.String (IsString)
import Vulkan.Core10.Enums.SamplerAddressMode (SamplerAddressMode(SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE))
-- No documentation found for TopLevel "VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE_KHR"
pattern SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE_KHR = SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE


type KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION"
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 3


type KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME = "VK_KHR_sampler_mirror_clamp_to_edge"

-- No documentation found for TopLevel "VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME"
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME = "VK_KHR_sampler_mirror_clamp_to_edge"

