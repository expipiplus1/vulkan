{-# language CPP #-}
-- | = Name
--
-- VK_KHR_draw_indirect_count - device extension
--
-- == VK_KHR_draw_indirect_count
--
-- [__Name String__]
--     @VK_KHR_draw_indirect_count@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     170
--
-- [__Revision__]
--     1
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
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_draw_indirect_count] @pdaniell-nv%0A<<Here describe the issue or question you have about the VK_KHR_draw_indirect_count extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-08-25
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Derrick Owens, AMD
--
--     -   Graham Sellers, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Dominik Witczak, AMD
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension is based off the @VK_AMD_draw_indirect_count@ extension.
-- This extension allows an application to source the number of draws for
-- indirect drawing calls from a buffer.
--
-- Applications might want to do culling on the GPU via a compute shader
-- prior to drawing. This enables the application to generate an arbitrary
-- number of drawing commands and execute them without host intervention.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. However, if Vulkan 1.2 is supported and this
-- extension is not, the entry points
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount'
-- and
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount'
-- are optional. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'cmdDrawIndexedIndirectCountKHR'
--
-- -   'cmdDrawIndirectCountKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME'
--
-- -   'KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 1, 2017-08-25 (Piers Daniell)
--
--     -   Initial draft based off VK_AMD_draw_indirect_count
--
-- == See Also
--
-- 'cmdDrawIndexedIndirectCountKHR', 'cmdDrawIndirectCountKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_draw_indirect_count Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_draw_indirect_count  ( cmdDrawIndirectCountKHR
                                                     , cmdDrawIndexedIndirectCountKHR
                                                     , KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
                                                     , pattern KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION
                                                     , KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
                                                     , pattern KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count (cmdDrawIndexedIndirectCount)
import Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count (cmdDrawIndirectCount)
-- No documentation found for TopLevel "vkCmdDrawIndirectCountKHR"
cmdDrawIndirectCountKHR = cmdDrawIndirectCount


-- No documentation found for TopLevel "vkCmdDrawIndexedIndirectCountKHR"
cmdDrawIndexedIndirectCountKHR = cmdDrawIndexedIndirectCount


type KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DRAW_INDIRECT_COUNT_SPEC_VERSION = 1


type KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_KHR_draw_indirect_count"

-- No documentation found for TopLevel "VK_KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_KHR_draw_indirect_count"

