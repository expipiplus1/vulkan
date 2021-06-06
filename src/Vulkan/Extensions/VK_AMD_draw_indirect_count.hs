{-# language CPP #-}
-- | = Name
--
-- VK_AMD_draw_indirect_count - device extension
--
-- == VK_AMD_draw_indirect_count
--
-- [__Name String__]
--     @VK_AMD_draw_indirect_count@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     34
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to @VK_KHR_draw_indirect_count@ extension
--
--         -   Which in turn was /promoted/ to
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_AMD_draw_indirect_count:%20&body=@drakos-amd%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-08-23
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to
--         <VK_KHR_draw_indirect_count.html VK_KHR_draw_indirect_count>
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
-- == Description
--
-- This extension allows an application to source the number of draws for
-- indirect drawing commands from a buffer. This enables applications to
-- generate an arbitrary number of drawing commands and execute them
-- without host intervention.
--
-- == Promotion to @VK_KHR_draw_indirect_count@
--
-- All functionality in this extension is included in
-- @VK_KHR_draw_indirect_count@, with the suffix changed to KHR. The
-- original type, enum and command names are still available as aliases of
-- the core functionality.
--
-- == New Commands
--
-- -   'cmdDrawIndexedIndirectCountAMD'
--
-- -   'cmdDrawIndirectCountAMD'
--
-- == New Enum Constants
--
-- -   'AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME'
--
-- -   'AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 2, 2016-08-23 (Dominik Witczak)
--
--     -   Minor fixes
--
-- -   Revision 1, 2016-07-21 (Matthaeus Chajdas)
--
--     -   Initial draft
--
-- = See Also
--
-- 'cmdDrawIndexedIndirectCountAMD', 'cmdDrawIndirectCountAMD'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_draw_indirect_count Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_draw_indirect_count  ( cmdDrawIndirectCountAMD
                                                     , cmdDrawIndexedIndirectCountAMD
                                                     , AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
                                                     , pattern AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION
                                                     , AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
                                                     , pattern AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count (cmdDrawIndexedIndirectCount)
import Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count (cmdDrawIndirectCount)
-- No documentation found for TopLevel "vkCmdDrawIndirectCountAMD"
cmdDrawIndirectCountAMD = cmdDrawIndirectCount


-- No documentation found for TopLevel "vkCmdDrawIndexedIndirectCountAMD"
cmdDrawIndexedIndirectCountAMD = cmdDrawIndexedIndirectCount


type AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION"
pattern AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_DRAW_INDIRECT_COUNT_SPEC_VERSION = 2


type AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_AMD_draw_indirect_count"

-- No documentation found for TopLevel "VK_AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME"
pattern AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_DRAW_INDIRECT_COUNT_EXTENSION_NAME = "VK_AMD_draw_indirect_count"

