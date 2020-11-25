{-# language CPP #-}
-- | = Name
--
-- VK_KHR_relaxed_block_layout - device extension
--
-- = Registered Extension Number
--
-- 145
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- = Deprecation state
--
-- -   /Promoted/ to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-03-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   John Kessenich, Google
--
-- == Description
--
-- The @VK_KHR_relaxed_block_layout@ extension allows implementations to
-- indicate they can support more variation in block @Offset@ decorations.
-- For example, placing a vector of three floats at an offset of 16×N + 4.
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources-layout Offset and Stride Assignment>
-- for details.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Enum Constants
--
-- -   'KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME'
--
-- -   'KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 1, 2017-03-26 (JohnK)
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_relaxed_block_layout Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_relaxed_block_layout  ( KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION
                                                      , pattern KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION
                                                      , KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME
                                                      , pattern KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME
                                                      ) where

import Data.String (IsString)

type KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION"
pattern KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION = 1


type KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME = "VK_KHR_relaxed_block_layout"

-- No documentation found for TopLevel "VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME"
pattern KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME = "VK_KHR_relaxed_block_layout"

