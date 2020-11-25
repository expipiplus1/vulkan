{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_non_semantic_info - device extension
--
-- = Registered Extension Number
--
-- 294
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-10-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         {spirv}\/KHR\/SPV_KHR_non_semantic_info.html[@SPV_KHR_non_semantic_info@]
--
-- [__Contributors__]
--
--     -   Baldur Karlsson, Valve
--
-- == Description
--
-- This extension allows the use of the @SPV_KHR_non_semantic_info@
-- extension in SPIR-V shader modules.
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME'
--
-- -   'KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 1, 2019-10-16 (Baldur Karlsson)
--
--     -   Initial revision
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_non_semantic_info Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_non_semantic_info  ( KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION
                                                          , pattern KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION
                                                          , KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
                                                          , pattern KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)

type KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION"
pattern KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION = 1


type KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME = "VK_KHR_shader_non_semantic_info"

-- No documentation found for TopLevel "VK_KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME"
pattern KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME = "VK_KHR_shader_non_semantic_info"

