{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_non_semantic_info - device extension
--
-- == VK_KHR_shader_non_semantic_info
--
-- [__Name String__]
--     @VK_KHR_shader_non_semantic_info@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     294
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_non_semantic_info.html SPV_KHR_non_semantic_info>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Baldur Karlsson
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_non_semantic_info] @baldurk%0A*Here describe the issue or question you have about the VK_KHR_shader_non_semantic_info extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-10-16
--
-- [__IP Status__]
--     No known IP claims.
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
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3 Because
-- the extension has no API controlling its functionality, this results
-- only in a change to the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-extensions-table SPIR-V Extensions table>.
--
-- == Version History
--
-- -   Revision 1, 2019-10-16 (Baldur Karlsson)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_non_semantic_info Vulkan Specification>
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

