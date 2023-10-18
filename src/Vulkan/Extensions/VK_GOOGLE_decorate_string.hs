{-# language CPP #-}
-- | = Name
--
-- VK_GOOGLE_decorate_string - device extension
--
-- == VK_GOOGLE_decorate_string
--
-- [__Name String__]
--     @VK_GOOGLE_decorate_string@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     225
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Hai Nguyen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_GOOGLE_decorate_string] @chaoticbob%0A*Here describe the issue or question you have about the VK_GOOGLE_decorate_string extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-07-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/GOOGLE/SPV_GOOGLE_decorate_string.html SPV_GOOGLE_decorate_string>
--
-- [__Contributors__]
--
--     -   Hai Nguyen, Google
--
--     -   Neil Henning, AMD
--
-- == Description
--
-- The @VK_GOOGLE_decorate_string@ extension allows use of the
-- @SPV_GOOGLE_decorate_string@ extension in SPIR-V shader modules.
--
-- == New Enum Constants
--
-- -   'GOOGLE_DECORATE_STRING_EXTENSION_NAME'
--
-- -   'GOOGLE_DECORATE_STRING_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 1, 2018-07-09 (Neil Henning)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_GOOGLE_decorate_string Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_GOOGLE_decorate_string  ( GOOGLE_DECORATE_STRING_SPEC_VERSION
                                                    , pattern GOOGLE_DECORATE_STRING_SPEC_VERSION
                                                    , GOOGLE_DECORATE_STRING_EXTENSION_NAME
                                                    , pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME
                                                    ) where

import Data.String (IsString)

type GOOGLE_DECORATE_STRING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GOOGLE_DECORATE_STRING_SPEC_VERSION"
pattern GOOGLE_DECORATE_STRING_SPEC_VERSION :: forall a . Integral a => a
pattern GOOGLE_DECORATE_STRING_SPEC_VERSION = 1


type GOOGLE_DECORATE_STRING_EXTENSION_NAME = "VK_GOOGLE_decorate_string"

-- No documentation found for TopLevel "VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME"
pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME = "VK_GOOGLE_decorate_string"

