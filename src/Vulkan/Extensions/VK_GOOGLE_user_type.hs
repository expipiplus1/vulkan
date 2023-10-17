{-# language CPP #-}
-- | = Name
--
-- VK_GOOGLE_user_type - device extension
--
-- == VK_GOOGLE_user_type
--
-- [__Name String__]
--     @VK_GOOGLE_user_type@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     290
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Kaye Mason
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_GOOGLE_user_type] @chaleur%0A*Here describe the issue or question you have about the VK_GOOGLE_user_type extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-07-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/GOOGLE/SPV_GOOGLE_user_type.html SPV_GOOGLE_user_type>
--
-- [__Contributors__]
--
--     -   Kaye Mason, Google
--
--     -   Hai Nguyen, Google
--
-- == Description
--
-- The @VK_GOOGLE_user_type@ extension allows use of the
-- @SPV_GOOGLE_user_type@ extension in SPIR-V shader modules.
--
-- == New Enum Constants
--
-- -   'GOOGLE_USER_TYPE_EXTENSION_NAME'
--
-- -   'GOOGLE_USER_TYPE_SPEC_VERSION'
--
-- == Version History
--
-- -   Revision 1, 2019-09-07 (Kaye Mason)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_GOOGLE_user_type Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_GOOGLE_user_type  ( GOOGLE_USER_TYPE_SPEC_VERSION
                                              , pattern GOOGLE_USER_TYPE_SPEC_VERSION
                                              , GOOGLE_USER_TYPE_EXTENSION_NAME
                                              , pattern GOOGLE_USER_TYPE_EXTENSION_NAME
                                              ) where

import Data.String (IsString)

type GOOGLE_USER_TYPE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GOOGLE_USER_TYPE_SPEC_VERSION"
pattern GOOGLE_USER_TYPE_SPEC_VERSION :: forall a . Integral a => a
pattern GOOGLE_USER_TYPE_SPEC_VERSION = 1


type GOOGLE_USER_TYPE_EXTENSION_NAME = "VK_GOOGLE_user_type"

-- No documentation found for TopLevel "VK_GOOGLE_USER_TYPE_EXTENSION_NAME"
pattern GOOGLE_USER_TYPE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GOOGLE_USER_TYPE_EXTENSION_NAME = "VK_GOOGLE_user_type"

