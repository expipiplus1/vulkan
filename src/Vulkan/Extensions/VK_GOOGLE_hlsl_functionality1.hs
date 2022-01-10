{-# language CPP #-}
-- | = Name
--
-- VK_GOOGLE_hlsl_functionality1 - device extension
--
-- == VK_GOOGLE_hlsl_functionality1
--
-- [__Name String__]
--     @VK_GOOGLE_hlsl_functionality1@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     224
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Hai Nguyen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_GOOGLE_hlsl_functionality1] @chaoticbob%0A<<Here describe the issue or question you have about the VK_GOOGLE_hlsl_functionality1 extension>> >
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
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/GOOGLE/SPV_GOOGLE_hlsl_functionality1.html SPV_GOOGLE_hlsl_functionality1>
--
-- [__Contributors__]
--
--     -   Hai Nguyen, Google
--
--     -   Neil Henning, AMD
--
-- == Description
--
-- The @VK_GOOGLE_hlsl_functionality1@ extension allows use of the
-- @SPV_GOOGLE_hlsl_functionality1@ extension in SPIR-V shader modules.
--
-- == New Enum Constants
--
-- -   'GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME'
--
-- -   'GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION'
--
-- -   'GOOGLE_HLSL_FUNCTIONALITY_1_EXTENSION_NAME'
--
-- -   'GOOGLE_HLSL_FUNCTIONALITY_1_SPEC_VERSION'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_GOOGLE_hlsl_functionality1 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_GOOGLE_hlsl_functionality1  ( pattern GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION
                                                        , pattern GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME
                                                        , GOOGLE_HLSL_FUNCTIONALITY_1_SPEC_VERSION
                                                        , pattern GOOGLE_HLSL_FUNCTIONALITY_1_SPEC_VERSION
                                                        , GOOGLE_HLSL_FUNCTIONALITY_1_EXTENSION_NAME
                                                        , pattern GOOGLE_HLSL_FUNCTIONALITY_1_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)

-- No documentation found for TopLevel "VK_GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION"
pattern GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION = GOOGLE_HLSL_FUNCTIONALITY_1_SPEC_VERSION


-- No documentation found for TopLevel "VK_GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME"
pattern GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME = GOOGLE_HLSL_FUNCTIONALITY_1_EXTENSION_NAME


type GOOGLE_HLSL_FUNCTIONALITY_1_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GOOGLE_HLSL_FUNCTIONALITY_1_SPEC_VERSION"
pattern GOOGLE_HLSL_FUNCTIONALITY_1_SPEC_VERSION :: forall a . Integral a => a
pattern GOOGLE_HLSL_FUNCTIONALITY_1_SPEC_VERSION = 1


type GOOGLE_HLSL_FUNCTIONALITY_1_EXTENSION_NAME = "VK_GOOGLE_hlsl_functionality1"

-- No documentation found for TopLevel "VK_GOOGLE_HLSL_FUNCTIONALITY_1_EXTENSION_NAME"
pattern GOOGLE_HLSL_FUNCTIONALITY_1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GOOGLE_HLSL_FUNCTIONALITY_1_EXTENSION_NAME = "VK_GOOGLE_hlsl_functionality1"

