{-# language CPP #-}
-- | = Name
--
-- VK_GOOGLE_hlsl_functionality1 - device extension
--
-- = Registered Extension Number
--
-- 224
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
--     2018-07-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         {spirv}\/GOOGLE\/SPV_GOOGLE_hlsl_functionality1.html[@SPV_GOOGLE_hlsl_functionality1@]
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
-- == Version History
--
-- -   Revision 1, 2018-07-09 (Neil Henning)
--
--     -   Initial draft
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_GOOGLE_hlsl_functionality1 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_GOOGLE_hlsl_functionality1  ( GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION
                                                        , pattern GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION
                                                        , GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME
                                                        , pattern GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)

type GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION"
pattern GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION :: forall a . Integral a => a
pattern GOOGLE_HLSL_FUNCTIONALITY1_SPEC_VERSION = 1


type GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME = "VK_GOOGLE_hlsl_functionality1"

-- No documentation found for TopLevel "VK_GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME"
pattern GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GOOGLE_HLSL_FUNCTIONALITY1_EXTENSION_NAME = "VK_GOOGLE_hlsl_functionality1"

