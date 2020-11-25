{-# language CPP #-}
-- | = Name
--
-- VK_AMD_mixed_attachment_samples - device extension
--
-- = Registered Extension Number
--
-- 137
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
--     2017-07-24
--
-- [__Contributors__]
--
--     -   Mais Alnasser, AMD
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension enables applications to use multisampled rendering with a
-- depth\/stencil sample count that is larger than the color sample count.
-- Having a depth\/stencil sample count larger than the color sample count
-- allows maintaining geometry and coverage information at a higher sample
-- rate than color information. All samples are depth\/stencil tested, but
-- only the first color sample count number of samples get a corresponding
-- color output.
--
-- == New Enum Constants
--
-- -   'AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME'
--
-- -   'AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2017-07-24 (Daniel Rakos)
--
--     -   Internal revisions
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_mixed_attachment_samples Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_mixed_attachment_samples  ( AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION
                                                          , pattern AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION
                                                          , AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME
                                                          , pattern AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)

type AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION"
pattern AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION = 1


type AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME = "VK_AMD_mixed_attachment_samples"

-- No documentation found for TopLevel "VK_AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME"
pattern AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME = "VK_AMD_mixed_attachment_samples"

