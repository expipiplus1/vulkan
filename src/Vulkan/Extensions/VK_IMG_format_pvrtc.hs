{-# language CPP #-}
-- | = Name
--
-- VK_IMG_format_pvrtc - device extension
--
-- = Registered Extension Number
--
-- 55
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
--     2019-09-02
--
-- [__IP Status__]
--     Imagination Technologies Proprietary
--
-- [__Contributors__]
--
--     -   Stuart Smith, Imagination Technologies
--
-- == Description
--
-- @VK_IMG_format_pvrtc@ provides additional texture compression
-- functionality specific to Imagination Technologies PowerVR Texture
-- compression format (called PVRTC).
--
-- == New Enum Constants
--
-- -   'IMG_FORMAT_PVRTC_EXTENSION_NAME'
--
-- -   'IMG_FORMAT_PVRTC_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG'
--
-- == Version History
--
-- -   Revision 1, 2019-09-02 (Stuart Smith)
--
--     -   Initial version
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_IMG_format_pvrtc Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_IMG_format_pvrtc  ( IMG_FORMAT_PVRTC_SPEC_VERSION
                                              , pattern IMG_FORMAT_PVRTC_SPEC_VERSION
                                              , IMG_FORMAT_PVRTC_EXTENSION_NAME
                                              , pattern IMG_FORMAT_PVRTC_EXTENSION_NAME
                                              ) where

import Data.String (IsString)

type IMG_FORMAT_PVRTC_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_IMG_FORMAT_PVRTC_SPEC_VERSION"
pattern IMG_FORMAT_PVRTC_SPEC_VERSION :: forall a . Integral a => a
pattern IMG_FORMAT_PVRTC_SPEC_VERSION = 1


type IMG_FORMAT_PVRTC_EXTENSION_NAME = "VK_IMG_format_pvrtc"

-- No documentation found for TopLevel "VK_IMG_FORMAT_PVRTC_EXTENSION_NAME"
pattern IMG_FORMAT_PVRTC_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern IMG_FORMAT_PVRTC_EXTENSION_NAME = "VK_IMG_format_pvrtc"

