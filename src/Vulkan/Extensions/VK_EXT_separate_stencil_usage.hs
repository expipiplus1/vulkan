{-# language CPP #-}
-- | = Name
--
-- VK_EXT_separate_stencil_usage - device extension
--
-- == VK_EXT_separate_stencil_usage
--
-- [__Name String__]
--     @VK_EXT_separate_stencil_usage@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     247
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_separate_stencil_usage:%20&body=@drakos-amd%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-11-08
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Rakos, AMD
--
--     -   Jordan Logan, AMD
--
-- == Description
--
-- This extension allows specifying separate usage flags for the stencil
-- aspect of images with a depth-stencil format at image creation time.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the EXT suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2':
--
--     -   'ImageStencilUsageCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME'
--
-- -   'EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-11-08 (Daniel Rakos)
--
--     -   Internal revisions.
--
-- = See Also
--
-- 'ImageStencilUsageCreateInfoEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_separate_stencil_usage Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_separate_stencil_usage  ( pattern STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
                                                        , ImageStencilUsageCreateInfoEXT
                                                        , EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
                                                        , pattern EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION
                                                        , EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
                                                        , pattern EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage (ImageStencilUsageCreateInfo)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT = STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO


-- No documentation found for TopLevel "VkImageStencilUsageCreateInfoEXT"
type ImageStencilUsageCreateInfoEXT = ImageStencilUsageCreateInfo


type EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION"
pattern EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SEPARATE_STENCIL_USAGE_SPEC_VERSION = 1


type EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME = "VK_EXT_separate_stencil_usage"

-- No documentation found for TopLevel "VK_EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME"
pattern EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SEPARATE_STENCIL_USAGE_EXTENSION_NAME = "VK_EXT_separate_stencil_usage"

