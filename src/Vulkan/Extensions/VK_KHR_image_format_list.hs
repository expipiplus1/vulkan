{-# language CPP #-}
-- | = Name
--
-- VK_KHR_image_format_list - device extension
--
-- == VK_KHR_image_format_list
--
-- [__Name String__]
--     @VK_KHR_image_format_list@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     148
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
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Faith Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_image_format_list] @gfxstrand%0A*Here describe the issue or question you have about the VK_KHR_image_format_list extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-03-20
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Faith Ekstrand, Intel
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jeff Leger, Qualcomm
--
--     -   Neil Henning, Codeplay
--
-- == Description
--
-- On some implementations, setting the
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- on image creation can cause access to that image to perform worse than
-- an equivalent image created without
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- because the implementation does not know what view formats will be
-- paired with the image.
--
-- This extension allows an application to provide the list of all formats
-- that /can/ be used with an image when it is created. The implementation
-- may then be able to create a more efficient image that supports the
-- subset of formats required by the application without having to support
-- all formats in the format compatibility class of the image format.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Image.ImageCreateInfo',
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2':
--
--     -   'ImageFormatListCreateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME'
--
-- -   'KHR_IMAGE_FORMAT_LIST_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2017-03-20 (Faith Ekstrand)
--
--     -   Initial revision
--
-- == See Also
--
-- 'ImageFormatListCreateInfoKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_image_format_list Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_image_format_list  ( pattern STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
                                                   , ImageFormatListCreateInfoKHR
                                                   , KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
                                                   , pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION
                                                   , KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
                                                   , pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME
                                                   ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_image_format_list (ImageFormatListCreateInfo)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR = STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO


-- No documentation found for TopLevel "VkImageFormatListCreateInfoKHR"
type ImageFormatListCreateInfoKHR = ImageFormatListCreateInfo


type KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_SPEC_VERSION"
pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_IMAGE_FORMAT_LIST_SPEC_VERSION = 1


type KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME = "VK_KHR_image_format_list"

-- No documentation found for TopLevel "VK_KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME"
pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_IMAGE_FORMAT_LIST_EXTENSION_NAME = "VK_KHR_image_format_list"

