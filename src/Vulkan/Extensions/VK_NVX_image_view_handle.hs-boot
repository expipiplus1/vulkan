{-# language CPP #-}
-- | = Name
--
-- VK_NVX_image_view_handle - device extension
--
-- == VK_NVX_image_view_handle
--
-- [__Name String__]
--     @VK_NVX_image_view_handle@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     31
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NVX_image_view_handle] @ewerness-nv%0A*Here describe the issue or question you have about the VK_NVX_image_view_handle extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-04-03
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension allows applications to query an opaque handle from an
-- image view for use as a sampled image or storage image. This provides no
-- direct functionality itself.
--
-- == New Commands
--
-- -   'getImageViewAddressNVX'
--
-- -   'getImageViewHandleNVX'
--
-- == New Structures
--
-- -   'ImageViewAddressPropertiesNVX'
--
-- -   'ImageViewHandleInfoNVX'
--
-- == New Enum Constants
--
-- -   'NVX_IMAGE_VIEW_HANDLE_EXTENSION_NAME'
--
-- -   'NVX_IMAGE_VIEW_HANDLE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX'
--
-- == Version History
--
-- -   Revision 2, 2020-04-03 (Piers Daniell)
--
--     -   Add 'getImageViewAddressNVX'
--
-- -   Revision 1, 2018-12-07 (Eric Werness)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'ImageViewAddressPropertiesNVX', 'ImageViewHandleInfoNVX',
-- 'getImageViewAddressNVX', 'getImageViewHandleNVX'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NVX_image_view_handle Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NVX_image_view_handle  ( ImageViewAddressPropertiesNVX
                                                   , ImageViewHandleInfoNVX
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImageViewAddressPropertiesNVX

instance ToCStruct ImageViewAddressPropertiesNVX
instance Show ImageViewAddressPropertiesNVX

instance FromCStruct ImageViewAddressPropertiesNVX


data ImageViewHandleInfoNVX

instance ToCStruct ImageViewHandleInfoNVX
instance Show ImageViewHandleInfoNVX

instance FromCStruct ImageViewHandleInfoNVX

