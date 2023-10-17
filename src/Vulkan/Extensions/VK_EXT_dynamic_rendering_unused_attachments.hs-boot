{-# language CPP #-}
-- | = Name
--
-- VK_EXT_dynamic_rendering_unused_attachments - device extension
--
-- == VK_EXT_dynamic_rendering_unused_attachments
--
-- [__Name String__]
--     @VK_EXT_dynamic_rendering_unused_attachments@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     500
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Version 1.3>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_dynamic_rendering_unused_attachments] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_dynamic_rendering_unused_attachments extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_dynamic_rendering_unused_attachments.adoc VK_EXT_dynamic_rendering_unused_attachments>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Story, Nintendo
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   James Fitzpatrick, Imagination Technologies
--
--     -   Pan Gao, Huawei Technologies
--
--     -   Ricardo Garcia, Igalia
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- This extension lifts some restrictions in the @VK_KHR_dynamic_rendering@
-- extension to allow render pass instances and bound pipelines within
-- those render pass instances to have an unused attachment specified in
-- one but not the other. It also allows pipelines to use different formats
-- in a render pass as long the attachment is NULL.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_EXTENSION_NAME'
--
-- -   'EXT_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DYNAMIC_RENDERING_UNUSED_ATTACHMENTS_FEATURES_EXT'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-05-22 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_dynamic_rendering_unused_attachments Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_dynamic_rendering_unused_attachments  (PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT

instance ToCStruct PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT
instance Show PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT

instance FromCStruct PhysicalDeviceDynamicRenderingUnusedAttachmentsFeaturesEXT

