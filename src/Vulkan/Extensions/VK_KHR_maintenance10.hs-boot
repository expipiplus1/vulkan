{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance10 - device extension
--
-- = VK_KHR_maintenance10
--
-- [__Name String__]
--     @VK_KHR_maintenance10@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     631
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_3
--
--     -   Interacts with VK_VERSION_1_4
--
--     -   Interacts with VK_KHR_copy_commands2
--
--     -   Interacts with VK_KHR_dynamic_rendering
--
--     -   Interacts with VK_KHR_dynamic_rendering_local_read
--
--     -   Interacts with VK_KHR_format_feature_flags2
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance10] @zmike%0A*Here describe the issue or question you have about the VK_KHR_maintenance10 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance10.adoc VK_KHR_maintenance10>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-13
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_KHR_format_feature_flags2@
--
--     -   This extension interacts with @VK_EXT_extended_dynamic_state3@
--
--     -   This extension interacts with
--         @VK_KHR_dynamic_rendering_local_read@
--
--     -   This extension interacts with @VK_KHR_depth_stencil_resolve@
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Hans-Kristian Arntzen, Valve
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance10 VK_KHR_maintenance10>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The new features are as follows:
--
-- -   New image format feature bits that indicate support for copying
--     depth or stencil aspects using non-graphics queue families
--
-- -   If
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state3.cmdSetSampleMaskEXT'
--     is called with @pSampleMask@ set to @NULL@, it is treated as if the
--     mask has all bits set to @1@.
--
-- -   Add vkCmdEndRendering2KHR as an extensible version of
--     vkCmdEndRendering
--
-- -   Add input attachment information to dynamic rendering
--
-- -   Require that vertex inputs follow sRGB encoding when those formats
--     are used, instead of being underspecified.
--
-- -   Add a query to determine if sRGB images are resolved in nonlinear or
--     linear space by default
--
-- -   Add an optional feature to allow applications to override the
--     default sRGB resolve behavior
--
-- -   Add resolve mode and depth-stencil resolve support to
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdResolveImage2'
--     to bring it in-line with render pass attachment resolves
--
-- == New Commands
--
-- -   'cmdEndRendering2KHR'
--
-- == New Structures
--
-- -   'RenderingEndInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance10FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMaintenance10PropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingAttachmentInfo':
--
--     -   'RenderingAttachmentFlagsInfoKHR'
--
-- -   Extending
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.ResolveImageInfo2':
--
--     -   'ResolveImageModeInfoKHR'
--
-- == New Enums
--
-- -   'RenderingAttachmentFlagBitsKHR'
--
-- -   'ResolveImageFlagBitsKHR'
--
-- == New Bitmasks
--
-- -   'RenderingAttachmentFlagsKHR'
--
-- -   'ResolveImageFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_10_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_10_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.AttachmentDescriptionFlagBits':
--
--     -   'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.AttachmentDescriptionFlagBits.ATTACHMENT_DESCRIPTION_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_10_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_ATTACHMENT_FLAGS_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_END_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RESOLVE_IMAGE_MODE_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4 Vulkan Version 1.4>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering_local_read VK_KHR_dynamic_rendering_local_read>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending 'RenderingAttachmentFlagBitsKHR':
--
--     -   'RENDERING_ATTACHMENT_INPUT_ATTACHMENT_FEEDBACK_BIT_KHR'
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_LOCAL_READ_CONCURRENT_ACCESS_CONTROL_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_DEPTH_COPY_ON_COMPUTE_QUEUE_BIT_KHR'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_DEPTH_COPY_ON_TRANSFER_QUEUE_BIT_KHR'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STENCIL_COPY_ON_COMPUTE_QUEUE_BIT_KHR'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STENCIL_COPY_ON_TRANSFER_QUEUE_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_copy_commands2 VK_KHR_copy_commands2>
-- is supported:
--
-- -   Extending 'ResolveImageFlagBitsKHR':
--
--     -   'RESOLVE_IMAGE_ENABLE_TRANSFER_FUNCTION_BIT_KHR'
--
--     -   'RESOLVE_IMAGE_SKIP_TRANSFER_FUNCTION_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- is supported:
--
-- -   Extending 'RenderingAttachmentFlagBitsKHR':
--
--     -   'RENDERING_ATTACHMENT_RESOLVE_ENABLE_TRANSFER_FUNCTION_BIT_KHR'
--
--     -   'RENDERING_ATTACHMENT_RESOLVE_SKIP_TRANSFER_FUNCTION_BIT_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-05-13 (Mike Blumenkrantz)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_maintenance10 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance10  ( PhysicalDeviceMaintenance10FeaturesKHR
                                               , PhysicalDeviceMaintenance10PropertiesKHR
                                               , RenderingAttachmentFlagsInfoKHR
                                               , RenderingEndInfoKHR
                                               , ResolveImageModeInfoKHR
                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data PhysicalDeviceMaintenance10FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance10FeaturesKHR
instance Show PhysicalDeviceMaintenance10FeaturesKHR

instance FromCStruct PhysicalDeviceMaintenance10FeaturesKHR


data PhysicalDeviceMaintenance10PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance10PropertiesKHR
instance Show PhysicalDeviceMaintenance10PropertiesKHR

instance FromCStruct PhysicalDeviceMaintenance10PropertiesKHR


data RenderingAttachmentFlagsInfoKHR

instance ToCStruct RenderingAttachmentFlagsInfoKHR
instance Show RenderingAttachmentFlagsInfoKHR

instance FromCStruct RenderingAttachmentFlagsInfoKHR


type role RenderingEndInfoKHR nominal
data RenderingEndInfoKHR (es :: [Type])

instance ( Extendss RenderingEndInfoKHR es
         , PokeChain es ) => ToCStruct (RenderingEndInfoKHR es)
instance Show (Chain es) => Show (RenderingEndInfoKHR es)

instance ( Extendss RenderingEndInfoKHR es
         , PeekChain es ) => FromCStruct (RenderingEndInfoKHR es)


data ResolveImageModeInfoKHR

instance ToCStruct ResolveImageModeInfoKHR
instance Show ResolveImageModeInfoKHR

instance FromCStruct ResolveImageModeInfoKHR

