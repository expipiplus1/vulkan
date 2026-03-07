{-# language CPP #-}
-- | = Name
--
-- VK_EXT_custom_resolve - device extension
--
-- = VK_EXT_custom_resolve
--
-- [__Name String__]
--     @VK_EXT_custom_resolve@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     629
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
--     -   Interacts with VK_KHR_dynamic_rendering
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_custom_resolve] @zmike%0A*Here describe the issue or question you have about the VK_EXT_custom_resolve extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_custom_resolve.adoc VK_EXT_custom_resolve>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-13
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_KHR_dynamic_rendering@
--
--     -   This extension interacts with
--         @VK_EXT_dynamic_rendering_unused_attachments@
--
--     -   This extension interacts with @VK_EXT_fragment_density_map@
--
--     -   This extension interacts with @VK_EXT_graphics_pipeline_library@
--
--     -   This extension interacts with @VK_EXT_shader_object@
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Connor Abbott, Valve
--
--     -   Samuel Pitoiset, Valve
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Ting Wei, ARM
--
--     -   Ricardo Garcia, Igalia
--
--     -   Spencer Fricke, LunarG
--
--     -   Piers Daniell, Nvidia
--
-- == Description
--
-- This extension provides functionality for using shaders to resolve
-- multisample rendering attachments.
--
-- It builds upon mechanics introduced by
-- VK_QCOM_render_pass_shader_resolve, additionally adding support for
-- dynamic rendering.
--
-- == New Commands
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- is supported:
--
-- -   'cmdBeginCustomResolveEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCustomResolveFeaturesEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- is supported:
--
-- -   'BeginCustomResolveInfoEXT'
--
-- -   Extending
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateInfoEXT':
--
--     -   'CustomResolveCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_CUSTOM_RESOLVE_EXTENSION_NAME'
--
-- -   'EXT_CUSTOM_RESOLVE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_RESOLVE_FEATURES_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits':
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_CUSTOM_RESOLVE_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CUSTOM_RESOLVE_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_FRAGMENT_REGION_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core12.Enums.ResolveModeFlagBits.ResolveModeFlagBits':
--
--     -   'Vulkan.Core12.Enums.ResolveModeFlagBits.RESOLVE_MODE_CUSTOM_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BEGIN_CUSTOM_RESOLVE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CUSTOM_RESOLVE_CREATE_INFO_EXT'
--
-- == Issues
--
-- 1) How will this work with shader objects?
--
-- Some vendors emit an epilog at the end of the FS that stores each
-- color\/depth\/stencil attachment to the appropriate tilebuffer location,
-- and to do that they need to know the layout of the tilebuffer which
-- depends on the attachment formats\/sample counts. We agreed that for
-- shader object the FS epilog is emitted dynamically when the draw
-- happens.
--
-- == Version History
--
-- -   Revision 1, 2025-05-13 (Mike Blumenkrantz)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_custom_resolve Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_custom_resolve  ( BeginCustomResolveInfoEXT
                                                , CustomResolveCreateInfoEXT
                                                , PhysicalDeviceCustomResolveFeaturesEXT
                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data BeginCustomResolveInfoEXT

instance ToCStruct BeginCustomResolveInfoEXT
instance Show BeginCustomResolveInfoEXT

instance FromCStruct BeginCustomResolveInfoEXT


data CustomResolveCreateInfoEXT

instance ToCStruct CustomResolveCreateInfoEXT
instance Show CustomResolveCreateInfoEXT

instance FromCStruct CustomResolveCreateInfoEXT


data PhysicalDeviceCustomResolveFeaturesEXT

instance ToCStruct PhysicalDeviceCustomResolveFeaturesEXT
instance Show PhysicalDeviceCustomResolveFeaturesEXT

instance FromCStruct PhysicalDeviceCustomResolveFeaturesEXT

