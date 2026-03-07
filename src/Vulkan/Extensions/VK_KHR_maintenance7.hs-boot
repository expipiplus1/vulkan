{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance7 - device extension
--
-- = VK_KHR_maintenance7
--
-- [__Name String__]
--     @VK_KHR_maintenance7@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     563
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance7] @zmike%0A*Here describe the issue or question you have about the VK_KHR_maintenance7 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance7.adoc VK_KHR_maintenance7>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-01-30
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Pan Gao, Huawei
--
--     -   Tobias Hector, AMD
--
--     -   Jon Leech, Khronos
--
--     -   Daniel Story, Nintendo
--
--     -   Shahbaz Youssefi, Google
--
--     -   Yiwei Zhang, Google
--
--     -   Matthew Netsch, Qualcomm
--
-- == Description
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance7 VK_KHR_maintenance7>
-- adds a collection of minor features, none of which would warrant an
-- entire extension of their own.
--
-- The proposed new features are as follows:
--
-- -   Add a property query to determine if a framebuffer writes to depth
--     or stencil aspect does not trigger a write access in the sibling
--     aspect. For example, this allows sampling stencil aspect as a
--     texture while rendering to the sibling depth attachment and
--     vice-versa given appropriate image layouts.
--
-- -   Add a way to query information regarding the underlying devices in
--     environments where the Vulkan implementation is provided through
--     layered implementations. For example, running on Mesa\/Venus, driver
--     ID is returned as
--     'Vulkan.Core12.Enums.DriverId.DRIVER_ID_MESA_VENUS', but it can be
--     necessary to know what the real driver under the hood is. The new
--     'PhysicalDeviceLayeredApiPropertiesKHR' structure can be used to
--     gather information regarding layers underneath the top-level
--     physical device.
--
-- -   Promote
--     'Vulkan.Extensions.VK_EXT_nested_command_buffer.RENDERING_CONTENTS_INLINE_BIT_EXT'
--     and
--     'Vulkan.Extensions.VK_EXT_nested_command_buffer.SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_EXT'
--     to KHR
--
-- -   Add a limit to report the maximum total count of dynamic uniform
--     buffers and dynamic storage buffers that can be included in a
--     pipeline layout.
--
-- -   Require that for an unsigned integer query, the 32-bit result value
--     /must/ be equal to the 32 least significant bits of the equivalent
--     64-bit result value.
--
-- -   Add query for robust access support when using fragment shading rate
--     attachments
--
-- == New Structures
--
-- -   'PhysicalDeviceLayeredApiPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance7FeaturesKHR'
--
-- -   Extending 'PhysicalDeviceLayeredApiPropertiesKHR':
--
--     -   'PhysicalDeviceLayeredApiVulkanPropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceLayeredApiPropertiesListKHR'
--
--     -   'PhysicalDeviceMaintenance7PropertiesKHR'
--
-- == New Enums
--
-- -   'PhysicalDeviceLayeredApiKHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_7_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_7_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.RenderingFlagBits.RenderingFlagBits':
--
--     -   'Vulkan.Core13.Enums.RenderingFlagBits.RENDERING_CONTENTS_INLINE_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_PROPERTIES_LIST_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_API_VULKAN_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_7_PROPERTIES_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.SubpassContents.SubpassContents':
--
--     -   'Vulkan.Core10.Enums.SubpassContents.SUBPASS_CONTENTS_INLINE_AND_SECONDARY_COMMAND_BUFFERS_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2024-01-30 (Jon Leech)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_maintenance7 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance7  ( PhysicalDeviceLayeredApiPropertiesKHR
                                              , PhysicalDeviceLayeredApiPropertiesListKHR
                                              , PhysicalDeviceLayeredApiVulkanPropertiesKHR
                                              , PhysicalDeviceMaintenance7FeaturesKHR
                                              , PhysicalDeviceMaintenance7PropertiesKHR
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role PhysicalDeviceLayeredApiPropertiesKHR nominal
data PhysicalDeviceLayeredApiPropertiesKHR (es :: [Type])

instance ( Extendss PhysicalDeviceLayeredApiPropertiesKHR es
         , PokeChain es ) => ToCStruct (PhysicalDeviceLayeredApiPropertiesKHR es)
instance Show (Chain es) => Show (PhysicalDeviceLayeredApiPropertiesKHR es)

instance ( Extendss PhysicalDeviceLayeredApiPropertiesKHR es
         , PeekChain es ) => FromCStruct (PhysicalDeviceLayeredApiPropertiesKHR es)


data PhysicalDeviceLayeredApiPropertiesListKHR

instance ToCStruct PhysicalDeviceLayeredApiPropertiesListKHR
instance Show PhysicalDeviceLayeredApiPropertiesListKHR

instance FromCStruct PhysicalDeviceLayeredApiPropertiesListKHR


data PhysicalDeviceLayeredApiVulkanPropertiesKHR

instance ToCStruct PhysicalDeviceLayeredApiVulkanPropertiesKHR
instance Show PhysicalDeviceLayeredApiVulkanPropertiesKHR

instance FromCStruct PhysicalDeviceLayeredApiVulkanPropertiesKHR


data PhysicalDeviceMaintenance7FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance7FeaturesKHR
instance Show PhysicalDeviceMaintenance7FeaturesKHR

instance FromCStruct PhysicalDeviceMaintenance7FeaturesKHR


data PhysicalDeviceMaintenance7PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance7PropertiesKHR
instance Show PhysicalDeviceMaintenance7PropertiesKHR

instance FromCStruct PhysicalDeviceMaintenance7PropertiesKHR

