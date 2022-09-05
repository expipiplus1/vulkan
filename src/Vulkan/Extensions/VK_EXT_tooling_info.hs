{-# language CPP #-}
-- | = Name
--
-- VK_EXT_tooling_info - device extension
--
-- == VK_EXT_tooling_info
--
-- [__Name String__]
--     @VK_EXT_tooling_info@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     246
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_tooling_info] @tobski%0A<<Here describe the issue or question you have about the VK_EXT_tooling_info extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-11-05
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
--
-- [__Contributors__]
--
--     -   Rolando Caloca
--
--     -   Matthaeus Chajdas
--
--     -   Baldur Karlsson
--
--     -   Daniel Rakos
--
-- == Description
--
-- When an error occurs during application development, a common question
-- is \"What tools are actually running right now?\" This extension adds
-- the ability to query that information directly from the Vulkan
-- implementation.
--
-- Outdated versions of one tool might not play nicely with another, or
-- perhaps a tool is not actually running when it should have been. Trying
-- to figure that out can cause headaches as it is necessary to consult
-- each known tool to figure out what is going on — in some cases the tool
-- might not even be known.
--
-- Typically, the expectation is that developers will simply print out this
-- information for visual inspection when an issue occurs, however a small
-- amount of semantic information about what the tool is doing is provided
-- to help identify it programmatically. For example, if the advertised
-- limits or features of an implementation are unexpected, is there a tool
-- active which modifies these limits? Or if an application is providing
-- debug markers, but the implementation is not actually doing anything
-- with that information, this can quickly point that out.
--
-- == New Commands
--
-- -   'getPhysicalDeviceToolPropertiesEXT'
--
-- == New Structures
--
-- -   'PhysicalDeviceToolPropertiesEXT'
--
-- == New Enums
--
-- -   'ToolPurposeFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'ToolPurposeFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_TOOLING_INFO_EXTENSION_NAME'
--
-- -   'EXT_TOOLING_INFO_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_marker VK_EXT_debug_marker>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.ToolPurposeFlagBits.ToolPurposeFlagBits':
--
--     -   'Vulkan.Core13.Enums.ToolPurposeFlagBits.TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_report VK_EXT_debug_report>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.ToolPurposeFlagBits.ToolPurposeFlagBits':
--
--     -   'Vulkan.Core13.Enums.ToolPurposeFlagBits.TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_utils VK_EXT_debug_utils>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.ToolPurposeFlagBits.ToolPurposeFlagBits':
--
--     -   'Vulkan.Core13.Enums.ToolPurposeFlagBits.TOOL_PURPOSE_DEBUG_MARKERS_BIT_EXT'
--
--     -   'Vulkan.Core13.Enums.ToolPurposeFlagBits.TOOL_PURPOSE_DEBUG_REPORTING_BIT_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- EXT suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == Examples
--
-- __Printing Tool Information__
--
-- > uint32_t num_tools;
-- > VkPhysicalDeviceToolPropertiesEXT *pToolProperties;
-- > vkGetPhysicalDeviceToolPropertiesEXT(physicalDevice, &num_tools, NULL);
-- >
-- > pToolProperties = (VkPhysicalDeviceToolPropertiesEXT*)malloc(sizeof(VkPhysicalDeviceToolPropertiesEXT) * num_tools);
-- >
-- > vkGetPhysicalDeviceToolPropertiesEXT(physicalDevice, &num_tools, pToolProperties);
-- >
-- > for (int i = 0; i < num_tools; ++i) {
-- >     printf("%s:\n", pToolProperties[i].name);
-- >     printf("Version:\n");
-- >     printf("%s:\n", pToolProperties[i].version);
-- >     printf("Description:\n");
-- >     printf("\t%s\n", pToolProperties[i].description);
-- >     printf("Purposes:\n");
-- >     printf("\t%s\n", VkToolPurposeFlagBitsEXT_to_string(pToolProperties[i].purposes));
-- >     if (strnlen_s(pToolProperties[i].layer,VK_MAX_EXTENSION_NAME_SIZE) > 0) {
-- >         printf("Corresponding Layer:\n");
-- >         printf("\t%s\n", pToolProperties[i].layer);
-- >     }
-- > }
--
-- == Issues
--
-- 1) Why is this information separate from the layer mechanism?
--
-- Some tooling may be built into a driver, or be part of the Vulkan loader
-- etc. Tying this information directly to layers would have been awkward
-- at best.
--
-- == Version History
--
-- -   Revision 1, 2018-11-05 (Tobias Hector)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceToolPropertiesEXT', 'ToolPurposeFlagBitsEXT',
-- 'ToolPurposeFlagsEXT', 'getPhysicalDeviceToolPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_tooling_info Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_tooling_info  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT
                                              , getPhysicalDeviceToolPropertiesEXT
                                              , ToolPurposeFlagsEXT
                                              , ToolPurposeFlagBitsEXT
                                              , PhysicalDeviceToolPropertiesEXT
                                              , EXT_TOOLING_INFO_SPEC_VERSION
                                              , pattern EXT_TOOLING_INFO_SPEC_VERSION
                                              , EXT_TOOLING_INFO_EXTENSION_NAME
                                              , pattern EXT_TOOLING_INFO_EXTENSION_NAME
                                              ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_EXT_tooling_info (getPhysicalDeviceToolProperties)
import Vulkan.Core13.Promoted_From_VK_EXT_tooling_info (PhysicalDeviceToolProperties)
import Vulkan.Core13.Enums.ToolPurposeFlagBits (ToolPurposeFlagBits)
import Vulkan.Core13.Enums.ToolPurposeFlagBits (ToolPurposeFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES


-- No documentation found for TopLevel "vkGetPhysicalDeviceToolPropertiesEXT"
getPhysicalDeviceToolPropertiesEXT = getPhysicalDeviceToolProperties


-- No documentation found for TopLevel "VkToolPurposeFlagsEXT"
type ToolPurposeFlagsEXT = ToolPurposeFlags


-- No documentation found for TopLevel "VkToolPurposeFlagBitsEXT"
type ToolPurposeFlagBitsEXT = ToolPurposeFlagBits


-- No documentation found for TopLevel "VkPhysicalDeviceToolPropertiesEXT"
type PhysicalDeviceToolPropertiesEXT = PhysicalDeviceToolProperties


type EXT_TOOLING_INFO_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_TOOLING_INFO_SPEC_VERSION"
pattern EXT_TOOLING_INFO_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_TOOLING_INFO_SPEC_VERSION = 1


type EXT_TOOLING_INFO_EXTENSION_NAME = "VK_EXT_tooling_info"

-- No documentation found for TopLevel "VK_EXT_TOOLING_INFO_EXTENSION_NAME"
pattern EXT_TOOLING_INFO_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_TOOLING_INFO_EXTENSION_NAME = "VK_EXT_tooling_info"

