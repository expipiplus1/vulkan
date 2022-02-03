{-# language CPP #-}
-- | = Name
--
-- VK_EXT_debug_marker - device extension
--
-- == VK_EXT_debug_marker
--
-- [__Name String__]
--     @VK_EXT_debug_marker@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     23
--
-- [__Revision__]
--     4
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_EXT_debug_report@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to @VK_EXT_debug_utils@ extension
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Debugging tools>
--
-- [__Contact__]
--
--     -   Baldur Karlsson
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_debug_marker] @baldurk%0A<<Here describe the issue or question you have about the VK_EXT_debug_marker extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-01-31
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Baldur Karlsson
--
--     -   Dan Ginsburg, Valve
--
--     -   Jon Ashburn, LunarG
--
--     -   Kyle Spagnoli, NVIDIA
--
-- == Description
--
-- The @VK_EXT_debug_marker@ extension is a device extension. It introduces
-- concepts of object naming and tagging, for better tracking of Vulkan
-- objects, as well as additional commands for recording annotations of
-- named sections of a workload to aid organization and offline analysis in
-- external tools.
--
-- == New Commands
--
-- -   'cmdDebugMarkerBeginEXT'
--
-- -   'cmdDebugMarkerEndEXT'
--
-- -   'cmdDebugMarkerInsertEXT'
--
-- -   'debugMarkerSetObjectNameEXT'
--
-- -   'debugMarkerSetObjectTagEXT'
--
-- == New Structures
--
-- -   'DebugMarkerMarkerInfoEXT'
--
-- -   'DebugMarkerObjectNameInfoEXT'
--
-- -   'DebugMarkerObjectTagInfoEXT'
--
-- == New Enums
--
-- -   'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEBUG_MARKER_EXTENSION_NAME'
--
-- -   'EXT_DEBUG_MARKER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT'
--
-- == Examples
--
-- __Example 1__
--
-- Associate a name with an image, for easier debugging in external tools
-- or with validation layers that can print a friendly name when referring
-- to objects in error messages.
--
-- >     extern VkDevice device;
-- >     extern VkImage image;
-- >
-- >     // Must call extension functions through a function pointer:
-- >     PFN_vkDebugMarkerSetObjectNameEXT pfnDebugMarkerSetObjectNameEXT = (PFN_vkDebugMarkerSetObjectNameEXT)vkGetDeviceProcAddr(device, "vkDebugMarkerSetObjectNameEXT");
-- >
-- >     // Set a name on the image
-- >     const VkDebugMarkerObjectNameInfoEXT imageNameInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT, // sType
-- >         NULL,                                           // pNext
-- >         VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT,          // objectType
-- >         (uint64_t)image,                                // object
-- >         "Brick Diffuse Texture",                        // pObjectName
-- >     };
-- >
-- >     pfnDebugMarkerSetObjectNameEXT(device, &imageNameInfo);
-- >
-- >     // A subsequent error might print:
-- >     //   Image 'Brick Diffuse Texture' (0xc0dec0dedeadbeef) is used in a
-- >     //   command buffer with no memory bound to it.
--
-- __Example 2__
--
-- Annotating regions of a workload with naming information so that offline
-- analysis tools can display a more usable visualisation of the commands
-- submitted.
--
-- >     extern VkDevice device;
-- >     extern VkCommandBuffer commandBuffer;
-- >
-- >     // Must call extension functions through a function pointer:
-- >     PFN_vkCmdDebugMarkerBeginEXT pfnCmdDebugMarkerBeginEXT = (PFN_vkCmdDebugMarkerBeginEXT)vkGetDeviceProcAddr(device, "vkCmdDebugMarkerBeginEXT");
-- >     PFN_vkCmdDebugMarkerEndEXT pfnCmdDebugMarkerEndEXT = (PFN_vkCmdDebugMarkerEndEXT)vkGetDeviceProcAddr(device, "vkCmdDebugMarkerEndEXT");
-- >     PFN_vkCmdDebugMarkerInsertEXT pfnCmdDebugMarkerInsertEXT = (PFN_vkCmdDebugMarkerInsertEXT)vkGetDeviceProcAddr(device, "vkCmdDebugMarkerInsertEXT");
-- >
-- >     // Describe the area being rendered
-- >     const VkDebugMarkerMarkerInfoEXT houseMarker =
-- >     {
-- >         VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT, // sType
-- >         NULL,                                           // pNext
-- >         "Brick House",                                  // pMarkerName
-- >         { 1.0f, 0.0f, 0.0f, 1.0f },                     // color
-- >     };
-- >
-- >     // Start an annotated group of calls under the 'Brick House' name
-- >     pfnCmdDebugMarkerBeginEXT(commandBuffer, &houseMarker);
-- >     {
-- >         // A mutable structure for each part being rendered
-- >         VkDebugMarkerMarkerInfoEXT housePartMarker =
-- >         {
-- >             VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT, // sType
-- >             NULL,                                           // pNext
-- >             NULL,                                           // pMarkerName
-- >             { 0.0f, 0.0f, 0.0f, 0.0f },                     // color
-- >         };
-- >
-- >         // Set the name and insert the marker
-- >         housePartMarker.pMarkerName = "Walls";
-- >         pfnCmdDebugMarkerInsertEXT(commandBuffer, &housePartMarker);
-- >
-- >         // Insert the drawcall for the walls
-- >         vkCmdDrawIndexed(commandBuffer, 1000, 1, 0, 0, 0);
-- >
-- >         // Insert a recursive region for two sets of windows
-- >         housePartMarker.pMarkerName = "Windows";
-- >         pfnCmdDebugMarkerBeginEXT(commandBuffer, &housePartMarker);
-- >         {
-- >             vkCmdDrawIndexed(commandBuffer, 75, 6, 1000, 0, 0);
-- >             vkCmdDrawIndexed(commandBuffer, 100, 2, 1450, 0, 0);
-- >         }
-- >         pfnCmdDebugMarkerEndEXT(commandBuffer);
-- >
-- >         housePartMarker.pMarkerName = "Front Door";
-- >         pfnCmdDebugMarkerInsertEXT(commandBuffer, &housePartMarker);
-- >
-- >         vkCmdDrawIndexed(commandBuffer, 350, 1, 1650, 0, 0);
-- >
-- >         housePartMarker.pMarkerName = "Roof";
-- >         pfnCmdDebugMarkerInsertEXT(commandBuffer, &housePartMarker);
-- >
-- >         vkCmdDrawIndexed(commandBuffer, 500, 1, 2000, 0, 0);
-- >     }
-- >     // End the house annotation started above
-- >     pfnCmdDebugMarkerEndEXT(commandBuffer);
--
-- == Issues
--
-- 1) Should the tag or name for an object be specified using the @pNext@
-- parameter in the object’s @Vk*CreateInfo@ structure?
--
-- __RESOLVED__: No. While this fits with other Vulkan patterns and would
-- allow more type safety and future proofing against future objects, it
-- has notable downsides. In particular passing the name at @Vk*CreateInfo@
-- time does not allow renaming, prevents late binding of naming
-- information, and does not allow naming of implicitly created objects
-- such as queues and swapchain images.
--
-- 2) Should the command annotation functions 'cmdDebugMarkerBeginEXT' and
-- 'cmdDebugMarkerEndEXT' support the ability to specify a color?
--
-- __RESOLVED__: Yes. The functions have been expanded to take an optional
-- color which can be used at will by implementations consuming the command
-- buffer annotations in their visualisation.
--
-- 3) Should the functions added in this extension accept an extensible
-- structure as their parameter for a more flexible API, as opposed to
-- direct function parameters? If so, which functions?
--
-- __RESOLVED__: Yes. All functions have been modified to take a structure
-- type with extensible @pNext@ pointer, to allow future extensions to add
-- additional annotation information in the same commands.
--
-- == Version History
--
-- -   Revision 1, 2016-02-24 (Baldur Karlsson)
--
--     -   Initial draft, based on LunarG marker spec
--
-- -   Revision 2, 2016-02-26 (Baldur Karlsson)
--
--     -   Renamed Dbg to DebugMarker in function names
--
--     -   Allow markers in secondary command buffers under certain
--         circumstances
--
--     -   Minor language tweaks and edits
--
-- -   Revision 3, 2016-04-23 (Baldur Karlsson)
--
--     -   Reorganise spec layout to closer match desired organisation
--
--     -   Added optional color to markers (both regions and inserted
--         labels)
--
--     -   Changed functions to take extensible structs instead of direct
--         function parameters
--
-- -   Revision 4, 2017-01-31 (Baldur Karlsson)
--
--     -   Added explicit dependency on VK_EXT_debug_report
--
--     -   Moved definition of
--         'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT'
--         to debug report chapter.
--
--     -   Fixed typo in dates in revision history
--
-- == See Also
--
-- 'DebugMarkerMarkerInfoEXT', 'DebugMarkerObjectNameInfoEXT',
-- 'DebugMarkerObjectTagInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT',
-- 'cmdDebugMarkerBeginEXT', 'cmdDebugMarkerEndEXT',
-- 'cmdDebugMarkerInsertEXT', 'debugMarkerSetObjectNameEXT',
-- 'debugMarkerSetObjectTagEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_debug_marker Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_debug_marker  ( DebugMarkerMarkerInfoEXT
                                              , DebugMarkerObjectNameInfoEXT
                                              , DebugMarkerObjectTagInfoEXT
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DebugMarkerMarkerInfoEXT

instance ToCStruct DebugMarkerMarkerInfoEXT
instance Show DebugMarkerMarkerInfoEXT

instance FromCStruct DebugMarkerMarkerInfoEXT


data DebugMarkerObjectNameInfoEXT

instance ToCStruct DebugMarkerObjectNameInfoEXT
instance Show DebugMarkerObjectNameInfoEXT

instance FromCStruct DebugMarkerObjectNameInfoEXT


data DebugMarkerObjectTagInfoEXT

instance ToCStruct DebugMarkerObjectTagInfoEXT
instance Show DebugMarkerObjectTagInfoEXT

instance FromCStruct DebugMarkerObjectTagInfoEXT

