{-# language CPP #-}
-- | = Name
--
-- VK_EXT_debug_utils - instance extension
--
-- == VK_EXT_debug_utils
--
-- [__Name String__]
--     @VK_EXT_debug_utils@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     129
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Debugging tools>
--
-- [__Contact__]
--
--     -   Mark Young
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_debug_utils] @marky-lunarg%0A*Here describe the issue or question you have about the VK_EXT_debug_utils extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-04-03
--
-- [__Revision__]
--     2
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Dependencies__]
--
--     -   This extension is written against version 1.0 of the Vulkan API.
--
--     -   Requires 'Vulkan.Core10.Enums.ObjectType.ObjectType'
--
-- [__Contributors__]
--
--     -   Mark Young, LunarG
--
--     -   Baldur Karlsson
--
--     -   Ian Elliott, Google
--
--     -   Courtney Goeltzenleuchter, Google
--
--     -   Karl Schultz, LunarG
--
--     -   Mark Lobodzinski, LunarG
--
--     -   Mike Schuchardt, LunarG
--
--     -   Jaakko Konttinen, AMD
--
--     -   Dan Ginsburg, Valve Software
--
--     -   Rolando Olivares, Epic Games
--
--     -   Dan Baker, Oxide Games
--
--     -   Kyle Spagnoli, NVIDIA
--
--     -   Jon Ashburn, LunarG
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- Due to the nature of the Vulkan interface, there is very little error
-- information available to the developer and application. By using the
-- @VK_EXT_debug_utils@ extension, developers /can/ obtain more
-- information. When combined with validation layers, even more detailed
-- feedback on the application’s use of Vulkan will be provided.
--
-- This extension provides the following capabilities:
--
-- -   The ability to create a debug messenger which will pass along debug
--     messages to an application supplied callback.
--
-- -   The ability to identify specific Vulkan objects using a name or tag
--     to improve tracking.
--
-- -   The ability to identify specific sections within a
--     'Vulkan.Core10.Handles.Queue' or
--     'Vulkan.Core10.Handles.CommandBuffer' using labels to aid
--     organization and offline analysis in external tools.
--
-- The main difference between this extension and @VK_EXT_debug_report@ and
-- @VK_EXT_debug_marker@ is that those extensions use
-- 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT' to
-- identify objects. This extension uses the core
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType' in place of
-- 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT'. The
-- primary reason for this move is that no future object type handle
-- enumeration values will be added to
-- 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT' since
-- the creation of 'Vulkan.Core10.Enums.ObjectType.ObjectType'.
--
-- In addition, this extension combines the functionality of both
-- @VK_EXT_debug_report@ and @VK_EXT_debug_marker@ by allowing object name
-- and debug markers (now called labels) to be returned to the
-- application’s callback function. This should assist in clarifying the
-- details of a debug message including: what objects are involved and
-- potentially which location within a 'Vulkan.Core10.Handles.Queue' or
-- 'Vulkan.Core10.Handles.CommandBuffer' the message occurred.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT'
--
-- == New Commands
--
-- -   'cmdBeginDebugUtilsLabelEXT'
--
-- -   'cmdEndDebugUtilsLabelEXT'
--
-- -   'cmdInsertDebugUtilsLabelEXT'
--
-- -   'createDebugUtilsMessengerEXT'
--
-- -   'destroyDebugUtilsMessengerEXT'
--
-- -   'queueBeginDebugUtilsLabelEXT'
--
-- -   'queueEndDebugUtilsLabelEXT'
--
-- -   'queueInsertDebugUtilsLabelEXT'
--
-- -   'setDebugUtilsObjectNameEXT'
--
-- -   'setDebugUtilsObjectTagEXT'
--
-- -   'submitDebugUtilsMessageEXT'
--
-- == New Structures
--
-- -   'DebugUtilsLabelEXT'
--
-- -   'DebugUtilsMessengerCallbackDataEXT'
--
-- -   'DebugUtilsObjectTagInfoEXT'
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo':
--
--     -   'DebugUtilsMessengerCreateInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo':
--
--     -   'DebugUtilsObjectNameInfoEXT'
--
-- == New Function Pointers
--
-- -   'PFN_vkDebugUtilsMessengerCallbackEXT'
--
-- == New Enums
--
-- -   'DebugUtilsMessageSeverityFlagBitsEXT'
--
-- -   'DebugUtilsMessageTypeFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'DebugUtilsMessageSeverityFlagsEXT'
--
-- -   'DebugUtilsMessageTypeFlagsEXT'
--
-- -   'DebugUtilsMessengerCallbackDataFlagsEXT'
--
-- -   'DebugUtilsMessengerCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEBUG_UTILS_EXTENSION_NAME'
--
-- -   'EXT_DEBUG_UTILS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT'
--
-- == Examples
--
-- __Example 1__
--
-- @VK_EXT_debug_utils@ allows an application to register multiple
-- callbacks with any Vulkan component wishing to report debug information.
-- Some callbacks may log the information to a file, others may cause a
-- debug break point or other application defined behavior. An application
-- /can/ register callbacks even when no validation layers are enabled, but
-- they will only be called for loader and, if implemented, driver events.
--
-- To capture events that occur while creating or destroying an instance an
-- application /can/ link a 'DebugUtilsMessengerCreateInfoEXT' structure to
-- the @pNext@ element of the
-- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo' structure given
-- to 'Vulkan.Core10.DeviceInitialization.createInstance'.
--
-- Example uses: Create three callback objects. One will log errors and
-- warnings to the debug console using Windows @OutputDebugString@. The
-- second will cause the debugger to break at that callback when an error
-- happens and the third will log warnings to stdout.
--
-- >     extern VkInstance instance;
-- >     VkResult res;
-- >     VkDebugUtilsMessengerEXT cb1, cb2, cb3;
-- >
-- >     // Must call extension functions through a function pointer:
-- >     PFN_vkCreateDebugUtilsMessengerEXT pfnCreateDebugUtilsMessengerEXT = (PFN_vkCreateDebugUtilsMessengerEXT)vkGetInstanceProcAddr(instance, "vkCreateDebugUtilsMessengerEXT");
-- >     PFN_vkDestroyDebugUtilsMessengerEXT pfnDestroyDebugUtilsMessengerEXT = (PFN_vkDestroyDebugUtilsMessengerEXT)vkGetInstanceProcAddr(instance, "vkDestroyDebugUtilsMessengerEXT");
-- >
-- >     VkDebugUtilsMessengerCreateInfoEXT callback1 = {
-- >         .sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = 0,
-- >         .messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT |
-- >                            VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,
-- >         .messageType= VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |
-- >                       VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
-- >         .pfnUserCallback = myOutputDebugString,
-- >         .pUserData = NULL
-- >     };
-- >     res = pfnCreateDebugUtilsMessengerEXT(instance, &callback1, NULL, &cb1);
-- >     if (res != VK_SUCCESS) {
-- >        // Do error handling for VK_ERROR_OUT_OF_MEMORY
-- >     }
-- >
-- >     callback1.messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
-- >     callback1.pfnUserCallback = myDebugBreak;
-- >     callback1.pUserData = NULL;
-- >     res = pfnCreateDebugUtilsMessengerEXT(instance, &callback1, NULL, &cb2);
-- >     if (res != VK_SUCCESS) {
-- >        // Do error handling for VK_ERROR_OUT_OF_MEMORY
-- >     }
-- >
-- >     VkDebugUtilsMessengerCreateInfoEXT callback3 = {
-- >         .sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = 0,
-- >         .messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,
-- >         .messageType = VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |
-- >                        VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
-- >         .pfnUserCallback = mystdOutLogger,
-- >         .pUserData = NULL
-- >     };
-- >     res = pfnCreateDebugUtilsMessengerEXT(instance, &callback3, NULL, &cb3);
-- >     if (res != VK_SUCCESS) {
-- >        // Do error handling for VK_ERROR_OUT_OF_MEMORY
-- >     }
-- >
-- >     ...
-- >
-- >     // Remove callbacks when cleaning up
-- >     pfnDestroyDebugUtilsMessengerEXT(instance, cb1, NULL);
-- >     pfnDestroyDebugUtilsMessengerEXT(instance, cb2, NULL);
-- >     pfnDestroyDebugUtilsMessengerEXT(instance, cb3, NULL);
--
-- __Example 2__
--
-- Associate a name with an image, for easier debugging in external tools
-- or with validation layers that can print a friendly name when referring
-- to objects in error messages.
--
-- >     extern VkInstance instance;
-- >     extern VkDevice device;
-- >     extern VkImage image;
-- >
-- >     // Must call extension functions through a function pointer:
-- >     PFN_vkSetDebugUtilsObjectNameEXT pfnSetDebugUtilsObjectNameEXT = (PFN_vkSetDebugUtilsObjectNameEXT)vkGetInstanceProcAddr(instance, "vkSetDebugUtilsObjectNameEXT");
-- >
-- >     // Set a name on the image
-- >     const VkDebugUtilsObjectNameInfoEXT imageNameInfo =
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT,
-- >         .pNext = NULL,
-- >         .objectType = VK_OBJECT_TYPE_IMAGE,
-- >         .objectHandle = (uint64_t)image,
-- >         .pObjectName = "Brick Diffuse Texture",
-- >     };
-- >
-- >     pfnSetDebugUtilsObjectNameEXT(device, &imageNameInfo);
-- >
-- >     // A subsequent error might print:
-- >     //   Image 'Brick Diffuse Texture' (0xc0dec0dedeadbeef) is used in a
-- >     //   command buffer with no memory bound to it.
--
-- __Example 3__
--
-- Annotating regions of a workload with naming information so that offline
-- analysis tools can display a more usable visualization of the commands
-- submitted.
--
-- >     extern VkInstance instance;
-- >     extern VkCommandBuffer commandBuffer;
-- >
-- >     // Must call extension functions through a function pointer:
-- >     PFN_vkQueueBeginDebugUtilsLabelEXT pfnQueueBeginDebugUtilsLabelEXT = (PFN_vkQueueBeginDebugUtilsLabelEXT)vkGetInstanceProcAddr(instance, "vkQueueBeginDebugUtilsLabelEXT");
-- >     PFN_vkQueueEndDebugUtilsLabelEXT pfnQueueEndDebugUtilsLabelEXT = (PFN_vkQueueEndDebugUtilsLabelEXT)vkGetInstanceProcAddr(instance, "vkQueueEndDebugUtilsLabelEXT");
-- >     PFN_vkCmdBeginDebugUtilsLabelEXT pfnCmdBeginDebugUtilsLabelEXT = (PFN_vkCmdBeginDebugUtilsLabelEXT)vkGetInstanceProcAddr(instance, "vkCmdBeginDebugUtilsLabelEXT");
-- >     PFN_vkCmdEndDebugUtilsLabelEXT pfnCmdEndDebugUtilsLabelEXT = (PFN_vkCmdEndDebugUtilsLabelEXT)vkGetInstanceProcAddr(instance, "vkCmdEndDebugUtilsLabelEXT");
-- >     PFN_vkCmdInsertDebugUtilsLabelEXT pfnCmdInsertDebugUtilsLabelEXT = (PFN_vkCmdInsertDebugUtilsLabelEXT)vkGetInstanceProcAddr(instance, "vkCmdInsertDebugUtilsLabelEXT");
-- >
-- >     // Describe the area being rendered
-- >     const VkDebugUtilsLabelEXT houseLabel =
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT,
-- >         .pNext = NULL,
-- >         .pLabelName = "Brick House",
-- >         .color = { 1.0f, 0.0f, 0.0f, 1.0f },
-- >     };
-- >
-- >     // Start an annotated group of calls under the 'Brick House' name
-- >     pfnCmdBeginDebugUtilsLabelEXT(commandBuffer, &houseLabel);
-- >     {
-- >         // A mutable structure for each part being rendered
-- >         VkDebugUtilsLabelEXT housePartLabel =
-- >         {
-- >             .sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT,
-- >             .pNext = NULL,
-- >             .pLabelName = NULL,
-- >             .color = { 0.0f, 0.0f, 0.0f, 0.0f },
-- >         };
-- >
-- >         // Set the name and insert the marker
-- >         housePartLabel.pLabelName = "Walls";
-- >         pfnCmdInsertDebugUtilsLabelEXT(commandBuffer, &housePartLabel);
-- >
-- >         // Insert the drawcall for the walls
-- >         vkCmdDrawIndexed(commandBuffer, 1000, 1, 0, 0, 0);
-- >
-- >         // Insert a recursive region for two sets of windows
-- >         housePartLabel.pLabelName = "Windows";
-- >         pfnCmdBeginDebugUtilsLabelEXT(commandBuffer, &housePartLabel);
-- >         {
-- >             vkCmdDrawIndexed(commandBuffer, 75, 6, 1000, 0, 0);
-- >             vkCmdDrawIndexed(commandBuffer, 100, 2, 1450, 0, 0);
-- >         }
-- >         pfnCmdEndDebugUtilsLabelEXT(commandBuffer);
-- >
-- >         housePartLabel.pLabelName = "Front Door";
-- >         pfnCmdInsertDebugUtilsLabelEXT(commandBuffer, &housePartLabel);
-- >
-- >         vkCmdDrawIndexed(commandBuffer, 350, 1, 1650, 0, 0);
-- >
-- >         housePartLabel.pLabelName = "Roof";
-- >         pfnCmdInsertDebugUtilsLabelEXT(commandBuffer, &housePartLabel);
-- >
-- >         vkCmdDrawIndexed(commandBuffer, 500, 1, 2000, 0, 0);
-- >     }
-- >     // End the house annotation started above
-- >     pfnCmdEndDebugUtilsLabelEXT(commandBuffer);
-- >
-- >     // Do other work
-- >
-- >     vkEndCommandBuffer(commandBuffer);
-- >
-- >     // Describe the queue being used
-- >     const VkDebugUtilsLabelEXT queueLabel =
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT,
-- >         .pNext = NULL,
-- >         .pLabelName = "Main Render Work",
-- >         .color = { 0.0f, 1.0f, 0.0f, 1.0f },
-- >     };
-- >
-- >     // Identify the queue label region
-- >     pfnQueueBeginDebugUtilsLabelEXT(queue, &queueLabel);
-- >
-- >     // Submit the work for the main render thread
-- >     const VkCommandBuffer cmd_bufs[] = {commandBuffer};
-- >     VkSubmitInfo submit_info =
-- >     {
-- >         .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
-- >         .pNext = NULL,
-- >         .waitSemaphoreCount = 0,
-- >         .pWaitSemaphores = NULL,
-- >         .pWaitDstStageMask = NULL,
-- >         .commandBufferCount = 1,
-- >         .pCommandBuffers = cmd_bufs,
-- >         .signalSemaphoreCount = 0,
-- >         .pSignalSemaphores = NULL
-- >     };
-- >     vkQueueSubmit(queue, 1, &submit_info, fence);
-- >
-- >     // End the queue label region
-- >     pfnQueueEndDebugUtilsLabelEXT(queue);
--
-- == Issues
--
-- 1) Should we just name this extension @VK_EXT_debug_report2@
--
-- __RESOLVED__: No. There is enough additional changes to the structures
-- to break backwards compatibility. So, a new name was decided that would
-- not indicate any interaction with the previous extension.
--
-- 2) Will validation layers immediately support all the new features.
--
-- __RESOLVED__: Not immediately. As one can imagine, there is a lot of
-- work involved with converting the validation layer logging over to the
-- new functionality. Basic logging, as seen in the origin
-- @VK_EXT_debug_report@ extension will be made available immediately.
-- However, adding the labels and object names will take time. Since the
-- priority for Khronos at this time is to continue focusing on Valid Usage
-- statements, it may take a while before the new functionality is fully
-- exposed.
--
-- 3) If the validation layers will not expose the new functionality
-- immediately, then what is the point of this extension?
--
-- __RESOLVED__: We needed a replacement for @VK_EXT_debug_report@ because
-- the 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT'
-- enumeration will no longer be updated and any new objects will need to
-- be debugged using the new functionality provided by this extension.
--
-- 4) Should this extension be split into two separate parts (1 extension
-- that is an instance extension providing the callback functionality, and
-- another device extension providing the general debug marker and
-- annotation functionality)?
--
-- __RESOLVED__: No, the functionality for this extension is too closely
-- related. If we did split up the extension, where would the structures
-- and enums live, and how would you define that the device behavior in the
-- instance extension is really only valid if the device extension is
-- enabled, and the functionality is passed in. It is cleaner to just
-- define this all as an instance extension, plus it allows the application
-- to enable all debug functionality provided with one enable string during
-- 'Vulkan.Core10.DeviceInitialization.createInstance'.
--
-- == Version History
--
-- -   Revision 1, 2017-09-14 (Mark Young and all listed Contributors)
--
--     -   Initial draft, based on @VK_EXT_debug_report@ and
--         @VK_EXT_debug_marker@ in addition to previous feedback supplied
--         from various companies including Valve, Epic, and Oxide games.
--
-- -   Revision 2, 2020-04-03 (Mark Young and Piers Daniell)
--
--     -   Updated to allow either @NULL@ or an empty string to be passed
--         in for @pObjectName@ in 'DebugUtilsObjectNameInfoEXT', because
--         the loader and various drivers support @NULL@ already.
--
-- == See Also
--
-- 'PFN_vkDebugUtilsMessengerCallbackEXT', 'DebugUtilsLabelEXT',
-- 'DebugUtilsMessageSeverityFlagBitsEXT',
-- 'DebugUtilsMessageSeverityFlagsEXT', 'DebugUtilsMessageTypeFlagBitsEXT',
-- 'DebugUtilsMessageTypeFlagsEXT', 'DebugUtilsMessengerCallbackDataEXT',
-- 'DebugUtilsMessengerCallbackDataFlagsEXT',
-- 'DebugUtilsMessengerCreateFlagsEXT', 'DebugUtilsMessengerCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT',
-- 'DebugUtilsObjectNameInfoEXT', 'DebugUtilsObjectTagInfoEXT',
-- 'cmdBeginDebugUtilsLabelEXT', 'cmdEndDebugUtilsLabelEXT',
-- 'cmdInsertDebugUtilsLabelEXT', 'createDebugUtilsMessengerEXT',
-- 'destroyDebugUtilsMessengerEXT', 'queueBeginDebugUtilsLabelEXT',
-- 'queueEndDebugUtilsLabelEXT', 'queueInsertDebugUtilsLabelEXT',
-- 'setDebugUtilsObjectNameEXT', 'setDebugUtilsObjectTagEXT',
-- 'submitDebugUtilsMessageEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_debug_utils Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_debug_utils  ( DebugUtilsLabelEXT
                                             , DebugUtilsMessengerCallbackDataEXT
                                             , DebugUtilsMessengerCreateInfoEXT
                                             , DebugUtilsObjectNameInfoEXT
                                             , DebugUtilsObjectTagInfoEXT
                                             , DebugUtilsMessageSeverityFlagsEXT
                                             , DebugUtilsMessageSeverityFlagBitsEXT
                                             , DebugUtilsMessageTypeFlagsEXT
                                             , DebugUtilsMessageTypeFlagBitsEXT
                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data DebugUtilsLabelEXT

instance ToCStruct DebugUtilsLabelEXT
instance Show DebugUtilsLabelEXT

instance FromCStruct DebugUtilsLabelEXT


type role DebugUtilsMessengerCallbackDataEXT nominal
data DebugUtilsMessengerCallbackDataEXT (es :: [Type])

instance ( Extendss DebugUtilsMessengerCallbackDataEXT es
         , PokeChain es ) => ToCStruct (DebugUtilsMessengerCallbackDataEXT es)
instance Show (Chain es) => Show (DebugUtilsMessengerCallbackDataEXT es)

instance ( Extendss DebugUtilsMessengerCallbackDataEXT es
         , PeekChain es ) => FromCStruct (DebugUtilsMessengerCallbackDataEXT es)


data DebugUtilsMessengerCreateInfoEXT

instance ToCStruct DebugUtilsMessengerCreateInfoEXT
instance Show DebugUtilsMessengerCreateInfoEXT

instance FromCStruct DebugUtilsMessengerCreateInfoEXT


data DebugUtilsObjectNameInfoEXT

instance ToCStruct DebugUtilsObjectNameInfoEXT
instance Show DebugUtilsObjectNameInfoEXT

instance FromCStruct DebugUtilsObjectNameInfoEXT


data DebugUtilsObjectTagInfoEXT

instance ToCStruct DebugUtilsObjectTagInfoEXT
instance Show DebugUtilsObjectTagInfoEXT

instance FromCStruct DebugUtilsObjectTagInfoEXT


type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT

data DebugUtilsMessageSeverityFlagBitsEXT


type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT

data DebugUtilsMessageTypeFlagBitsEXT

