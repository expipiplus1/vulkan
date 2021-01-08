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
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Debugging tools>
--
-- [__Contact__]
--
--     -   Mark Young
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_debug_utils:%20&body=@marky-lunarg%20 >
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
-- -   'DebugUtilsObjectNameInfoEXT'
--
-- -   'DebugUtilsObjectTagInfoEXT'
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo':
--
--     -   'DebugUtilsMessengerCreateInfoEXT'
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
-- to 'Vulkan.Core10.DeviceInitialization.createInstance'. This callback is
-- only valid for the duration of the
-- 'Vulkan.Core10.DeviceInitialization.createInstance' and the
-- 'Vulkan.Core10.DeviceInitialization.destroyInstance' call. Use
-- 'createDebugUtilsMessengerEXT' to create persistent callback objects.
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
-- >     PFN_vkCreateDebugUtilsMessengerEXT pfnCreateDebugUtilsMessengerEXT = (PFN_vkCreateDebugUtilsMessengerEXT)vkGetDeviceProcAddr(device, "vkCreateDebugUtilsMessengerEXT");
-- >     PFN_vkDestroyDebugUtilsMessengerEXT pfnDestroyDebugUtilsMessengerEXT = (PFN_vkDestroyDebugUtilsMessengerEXT)vkGetDeviceProcAddr(device, "vkDestroyDebugUtilsMessengerEXT");
-- >
-- >     VkDebugUtilsMessengeCreateInfoEXT callback1 = {
-- >             VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,  // sType
-- >             NULL,                                                     // pNext
-- >             0,                                                        // flags
-- >             VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT |           // messageSeverity
-- >             VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,
-- >             VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |             // messageType
-- >             VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
-- >             myOutputDebugString,                                      // pfnUserCallback
-- >             NULL                                                      // pUserData
-- >     };
-- >     res = pfnCreateDebugUtilsMessengerEXT(instance, &callback1, NULL, &cb1);
-- >     if (res != VK_SUCCESS) {
-- >        // Do error handling for VK_ERROR_OUT_OF_MEMORY
-- >     }
-- >
-- >     callback1.messageSeverity = VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT;
-- >     callback1.pfnCallback = myDebugBreak;
-- >     callback1.pUserData = NULL;
-- >     res = pfnCreateDebugUtilsMessengerEXT(instance, &callback1, NULL, &cb2);
-- >     if (res != VK_SUCCESS) {
-- >        // Do error handling for VK_ERROR_OUT_OF_MEMORY
-- >     }
-- >
-- >     VkDebugUtilsMessengerCreateInfoEXT callback3 = {
-- >             VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,  // sType
-- >             NULL,                                                     // pNext
-- >             0,                                                        // flags
-- >             VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,          // messageSeverity
-- >             VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT |             // messageType
-- >             VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT,
-- >             mystdOutLogger,                                           // pfnUserCallback
-- >             NULL                                                      // pUserData
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
-- >     extern VkDevice device;
-- >     extern VkImage image;
-- >
-- >     // Must call extension functions through a function pointer:
-- >     PFN_vkSetDebugUtilsObjectNameEXT pfnSetDebugUtilsObjectNameEXT = (PFN_vkSetDebugUtilsObjectNameEXT)vkGetDeviceProcAddr(device, "vkSetDebugUtilsObjectNameEXT");
-- >
-- >     // Set a name on the image
-- >     const VkDebugUtilsObjectNameInfoEXT imageNameInfo =
-- >     {
-- >         VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT, // sType
-- >         NULL,                                               // pNext
-- >         VK_OBJECT_TYPE_IMAGE,                               // objectType
-- >         (uint64_t)image,                                    // object
-- >         "Brick Diffuse Texture",                            // pObjectName
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
-- >     extern VkDevice device;
-- >     extern VkCommandBuffer commandBuffer;
-- >
-- >     // Must call extension functions through a function pointer:
-- >     PFN_vkQueueBeginDebugUtilsLabelEXT pfnQueueBeginDebugUtilsLabelEXT = (PFN_vkQueueBeginDebugUtilsLabelEXT)vkGetDeviceProcAddr(device, "vkQueueBeginDebugUtilsLabelEXT");
-- >     PFN_vkQueueEndDebugUtilsLabelEXT pfnQueueEndDebugUtilsLabelEXT = (PFN_vkQueueEndDebugUtilsLabelEXT)vkGetDeviceProcAddr(device, "vkQueueEndDebugUtilsLabelEXT");
-- >     PFN_vkCmdBeginDebugUtilsLabelEXT pfnCmdBeginDebugUtilsLabelEXT = (PFN_vkCmdBeginDebugUtilsLabelEXT)vkGetDeviceProcAddr(device, "vkCmdBeginDebugUtilsLabelEXT");
-- >     PFN_vkCmdEndDebugUtilsLabelEXT pfnCmdEndDebugUtilsLabelEXT = (PFN_vkCmdEndDebugUtilsLabelEXT)vkGetDeviceProcAddr(device, "vkCmdEndDebugUtilsLabelEXT");
-- >     PFN_vkCmdInsertDebugUtilsLabelEXT pfnCmdInsertDebugUtilsLabelEXT = (PFN_vkCmdInsertDebugUtilsLabelEXT)vkGetDeviceProcAddr(device, "vkCmdInsertDebugUtilsLabelEXT");
-- >
-- >     // Describe the area being rendered
-- >     const VkDebugUtilsLabelEXT houseLabel =
-- >     {
-- >         VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT, // sType
-- >         NULL,                                    // pNext
-- >         "Brick House",                           // pLabelName
-- >         { 1.0f, 0.0f, 0.0f, 1.0f },              // color
-- >     };
-- >
-- >     // Start an annotated group of calls under the 'Brick House' name
-- >     pfnCmdBeginDebugUtilsLabelEXT(commandBuffer, &houseLabel);
-- >     {
-- >         // A mutable structure for each part being rendered
-- >         VkDebugUtilsLabelEXT housePartLabel =
-- >         {
-- >             VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT, // sType
-- >             NULL,                                    // pNext
-- >             NULL,                                    // pLabelName
-- >             { 0.0f, 0.0f, 0.0f, 0.0f },              // color
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
-- >         VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT, // sType
-- >         NULL,                                    // pNext
-- >         "Main Render Work",                      // pLabelName
-- >         { 0.0f, 1.0f, 0.0f, 1.0f },              // color
-- >     };
-- >
-- >     // Identify the queue label region
-- >     pfnQueueBeginDebugUtilsLabelEXT(queue, &queueLabel);
-- >
-- >     // Submit the work for the main render thread
-- >     const VkCommandBuffer cmd_bufs[] = {commandBuffer};
-- >     VkSubmitInfo submit_info = {.sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
-- >                                 .pNext = NULL,
-- >                                 .waitSemaphoreCount = 0,
-- >                                 .pWaitSemaphores = NULL,
-- >                                 .pWaitDstStageMask = NULL,
-- >                                 .commandBufferCount = 1,
-- >                                 .pCommandBuffers = cmd_bufs,
-- >                                 .signalSemaphoreCount = 0,
-- >                                 .pSignalSemaphores = NULL};
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
-- 3) If the validation layers won’t expose the new functionality
-- immediately, then what’s the point of this extension?
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
-- enabled, and the functionality is passed in. It’s cleaner to just define
-- this all as an instance extension, plus it allows the application to
-- enable all debug functionality provided with one enable string during
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
-- = See Also
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
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_utils Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_debug_utils  ( setDebugUtilsObjectNameEXT
                                             , setDebugUtilsObjectTagEXT
                                             , queueBeginDebugUtilsLabelEXT
                                             , queueEndDebugUtilsLabelEXT
                                             , queueInsertDebugUtilsLabelEXT
                                             , cmdBeginDebugUtilsLabelEXT
                                             , cmdUseDebugUtilsLabelEXT
                                             , cmdEndDebugUtilsLabelEXT
                                             , cmdInsertDebugUtilsLabelEXT
                                             , createDebugUtilsMessengerEXT
                                             , withDebugUtilsMessengerEXT
                                             , destroyDebugUtilsMessengerEXT
                                             , submitDebugUtilsMessageEXT
                                             , DebugUtilsObjectNameInfoEXT(..)
                                             , DebugUtilsObjectTagInfoEXT(..)
                                             , DebugUtilsLabelEXT(..)
                                             , DebugUtilsMessengerCreateInfoEXT(..)
                                             , DebugUtilsMessengerCallbackDataEXT(..)
                                             , DebugUtilsMessengerCreateFlagsEXT(..)
                                             , DebugUtilsMessengerCallbackDataFlagsEXT(..)
                                             , DebugUtilsMessageSeverityFlagsEXT
                                             , DebugUtilsMessageSeverityFlagBitsEXT( DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
                                                                                   , ..
                                                                                   )
                                             , DebugUtilsMessageTypeFlagsEXT
                                             , DebugUtilsMessageTypeFlagBitsEXT( DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                                                                               , DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                                                                               , DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
                                                                               , ..
                                                                               )
                                             , PFN_vkDebugUtilsMessengerCallbackEXT
                                             , FN_vkDebugUtilsMessengerCallbackEXT
                                             , EXT_DEBUG_UTILS_SPEC_VERSION
                                             , pattern EXT_DEBUG_UTILS_SPEC_VERSION
                                             , EXT_DEBUG_UTILS_EXTENSION_NAME
                                             , pattern EXT_DEBUG_UTILS_EXTENSION_NAME
                                             , DebugUtilsMessengerEXT(..)
                                             ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.Handles (DebugUtilsMessengerEXT)
import Vulkan.Extensions.Handles (DebugUtilsMessengerEXT(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdInsertDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkQueueBeginDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkQueueEndDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkQueueInsertDebugUtilsLabelEXT))
import Vulkan.Dynamic (DeviceCmds(pVkSetDebugUtilsObjectNameEXT))
import Vulkan.Dynamic (DeviceCmds(pVkSetDebugUtilsObjectTagEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDebugUtilsMessengerEXT))
import Vulkan.Dynamic (InstanceCmds(pVkDestroyDebugUtilsMessengerEXT))
import Vulkan.Dynamic (InstanceCmds(pVkSubmitDebugUtilsMessageEXT))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Enums.ObjectType (ObjectType)
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DebugUtilsMessengerEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetDebugUtilsObjectNameEXT
  :: FunPtr (Ptr Device_T -> Ptr DebugUtilsObjectNameInfoEXT -> IO Result) -> Ptr Device_T -> Ptr DebugUtilsObjectNameInfoEXT -> IO Result

-- | vkSetDebugUtilsObjectNameEXT - Give a user-friendly name to an object
--
-- == Valid Usage
--
-- -   #VUID-vkSetDebugUtilsObjectNameEXT-pNameInfo-02587#
--     @pNameInfo->objectType@ /must/ not be
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- -   #VUID-vkSetDebugUtilsObjectNameEXT-pNameInfo-02588#
--     @pNameInfo->objectHandle@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkSetDebugUtilsObjectNameEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkSetDebugUtilsObjectNameEXT-pNameInfo-parameter# @pNameInfo@
--     /must/ be a valid pointer to a valid 'DebugUtilsObjectNameInfoEXT'
--     structure
--
-- == Host Synchronization
--
-- -   Host access to @pNameInfo->objectHandle@ /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'DebugUtilsObjectNameInfoEXT', 'Vulkan.Core10.Handles.Device'
setDebugUtilsObjectNameEXT :: forall io
                            . (MonadIO io)
                           => -- | @device@ is the device that created the object.
                              Device
                           -> -- | @pNameInfo@ is a pointer to a 'DebugUtilsObjectNameInfoEXT' structure
                              -- specifying parameters of the name to set on the object.
                              DebugUtilsObjectNameInfoEXT
                           -> io ()
setDebugUtilsObjectNameEXT device nameInfo = liftIO . evalContT $ do
  let vkSetDebugUtilsObjectNameEXTPtr = pVkSetDebugUtilsObjectNameEXT (deviceCmds (device :: Device))
  lift $ unless (vkSetDebugUtilsObjectNameEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetDebugUtilsObjectNameEXT is null" Nothing Nothing
  let vkSetDebugUtilsObjectNameEXT' = mkVkSetDebugUtilsObjectNameEXT vkSetDebugUtilsObjectNameEXTPtr
  pNameInfo <- ContT $ withCStruct (nameInfo)
  r <- lift $ traceAroundEvent "vkSetDebugUtilsObjectNameEXT" (vkSetDebugUtilsObjectNameEXT' (deviceHandle (device)) pNameInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetDebugUtilsObjectTagEXT
  :: FunPtr (Ptr Device_T -> Ptr DebugUtilsObjectTagInfoEXT -> IO Result) -> Ptr Device_T -> Ptr DebugUtilsObjectTagInfoEXT -> IO Result

-- | vkSetDebugUtilsObjectTagEXT - Attach arbitrary data to an object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkSetDebugUtilsObjectTagEXT-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkSetDebugUtilsObjectTagEXT-pTagInfo-parameter# @pTagInfo@
--     /must/ be a valid pointer to a valid 'DebugUtilsObjectTagInfoEXT'
--     structure
--
-- == Host Synchronization
--
-- -   Host access to @pTagInfo->objectHandle@ /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'DebugUtilsObjectTagInfoEXT', 'Vulkan.Core10.Handles.Device'
setDebugUtilsObjectTagEXT :: forall io
                           . (MonadIO io)
                          => -- | @device@ is the device that created the object.
                             Device
                          -> -- | @pTagInfo@ is a pointer to a 'DebugUtilsObjectTagInfoEXT' structure
                             -- specifying parameters of the tag to attach to the object.
                             DebugUtilsObjectTagInfoEXT
                          -> io ()
setDebugUtilsObjectTagEXT device tagInfo = liftIO . evalContT $ do
  let vkSetDebugUtilsObjectTagEXTPtr = pVkSetDebugUtilsObjectTagEXT (deviceCmds (device :: Device))
  lift $ unless (vkSetDebugUtilsObjectTagEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetDebugUtilsObjectTagEXT is null" Nothing Nothing
  let vkSetDebugUtilsObjectTagEXT' = mkVkSetDebugUtilsObjectTagEXT vkSetDebugUtilsObjectTagEXTPtr
  pTagInfo <- ContT $ withCStruct (tagInfo)
  r <- lift $ traceAroundEvent "vkSetDebugUtilsObjectTagEXT" (vkSetDebugUtilsObjectTagEXT' (deviceHandle (device)) pTagInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueBeginDebugUtilsLabelEXT
  :: FunPtr (Ptr Queue_T -> Ptr DebugUtilsLabelEXT -> IO ()) -> Ptr Queue_T -> Ptr DebugUtilsLabelEXT -> IO ()

-- | vkQueueBeginDebugUtilsLabelEXT - Open a queue debug label region
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | Any                                                                                                                   | -                                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'DebugUtilsLabelEXT', 'Vulkan.Core10.Handles.Queue'
queueBeginDebugUtilsLabelEXT :: forall io
                              . (MonadIO io)
                             => -- | @queue@ is the queue in which to start a debug label region.
                                --
                                -- #VUID-vkQueueBeginDebugUtilsLabelEXT-queue-parameter# @queue@ /must/ be
                                -- a valid 'Vulkan.Core10.Handles.Queue' handle
                                Queue
                             -> -- | @pLabelInfo@ is a pointer to a 'DebugUtilsLabelEXT' structure specifying
                                -- parameters of the label region to open.
                                --
                                -- #VUID-vkQueueBeginDebugUtilsLabelEXT-pLabelInfo-parameter# @pLabelInfo@
                                -- /must/ be a valid pointer to a valid 'DebugUtilsLabelEXT' structure
                                ("labelInfo" ::: DebugUtilsLabelEXT)
                             -> io ()
queueBeginDebugUtilsLabelEXT queue labelInfo = liftIO . evalContT $ do
  let vkQueueBeginDebugUtilsLabelEXTPtr = pVkQueueBeginDebugUtilsLabelEXT (deviceCmds (queue :: Queue))
  lift $ unless (vkQueueBeginDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueBeginDebugUtilsLabelEXT is null" Nothing Nothing
  let vkQueueBeginDebugUtilsLabelEXT' = mkVkQueueBeginDebugUtilsLabelEXT vkQueueBeginDebugUtilsLabelEXTPtr
  pLabelInfo <- ContT $ withCStruct (labelInfo)
  lift $ traceAroundEvent "vkQueueBeginDebugUtilsLabelEXT" (vkQueueBeginDebugUtilsLabelEXT' (queueHandle (queue)) pLabelInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueEndDebugUtilsLabelEXT
  :: FunPtr (Ptr Queue_T -> IO ()) -> Ptr Queue_T -> IO ()

-- | vkQueueEndDebugUtilsLabelEXT - Close a queue debug label region
--
-- = Description
--
-- The calls to 'queueBeginDebugUtilsLabelEXT' and
-- 'queueEndDebugUtilsLabelEXT' /must/ be matched and balanced.
--
-- == Valid Usage
--
-- -   #VUID-vkQueueEndDebugUtilsLabelEXT-None-01911# There /must/ be an
--     outstanding 'queueBeginDebugUtilsLabelEXT' command prior to the
--     'queueEndDebugUtilsLabelEXT' on the queue
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkQueueEndDebugUtilsLabelEXT-queue-parameter# @queue@ /must/
--     be a valid 'Vulkan.Core10.Handles.Queue' handle
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | Any                                                                                                                   | -                                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Queue'
queueEndDebugUtilsLabelEXT :: forall io
                            . (MonadIO io)
                           => -- | @queue@ is the queue in which a debug label region should be closed.
                              Queue
                           -> io ()
queueEndDebugUtilsLabelEXT queue = liftIO $ do
  let vkQueueEndDebugUtilsLabelEXTPtr = pVkQueueEndDebugUtilsLabelEXT (deviceCmds (queue :: Queue))
  unless (vkQueueEndDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueEndDebugUtilsLabelEXT is null" Nothing Nothing
  let vkQueueEndDebugUtilsLabelEXT' = mkVkQueueEndDebugUtilsLabelEXT vkQueueEndDebugUtilsLabelEXTPtr
  traceAroundEvent "vkQueueEndDebugUtilsLabelEXT" (vkQueueEndDebugUtilsLabelEXT' (queueHandle (queue)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueInsertDebugUtilsLabelEXT
  :: FunPtr (Ptr Queue_T -> Ptr DebugUtilsLabelEXT -> IO ()) -> Ptr Queue_T -> Ptr DebugUtilsLabelEXT -> IO ()

-- | vkQueueInsertDebugUtilsLabelEXT - Insert a label into a queue
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | Any                                                                                                                   | -                                                                                                                                   |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'DebugUtilsLabelEXT', 'Vulkan.Core10.Handles.Queue'
queueInsertDebugUtilsLabelEXT :: forall io
                               . (MonadIO io)
                              => -- | @queue@ is the queue into which a debug label will be inserted.
                                 --
                                 -- #VUID-vkQueueInsertDebugUtilsLabelEXT-queue-parameter# @queue@ /must/ be
                                 -- a valid 'Vulkan.Core10.Handles.Queue' handle
                                 Queue
                              -> -- | @pLabelInfo@ is a pointer to a 'DebugUtilsLabelEXT' structure specifying
                                 -- parameters of the label to insert.
                                 --
                                 -- #VUID-vkQueueInsertDebugUtilsLabelEXT-pLabelInfo-parameter# @pLabelInfo@
                                 -- /must/ be a valid pointer to a valid 'DebugUtilsLabelEXT' structure
                                 ("labelInfo" ::: DebugUtilsLabelEXT)
                              -> io ()
queueInsertDebugUtilsLabelEXT queue labelInfo = liftIO . evalContT $ do
  let vkQueueInsertDebugUtilsLabelEXTPtr = pVkQueueInsertDebugUtilsLabelEXT (deviceCmds (queue :: Queue))
  lift $ unless (vkQueueInsertDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueInsertDebugUtilsLabelEXT is null" Nothing Nothing
  let vkQueueInsertDebugUtilsLabelEXT' = mkVkQueueInsertDebugUtilsLabelEXT vkQueueInsertDebugUtilsLabelEXTPtr
  pLabelInfo <- ContT $ withCStruct (labelInfo)
  lift $ traceAroundEvent "vkQueueInsertDebugUtilsLabelEXT" (vkQueueInsertDebugUtilsLabelEXT' (queueHandle (queue)) pLabelInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginDebugUtilsLabelEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DebugUtilsLabelEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr DebugUtilsLabelEXT -> IO ()

-- | vkCmdBeginDebugUtilsLabelEXT - Open a command buffer debug label region
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginDebugUtilsLabelEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginDebugUtilsLabelEXT-pLabelInfo-parameter#
--     @pLabelInfo@ /must/ be a valid pointer to a valid
--     'DebugUtilsLabelEXT' structure
--
-- -   #VUID-vkCmdBeginDebugUtilsLabelEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginDebugUtilsLabelEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DebugUtilsLabelEXT'
cmdBeginDebugUtilsLabelEXT :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer into which the command is
                              -- recorded.
                              CommandBuffer
                           -> -- | @pLabelInfo@ is a pointer to a 'DebugUtilsLabelEXT' structure specifying
                              -- parameters of the label region to open.
                              ("labelInfo" ::: DebugUtilsLabelEXT)
                           -> io ()
cmdBeginDebugUtilsLabelEXT commandBuffer labelInfo = liftIO . evalContT $ do
  let vkCmdBeginDebugUtilsLabelEXTPtr = pVkCmdBeginDebugUtilsLabelEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBeginDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginDebugUtilsLabelEXT is null" Nothing Nothing
  let vkCmdBeginDebugUtilsLabelEXT' = mkVkCmdBeginDebugUtilsLabelEXT vkCmdBeginDebugUtilsLabelEXTPtr
  pLabelInfo <- ContT $ withCStruct (labelInfo)
  lift $ traceAroundEvent "vkCmdBeginDebugUtilsLabelEXT" (vkCmdBeginDebugUtilsLabelEXT' (commandBufferHandle (commandBuffer)) pLabelInfo)
  pure $ ()

-- | This function will call the supplied action between calls to
-- 'cmdBeginDebugUtilsLabelEXT' and 'cmdEndDebugUtilsLabelEXT'
--
-- Note that 'cmdEndDebugUtilsLabelEXT' is *not* called if an exception is
-- thrown by the inner action.
cmdUseDebugUtilsLabelEXT :: forall io r . MonadIO io => CommandBuffer -> DebugUtilsLabelEXT -> io r -> io r
cmdUseDebugUtilsLabelEXT commandBuffer pLabelInfo a =
  (cmdBeginDebugUtilsLabelEXT commandBuffer pLabelInfo) *> a <* (cmdEndDebugUtilsLabelEXT commandBuffer)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndDebugUtilsLabelEXT
  :: FunPtr (Ptr CommandBuffer_T -> IO ()) -> Ptr CommandBuffer_T -> IO ()

-- | vkCmdEndDebugUtilsLabelEXT - Close a command buffer label region
--
-- = Description
--
-- An application /may/ open a debug label region in one command buffer and
-- close it in another, or otherwise split debug label regions across
-- multiple command buffers or multiple queue submissions. When viewed from
-- the linear series of submissions to a single queue, the calls to
-- 'cmdBeginDebugUtilsLabelEXT' and 'cmdEndDebugUtilsLabelEXT' /must/ be
-- matched and balanced.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndDebugUtilsLabelEXT-commandBuffer-01912# There /must/
--     be an outstanding 'cmdBeginDebugUtilsLabelEXT' command prior to the
--     'cmdEndDebugUtilsLabelEXT' on the queue that @commandBuffer@ is
--     submitted to
--
-- -   #VUID-vkCmdEndDebugUtilsLabelEXT-commandBuffer-01913# If
--     @commandBuffer@ is a secondary command buffer, there /must/ be an
--     outstanding 'cmdBeginDebugUtilsLabelEXT' command recorded to
--     @commandBuffer@ that has not previously been ended by a call to
--     'cmdEndDebugUtilsLabelEXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndDebugUtilsLabelEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndDebugUtilsLabelEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndDebugUtilsLabelEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdEndDebugUtilsLabelEXT :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command is
                            -- recorded.
                            CommandBuffer
                         -> io ()
cmdEndDebugUtilsLabelEXT commandBuffer = liftIO $ do
  let vkCmdEndDebugUtilsLabelEXTPtr = pVkCmdEndDebugUtilsLabelEXT (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdEndDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndDebugUtilsLabelEXT is null" Nothing Nothing
  let vkCmdEndDebugUtilsLabelEXT' = mkVkCmdEndDebugUtilsLabelEXT vkCmdEndDebugUtilsLabelEXTPtr
  traceAroundEvent "vkCmdEndDebugUtilsLabelEXT" (vkCmdEndDebugUtilsLabelEXT' (commandBufferHandle (commandBuffer)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdInsertDebugUtilsLabelEXT
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DebugUtilsLabelEXT -> IO ()) -> Ptr CommandBuffer_T -> Ptr DebugUtilsLabelEXT -> IO ()

-- | vkCmdInsertDebugUtilsLabelEXT - Insert a label into a command buffer
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdInsertDebugUtilsLabelEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdInsertDebugUtilsLabelEXT-pLabelInfo-parameter#
--     @pLabelInfo@ /must/ be a valid pointer to a valid
--     'DebugUtilsLabelEXT' structure
--
-- -   #VUID-vkCmdInsertDebugUtilsLabelEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdInsertDebugUtilsLabelEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics, or compute operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Graphics                                                                                                              |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        | Compute                                                                                                               |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DebugUtilsLabelEXT'
cmdInsertDebugUtilsLabelEXT :: forall io
                             . (MonadIO io)
                            => -- | @commandBuffer@ is the command buffer into which the command is
                               -- recorded.
                               CommandBuffer
                            -> -- No documentation found for Nested "vkCmdInsertDebugUtilsLabelEXT" "pLabelInfo"
                               ("labelInfo" ::: DebugUtilsLabelEXT)
                            -> io ()
cmdInsertDebugUtilsLabelEXT commandBuffer labelInfo = liftIO . evalContT $ do
  let vkCmdInsertDebugUtilsLabelEXTPtr = pVkCmdInsertDebugUtilsLabelEXT (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdInsertDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdInsertDebugUtilsLabelEXT is null" Nothing Nothing
  let vkCmdInsertDebugUtilsLabelEXT' = mkVkCmdInsertDebugUtilsLabelEXT vkCmdInsertDebugUtilsLabelEXTPtr
  pLabelInfo <- ContT $ withCStruct (labelInfo)
  lift $ traceAroundEvent "vkCmdInsertDebugUtilsLabelEXT" (vkCmdInsertDebugUtilsLabelEXT' (commandBufferHandle (commandBuffer)) pLabelInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDebugUtilsMessengerEXT
  :: FunPtr (Ptr Instance_T -> Ptr DebugUtilsMessengerCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr DebugUtilsMessengerEXT -> IO Result) -> Ptr Instance_T -> Ptr DebugUtilsMessengerCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr DebugUtilsMessengerEXT -> IO Result

-- | vkCreateDebugUtilsMessengerEXT - Create a debug messenger object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDebugUtilsMessengerEXT-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateDebugUtilsMessengerEXT-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'DebugUtilsMessengerCreateInfoEXT' structure
--
-- -   #VUID-vkCreateDebugUtilsMessengerEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateDebugUtilsMessengerEXT-pMessenger-parameter#
--     @pMessenger@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- The application /must/ ensure that 'createDebugUtilsMessengerEXT' is not
-- executed in parallel with any Vulkan command that is also called with
-- @instance@ or child of @instance@ as the dispatchable argument.
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'DebugUtilsMessengerCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT',
-- 'Vulkan.Core10.Handles.Instance'
createDebugUtilsMessengerEXT :: forall io
                              . (MonadIO io)
                             => -- | @instance@ is the instance the messenger will be used with.
                                Instance
                             -> -- | @pCreateInfo@ is a pointer to a 'DebugUtilsMessengerCreateInfoEXT'
                                -- structure containing the callback pointer, as well as defining
                                -- conditions under which this messenger will trigger the callback.
                                DebugUtilsMessengerCreateInfoEXT
                             -> -- | @pAllocator@ controls host memory allocation as described in the
                                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                -- chapter.
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (DebugUtilsMessengerEXT)
createDebugUtilsMessengerEXT instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateDebugUtilsMessengerEXTPtr = pVkCreateDebugUtilsMessengerEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateDebugUtilsMessengerEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDebugUtilsMessengerEXT is null" Nothing Nothing
  let vkCreateDebugUtilsMessengerEXT' = mkVkCreateDebugUtilsMessengerEXT vkCreateDebugUtilsMessengerEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPMessenger <- ContT $ bracket (callocBytes @DebugUtilsMessengerEXT 8) free
  r <- lift $ traceAroundEvent "vkCreateDebugUtilsMessengerEXT" (vkCreateDebugUtilsMessengerEXT' (instanceHandle (instance')) pCreateInfo pAllocator (pPMessenger))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pMessenger <- lift $ peek @DebugUtilsMessengerEXT pPMessenger
  pure $ (pMessenger)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDebugUtilsMessengerEXT' and 'destroyDebugUtilsMessengerEXT'
--
-- To ensure that 'destroyDebugUtilsMessengerEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDebugUtilsMessengerEXT :: forall io r . MonadIO io => Instance -> DebugUtilsMessengerCreateInfoEXT -> Maybe AllocationCallbacks -> (io DebugUtilsMessengerEXT -> (DebugUtilsMessengerEXT -> io ()) -> r) -> r
withDebugUtilsMessengerEXT instance' pCreateInfo pAllocator b =
  b (createDebugUtilsMessengerEXT instance' pCreateInfo pAllocator)
    (\(o0) -> destroyDebugUtilsMessengerEXT instance' o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDebugUtilsMessengerEXT
  :: FunPtr (Ptr Instance_T -> DebugUtilsMessengerEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Instance_T -> DebugUtilsMessengerEXT -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyDebugUtilsMessengerEXT - Destroy a debug messenger object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyDebugUtilsMessengerEXT-messenger-01915# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @messenger@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyDebugUtilsMessengerEXT-messenger-01916# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @messenger@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyDebugUtilsMessengerEXT-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkDestroyDebugUtilsMessengerEXT-messenger-parameter# If
--     @messenger@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @messenger@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' handle
--
-- -   #VUID-vkDestroyDebugUtilsMessengerEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyDebugUtilsMessengerEXT-messenger-parent# If
--     @messenger@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @instance@
--
-- == Host Synchronization
--
-- -   Host access to @messenger@ /must/ be externally synchronized
--
-- The application /must/ ensure that 'destroyDebugUtilsMessengerEXT' is
-- not executed in parallel with any Vulkan command that is also called
-- with @instance@ or child of @instance@ as the dispatchable argument.
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT',
-- 'Vulkan.Core10.Handles.Instance'
destroyDebugUtilsMessengerEXT :: forall io
                               . (MonadIO io)
                              => -- | @instance@ is the instance where the callback was created.
                                 Instance
                              -> -- | @messenger@ is the 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT'
                                 -- object to destroy. @messenger@ is an externally synchronized object and
                                 -- /must/ not be used on more than one thread at a time. This means that
                                 -- 'destroyDebugUtilsMessengerEXT' /must/ not be called when a callback is
                                 -- active.
                                 DebugUtilsMessengerEXT
                              -> -- | @pAllocator@ controls host memory allocation as described in the
                                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                 -- chapter.
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io ()
destroyDebugUtilsMessengerEXT instance' messenger allocator = liftIO . evalContT $ do
  let vkDestroyDebugUtilsMessengerEXTPtr = pVkDestroyDebugUtilsMessengerEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkDestroyDebugUtilsMessengerEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDebugUtilsMessengerEXT is null" Nothing Nothing
  let vkDestroyDebugUtilsMessengerEXT' = mkVkDestroyDebugUtilsMessengerEXT vkDestroyDebugUtilsMessengerEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyDebugUtilsMessengerEXT" (vkDestroyDebugUtilsMessengerEXT' (instanceHandle (instance')) (messenger) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSubmitDebugUtilsMessageEXT
  :: FunPtr (Ptr Instance_T -> DebugUtilsMessageSeverityFlagBitsEXT -> DebugUtilsMessageTypeFlagsEXT -> Ptr DebugUtilsMessengerCallbackDataEXT -> IO ()) -> Ptr Instance_T -> DebugUtilsMessageSeverityFlagBitsEXT -> DebugUtilsMessageTypeFlagsEXT -> Ptr DebugUtilsMessengerCallbackDataEXT -> IO ()

-- | vkSubmitDebugUtilsMessageEXT - Inject a message into a debug stream
--
-- = Description
--
-- The call will propagate through the layers and generate callback(s) as
-- indicated by the message’s flags. The parameters are passed on to the
-- callback in addition to the @pUserData@ value that was defined at the
-- time the messenger was registered.
--
-- == Valid Usage
--
-- -   #VUID-vkSubmitDebugUtilsMessageEXT-objectType-02591# The
--     @objectType@ member of each element of @pCallbackData->pObjects@
--     /must/ not be 'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkSubmitDebugUtilsMessageEXT-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkSubmitDebugUtilsMessageEXT-messageSeverity-parameter#
--     @messageSeverity@ /must/ be a valid
--     'DebugUtilsMessageSeverityFlagBitsEXT' value
--
-- -   #VUID-vkSubmitDebugUtilsMessageEXT-messageTypes-parameter#
--     @messageTypes@ /must/ be a valid combination of
--     'DebugUtilsMessageTypeFlagBitsEXT' values
--
-- -   #VUID-vkSubmitDebugUtilsMessageEXT-messageTypes-requiredbitmask#
--     @messageTypes@ /must/ not be @0@
--
-- -   #VUID-vkSubmitDebugUtilsMessageEXT-pCallbackData-parameter#
--     @pCallbackData@ /must/ be a valid pointer to a valid
--     'DebugUtilsMessengerCallbackDataEXT' structure
--
-- = See Also
--
-- 'DebugUtilsMessageSeverityFlagBitsEXT', 'DebugUtilsMessageTypeFlagsEXT',
-- 'DebugUtilsMessengerCallbackDataEXT', 'Vulkan.Core10.Handles.Instance'
submitDebugUtilsMessageEXT :: forall io
                            . (MonadIO io)
                           => -- | @instance@ is the debug stream’s 'Vulkan.Core10.Handles.Instance'.
                              Instance
                           -> -- | @messageSeverity@ is the 'DebugUtilsMessageSeverityFlagBitsEXT' severity
                              -- of this event\/message.
                              DebugUtilsMessageSeverityFlagBitsEXT
                           -> -- | @messageTypes@ is a bitmask of 'DebugUtilsMessageTypeFlagBitsEXT'
                              -- specifying which type of event(s) to identify with this message.
                              ("messageTypes" ::: DebugUtilsMessageTypeFlagsEXT)
                           -> -- | @pCallbackData@ contains all the callback related data in the
                              -- 'DebugUtilsMessengerCallbackDataEXT' structure.
                              DebugUtilsMessengerCallbackDataEXT
                           -> io ()
submitDebugUtilsMessageEXT instance' messageSeverity messageTypes callbackData = liftIO . evalContT $ do
  let vkSubmitDebugUtilsMessageEXTPtr = pVkSubmitDebugUtilsMessageEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkSubmitDebugUtilsMessageEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSubmitDebugUtilsMessageEXT is null" Nothing Nothing
  let vkSubmitDebugUtilsMessageEXT' = mkVkSubmitDebugUtilsMessageEXT vkSubmitDebugUtilsMessageEXTPtr
  pCallbackData <- ContT $ withCStruct (callbackData)
  lift $ traceAroundEvent "vkSubmitDebugUtilsMessageEXT" (vkSubmitDebugUtilsMessageEXT' (instanceHandle (instance')) (messageSeverity) (messageTypes) pCallbackData)
  pure $ ()


-- | VkDebugUtilsObjectNameInfoEXT - Specify parameters of a name to give to
-- an object
--
-- = Description
--
-- Applications /may/ change the name associated with an object simply by
-- calling 'setDebugUtilsObjectNameEXT' again with a new string. If
-- @pObjectName@ is either @NULL@ or an empty string, then any previously
-- set name is removed.
--
-- == Valid Usage
--
-- -   #VUID-VkDebugUtilsObjectNameInfoEXT-objectType-02589# If
--     @objectType@ is
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN', @objectHandle@
--     /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkDebugUtilsObjectNameInfoEXT-objectType-02590# If
--     @objectType@ is not
--     'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN', @objectHandle@
--     /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE' or a valid Vulkan
--     handle of the type associated with @objectType@ as defined in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-object-types  and Vulkan Handle Relationship>
--     table
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDebugUtilsObjectNameInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT'
--
-- -   #VUID-VkDebugUtilsObjectNameInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkDebugUtilsObjectNameInfoEXT-objectType-parameter#
--     @objectType@ /must/ be a valid
--     'Vulkan.Core10.Enums.ObjectType.ObjectType' value
--
-- -   #VUID-VkDebugUtilsObjectNameInfoEXT-pObjectName-parameter# If
--     @pObjectName@ is not @NULL@, @pObjectName@ /must/ be a
--     null-terminated UTF-8 string
--
-- = See Also
--
-- 'DebugUtilsMessengerCallbackDataEXT',
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'setDebugUtilsObjectNameEXT'
data DebugUtilsObjectNameInfoEXT = DebugUtilsObjectNameInfoEXT
  { -- | @objectType@ is a 'Vulkan.Core10.Enums.ObjectType.ObjectType' specifying
    -- the type of the object to be named.
    objectType :: ObjectType
  , -- | @objectHandle@ is the object to be named.
    objectHandle :: Word64
  , -- | @pObjectName@ is either @NULL@ or a null-terminated UTF-8 string
    -- specifying the name to apply to @objectHandle@.
    objectName :: Maybe ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsObjectNameInfoEXT)
#endif
deriving instance Show DebugUtilsObjectNameInfoEXT

instance ToCStruct DebugUtilsObjectNameInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsObjectNameInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr ObjectType)) (objectType)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word64)) (objectHandle)
    pObjectName'' <- case (objectName) of
      Nothing -> pure nullPtr
      Just j -> ContT $ useAsCString (j)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) pObjectName''
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ObjectType)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct DebugUtilsObjectNameInfoEXT where
  peekCStruct p = do
    objectType <- peek @ObjectType ((p `plusPtr` 16 :: Ptr ObjectType))
    objectHandle <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pObjectName <- peek @(Ptr CChar) ((p `plusPtr` 32 :: Ptr (Ptr CChar)))
    pObjectName' <- maybePeek (\j -> packCString (j)) pObjectName
    pure $ DebugUtilsObjectNameInfoEXT
             objectType objectHandle pObjectName'

instance Zero DebugUtilsObjectNameInfoEXT where
  zero = DebugUtilsObjectNameInfoEXT
           zero
           zero
           Nothing


-- | VkDebugUtilsObjectTagInfoEXT - Specify parameters of a tag to attach to
-- an object
--
-- = Description
--
-- The @tagName@ parameter gives a name or identifier to the type of data
-- being tagged. This can be used by debugging layers to easily filter for
-- only data that can be used by that implementation.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ObjectType.ObjectType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'setDebugUtilsObjectTagEXT'
data DebugUtilsObjectTagInfoEXT = DebugUtilsObjectTagInfoEXT
  { -- | @objectType@ is a 'Vulkan.Core10.Enums.ObjectType.ObjectType' specifying
    -- the type of the object to be named.
    --
    -- #VUID-VkDebugUtilsObjectTagInfoEXT-objectType-01908# @objectType@ /must/
    -- not be 'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
    --
    -- #VUID-VkDebugUtilsObjectTagInfoEXT-objectType-parameter# @objectType@
    -- /must/ be a valid 'Vulkan.Core10.Enums.ObjectType.ObjectType' value
    objectType :: ObjectType
  , -- | @objectHandle@ is the object to be tagged.
    --
    -- #VUID-VkDebugUtilsObjectTagInfoEXT-objectHandle-01910# @objectHandle@
    -- /must/ be a valid Vulkan handle of the type associated with @objectType@
    -- as defined in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-object-types  and Vulkan Handle Relationship>
    -- table
    objectHandle :: Word64
  , -- | @tagName@ is a numerical identifier of the tag.
    tagName :: Word64
  , -- | @tagSize@ is the number of bytes of data to attach to the object.
    --
    -- #VUID-VkDebugUtilsObjectTagInfoEXT-tagSize-arraylength# @tagSize@ /must/
    -- be greater than @0@
    tagSize :: Word64
  , -- | @pTag@ is a pointer to an array of @tagSize@ bytes containing the data
    -- to be associated with the object.
    --
    -- #VUID-VkDebugUtilsObjectTagInfoEXT-pTag-parameter# @pTag@ /must/ be a
    -- valid pointer to an array of @tagSize@ bytes
    tag :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsObjectTagInfoEXT)
#endif
deriving instance Show DebugUtilsObjectTagInfoEXT

instance ToCStruct DebugUtilsObjectTagInfoEXT where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsObjectTagInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ObjectType)) (objectType)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (objectHandle)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (tagName)
    poke ((p `plusPtr` 40 :: Ptr CSize)) (CSize (tagSize))
    poke ((p `plusPtr` 48 :: Ptr (Ptr ()))) (tag)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ObjectType)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr CSize)) (CSize (zero))
    poke ((p `plusPtr` 48 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct DebugUtilsObjectTagInfoEXT where
  peekCStruct p = do
    objectType <- peek @ObjectType ((p `plusPtr` 16 :: Ptr ObjectType))
    objectHandle <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    tagName <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    tagSize <- peek @CSize ((p `plusPtr` 40 :: Ptr CSize))
    pTag <- peek @(Ptr ()) ((p `plusPtr` 48 :: Ptr (Ptr ())))
    pure $ DebugUtilsObjectTagInfoEXT
             objectType objectHandle tagName (coerce @CSize @Word64 tagSize) pTag

instance Storable DebugUtilsObjectTagInfoEXT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DebugUtilsObjectTagInfoEXT where
  zero = DebugUtilsObjectTagInfoEXT
           zero
           zero
           zero
           zero
           zero


-- | VkDebugUtilsLabelEXT - Specify parameters of a label region
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DebugUtilsMessengerCallbackDataEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBeginDebugUtilsLabelEXT', 'cmdInsertDebugUtilsLabelEXT',
-- 'queueBeginDebugUtilsLabelEXT', 'queueInsertDebugUtilsLabelEXT'
data DebugUtilsLabelEXT = DebugUtilsLabelEXT
  { -- | @pLabelName@ is a pointer to a null-terminated UTF-8 string containing
    -- the name of the label.
    --
    -- #VUID-VkDebugUtilsLabelEXT-pLabelName-parameter# @pLabelName@ /must/ be
    -- a null-terminated UTF-8 string
    labelName :: ByteString
  , -- | @color@ is an optional RGBA color value that can be associated with the
    -- label. A particular implementation /may/ choose to ignore this color
    -- value. The values contain RGBA values in order, in the range 0.0 to 1.0.
    -- If all elements in @color@ are set to 0.0 then it is ignored.
    color :: (Float, Float, Float, Float)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsLabelEXT)
#endif
deriving instance Show DebugUtilsLabelEXT

instance ToCStruct DebugUtilsLabelEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsLabelEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pLabelName'' <- ContT $ useAsCString (labelName)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pLabelName''
    let pColor' = lowerArrayPtr ((p `plusPtr` 24 :: Ptr (FixedArray 4 CFloat)))
    lift $ case (color) of
      (e0, e1, e2, e3) -> do
        poke (pColor' :: Ptr CFloat) (CFloat (e0))
        poke (pColor' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pColor' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pColor' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pLabelName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) pLabelName''
    lift $ f

instance FromCStruct DebugUtilsLabelEXT where
  peekCStruct p = do
    pLabelName <- packCString =<< peek ((p `plusPtr` 16 :: Ptr (Ptr CChar)))
    let pcolor = lowerArrayPtr @CFloat ((p `plusPtr` 24 :: Ptr (FixedArray 4 CFloat)))
    color0 <- peek @CFloat ((pcolor `advancePtrBytes` 0 :: Ptr CFloat))
    color1 <- peek @CFloat ((pcolor `advancePtrBytes` 4 :: Ptr CFloat))
    color2 <- peek @CFloat ((pcolor `advancePtrBytes` 8 :: Ptr CFloat))
    color3 <- peek @CFloat ((pcolor `advancePtrBytes` 12 :: Ptr CFloat))
    pure $ DebugUtilsLabelEXT
             pLabelName (((coerce @CFloat @Float color0), (coerce @CFloat @Float color1), (coerce @CFloat @Float color2), (coerce @CFloat @Float color3)))

instance Zero DebugUtilsLabelEXT where
  zero = DebugUtilsLabelEXT
           mempty
           (zero, zero, zero, zero)


-- | VkDebugUtilsMessengerCreateInfoEXT - Structure specifying parameters of
-- a newly created debug messenger
--
-- = Description
--
-- For each 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' that is
-- created the 'DebugUtilsMessengerCreateInfoEXT'::@messageSeverity@ and
-- 'DebugUtilsMessengerCreateInfoEXT'::@messageType@ determine when that
-- 'DebugUtilsMessengerCreateInfoEXT'::@pfnUserCallback@ is called. The
-- process to determine if the user’s @pfnUserCallback@ is triggered when
-- an event occurs is as follows:
--
-- 1.  The implementation will perform a bitwise AND of the event’s
--     'DebugUtilsMessageSeverityFlagBitsEXT' with the @messageSeverity@
--     provided during creation of the
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' object.
--
--     1.  If the value is 0, the message is skipped.
--
-- 2.  The implementation will perform bitwise AND of the event’s
--     'DebugUtilsMessageTypeFlagBitsEXT' with the @messageType@ provided
--     during the creation of the
--     'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' object.
--
--     1.  If the value is 0, the message is skipped.
--
-- 3.  The callback will trigger a debug message for the current event
--
-- The callback will come directly from the component that detected the
-- event, unless some other layer intercepts the calls for its own purposes
-- (filter them in a different way, log to a system error log, etc.).
--
-- An application /can/ receive multiple callbacks if multiple
-- 'Vulkan.Extensions.Handles.DebugUtilsMessengerEXT' objects are created.
-- A callback will always be executed in the same thread as the originating
-- Vulkan call.
--
-- A callback /can/ be called from multiple threads simultaneously (if the
-- application is making Vulkan calls from multiple threads).
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PFN_vkDebugUtilsMessengerCallbackEXT',
-- 'DebugUtilsMessageSeverityFlagsEXT', 'DebugUtilsMessageTypeFlagsEXT',
-- 'DebugUtilsMessengerCreateFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDebugUtilsMessengerEXT'
data DebugUtilsMessengerCreateInfoEXT = DebugUtilsMessengerCreateInfoEXT
  { -- | @flags@ is @0@ and is reserved for future use.
    --
    -- #VUID-VkDebugUtilsMessengerCreateInfoEXT-flags-zerobitmask# @flags@
    -- /must/ be @0@
    flags :: DebugUtilsMessengerCreateFlagsEXT
  , -- | @messageSeverity@ is a bitmask of 'DebugUtilsMessageSeverityFlagBitsEXT'
    -- specifying which severity of event(s) will cause this callback to be
    -- called.
    --
    -- #VUID-VkDebugUtilsMessengerCreateInfoEXT-messageSeverity-parameter#
    -- @messageSeverity@ /must/ be a valid combination of
    -- 'DebugUtilsMessageSeverityFlagBitsEXT' values
    --
    -- #VUID-VkDebugUtilsMessengerCreateInfoEXT-messageSeverity-requiredbitmask#
    -- @messageSeverity@ /must/ not be @0@
    messageSeverity :: DebugUtilsMessageSeverityFlagsEXT
  , -- | @messageType@ is a bitmask of 'DebugUtilsMessageTypeFlagBitsEXT'
    -- specifying which type of event(s) will cause this callback to be called.
    --
    -- #VUID-VkDebugUtilsMessengerCreateInfoEXT-messageType-parameter#
    -- @messageType@ /must/ be a valid combination of
    -- 'DebugUtilsMessageTypeFlagBitsEXT' values
    --
    -- #VUID-VkDebugUtilsMessengerCreateInfoEXT-messageType-requiredbitmask#
    -- @messageType@ /must/ not be @0@
    messageType :: DebugUtilsMessageTypeFlagsEXT
  , -- | @pfnUserCallback@ is the application callback function to call.
    --
    -- #VUID-VkDebugUtilsMessengerCreateInfoEXT-pfnUserCallback-01914#
    -- @pfnUserCallback@ /must/ be a valid
    -- 'PFN_vkDebugUtilsMessengerCallbackEXT'
    --
    -- #VUID-VkDebugUtilsMessengerCreateInfoEXT-pfnUserCallback-parameter#
    -- @pfnUserCallback@ /must/ be a valid
    -- 'PFN_vkDebugUtilsMessengerCallbackEXT' value
    pfnUserCallback :: PFN_vkDebugUtilsMessengerCallbackEXT
  , -- | @pUserData@ is user data to be passed to the callback.
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsMessengerCreateInfoEXT)
#endif
deriving instance Show DebugUtilsMessengerCreateInfoEXT

instance ToCStruct DebugUtilsMessengerCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsMessengerCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DebugUtilsMessengerCreateFlagsEXT)) (flags)
    poke ((p `plusPtr` 20 :: Ptr DebugUtilsMessageSeverityFlagsEXT)) (messageSeverity)
    poke ((p `plusPtr` 24 :: Ptr DebugUtilsMessageTypeFlagsEXT)) (messageType)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkDebugUtilsMessengerCallbackEXT)) (pfnUserCallback)
    poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr DebugUtilsMessageSeverityFlagsEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DebugUtilsMessageTypeFlagsEXT)) (zero)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkDebugUtilsMessengerCallbackEXT)) (zero)
    f

instance FromCStruct DebugUtilsMessengerCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @DebugUtilsMessengerCreateFlagsEXT ((p `plusPtr` 16 :: Ptr DebugUtilsMessengerCreateFlagsEXT))
    messageSeverity <- peek @DebugUtilsMessageSeverityFlagsEXT ((p `plusPtr` 20 :: Ptr DebugUtilsMessageSeverityFlagsEXT))
    messageType <- peek @DebugUtilsMessageTypeFlagsEXT ((p `plusPtr` 24 :: Ptr DebugUtilsMessageTypeFlagsEXT))
    pfnUserCallback <- peek @PFN_vkDebugUtilsMessengerCallbackEXT ((p `plusPtr` 32 :: Ptr PFN_vkDebugUtilsMessengerCallbackEXT))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pure $ DebugUtilsMessengerCreateInfoEXT
             flags messageSeverity messageType pfnUserCallback pUserData

instance Storable DebugUtilsMessengerCreateInfoEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DebugUtilsMessengerCreateInfoEXT where
  zero = DebugUtilsMessengerCreateInfoEXT
           zero
           zero
           zero
           zero
           zero


-- | VkDebugUtilsMessengerCallbackDataEXT - Structure specifying parameters
-- returned to the callback
--
-- = Description
--
-- Note
--
-- This structure should only be considered valid during the lifetime of
-- the triggered callback.
--
-- Since adding queue and command buffer labels behaves like pushing and
-- popping onto a stack, the order of both @pQueueLabels@ and
-- @pCmdBufLabels@ is based on the order the labels were defined. The
-- result is that the first label in either @pQueueLabels@ or
-- @pCmdBufLabels@ will be the first defined (and therefore the oldest)
-- while the last label in each list will be the most recent.
--
-- Note
--
-- @pQueueLabels@ will only be non-@NULL@ if one of the objects in
-- @pObjects@ can be related directly to a defined
-- 'Vulkan.Core10.Handles.Queue' which has had one or more labels
-- associated with it.
--
-- Likewise, @pCmdBufLabels@ will only be non-@NULL@ if one of the objects
-- in @pObjects@ can be related directly to a defined
-- 'Vulkan.Core10.Handles.CommandBuffer' which has had one or more labels
-- associated with it. Additionally, while command buffer labels allow for
-- beginning and ending across different command buffers, the debug
-- messaging framework /cannot/ guarantee that labels in @pCmdBufLables@
-- will contain those defined outside of the associated command buffer.
-- This is partially due to the fact that the association of one command
-- buffer with another may not have been defined at the time the debug
-- message is triggered.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDebugUtilsMessengerCallbackDataEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT'
--
-- -   #VUID-VkDebugUtilsMessengerCallbackDataEXT-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkDebugUtilsMessengerCallbackDataEXT-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- -   #VUID-VkDebugUtilsMessengerCallbackDataEXT-pMessageIdName-parameter#
--     If @pMessageIdName@ is not @NULL@, @pMessageIdName@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   #VUID-VkDebugUtilsMessengerCallbackDataEXT-pMessage-parameter#
--     @pMessage@ /must/ be a null-terminated UTF-8 string
--
-- -   #VUID-VkDebugUtilsMessengerCallbackDataEXT-pQueueLabels-parameter#
--     If @queueLabelCount@ is not @0@, @pQueueLabels@ /must/ be a valid
--     pointer to an array of @queueLabelCount@ valid 'DebugUtilsLabelEXT'
--     structures
--
-- -   #VUID-VkDebugUtilsMessengerCallbackDataEXT-pCmdBufLabels-parameter#
--     If @cmdBufLabelCount@ is not @0@, @pCmdBufLabels@ /must/ be a valid
--     pointer to an array of @cmdBufLabelCount@ valid 'DebugUtilsLabelEXT'
--     structures
--
-- -   #VUID-VkDebugUtilsMessengerCallbackDataEXT-pObjects-parameter# If
--     @objectCount@ is not @0@, @pObjects@ /must/ be a valid pointer to an
--     array of @objectCount@ valid 'DebugUtilsObjectNameInfoEXT'
--     structures
--
-- = See Also
--
-- 'DebugUtilsLabelEXT', 'DebugUtilsMessengerCallbackDataFlagsEXT',
-- 'DebugUtilsObjectNameInfoEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'submitDebugUtilsMessageEXT'
data DebugUtilsMessengerCallbackDataEXT = DebugUtilsMessengerCallbackDataEXT
  { -- | @flags@ is @0@ and is reserved for future use.
    flags :: DebugUtilsMessengerCallbackDataFlagsEXT
  , -- | @pMessageIdName@ is a null-terminated string that identifies the
    -- particular message ID that is associated with the provided message. If
    -- the message corresponds to a validation layer message, then this string
    -- may contain the portion of the Vulkan specification that is believed to
    -- have been violated.
    messageIdName :: Maybe ByteString
  , -- | @messageIdNumber@ is the ID number of the triggering message. If the
    -- message corresponds to a validation layer message, then this number is
    -- related to the internal number associated with the message being
    -- triggered.
    messageIdNumber :: Int32
  , -- | @pMessage@ is a null-terminated string detailing the trigger conditions.
    message :: ByteString
  , -- | @pQueueLabels@ is @NULL@ or a pointer to an array of
    -- 'DebugUtilsLabelEXT' active in the current 'Vulkan.Core10.Handles.Queue'
    -- at the time the callback was triggered. Refer to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-queue-labels Queue Labels>
    -- for more information.
    queueLabels :: Vector DebugUtilsLabelEXT
  , -- | @pCmdBufLabels@ is @NULL@ or a pointer to an array of
    -- 'DebugUtilsLabelEXT' active in the current
    -- 'Vulkan.Core10.Handles.CommandBuffer' at the time the callback was
    -- triggered. Refer to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debugging-command-buffer-labels Command Buffer Labels>
    -- for more information.
    cmdBufLabels :: Vector DebugUtilsLabelEXT
  , -- | @pObjects@ is a pointer to an array of 'DebugUtilsObjectNameInfoEXT'
    -- objects related to the detected issue. The array is roughly in order or
    -- importance, but the 0th element is always guaranteed to be the most
    -- important object for this message.
    objects :: Vector DebugUtilsObjectNameInfoEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsMessengerCallbackDataEXT)
#endif
deriving instance Show DebugUtilsMessengerCallbackDataEXT

instance ToCStruct DebugUtilsMessengerCallbackDataEXT where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsMessengerCallbackDataEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DebugUtilsMessengerCallbackDataFlagsEXT)) (flags)
    pMessageIdName'' <- case (messageIdName) of
      Nothing -> pure nullPtr
      Just j -> ContT $ useAsCString (j)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CChar))) pMessageIdName''
    lift $ poke ((p `plusPtr` 32 :: Ptr Int32)) (messageIdNumber)
    pMessage'' <- ContT $ useAsCString (message)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr CChar))) pMessage''
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueLabels)) :: Word32))
    pPQueueLabels' <- ContT $ allocaBytesAligned @DebugUtilsLabelEXT ((Data.Vector.length (queueLabels)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPQueueLabels' `plusPtr` (40 * (i)) :: Ptr DebugUtilsLabelEXT) (e) . ($ ())) (queueLabels)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr DebugUtilsLabelEXT))) (pPQueueLabels')
    lift $ poke ((p `plusPtr` 64 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (cmdBufLabels)) :: Word32))
    pPCmdBufLabels' <- ContT $ allocaBytesAligned @DebugUtilsLabelEXT ((Data.Vector.length (cmdBufLabels)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCmdBufLabels' `plusPtr` (40 * (i)) :: Ptr DebugUtilsLabelEXT) (e) . ($ ())) (cmdBufLabels)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr DebugUtilsLabelEXT))) (pPCmdBufLabels')
    lift $ poke ((p `plusPtr` 80 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (objects)) :: Word32))
    pPObjects' <- ContT $ allocaBytesAligned @DebugUtilsObjectNameInfoEXT ((Data.Vector.length (objects)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPObjects' `plusPtr` (40 * (i)) :: Ptr DebugUtilsObjectNameInfoEXT) (e) . ($ ())) (objects)
    lift $ poke ((p `plusPtr` 88 :: Ptr (Ptr DebugUtilsObjectNameInfoEXT))) (pPObjects')
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pMessage'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr CChar))) pMessage''
    lift $ f

instance FromCStruct DebugUtilsMessengerCallbackDataEXT where
  peekCStruct p = do
    flags <- peek @DebugUtilsMessengerCallbackDataFlagsEXT ((p `plusPtr` 16 :: Ptr DebugUtilsMessengerCallbackDataFlagsEXT))
    pMessageIdName <- peek @(Ptr CChar) ((p `plusPtr` 24 :: Ptr (Ptr CChar)))
    pMessageIdName' <- maybePeek (\j -> packCString (j)) pMessageIdName
    messageIdNumber <- peek @Int32 ((p `plusPtr` 32 :: Ptr Int32))
    pMessage <- packCString =<< peek ((p `plusPtr` 40 :: Ptr (Ptr CChar)))
    queueLabelCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pQueueLabels <- peek @(Ptr DebugUtilsLabelEXT) ((p `plusPtr` 56 :: Ptr (Ptr DebugUtilsLabelEXT)))
    pQueueLabels' <- generateM (fromIntegral queueLabelCount) (\i -> peekCStruct @DebugUtilsLabelEXT ((pQueueLabels `advancePtrBytes` (40 * (i)) :: Ptr DebugUtilsLabelEXT)))
    cmdBufLabelCount <- peek @Word32 ((p `plusPtr` 64 :: Ptr Word32))
    pCmdBufLabels <- peek @(Ptr DebugUtilsLabelEXT) ((p `plusPtr` 72 :: Ptr (Ptr DebugUtilsLabelEXT)))
    pCmdBufLabels' <- generateM (fromIntegral cmdBufLabelCount) (\i -> peekCStruct @DebugUtilsLabelEXT ((pCmdBufLabels `advancePtrBytes` (40 * (i)) :: Ptr DebugUtilsLabelEXT)))
    objectCount <- peek @Word32 ((p `plusPtr` 80 :: Ptr Word32))
    pObjects <- peek @(Ptr DebugUtilsObjectNameInfoEXT) ((p `plusPtr` 88 :: Ptr (Ptr DebugUtilsObjectNameInfoEXT)))
    pObjects' <- generateM (fromIntegral objectCount) (\i -> peekCStruct @DebugUtilsObjectNameInfoEXT ((pObjects `advancePtrBytes` (40 * (i)) :: Ptr DebugUtilsObjectNameInfoEXT)))
    pure $ DebugUtilsMessengerCallbackDataEXT
             flags pMessageIdName' messageIdNumber pMessage pQueueLabels' pCmdBufLabels' pObjects'

instance Zero DebugUtilsMessengerCallbackDataEXT where
  zero = DebugUtilsMessengerCallbackDataEXT
           zero
           Nothing
           zero
           mempty
           mempty
           mempty
           mempty


-- | VkDebugUtilsMessengerCreateFlagsEXT - Reserved for future use
--
-- = Description
--
-- 'DebugUtilsMessengerCreateFlagsEXT' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'DebugUtilsMessengerCreateInfoEXT'
newtype DebugUtilsMessengerCreateFlagsEXT = DebugUtilsMessengerCreateFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDebugUtilsMessengerCreateFlagsEXT :: String
conNameDebugUtilsMessengerCreateFlagsEXT = "DebugUtilsMessengerCreateFlagsEXT"

enumPrefixDebugUtilsMessengerCreateFlagsEXT :: String
enumPrefixDebugUtilsMessengerCreateFlagsEXT = ""

showTableDebugUtilsMessengerCreateFlagsEXT :: [(DebugUtilsMessengerCreateFlagsEXT, String)]
showTableDebugUtilsMessengerCreateFlagsEXT = []

instance Show DebugUtilsMessengerCreateFlagsEXT where
  showsPrec = enumShowsPrec enumPrefixDebugUtilsMessengerCreateFlagsEXT
                            showTableDebugUtilsMessengerCreateFlagsEXT
                            conNameDebugUtilsMessengerCreateFlagsEXT
                            (\(DebugUtilsMessengerCreateFlagsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DebugUtilsMessengerCreateFlagsEXT where
  readPrec = enumReadPrec enumPrefixDebugUtilsMessengerCreateFlagsEXT
                          showTableDebugUtilsMessengerCreateFlagsEXT
                          conNameDebugUtilsMessengerCreateFlagsEXT
                          DebugUtilsMessengerCreateFlagsEXT


-- | VkDebugUtilsMessengerCallbackDataFlagsEXT - Reserved for future use
--
-- = Description
--
-- 'DebugUtilsMessengerCallbackDataFlagsEXT' is a bitmask type for setting
-- a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'DebugUtilsMessengerCallbackDataEXT'
newtype DebugUtilsMessengerCallbackDataFlagsEXT = DebugUtilsMessengerCallbackDataFlagsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDebugUtilsMessengerCallbackDataFlagsEXT :: String
conNameDebugUtilsMessengerCallbackDataFlagsEXT = "DebugUtilsMessengerCallbackDataFlagsEXT"

enumPrefixDebugUtilsMessengerCallbackDataFlagsEXT :: String
enumPrefixDebugUtilsMessengerCallbackDataFlagsEXT = ""

showTableDebugUtilsMessengerCallbackDataFlagsEXT :: [(DebugUtilsMessengerCallbackDataFlagsEXT, String)]
showTableDebugUtilsMessengerCallbackDataFlagsEXT = []

instance Show DebugUtilsMessengerCallbackDataFlagsEXT where
  showsPrec = enumShowsPrec enumPrefixDebugUtilsMessengerCallbackDataFlagsEXT
                            showTableDebugUtilsMessengerCallbackDataFlagsEXT
                            conNameDebugUtilsMessengerCallbackDataFlagsEXT
                            (\(DebugUtilsMessengerCallbackDataFlagsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DebugUtilsMessengerCallbackDataFlagsEXT where
  readPrec = enumReadPrec enumPrefixDebugUtilsMessengerCallbackDataFlagsEXT
                          showTableDebugUtilsMessengerCallbackDataFlagsEXT
                          conNameDebugUtilsMessengerCallbackDataFlagsEXT
                          DebugUtilsMessengerCallbackDataFlagsEXT


type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT

-- | VkDebugUtilsMessageSeverityFlagBitsEXT - Bitmask specifying which
-- severities of events cause a debug messenger callback
--
-- = See Also
--
-- 'DebugUtilsMessageSeverityFlagsEXT', 'submitDebugUtilsMessageEXT'
newtype DebugUtilsMessageSeverityFlagBitsEXT = DebugUtilsMessageSeverityFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT' specifies the most
-- verbose output indicating all diagnostic messages from the Vulkan
-- loader, layers, and drivers should be captured.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x00000001
-- | 'DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT' specifies an informational
-- message such as resource details that may be handy when debugging an
-- application.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT    = DebugUtilsMessageSeverityFlagBitsEXT 0x00000010
-- | 'DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT' specifies use of Vulkan
-- that /may/ expose an app bug. Such cases may not be immediately harmful,
-- such as a fragment shader outputting to a location with no attachment.
-- Other cases /may/ point to behavior that is almost certainly bad when
-- unintended such as using an image whose memory has not been filled. In
-- general if you see a warning but you know that the behavior is
-- intended\/desired, then simply ignore the warning.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x00000100
-- | 'DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT' specifies that the
-- application has violated a valid usage condition of the specification.
pattern DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT   = DebugUtilsMessageSeverityFlagBitsEXT 0x00001000

conNameDebugUtilsMessageSeverityFlagBitsEXT :: String
conNameDebugUtilsMessageSeverityFlagBitsEXT = "DebugUtilsMessageSeverityFlagBitsEXT"

enumPrefixDebugUtilsMessageSeverityFlagBitsEXT :: String
enumPrefixDebugUtilsMessageSeverityFlagBitsEXT = "DEBUG_UTILS_MESSAGE_SEVERITY_"

showTableDebugUtilsMessageSeverityFlagBitsEXT :: [(DebugUtilsMessageSeverityFlagBitsEXT, String)]
showTableDebugUtilsMessageSeverityFlagBitsEXT =
  [ (DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT, "VERBOSE_BIT_EXT")
  , (DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT   , "INFO_BIT_EXT")
  , (DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT, "WARNING_BIT_EXT")
  , (DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT  , "ERROR_BIT_EXT")
  ]

instance Show DebugUtilsMessageSeverityFlagBitsEXT where
  showsPrec = enumShowsPrec enumPrefixDebugUtilsMessageSeverityFlagBitsEXT
                            showTableDebugUtilsMessageSeverityFlagBitsEXT
                            conNameDebugUtilsMessageSeverityFlagBitsEXT
                            (\(DebugUtilsMessageSeverityFlagBitsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DebugUtilsMessageSeverityFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixDebugUtilsMessageSeverityFlagBitsEXT
                          showTableDebugUtilsMessageSeverityFlagBitsEXT
                          conNameDebugUtilsMessageSeverityFlagBitsEXT
                          DebugUtilsMessageSeverityFlagBitsEXT


type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT

-- | VkDebugUtilsMessageTypeFlagBitsEXT - Bitmask specifying which types of
-- events cause a debug messenger callback
--
-- = See Also
--
-- 'DebugUtilsMessageTypeFlagsEXT'
newtype DebugUtilsMessageTypeFlagBitsEXT = DebugUtilsMessageTypeFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT' specifies that some general
-- event has occurred. This is typically a non-specification,
-- non-performance event.
pattern DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT     = DebugUtilsMessageTypeFlagBitsEXT 0x00000001
-- | 'DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT' specifies that something
-- has occurred during validation against the Vulkan specification that may
-- indicate invalid behavior.
pattern DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT  = DebugUtilsMessageTypeFlagBitsEXT 0x00000002
-- | 'DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT' specifies a potentially
-- non-optimal use of Vulkan, e.g. using
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage' when setting
-- 'Vulkan.Core10.Pass.AttachmentDescription'::@loadOp@ to
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR' would
-- have worked.
pattern DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = DebugUtilsMessageTypeFlagBitsEXT 0x00000004

conNameDebugUtilsMessageTypeFlagBitsEXT :: String
conNameDebugUtilsMessageTypeFlagBitsEXT = "DebugUtilsMessageTypeFlagBitsEXT"

enumPrefixDebugUtilsMessageTypeFlagBitsEXT :: String
enumPrefixDebugUtilsMessageTypeFlagBitsEXT = "DEBUG_UTILS_MESSAGE_TYPE_"

showTableDebugUtilsMessageTypeFlagBitsEXT :: [(DebugUtilsMessageTypeFlagBitsEXT, String)]
showTableDebugUtilsMessageTypeFlagBitsEXT =
  [ (DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT    , "GENERAL_BIT_EXT")
  , (DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT , "VALIDATION_BIT_EXT")
  , (DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT, "PERFORMANCE_BIT_EXT")
  ]

instance Show DebugUtilsMessageTypeFlagBitsEXT where
  showsPrec = enumShowsPrec enumPrefixDebugUtilsMessageTypeFlagBitsEXT
                            showTableDebugUtilsMessageTypeFlagBitsEXT
                            conNameDebugUtilsMessageTypeFlagBitsEXT
                            (\(DebugUtilsMessageTypeFlagBitsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DebugUtilsMessageTypeFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixDebugUtilsMessageTypeFlagBitsEXT
                          showTableDebugUtilsMessageTypeFlagBitsEXT
                          conNameDebugUtilsMessageTypeFlagBitsEXT
                          DebugUtilsMessageTypeFlagBitsEXT


type FN_vkDebugUtilsMessengerCallbackEXT = DebugUtilsMessageSeverityFlagBitsEXT -> ("messageTypes" ::: DebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr DebugUtilsMessengerCallbackDataEXT) -> ("pUserData" ::: Ptr ()) -> IO Bool32
-- | PFN_vkDebugUtilsMessengerCallbackEXT - Application-defined debug
-- messenger callback function
--
-- = Description
--
-- The callback /must/ not call 'destroyDebugUtilsMessengerEXT'.
--
-- The callback returns a 'Vulkan.Core10.FundamentalTypes.Bool32', which is
-- interpreted in a layer-specified manner. The application /should/ always
-- return 'Vulkan.Core10.FundamentalTypes.FALSE'. The
-- 'Vulkan.Core10.FundamentalTypes.TRUE' value is reserved for use in layer
-- development.
--
-- = See Also
--
-- 'DebugUtilsMessengerCreateInfoEXT'
type PFN_vkDebugUtilsMessengerCallbackEXT = FunPtr FN_vkDebugUtilsMessengerCallbackEXT


type EXT_DEBUG_UTILS_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_SPEC_VERSION"
pattern EXT_DEBUG_UTILS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEBUG_UTILS_SPEC_VERSION = 2


type EXT_DEBUG_UTILS_EXTENSION_NAME = "VK_EXT_debug_utils"

-- No documentation found for TopLevel "VK_EXT_DEBUG_UTILS_EXTENSION_NAME"
pattern EXT_DEBUG_UTILS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEBUG_UTILS_EXTENSION_NAME = "VK_EXT_debug_utils"

