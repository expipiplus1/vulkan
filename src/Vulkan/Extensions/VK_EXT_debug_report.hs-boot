{-# language CPP #-}
-- | = Name
--
-- VK_EXT_debug_report - instance extension
--
-- == VK_EXT_debug_report
--
-- [__Name String__]
--     @VK_EXT_debug_report@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     12
--
-- [__Revision__]
--     10
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_1
--
-- [__Deprecation State__]
--
--     -   /Deprecated/ by
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_utils VK_EXT_debug_utils>
--         extension
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Debugging tools>
--
-- [__Contact__]
--
--     -   Courtney Goeltzenleuchter
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_debug_report] @courtney-g%0A*Here describe the issue or question you have about the VK_EXT_debug_report extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-12-14
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Courtney Goeltzenleuchter, LunarG
--
--     -   Dan Ginsburg, Valve
--
--     -   Jon Ashburn, LunarG
--
--     -   Mark Lobodzinski, LunarG
--
-- == Description
--
-- Due to the nature of the Vulkan interface, there is very little error
-- information available to the developer and application. By enabling
-- optional validation layers and using the @VK_EXT_debug_report@
-- extension, developers /can/ obtain much more detailed feedback on the
-- application’s use of Vulkan. This extension defines a way for layers and
-- the implementation to call back to the application for events of
-- interest to the application.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.DebugReportCallbackEXT'
--
-- == New Commands
--
-- -   'createDebugReportCallbackEXT'
--
-- -   'debugReportMessageEXT'
--
-- -   'destroyDebugReportCallbackEXT'
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo':
--
--     -   'DebugReportCallbackCreateInfoEXT'
--
-- == New Function Pointers
--
-- -   'PFN_vkDebugReportCallbackEXT'
--
-- == New Enums
--
-- -   'DebugReportFlagBitsEXT'
--
-- -   'DebugReportObjectTypeEXT'
--
-- == New Bitmasks
--
-- -   'DebugReportFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEBUG_REPORT_EXTENSION_NAME'
--
-- -   'EXT_DEBUG_REPORT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- is supported:
--
-- -   Extending 'DebugReportObjectTypeEXT':
--
--     -   'DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT'
--
--     -   'DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT'
--
-- == Examples
--
-- @VK_EXT_debug_report@ allows an application to register multiple
-- callbacks with the validation layers. Some callbacks may log the
-- information to a file, others may cause a debug break point or other
-- application defined behavior. An application /can/ register callbacks
-- even when no validation layers are enabled, but they will only be called
-- for loader and, if implemented, driver events.
--
-- To capture events that occur while creating or destroying an instance an
-- application /can/ link a 'DebugReportCallbackCreateInfoEXT' structure to
-- the @pNext@ element of the
-- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo' structure given
-- to 'Vulkan.Core10.DeviceInitialization.createInstance'.
--
-- Example uses: Create three callback objects. One will log errors and
-- warnings to the debug console using Windows @OutputDebugString@. The
-- second will cause the debugger to break at that callback when an error
-- happens and the third will log warnings to stdout.
--
-- >     VkResult res;
-- >     VkDebugReportCallbackEXT cb1, cb2, cb3;
-- >
-- >     VkDebugReportCallbackCreateInfoEXT callback1 = {
-- >         .sType = VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = VK_DEBUG_REPORT_ERROR_BIT_EXT |
-- >                  VK_DEBUG_REPORT_WARNING_BIT_EXT,
-- >         .pfnCallback = myOutputDebugString,
-- >         .pUserData = NULL
-- >     };
-- >     res = vkCreateDebugReportCallbackEXT(instance, &callback1, &cb1);
-- >     if (res != VK_SUCCESS)
-- >        /* Do error handling for VK_ERROR_OUT_OF_MEMORY */
-- >
-- >     callback.flags = VK_DEBUG_REPORT_ERROR_BIT_EXT;
-- >     callback.pfnCallback = myDebugBreak;
-- >     callback.pUserData = NULL;
-- >     res = vkCreateDebugReportCallbackEXT(instance, &callback, &cb2);
-- >     if (res != VK_SUCCESS)
-- >        /* Do error handling for VK_ERROR_OUT_OF_MEMORY */
-- >
-- >     VkDebugReportCallbackCreateInfoEXT callback3 = {
-- >         .sType = VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT,
-- >         .pNext = NULL,
-- >         .flags = VK_DEBUG_REPORT_WARNING_BIT_EXT,
-- >         .pfnCallback = mystdOutLogger,
-- >         .pUserData = NULL
-- >     };
-- >     res = vkCreateDebugReportCallbackEXT(instance, &callback3, &cb3);
-- >     if (res != VK_SUCCESS)
-- >        /* Do error handling for VK_ERROR_OUT_OF_MEMORY */
-- >
-- >     ...
-- >
-- >     /* remove callbacks when cleaning up */
-- >     vkDestroyDebugReportCallbackEXT(instance, cb1);
-- >     vkDestroyDebugReportCallbackEXT(instance, cb2);
-- >     vkDestroyDebugReportCallbackEXT(instance, cb3);
--
-- Note
--
-- In the initial release of the @VK_EXT_debug_report@ extension, the token
-- 'STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT' was used. Starting in
-- version 2 of the extension branch,
-- 'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT'
-- is used instead for consistency with Vulkan naming rules. The older enum
-- is still available for backwards compatibility.
--
-- Note
--
-- In the initial release of the @VK_EXT_debug_report@ extension, the token
-- 'DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT' was used. Starting in
-- version 8 of the extension branch,
-- 'DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT' is used instead
-- for consistency with Vulkan naming rules. The older enum is still
-- available for backwards compatibility.
--
-- == Issues
--
-- 1) What is the hierarchy \/ seriousness of the message flags? E.g.
-- @ERROR@ > @WARN@ > @PERF_WARN@ …​
--
-- __RESOLVED__: There is no specific hierarchy. Each bit is independent
-- and should be checked via bitwise AND. For example:
--
-- >     if (localFlags & VK_DEBUG_REPORT_ERROR_BIT_EXT) {
-- >         process error message
-- >     }
-- >     if (localFlags & VK_DEBUG_REPORT_DEBUG_BIT_EXT) {
-- >         process debug message
-- >     }
--
-- The validation layers do use them in a hierarchical way (@ERROR@ >
-- @WARN@ > @PERF@, @WARN@ > @DEBUG@ > @INFO@) and they (at least at the
-- time of this writing) only set one bit at a time. But it is not a
-- requirement of this extension.
--
-- It is possible that a layer may intercept and change, or augment the
-- flags with extension values the application’s debug report handler may
-- not be familiar with, so it is important to treat each flag
-- independently.
--
-- 2) Should there be a VU requiring
-- 'DebugReportCallbackCreateInfoEXT'::@flags@ to be non-zero?
--
-- __RESOLVED__: It may not be very useful, but we do not need VU statement
-- requiring the 'DebugReportCallbackCreateInfoEXT'::@msgFlags@ at
-- create-time to be non-zero. One can imagine that apps may prefer it as
-- it allows them to set the mask as desired - including nothing - at
-- runtime without having to check.
--
-- 3) What is the difference between 'DEBUG_REPORT_DEBUG_BIT_EXT' and
-- 'DEBUG_REPORT_INFORMATION_BIT_EXT'?
--
-- __RESOLVED__: 'DEBUG_REPORT_DEBUG_BIT_EXT' specifies information that
-- could be useful debugging the Vulkan implementation itself.
--
-- 4) How do you compare handles returned by the debug_report callback to
-- the application’s handles?
--
-- __RESOLVED__: Due to the different nature of dispatchable and
-- nondispatchable handles there is no generic way (that we know of) that
-- works for common compilers with 32bit, 64bit, C and C++. We recommend
-- applications use the same cast that the validation layers use:
--
-- +
--
-- > reinterpret_cast<uint64_t &>(dispatchableHandle)
-- > (uint64_t)(nondispatchableHandle)
--
-- + This does require that the app treat dispatchable and nondispatchable
-- handles differently.
--
-- == Version History
--
-- -   Revision 1, 2015-05-20 (Courtney Goetzenleuchter)
--
--     -   Initial draft, based on LunarG KHR spec, other KHR specs
--
-- -   Revision 2, 2016-02-16 (Courtney Goetzenleuchter)
--
--     -   Update usage, documentation
--
-- -   Revision 3, 2016-06-14 (Courtney Goetzenleuchter)
--
--     -   Update VK_EXT_DEBUG_REPORT_SPEC_VERSION to indicate added
--         support for vkCreateInstance and vkDestroyInstance
--
-- -   Revision 4, 2016-12-08 (Mark Lobodzinski)
--
--     -   Added Display_KHR, DisplayModeKHR extension objects
--
--     -   Added ObjectTable_NVX, IndirectCommandsLayout_NVX extension
--         objects
--
--     -   Bumped spec revision
--
--     -   Retroactively added version history
--
-- -   Revision 5, 2017-01-31 (Baldur Karlsson)
--
--     -   Moved definition of 'DebugReportObjectTypeEXT' from debug marker
--         chapter
--
-- -   Revision 6, 2017-01-31 (Baldur Karlsson)
--
--     -   Added
--         VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_KHR_EXT
--
-- -   Revision 7, 2017-04-20 (Courtney Goeltzenleuchter)
--
--     -   Clarify wording and address questions from developers.
--
-- -   Revision 8, 2017-04-21 (Courtney Goeltzenleuchter)
--
--     -   Remove unused enum VkDebugReportErrorEXT
--
-- -   Revision 9, 2017-09-12 (Tobias Hector)
--
--     -   Added interactions with Vulkan 1.1
--
-- -   Revision 10, 2020-12-14 (Courtney Goetzenleuchter)
--
--     -   Add issue 4 discussing matching handles returned by the
--         extension, based on suggestion in public issue 368.
--
-- == See Also
--
-- 'PFN_vkDebugReportCallbackEXT', 'DebugReportCallbackCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.DebugReportCallbackEXT',
-- 'DebugReportFlagBitsEXT', 'DebugReportFlagsEXT',
-- 'DebugReportObjectTypeEXT', 'createDebugReportCallbackEXT',
-- 'debugReportMessageEXT', 'destroyDebugReportCallbackEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_debug_report Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_debug_report  ( DebugReportCallbackCreateInfoEXT
                                              , DebugReportFlagsEXT
                                              , DebugReportFlagBitsEXT
                                              , DebugReportObjectTypeEXT
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DebugReportCallbackCreateInfoEXT

instance ToCStruct DebugReportCallbackCreateInfoEXT
instance Show DebugReportCallbackCreateInfoEXT

instance FromCStruct DebugReportCallbackCreateInfoEXT


type DebugReportFlagsEXT = DebugReportFlagBitsEXT

data DebugReportFlagBitsEXT


data DebugReportObjectTypeEXT

