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
--     9
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Deprecation state__]
--
--     -   /Deprecated/ by @VK_EXT_debug_utils@ extension
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Debugging tools>
--
-- [__Contact__]
--
--     -   Courtney Goeltzenleuchter
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_debug_report:%20&body=@courtney-g%20 >
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
-- to 'Vulkan.Core10.DeviceInitialization.createInstance'. This callback is
-- only valid for the duration of the
-- 'Vulkan.Core10.DeviceInitialization.createInstance' and the
-- 'Vulkan.Core10.DeviceInitialization.destroyInstance' call. Use
-- 'createDebugReportCallbackEXT' to create persistent callback objects.
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
-- >             VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT,    // sType
-- >             NULL,                                                       // pNext
-- >             VK_DEBUG_REPORT_ERROR_BIT_EXT |                             // flags
-- >             VK_DEBUG_REPORT_WARNING_BIT_EXT,
-- >             myOutputDebugString,                                        // pfnCallback
-- >             NULL                                                        // pUserData
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
-- >             VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT,    // sType
-- >             NULL,                                                       // pNext
-- >             VK_DEBUG_REPORT_WARNING_BIT_EXT,                            // flags
-- >             mystdOutLogger,                                             // pfnCallback
-- >             NULL                                                        // pUserData
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
-- RESOLVED: Due to the different nature of dispatchable and
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
-- = See Also
--
-- 'PFN_vkDebugReportCallbackEXT', 'DebugReportCallbackCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.DebugReportCallbackEXT',
-- 'DebugReportFlagBitsEXT', 'DebugReportFlagsEXT',
-- 'DebugReportObjectTypeEXT', 'createDebugReportCallbackEXT',
-- 'debugReportMessageEXT', 'destroyDebugReportCallbackEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_debug_report Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_debug_report  ( createDebugReportCallbackEXT
                                              , withDebugReportCallbackEXT
                                              , destroyDebugReportCallbackEXT
                                              , debugReportMessageEXT
                                              , pattern STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT
                                              , pattern DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT
                                              , pattern DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT
                                              , DebugReportCallbackCreateInfoEXT(..)
                                              , DebugReportFlagsEXT
                                              , DebugReportFlagBitsEXT( DEBUG_REPORT_INFORMATION_BIT_EXT
                                                                      , DEBUG_REPORT_WARNING_BIT_EXT
                                                                      , DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT
                                                                      , DEBUG_REPORT_ERROR_BIT_EXT
                                                                      , DEBUG_REPORT_DEBUG_BIT_EXT
                                                                      , ..
                                                                      )
                                              , DebugReportObjectTypeEXT( DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT
                                                                        , DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT
                                                                        , ..
                                                                        )
                                              , PFN_vkDebugReportCallbackEXT
                                              , FN_vkDebugReportCallbackEXT
                                              , EXT_DEBUG_REPORT_SPEC_VERSION
                                              , pattern EXT_DEBUG_REPORT_SPEC_VERSION
                                              , EXT_DEBUG_REPORT_EXTENSION_NAME
                                              , pattern EXT_DEBUG_REPORT_EXTENSION_NAME
                                              , DebugReportCallbackEXT(..)
                                              ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Foreign.C.Types (CChar(..))
import Foreign.C.Types (CSize(..))
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
import Foreign.C.Types (CSize)
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
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Extensions.Handles (DebugReportCallbackEXT)
import Vulkan.Extensions.Handles (DebugReportCallbackEXT(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Handles (Instance)
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDebugReportCallbackEXT))
import Vulkan.Dynamic (InstanceCmds(pVkDebugReportMessageEXT))
import Vulkan.Dynamic (InstanceCmds(pVkDestroyDebugReportCallbackEXT))
import Vulkan.Core10.Handles (Instance_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DebugReportCallbackEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDebugReportCallbackEXT
  :: FunPtr (Ptr Instance_T -> Ptr DebugReportCallbackCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr DebugReportCallbackEXT -> IO Result) -> Ptr Instance_T -> Ptr DebugReportCallbackCreateInfoEXT -> Ptr AllocationCallbacks -> Ptr DebugReportCallbackEXT -> IO Result

-- | vkCreateDebugReportCallbackEXT - Create a debug report callback object
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDebugReportCallbackEXT-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkCreateDebugReportCallbackEXT-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'DebugReportCallbackCreateInfoEXT' structure
--
-- -   #VUID-vkCreateDebugReportCallbackEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateDebugReportCallbackEXT-pCallback-parameter#
--     @pCallback@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.DebugReportCallbackEXT' handle
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
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'DebugReportCallbackCreateInfoEXT',
-- 'Vulkan.Extensions.Handles.DebugReportCallbackEXT',
-- 'Vulkan.Core10.Handles.Instance'
createDebugReportCallbackEXT :: forall io
                              . (MonadIO io)
                             => -- | @instance@ is the instance the callback will be logged on.
                                Instance
                             -> -- | @pCreateInfo@ is a pointer to a 'DebugReportCallbackCreateInfoEXT'
                                -- structure defining the conditions under which this callback will be
                                -- called.
                                DebugReportCallbackCreateInfoEXT
                             -> -- | @pAllocator@ controls host memory allocation as described in the
                                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                -- chapter.
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (DebugReportCallbackEXT)
createDebugReportCallbackEXT instance' createInfo allocator = liftIO . evalContT $ do
  let vkCreateDebugReportCallbackEXTPtr = pVkCreateDebugReportCallbackEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkCreateDebugReportCallbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDebugReportCallbackEXT is null" Nothing Nothing
  let vkCreateDebugReportCallbackEXT' = mkVkCreateDebugReportCallbackEXT vkCreateDebugReportCallbackEXTPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPCallback <- ContT $ bracket (callocBytes @DebugReportCallbackEXT 8) free
  r <- lift $ traceAroundEvent "vkCreateDebugReportCallbackEXT" (vkCreateDebugReportCallbackEXT' (instanceHandle (instance')) pCreateInfo pAllocator (pPCallback))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCallback <- lift $ peek @DebugReportCallbackEXT pPCallback
  pure $ (pCallback)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDebugReportCallbackEXT' and 'destroyDebugReportCallbackEXT'
--
-- To ensure that 'destroyDebugReportCallbackEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDebugReportCallbackEXT :: forall io r . MonadIO io => Instance -> DebugReportCallbackCreateInfoEXT -> Maybe AllocationCallbacks -> (io DebugReportCallbackEXT -> (DebugReportCallbackEXT -> io ()) -> r) -> r
withDebugReportCallbackEXT instance' pCreateInfo pAllocator b =
  b (createDebugReportCallbackEXT instance' pCreateInfo pAllocator)
    (\(o0) -> destroyDebugReportCallbackEXT instance' o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDebugReportCallbackEXT
  :: FunPtr (Ptr Instance_T -> DebugReportCallbackEXT -> Ptr AllocationCallbacks -> IO ()) -> Ptr Instance_T -> DebugReportCallbackEXT -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyDebugReportCallbackEXT - Destroy a debug report callback object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyDebugReportCallbackEXT-instance-01242# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @callback@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyDebugReportCallbackEXT-instance-01243# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @callback@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyDebugReportCallbackEXT-instance-parameter# @instance@
--     /must/ be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkDestroyDebugReportCallbackEXT-callback-parameter# If
--     @callback@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @callback@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DebugReportCallbackEXT' handle
--
-- -   #VUID-vkDestroyDebugReportCallbackEXT-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyDebugReportCallbackEXT-callback-parent# If @callback@
--     is a valid handle, it /must/ have been created, allocated, or
--     retrieved from @instance@
--
-- == Host Synchronization
--
-- -   Host access to @callback@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.DebugReportCallbackEXT',
-- 'Vulkan.Core10.Handles.Instance'
destroyDebugReportCallbackEXT :: forall io
                               . (MonadIO io)
                              => -- | @instance@ is the instance where the callback was created.
                                 Instance
                              -> -- | @callback@ is the 'Vulkan.Extensions.Handles.DebugReportCallbackEXT'
                                 -- object to destroy. @callback@ is an externally synchronized object and
                                 -- /must/ not be used on more than one thread at a time. This means that
                                 -- 'destroyDebugReportCallbackEXT' /must/ not be called when a callback is
                                 -- active.
                                 DebugReportCallbackEXT
                              -> -- | @pAllocator@ controls host memory allocation as described in the
                                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                 -- chapter.
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io ()
destroyDebugReportCallbackEXT instance' callback allocator = liftIO . evalContT $ do
  let vkDestroyDebugReportCallbackEXTPtr = pVkDestroyDebugReportCallbackEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkDestroyDebugReportCallbackEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDebugReportCallbackEXT is null" Nothing Nothing
  let vkDestroyDebugReportCallbackEXT' = mkVkDestroyDebugReportCallbackEXT vkDestroyDebugReportCallbackEXTPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyDebugReportCallbackEXT" (vkDestroyDebugReportCallbackEXT' (instanceHandle (instance')) (callback) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugReportMessageEXT
  :: FunPtr (Ptr Instance_T -> DebugReportFlagsEXT -> DebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> IO ()) -> Ptr Instance_T -> DebugReportFlagsEXT -> DebugReportObjectTypeEXT -> Word64 -> CSize -> Int32 -> Ptr CChar -> Ptr CChar -> IO ()

-- | vkDebugReportMessageEXT - Inject a message into a debug stream
--
-- = Description
--
-- The call will propagate through the layers and generate callback(s) as
-- indicated by the message’s flags. The parameters are passed on to the
-- callback in addition to the @pUserData@ value that was defined at the
-- time the callback was registered.
--
-- == Valid Usage
--
-- -   #VUID-vkDebugReportMessageEXT-object-01241# @object@ /must/ be a
--     Vulkan object or 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkDebugReportMessageEXT-objectType-01498# If @objectType@ is
--     not 'DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT' and @object@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @object@ /must/ be a
--     Vulkan object of the corresponding type associated with @objectType@
--     as defined in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debug-report-object-types>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDebugReportMessageEXT-instance-parameter# @instance@ /must/
--     be a valid 'Vulkan.Core10.Handles.Instance' handle
--
-- -   #VUID-vkDebugReportMessageEXT-flags-parameter# @flags@ /must/ be a
--     valid combination of 'DebugReportFlagBitsEXT' values
--
-- -   #VUID-vkDebugReportMessageEXT-flags-requiredbitmask# @flags@ /must/
--     not be @0@
--
-- -   #VUID-vkDebugReportMessageEXT-objectType-parameter# @objectType@
--     /must/ be a valid 'DebugReportObjectTypeEXT' value
--
-- -   #VUID-vkDebugReportMessageEXT-pLayerPrefix-parameter# @pLayerPrefix@
--     /must/ be a null-terminated UTF-8 string
--
-- -   #VUID-vkDebugReportMessageEXT-pMessage-parameter# @pMessage@ /must/
--     be a null-terminated UTF-8 string
--
-- = See Also
--
-- 'DebugReportFlagsEXT', 'DebugReportObjectTypeEXT',
-- 'Vulkan.Core10.Handles.Instance'
debugReportMessageEXT :: forall io
                       . (MonadIO io)
                      => -- | @instance@ is the debug stream’s 'Vulkan.Core10.Handles.Instance'.
                         Instance
                      -> -- | @flags@ specifies the 'DebugReportFlagBitsEXT' classification of this
                         -- event\/message.
                         DebugReportFlagsEXT
                      -> -- | @objectType@ is a 'DebugReportObjectTypeEXT' specifying the type of
                         -- object being used or created at the time the event was triggered.
                         DebugReportObjectTypeEXT
                      -> -- | @object@ is the object where the issue was detected. @object@ /can/ be
                         -- 'Vulkan.Core10.APIConstants.NULL_HANDLE' if there is no object
                         -- associated with the event.
                         ("object" ::: Word64)
                      -> -- | @location@ is an application defined value.
                         ("location" ::: Word64)
                      -> -- | @messageCode@ is an application defined value.
                         ("messageCode" ::: Int32)
                      -> -- | @pLayerPrefix@ is the abbreviation of the component making this
                         -- event\/message.
                         ("layerPrefix" ::: ByteString)
                      -> -- | @pMessage@ is a null-terminated string detailing the trigger conditions.
                         ("message" ::: ByteString)
                      -> io ()
debugReportMessageEXT instance' flags objectType object location messageCode layerPrefix message = liftIO . evalContT $ do
  let vkDebugReportMessageEXTPtr = pVkDebugReportMessageEXT (instanceCmds (instance' :: Instance))
  lift $ unless (vkDebugReportMessageEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDebugReportMessageEXT is null" Nothing Nothing
  let vkDebugReportMessageEXT' = mkVkDebugReportMessageEXT vkDebugReportMessageEXTPtr
  pLayerPrefix <- ContT $ useAsCString (layerPrefix)
  pMessage <- ContT $ useAsCString (message)
  lift $ traceAroundEvent "vkDebugReportMessageEXT" (vkDebugReportMessageEXT' (instanceHandle (instance')) (flags) (objectType) (object) (CSize (location)) (messageCode) pLayerPrefix pMessage)
  pure $ ()


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DEBUG_REPORT_CREATE_INFO_EXT = STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT


-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_EXT = DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT


-- No documentation found for TopLevel "VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT = DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT


-- | VkDebugReportCallbackCreateInfoEXT - Structure specifying parameters of
-- a newly created debug report callback
--
-- = Description
--
-- For each 'Vulkan.Extensions.Handles.DebugReportCallbackEXT' that is
-- created the 'DebugReportCallbackCreateInfoEXT'::@flags@ determine when
-- that 'DebugReportCallbackCreateInfoEXT'::@pfnCallback@ is called. When
-- an event happens, the implementation will do a bitwise AND of the
-- event’s 'DebugReportFlagBitsEXT' flags to each
-- 'Vulkan.Extensions.Handles.DebugReportCallbackEXT' object’s flags. For
-- each non-zero result the corresponding callback will be called. The
-- callback will come directly from the component that detected the event,
-- unless some other layer intercepts the calls for its own purposes
-- (filter them in a different way, log to a system error log, etc.).
--
-- An application /may/ receive multiple callbacks if multiple
-- 'Vulkan.Extensions.Handles.DebugReportCallbackEXT' objects were created.
-- A callback will always be executed in the same thread as the originating
-- Vulkan call.
--
-- A callback may be called from multiple threads simultaneously (if the
-- application is making Vulkan calls from multiple threads).
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PFN_vkDebugReportCallbackEXT', 'DebugReportFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDebugReportCallbackEXT'
data DebugReportCallbackCreateInfoEXT = DebugReportCallbackCreateInfoEXT
  { -- | @flags@ is a bitmask of 'DebugReportFlagBitsEXT' specifying which
    -- event(s) will cause this callback to be called.
    --
    -- #VUID-VkDebugReportCallbackCreateInfoEXT-flags-parameter# @flags@ /must/
    -- be a valid combination of 'DebugReportFlagBitsEXT' values
    flags :: DebugReportFlagsEXT
  , -- | @pfnCallback@ is the application callback function to call.
    --
    -- #VUID-VkDebugReportCallbackCreateInfoEXT-pfnCallback-parameter#
    -- @pfnCallback@ /must/ be a valid 'PFN_vkDebugReportCallbackEXT' value
    pfnCallback :: PFN_vkDebugReportCallbackEXT
  , -- | @pUserData@ is user data to be passed to the callback.
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugReportCallbackCreateInfoEXT)
#endif
deriving instance Show DebugReportCallbackCreateInfoEXT

instance ToCStruct DebugReportCallbackCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugReportCallbackCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DebugReportFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkDebugReportCallbackEXT)) (pfnCallback)
    poke ((p `plusPtr` 32 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkDebugReportCallbackEXT)) (zero)
    f

instance FromCStruct DebugReportCallbackCreateInfoEXT where
  peekCStruct p = do
    flags <- peek @DebugReportFlagsEXT ((p `plusPtr` 16 :: Ptr DebugReportFlagsEXT))
    pfnCallback <- peek @PFN_vkDebugReportCallbackEXT ((p `plusPtr` 24 :: Ptr PFN_vkDebugReportCallbackEXT))
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 32 :: Ptr (Ptr ())))
    pure $ DebugReportCallbackCreateInfoEXT
             flags pfnCallback pUserData

instance Storable DebugReportCallbackCreateInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DebugReportCallbackCreateInfoEXT where
  zero = DebugReportCallbackCreateInfoEXT
           zero
           zero
           zero


type DebugReportFlagsEXT = DebugReportFlagBitsEXT

-- | VkDebugReportFlagBitsEXT - Bitmask specifying events which cause a debug
-- report callback
--
-- = See Also
--
-- 'DebugReportFlagsEXT'
newtype DebugReportFlagBitsEXT = DebugReportFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'DEBUG_REPORT_INFORMATION_BIT_EXT' specifies an informational message
-- such as resource details that may be handy when debugging an
-- application.
pattern DEBUG_REPORT_INFORMATION_BIT_EXT         = DebugReportFlagBitsEXT 0x00000001
-- | 'DEBUG_REPORT_WARNING_BIT_EXT' specifies use of Vulkan that /may/ expose
-- an app bug. Such cases may not be immediately harmful, such as a
-- fragment shader outputting to a location with no attachment. Other cases
-- /may/ point to behavior that is almost certainly bad when unintended
-- such as using an image whose memory has not been filled. In general if
-- you see a warning but you know that the behavior is intended\/desired,
-- then simply ignore the warning.
pattern DEBUG_REPORT_WARNING_BIT_EXT             = DebugReportFlagBitsEXT 0x00000002
-- | 'DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT' specifies a potentially
-- non-optimal use of Vulkan, e.g. using
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage' when setting
-- 'Vulkan.Core10.Pass.AttachmentDescription'::@loadOp@ to
-- 'Vulkan.Core10.Enums.AttachmentLoadOp.ATTACHMENT_LOAD_OP_CLEAR' would
-- have worked.
pattern DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT = DebugReportFlagBitsEXT 0x00000004
-- | 'DEBUG_REPORT_ERROR_BIT_EXT' specifies that the application has violated
-- a valid usage condition of the specification.
pattern DEBUG_REPORT_ERROR_BIT_EXT               = DebugReportFlagBitsEXT 0x00000008
-- | 'DEBUG_REPORT_DEBUG_BIT_EXT' specifies diagnostic information from the
-- implementation and layers.
pattern DEBUG_REPORT_DEBUG_BIT_EXT               = DebugReportFlagBitsEXT 0x00000010

conNameDebugReportFlagBitsEXT :: String
conNameDebugReportFlagBitsEXT = "DebugReportFlagBitsEXT"

enumPrefixDebugReportFlagBitsEXT :: String
enumPrefixDebugReportFlagBitsEXT = "DEBUG_REPORT_"

showTableDebugReportFlagBitsEXT :: [(DebugReportFlagBitsEXT, String)]
showTableDebugReportFlagBitsEXT =
  [ (DEBUG_REPORT_INFORMATION_BIT_EXT        , "INFORMATION_BIT_EXT")
  , (DEBUG_REPORT_WARNING_BIT_EXT            , "WARNING_BIT_EXT")
  , (DEBUG_REPORT_PERFORMANCE_WARNING_BIT_EXT, "PERFORMANCE_WARNING_BIT_EXT")
  , (DEBUG_REPORT_ERROR_BIT_EXT              , "ERROR_BIT_EXT")
  , (DEBUG_REPORT_DEBUG_BIT_EXT              , "DEBUG_BIT_EXT")
  ]

instance Show DebugReportFlagBitsEXT where
  showsPrec = enumShowsPrec enumPrefixDebugReportFlagBitsEXT
                            showTableDebugReportFlagBitsEXT
                            conNameDebugReportFlagBitsEXT
                            (\(DebugReportFlagBitsEXT x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read DebugReportFlagBitsEXT where
  readPrec = enumReadPrec enumPrefixDebugReportFlagBitsEXT
                          showTableDebugReportFlagBitsEXT
                          conNameDebugReportFlagBitsEXT
                          DebugReportFlagBitsEXT


-- | VkDebugReportObjectTypeEXT - Specify the type of an object handle
--
-- = Description
--
-- \'
--
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DebugReportObjectTypeEXT'                                | Vulkan Handle Type                                 |
-- +===========================================================+====================================================+
-- | 'DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT'                    | Unknown\/Undefined Handle                          |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT'                   | 'Vulkan.Core10.Handles.Instance'                   |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT'            | 'Vulkan.Core10.Handles.PhysicalDevice'             |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT'                     | 'Vulkan.Core10.Handles.Device'                     |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT'                      | 'Vulkan.Core10.Handles.Queue'                      |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT'                  | 'Vulkan.Core10.Handles.Semaphore'                  |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT'             | 'Vulkan.Core10.Handles.CommandBuffer'              |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT'                      | 'Vulkan.Core10.Handles.Fence'                      |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT'              | 'Vulkan.Core10.Handles.DeviceMemory'               |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT'                     | 'Vulkan.Core10.Handles.Buffer'                     |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT'                      | 'Vulkan.Core10.Handles.Image'                      |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT'                      | 'Vulkan.Core10.Handles.Event'                      |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT'                 | 'Vulkan.Core10.Handles.QueryPool'                  |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT'                | 'Vulkan.Core10.Handles.BufferView'                 |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT'                 | 'Vulkan.Core10.Handles.ImageView'                  |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT'              | 'Vulkan.Core10.Handles.ShaderModule'               |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT'             | 'Vulkan.Core10.Handles.PipelineCache'              |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT'            | 'Vulkan.Core10.Handles.PipelineLayout'             |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT'                | 'Vulkan.Core10.Handles.RenderPass'                 |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT'                   | 'Vulkan.Core10.Handles.Pipeline'                   |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT'      | 'Vulkan.Core10.Handles.DescriptorSetLayout'        |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT'                    | 'Vulkan.Core10.Handles.Sampler'                    |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT'            | 'Vulkan.Core10.Handles.DescriptorPool'             |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT'             | 'Vulkan.Core10.Handles.DescriptorSet'              |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT'                | 'Vulkan.Core10.Handles.Framebuffer'                |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT'               | 'Vulkan.Core10.Handles.CommandPool'                |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT'                | 'Vulkan.Extensions.Handles.SurfaceKHR'             |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT'              | 'Vulkan.Extensions.Handles.SwapchainKHR'           |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT'  | 'Vulkan.Extensions.Handles.DebugReportCallbackEXT' |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT'                | 'Vulkan.Extensions.Handles.DisplayKHR'             |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT'           | 'Vulkan.Extensions.Handles.DisplayModeKHR'         |
-- +-----------------------------------------------------------+----------------------------------------------------+
-- | 'DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT' | 'Vulkan.Core11.Handles.DescriptorUpdateTemplate'   |
-- +-----------------------------------------------------------+----------------------------------------------------+
--
-- 'DebugReportObjectTypeEXT' and Vulkan Handle Relationship
--
-- Note
--
-- The primary expected use of
-- 'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED_EXT' is for
-- validation layer testing. It is not expected that an application would
-- see this error code during normal use of the validation layers.
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_EXT_debug_marker.DebugMarkerObjectNameInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_marker.DebugMarkerObjectTagInfoEXT',
-- 'debugReportMessageEXT'
newtype DebugReportObjectTypeEXT = DebugReportObjectTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT                    = DebugReportObjectTypeEXT 0
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT                   = DebugReportObjectTypeEXT 1
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT            = DebugReportObjectTypeEXT 2
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT                     = DebugReportObjectTypeEXT 3
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT                      = DebugReportObjectTypeEXT 4
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT                  = DebugReportObjectTypeEXT 5
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT             = DebugReportObjectTypeEXT 6
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT                      = DebugReportObjectTypeEXT 7
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT              = DebugReportObjectTypeEXT 8
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT                     = DebugReportObjectTypeEXT 9
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT                      = DebugReportObjectTypeEXT 10
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT                      = DebugReportObjectTypeEXT 11
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT                 = DebugReportObjectTypeEXT 12
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT                = DebugReportObjectTypeEXT 13
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT                 = DebugReportObjectTypeEXT 14
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT              = DebugReportObjectTypeEXT 15
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT             = DebugReportObjectTypeEXT 16
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT            = DebugReportObjectTypeEXT 17
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT                = DebugReportObjectTypeEXT 18
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT                   = DebugReportObjectTypeEXT 19
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT      = DebugReportObjectTypeEXT 20
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT                    = DebugReportObjectTypeEXT 21
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT            = DebugReportObjectTypeEXT 22
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT             = DebugReportObjectTypeEXT 23
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT                = DebugReportObjectTypeEXT 24
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT               = DebugReportObjectTypeEXT 25
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT                = DebugReportObjectTypeEXT 26
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT              = DebugReportObjectTypeEXT 27
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT  = DebugReportObjectTypeEXT 28
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT                = DebugReportObjectTypeEXT 29
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT           = DebugReportObjectTypeEXT 30
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT       = DebugReportObjectTypeEXT 33
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT  = DebugReportObjectTypeEXT 1000165000
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT   = DebugReportObjectTypeEXT 1000156000
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT = DebugReportObjectTypeEXT 1000150000
-- No documentation found for Nested "VkDebugReportObjectTypeEXT" "VK_DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT"
pattern DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT = DebugReportObjectTypeEXT 1000085000
{-# complete DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT,
             DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT,
             DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT,
             DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT,
             DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT,
             DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT,
             DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT,
             DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT,
             DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT,
             DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT,
             DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT,
             DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT,
             DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT,
             DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT,
             DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT,
             DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT,
             DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT :: DebugReportObjectTypeEXT #-}

conNameDebugReportObjectTypeEXT :: String
conNameDebugReportObjectTypeEXT = "DebugReportObjectTypeEXT"

enumPrefixDebugReportObjectTypeEXT :: String
enumPrefixDebugReportObjectTypeEXT = "DEBUG_REPORT_OBJECT_TYPE_"

showTableDebugReportObjectTypeEXT :: [(DebugReportObjectTypeEXT, String)]
showTableDebugReportObjectTypeEXT =
  [ (DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT                   , "UNKNOWN_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_INSTANCE_EXT                  , "INSTANCE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_PHYSICAL_DEVICE_EXT           , "PHYSICAL_DEVICE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DEVICE_EXT                    , "DEVICE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_QUEUE_EXT                     , "QUEUE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SEMAPHORE_EXT                 , "SEMAPHORE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_COMMAND_BUFFER_EXT            , "COMMAND_BUFFER_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_FENCE_EXT                     , "FENCE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DEVICE_MEMORY_EXT             , "DEVICE_MEMORY_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_BUFFER_EXT                    , "BUFFER_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_IMAGE_EXT                     , "IMAGE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_EVENT_EXT                     , "EVENT_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_QUERY_POOL_EXT                , "QUERY_POOL_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_BUFFER_VIEW_EXT               , "BUFFER_VIEW_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_IMAGE_VIEW_EXT                , "IMAGE_VIEW_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SHADER_MODULE_EXT             , "SHADER_MODULE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_PIPELINE_CACHE_EXT            , "PIPELINE_CACHE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_PIPELINE_LAYOUT_EXT           , "PIPELINE_LAYOUT_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_RENDER_PASS_EXT               , "RENDER_PASS_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_PIPELINE_EXT                  , "PIPELINE_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT_EXT     , "DESCRIPTOR_SET_LAYOUT_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SAMPLER_EXT                   , "SAMPLER_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_POOL_EXT           , "DESCRIPTOR_POOL_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_SET_EXT            , "DESCRIPTOR_SET_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_FRAMEBUFFER_EXT               , "FRAMEBUFFER_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_COMMAND_POOL_EXT              , "COMMAND_POOL_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SURFACE_KHR_EXT               , "SURFACE_KHR_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SWAPCHAIN_KHR_EXT             , "SWAPCHAIN_KHR_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT_EXT , "DEBUG_REPORT_CALLBACK_EXT_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DISPLAY_KHR_EXT               , "DISPLAY_KHR_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DISPLAY_MODE_KHR_EXT          , "DISPLAY_MODE_KHR_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_VALIDATION_CACHE_EXT_EXT      , "VALIDATION_CACHE_EXT_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT , "ACCELERATION_STRUCTURE_NV_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION_EXT  , "SAMPLER_YCBCR_CONVERSION_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR_EXT, "ACCELERATION_STRUCTURE_KHR_EXT")
  , (DEBUG_REPORT_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_EXT, "DESCRIPTOR_UPDATE_TEMPLATE_EXT")
  ]

instance Show DebugReportObjectTypeEXT where
  showsPrec = enumShowsPrec enumPrefixDebugReportObjectTypeEXT
                            showTableDebugReportObjectTypeEXT
                            conNameDebugReportObjectTypeEXT
                            (\(DebugReportObjectTypeEXT x) -> x)
                            (showsPrec 11)

instance Read DebugReportObjectTypeEXT where
  readPrec = enumReadPrec enumPrefixDebugReportObjectTypeEXT
                          showTableDebugReportObjectTypeEXT
                          conNameDebugReportObjectTypeEXT
                          DebugReportObjectTypeEXT


type FN_vkDebugReportCallbackEXT = DebugReportFlagsEXT -> DebugReportObjectTypeEXT -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> ("pUserData" ::: Ptr ()) -> IO Bool32
-- | PFN_vkDebugReportCallbackEXT - Application-defined debug report callback
-- function
--
-- = Description
--
-- The callback /must/ not call 'destroyDebugReportCallbackEXT'.
--
-- The callback returns a 'Vulkan.Core10.FundamentalTypes.Bool32', which is
-- interpreted in a layer-specified manner. The application /should/ always
-- return 'Vulkan.Core10.FundamentalTypes.FALSE'. The
-- 'Vulkan.Core10.FundamentalTypes.TRUE' value is reserved for use in layer
-- development.
--
-- @object@ /must/ be a Vulkan object or
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE'. If @objectType@ is not
-- 'DEBUG_REPORT_OBJECT_TYPE_UNKNOWN_EXT' and @object@ is not
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', @object@ /must/ be a Vulkan
-- object of the corresponding type associated with @objectType@ as defined
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#debug-report-object-types>.
--
-- = See Also
--
-- 'DebugReportCallbackCreateInfoEXT'
type PFN_vkDebugReportCallbackEXT = FunPtr FN_vkDebugReportCallbackEXT


type EXT_DEBUG_REPORT_SPEC_VERSION = 9

-- No documentation found for TopLevel "VK_EXT_DEBUG_REPORT_SPEC_VERSION"
pattern EXT_DEBUG_REPORT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEBUG_REPORT_SPEC_VERSION = 9


type EXT_DEBUG_REPORT_EXTENSION_NAME = "VK_EXT_debug_report"

-- No documentation found for TopLevel "VK_EXT_DEBUG_REPORT_EXTENSION_NAME"
pattern EXT_DEBUG_REPORT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEBUG_REPORT_EXTENSION_NAME = "VK_EXT_debug_report"

