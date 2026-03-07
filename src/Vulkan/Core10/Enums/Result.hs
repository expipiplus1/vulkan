{-# language CPP #-}
-- No documentation found for Chapter "Result"
module Vulkan.Core10.Enums.Result  (Result( SUCCESS
                                          , NOT_READY
                                          , TIMEOUT
                                          , EVENT_SET
                                          , EVENT_RESET
                                          , INCOMPLETE
                                          , ERROR_OUT_OF_HOST_MEMORY
                                          , ERROR_OUT_OF_DEVICE_MEMORY
                                          , ERROR_INITIALIZATION_FAILED
                                          , ERROR_DEVICE_LOST
                                          , ERROR_MEMORY_MAP_FAILED
                                          , ERROR_LAYER_NOT_PRESENT
                                          , ERROR_EXTENSION_NOT_PRESENT
                                          , ERROR_FEATURE_NOT_PRESENT
                                          , ERROR_INCOMPATIBLE_DRIVER
                                          , ERROR_TOO_MANY_OBJECTS
                                          , ERROR_FORMAT_NOT_SUPPORTED
                                          , ERROR_FRAGMENTED_POOL
                                          , ERROR_UNKNOWN
                                          , ERROR_NOT_ENOUGH_SPACE_KHR
                                          , PIPELINE_BINARY_MISSING_KHR
                                          , INCOMPATIBLE_SHADER_BINARY_EXT
                                          , ERROR_COMPRESSION_EXHAUSTED_EXT
                                          , OPERATION_NOT_DEFERRED_KHR
                                          , OPERATION_DEFERRED_KHR
                                          , THREAD_DONE_KHR
                                          , THREAD_IDLE_KHR
                                          , ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
                                          , ERROR_PRESENT_TIMING_QUEUE_FULL_EXT
                                          , ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
                                          , ERROR_INVALID_SHADER_NV
                                          , ERROR_INCOMPATIBLE_DISPLAY_KHR
                                          , ERROR_OUT_OF_DATE_KHR
                                          , SUBOPTIMAL_KHR
                                          , ERROR_NATIVE_WINDOW_IN_USE_KHR
                                          , ERROR_SURFACE_LOST_KHR
                                          , ERROR_NO_PIPELINE_MATCH
                                          , ERROR_INVALID_PIPELINE_CACHE_DATA
                                          , ERROR_NOT_PERMITTED
                                          , PIPELINE_COMPILE_REQUIRED
                                          , ERROR_FRAGMENTATION
                                          , ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS
                                          , ERROR_INVALID_EXTERNAL_HANDLE
                                          , ERROR_OUT_OF_POOL_MEMORY
                                          , ERROR_VALIDATION_FAILED
                                          , ..
                                          )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkResult - Vulkan command return codes
--
-- = Description
--
-- -   'SUCCESS' Command successfully completed
--
-- -   'NOT_READY' A fence or query has not yet completed
--
-- -   'TIMEOUT' A wait operation has not completed in the specified time
--
-- -   'EVENT_SET' An event is signaled
--
-- -   'EVENT_RESET' An event is unsignaled
--
-- -   'INCOMPLETE' A return array was too small for the result
--
-- -   'SUBOPTIMAL_KHR' A swapchain no longer matches the surface
--     properties exactly, but /can/ still be used to present to the
--     surface successfully.
--
-- -   'THREAD_IDLE_KHR' A deferred operation is not complete but there is
--     currently no work for this thread to do at the time of this call.
--
-- -   'THREAD_DONE_KHR' A deferred operation is not complete but there is
--     no work remaining to assign to additional threads.
--
-- -   'OPERATION_DEFERRED_KHR' A deferred operation was requested and at
--     least some of the work was deferred.
--
-- -   'OPERATION_NOT_DEFERRED_KHR' A deferred operation was requested and
--     no operations were deferred.
--
-- -   'PIPELINE_COMPILE_REQUIRED' A requested pipeline creation would have
--     required compilation, but the application requested compilation to
--     not be performed.
--
-- -   'PIPELINE_BINARY_MISSING_KHR' The application attempted to create a
--     pipeline binary by querying an internal cache, but the internal
--     cache entry did not exist.
--
-- -   'INCOMPATIBLE_SHADER_BINARY_EXT' The provided binary shader code is
--     not compatible with this device.
--
--     In the initial version of the @VK_EXT_shader_object@ extension, this
--     return code was named
--     'Vulkan.Extensions.VK_EXT_shader_object.ERROR_INCOMPATIBLE_SHADER_BINARY_EXT'
--     and improperly described as an error code. The name has been
--     changed, but the old name is retained as an alias for compatibility
--     with old code.
--
-- -   'ERROR_OUT_OF_HOST_MEMORY' A host memory allocation has failed.
--
-- -   'ERROR_OUT_OF_DEVICE_MEMORY' A device memory allocation has failed.
--
-- -   'ERROR_INITIALIZATION_FAILED' Initialization of an object could not
--     be completed for implementation-specific reasons.
--
-- -   'ERROR_DEVICE_LOST' The logical or physical device has been lost.
--     See
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#devsandqueues-lost-device Lost Device>
--
-- -   'ERROR_MEMORY_MAP_FAILED' Mapping of a memory object has failed.
--
-- -   'ERROR_LAYER_NOT_PRESENT' A requested layer is not present or could
--     not be loaded.
--
-- -   'ERROR_EXTENSION_NOT_PRESENT' A requested extension is not
--     supported.
--
-- -   'ERROR_FEATURE_NOT_PRESENT' A requested feature is not supported.
--
-- -   'ERROR_INCOMPATIBLE_DRIVER' The requested version of Vulkan is not
--     supported by the driver or is otherwise incompatible for
--     implementation-specific reasons.
--
-- -   'ERROR_TOO_MANY_OBJECTS' Too many objects of the type have already
--     been created.
--
-- -   'ERROR_FORMAT_NOT_SUPPORTED' A requested format is not supported on
--     this device.
--
-- -   'ERROR_FRAGMENTED_POOL' A pool allocation has failed due to
--     fragmentation of the pool’s memory. This /must/ only be returned if
--     no attempt to allocate host or device memory was made to accommodate
--     the new allocation. This /should/ be returned in preference to
--     'ERROR_OUT_OF_POOL_MEMORY', but only if the implementation is
--     certain that the pool allocation failure was due to fragmentation.
--
-- -   'ERROR_SURFACE_LOST_KHR' A surface is no longer available.
--
-- -   'ERROR_NATIVE_WINDOW_IN_USE_KHR' The requested window is already in
--     use by Vulkan or another API in a manner which prevents it from
--     being used again.
--
-- -   'ERROR_OUT_OF_DATE_KHR' A surface has changed in such a way that it
--     is no longer compatible with the swapchain, and further presentation
--     requests using the swapchain will fail. Applications /must/ query
--     the new surface properties and recreate their swapchain if they wish
--     to continue presenting to the surface.
--
-- -   'ERROR_INCOMPATIBLE_DISPLAY_KHR' The display used by a swapchain
--     does not use the same presentable image layout, or is incompatible
--     in a way that prevents sharing an image.
--
-- -   'ERROR_INVALID_SHADER_NV' One or more shaders failed to compile or
--     link. More details are reported back to the application via
--     @VK_EXT_debug_report@ if enabled.
--
-- -   'ERROR_OUT_OF_POOL_MEMORY' A pool memory allocation has failed. This
--     /must/ only be returned if no attempt to allocate host or device
--     memory was made to accommodate the new allocation. If the failure
--     was definitely due to fragmentation of the pool,
--     'ERROR_FRAGMENTED_POOL' /should/ be returned instead.
--
-- -   'ERROR_INVALID_EXTERNAL_HANDLE' An external handle is not a valid
--     handle of the specified type.
--
-- -   'ERROR_FRAGMENTATION' A descriptor pool creation has failed due to
--     fragmentation.
--
-- -   'Vulkan.Extensions.VK_EXT_buffer_device_address.ERROR_INVALID_DEVICE_ADDRESS_EXT'
--     A buffer creation failed because the requested address is not
--     available.
--
-- -   'ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS' A buffer creation or memory
--     allocation failed because the requested address is not available. A
--     shader group handle assignment failed because the requested shader
--     group handle information is no longer valid.
--
-- -   'ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT' An operation on a
--     swapchain created with
--     'Vulkan.Extensions.VK_EXT_full_screen_exclusive.FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT'
--     failed as it did not have exclusive full-screen access. This /may/
--     occur due to implementation-dependent reasons, outside of the
--     application’s control.
--
-- -   'ERROR_VALIDATION_FAILED' A command failed because invalid usage was
--     detected by the implementation or a validation layer. This /may/
--     result in the command not being dispatched to the ICD.
--
-- -   'ERROR_COMPRESSION_EXHAUSTED_EXT' An image creation failed because
--     internal resources required for compression are exhausted. This
--     /must/ only be returned when fixed-rate compression is requested.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkResult VK_ERROR_IMAGE_USAGE_NOT_SUPPORTED_KHR>
--     The requested
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlags' are not
--     supported.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkResult VK_ERROR_VIDEO_PICTURE_LAYOUT_NOT_SUPPORTED_KHR>
--     The requested video picture layout is not supported.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkResult VK_ERROR_VIDEO_PROFILE_OPERATION_NOT_SUPPORTED_KHR>
--     A video profile operation specified via
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileInfoKHR VkVideoProfileInfoKHR>::@videoCodecOperation@
--     is not supported.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkResult VK_ERROR_VIDEO_PROFILE_FORMAT_NOT_SUPPORTED_KHR>
--     Format parameters in a requested
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileInfoKHR VkVideoProfileInfoKHR>
--     chain are not supported.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkResult VK_ERROR_VIDEO_PROFILE_CODEC_NOT_SUPPORTED_KHR>
--     Codec-specific parameters in a requested
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileInfoKHR VkVideoProfileInfoKHR>
--     chain are not supported.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkResult VK_ERROR_VIDEO_STD_VERSION_NOT_SUPPORTED_KHR>
--     The specified video Std header version is not supported.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkResult VK_ERROR_INVALID_VIDEO_STD_PARAMETERS_KHR>
--     The specified Video Std parameters do not adhere to the syntactic or
--     semantic requirements of the used video compression standard, or
--     values derived from parameters according to the rules defined by the
--     used video compression standard do not adhere to the capabilities of
--     the video compression standard or the implementation.
--
-- -   'ERROR_NOT_PERMITTED' The driver implementation has denied a request
--     to acquire a priority above the default priority
--     ('Vulkan.Extensions.VK_EXT_global_priority.QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT')
--     because the application does not have sufficient privileges.
--
-- -   'ERROR_NOT_ENOUGH_SPACE_KHR' The application did not provide enough
--     space to return all the required data.
--
-- -   'ERROR_UNKNOWN' An unknown error has occurred; either the
--     application has provided invalid input, or an implementation failure
--     has occurred.
--
-- If a command returns a runtime error, unless otherwise specified any
-- output parameters will have undefined contents, except that if the
-- output parameter is a structure with @sType@ and @pNext@ fields, those
-- fields will be unmodified. Any structures chained from @pNext@ will also
-- have undefined contents, except that @sType@ and @pNext@ will be
-- unmodified.
--
-- @VK_ERROR_OUT_OF_*_MEMORY@ errors do not modify any currently existing
-- Vulkan objects. Objects that have already been successfully created
-- /can/ still be used by the application.
--
-- As a general rule, @Free@, @Release@, and @Reset@ commands do not return
-- 'ERROR_OUT_OF_HOST_MEMORY', while any other command with a return code
-- /may/ return it. Any exceptions from this rule are described for those
-- commands.
--
-- 'ERROR_UNKNOWN' will be returned by an implementation when an unexpected
-- error occurs that cannot be attributed to valid behavior of the
-- application and implementation. Under these conditions, it /may/ be
-- returned from any command returning a 'Result'.
--
-- 'ERROR_UNKNOWN' is not expected to ever be returned if the application
-- behavior is valid, and if the implementation is bug-free. If
-- 'ERROR_UNKNOWN' is returned, the application should be checked against
-- the latest validation layers to verify correct behavior as much as
-- possible. If no issues are identified it could be an implementation
-- issue, and the implementor should be contacted for support.
--
-- Any command returning a 'Result' /may/ return 'ERROR_VALIDATION_FAILED'
-- if a violation of valid usage is detected.
--
-- Performance-critical commands generally do not have return codes. If a
-- runtime error occurs in such commands, the implementation will defer
-- reporting the error until a specified point. For commands that record
-- into command buffers (@vkCmd*@) runtime errors are reported by
-- 'Vulkan.Core10.CommandBuffer.endCommandBuffer'.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.BindMemoryStatus',
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'
newtype Result = Result Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkResult" "VK_SUCCESS"
pattern SUCCESS = Result 0

-- No documentation found for Nested "VkResult" "VK_NOT_READY"
pattern NOT_READY = Result 1

-- No documentation found for Nested "VkResult" "VK_TIMEOUT"
pattern TIMEOUT = Result 2

-- No documentation found for Nested "VkResult" "VK_EVENT_SET"
pattern EVENT_SET = Result 3

-- No documentation found for Nested "VkResult" "VK_EVENT_RESET"
pattern EVENT_RESET = Result 4

-- No documentation found for Nested "VkResult" "VK_INCOMPLETE"
pattern INCOMPLETE = Result 5

-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_HOST_MEMORY"
pattern ERROR_OUT_OF_HOST_MEMORY = Result (-1)

-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_DEVICE_MEMORY"
pattern ERROR_OUT_OF_DEVICE_MEMORY = Result (-2)

-- No documentation found for Nested "VkResult" "VK_ERROR_INITIALIZATION_FAILED"
pattern ERROR_INITIALIZATION_FAILED = Result (-3)

-- No documentation found for Nested "VkResult" "VK_ERROR_DEVICE_LOST"
pattern ERROR_DEVICE_LOST = Result (-4)

-- No documentation found for Nested "VkResult" "VK_ERROR_MEMORY_MAP_FAILED"
pattern ERROR_MEMORY_MAP_FAILED = Result (-5)

-- No documentation found for Nested "VkResult" "VK_ERROR_LAYER_NOT_PRESENT"
pattern ERROR_LAYER_NOT_PRESENT = Result (-6)

-- No documentation found for Nested "VkResult" "VK_ERROR_EXTENSION_NOT_PRESENT"
pattern ERROR_EXTENSION_NOT_PRESENT = Result (-7)

-- No documentation found for Nested "VkResult" "VK_ERROR_FEATURE_NOT_PRESENT"
pattern ERROR_FEATURE_NOT_PRESENT = Result (-8)

-- No documentation found for Nested "VkResult" "VK_ERROR_INCOMPATIBLE_DRIVER"
pattern ERROR_INCOMPATIBLE_DRIVER = Result (-9)

-- No documentation found for Nested "VkResult" "VK_ERROR_TOO_MANY_OBJECTS"
pattern ERROR_TOO_MANY_OBJECTS = Result (-10)

-- No documentation found for Nested "VkResult" "VK_ERROR_FORMAT_NOT_SUPPORTED"
pattern ERROR_FORMAT_NOT_SUPPORTED = Result (-11)

-- No documentation found for Nested "VkResult" "VK_ERROR_FRAGMENTED_POOL"
pattern ERROR_FRAGMENTED_POOL = Result (-12)

-- No documentation found for Nested "VkResult" "VK_ERROR_UNKNOWN"
pattern ERROR_UNKNOWN = Result (-13)

-- No documentation found for Nested "VkResult" "VK_ERROR_NOT_ENOUGH_SPACE_KHR"
pattern ERROR_NOT_ENOUGH_SPACE_KHR = Result (-1000483000)

-- No documentation found for Nested "VkResult" "VK_PIPELINE_BINARY_MISSING_KHR"
pattern PIPELINE_BINARY_MISSING_KHR = Result 1000483000

-- No documentation found for Nested "VkResult" "VK_INCOMPATIBLE_SHADER_BINARY_EXT"
pattern INCOMPATIBLE_SHADER_BINARY_EXT = Result 1000482000

-- No documentation found for Nested "VkResult" "VK_ERROR_COMPRESSION_EXHAUSTED_EXT"
pattern ERROR_COMPRESSION_EXHAUSTED_EXT = Result (-1000338000)

-- No documentation found for Nested "VkResult" "VK_OPERATION_NOT_DEFERRED_KHR"
pattern OPERATION_NOT_DEFERRED_KHR = Result 1000268003

-- No documentation found for Nested "VkResult" "VK_OPERATION_DEFERRED_KHR"
pattern OPERATION_DEFERRED_KHR = Result 1000268002

-- No documentation found for Nested "VkResult" "VK_THREAD_DONE_KHR"
pattern THREAD_DONE_KHR = Result 1000268001

-- No documentation found for Nested "VkResult" "VK_THREAD_IDLE_KHR"
pattern THREAD_IDLE_KHR = Result 1000268000

-- No documentation found for Nested "VkResult" "VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT"
pattern ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT = Result (-1000255000)

-- No documentation found for Nested "VkResult" "VK_ERROR_PRESENT_TIMING_QUEUE_FULL_EXT"
pattern ERROR_PRESENT_TIMING_QUEUE_FULL_EXT = Result (-1000208000)

-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT"
pattern ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT = Result (-1000158000)

-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_SHADER_NV"
pattern ERROR_INVALID_SHADER_NV = Result (-1000012000)

-- No documentation found for Nested "VkResult" "VK_ERROR_INCOMPATIBLE_DISPLAY_KHR"
pattern ERROR_INCOMPATIBLE_DISPLAY_KHR = Result (-1000003001)

-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_DATE_KHR"
pattern ERROR_OUT_OF_DATE_KHR = Result (-1000001004)

-- No documentation found for Nested "VkResult" "VK_SUBOPTIMAL_KHR"
pattern SUBOPTIMAL_KHR = Result 1000001003

-- No documentation found for Nested "VkResult" "VK_ERROR_NATIVE_WINDOW_IN_USE_KHR"
pattern ERROR_NATIVE_WINDOW_IN_USE_KHR = Result (-1000000001)

-- No documentation found for Nested "VkResult" "VK_ERROR_SURFACE_LOST_KHR"
pattern ERROR_SURFACE_LOST_KHR = Result (-1000000000)

-- No documentation found for Nested "VkResult" "VK_ERROR_NO_PIPELINE_MATCH"
pattern ERROR_NO_PIPELINE_MATCH = Result (-1000298001)

-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_PIPELINE_CACHE_DATA"
pattern ERROR_INVALID_PIPELINE_CACHE_DATA = Result (-1000298000)

-- No documentation found for Nested "VkResult" "VK_ERROR_NOT_PERMITTED"
pattern ERROR_NOT_PERMITTED = Result (-1000174001)

-- No documentation found for Nested "VkResult" "VK_PIPELINE_COMPILE_REQUIRED"
pattern PIPELINE_COMPILE_REQUIRED = Result 1000297000

-- No documentation found for Nested "VkResult" "VK_ERROR_FRAGMENTATION"
pattern ERROR_FRAGMENTATION = Result (-1000161000)

-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS"
pattern ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS = Result (-1000257000)

-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_EXTERNAL_HANDLE"
pattern ERROR_INVALID_EXTERNAL_HANDLE = Result (-1000072003)

-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_POOL_MEMORY"
pattern ERROR_OUT_OF_POOL_MEMORY = Result (-1000069000)

-- No documentation found for Nested "VkResult" "VK_ERROR_VALIDATION_FAILED"
pattern ERROR_VALIDATION_FAILED = Result (-1000011001)

{-# COMPLETE
  SUCCESS
  , NOT_READY
  , TIMEOUT
  , EVENT_SET
  , EVENT_RESET
  , INCOMPLETE
  , ERROR_OUT_OF_HOST_MEMORY
  , ERROR_OUT_OF_DEVICE_MEMORY
  , ERROR_INITIALIZATION_FAILED
  , ERROR_DEVICE_LOST
  , ERROR_MEMORY_MAP_FAILED
  , ERROR_LAYER_NOT_PRESENT
  , ERROR_EXTENSION_NOT_PRESENT
  , ERROR_FEATURE_NOT_PRESENT
  , ERROR_INCOMPATIBLE_DRIVER
  , ERROR_TOO_MANY_OBJECTS
  , ERROR_FORMAT_NOT_SUPPORTED
  , ERROR_FRAGMENTED_POOL
  , ERROR_UNKNOWN
  , ERROR_NOT_ENOUGH_SPACE_KHR
  , PIPELINE_BINARY_MISSING_KHR
  , INCOMPATIBLE_SHADER_BINARY_EXT
  , ERROR_COMPRESSION_EXHAUSTED_EXT
  , OPERATION_NOT_DEFERRED_KHR
  , OPERATION_DEFERRED_KHR
  , THREAD_DONE_KHR
  , THREAD_IDLE_KHR
  , ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , ERROR_PRESENT_TIMING_QUEUE_FULL_EXT
  , ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
  , ERROR_INVALID_SHADER_NV
  , ERROR_INCOMPATIBLE_DISPLAY_KHR
  , ERROR_OUT_OF_DATE_KHR
  , SUBOPTIMAL_KHR
  , ERROR_NATIVE_WINDOW_IN_USE_KHR
  , ERROR_SURFACE_LOST_KHR
  , ERROR_NO_PIPELINE_MATCH
  , ERROR_INVALID_PIPELINE_CACHE_DATA
  , ERROR_NOT_PERMITTED
  , PIPELINE_COMPILE_REQUIRED
  , ERROR_FRAGMENTATION
  , ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS
  , ERROR_INVALID_EXTERNAL_HANDLE
  , ERROR_OUT_OF_POOL_MEMORY
  , ERROR_VALIDATION_FAILED ::
    Result
  #-}

conNameResult :: String
conNameResult = "Result"

enumPrefixResult :: String
enumPrefixResult = ""

showTableResult :: [(Result, String)]
showTableResult =
  [ (SUCCESS, "SUCCESS")
  , (NOT_READY, "NOT_READY")
  , (TIMEOUT, "TIMEOUT")
  , (EVENT_SET, "EVENT_SET")
  , (EVENT_RESET, "EVENT_RESET")
  , (INCOMPLETE, "INCOMPLETE")
  , (ERROR_OUT_OF_HOST_MEMORY, "ERROR_OUT_OF_HOST_MEMORY")
  , (ERROR_OUT_OF_DEVICE_MEMORY, "ERROR_OUT_OF_DEVICE_MEMORY")
  , (ERROR_INITIALIZATION_FAILED, "ERROR_INITIALIZATION_FAILED")
  , (ERROR_DEVICE_LOST, "ERROR_DEVICE_LOST")
  , (ERROR_MEMORY_MAP_FAILED, "ERROR_MEMORY_MAP_FAILED")
  , (ERROR_LAYER_NOT_PRESENT, "ERROR_LAYER_NOT_PRESENT")
  , (ERROR_EXTENSION_NOT_PRESENT, "ERROR_EXTENSION_NOT_PRESENT")
  , (ERROR_FEATURE_NOT_PRESENT, "ERROR_FEATURE_NOT_PRESENT")
  , (ERROR_INCOMPATIBLE_DRIVER, "ERROR_INCOMPATIBLE_DRIVER")
  , (ERROR_TOO_MANY_OBJECTS, "ERROR_TOO_MANY_OBJECTS")
  , (ERROR_FORMAT_NOT_SUPPORTED, "ERROR_FORMAT_NOT_SUPPORTED")
  , (ERROR_FRAGMENTED_POOL, "ERROR_FRAGMENTED_POOL")
  , (ERROR_UNKNOWN, "ERROR_UNKNOWN")
  , (ERROR_NOT_ENOUGH_SPACE_KHR, "ERROR_NOT_ENOUGH_SPACE_KHR")
  , (PIPELINE_BINARY_MISSING_KHR, "PIPELINE_BINARY_MISSING_KHR")
  ,
    ( INCOMPATIBLE_SHADER_BINARY_EXT
    , "INCOMPATIBLE_SHADER_BINARY_EXT"
    )
  ,
    ( ERROR_COMPRESSION_EXHAUSTED_EXT
    , "ERROR_COMPRESSION_EXHAUSTED_EXT"
    )
  , (OPERATION_NOT_DEFERRED_KHR, "OPERATION_NOT_DEFERRED_KHR")
  , (OPERATION_DEFERRED_KHR, "OPERATION_DEFERRED_KHR")
  , (THREAD_DONE_KHR, "THREAD_DONE_KHR")
  , (THREAD_IDLE_KHR, "THREAD_IDLE_KHR")
  ,
    ( ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
    , "ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT"
    )
  ,
    ( ERROR_PRESENT_TIMING_QUEUE_FULL_EXT
    , "ERROR_PRESENT_TIMING_QUEUE_FULL_EXT"
    )
  ,
    ( ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
    , "ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT"
    )
  , (ERROR_INVALID_SHADER_NV, "ERROR_INVALID_SHADER_NV")
  ,
    ( ERROR_INCOMPATIBLE_DISPLAY_KHR
    , "ERROR_INCOMPATIBLE_DISPLAY_KHR"
    )
  , (ERROR_OUT_OF_DATE_KHR, "ERROR_OUT_OF_DATE_KHR")
  , (SUBOPTIMAL_KHR, "SUBOPTIMAL_KHR")
  ,
    ( ERROR_NATIVE_WINDOW_IN_USE_KHR
    , "ERROR_NATIVE_WINDOW_IN_USE_KHR"
    )
  , (ERROR_SURFACE_LOST_KHR, "ERROR_SURFACE_LOST_KHR")
  , (ERROR_NO_PIPELINE_MATCH, "ERROR_NO_PIPELINE_MATCH")
  ,
    ( ERROR_INVALID_PIPELINE_CACHE_DATA
    , "ERROR_INVALID_PIPELINE_CACHE_DATA"
    )
  , (ERROR_NOT_PERMITTED, "ERROR_NOT_PERMITTED")
  , (PIPELINE_COMPILE_REQUIRED, "PIPELINE_COMPILE_REQUIRED")
  , (ERROR_FRAGMENTATION, "ERROR_FRAGMENTATION")
  ,
    ( ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS
    , "ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS"
    )
  ,
    ( ERROR_INVALID_EXTERNAL_HANDLE
    , "ERROR_INVALID_EXTERNAL_HANDLE"
    )
  , (ERROR_OUT_OF_POOL_MEMORY, "ERROR_OUT_OF_POOL_MEMORY")
  , (ERROR_VALIDATION_FAILED, "ERROR_VALIDATION_FAILED")
  ]

instance Show Result where
  showsPrec =
    enumShowsPrec
      enumPrefixResult
      showTableResult
      conNameResult
      (\(Result x) -> x)
      (showsPrec 11)

instance Read Result where
  readPrec =
    enumReadPrec
      enumPrefixResult
      showTableResult
      conNameResult
      Result
