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
                                          , ERROR_INCOMPATIBLE_SHADER_BINARY_EXT
                                          , ERROR_COMPRESSION_EXHAUSTED_EXT
                                          , OPERATION_NOT_DEFERRED_KHR
                                          , OPERATION_DEFERRED_KHR
                                          , THREAD_DONE_KHR
                                          , THREAD_IDLE_KHR
                                          , ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
                                          , ERROR_NOT_PERMITTED_KHR
                                          , ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
                                          , ERROR_INVALID_SHADER_NV
                                          , ERROR_VALIDATION_FAILED_EXT
                                          , ERROR_INCOMPATIBLE_DISPLAY_KHR
                                          , ERROR_OUT_OF_DATE_KHR
                                          , SUBOPTIMAL_KHR
                                          , ERROR_NATIVE_WINDOW_IN_USE_KHR
                                          , ERROR_SURFACE_LOST_KHR
                                          , ERROR_NO_PIPELINE_MATCH
                                          , ERROR_INVALID_PIPELINE_CACHE_DATA
                                          , ERROR_VALIDATION_FAILED
                                          , PIPELINE_COMPILE_REQUIRED
                                          , ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS
                                          , ERROR_FRAGMENTATION
                                          , ERROR_INVALID_EXTERNAL_HANDLE
                                          , ERROR_OUT_OF_POOL_MEMORY
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
-- Note
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
-- Note
--
-- 'ERROR_UNKNOWN' is not expected to ever be returned if the application
-- behavior is valid, and if the implementation is bug-free. If
-- 'ERROR_UNKNOWN' is received, the application should be checked against
-- the latest validation layers to verify correct behavior as much as
-- possible. If no issues are identified it could be an implementation
-- issue, and the implementor should be contacted for support.
--
-- Any command returning a 'Result' /may/ return
-- 'ERROR_VALIDATION_FAILED_EXT' if a violation of valid usage is detected,
-- even though commands do not explicitly list this as a possible return
-- code.
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
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'
newtype Result = Result Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SUCCESS' Command successfully completed
pattern SUCCESS = Result 0

-- | 'NOT_READY' A fence or query has not yet completed
pattern NOT_READY = Result 1

-- | 'TIMEOUT' A wait operation has not completed in the specified time
pattern TIMEOUT = Result 2

-- | 'EVENT_SET' An event is signaled
pattern EVENT_SET = Result 3

-- | 'EVENT_RESET' An event is unsignaled
pattern EVENT_RESET = Result 4

-- | 'INCOMPLETE' A return array was too small for the result
pattern INCOMPLETE = Result 5

-- | 'ERROR_OUT_OF_HOST_MEMORY' A host memory allocation has failed.
pattern ERROR_OUT_OF_HOST_MEMORY = Result (-1)

-- | 'ERROR_OUT_OF_DEVICE_MEMORY' A device memory allocation has failed.
pattern ERROR_OUT_OF_DEVICE_MEMORY = Result (-2)

-- | 'ERROR_INITIALIZATION_FAILED' Initialization of an object could not be
-- completed for implementation-specific reasons.
pattern ERROR_INITIALIZATION_FAILED = Result (-3)

-- | 'ERROR_DEVICE_LOST' The logical or physical device has been lost. See
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>
pattern ERROR_DEVICE_LOST = Result (-4)

-- | 'ERROR_MEMORY_MAP_FAILED' Mapping of a memory object has failed.
pattern ERROR_MEMORY_MAP_FAILED = Result (-5)

-- | 'ERROR_LAYER_NOT_PRESENT' A requested layer is not present or could not
-- be loaded.
pattern ERROR_LAYER_NOT_PRESENT = Result (-6)

-- | 'ERROR_EXTENSION_NOT_PRESENT' A requested extension is not supported.
pattern ERROR_EXTENSION_NOT_PRESENT = Result (-7)

-- | 'ERROR_FEATURE_NOT_PRESENT' A requested feature is not supported.
pattern ERROR_FEATURE_NOT_PRESENT = Result (-8)

-- | 'ERROR_INCOMPATIBLE_DRIVER' The requested version of Vulkan is not
-- supported by the driver or is otherwise incompatible for
-- implementation-specific reasons.
pattern ERROR_INCOMPATIBLE_DRIVER = Result (-9)

-- | 'ERROR_TOO_MANY_OBJECTS' Too many objects of the type have already been
-- created.
pattern ERROR_TOO_MANY_OBJECTS = Result (-10)

-- | 'ERROR_FORMAT_NOT_SUPPORTED' A requested format is not supported on this
-- device.
pattern ERROR_FORMAT_NOT_SUPPORTED = Result (-11)

-- | 'ERROR_FRAGMENTED_POOL' A pool allocation has failed due to
-- fragmentation of the pool’s memory. This /must/ only be returned if no
-- attempt to allocate host or device memory was made to accommodate the
-- new allocation. This /should/ be returned in preference to
-- 'ERROR_OUT_OF_POOL_MEMORY', but only if the implementation is certain
-- that the pool allocation failure was due to fragmentation.
pattern ERROR_FRAGMENTED_POOL = Result (-12)

-- | 'ERROR_UNKNOWN' An unknown error has occurred; either the application
-- has provided invalid input, or an implementation failure has occurred.
pattern ERROR_UNKNOWN = Result (-13)

-- | 'ERROR_INCOMPATIBLE_SHADER_BINARY_EXT' The provided binary shader code
-- is not compatible with this device.
pattern ERROR_INCOMPATIBLE_SHADER_BINARY_EXT = Result 1000482000

-- | 'ERROR_COMPRESSION_EXHAUSTED_EXT' An image creation failed because
-- internal resources required for compression are exhausted. This /must/
-- only be returned when fixed-rate compression is requested.
pattern ERROR_COMPRESSION_EXHAUSTED_EXT = Result (-1000338000)

-- | 'OPERATION_NOT_DEFERRED_KHR' A deferred operation was requested and no
-- operations were deferred.
pattern OPERATION_NOT_DEFERRED_KHR = Result 1000268003

-- | 'OPERATION_DEFERRED_KHR' A deferred operation was requested and at least
-- some of the work was deferred.
pattern OPERATION_DEFERRED_KHR = Result 1000268002

-- | 'THREAD_DONE_KHR' A deferred operation is not complete but there is no
-- work remaining to assign to additional threads.
pattern THREAD_DONE_KHR = Result 1000268001

-- | 'THREAD_IDLE_KHR' A deferred operation is not complete but there is
-- currently no work for this thread to do at the time of this call.
pattern THREAD_IDLE_KHR = Result 1000268000

-- | 'ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT' An operation on a swapchain
-- created with
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT'
-- failed as it did not have exclusive full-screen access. This /may/ occur
-- due to implementation-dependent reasons, outside of the application’s
-- control.
pattern ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT = Result (-1000255000)

-- No documentation found for Nested "VkResult" "VK_ERROR_NOT_PERMITTED_KHR"
pattern ERROR_NOT_PERMITTED_KHR = Result (-1000174001)

-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT"
pattern ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT = Result (-1000158000)

-- | 'ERROR_INVALID_SHADER_NV' One or more shaders failed to compile or link.
-- More details are reported back to the application via
-- @VK_EXT_debug_report@ if enabled.
pattern ERROR_INVALID_SHADER_NV = Result (-1000012000)

-- | 'ERROR_VALIDATION_FAILED_EXT' A command failed because invalid usage was
-- detected by the implementation or a validation-layer.
pattern ERROR_VALIDATION_FAILED_EXT = Result (-1000011001)

-- | 'ERROR_INCOMPATIBLE_DISPLAY_KHR' The display used by a swapchain does
-- not use the same presentable image layout, or is incompatible in a way
-- that prevents sharing an image.
pattern ERROR_INCOMPATIBLE_DISPLAY_KHR = Result (-1000003001)

-- | 'ERROR_OUT_OF_DATE_KHR' A surface has changed in such a way that it is
-- no longer compatible with the swapchain, and further presentation
-- requests using the swapchain will fail. Applications /must/ query the
-- new surface properties and recreate their swapchain if they wish to
-- continue presenting to the surface.
pattern ERROR_OUT_OF_DATE_KHR = Result (-1000001004)

-- | 'SUBOPTIMAL_KHR' A swapchain no longer matches the surface properties
-- exactly, but /can/ still be used to present to the surface successfully.
pattern SUBOPTIMAL_KHR = Result 1000001003

-- | 'ERROR_NATIVE_WINDOW_IN_USE_KHR' The requested window is already in use
-- by Vulkan or another API in a manner which prevents it from being used
-- again.
pattern ERROR_NATIVE_WINDOW_IN_USE_KHR = Result (-1000000001)

-- | 'ERROR_SURFACE_LOST_KHR' A surface is no longer available.
pattern ERROR_SURFACE_LOST_KHR = Result (-1000000000)

-- No documentation found for Nested "VkResult" "VK_ERROR_NO_PIPELINE_MATCH"
pattern ERROR_NO_PIPELINE_MATCH = Result (-1000298001)

-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_PIPELINE_CACHE_DATA"
pattern ERROR_INVALID_PIPELINE_CACHE_DATA = Result (-1000298000)

-- No documentation found for Nested "VkResult" "VK_ERROR_VALIDATION_FAILED"
pattern ERROR_VALIDATION_FAILED = Result (-1000011001)

-- | 'PIPELINE_COMPILE_REQUIRED' A requested pipeline creation would have
-- required compilation, but the application requested compilation to not
-- be performed.
pattern PIPELINE_COMPILE_REQUIRED = Result 1000297000

-- | 'ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS' A buffer creation or memory
-- allocation failed because the requested address is not available. A
-- shader group handle assignment failed because the requested shader group
-- handle information is no longer valid.
pattern ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS = Result (-1000257000)

-- | 'ERROR_FRAGMENTATION' A descriptor pool creation has failed due to
-- fragmentation.
pattern ERROR_FRAGMENTATION = Result (-1000161000)

-- | 'ERROR_INVALID_EXTERNAL_HANDLE' An external handle is not a valid handle
-- of the specified type.
pattern ERROR_INVALID_EXTERNAL_HANDLE = Result (-1000072003)

-- | 'ERROR_OUT_OF_POOL_MEMORY' A pool memory allocation has failed. This
-- /must/ only be returned if no attempt to allocate host or device memory
-- was made to accommodate the new allocation. If the failure was
-- definitely due to fragmentation of the pool, 'ERROR_FRAGMENTED_POOL'
-- /should/ be returned instead.
pattern ERROR_OUT_OF_POOL_MEMORY = Result (-1000069000)

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
  , ERROR_INCOMPATIBLE_SHADER_BINARY_EXT
  , ERROR_COMPRESSION_EXHAUSTED_EXT
  , OPERATION_NOT_DEFERRED_KHR
  , OPERATION_DEFERRED_KHR
  , THREAD_DONE_KHR
  , THREAD_IDLE_KHR
  , ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  , ERROR_NOT_PERMITTED_KHR
  , ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
  , ERROR_INVALID_SHADER_NV
  , ERROR_VALIDATION_FAILED_EXT
  , ERROR_INCOMPATIBLE_DISPLAY_KHR
  , ERROR_OUT_OF_DATE_KHR
  , SUBOPTIMAL_KHR
  , ERROR_NATIVE_WINDOW_IN_USE_KHR
  , ERROR_SURFACE_LOST_KHR
  , ERROR_NO_PIPELINE_MATCH
  , ERROR_INVALID_PIPELINE_CACHE_DATA
  , ERROR_VALIDATION_FAILED
  , PIPELINE_COMPILE_REQUIRED
  , ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS
  , ERROR_FRAGMENTATION
  , ERROR_INVALID_EXTERNAL_HANDLE
  , ERROR_OUT_OF_POOL_MEMORY ::
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
  ,
    ( ERROR_INCOMPATIBLE_SHADER_BINARY_EXT
    , "ERROR_INCOMPATIBLE_SHADER_BINARY_EXT"
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
  , (ERROR_NOT_PERMITTED_KHR, "ERROR_NOT_PERMITTED_KHR")
  ,
    ( ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
    , "ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT"
    )
  , (ERROR_INVALID_SHADER_NV, "ERROR_INVALID_SHADER_NV")
  , (ERROR_VALIDATION_FAILED_EXT, "ERROR_VALIDATION_FAILED_EXT")
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
  , (ERROR_VALIDATION_FAILED, "ERROR_VALIDATION_FAILED")
  , (PIPELINE_COMPILE_REQUIRED, "PIPELINE_COMPILE_REQUIRED")
  ,
    ( ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS
    , "ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS"
    )
  , (ERROR_FRAGMENTATION, "ERROR_FRAGMENTATION")
  ,
    ( ERROR_INVALID_EXTERNAL_HANDLE
    , "ERROR_INVALID_EXTERNAL_HANDLE"
    )
  , (ERROR_OUT_OF_POOL_MEMORY, "ERROR_OUT_OF_POOL_MEMORY")
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
