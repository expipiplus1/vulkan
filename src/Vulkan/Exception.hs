{-# language CPP #-}
-- No documentation found for Chapter "Exception"
module Vulkan.Exception  ( VulkanException(..)
                         , resultString
                         ) where

import GHC.Exception.Type (Exception(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
-- | This exception is thrown from calls to marshalled Vulkan commands
-- which return a negative 'Result'.
newtype VulkanException = VulkanException { vulkanExceptionResult :: Result }
  deriving (Eq, Ord, Read, Show)

instance Exception VulkanException where
  displayException (VulkanException r) = show r ++ ": " ++ resultString r

-- | A human understandable message for each 'Result'
resultString :: Result -> String
resultString = \case
  SUCCESS -> "Command successfully completed"
  NOT_READY -> "A fence or query has not yet completed"
  TIMEOUT -> "A wait operation has not completed in the specified time"
  EVENT_SET -> "An event is signaled"
  EVENT_RESET -> "An event is unsignaled"
  INCOMPLETE -> "A return array was too small for the result"
  ERROR_OUT_OF_HOST_MEMORY -> "A host memory allocation has failed"
  ERROR_OUT_OF_DEVICE_MEMORY -> "A device memory allocation has failed"
  ERROR_INITIALIZATION_FAILED -> "Initialization of an object could not be completed for implementation-specific reasons"
  ERROR_DEVICE_LOST -> "The logical or physical device has been lost"
  ERROR_MEMORY_MAP_FAILED -> "Mapping of a memory object has failed"
  ERROR_LAYER_NOT_PRESENT -> "A requested layer is not present or could not be loaded"
  ERROR_EXTENSION_NOT_PRESENT -> "A requested extension is not supported"
  ERROR_FEATURE_NOT_PRESENT -> "A requested feature is not supported"
  ERROR_INCOMPATIBLE_DRIVER -> "The requested version of Vulkan is not supported by the driver or is otherwise incompatible for implementation-specific reasons"
  ERROR_TOO_MANY_OBJECTS -> "Too many objects of the type have already been created"
  ERROR_FORMAT_NOT_SUPPORTED -> "A requested format is not supported on this device"
  ERROR_FRAGMENTED_POOL -> "A pool allocation has failed due to fragmentation of the pool's memory"
  ERROR_UNKNOWN -> "An unknown error has occurred; either the application has provided invalid input, or an implementation failure has occurred"
  PIPELINE_COMPILE_REQUIRED_EXT -> "A requested pipeline creation would have required compilation, but the application requested compilation to not be performed"
  OPERATION_NOT_DEFERRED_KHR -> "A deferred operation was requested and no operations were deferred"
  OPERATION_DEFERRED_KHR -> "A deferred operation was requested and at least some of the work was deferred"
  THREAD_DONE_KHR -> "A deferred operation is not complete but there is no work remaining to assign to additional threads"
  THREAD_IDLE_KHR -> "A deferred operation is not complete but there is currently no work for this thread to do at the time of this call"
  ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT -> "An operation on a swapchain created with VK_FULL_SCREEN_EXCLUSIVE_APPLICATION_CONTROLLED_EXT failed as it did not have exlusive full-screen access"
  ERROR_INVALID_SHADER_NV -> "One or more shaders failed to compile or link"
  ERROR_INCOMPATIBLE_DISPLAY_KHR -> "The display used by a swapchain does not use the same presentable image layout, or is incompatible in a way that prevents sharing an image"
  ERROR_OUT_OF_DATE_KHR -> "A surface has changed in such a way that it is no longer compatible with the swapchain, and further presentation requests using the swapchain will fail"
  SUBOPTIMAL_KHR -> "A swapchain no longer matches the surface properties exactly, but can still be used to present to the surface successfully"
  ERROR_NATIVE_WINDOW_IN_USE_KHR -> "The requested window is already in use by Vulkan or another API in a manner which prevents it from being used again"
  ERROR_SURFACE_LOST_KHR -> "A surface is no longer available"
  ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS -> "A buffer creation or memory allocation failed because the requested address is not available"
  ERROR_FRAGMENTATION -> "A descriptor pool creation has failed due to fragmentation"
  ERROR_INVALID_EXTERNAL_HANDLE -> "An external handle is not a valid handle of the specified type"
  ERROR_OUT_OF_POOL_MEMORY -> "A pool memory allocation has failed"
  r -> show r

