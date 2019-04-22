{-# language Strict #-}
{-# language CPP #-}
{-# language LambdaCase #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Exception
  ( VulkanException(..)
  , resultString
  ) where

import Control.Exception
  ( Exception(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_ERROR_DEVICE_LOST
  , pattern VK_ERROR_EXTENSION_NOT_PRESENT
  , pattern VK_ERROR_FEATURE_NOT_PRESENT
  , pattern VK_ERROR_FORMAT_NOT_SUPPORTED
  , pattern VK_ERROR_FRAGMENTED_POOL
  , pattern VK_ERROR_INCOMPATIBLE_DRIVER
  , pattern VK_ERROR_INITIALIZATION_FAILED
  , pattern VK_ERROR_LAYER_NOT_PRESENT
  , pattern VK_ERROR_MEMORY_MAP_FAILED
  , pattern VK_ERROR_OUT_OF_DEVICE_MEMORY
  , pattern VK_ERROR_OUT_OF_HOST_MEMORY
  , pattern VK_ERROR_TOO_MANY_OBJECTS
  , pattern VK_EVENT_RESET
  , pattern VK_EVENT_SET
  , pattern VK_INCOMPLETE
  , pattern VK_NOT_READY
  , pattern VK_SUCCESS
  , pattern VK_TIMEOUT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory
  ( pattern VK_ERROR_INVALID_EXTERNAL_HANDLE
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( pattern VK_ERROR_OUT_OF_POOL_MEMORY
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( pattern VK_ERROR_INVALID_DEVICE_ADDRESS_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( pattern VK_ERROR_VALIDATION_FAILED_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( pattern VK_ERROR_FRAGMENTATION_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( pattern VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( pattern VK_ERROR_NOT_PERMITTED_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( pattern VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( pattern VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( pattern VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
  , pattern VK_ERROR_SURFACE_LOST_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( pattern VK_ERROR_OUT_OF_DATE_KHR
  , pattern VK_SUBOPTIMAL_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_glsl_shader
  ( pattern VK_ERROR_INVALID_SHADER_NV
  )


-- | This exception is thrown from calls to marshalled vulkan commands
-- which return a negative VkResult.
newtype VulkanException = VulkanException { vulkanExceptionResult :: VkResult }
  deriving (Eq, Ord, Read, Show)

instance Exception VulkanException where
  displayException (VulkanException r) = show r ++ ": " ++ resultString r

-- | A human understandable message for each VkResult
resultString :: VkResult -> String
resultString = \case
  VK_SUCCESS -> "Command successfully completed"
  VK_NOT_READY -> "A fence or query has not yet completed"
  VK_TIMEOUT -> "A wait operation has not completed in the specified time"
  VK_EVENT_SET -> "An event is signaled"
  VK_EVENT_RESET -> "An event is unsignaled"
  VK_INCOMPLETE -> "A return array was too small for the result"
  VK_ERROR_OUT_OF_HOST_MEMORY -> "A host memory allocation has failed"
  VK_ERROR_OUT_OF_DEVICE_MEMORY -> "A device memory allocation has failed"
  VK_ERROR_INITIALIZATION_FAILED -> "Initialization of an object could not be completed for implementation-specific reasons"
  VK_ERROR_DEVICE_LOST -> "The logical or physical device has been lost"
  VK_ERROR_MEMORY_MAP_FAILED -> "Mapping of a memory object has failed"
  VK_ERROR_LAYER_NOT_PRESENT -> "A requested layer is not present or could not be loaded"
  VK_ERROR_EXTENSION_NOT_PRESENT -> "A requested extension is not supported"
  VK_ERROR_FEATURE_NOT_PRESENT -> "A requested feature is not supported"
  VK_ERROR_INCOMPATIBLE_DRIVER -> "The requested version of Vulkan is not supported by the driver or is otherwise incompatible for implementation-specific reasons"
  VK_ERROR_TOO_MANY_OBJECTS -> "Too many objects of the type have already been created"
  VK_ERROR_FORMAT_NOT_SUPPORTED -> "A requested format is not supported on this device"
  VK_ERROR_FRAGMENTED_POOL -> "A pool allocation has failed due to fragmentation of the pool's memory"
  VK_ERROR_OUT_OF_POOL_MEMORY -> show VK_ERROR_OUT_OF_POOL_MEMORY
  VK_ERROR_INVALID_EXTERNAL_HANDLE -> show VK_ERROR_INVALID_EXTERNAL_HANDLE
  VK_ERROR_SURFACE_LOST_KHR -> show VK_ERROR_SURFACE_LOST_KHR
  VK_ERROR_NATIVE_WINDOW_IN_USE_KHR -> show VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
  VK_SUBOPTIMAL_KHR -> show VK_SUBOPTIMAL_KHR
  VK_ERROR_OUT_OF_DATE_KHR -> show VK_ERROR_OUT_OF_DATE_KHR
  VK_ERROR_INCOMPATIBLE_DISPLAY_KHR -> show VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
  VK_ERROR_VALIDATION_FAILED_EXT -> show VK_ERROR_VALIDATION_FAILED_EXT
  VK_ERROR_INVALID_SHADER_NV -> show VK_ERROR_INVALID_SHADER_NV
  VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT -> show VK_ERROR_INVALID_DRM_FORMAT_MODIFIER_PLANE_LAYOUT_EXT
  VK_ERROR_FRAGMENTATION_EXT -> show VK_ERROR_FRAGMENTATION_EXT
  VK_ERROR_NOT_PERMITTED_EXT -> show VK_ERROR_NOT_PERMITTED_EXT
  VK_ERROR_INVALID_DEVICE_ADDRESS_EXT -> show VK_ERROR_INVALID_DEVICE_ADDRESS_EXT
  VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT -> show VK_ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT
  r -> show r
