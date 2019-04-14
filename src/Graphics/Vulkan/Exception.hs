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
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( pattern VK_ERROR_VALIDATION_FAILED_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( pattern VK_ERROR_FRAGMENTATION_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( pattern VK_ERROR_NOT_PERMITTED_EXT
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
  VK_SUCCESS -> show VK_SUCCESS
  VK_NOT_READY -> show VK_NOT_READY
  VK_TIMEOUT -> show VK_TIMEOUT
  VK_EVENT_SET -> show VK_EVENT_SET
  VK_EVENT_RESET -> show VK_EVENT_RESET
  VK_INCOMPLETE -> show VK_INCOMPLETE
  VK_ERROR_OUT_OF_HOST_MEMORY -> show VK_ERROR_OUT_OF_HOST_MEMORY
  VK_ERROR_OUT_OF_DEVICE_MEMORY -> show VK_ERROR_OUT_OF_DEVICE_MEMORY
  VK_ERROR_INITIALIZATION_FAILED -> show VK_ERROR_INITIALIZATION_FAILED
  VK_ERROR_DEVICE_LOST -> show VK_ERROR_DEVICE_LOST
  VK_ERROR_MEMORY_MAP_FAILED -> show VK_ERROR_MEMORY_MAP_FAILED
  VK_ERROR_LAYER_NOT_PRESENT -> show VK_ERROR_LAYER_NOT_PRESENT
  VK_ERROR_EXTENSION_NOT_PRESENT -> show VK_ERROR_EXTENSION_NOT_PRESENT
  VK_ERROR_FEATURE_NOT_PRESENT -> show VK_ERROR_FEATURE_NOT_PRESENT
  VK_ERROR_INCOMPATIBLE_DRIVER -> show VK_ERROR_INCOMPATIBLE_DRIVER
  VK_ERROR_TOO_MANY_OBJECTS -> show VK_ERROR_TOO_MANY_OBJECTS
  VK_ERROR_FORMAT_NOT_SUPPORTED -> show VK_ERROR_FORMAT_NOT_SUPPORTED
  VK_ERROR_FRAGMENTED_POOL -> show VK_ERROR_FRAGMENTED_POOL
  VK_ERROR_OUT_OF_POOL_MEMORY -> show VK_ERROR_OUT_OF_POOL_MEMORY
  VK_ERROR_INVALID_EXTERNAL_HANDLE -> show VK_ERROR_INVALID_EXTERNAL_HANDLE
  VK_ERROR_SURFACE_LOST_KHR -> show VK_ERROR_SURFACE_LOST_KHR
  VK_ERROR_NATIVE_WINDOW_IN_USE_KHR -> show VK_ERROR_NATIVE_WINDOW_IN_USE_KHR
  VK_SUBOPTIMAL_KHR -> show VK_SUBOPTIMAL_KHR
  VK_ERROR_OUT_OF_DATE_KHR -> show VK_ERROR_OUT_OF_DATE_KHR
  VK_ERROR_INCOMPATIBLE_DISPLAY_KHR -> show VK_ERROR_INCOMPATIBLE_DISPLAY_KHR
  VK_ERROR_VALIDATION_FAILED_EXT -> show VK_ERROR_VALIDATION_FAILED_EXT
  VK_ERROR_INVALID_SHADER_NV -> show VK_ERROR_INVALID_SHADER_NV
  VK_ERROR_FRAGMENTATION_EXT -> show VK_ERROR_FRAGMENTATION_EXT
  VK_ERROR_NOT_PERMITTED_EXT -> show VK_ERROR_NOT_PERMITTED_EXT
  r -> show r
