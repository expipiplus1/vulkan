{-# language CPP #-}
-- No documentation found for Chapter "Exception"
module Vulkan.Exception  (VulkanException(..)) where

import GHC.Exception.Type (Exception(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
-- | This exception is thrown from calls to marshalled Vulkan commands
-- which return a negative VkResult.
newtype VulkanException = VulkanException { vulkanExceptionResult :: Result }
  deriving (Eq, Ord, Read, Show)

instance Exception VulkanException where
  displayException (VulkanException r) = show r ++ ": " ++ resultString r

-- | A human understandable message for each VkResult
resultString :: Result -> String
resultString = \case
  SUCCESS -> "Command completed successfully"
  NOT_READY -> "A fence or query has not yet completed"
  TIMEOUT -> "A wait operation has not completed in the specified time"
  EVENT_SET -> "An event is signaled"
  EVENT_RESET -> "An event is unsignaled"
  INCOMPLETE -> "A return array was too small for the result"
  ERROR_OUT_OF_HOST_MEMORY -> "A host memory allocation has failed"
  ERROR_OUT_OF_DEVICE_MEMORY -> "A device memory allocation has failed"
  ERROR_INITIALIZATION_FAILED -> "Initialization of an object has failed"
  ERROR_DEVICE_LOST -> "The logical device has been lost. See &lt;&lt;devsandqueues-lost-device&gt;&gt;"
  ERROR_MEMORY_MAP_FAILED -> "Mapping of a memory object has failed"
  ERROR_LAYER_NOT_PRESENT -> "Layer specified does not exist"
  ERROR_EXTENSION_NOT_PRESENT -> "Extension specified does not exist"
  ERROR_FEATURE_NOT_PRESENT -> "Requested feature is not available on this device"
  ERROR_INCOMPATIBLE_DRIVER -> "Unable to find a Vulkan driver"
  ERROR_TOO_MANY_OBJECTS -> "Too many objects of the type have already been created"
  ERROR_FORMAT_NOT_SUPPORTED -> "Requested format is not supported on this device"
  ERROR_FRAGMENTED_POOL -> "A requested pool allocation has failed due to fragmentation of the pool's memory"
  ERROR_UNKNOWN -> "An unknown error has occurred, due to an implementation or application bug"
  r -> show r

