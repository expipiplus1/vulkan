{-# language CPP #-}
-- No documentation found for Chapter "Exception"
module OpenXR.Exception  (OpenXrException(..)) where

import GHC.Exception.Type (Exception(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
-- | This exception is thrown from calls to marshalled Vulkan commands
-- which return a negative VkResult.
newtype OpenXrException = OpenXrException { vulkanExceptionResult :: Result }
  deriving (Eq, Ord, Read, Show)

instance Exception OpenXrException where
  displayException (OpenXrException r) = show r ++ ": " ++ resultString r

-- | A human understandable message for each VkResult
resultString :: Result -> String
resultString = \case
  SUCCESS -> "Function successfully completed."
  TIMEOUT_EXPIRED -> "The specified timeout time occurred before the operation could complete."
  SESSION_LOSS_PENDING -> "The session will be lost soon."
  EVENT_UNAVAILABLE -> "No event was available."
  SPACE_BOUNDS_UNAVAILABLE -> "The space's bounds are not known at the moment."
  SESSION_NOT_FOCUSED -> "The session is not in the focused state."
  FRAME_DISCARDED -> "A frame has been discarded from composition."
  ERROR_VALIDATION_FAILURE -> "The function usage was invalid in some way."
  ERROR_RUNTIME_FAILURE -> "The runtime failed to handle the function in an unexpected way that is not covered by another error result. "
  ERROR_OUT_OF_MEMORY -> "A memory allocation has failed."
  ERROR_API_VERSION_UNSUPPORTED -> "The runtime does not support the requested API version."
  ERROR_INITIALIZATION_FAILED -> "Initialization of object could not be completed."
  ERROR_FUNCTION_UNSUPPORTED -> "The requested function was not found or is otherwise unsupported."
  ERROR_FEATURE_UNSUPPORTED -> "The requested feature is not supported."
  ERROR_EXTENSION_NOT_PRESENT -> "A requested extension is not supported."
  ERROR_LIMIT_REACHED -> "The runtime supports no more of the requested resource."
  ERROR_SIZE_INSUFFICIENT -> "The supplied size was smaller than required."
  ERROR_HANDLE_INVALID -> "A supplied object handle was invalid."
  ERROR_INSTANCE_LOST -> "The slink:XrInstance was lost or could not be found. It will need to be destroyed and optionally recreated."
  ERROR_SESSION_RUNNING -> "The session &lt;&lt;session_running, is already running&gt;&gt;."
  ERROR_SESSION_NOT_RUNNING -> "The session &lt;&lt;session_not_running, is not yet running&gt;&gt;."
  ERROR_SESSION_LOST -> "The slink:XrSession was lost. It will need to be destroyed and optionally recreated."
  ERROR_SYSTEM_INVALID -> "The provided basetype:XrSystemId was invalid."
  ERROR_PATH_INVALID -> "The provided basetype:XrPath was not valid."
  ERROR_PATH_COUNT_EXCEEDED -> "The maximum number of supported semantic paths has been reached."
  ERROR_PATH_FORMAT_INVALID -> "The semantic path character format is invalid."
  ERROR_PATH_UNSUPPORTED -> "The semantic path is unsupported."
  ERROR_LAYER_INVALID -> "The layer was NULL or otherwise invalid."
  ERROR_LAYER_LIMIT_EXCEEDED -> "The number of specified layers is greater than the supported number."
  ERROR_SWAPCHAIN_RECT_INVALID -> "The image rect was negatively sized or otherwise invalid."
  ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED -> "The image format is not supported by the runtime or platform."
  ERROR_ACTION_TYPE_MISMATCH -> "The API used to retrieve an action's state does not match the action's type."
  ERROR_SESSION_NOT_READY -> "The session is not in the ready state."
  ERROR_SESSION_NOT_STOPPING -> "The session is not in the stopping state."
  ERROR_TIME_INVALID -> "The provided XrTime was zero, negative, or out of range."
  ERROR_REFERENCE_SPACE_UNSUPPORTED -> "The specified reference space is not supported by the runtime or system."
  ERROR_FILE_ACCESS_ERROR -> "The file could not be accessed."
  ERROR_FILE_CONTENTS_INVALID -> "The file's contents were invalid."
  ERROR_FORM_FACTOR_UNSUPPORTED -> "The specified form factor is not supported by the current runtime or platform."
  ERROR_FORM_FACTOR_UNAVAILABLE -> "The specified form factor is supported, but the device is currently not available, e.g. not plugged in or powered off."
  ERROR_API_LAYER_NOT_PRESENT -> "A requested API layer is not present or could not be loaded."
  ERROR_CALL_ORDER_INVALID -> "The call was made without having made a previously required call."
  ERROR_GRAPHICS_DEVICE_INVALID -> "The given graphics device is not in a valid state. The graphics device could be lost or initialized without meeting graphics requirements."
  ERROR_POSE_INVALID -> "The supplied pose was invalid with respect to the requirements."
  ERROR_INDEX_OUT_OF_RANGE -> "The supplied index was outside the range of valid indices."
  ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED -> "The specified view configuration type is not supported by the runtime or platform."
  ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED -> "The specified environment blend mode is not supported by the runtime or platform."
  ERROR_NAME_DUPLICATED -> "The name provided was a duplicate of an already-existing resource."
  ERROR_NAME_INVALID -> "The name provided was invalid."
  ERROR_ACTIONSET_NOT_ATTACHED -> "A referenced action set is not attached to the session."
  ERROR_ACTIONSETS_ALREADY_ATTACHED -> "The session already has attached action sets."
  ERROR_LOCALIZED_NAME_DUPLICATED -> "The localized name provided was a duplicate of an already-existing resource."
  ERROR_LOCALIZED_NAME_INVALID -> "The localized name provided was invalid."
  ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING -> "The xrGetGraphicsRequirements* call was not made before calling xrCreateSession."
  ERROR_COLOR_SPACE_UNSUPPORTED_FB -> "The color space is not supported by the runtime."
  ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB -> "The display refresh rate is not supported by the platform."
  ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT -> "The controller model key is invalid."
  ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT -> "The secondary view configuration was not enabled when creating the session."
  ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT -> "Spatial anchor could not be created at that location."
  ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR -> "xrSetAndroidApplicationThreadKHR failed setting the thread attributes/priority."
  ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR -> "xrSetAndroidApplicationThreadKHR failed as thread id is invalid."
  r -> show r

