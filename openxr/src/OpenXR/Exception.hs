{-# language CPP #-}
-- No documentation found for Chapter "Exception"
module OpenXR.Exception  ( OpenXrException(..)
                         , resultString
                         ) where

import GHC.Exception.Type (Exception(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
-- | This exception is thrown from calls to marshalled OpenXR commands
-- which return a negative 'Result'.
newtype OpenXrException = OpenXrException { openxrExceptionResult :: Result }
  deriving (Eq, Ord, Read, Show)

instance Exception OpenXrException where
  displayException (OpenXrException r) = show r ++ ": " ++ resultString r

-- | A human understandable message for each 'Result'
resultString :: Result -> String
resultString = \case
  SUCCESS -> "Function successfully completed"
  TIMEOUT_EXPIRED -> "A specified timeout time occurred before the operation could complete"
  SESSION_LOSS_PENDING -> "A session will be lost soon"
  EVENT_UNAVAILABLE -> "No event was available"
  SPACE_BOUNDS_UNAVAILABLE -> "A space's bounds are not known at the moment"
  SESSION_NOT_FOCUSED -> "A session is not in the focused state"
  FRAME_DISCARDED -> "A frame has been discarded from composition"
  ERROR_VALIDATION_FAILURE -> "A function usage was invalid in some way"
  ERROR_RUNTIME_FAILURE -> "A runtime failed to handle the function in an unexpected way that is not covered by another error result"
  ERROR_OUT_OF_MEMORY -> "A memory allocation has failed"
  ERROR_API_VERSION_UNSUPPORTED -> "A runtime does not support the requested API version"
  ERROR_INITIALIZATION_FAILED -> "Initialization of object could not be completed"
  ERROR_FUNCTION_UNSUPPORTED -> "A requested function was not found or is otherwise unsupported"
  ERROR_FEATURE_UNSUPPORTED -> "A requested feature is not supported"
  ERROR_EXTENSION_NOT_PRESENT -> "A requested extension is not supported"
  ERROR_LIMIT_REACHED -> "A runtime supports no more of the requested resource"
  ERROR_SIZE_INSUFFICIENT -> "A supplied size was smaller than required"
  ERROR_HANDLE_INVALID -> "A supplied object handle was invalid"
  ERROR_INSTANCE_LOST -> "A XrInstance was lost or could not be found. It will need to be destroyed and optionally recreated"
  ERROR_SESSION_RUNNING -> "A session is already running"
  ERROR_SESSION_NOT_RUNNING -> "A session is not yet running"
  ERROR_SESSION_LOST -> "A XrSession was lost. It will need to be destroyed and optionally recreated"
  ERROR_SYSTEM_INVALID -> "A provided XrSystemId was invalid"
  ERROR_PATH_INVALID -> "A provided XrPath was not valid"
  ERROR_PATH_COUNT_EXCEEDED -> "A maximum number of supported semantic paths has been reached"
  ERROR_PATH_FORMAT_INVALID -> "A semantic path character format is invalid"
  ERROR_PATH_UNSUPPORTED -> "A semantic path is unsupported"
  ERROR_LAYER_INVALID -> "A layer was NULL or otherwise invalid"
  ERROR_LAYER_LIMIT_EXCEEDED -> "A number of specified layers is greater than the supported number"
  ERROR_SWAPCHAIN_RECT_INVALID -> "A image rect was negatively sized or otherwise invalid"
  ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED -> "A image format is not supported by the runtime or platform"
  ERROR_ACTION_TYPE_MISMATCH -> "A API used to retrieve an action's state does not match the action's type"
  ERROR_SESSION_NOT_READY -> "A session is not in the ready state"
  ERROR_SESSION_NOT_STOPPING -> "A session is not in the stopping state"
  ERROR_TIME_INVALID -> "A provided XrTime was zero, negative, or out of range"
  ERROR_REFERENCE_SPACE_UNSUPPORTED -> "A specified reference space is not supported by the runtime or system"
  ERROR_FILE_ACCESS_ERROR -> "A file could not be accessed"
  ERROR_FILE_CONTENTS_INVALID -> "A file's contents were invalid"
  ERROR_FORM_FACTOR_UNSUPPORTED -> "A specified form factor is not supported by the current runtime or platform"
  ERROR_FORM_FACTOR_UNAVAILABLE -> "A specified form factor is supported, but the device is currently not available, e.g. not plugged in or powered off"
  ERROR_API_LAYER_NOT_PRESENT -> "A requested API layer is not present or could not be loaded"
  ERROR_CALL_ORDER_INVALID -> "A call was made without having made a previously required call"
  ERROR_GRAPHICS_DEVICE_INVALID -> "A given graphics device is not in a valid state. The graphics device could be lost or initialized without meeting graphics requirements"
  ERROR_POSE_INVALID -> "A supplied pose was invalid with respect to the requirements"
  ERROR_INDEX_OUT_OF_RANGE -> "A supplied index was outside the range of valid indices"
  ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED -> "A specified view configuration type is not supported by the runtime or platform"
  ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED -> "A specified environment blend mode is not supported by the runtime or platform"
  ERROR_NAME_DUPLICATED -> "A name provided was a duplicate of an already-existing resource"
  ERROR_NAME_INVALID -> "A name provided was invalid"
  ERROR_ACTIONSET_NOT_ATTACHED -> "A referenced action set is not attached to the session"
  ERROR_ACTIONSETS_ALREADY_ATTACHED -> "A session already has attached action sets"
  ERROR_LOCALIZED_NAME_DUPLICATED -> "A localized name provided was a duplicate of an already-existing resource"
  ERROR_LOCALIZED_NAME_INVALID -> "A localized name provided was invalid"
  ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING -> "A xrGetGraphicsRequirements* call was not made before calling xrCreateSession"
  ERROR_COLOR_SPACE_UNSUPPORTED_FB -> "A color space is not supported by the runtime. (Added by the extension)"
  ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB -> "A display refresh rate is not supported by the platform. (Added by the extension)"
  ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT -> "A controller model key is invalid. (Added by the extension)"
  ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT -> "A secondary view configuration was not enabled when creating the session. (Added by the extension)"
  ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT -> "Spatial anchor could not be created at that location. (Added by the extension)"
  ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR -> "XrSetAndroidApplicationThreadKHR failed setting the thread attributes/priority. (Added by the extension)"
  ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR -> "XrSetAndroidApplicationThreadKHR failed as thread id is invalid. (Added by the extension)"
  r -> show r

