{-# language CPP #-}
-- No documentation found for Chapter "Result"
module OpenXR.Core10.Enums.Result  ( pattern SUCCEEDED
                                   , pattern UNQUALIFIED_SUCCESS
                                   , pattern FAILED
                                   , Result( SUCCESS
                                           , TIMEOUT_EXPIRED
                                           , SESSION_LOSS_PENDING
                                           , EVENT_UNAVAILABLE
                                           , SPACE_BOUNDS_UNAVAILABLE
                                           , SESSION_NOT_FOCUSED
                                           , FRAME_DISCARDED
                                           , ERROR_VALIDATION_FAILURE
                                           , ERROR_RUNTIME_FAILURE
                                           , ERROR_OUT_OF_MEMORY
                                           , ERROR_API_VERSION_UNSUPPORTED
                                           , ERROR_INITIALIZATION_FAILED
                                           , ERROR_FUNCTION_UNSUPPORTED
                                           , ERROR_FEATURE_UNSUPPORTED
                                           , ERROR_EXTENSION_NOT_PRESENT
                                           , ERROR_LIMIT_REACHED
                                           , ERROR_SIZE_INSUFFICIENT
                                           , ERROR_HANDLE_INVALID
                                           , ERROR_INSTANCE_LOST
                                           , ERROR_SESSION_RUNNING
                                           , ERROR_SESSION_NOT_RUNNING
                                           , ERROR_SESSION_LOST
                                           , ERROR_SYSTEM_INVALID
                                           , ERROR_PATH_INVALID
                                           , ERROR_PATH_COUNT_EXCEEDED
                                           , ERROR_PATH_FORMAT_INVALID
                                           , ERROR_PATH_UNSUPPORTED
                                           , ERROR_LAYER_INVALID
                                           , ERROR_LAYER_LIMIT_EXCEEDED
                                           , ERROR_SWAPCHAIN_RECT_INVALID
                                           , ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED
                                           , ERROR_ACTION_TYPE_MISMATCH
                                           , ERROR_SESSION_NOT_READY
                                           , ERROR_SESSION_NOT_STOPPING
                                           , ERROR_TIME_INVALID
                                           , ERROR_REFERENCE_SPACE_UNSUPPORTED
                                           , ERROR_FILE_ACCESS_ERROR
                                           , ERROR_FILE_CONTENTS_INVALID
                                           , ERROR_FORM_FACTOR_UNSUPPORTED
                                           , ERROR_FORM_FACTOR_UNAVAILABLE
                                           , ERROR_API_LAYER_NOT_PRESENT
                                           , ERROR_CALL_ORDER_INVALID
                                           , ERROR_GRAPHICS_DEVICE_INVALID
                                           , ERROR_POSE_INVALID
                                           , ERROR_INDEX_OUT_OF_RANGE
                                           , ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED
                                           , ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED
                                           , ERROR_NAME_DUPLICATED
                                           , ERROR_NAME_INVALID
                                           , ERROR_ACTIONSET_NOT_ATTACHED
                                           , ERROR_ACTIONSETS_ALREADY_ATTACHED
                                           , ERROR_LOCALIZED_NAME_DUPLICATED
                                           , ERROR_LOCALIZED_NAME_INVALID
                                           , ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING
                                           , ERROR_COLOR_SPACE_UNSUPPORTED_FB
                                           , ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB
                                           , ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT
                                           , ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT
                                           , ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT
                                           , ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR
                                           , ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR
                                           , ..
                                           )
                                   ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import OpenXR.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | XR_SUCCEEDED - Success convenience macro
--
-- = Description
--
-- This may be a qualified success such as 'FRAME_DISCARDED'.
--
-- = See Also
--
-- 'FAILED', 'UNQUALIFIED_SUCCESS'
pattern SUCCEEDED :: Result
pattern SUCCEEDED <- ((SUCCESS <=) -> True)

-- | XR_UNQUALIFIED_SUCCESS - Unqualified success convenience macro
--
-- = Description
--
-- used to compare an 'Result' to @0@ ('SUCCESS') exclusively.
--
-- = See Also
--
-- 'FAILED', 'SUCCEEDED'
pattern UNQUALIFIED_SUCCESS :: Result
pattern UNQUALIFIED_SUCCESS <- ((SUCCESS ==) -> True)

-- | XR_FAILED - Failure convenience macro
--
-- = Description
--
-- some way.
--
-- = See Also
--
-- 'SUCCEEDED', 'UNQUALIFIED_SUCCESS'
pattern FAILED :: Result
pattern FAILED <- ((SUCCESS >) -> True)

{-# complete SUCCEEDED, FAILED #-}


-- | XrResult - Result codes
--
-- = Description
--
-- All return codes in the API are reported via 'Result' return values.
--
-- Some common suffixes shared across many of the return codes are defined
-- below:
--
-- -   @_INVALID@: The specified handle, atom or value is formatted
--     incorrectly, or the specified handle was never created or has been
--     destroyed.
--
-- -   @_UNSUPPORTED@: The specified handle, atom, enumerant or value is
--     formatted correctly but cannot be used for the lifetime of this
--     function’s parent handle.
--
-- -   @_UNAVAILABLE@: The specified handle, atom, enumerant or value is
--     supported by this function’s parent handle but not at this moment.
--
-- = See Also
--
-- 'OpenXR.Core10.Instance.resultToString'
newtype Result = Result Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | Function successfully completed.
pattern SUCCESS                            = Result 0
-- | The specified timeout time occurred before the operation could complete.
pattern TIMEOUT_EXPIRED                    = Result 1
-- | The session will be lost soon.
pattern SESSION_LOSS_PENDING               = Result 3
-- | No event was available.
pattern EVENT_UNAVAILABLE                  = Result 4
-- | The space’s bounds are not known at the moment.
pattern SPACE_BOUNDS_UNAVAILABLE           = Result 7
-- | The session is not in the focused state.
pattern SESSION_NOT_FOCUSED                = Result 8
-- | A frame has been discarded from composition.
pattern FRAME_DISCARDED                    = Result 9
-- | The function usage was invalid in some way.
pattern ERROR_VALIDATION_FAILURE           = Result (-1)
-- | The runtime failed to handle the function in an unexpected way that is
-- not covered by another error result.
pattern ERROR_RUNTIME_FAILURE              = Result (-2)
-- | A memory allocation has failed.
pattern ERROR_OUT_OF_MEMORY                = Result (-3)
-- | The runtime does not support the requested API version.
pattern ERROR_API_VERSION_UNSUPPORTED      = Result (-4)
-- | Initialization of object could not be completed.
pattern ERROR_INITIALIZATION_FAILED        = Result (-6)
-- | The requested function was not found or is otherwise unsupported.
pattern ERROR_FUNCTION_UNSUPPORTED         = Result (-7)
-- | The requested feature is not supported.
pattern ERROR_FEATURE_UNSUPPORTED          = Result (-8)
-- | A requested extension is not supported.
pattern ERROR_EXTENSION_NOT_PRESENT        = Result (-9)
-- | The runtime supports no more of the requested resource.
pattern ERROR_LIMIT_REACHED                = Result (-10)
-- | The supplied size was smaller than required.
pattern ERROR_SIZE_INSUFFICIENT            = Result (-11)
-- | A supplied object handle was invalid.
pattern ERROR_HANDLE_INVALID               = Result (-12)
-- | The 'OpenXR.Core10.Handles.Instance' was lost or could not be found. It
-- will need to be destroyed and optionally recreated.
pattern ERROR_INSTANCE_LOST                = Result (-13)
-- | The session
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_running is already running>.
pattern ERROR_SESSION_RUNNING              = Result (-14)
-- | The session
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_not_running is not yet running>.
pattern ERROR_SESSION_NOT_RUNNING          = Result (-16)
-- | The 'OpenXR.Core10.Handles.Session' was lost. It will need to be
-- destroyed and optionally recreated.
pattern ERROR_SESSION_LOST                 = Result (-17)
-- | The provided
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
-- was invalid.
pattern ERROR_SYSTEM_INVALID               = Result (-18)
-- | The provided
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- was not valid.
pattern ERROR_PATH_INVALID                 = Result (-19)
-- | The maximum number of supported semantic paths has been reached.
pattern ERROR_PATH_COUNT_EXCEEDED          = Result (-20)
-- | The semantic path character format is invalid.
pattern ERROR_PATH_FORMAT_INVALID          = Result (-21)
-- | The semantic path is unsupported.
pattern ERROR_PATH_UNSUPPORTED             = Result (-22)
-- | The layer was NULL or otherwise invalid.
pattern ERROR_LAYER_INVALID                = Result (-23)
-- | The number of specified layers is greater than the supported number.
pattern ERROR_LAYER_LIMIT_EXCEEDED         = Result (-24)
-- | The image rect was negatively sized or otherwise invalid.
pattern ERROR_SWAPCHAIN_RECT_INVALID       = Result (-25)
-- | The image format is not supported by the runtime or platform.
pattern ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED = Result (-26)
-- | The API used to retrieve an action’s state does not match the action’s
-- type.
pattern ERROR_ACTION_TYPE_MISMATCH         = Result (-27)
-- | The session is not in the ready state.
pattern ERROR_SESSION_NOT_READY            = Result (-28)
-- | The session is not in the stopping state.
pattern ERROR_SESSION_NOT_STOPPING         = Result (-29)
-- | The provided
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
-- was zero, negative, or out of range.
pattern ERROR_TIME_INVALID                 = Result (-30)
-- | The specified reference space is not supported by the runtime or system.
pattern ERROR_REFERENCE_SPACE_UNSUPPORTED  = Result (-31)
-- | The file could not be accessed.
pattern ERROR_FILE_ACCESS_ERROR            = Result (-32)
-- | The file’s contents were invalid.
pattern ERROR_FILE_CONTENTS_INVALID        = Result (-33)
-- | The specified form factor is not supported by the current runtime or
-- platform.
pattern ERROR_FORM_FACTOR_UNSUPPORTED      = Result (-34)
-- | The specified form factor is supported, but the device is currently not
-- available, e.g. not plugged in or powered off.
pattern ERROR_FORM_FACTOR_UNAVAILABLE      = Result (-35)
-- | A requested API layer is not present or could not be loaded.
pattern ERROR_API_LAYER_NOT_PRESENT        = Result (-36)
-- | The call was made without having made a previously required call.
pattern ERROR_CALL_ORDER_INVALID           = Result (-37)
-- | The given graphics device is not in a valid state. The graphics device
-- could be lost or initialized without meeting graphics requirements.
pattern ERROR_GRAPHICS_DEVICE_INVALID      = Result (-38)
-- | The supplied pose was invalid with respect to the requirements.
pattern ERROR_POSE_INVALID                 = Result (-39)
-- | The supplied index was outside the range of valid indices.
pattern ERROR_INDEX_OUT_OF_RANGE           = Result (-40)
-- | The specified view configuration type is not supported by the runtime or
-- platform.
pattern ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED = Result (-41)
-- | The specified environment blend mode is not supported by the runtime or
-- platform.
pattern ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED = Result (-42)
-- | The name provided was a duplicate of an already-existing resource.
pattern ERROR_NAME_DUPLICATED              = Result (-44)
-- | The name provided was invalid.
pattern ERROR_NAME_INVALID                 = Result (-45)
-- | A referenced action set is not attached to the session.
pattern ERROR_ACTIONSET_NOT_ATTACHED       = Result (-46)
-- | The session already has attached action sets.
pattern ERROR_ACTIONSETS_ALREADY_ATTACHED  = Result (-47)
-- | The localized name provided was a duplicate of an already-existing
-- resource.
pattern ERROR_LOCALIZED_NAME_DUPLICATED    = Result (-48)
-- | The localized name provided was invalid.
pattern ERROR_LOCALIZED_NAME_INVALID       = Result (-49)
-- | The @xrGetGraphicsRequirements@* call was not made before calling
-- 'OpenXR.Core10.Device.createSession'.
pattern ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING = Result (-50)
-- | The color space is not supported by the runtime. (Added by the @@
-- extension)
pattern ERROR_COLOR_SPACE_UNSUPPORTED_FB   = Result (-1000108000)
-- | The display refresh rate is not supported by the platform. (Added by the
-- @@ extension)
pattern ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB = Result (-1000101000)
-- | The controller model key is invalid. (Added by the @@ extension)
pattern ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT = Result (-1000055000)
-- | The secondary view configuration was not enabled when creating the
-- session. (Added by the @@ extension)
pattern ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT = Result (-1000053000)
-- | Spatial anchor could not be created at that location. (Added by the @@
-- extension)
pattern ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT = Result (-1000039001)
-- | xrSetAndroidApplicationThreadKHR failed setting the thread
-- attributes\/priority. (Added by the @@ extension)
pattern ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR = Result (-1000003001)
-- | xrSetAndroidApplicationThreadKHR failed as thread id is invalid. (Added
-- by the @@ extension)
pattern ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR = Result (-1000003000)
{-# complete SUCCESS,
             TIMEOUT_EXPIRED,
             SESSION_LOSS_PENDING,
             EVENT_UNAVAILABLE,
             SPACE_BOUNDS_UNAVAILABLE,
             SESSION_NOT_FOCUSED,
             FRAME_DISCARDED,
             ERROR_VALIDATION_FAILURE,
             ERROR_RUNTIME_FAILURE,
             ERROR_OUT_OF_MEMORY,
             ERROR_API_VERSION_UNSUPPORTED,
             ERROR_INITIALIZATION_FAILED,
             ERROR_FUNCTION_UNSUPPORTED,
             ERROR_FEATURE_UNSUPPORTED,
             ERROR_EXTENSION_NOT_PRESENT,
             ERROR_LIMIT_REACHED,
             ERROR_SIZE_INSUFFICIENT,
             ERROR_HANDLE_INVALID,
             ERROR_INSTANCE_LOST,
             ERROR_SESSION_RUNNING,
             ERROR_SESSION_NOT_RUNNING,
             ERROR_SESSION_LOST,
             ERROR_SYSTEM_INVALID,
             ERROR_PATH_INVALID,
             ERROR_PATH_COUNT_EXCEEDED,
             ERROR_PATH_FORMAT_INVALID,
             ERROR_PATH_UNSUPPORTED,
             ERROR_LAYER_INVALID,
             ERROR_LAYER_LIMIT_EXCEEDED,
             ERROR_SWAPCHAIN_RECT_INVALID,
             ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED,
             ERROR_ACTION_TYPE_MISMATCH,
             ERROR_SESSION_NOT_READY,
             ERROR_SESSION_NOT_STOPPING,
             ERROR_TIME_INVALID,
             ERROR_REFERENCE_SPACE_UNSUPPORTED,
             ERROR_FILE_ACCESS_ERROR,
             ERROR_FILE_CONTENTS_INVALID,
             ERROR_FORM_FACTOR_UNSUPPORTED,
             ERROR_FORM_FACTOR_UNAVAILABLE,
             ERROR_API_LAYER_NOT_PRESENT,
             ERROR_CALL_ORDER_INVALID,
             ERROR_GRAPHICS_DEVICE_INVALID,
             ERROR_POSE_INVALID,
             ERROR_INDEX_OUT_OF_RANGE,
             ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED,
             ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED,
             ERROR_NAME_DUPLICATED,
             ERROR_NAME_INVALID,
             ERROR_ACTIONSET_NOT_ATTACHED,
             ERROR_ACTIONSETS_ALREADY_ATTACHED,
             ERROR_LOCALIZED_NAME_DUPLICATED,
             ERROR_LOCALIZED_NAME_INVALID,
             ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING,
             ERROR_COLOR_SPACE_UNSUPPORTED_FB,
             ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB,
             ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT,
             ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT,
             ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT,
             ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR,
             ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR :: Result #-}

conNameResult :: String
conNameResult = "Result"

enumPrefixResult :: String
enumPrefixResult = ""

showTableResult :: [(Result, String)]
showTableResult =
  [ (SUCCESS                           , "SUCCESS")
  , (TIMEOUT_EXPIRED                   , "TIMEOUT_EXPIRED")
  , (SESSION_LOSS_PENDING              , "SESSION_LOSS_PENDING")
  , (EVENT_UNAVAILABLE                 , "EVENT_UNAVAILABLE")
  , (SPACE_BOUNDS_UNAVAILABLE          , "SPACE_BOUNDS_UNAVAILABLE")
  , (SESSION_NOT_FOCUSED               , "SESSION_NOT_FOCUSED")
  , (FRAME_DISCARDED                   , "FRAME_DISCARDED")
  , (ERROR_VALIDATION_FAILURE          , "ERROR_VALIDATION_FAILURE")
  , (ERROR_RUNTIME_FAILURE             , "ERROR_RUNTIME_FAILURE")
  , (ERROR_OUT_OF_MEMORY               , "ERROR_OUT_OF_MEMORY")
  , (ERROR_API_VERSION_UNSUPPORTED     , "ERROR_API_VERSION_UNSUPPORTED")
  , (ERROR_INITIALIZATION_FAILED       , "ERROR_INITIALIZATION_FAILED")
  , (ERROR_FUNCTION_UNSUPPORTED        , "ERROR_FUNCTION_UNSUPPORTED")
  , (ERROR_FEATURE_UNSUPPORTED         , "ERROR_FEATURE_UNSUPPORTED")
  , (ERROR_EXTENSION_NOT_PRESENT       , "ERROR_EXTENSION_NOT_PRESENT")
  , (ERROR_LIMIT_REACHED               , "ERROR_LIMIT_REACHED")
  , (ERROR_SIZE_INSUFFICIENT           , "ERROR_SIZE_INSUFFICIENT")
  , (ERROR_HANDLE_INVALID              , "ERROR_HANDLE_INVALID")
  , (ERROR_INSTANCE_LOST               , "ERROR_INSTANCE_LOST")
  , (ERROR_SESSION_RUNNING             , "ERROR_SESSION_RUNNING")
  , (ERROR_SESSION_NOT_RUNNING         , "ERROR_SESSION_NOT_RUNNING")
  , (ERROR_SESSION_LOST                , "ERROR_SESSION_LOST")
  , (ERROR_SYSTEM_INVALID              , "ERROR_SYSTEM_INVALID")
  , (ERROR_PATH_INVALID                , "ERROR_PATH_INVALID")
  , (ERROR_PATH_COUNT_EXCEEDED         , "ERROR_PATH_COUNT_EXCEEDED")
  , (ERROR_PATH_FORMAT_INVALID         , "ERROR_PATH_FORMAT_INVALID")
  , (ERROR_PATH_UNSUPPORTED            , "ERROR_PATH_UNSUPPORTED")
  , (ERROR_LAYER_INVALID               , "ERROR_LAYER_INVALID")
  , (ERROR_LAYER_LIMIT_EXCEEDED        , "ERROR_LAYER_LIMIT_EXCEEDED")
  , (ERROR_SWAPCHAIN_RECT_INVALID      , "ERROR_SWAPCHAIN_RECT_INVALID")
  , (ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED, "ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED")
  , (ERROR_ACTION_TYPE_MISMATCH        , "ERROR_ACTION_TYPE_MISMATCH")
  , (ERROR_SESSION_NOT_READY           , "ERROR_SESSION_NOT_READY")
  , (ERROR_SESSION_NOT_STOPPING        , "ERROR_SESSION_NOT_STOPPING")
  , (ERROR_TIME_INVALID                , "ERROR_TIME_INVALID")
  , (ERROR_REFERENCE_SPACE_UNSUPPORTED , "ERROR_REFERENCE_SPACE_UNSUPPORTED")
  , (ERROR_FILE_ACCESS_ERROR           , "ERROR_FILE_ACCESS_ERROR")
  , (ERROR_FILE_CONTENTS_INVALID       , "ERROR_FILE_CONTENTS_INVALID")
  , (ERROR_FORM_FACTOR_UNSUPPORTED     , "ERROR_FORM_FACTOR_UNSUPPORTED")
  , (ERROR_FORM_FACTOR_UNAVAILABLE     , "ERROR_FORM_FACTOR_UNAVAILABLE")
  , (ERROR_API_LAYER_NOT_PRESENT       , "ERROR_API_LAYER_NOT_PRESENT")
  , (ERROR_CALL_ORDER_INVALID          , "ERROR_CALL_ORDER_INVALID")
  , (ERROR_GRAPHICS_DEVICE_INVALID     , "ERROR_GRAPHICS_DEVICE_INVALID")
  , (ERROR_POSE_INVALID                , "ERROR_POSE_INVALID")
  , (ERROR_INDEX_OUT_OF_RANGE          , "ERROR_INDEX_OUT_OF_RANGE")
  , (ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED, "ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED")
  , (ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED, "ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED")
  , (ERROR_NAME_DUPLICATED             , "ERROR_NAME_DUPLICATED")
  , (ERROR_NAME_INVALID                , "ERROR_NAME_INVALID")
  , (ERROR_ACTIONSET_NOT_ATTACHED      , "ERROR_ACTIONSET_NOT_ATTACHED")
  , (ERROR_ACTIONSETS_ALREADY_ATTACHED , "ERROR_ACTIONSETS_ALREADY_ATTACHED")
  , (ERROR_LOCALIZED_NAME_DUPLICATED   , "ERROR_LOCALIZED_NAME_DUPLICATED")
  , (ERROR_LOCALIZED_NAME_INVALID      , "ERROR_LOCALIZED_NAME_INVALID")
  , (ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING, "ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING")
  , (ERROR_COLOR_SPACE_UNSUPPORTED_FB  , "ERROR_COLOR_SPACE_UNSUPPORTED_FB")
  , (ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB, "ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB")
  , (ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT, "ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT")
  , ( ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT
    , "ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT"
    )
  , (ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT     , "ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT")
  , (ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR   , "ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR")
  , (ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR, "ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR")
  ]

instance Show Result where
  showsPrec = enumShowsPrec enumPrefixResult showTableResult conNameResult (\(Result x) -> x) (showsPrec 11)

instance Read Result where
  readPrec = enumReadPrec enumPrefixResult showTableResult conNameResult Result

