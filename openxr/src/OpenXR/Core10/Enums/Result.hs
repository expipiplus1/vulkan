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
-- = See Also
--
-- 'OpenXR.Core10.Instance.resultToString'
newtype Result = Result Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "XrResult" "XR_SUCCESS"
pattern SUCCESS = Result 0

-- No documentation found for Nested "XrResult" "XR_TIMEOUT_EXPIRED"
pattern TIMEOUT_EXPIRED = Result 1

-- No documentation found for Nested "XrResult" "XR_SESSION_LOSS_PENDING"
pattern SESSION_LOSS_PENDING = Result 3

-- No documentation found for Nested "XrResult" "XR_EVENT_UNAVAILABLE"
pattern EVENT_UNAVAILABLE = Result 4

-- No documentation found for Nested "XrResult" "XR_SPACE_BOUNDS_UNAVAILABLE"
pattern SPACE_BOUNDS_UNAVAILABLE = Result 7

-- No documentation found for Nested "XrResult" "XR_SESSION_NOT_FOCUSED"
pattern SESSION_NOT_FOCUSED = Result 8

-- No documentation found for Nested "XrResult" "XR_FRAME_DISCARDED"
pattern FRAME_DISCARDED = Result 9

-- No documentation found for Nested "XrResult" "XR_ERROR_VALIDATION_FAILURE"
pattern ERROR_VALIDATION_FAILURE = Result (-1)

-- No documentation found for Nested "XrResult" "XR_ERROR_RUNTIME_FAILURE"
pattern ERROR_RUNTIME_FAILURE = Result (-2)

-- No documentation found for Nested "XrResult" "XR_ERROR_OUT_OF_MEMORY"
pattern ERROR_OUT_OF_MEMORY = Result (-3)

-- No documentation found for Nested "XrResult" "XR_ERROR_API_VERSION_UNSUPPORTED"
pattern ERROR_API_VERSION_UNSUPPORTED = Result (-4)

-- No documentation found for Nested "XrResult" "XR_ERROR_INITIALIZATION_FAILED"
pattern ERROR_INITIALIZATION_FAILED = Result (-6)

-- No documentation found for Nested "XrResult" "XR_ERROR_FUNCTION_UNSUPPORTED"
pattern ERROR_FUNCTION_UNSUPPORTED = Result (-7)

-- No documentation found for Nested "XrResult" "XR_ERROR_FEATURE_UNSUPPORTED"
pattern ERROR_FEATURE_UNSUPPORTED = Result (-8)

-- No documentation found for Nested "XrResult" "XR_ERROR_EXTENSION_NOT_PRESENT"
pattern ERROR_EXTENSION_NOT_PRESENT = Result (-9)

-- No documentation found for Nested "XrResult" "XR_ERROR_LIMIT_REACHED"
pattern ERROR_LIMIT_REACHED = Result (-10)

-- No documentation found for Nested "XrResult" "XR_ERROR_SIZE_INSUFFICIENT"
pattern ERROR_SIZE_INSUFFICIENT = Result (-11)

-- No documentation found for Nested "XrResult" "XR_ERROR_HANDLE_INVALID"
pattern ERROR_HANDLE_INVALID = Result (-12)

-- No documentation found for Nested "XrResult" "XR_ERROR_INSTANCE_LOST"
pattern ERROR_INSTANCE_LOST = Result (-13)

-- No documentation found for Nested "XrResult" "XR_ERROR_SESSION_RUNNING"
pattern ERROR_SESSION_RUNNING = Result (-14)

-- No documentation found for Nested "XrResult" "XR_ERROR_SESSION_NOT_RUNNING"
pattern ERROR_SESSION_NOT_RUNNING = Result (-16)

-- No documentation found for Nested "XrResult" "XR_ERROR_SESSION_LOST"
pattern ERROR_SESSION_LOST = Result (-17)

-- No documentation found for Nested "XrResult" "XR_ERROR_SYSTEM_INVALID"
pattern ERROR_SYSTEM_INVALID = Result (-18)

-- No documentation found for Nested "XrResult" "XR_ERROR_PATH_INVALID"
pattern ERROR_PATH_INVALID = Result (-19)

-- No documentation found for Nested "XrResult" "XR_ERROR_PATH_COUNT_EXCEEDED"
pattern ERROR_PATH_COUNT_EXCEEDED = Result (-20)

-- No documentation found for Nested "XrResult" "XR_ERROR_PATH_FORMAT_INVALID"
pattern ERROR_PATH_FORMAT_INVALID = Result (-21)

-- No documentation found for Nested "XrResult" "XR_ERROR_PATH_UNSUPPORTED"
pattern ERROR_PATH_UNSUPPORTED = Result (-22)

-- No documentation found for Nested "XrResult" "XR_ERROR_LAYER_INVALID"
pattern ERROR_LAYER_INVALID = Result (-23)

-- No documentation found for Nested "XrResult" "XR_ERROR_LAYER_LIMIT_EXCEEDED"
pattern ERROR_LAYER_LIMIT_EXCEEDED = Result (-24)

-- No documentation found for Nested "XrResult" "XR_ERROR_SWAPCHAIN_RECT_INVALID"
pattern ERROR_SWAPCHAIN_RECT_INVALID = Result (-25)

-- No documentation found for Nested "XrResult" "XR_ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED"
pattern ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED = Result (-26)

-- No documentation found for Nested "XrResult" "XR_ERROR_ACTION_TYPE_MISMATCH"
pattern ERROR_ACTION_TYPE_MISMATCH = Result (-27)

-- No documentation found for Nested "XrResult" "XR_ERROR_SESSION_NOT_READY"
pattern ERROR_SESSION_NOT_READY = Result (-28)

-- No documentation found for Nested "XrResult" "XR_ERROR_SESSION_NOT_STOPPING"
pattern ERROR_SESSION_NOT_STOPPING = Result (-29)

-- No documentation found for Nested "XrResult" "XR_ERROR_TIME_INVALID"
pattern ERROR_TIME_INVALID = Result (-30)

-- No documentation found for Nested "XrResult" "XR_ERROR_REFERENCE_SPACE_UNSUPPORTED"
pattern ERROR_REFERENCE_SPACE_UNSUPPORTED = Result (-31)

-- No documentation found for Nested "XrResult" "XR_ERROR_FILE_ACCESS_ERROR"
pattern ERROR_FILE_ACCESS_ERROR = Result (-32)

-- No documentation found for Nested "XrResult" "XR_ERROR_FILE_CONTENTS_INVALID"
pattern ERROR_FILE_CONTENTS_INVALID = Result (-33)

-- No documentation found for Nested "XrResult" "XR_ERROR_FORM_FACTOR_UNSUPPORTED"
pattern ERROR_FORM_FACTOR_UNSUPPORTED = Result (-34)

-- No documentation found for Nested "XrResult" "XR_ERROR_FORM_FACTOR_UNAVAILABLE"
pattern ERROR_FORM_FACTOR_UNAVAILABLE = Result (-35)

-- No documentation found for Nested "XrResult" "XR_ERROR_API_LAYER_NOT_PRESENT"
pattern ERROR_API_LAYER_NOT_PRESENT = Result (-36)

-- No documentation found for Nested "XrResult" "XR_ERROR_CALL_ORDER_INVALID"
pattern ERROR_CALL_ORDER_INVALID = Result (-37)

-- No documentation found for Nested "XrResult" "XR_ERROR_GRAPHICS_DEVICE_INVALID"
pattern ERROR_GRAPHICS_DEVICE_INVALID = Result (-38)

-- No documentation found for Nested "XrResult" "XR_ERROR_POSE_INVALID"
pattern ERROR_POSE_INVALID = Result (-39)

-- No documentation found for Nested "XrResult" "XR_ERROR_INDEX_OUT_OF_RANGE"
pattern ERROR_INDEX_OUT_OF_RANGE = Result (-40)

-- No documentation found for Nested "XrResult" "XR_ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED"
pattern ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED = Result (-41)

-- No documentation found for Nested "XrResult" "XR_ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED"
pattern ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED = Result (-42)

-- No documentation found for Nested "XrResult" "XR_ERROR_NAME_DUPLICATED"
pattern ERROR_NAME_DUPLICATED = Result (-44)

-- No documentation found for Nested "XrResult" "XR_ERROR_NAME_INVALID"
pattern ERROR_NAME_INVALID = Result (-45)

-- No documentation found for Nested "XrResult" "XR_ERROR_ACTIONSET_NOT_ATTACHED"
pattern ERROR_ACTIONSET_NOT_ATTACHED = Result (-46)

-- No documentation found for Nested "XrResult" "XR_ERROR_ACTIONSETS_ALREADY_ATTACHED"
pattern ERROR_ACTIONSETS_ALREADY_ATTACHED = Result (-47)

-- No documentation found for Nested "XrResult" "XR_ERROR_LOCALIZED_NAME_DUPLICATED"
pattern ERROR_LOCALIZED_NAME_DUPLICATED = Result (-48)

-- No documentation found for Nested "XrResult" "XR_ERROR_LOCALIZED_NAME_INVALID"
pattern ERROR_LOCALIZED_NAME_INVALID = Result (-49)

-- No documentation found for Nested "XrResult" "XR_ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING"
pattern ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING = Result (-50)

-- No documentation found for Nested "XrResult" "XR_ERROR_COLOR_SPACE_UNSUPPORTED_FB"
pattern ERROR_COLOR_SPACE_UNSUPPORTED_FB = Result (-1000108000)

-- No documentation found for Nested "XrResult" "XR_ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB"
pattern ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB = Result (-1000101000)

-- No documentation found for Nested "XrResult" "XR_ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT"
pattern ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT = Result (-1000055000)

-- No documentation found for Nested "XrResult" "XR_ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT"
pattern ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT = Result (-1000053000)

-- No documentation found for Nested "XrResult" "XR_ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT"
pattern ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT = Result (-1000039001)

-- No documentation found for Nested "XrResult" "XR_ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR"
pattern ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR = Result (-1000003001)

-- No documentation found for Nested "XrResult" "XR_ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR"
pattern ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR = Result (-1000003000)

{-# COMPLETE
  SUCCESS
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
  , ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR ::
    Result
  #-}

conNameResult :: String
conNameResult = "Result"

enumPrefixResult :: String
enumPrefixResult = ""

showTableResult :: [(Result, String)]
showTableResult =
  [ (SUCCESS, "SUCCESS")
  , (TIMEOUT_EXPIRED, "TIMEOUT_EXPIRED")
  , (SESSION_LOSS_PENDING, "SESSION_LOSS_PENDING")
  , (EVENT_UNAVAILABLE, "EVENT_UNAVAILABLE")
  , (SPACE_BOUNDS_UNAVAILABLE, "SPACE_BOUNDS_UNAVAILABLE")
  , (SESSION_NOT_FOCUSED, "SESSION_NOT_FOCUSED")
  , (FRAME_DISCARDED, "FRAME_DISCARDED")
  , (ERROR_VALIDATION_FAILURE, "ERROR_VALIDATION_FAILURE")
  , (ERROR_RUNTIME_FAILURE, "ERROR_RUNTIME_FAILURE")
  , (ERROR_OUT_OF_MEMORY, "ERROR_OUT_OF_MEMORY")
  ,
    ( ERROR_API_VERSION_UNSUPPORTED
    , "ERROR_API_VERSION_UNSUPPORTED"
    )
  , (ERROR_INITIALIZATION_FAILED, "ERROR_INITIALIZATION_FAILED")
  , (ERROR_FUNCTION_UNSUPPORTED, "ERROR_FUNCTION_UNSUPPORTED")
  , (ERROR_FEATURE_UNSUPPORTED, "ERROR_FEATURE_UNSUPPORTED")
  , (ERROR_EXTENSION_NOT_PRESENT, "ERROR_EXTENSION_NOT_PRESENT")
  , (ERROR_LIMIT_REACHED, "ERROR_LIMIT_REACHED")
  , (ERROR_SIZE_INSUFFICIENT, "ERROR_SIZE_INSUFFICIENT")
  , (ERROR_HANDLE_INVALID, "ERROR_HANDLE_INVALID")
  , (ERROR_INSTANCE_LOST, "ERROR_INSTANCE_LOST")
  , (ERROR_SESSION_RUNNING, "ERROR_SESSION_RUNNING")
  , (ERROR_SESSION_NOT_RUNNING, "ERROR_SESSION_NOT_RUNNING")
  , (ERROR_SESSION_LOST, "ERROR_SESSION_LOST")
  , (ERROR_SYSTEM_INVALID, "ERROR_SYSTEM_INVALID")
  , (ERROR_PATH_INVALID, "ERROR_PATH_INVALID")
  , (ERROR_PATH_COUNT_EXCEEDED, "ERROR_PATH_COUNT_EXCEEDED")
  , (ERROR_PATH_FORMAT_INVALID, "ERROR_PATH_FORMAT_INVALID")
  , (ERROR_PATH_UNSUPPORTED, "ERROR_PATH_UNSUPPORTED")
  , (ERROR_LAYER_INVALID, "ERROR_LAYER_INVALID")
  , (ERROR_LAYER_LIMIT_EXCEEDED, "ERROR_LAYER_LIMIT_EXCEEDED")
  ,
    ( ERROR_SWAPCHAIN_RECT_INVALID
    , "ERROR_SWAPCHAIN_RECT_INVALID"
    )
  ,
    ( ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED
    , "ERROR_SWAPCHAIN_FORMAT_UNSUPPORTED"
    )
  , (ERROR_ACTION_TYPE_MISMATCH, "ERROR_ACTION_TYPE_MISMATCH")
  , (ERROR_SESSION_NOT_READY, "ERROR_SESSION_NOT_READY")
  , (ERROR_SESSION_NOT_STOPPING, "ERROR_SESSION_NOT_STOPPING")
  , (ERROR_TIME_INVALID, "ERROR_TIME_INVALID")
  ,
    ( ERROR_REFERENCE_SPACE_UNSUPPORTED
    , "ERROR_REFERENCE_SPACE_UNSUPPORTED"
    )
  , (ERROR_FILE_ACCESS_ERROR, "ERROR_FILE_ACCESS_ERROR")
  , (ERROR_FILE_CONTENTS_INVALID, "ERROR_FILE_CONTENTS_INVALID")
  ,
    ( ERROR_FORM_FACTOR_UNSUPPORTED
    , "ERROR_FORM_FACTOR_UNSUPPORTED"
    )
  ,
    ( ERROR_FORM_FACTOR_UNAVAILABLE
    , "ERROR_FORM_FACTOR_UNAVAILABLE"
    )
  , (ERROR_API_LAYER_NOT_PRESENT, "ERROR_API_LAYER_NOT_PRESENT")
  , (ERROR_CALL_ORDER_INVALID, "ERROR_CALL_ORDER_INVALID")
  ,
    ( ERROR_GRAPHICS_DEVICE_INVALID
    , "ERROR_GRAPHICS_DEVICE_INVALID"
    )
  , (ERROR_POSE_INVALID, "ERROR_POSE_INVALID")
  , (ERROR_INDEX_OUT_OF_RANGE, "ERROR_INDEX_OUT_OF_RANGE")
  ,
    ( ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED
    , "ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED"
    )
  ,
    ( ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED
    , "ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED"
    )
  , (ERROR_NAME_DUPLICATED, "ERROR_NAME_DUPLICATED")
  , (ERROR_NAME_INVALID, "ERROR_NAME_INVALID")
  ,
    ( ERROR_ACTIONSET_NOT_ATTACHED
    , "ERROR_ACTIONSET_NOT_ATTACHED"
    )
  ,
    ( ERROR_ACTIONSETS_ALREADY_ATTACHED
    , "ERROR_ACTIONSETS_ALREADY_ATTACHED"
    )
  ,
    ( ERROR_LOCALIZED_NAME_DUPLICATED
    , "ERROR_LOCALIZED_NAME_DUPLICATED"
    )
  ,
    ( ERROR_LOCALIZED_NAME_INVALID
    , "ERROR_LOCALIZED_NAME_INVALID"
    )
  ,
    ( ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING
    , "ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING"
    )
  ,
    ( ERROR_COLOR_SPACE_UNSUPPORTED_FB
    , "ERROR_COLOR_SPACE_UNSUPPORTED_FB"
    )
  ,
    ( ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB
    , "ERROR_DISPLAY_REFRESH_RATE_UNSUPPORTED_FB"
    )
  ,
    ( ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT
    , "ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT"
    )
  ,
    ( ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT
    , "ERROR_SECONDARY_VIEW_CONFIGURATION_TYPE_NOT_ENABLED_MSFT"
    )
  ,
    ( ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT
    , "ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT"
    )
  ,
    ( ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR
    , "ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR"
    )
  ,
    ( ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR
    , "ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR"
    )
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
