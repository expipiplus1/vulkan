{-# language CPP #-}
-- No documentation found for Chapter "APIConstants"
module OpenXR.Core10.APIConstants  ( pattern MIN_HAPTIC_DURATION
                                   , MAX_EXTENSION_NAME_SIZE
                                   , pattern MAX_EXTENSION_NAME_SIZE
                                   , MAX_API_LAYER_NAME_SIZE
                                   , pattern MAX_API_LAYER_NAME_SIZE
                                   , MAX_API_LAYER_DESCRIPTION_SIZE
                                   , pattern MAX_API_LAYER_DESCRIPTION_SIZE
                                   , MAX_SYSTEM_NAME_SIZE
                                   , pattern MAX_SYSTEM_NAME_SIZE
                                   , MAX_APPLICATION_NAME_SIZE
                                   , pattern MAX_APPLICATION_NAME_SIZE
                                   , MAX_ENGINE_NAME_SIZE
                                   , pattern MAX_ENGINE_NAME_SIZE
                                   , MAX_RUNTIME_NAME_SIZE
                                   , pattern MAX_RUNTIME_NAME_SIZE
                                   , MAX_PATH_LENGTH
                                   , pattern MAX_PATH_LENGTH
                                   , MAX_STRUCTURE_NAME_SIZE
                                   , pattern MAX_STRUCTURE_NAME_SIZE
                                   , MAX_RESULT_STRING_SIZE
                                   , pattern MAX_RESULT_STRING_SIZE
                                   , MAX_GRAPHICS_APIS_SUPPORTED
                                   , pattern MAX_GRAPHICS_APIS_SUPPORTED
                                   , MAX_ACTION_SET_NAME_SIZE
                                   , pattern MAX_ACTION_SET_NAME_SIZE
                                   , MAX_ACTION_NAME_SIZE
                                   , pattern MAX_ACTION_NAME_SIZE
                                   , MAX_LOCALIZED_ACTION_SET_NAME_SIZE
                                   , pattern MAX_LOCALIZED_ACTION_SET_NAME_SIZE
                                   , MAX_LOCALIZED_ACTION_NAME_SIZE
                                   , pattern MAX_LOCALIZED_ACTION_NAME_SIZE
                                   , MIN_COMPOSITION_LAYERS_SUPPORTED
                                   , pattern MIN_COMPOSITION_LAYERS_SUPPORTED
                                   , NULL_PATH
                                   , pattern NULL_PATH
                                   , NULL_SYSTEM_ID
                                   , pattern NULL_SYSTEM_ID
                                   , NO_DURATION
                                   , pattern NO_DURATION
                                   , INFINITE_DURATION
                                   , pattern INFINITE_DURATION
                                   , FREQUENCY_UNSPECIFIED
                                   , pattern FREQUENCY_UNSPECIFIED
                                   , MAX_EVENT_DATA_SIZE
                                   , pattern MAX_EVENT_DATA_SIZE
                                   , HAND_JOINT_COUNT_EXT
                                   , pattern HAND_JOINT_COUNT_EXT
                                   , NULL_CONTROLLER_MODEL_KEY_MSFT
                                   , pattern NULL_CONTROLLER_MODEL_KEY_MSFT
                                   , pattern VK_NULL_HANDLE
                                   , IsHandle
                                   , HasObjectType(..)
                                   , Bool32(..)
                                   ) where

import Data.Int (Int64)
import Data.Word (Word64)
import OpenXR.Core10.Enums.ObjectType (ObjectType)
import OpenXR.Zero (Zero(..))
import OpenXR.Core10.FundamentalTypes (Bool32(..))
-- | XR_MIN_HAPTIC_DURATION - Indicates the shortest valid duration for the
-- device
--
-- = See Also
--
-- 'OpenXR.Core10.Haptics.applyHapticFeedback'
pattern MIN_HAPTIC_DURATION :: forall a . Integral a => a
pattern MIN_HAPTIC_DURATION = -1


type MAX_EXTENSION_NAME_SIZE = 128

-- No documentation found for TopLevel "XR_MAX_EXTENSION_NAME_SIZE"
pattern MAX_EXTENSION_NAME_SIZE :: forall a . Integral a => a
pattern MAX_EXTENSION_NAME_SIZE = 128


type MAX_API_LAYER_NAME_SIZE = 256

-- No documentation found for TopLevel "XR_MAX_API_LAYER_NAME_SIZE"
pattern MAX_API_LAYER_NAME_SIZE :: forall a . Integral a => a
pattern MAX_API_LAYER_NAME_SIZE = 256


type MAX_API_LAYER_DESCRIPTION_SIZE = 256

-- No documentation found for TopLevel "XR_MAX_API_LAYER_DESCRIPTION_SIZE"
pattern MAX_API_LAYER_DESCRIPTION_SIZE :: forall a . Integral a => a
pattern MAX_API_LAYER_DESCRIPTION_SIZE = 256


type MAX_SYSTEM_NAME_SIZE = 256

-- No documentation found for TopLevel "XR_MAX_SYSTEM_NAME_SIZE"
pattern MAX_SYSTEM_NAME_SIZE :: forall a . Integral a => a
pattern MAX_SYSTEM_NAME_SIZE = 256


type MAX_APPLICATION_NAME_SIZE = 128

-- No documentation found for TopLevel "XR_MAX_APPLICATION_NAME_SIZE"
pattern MAX_APPLICATION_NAME_SIZE :: forall a . Integral a => a
pattern MAX_APPLICATION_NAME_SIZE = 128


type MAX_ENGINE_NAME_SIZE = 128

-- No documentation found for TopLevel "XR_MAX_ENGINE_NAME_SIZE"
pattern MAX_ENGINE_NAME_SIZE :: forall a . Integral a => a
pattern MAX_ENGINE_NAME_SIZE = 128


type MAX_RUNTIME_NAME_SIZE = 128

-- No documentation found for TopLevel "XR_MAX_RUNTIME_NAME_SIZE"
pattern MAX_RUNTIME_NAME_SIZE :: forall a . Integral a => a
pattern MAX_RUNTIME_NAME_SIZE = 128


type MAX_PATH_LENGTH = 256

-- No documentation found for TopLevel "XR_MAX_PATH_LENGTH"
pattern MAX_PATH_LENGTH :: forall a . Integral a => a
pattern MAX_PATH_LENGTH = 256


type MAX_STRUCTURE_NAME_SIZE = 64

-- No documentation found for TopLevel "XR_MAX_STRUCTURE_NAME_SIZE"
pattern MAX_STRUCTURE_NAME_SIZE :: forall a . Integral a => a
pattern MAX_STRUCTURE_NAME_SIZE = 64


type MAX_RESULT_STRING_SIZE = 64

-- No documentation found for TopLevel "XR_MAX_RESULT_STRING_SIZE"
pattern MAX_RESULT_STRING_SIZE :: forall a . Integral a => a
pattern MAX_RESULT_STRING_SIZE = 64


type MAX_GRAPHICS_APIS_SUPPORTED = 32

-- No documentation found for TopLevel "XR_MAX_GRAPHICS_APIS_SUPPORTED"
pattern MAX_GRAPHICS_APIS_SUPPORTED :: forall a . Integral a => a
pattern MAX_GRAPHICS_APIS_SUPPORTED = 32


type MAX_ACTION_SET_NAME_SIZE = 64

-- No documentation found for TopLevel "XR_MAX_ACTION_SET_NAME_SIZE"
pattern MAX_ACTION_SET_NAME_SIZE :: forall a . Integral a => a
pattern MAX_ACTION_SET_NAME_SIZE = 64


type MAX_ACTION_NAME_SIZE = 64

-- No documentation found for TopLevel "XR_MAX_ACTION_NAME_SIZE"
pattern MAX_ACTION_NAME_SIZE :: forall a . Integral a => a
pattern MAX_ACTION_NAME_SIZE = 64


type MAX_LOCALIZED_ACTION_SET_NAME_SIZE = 128

-- No documentation found for TopLevel "XR_MAX_LOCALIZED_ACTION_SET_NAME_SIZE"
pattern MAX_LOCALIZED_ACTION_SET_NAME_SIZE :: forall a . Integral a => a
pattern MAX_LOCALIZED_ACTION_SET_NAME_SIZE = 128


type MAX_LOCALIZED_ACTION_NAME_SIZE = 128

-- No documentation found for TopLevel "XR_MAX_LOCALIZED_ACTION_NAME_SIZE"
pattern MAX_LOCALIZED_ACTION_NAME_SIZE :: forall a . Integral a => a
pattern MAX_LOCALIZED_ACTION_NAME_SIZE = 128


type MIN_COMPOSITION_LAYERS_SUPPORTED = 16

-- No documentation found for TopLevel "XR_MIN_COMPOSITION_LAYERS_SUPPORTED"
pattern MIN_COMPOSITION_LAYERS_SUPPORTED :: forall a . Integral a => a
pattern MIN_COMPOSITION_LAYERS_SUPPORTED = 16


type NULL_PATH = 0

-- | XR_NULL_PATH - A NULL semantic path
--
-- = Description
--
-- The only
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- value defined to be constant across all instances is the invalid path
-- 'NULL_PATH'. No well-formed path string is associated with 'NULL_PATH'.
-- Unless explicitly permitted, it /should/ not be passed to API calls or
-- used as a structure attribute when a valid
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- is required.
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
pattern NULL_PATH :: forall a . Integral a => a
pattern NULL_PATH = 0


type NULL_SYSTEM_ID = 0

-- | XR_NULL_SYSTEM_ID - NULL system identifier
--
-- = Description
--
-- The only
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
-- value defined to be constant across all instances is the invalid system
-- 'NULL_SYSTEM_ID'. No supported system is associated with
-- 'NULL_SYSTEM_ID'. Unless explicitly permitted, it /should/ not be passed
-- to API calls or used as a structure attribute when a valid
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
-- is required.
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'OpenXR.Core10.Device.getSystem'
pattern NULL_SYSTEM_ID :: forall a . Integral a => a
pattern NULL_SYSTEM_ID = 0


type NO_DURATION = 0

-- | XR_NO_DURATION - Constant for no duration\/immediate timeout
--
-- = Description
--
-- For the case of timeout durations, 'NO_DURATION' /may/ be used to
-- indicate that the timeout is immediate.
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >
pattern NO_DURATION :: forall a . Integral a => a
pattern NO_DURATION = 0


type INFINITE_DURATION = 0x7fffffffffffffff

-- | XR_INFINITE_DURATION - Constant for infinite duration\/never times out
--
-- = Description
--
-- 'INFINITE_DURATION' is a special value that /may/ be used to indicate
-- that the timeout never occurs. A timeout with a duration that refers to
-- the past has the same effect as a timeout of 'NO_DURATION'.
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >
pattern INFINITE_DURATION :: Int64
pattern INFINITE_DURATION = 0x7fffffffffffffff


type FREQUENCY_UNSPECIFIED = 0

-- | XR_FREQUENCY_UNSPECIFIED - Runtime should determine optimal frequency
-- for haptic pulse
--
-- = See Also
--
-- 'OpenXR.Core10.Haptics.applyHapticFeedback'
pattern FREQUENCY_UNSPECIFIED :: forall a . Integral a => a
pattern FREQUENCY_UNSPECIFIED = 0


type MAX_EVENT_DATA_SIZE = 4016 {- sizeof(XrEventDataBuffer) -}

-- | XR_MAX_EVENT_DATA_SIZE - Maximum event data buffer size
--
-- = See Also
--
-- 'OpenXR.Core10.Instance.EventDataBuffer'
pattern MAX_EVENT_DATA_SIZE :: Int
pattern MAX_EVENT_DATA_SIZE = 4016 {- sizeof(XrEventDataBuffer) -}


type HAND_JOINT_COUNT_EXT = 26

-- | XR_HAND_JOINT_COUNT_EXT - The number of hand joint enums defined in
-- XrHandJointEXT
--
-- = Description
--
-- 'HAND_JOINT_COUNT_EXT' defines the number of hand joint enumerants
-- defined in 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointEXT'
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointEXT'
pattern HAND_JOINT_COUNT_EXT :: forall a . Integral a => a
pattern HAND_JOINT_COUNT_EXT = 26


type NULL_CONTROLLER_MODEL_KEY_MSFT = 0

-- | XR_NULL_CONTROLLER_MODEL_KEY_MSFT - The value representing an invalid
-- model key
--
-- = Description
--
-- 'NULL_CONTROLLER_MODEL_KEY_MSFT' defines an invalid model key value.
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrControllerModelKeyMSFT >
pattern NULL_CONTROLLER_MODEL_KEY_MSFT :: forall a . Integral a => a
pattern NULL_CONTROLLER_MODEL_KEY_MSFT = 0


-- No documentation found for TopLevel "VK_NULL_HANDLE"
pattern VK_NULL_HANDLE :: IsHandle a => a
pattern VK_NULL_HANDLE <- ((== zero) -> True)
  where VK_NULL_HANDLE = zero

-- | A class for things which can be created with 'VK_NULL_HANDLE'.
class (Eq a, Zero a) => IsHandle a where


class HasObjectType a where
  objectTypeAndHandle :: a -> (ObjectType, Word64)

