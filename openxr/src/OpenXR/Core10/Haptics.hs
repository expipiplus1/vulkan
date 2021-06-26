{-# language CPP #-}
-- No documentation found for Chapter "Haptics"
module OpenXR.Core10.Haptics  ( applyHapticFeedback
                              , stopHapticFeedback
                              , HapticBaseHeader(..)
                              , IsHaptic(..)
                              , HapticActionInfo(..)
                              ) where

import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.NamedType ((:::))
import OpenXR.Core10.Handles (Action_T)
import {-# SOURCE #-} OpenXR.Core10.OtherTypes (HapticVibration)
import OpenXR.CStruct.Extends (Inheritable(..))
import OpenXR.Dynamic (InstanceCmds(pXrApplyHapticFeedback))
import OpenXR.Dynamic (InstanceCmds(pXrStopHapticFeedback))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.SemanticPaths (Path)
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.CStruct.Extends (SomeChild)
import OpenXR.CStruct.Extends (SomeChild(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAPTIC_ACTION_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAPTIC_VIBRATION))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrApplyHapticFeedback
  :: FunPtr (Ptr Session_T -> Ptr HapticActionInfo -> Ptr (SomeChild HapticBaseHeader) -> IO Result) -> Ptr Session_T -> Ptr HapticActionInfo -> Ptr (SomeChild HapticBaseHeader) -> IO Result

-- | xrApplyHapticFeedback - Apply haptic feedback
--
-- == Parameter Descriptions
--
-- = Description
--
-- Triggers a haptic event through the specified action of type
-- 'OpenXR.Core10.Enums.StructureType.TYPE_HAPTIC_VIBRATION'. The runtime
-- /should/ deliver this request to the appropriate device, but exactly
-- which device, if any, this event is sent to is up to the runtime to
-- decide. If an appropriate device is unavailable the runtime /may/ ignore
-- this request for haptic feedback.
--
-- If another haptic event from this session is currently happening on the
-- device bound to this action, the runtime /must/ interrupt that other
-- event and replace it with the new one.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTION_TYPE_MISMATCH'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
-- = See Also
--
-- 'HapticActionInfo', 'HapticBaseHeader',
-- 'OpenXR.Core10.OtherTypes.HapticVibration',
-- 'OpenXR.Core10.Handles.Session', 'stopHapticFeedback'
applyHapticFeedback :: forall a io
                     . (ToCStruct a, MonadIO io)
                    => -- | @session@ is the 'OpenXR.Core10.Handles.Session' to start outputting to.
                       --
                       -- #VUID-xrApplyHapticFeedback-session-parameter# @session@ /must/ be a
                       -- valid 'OpenXR.Core10.Handles.Session' handle
                       Session
                    -> -- | @hapticActionInfo@ is a pointer to 'HapticActionInfo' to provide action
                       -- and subaction paths information.
                       --
                       -- #VUID-xrApplyHapticFeedback-hapticActionInfo-parameter#
                       -- @hapticActionInfo@ /must/ be a pointer to a valid 'HapticActionInfo'
                       -- structure
                       HapticActionInfo
                    -> -- | @hapticFeedback@ is a pointer to a haptic event structure which starts
                       -- with an 'HapticBaseHeader'.
                       --
                       -- #VUID-xrApplyHapticFeedback-hapticFeedback-parameter# @hapticFeedback@
                       -- /must/ be a pointer to a valid 'HapticBaseHeader'-based structure. See
                       -- also: 'OpenXR.Core10.OtherTypes.HapticVibration'
                       ("hapticFeedback" ::: a)
                    -> io (Result)
applyHapticFeedback session hapticActionInfo hapticFeedback = liftIO . evalContT $ do
  let xrApplyHapticFeedbackPtr = pXrApplyHapticFeedback (instanceCmds (session :: Session))
  lift $ unless (xrApplyHapticFeedbackPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrApplyHapticFeedback is null" Nothing Nothing
  let xrApplyHapticFeedback' = mkXrApplyHapticFeedback xrApplyHapticFeedbackPtr
  hapticActionInfo' <- ContT $ withCStruct (hapticActionInfo)
  hapticFeedback' <- fmap castPtr $ ContT $ withCStruct (hapticFeedback)
  r <- lift $ traceAroundEvent "xrApplyHapticFeedback" (xrApplyHapticFeedback' (sessionHandle (session)) hapticActionInfo' hapticFeedback')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrStopHapticFeedback
  :: FunPtr (Ptr Session_T -> Ptr HapticActionInfo -> IO Result) -> Ptr Session_T -> Ptr HapticActionInfo -> IO Result

-- | xrStopHapticFeedback - Stop haptic feedback
--
-- == Parameter Descriptions
--
-- = Description
--
-- If a haptic event from this 'OpenXR.Core10.Handles.Action' is in
-- progress, when this function is called the runtime /must/ stop that
-- event.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTION_TYPE_MISMATCH'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
-- = See Also
--
-- 'HapticActionInfo', 'OpenXR.Core10.Handles.Session',
-- 'applyHapticFeedback'
stopHapticFeedback :: forall io
                    . (MonadIO io)
                   => -- | @session@ is the 'OpenXR.Core10.Handles.Session' to stop outputting to.
                      --
                      -- #VUID-xrStopHapticFeedback-session-parameter# @session@ /must/ be a
                      -- valid 'OpenXR.Core10.Handles.Session' handle
                      Session
                   -> -- | @hapticActionInfo@ is a pointer to an 'HapticActionInfo' to provide
                      -- action and subaction path information.
                      --
                      -- #VUID-xrStopHapticFeedback-hapticActionInfo-parameter#
                      -- @hapticActionInfo@ /must/ be a pointer to a valid 'HapticActionInfo'
                      -- structure
                      HapticActionInfo
                   -> io (Result)
stopHapticFeedback session hapticActionInfo = liftIO . evalContT $ do
  let xrStopHapticFeedbackPtr = pXrStopHapticFeedback (instanceCmds (session :: Session))
  lift $ unless (xrStopHapticFeedbackPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrStopHapticFeedback is null" Nothing Nothing
  let xrStopHapticFeedback' = mkXrStopHapticFeedback xrStopHapticFeedbackPtr
  hapticActionInfo' <- ContT $ withCStruct (hapticActionInfo)
  r <- lift $ traceAroundEvent "xrStopHapticFeedback" (xrStopHapticFeedback' (sessionHandle (session)) hapticActionInfo')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


-- | XrHapticBaseHeader - Base header for haptic feedback
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.HapticVibration',
-- 'OpenXR.Extensions.XR_VALVE_analog_threshold.InteractionProfileAnalogThresholdVALVE',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'applyHapticFeedback'
data HapticBaseHeader = HapticBaseHeader
  { -- | @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of this
    -- structure. This base structure itself has no associated
    -- 'OpenXR.Core10.Enums.StructureType.StructureType' value.
    --
    -- #VUID-XrHapticBaseHeader-type-type# @type@ /must/ be
    -- 'OpenXR.Core10.Enums.StructureType.TYPE_HAPTIC_VIBRATION'
    type' :: StructureType }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HapticBaseHeader)
#endif
deriving instance Show HapticBaseHeader

class ToCStruct a => IsHaptic a where
  toHapticBaseHeader :: a -> HapticBaseHeader

instance Inheritable HapticBaseHeader where
  peekSomeCChild :: Ptr (SomeChild HapticBaseHeader) -> IO (SomeChild HapticBaseHeader)
  peekSomeCChild p = do
    ty <- peek @StructureType (castPtr @(SomeChild HapticBaseHeader) @StructureType p)
    case ty of
      TYPE_HAPTIC_VIBRATION -> SomeChild <$> peekCStruct (castPtr @(SomeChild HapticBaseHeader) @HapticVibration p)
      c -> throwIO $
        IOError
          Nothing
          InvalidArgument
          "peekSomeCChild"
          ("Illegal struct inheritance of HapticBaseHeader with " <> show c)
          Nothing
          Nothing

instance ToCStruct HapticBaseHeader where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HapticBaseHeader{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (type')
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct HapticBaseHeader where
  peekCStruct p = do
    type' <- peek @StructureType ((p `plusPtr` 0 :: Ptr StructureType))
    pure $ HapticBaseHeader
             type'

instance Storable HapticBaseHeader where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HapticBaseHeader where
  zero = HapticBaseHeader
           zero


-- | XrHapticActionInfo - Information to output haptic feedback
--
-- == Member Descriptions
--
-- = Description
--
-- See 'OpenXR.Core10.Input.ActionCreateInfo' for a description of
-- subaction paths, and the restrictions on their use.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'applyHapticFeedback', 'stopHapticFeedback'
data HapticActionInfo = HapticActionInfo
  { -- | @action@ is the 'OpenXR.Core10.Handles.Action' handle for the desired
    -- output haptic action.
    --
    -- #VUID-XrHapticActionInfo-action-parameter# @action@ /must/ be a valid
    -- 'OpenXR.Core10.Handles.Action' handle
    action :: Ptr Action_T
  , -- | @subactionPath@ is the subaction path
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
    -- of the device to send the haptic event to, or
    -- 'OpenXR.Core10.APIConstants.NULL_PATH' to specify all subaction paths.
    -- If the subaction path is specified, it is one of the subaction paths
    -- that were specified when the action was created. If the subaction path
    -- was not specified when the action was created, the runtime /must/ return
    -- 'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'. If this parameter
    -- is specified, the runtime /must/ trigger the haptic events only on the
    -- device from the subaction path.
    subactionPath :: Path
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HapticActionInfo)
#endif
deriving instance Show HapticActionInfo

instance ToCStruct HapticActionInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HapticActionInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAPTIC_ACTION_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Action_T))) (action)
    poke ((p `plusPtr` 24 :: Ptr Path)) (subactionPath)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAPTIC_ACTION_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Action_T))) (zero)
    f

instance FromCStruct HapticActionInfo where
  peekCStruct p = do
    action <- peek @(Ptr Action_T) ((p `plusPtr` 16 :: Ptr (Ptr Action_T)))
    subactionPath <- peek @Path ((p `plusPtr` 24 :: Ptr Path))
    pure $ HapticActionInfo
             action subactionPath

instance Storable HapticActionInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HapticActionInfo where
  zero = HapticActionInfo
           zero
           zero

