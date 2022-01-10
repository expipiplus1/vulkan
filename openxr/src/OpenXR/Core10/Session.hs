{-# language CPP #-}
-- No documentation found for Chapter "Session"
module OpenXR.Core10.Session  ( beginSession
                              , useSession
                              , endSession
                              , requestExitSession
                              , SessionBeginInfo(..)
                              ) where

import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.CStruct.Extends (forgetExtensions)
import OpenXR.CStruct.Extends (Chain)
import OpenXR.CStruct.Extends (Extends)
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct.Extends (Extensible(..))
import OpenXR.Dynamic (InstanceCmds(pXrBeginSession))
import OpenXR.Dynamic (InstanceCmds(pXrEndSession))
import OpenXR.Dynamic (InstanceCmds(pXrRequestExitSession))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.CStruct.Extends (PeekChain)
import OpenXR.CStruct.Extends (PeekChain(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct.Extends (PokeChain(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_secondary_view_configuration (SecondaryViewConfigurationSessionBeginInfoMSFT)
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session(Session))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.ViewConfigurationType (ViewConfigurationType)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SESSION_BEGIN_INFO))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrBeginSession
  :: FunPtr (Ptr Session_T -> Ptr (SomeStruct SessionBeginInfo) -> IO Result) -> Ptr Session_T -> Ptr (SomeStruct SessionBeginInfo) -> IO Result

-- | xrBeginSession - Begins an XrSession
--
-- == Parameter Descriptions
--
-- = Description
--
-- When the application receives
-- 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged' event with the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_READY' state, the
-- application /should/ then call 'beginSession' to start rendering frames
-- for display to the user.
--
-- After this function successfully returns, the session
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_running is considered to be running>.
-- The application /should/ then start its frame loop consisting of some
-- sequence of
-- 'OpenXR.Core10.DisplayTiming.waitFrame'\/'OpenXR.Core10.DisplayTiming.beginFrame'\/'OpenXR.Core10.DisplayTiming.endFrame'
-- calls.
--
-- If the session
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_running is already running>
-- when the application calls 'beginSession', the runtime /must/ return
-- error 'OpenXR.Core10.Enums.Result.ERROR_SESSION_RUNNING'. If the session
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_not_running is not running>
-- when the application calls 'beginSession', but the session is not yet in
-- the 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_READY' state, the
-- runtime /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_READY'.
--
-- Note that a runtime /may/ decide not to show the user any given frame
-- from a session at any time, for example if the user has switched to a
-- different application’s running session. The application should check
-- whether 'OpenXR.Core10.DisplayTiming.waitFrame' returns an
-- 'OpenXR.Core10.DisplayTiming.FrameState' with @shouldRender@ set to true
-- before rendering a given frame to determine whether that frame will be
-- visible to the user.
--
-- Runtime session frame state /must/ start in a reset state when a session
-- transitions to
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_running running>
-- so that no state is carried over from when the same session was
-- previously running.
--
-- If @primaryViewConfigurationType@ in @beginInfo@ is not supported by the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
-- used to create the @session@, the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_READY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_RUNNING'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'SessionBeginInfo',
-- 'OpenXR.Core10.Device.createSession',
-- 'OpenXR.Core10.Device.destroySession', 'endSession'
beginSession :: forall a io
              . (Extendss SessionBeginInfo a, PokeChain a, MonadIO io)
             => -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
                --
                -- #VUID-xrBeginSession-session-parameter# @session@ /must/ be a valid
                -- 'OpenXR.Core10.Handles.Session' handle
                Session
             -> -- | @beginInfo@ is a pointer to an 'SessionBeginInfo' structure.
                --
                -- #VUID-xrBeginSession-beginInfo-parameter# @beginInfo@ /must/ be a
                -- pointer to a valid 'SessionBeginInfo' structure
                (SessionBeginInfo a)
             -> io (Result)
beginSession session beginInfo = liftIO . evalContT $ do
  let xrBeginSessionPtr = pXrBeginSession (case session of Session{instanceCmds} -> instanceCmds)
  lift $ unless (xrBeginSessionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrBeginSession is null" Nothing Nothing
  let xrBeginSession' = mkXrBeginSession xrBeginSessionPtr
  beginInfo' <- ContT $ withCStruct (beginInfo)
  r <- lift $ traceAroundEvent "xrBeginSession" (xrBeginSession' (sessionHandle (session)) (forgetExtensions beginInfo'))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)

-- | This function will call the supplied action between calls to
-- 'beginSession' and 'endSession'
--
-- Note that 'endSession' is *not* called if an exception is thrown by the
-- inner action.
useSession :: forall a io r . (Extendss SessionBeginInfo a, PokeChain a, MonadIO io) => Session -> SessionBeginInfo a -> (Result -> io r) -> io (Result, r)
useSession session beginInfo a =
  do
    x <- beginSession session beginInfo
    r <- a x
    d <- (\(_) -> endSession session) x
    pure (d, r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEndSession
  :: FunPtr (Ptr Session_T -> IO Result) -> Ptr Session_T -> IO Result

-- | xrEndSession - Ends an XrSession
--
-- == Parameter Descriptions
--
-- = Description
--
-- When the application receives
-- 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged' event with the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_STOPPING' state, the
-- application should stop its frame loop and then call 'endSession' to end
-- the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_running running>
-- session. This function signals to the runtime that the application will
-- no longer call 'OpenXR.Core10.DisplayTiming.waitFrame',
-- 'OpenXR.Core10.DisplayTiming.beginFrame' or
-- 'OpenXR.Core10.DisplayTiming.endFrame' from any thread allowing the
-- runtime to safely transition the session to
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_IDLE'. The application
-- /must/ also avoid reading input state or sending haptic output after
-- calling 'endSession'.
--
-- If the session
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_not_running is not running>
-- when the application calls 'endSession', the runtime /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_RUNNING'. If the session
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_running is still running>
-- when the application calls 'endSession', but the session is not yet in
-- the 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_STOPPING' state, the
-- runtime /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_STOPPING'.
--
-- If the application wishes to exit a running session, the application can
-- call 'requestExitSession' so that the session transitions from
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_IDLE' to
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_EXITING'.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_STOPPING'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_RUNNING'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'beginSession',
-- 'OpenXR.Core10.Device.createSession',
-- 'OpenXR.Core10.Device.destroySession'
endSession :: forall io
            . (MonadIO io)
           => -- | @session@ is a handle to a
              -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_running running>
              -- 'OpenXR.Core10.Handles.Session'.
              --
              -- #VUID-xrEndSession-session-parameter# @session@ /must/ be a valid
              -- 'OpenXR.Core10.Handles.Session' handle
              Session
           -> io (Result)
endSession session = liftIO $ do
  let xrEndSessionPtr = pXrEndSession (case session of Session{instanceCmds} -> instanceCmds)
  unless (xrEndSessionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEndSession is null" Nothing Nothing
  let xrEndSession' = mkXrEndSession xrEndSessionPtr
  r <- traceAroundEvent "xrEndSession" (xrEndSession' (sessionHandle (session)))
  when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrRequestExitSession
  :: FunPtr (Ptr Session_T -> IO Result) -> Ptr Session_T -> IO Result

-- | xrRequestExitSession - Request to exit a running session.
--
-- == Parameter Descriptions
--
-- = Description
--
-- An application can only call 'endSession' when the session is in the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_STOPPING' state, which
-- allows runtimes to seamlessly transition from one application’s session
-- to another. When an application wishes to exit a
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_running running>
-- session, the application can call 'requestExitSession', requesting that
-- the runtime transition through the various intermediate session states
-- including 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_STOPPING' to
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_EXITING'.
--
-- If @session@
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_not_running is not running>
-- when 'requestExitSession' is called,
-- 'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_RUNNING' /must/ be
-- returned.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_RUNNING'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'OpenXR.Core10.Device.destroySession',
-- 'endSession'
requestExitSession :: forall io
                    . (MonadIO io)
                   => -- | @session@ is a handle to a running 'OpenXR.Core10.Handles.Session'.
                      --
                      -- #VUID-xrRequestExitSession-session-parameter# @session@ /must/ be a
                      -- valid 'OpenXR.Core10.Handles.Session' handle
                      Session
                   -> io (Result)
requestExitSession session = liftIO $ do
  let xrRequestExitSessionPtr = pXrRequestExitSession (case session of Session{instanceCmds} -> instanceCmds)
  unless (xrRequestExitSessionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrRequestExitSession is null" Nothing Nothing
  let xrRequestExitSession' = mkXrRequestExitSession xrRequestExitSessionPtr
  r <- traceAroundEvent "xrRequestExitSession" (xrRequestExitSession' (sessionHandle (session)))
  when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


-- | XrSessionBeginInfo - Struct containing session begin info
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType',
-- 'beginSession'
data SessionBeginInfo (es :: [Type]) = SessionBeginInfo
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    --
    -- #VUID-XrSessionBeginInfo-next-next# @next@ /must/ be @NULL@ or a valid
    -- pointer to the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
    -- See also:
    -- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationSessionBeginInfoMSFT'
    next :: Chain es
  , -- | @primaryViewConfigurationType@ is the
    -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' to use
    -- during this session to provide images for the form factor’s primary
    -- displays.
    --
    -- #VUID-XrSessionBeginInfo-primaryViewConfigurationType-parameter#
    -- @primaryViewConfigurationType@ /must/ be a valid
    -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' value
    primaryViewConfigurationType :: ViewConfigurationType
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SessionBeginInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SessionBeginInfo es)

instance Extensible SessionBeginInfo where
  extensibleTypeName = "SessionBeginInfo"
  setNext SessionBeginInfo{..} next' = SessionBeginInfo{next = next', ..}
  getNext SessionBeginInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SessionBeginInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SecondaryViewConfigurationSessionBeginInfoMSFT = Just f
    | otherwise = Nothing

instance (Extendss SessionBeginInfo es, PokeChain es) => ToCStruct (SessionBeginInfo es) where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SessionBeginInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SESSION_BEGIN_INFO)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (primaryViewConfigurationType)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SESSION_BEGIN_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (zero)
    lift $ f

instance (Extendss SessionBeginInfo es, PeekChain es) => FromCStruct (SessionBeginInfo es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    primaryViewConfigurationType <- peek @ViewConfigurationType ((p `plusPtr` 16 :: Ptr ViewConfigurationType))
    pure $ SessionBeginInfo
             next' primaryViewConfigurationType

instance es ~ '[] => Zero (SessionBeginInfo es) where
  zero = SessionBeginInfo
           ()
           zero

