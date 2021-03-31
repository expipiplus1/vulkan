{-# language CPP #-}
-- No documentation found for Chapter "Device"
module OpenXR.Core10.Device  ( getSystem
                             , getSystemProperties
                             , createSession
                             , withSession
                             , destroySession
                             , enumerateEnvironmentBlendModes
                             , SystemId(..)
                             , SystemGetInfo(..)
                             , SystemProperties(..)
                             , SystemGraphicsProperties(..)
                             , SystemTrackingProperties(..)
                             , SessionCreateInfo(..)
                             ) where

import OpenXR.CStruct.Utils (FixedArray)
import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showParen)
import Numeric (showHex)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero)
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.Core10.FundamentalTypes (bool32ToBool)
import OpenXR.Core10.FundamentalTypes (boolToBool32)
import OpenXR.CStruct.Extends (forgetExtensions)
import OpenXR.CStruct.Utils (lowerArrayPtr)
import OpenXR.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import OpenXR.NamedType ((:::))
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.CStruct.Extends (Chain)
import OpenXR.Core10.Enums.EnvironmentBlendMode (EnvironmentBlendMode)
import OpenXR.Core10.Enums.EnvironmentBlendMode (EnvironmentBlendMode(..))
import OpenXR.CStruct.Extends (Extends)
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct.Extends (Extensible(..))
import OpenXR.Core10.Enums.FormFactor (FormFactor)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_D3D11_enable (GraphicsBindingD3D11KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_D3D12_enable (GraphicsBindingD3D12KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_MNDX_egl_enable (GraphicsBindingEGLMNDX)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_es_enable (GraphicsBindingOpenGLESAndroidKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (GraphicsBindingOpenGLWaylandKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (GraphicsBindingOpenGLWin32KHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (GraphicsBindingOpenGLXcbKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_opengl_enable (GraphicsBindingOpenGLXlibKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_vulkan_enable (GraphicsBindingVulkanKHR)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_holographic_window_attachment (HolographicWindowAttachmentMSFT)
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Dynamic (InstanceCmds(pXrCreateSession))
import OpenXR.Dynamic (InstanceCmds(pXrDestroySession))
import OpenXR.Dynamic (InstanceCmds(pXrEnumerateEnvironmentBlendModes))
import OpenXR.Dynamic (InstanceCmds(pXrGetSystem))
import OpenXR.Dynamic (InstanceCmds(pXrGetSystemProperties))
import OpenXR.Core10.Handles (Instance_T)
import OpenXR.Core10.APIConstants (MAX_SYSTEM_NAME_SIZE)
import OpenXR.Exception (OpenXrException(..))
import OpenXR.CStruct.Extends (PeekChain)
import OpenXR.CStruct.Extends (PeekChain(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct.Extends (PokeChain(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session(Session))
import OpenXR.Core10.Enums.SessionCreateFlagBits (SessionCreateFlags)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXTX_overlay (SessionCreateInfoOverlayEXTX)
import OpenXR.Core10.Handles (Session_T)
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_eye_gaze_interaction (SystemEyeGazeInteractionPropertiesEXT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (SystemHandTrackingMeshPropertiesMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_hand_tracking (SystemHandTrackingPropertiesEXT)
import OpenXR.Core10.Enums.ViewConfigurationType (ViewConfigurationType)
import OpenXR.Core10.Enums.ViewConfigurationType (ViewConfigurationType(..))
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SESSION_CREATE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SYSTEM_GET_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SYSTEM_PROPERTIES))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetSystem
  :: FunPtr (Ptr Instance_T -> Ptr SystemGetInfo -> Ptr SystemId -> IO Result) -> Ptr Instance_T -> Ptr SystemGetInfo -> Ptr SystemId -> IO Result

-- | xrGetSystem - Gets a system identifier
--
-- == Parameter Descriptions
--
-- = Description
--
-- To get an
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- an application specifies its desired
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#form_factor_description form factor>
-- to 'getSystem' and gets the runtime’s
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
-- associated with that configuration.
--
-- If the form factor is supported but temporarily unavailable, 'getSystem'
-- /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_FORM_FACTOR_UNAVAILABLE'. A runtime
-- /may/ return 'OpenXR.Core10.Enums.Result.SUCCESS' on a subsequent call
-- for a form factor it previously returned
-- 'OpenXR.Core10.Enums.Result.ERROR_FORM_FACTOR_UNAVAILABLE'. For example,
-- connecting or warming up hardware might cause an unavailable form factor
-- to become available.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FORM_FACTOR_UNAVAILABLE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FORM_FACTOR_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.APIConstants.NULL_SYSTEM_ID',
-- 'OpenXR.Core10.Handles.Instance', 'SystemGetInfo',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
getSystem :: forall io
           . (MonadIO io)
          => -- | @instance@ is the handle of the instance from which to get the
             -- information.
             --
             -- #VUID-xrGetSystem-instance-parameter# @instance@ /must/ be a valid
             -- 'OpenXR.Core10.Handles.Instance' handle
             Instance
          -> -- | @getInfo@ is a pointer to an 'SystemGetInfo' structure containing the
             -- application’s requests for a system.
             --
             -- #VUID-xrGetSystem-getInfo-parameter# @getInfo@ /must/ be a pointer to a
             -- valid 'SystemGetInfo' structure
             SystemGetInfo
          -> io (SystemId)
getSystem instance' getInfo = liftIO . evalContT $ do
  let xrGetSystemPtr = pXrGetSystem (instanceCmds (instance' :: Instance))
  lift $ unless (xrGetSystemPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetSystem is null" Nothing Nothing
  let xrGetSystem' = mkXrGetSystem xrGetSystemPtr
  getInfo' <- ContT $ withCStruct (getInfo)
  pSystemId <- ContT $ bracket (callocBytes @SystemId 8) free
  r <- lift $ traceAroundEvent "xrGetSystem" (xrGetSystem' (instanceHandle (instance')) getInfo' (pSystemId))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  systemId <- lift $ peek @SystemId pSystemId
  pure $ (systemId)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetSystemProperties
  :: FunPtr (Ptr Instance_T -> SystemId -> Ptr (SomeStruct SystemProperties) -> IO Result) -> Ptr Instance_T -> SystemId -> Ptr (SomeStruct SystemProperties) -> IO Result

-- | xrGetSystemProperties - Gets the properties of a particular system
--
-- == Parameter Descriptions
--
-- = Description
--
-- An application /can/ call 'getSystemProperties' to retrieve information
-- about the system such as vendor ID, system name, and graphics and
-- tracking properties.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'SystemProperties'
getSystemProperties :: forall a io
                     . (Extendss SystemProperties a, PokeChain a, PeekChain a, MonadIO io)
                    => -- | @instance@ is the instance from which @systemId@ was retrieved.
                       --
                       -- #VUID-xrGetSystemProperties-instance-parameter# @instance@ /must/ be a
                       -- valid 'OpenXR.Core10.Handles.Instance' handle
                       Instance
                    -> -- | @systemId@ is the
                       -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
                       -- whose properties will be queried.
                       SystemId
                    -> io (SystemProperties a)
getSystemProperties instance' systemId = liftIO . evalContT $ do
  let xrGetSystemPropertiesPtr = pXrGetSystemProperties (instanceCmds (instance' :: Instance))
  lift $ unless (xrGetSystemPropertiesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetSystemProperties is null" Nothing Nothing
  let xrGetSystemProperties' = mkXrGetSystemProperties xrGetSystemPropertiesPtr
  pProperties <- ContT (withZeroCStruct @(SystemProperties _))
  r <- lift $ traceAroundEvent "xrGetSystemProperties" (xrGetSystemProperties' (instanceHandle (instance')) (systemId) (forgetExtensions (pProperties)))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  properties <- lift $ peekCStruct @(SystemProperties _) pProperties
  pure $ (properties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateSession
  :: FunPtr (Ptr Instance_T -> Ptr (SomeStruct SessionCreateInfo) -> Ptr (Ptr Session_T) -> IO Result) -> Ptr Instance_T -> Ptr (SomeStruct SessionCreateInfo) -> Ptr (Ptr Session_T) -> IO Result

-- | xrCreateSession - Creates an XrSession
--
-- == Parameter Descriptions
--
-- = Description
--
-- Creates a session using the provided @createInfo@ and returns a handle
-- to that session. This session is created in the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_IDLE' state, and a
-- corresponding 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged'
-- event to the 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_IDLE' state
-- /must/ be generated as the first such event for the new session.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LIMIT_REACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_GRAPHICS_DEVICE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_GRAPHICS_REQUIREMENTS_CALL_MISSING'
--
-- = See Also
--
-- 'OpenXR.Core10.Instance.ExtensionProperties',
-- 'OpenXR.Core10.Handles.Instance', 'OpenXR.Core10.Handles.Session',
-- 'OpenXR.Core10.Enums.SessionCreateFlagBits.SessionCreateFlags',
-- 'SessionCreateInfo', 'OpenXR.Core10.Session.beginSession',
-- 'destroySession', 'OpenXR.Core10.Session.endSession'
createSession :: forall a io
               . (Extendss SessionCreateInfo a, PokeChain a, MonadIO io)
              => -- | @instance@ is the instance from which @systemId@ was retrieved.
                 --
                 -- #VUID-xrCreateSession-instance-parameter# @instance@ /must/ be a valid
                 -- 'OpenXR.Core10.Handles.Instance' handle
                 Instance
              -> -- | @createInfo@ is a pointer to an 'SessionCreateInfo' structure containing
                 -- information about how to create the session.
                 --
                 -- #VUID-xrCreateSession-createInfo-parameter# @createInfo@ /must/ be a
                 -- pointer to a valid 'SessionCreateInfo' structure
                 (SessionCreateInfo a)
              -> io (Session)
createSession instance' createInfo = liftIO . evalContT $ do
  let cmds = instanceCmds (instance' :: Instance)
  let xrCreateSessionPtr = pXrCreateSession cmds
  lift $ unless (xrCreateSessionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateSession is null" Nothing Nothing
  let xrCreateSession' = mkXrCreateSession xrCreateSessionPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pSession <- ContT $ bracket (callocBytes @(Ptr Session_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateSession" (xrCreateSession' (instanceHandle (instance')) (forgetExtensions createInfo') (pSession))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  session <- lift $ peek @(Ptr Session_T) pSession
  pure $ (((\h -> Session h cmds ) session))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSession' and 'destroySession'
--
-- To ensure that 'destroySession' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSession :: forall a io r . (Extendss SessionCreateInfo a, PokeChain a, MonadIO io) => Instance -> SessionCreateInfo a -> (io Session -> (Session -> io ()) -> r) -> r
withSession instance' createInfo b =
  b (createSession instance' createInfo)
    (\(o0) -> destroySession o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrDestroySession
  :: FunPtr (Ptr Session_T -> IO Result) -> Ptr Session_T -> IO Result

-- | xrDestroySession - Destroys an XrSession
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'OpenXR.Core10.Handles.Session' handles are destroyed using
-- 'destroySession'. When an 'OpenXR.Core10.Handles.Session' is destroyed,
-- all handles that are children of that 'OpenXR.Core10.Handles.Session'
-- are also destroyed.
--
-- The application is responsible for ensuring that it has no calls using
-- @session@ in progress when the session is destroyed.
--
-- 'destroySession' can be called when the session is in any session state.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrDestroySession-session-parameter# @session@ /must/ be a
--     valid 'OpenXR.Core10.Handles.Session' handle
--
-- == Thread Safety
--
-- -   Access to @session@, and any child handles, /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'OpenXR.Core10.Session.beginSession',
-- 'createSession', 'OpenXR.Core10.Session.endSession'
destroySession :: forall io
                . (MonadIO io)
               => -- | @session@ is the session to destroy.
                  Session
               -> io ()
destroySession session = liftIO $ do
  let xrDestroySessionPtr = pXrDestroySession (instanceCmds (session :: Session))
  unless (xrDestroySessionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrDestroySession is null" Nothing Nothing
  let xrDestroySession' = mkXrDestroySession xrDestroySessionPtr
  r <- traceAroundEvent "xrDestroySession" (xrDestroySession' (sessionHandle (session)))
  when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateEnvironmentBlendModes
  :: FunPtr (Ptr Instance_T -> SystemId -> ViewConfigurationType -> Word32 -> Ptr Word32 -> Ptr EnvironmentBlendMode -> IO Result) -> Ptr Instance_T -> SystemId -> ViewConfigurationType -> Word32 -> Ptr Word32 -> Ptr EnvironmentBlendMode -> IO Result

-- | xrEnumerateEnvironmentBlendModes - Lists environment blend modes
--
-- == Parameter Descriptions
--
-- -   @instance@ is the instance from which @systemId@ was retrieved.
--
-- -   @systemId@ is the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
--     whose environment blend modes will be enumerated.
--
-- -   @viewConfigurationType@ is the
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' to
--     enumerate.
--
-- -   @environmentBlendModeCapacityInput@ is the capacity of the
--     @environmentBlendModes@ array, or 0 to indicate a request to
--     retrieve the required capacity.
--
-- -   @environmentBlendModeCountOutput@ is a pointer to the count of
--     @environmentBlendModes@ written, or a pointer to the required
--     capacity in the case that @environmentBlendModeCapacityInput@ is 0.
--
-- -   @environmentBlendModes@ is a pointer to an array of
--     'OpenXR.Core10.Enums.EnvironmentBlendMode.EnvironmentBlendMode'
--     values, but /can/ be @NULL@ if @environmentBlendModeCapacityInput@
--     is 0.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @environmentBlendModes@ size.
--
-- = Description
--
-- Enumerates the set of environment blend modes that this runtime supports
-- for a given view configuration of the system. Environment blend modes
-- /should/ be in order from highest to lowest runtime preference.
--
-- Runtimes /must/ always return identical buffer contents from this
-- enumeration for the given @systemId@ and @viewConfigurationType@ for the
-- lifetime of the instance.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateEnvironmentBlendModes-instance-parameter#
--     @instance@ /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrEnumerateEnvironmentBlendModes-viewConfigurationType-parameter#
--     @viewConfigurationType@ /must/ be a valid
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
--     value
--
-- -   #VUID-xrEnumerateEnvironmentBlendModes-environmentBlendModeCountOutput-parameter#
--     @environmentBlendModeCountOutput@ /must/ be a pointer to a
--     @uint32_t@ value
--
-- -   #VUID-xrEnumerateEnvironmentBlendModes-environmentBlendModes-parameter#
--     If @environmentBlendModeCapacityInput@ is not @0@,
--     @environmentBlendModes@ /must/ be a pointer to an array of
--     @environmentBlendModeCapacityInput@
--     'OpenXR.Core10.Enums.EnvironmentBlendMode.EnvironmentBlendMode'
--     values
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.EnvironmentBlendMode.EnvironmentBlendMode',
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'
enumerateEnvironmentBlendModes :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "xrEnumerateEnvironmentBlendModes" "instance"
                                  Instance
                               -> -- No documentation found for Nested "xrEnumerateEnvironmentBlendModes" "systemId"
                                  SystemId
                               -> -- No documentation found for Nested "xrEnumerateEnvironmentBlendModes" "viewConfigurationType"
                                  ViewConfigurationType
                               -> io (("environmentBlendModes" ::: Vector EnvironmentBlendMode))
enumerateEnvironmentBlendModes instance' systemId viewConfigurationType = liftIO . evalContT $ do
  let xrEnumerateEnvironmentBlendModesPtr = pXrEnumerateEnvironmentBlendModes (instanceCmds (instance' :: Instance))
  lift $ unless (xrEnumerateEnvironmentBlendModesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateEnvironmentBlendModes is null" Nothing Nothing
  let xrEnumerateEnvironmentBlendModes' = mkXrEnumerateEnvironmentBlendModes xrEnumerateEnvironmentBlendModesPtr
  let instance'' = instanceHandle (instance')
  pEnvironmentBlendModeCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateEnvironmentBlendModes" (xrEnumerateEnvironmentBlendModes' instance'' (systemId) (viewConfigurationType) (0) (pEnvironmentBlendModeCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  environmentBlendModeCountOutput <- lift $ peek @Word32 pEnvironmentBlendModeCountOutput
  pEnvironmentBlendModes <- ContT $ bracket (callocBytes @EnvironmentBlendMode ((fromIntegral (environmentBlendModeCountOutput)) * 4)) free
  r' <- lift $ traceAroundEvent "xrEnumerateEnvironmentBlendModes" (xrEnumerateEnvironmentBlendModes' instance'' (systemId) (viewConfigurationType) ((environmentBlendModeCountOutput)) (pEnvironmentBlendModeCountOutput) (pEnvironmentBlendModes))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  environmentBlendModeCountOutput' <- lift $ peek @Word32 pEnvironmentBlendModeCountOutput
  environmentBlendModes' <- lift $ generateM (fromIntegral (environmentBlendModeCountOutput')) (\i -> peek @EnvironmentBlendMode ((pEnvironmentBlendModes `advancePtrBytes` (4 * (i)) :: Ptr EnvironmentBlendMode)))
  pure $ (environmentBlendModes')


-- | XrSystemId - Identifier for a system
--
-- = Description
--
-- An
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
-- is an opaque atom used by the runtime to identify a system. The value
-- 'OpenXR.Core10.APIConstants.NULL_SYSTEM_ID' is considered an invalid
-- system.
--
-- = See Also
--
-- 'OpenXR.Core10.APIConstants.NULL_SYSTEM_ID', 'SessionCreateInfo',
-- 'SystemProperties',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.VulkanDeviceCreateInfoKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.VulkanGraphicsDeviceGetInfoKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.VulkanInstanceCreateInfoKHR',
-- 'enumerateEnvironmentBlendModes',
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurationViews',
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurations',
-- 'OpenXR.Extensions.XR_KHR_D3D11_enable.getD3D11GraphicsRequirementsKHR',
-- 'OpenXR.Extensions.XR_KHR_D3D12_enable.getD3D12GraphicsRequirementsKHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_es_enable.getOpenGLESGraphicsRequirementsKHR',
-- 'OpenXR.Extensions.XR_KHR_opengl_enable.getOpenGLGraphicsRequirementsKHR',
-- 'getSystem', 'getSystemProperties',
-- 'OpenXR.Core10.ViewConfigurations.getViewConfigurationProperties',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanDeviceExtensionsKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanGraphicsDeviceKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable2.getVulkanGraphicsRequirements2KHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanGraphicsRequirementsKHR',
-- 'OpenXR.Extensions.XR_KHR_vulkan_enable.getVulkanInstanceExtensionsKHR'
newtype SystemId = SystemId Word64
  deriving newtype (Eq, Ord, Storable, Zero)
instance Show SystemId where
  showsPrec p (SystemId x) = showParen (p >= 11) (showString "SystemId 0x" . showHex x)


-- | XrSystemGetInfo - Specifies desired attributes of the system
--
-- == Member Descriptions
--
-- = Description
--
-- The 'SystemGetInfo' structure specifies attributes about a system as
-- desired by an application.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.FormFactor.FormFactor',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'getSystem'
data SystemGetInfo = SystemGetInfo
  { -- | @formFactor@ is the 'OpenXR.Core10.Enums.FormFactor.FormFactor'
    -- requested by the application.
    --
    -- #VUID-XrSystemGetInfo-formFactor-parameter# @formFactor@ /must/ be a
    -- valid 'OpenXR.Core10.Enums.FormFactor.FormFactor' value
    formFactor :: FormFactor }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SystemGetInfo)
#endif
deriving instance Show SystemGetInfo

instance ToCStruct SystemGetInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SystemGetInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_GET_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FormFactor)) (formFactor)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_GET_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr FormFactor)) (zero)
    f

instance FromCStruct SystemGetInfo where
  peekCStruct p = do
    formFactor <- peek @FormFactor ((p `plusPtr` 16 :: Ptr FormFactor))
    pure $ SystemGetInfo
             formFactor

instance Storable SystemGetInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SystemGetInfo where
  zero = SystemGetInfo
           zero


-- | XrSystemProperties - Properties of a particular system
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'SystemGraphicsProperties',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'SystemTrackingProperties', 'getSystem', 'getSystemProperties'
data SystemProperties (es :: [Type]) = SystemProperties
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    --
    -- #VUID-XrSystemProperties-next-next# @next@ /must/ be @NULL@ or a valid
    -- pointer to the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
    -- See also:
    -- 'OpenXR.Extensions.XR_EXT_eye_gaze_interaction.SystemEyeGazeInteractionPropertiesEXT',
    -- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.SystemHandTrackingMeshPropertiesMSFT',
    -- 'OpenXR.Extensions.XR_EXT_hand_tracking.SystemHandTrackingPropertiesEXT'
    next :: Chain es
  , -- | @systemId@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
    -- identifying the system.
    systemId :: SystemId
  , -- | @vendorId@ is a unique identifier for the vendor of the system.
    vendorId :: Word32
  , -- | @systemName@ is a string containing the name of the system.
    systemName :: ByteString
  , -- | @graphicsProperties@ is an 'SystemGraphicsProperties' structure
    -- specifying the system graphics properties.
    graphicsProperties :: SystemGraphicsProperties
  , -- | @trackingProperties@ is an 'SystemTrackingProperties' structure
    -- specifying system tracking properties.
    trackingProperties :: SystemTrackingProperties
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SystemProperties (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SystemProperties es)

instance Extensible SystemProperties where
  extensibleTypeName = "SystemProperties"
  setNext x next = x{next = next}
  getNext SystemProperties{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SystemProperties e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SystemHandTrackingMeshPropertiesMSFT = Just f
    | Just Refl <- eqT @e @SystemHandTrackingPropertiesEXT = Just f
    | Just Refl <- eqT @e @SystemEyeGazeInteractionPropertiesEXT = Just f
    | otherwise = Nothing

instance (Extendss SystemProperties es, PokeChain es) => ToCStruct (SystemProperties es) where
  withCStruct x f = allocaBytesAligned 304 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SystemProperties{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_PROPERTIES)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr SystemId)) (systemId)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (vendorId)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 28 :: Ptr (FixedArray MAX_SYSTEM_NAME_SIZE CChar))) (systemName)
    lift $ poke ((p `plusPtr` 284 :: Ptr SystemGraphicsProperties)) (graphicsProperties)
    lift $ poke ((p `plusPtr` 296 :: Ptr SystemTrackingProperties)) (trackingProperties)
    lift $ f
  cStructSize = 304
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_PROPERTIES)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr SystemId)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 28 :: Ptr (FixedArray MAX_SYSTEM_NAME_SIZE CChar))) (mempty)
    lift $ poke ((p `plusPtr` 284 :: Ptr SystemGraphicsProperties)) (zero)
    lift $ poke ((p `plusPtr` 296 :: Ptr SystemTrackingProperties)) (zero)
    lift $ f

instance (Extendss SystemProperties es, PeekChain es) => FromCStruct (SystemProperties es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    systemId <- peek @SystemId ((p `plusPtr` 16 :: Ptr SystemId))
    vendorId <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    systemName <- packCString (lowerArrayPtr ((p `plusPtr` 28 :: Ptr (FixedArray MAX_SYSTEM_NAME_SIZE CChar))))
    graphicsProperties <- peekCStruct @SystemGraphicsProperties ((p `plusPtr` 284 :: Ptr SystemGraphicsProperties))
    trackingProperties <- peekCStruct @SystemTrackingProperties ((p `plusPtr` 296 :: Ptr SystemTrackingProperties))
    pure $ SystemProperties
             next' systemId vendorId systemName graphicsProperties trackingProperties

instance es ~ '[] => Zero (SystemProperties es) where
  zero = SystemProperties
           ()
           zero
           zero
           mempty
           zero
           zero


-- | XrSystemGraphicsProperties - Graphics-related properties of a particular
-- system
--
-- == Member Descriptions
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'SystemProperties', 'SystemTrackingProperties', 'getSystem',
-- 'getSystemProperties'
data SystemGraphicsProperties = SystemGraphicsProperties
  { -- | @maxSwapchainImageHeight@ is the maximum swapchain image pixel height
    -- supported by this system.
    maxSwapchainImageHeight :: Word32
  , -- | @maxSwapchainImageWidth@ is the maximum swapchain image pixel width
    -- supported by this system.
    maxSwapchainImageWidth :: Word32
  , -- | @maxLayerCount@ is the maximum number of composition layers supported by
    -- this system. The runtime /must/ support at least
    -- 'OpenXR.Core10.APIConstants.MIN_COMPOSITION_LAYERS_SUPPORTED' layers.
    maxLayerCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SystemGraphicsProperties)
#endif
deriving instance Show SystemGraphicsProperties

instance ToCStruct SystemGraphicsProperties where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SystemGraphicsProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (maxSwapchainImageHeight)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (maxSwapchainImageWidth)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (maxLayerCount)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct SystemGraphicsProperties where
  peekCStruct p = do
    maxSwapchainImageHeight <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    maxSwapchainImageWidth <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    maxLayerCount <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ SystemGraphicsProperties
             maxSwapchainImageHeight maxSwapchainImageWidth maxLayerCount

instance Storable SystemGraphicsProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SystemGraphicsProperties where
  zero = SystemGraphicsProperties
           zero
           zero
           zero


-- | XrSystemTrackingProperties - Tracking-related properties of a particular
-- system
--
-- == Member Descriptions
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'SystemGraphicsProperties',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'SystemProperties', 'getSystem', 'getSystemProperties'
data SystemTrackingProperties = SystemTrackingProperties
  { -- | @orientationTracking@ is set to 'OpenXR.Core10.FundamentalTypes.TRUE' to
    -- indicate the system supports orientational tracking of the view pose(s),
    -- 'OpenXR.Core10.FundamentalTypes.FALSE' otherwise.
    orientationTracking :: Bool
  , -- | @positionTracking@ is set to 'OpenXR.Core10.FundamentalTypes.TRUE' to
    -- indicate the system supports positional tracking of the view pose(s),
    -- 'OpenXR.Core10.FundamentalTypes.FALSE' otherwise.
    positionTracking :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SystemTrackingProperties)
#endif
deriving instance Show SystemTrackingProperties

instance ToCStruct SystemTrackingProperties where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SystemTrackingProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Bool32)) (boolToBool32 (orientationTracking))
    poke ((p `plusPtr` 4 :: Ptr Bool32)) (boolToBool32 (positionTracking))
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 4 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SystemTrackingProperties where
  peekCStruct p = do
    orientationTracking <- peek @Bool32 ((p `plusPtr` 0 :: Ptr Bool32))
    positionTracking <- peek @Bool32 ((p `plusPtr` 4 :: Ptr Bool32))
    pure $ SystemTrackingProperties
             (bool32ToBool orientationTracking) (bool32ToBool positionTracking)

instance Storable SystemTrackingProperties where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SystemTrackingProperties where
  zero = SystemTrackingProperties
           zero
           zero


-- | XrSessionCreateInfo - Creates a session
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.SessionCreateFlagBits.SessionCreateFlags',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >,
-- 'createSession'
data SessionCreateInfo (es :: [Type]) = SessionCreateInfo
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR. Note that in most
    -- cases one graphics API extension specific struct needs to be in this
    -- next chain.
    --
    -- @next@, unless otherwise specified via an extension, /must/ contain
    -- exactly one graphics API binding structure (a structure whose name
    -- begins with @\"XrGraphicsBinding\"@) or
    -- 'OpenXR.Core10.Enums.Result.ERROR_GRAPHICS_DEVICE_INVALID' /must/ be
    -- returned.
    --
    -- #VUID-XrSessionCreateInfo-next-next# @next@ /must/ be @NULL@ or a valid
    -- pointer to the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
    -- See also:
    -- 'OpenXR.Extensions.XR_KHR_D3D11_enable.GraphicsBindingD3D11KHR',
    -- 'OpenXR.Extensions.XR_KHR_D3D12_enable.GraphicsBindingD3D12KHR',
    -- 'OpenXR.Extensions.XR_MNDX_egl_enable.GraphicsBindingEGLMNDX',
    -- 'OpenXR.Extensions.XR_KHR_opengl_es_enable.GraphicsBindingOpenGLESAndroidKHR',
    -- 'OpenXR.Extensions.XR_KHR_opengl_enable.GraphicsBindingOpenGLWaylandKHR',
    -- 'OpenXR.Extensions.XR_KHR_opengl_enable.GraphicsBindingOpenGLWin32KHR',
    -- 'OpenXR.Extensions.XR_KHR_opengl_enable.GraphicsBindingOpenGLXcbKHR',
    -- 'OpenXR.Extensions.XR_KHR_opengl_enable.GraphicsBindingOpenGLXlibKHR',
    -- 'OpenXR.Extensions.XR_KHR_vulkan_enable.GraphicsBindingVulkanKHR',
    -- 'OpenXR.Extensions.XR_MSFT_holographic_window_attachment.HolographicWindowAttachmentMSFT',
    -- 'OpenXR.Extensions.XR_EXTX_overlay.SessionCreateInfoOverlayEXTX'
    next :: Chain es
  , -- | @createFlags@ identifies
    -- 'OpenXR.Core10.Enums.SessionCreateFlagBits.SessionCreateFlags' that
    -- apply to the creation.
    --
    -- #VUID-XrSessionCreateInfo-createFlags-zerobitmask# @createFlags@ /must/
    -- be @0@
    createFlags :: SessionCreateFlags
  , -- | @systemId@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
    -- representing the system of devices to be used by this session.
    --
    -- @systemId@ /must/ be a valid
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrSystemId >
    -- or 'OpenXR.Core10.Enums.Result.ERROR_SYSTEM_INVALID' /must/ be returned.
    systemId :: SystemId
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SessionCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SessionCreateInfo es)

instance Extensible SessionCreateInfo where
  extensibleTypeName = "SessionCreateInfo"
  setNext x next = x{next = next}
  getNext SessionCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SessionCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @HolographicWindowAttachmentMSFT = Just f
    | Just Refl <- eqT @e @GraphicsBindingEGLMNDX = Just f
    | Just Refl <- eqT @e @SessionCreateInfoOverlayEXTX = Just f
    | Just Refl <- eqT @e @GraphicsBindingVulkanKHR = Just f
    | Just Refl <- eqT @e @GraphicsBindingOpenGLESAndroidKHR = Just f
    | Just Refl <- eqT @e @GraphicsBindingD3D12KHR = Just f
    | Just Refl <- eqT @e @GraphicsBindingD3D11KHR = Just f
    | Just Refl <- eqT @e @GraphicsBindingOpenGLWaylandKHR = Just f
    | Just Refl <- eqT @e @GraphicsBindingOpenGLXcbKHR = Just f
    | Just Refl <- eqT @e @GraphicsBindingOpenGLXlibKHR = Just f
    | Just Refl <- eqT @e @GraphicsBindingOpenGLWin32KHR = Just f
    | otherwise = Nothing

instance (Extendss SessionCreateInfo es, PokeChain es) => ToCStruct (SessionCreateInfo es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SessionCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SESSION_CREATE_INFO)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr SessionCreateFlags)) (createFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr SystemId)) (systemId)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SESSION_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr SystemId)) (zero)
    lift $ f

instance (Extendss SessionCreateInfo es, PeekChain es) => FromCStruct (SessionCreateInfo es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    createFlags <- peek @SessionCreateFlags ((p `plusPtr` 16 :: Ptr SessionCreateFlags))
    systemId <- peek @SystemId ((p `plusPtr` 24 :: Ptr SystemId))
    pure $ SessionCreateInfo
             next' createFlags systemId

instance es ~ '[] => Zero (SessionCreateInfo es) where
  zero = SessionCreateInfo
           ()
           zero
           zero

