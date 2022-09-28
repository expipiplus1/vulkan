{-# language CPP #-}
-- | = Name
--
-- XR_KHR_android_surface_swapchain - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_android_surface_swapchain  XR_KHR_android_surface_swapchain>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 5
--
-- = Revision
--
-- 4
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'createSwapchainAndroidSurfaceKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_android_surface_swapchain OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_android_surface_swapchain  ( createSwapchainAndroidSurfaceKHR
                                                           , withSwapchainAndroidSurfaceKHR
                                                           , KHR_android_surface_swapchain_SPEC_VERSION
                                                           , pattern KHR_android_surface_swapchain_SPEC_VERSION
                                                           , KHR_ANDROID_SURFACE_SWAPCHAIN_EXTENSION_NAME
                                                           , pattern KHR_ANDROID_SURFACE_SWAPCHAIN_EXTENSION_NAME
                                                           , Jobject
                                                           ) where

import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import OpenXR.CStruct (ToCStruct(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Foreign.Storable (Storable(peek))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.Core10.Image (destroySwapchain)
import OpenXR.CStruct.Extends (forgetExtensions)
import OpenXR.NamedType ((:::))
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.Dynamic (InstanceCmds(pXrCreateSwapchainAndroidSurfaceKHR))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session(Session))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Handles (Swapchain)
import OpenXR.Core10.Handles (Swapchain(Swapchain))
import OpenXR.Core10.Image (SwapchainCreateInfo)
import OpenXR.Core10.Handles (Swapchain_T)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateSwapchainAndroidSurfaceKHR
  :: FunPtr (Ptr Session_T -> Ptr (SomeStruct SwapchainCreateInfo) -> Ptr (Ptr Swapchain_T) -> Ptr Jobject -> IO Result) -> Ptr Session_T -> Ptr (SomeStruct SwapchainCreateInfo) -> Ptr (Ptr Swapchain_T) -> Ptr Jobject -> IO Result

-- | xrCreateSwapchainAndroidSurfaceKHR - Creates a swapchain and an Android
-- Surface
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'createSwapchainAndroidSurfaceKHR' creates an
-- 'OpenXR.Core10.Handles.Swapchain' object returned in @swapchain@ and an
-- Android Surface @jobject@ returned in @surface@. The @jobject@ /must/ be
-- valid to be passed back to Java code using JNI and /must/ be valid to be
-- used with ordinary Android APIs for submitting images to Surfaces. The
-- returned 'OpenXR.Core10.Handles.Swapchain' /must/ be valid to be
-- referenced in 'OpenXR.Core10.OtherTypes.SwapchainSubImage' structures to
-- show content on the screen. The width and height passed in
-- 'OpenXR.Core10.Image.SwapchainCreateInfo' /may/ not be persistent
-- throughout the life cycle of the created swapchain, since on Android,
-- the size of the images is controlled by the producer and possibly
-- changes at any time.
--
-- The only function that is allowed to be called on the
-- 'OpenXR.Core10.Handles.Swapchain' returned from this function is
-- 'OpenXR.Core10.Image.destroySwapchain'. For example, calling any of the
-- functions 'OpenXR.Core10.Image.enumerateSwapchainImages',
-- 'OpenXR.Core10.Image.acquireSwapchainImage',
-- 'OpenXR.Core10.Image.waitSwapchainImage' or
-- 'OpenXR.Core10.Image.releaseSwapchainImage' is invalid.
--
-- When the application receives the
-- 'OpenXR.Core10.OtherTypes.EventDataSessionStateChanged' event with the
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_STOPPING' state, it
-- /must/ ensure that no threads are writing to any of the Android surfaces
-- created with this extension before calling
-- 'OpenXR.Core10.Session.endSession'. The effect of writing frames to the
-- Surface when the session is in states other than
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_VISIBLE' or
-- 'OpenXR.Core10.Enums.SessionState.SESSION_STATE_FOCUSED' is undefined.
--
-- 'createSwapchainAndroidSurfaceKHR' /must/ return the same set of error
-- codes as 'OpenXR.Core10.Image.createSwapchain' under the same
-- circumstances, plus
-- 'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED' in case the
-- function is not supported.
--
-- == Valid Usage of 'OpenXR.Core10.Image.SwapchainCreateInfo' members
--
-- -   The 'OpenXR.Core10.Image.SwapchainCreateInfo'::@format@,
--     'OpenXR.Core10.Image.SwapchainCreateInfo'::@sampleCount@,
--     'OpenXR.Core10.Image.SwapchainCreateInfo'::@faceCount@,
--     'OpenXR.Core10.Image.SwapchainCreateInfo'::@arraySize@ and
--     'OpenXR.Core10.Image.SwapchainCreateInfo'::@mipCount@
--     members of the structure passed as the @info@ parameter /must/ be
--     zero.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrCreateSwapchainAndroidSurfaceKHR-extension-notenabled# The
--     @XR_KHR_android_surface_swapchain@ extension /must/ be enabled prior
--     to calling 'createSwapchainAndroidSurfaceKHR'
--
-- -   #VUID-xrCreateSwapchainAndroidSurfaceKHR-session-parameter#
--     @session@ /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrCreateSwapchainAndroidSurfaceKHR-info-parameter# @info@
--     /must/ be a pointer to a valid
--     'OpenXR.Core10.Image.SwapchainCreateInfo' structure
--
-- -   #VUID-xrCreateSwapchainAndroidSurfaceKHR-swapchain-parameter#
--     @swapchain@ /must/ be a pointer to an
--     'OpenXR.Core10.Handles.Swapchain' handle
--
-- -   #VUID-xrCreateSwapchainAndroidSurfaceKHR-surface-parameter#
--     @surface@ /must/ be a pointer to a @jobject@ value
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_LIMIT_REACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'OpenXR.Core10.Handles.Swapchain',
-- 'OpenXR.Core10.Image.SwapchainCreateInfo',
-- 'OpenXR.Core10.Image.destroySwapchain'
createSwapchainAndroidSurfaceKHR :: forall a io
                                  . ( Extendss SwapchainCreateInfo a
                                    , PokeChain a
                                    , MonadIO io )
                                 => -- | @session@ is an 'OpenXR.Core10.Handles.Session' handle previously
                                    -- created with 'OpenXR.Core10.Device.createSession'.
                                    Session
                                 -> -- | @info@ is a pointer to an 'OpenXR.Core10.Image.SwapchainCreateInfo'
                                    -- structure.
                                    (SwapchainCreateInfo a)
                                 -> -- | @surface@ is a pointer to a @jobject@ where the created Android Surface
                                    -- is returned.
                                    ("surface" ::: Ptr Jobject)
                                 -> io (Result, Swapchain)
createSwapchainAndroidSurfaceKHR session info surface = liftIO . evalContT $ do
  let cmds = case session of Session{instanceCmds} -> instanceCmds
  let xrCreateSwapchainAndroidSurfaceKHRPtr = pXrCreateSwapchainAndroidSurfaceKHR cmds
  lift $ unless (xrCreateSwapchainAndroidSurfaceKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateSwapchainAndroidSurfaceKHR is null" Nothing Nothing
  let xrCreateSwapchainAndroidSurfaceKHR' = mkXrCreateSwapchainAndroidSurfaceKHR xrCreateSwapchainAndroidSurfaceKHRPtr
  info' <- ContT $ withCStruct (info)
  pSwapchain <- ContT $ bracket (callocBytes @(Ptr Swapchain_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateSwapchainAndroidSurfaceKHR" (xrCreateSwapchainAndroidSurfaceKHR'
                                                                       (sessionHandle (session))
                                                                       (forgetExtensions info')
                                                                       (pSwapchain)
                                                                       (surface))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  swapchain <- lift $ peek @(Ptr Swapchain_T) pSwapchain
  pure $ (r, ((\h -> Swapchain h cmds ) swapchain))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSwapchainAndroidSurfaceKHR' and 'destroySwapchain'
--
-- To ensure that 'destroySwapchain' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSwapchainAndroidSurfaceKHR :: forall a io r . (Extendss SwapchainCreateInfo a, PokeChain a, MonadIO io) => Session -> SwapchainCreateInfo a -> Ptr Jobject -> (io (Result, Swapchain) -> ((Result, Swapchain) -> io ()) -> r) -> r
withSwapchainAndroidSurfaceKHR session info surface b =
  b (createSwapchainAndroidSurfaceKHR session info surface)
    (\(_, o1) -> destroySwapchain o1)


type KHR_android_surface_swapchain_SPEC_VERSION = 4

-- No documentation found for TopLevel "XR_KHR_android_surface_swapchain_SPEC_VERSION"
pattern KHR_android_surface_swapchain_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_android_surface_swapchain_SPEC_VERSION = 4


type KHR_ANDROID_SURFACE_SWAPCHAIN_EXTENSION_NAME = "XR_KHR_android_surface_swapchain"

-- No documentation found for TopLevel "XR_KHR_ANDROID_SURFACE_SWAPCHAIN_EXTENSION_NAME"
pattern KHR_ANDROID_SURFACE_SWAPCHAIN_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ANDROID_SURFACE_SWAPCHAIN_EXTENSION_NAME = "XR_KHR_android_surface_swapchain"


data Jobject

