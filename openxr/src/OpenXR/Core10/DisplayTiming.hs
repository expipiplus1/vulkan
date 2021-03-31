{-# language CPP #-}
-- No documentation found for Chapter "DisplayTiming"
module OpenXR.Core10.DisplayTiming  ( beginFrame
                                    , useFrame
                                    , locateViews
                                    , endFrame
                                    , waitFrame
                                    , waitFrameSafe
                                    , View(..)
                                    , ViewLocateInfo(..)
                                    , ViewState(..)
                                    , FrameBeginInfo(..)
                                    , FrameEndInfo(..)
                                    , FrameWaitInfo(..)
                                    , FrameState(..)
                                    ) where

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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
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
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.Core10.FundamentalTypes (bool32ToBool)
import OpenXR.Core10.FundamentalTypes (boolToBool32)
import OpenXR.CStruct.Extends (forgetExtensions)
import OpenXR.CStruct.Extends (withSomeChild)
import OpenXR.NamedType ((:::))
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.CStruct.Extends (Chain)
import OpenXR.Core10.OtherTypes (CompositionLayerBaseHeader)
import OpenXR.Core10.FundamentalTypes (Duration)
import OpenXR.Core10.Enums.EnvironmentBlendMode (EnvironmentBlendMode)
import OpenXR.CStruct.Extends (Extends)
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct.Extends (Extensible(..))
import OpenXR.Core10.OtherTypes (Fovf)
import OpenXR.CStruct.Extends (Inheritable(peekSomeCChild))
import OpenXR.Dynamic (InstanceCmds(pXrBeginFrame))
import OpenXR.Dynamic (InstanceCmds(pXrEndFrame))
import OpenXR.Dynamic (InstanceCmds(pXrLocateViews))
import OpenXR.Dynamic (InstanceCmds(pXrWaitFrame))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.CStruct.Extends (PeekChain)
import OpenXR.CStruct.Extends (PeekChain(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct.Extends (PokeChain(..))
import OpenXR.Core10.Space (Posef)
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_secondary_view_configuration (SecondaryViewConfigurationFrameEndInfoMSFT)
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_secondary_view_configuration (SecondaryViewConfigurationFrameStateMSFT)
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.CStruct.Extends (SomeChild)
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Handles (Space_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.FundamentalTypes (Time)
import OpenXR.Core10.Enums.ViewConfigurationType (ViewConfigurationType)
import OpenXR.Core10.Enums.ViewStateFlagBits (ViewStateFlags)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_FRAME_BEGIN_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_FRAME_END_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_FRAME_STATE))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_FRAME_WAIT_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VIEW))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VIEW_LOCATE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_VIEW_STATE))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrBeginFrame
  :: FunPtr (Ptr Session_T -> Ptr FrameBeginInfo -> IO Result) -> Ptr Session_T -> Ptr FrameBeginInfo -> IO Result

-- | xrBeginFrame - Marks a frame
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'beginFrame' is called prior to the start of frame rendering. The
-- application /should/ still call 'beginFrame' but omit rendering work for
-- the frame if 'FrameState'::@shouldRender@ is
-- 'OpenXR.Core10.FundamentalTypes.FALSE'.
--
-- The runtime /must/ return the error code
-- 'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID' if there was no
-- corresponding successful call to 'waitFrame'.
--
-- The runtime /must/ return the success code
-- 'OpenXR.Core10.Enums.Result.FRAME_DISCARDED' if a prior 'beginFrame' has
-- been called without an intervening call to 'endFrame'.
--
-- The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_RUNNING' if the @session@
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_not_running is not running>.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrBeginFrame-session-parameter# @session@ /must/ be a valid
--     'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrBeginFrame-frameBeginInfo-parameter# If @frameBeginInfo@ is
--     not @NULL@, @frameBeginInfo@ /must/ be a pointer to a valid
--     'FrameBeginInfo' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
--     -   'OpenXR.Core10.Enums.Result.FRAME_DISCARDED'
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_RUNNING'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'FrameBeginInfo', 'OpenXR.Core10.Handles.Session', 'endFrame',
-- 'waitFrame'
beginFrame :: forall io
            . (MonadIO io)
           => -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
              Session
           -> -- | @frameBeginInfo@ exists for extensibility purposes, it is @NULL@ or a
              -- pointer to a valid 'FrameBeginInfo'.
              ("frameBeginInfo" ::: Maybe FrameBeginInfo)
           -> io (Result)
beginFrame session frameBeginInfo = liftIO . evalContT $ do
  let xrBeginFramePtr = pXrBeginFrame (instanceCmds (session :: Session))
  lift $ unless (xrBeginFramePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrBeginFrame is null" Nothing Nothing
  let xrBeginFrame' = mkXrBeginFrame xrBeginFramePtr
  frameBeginInfo' <- case (frameBeginInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  r <- lift $ traceAroundEvent "xrBeginFrame" (xrBeginFrame' (sessionHandle (session)) frameBeginInfo')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)

-- | This function will call the supplied action between calls to
-- 'beginFrame' and 'endFrame'
--
-- Note that 'endFrame' is *not* called if an exception is thrown by the
-- inner action.
useFrame :: forall a io r . (Extendss FrameEndInfo a, PokeChain a, MonadIO io) => Session -> Maybe FrameBeginInfo -> FrameEndInfo a -> (Result -> io r) -> io (Result, r)
useFrame session frameBeginInfo frameEndInfo a =
  do
    x <- beginFrame session frameBeginInfo
    r <- a x
    d <- (\(_) -> endFrame session frameEndInfo) x
    pure (d, r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrLocateViews
  :: FunPtr (Ptr Session_T -> Ptr ViewLocateInfo -> Ptr ViewState -> Word32 -> Ptr Word32 -> Ptr View -> IO Result) -> Ptr Session_T -> Ptr ViewLocateInfo -> Ptr ViewState -> Word32 -> Ptr Word32 -> Ptr View -> IO Result

-- | xrLocateViews - Gets view and projection info
--
-- == Parameter Descriptions
--
-- -   @session@ is a handle to the provided
--     'OpenXR.Core10.Handles.Session'.
--
-- -   @viewLocateInfo@ is a pointer to a valid 'ViewLocateInfo' structure.
--
-- -   @viewState@ is the output structure with the viewer state
--     information.
--
-- -   @viewCapacityInput@ is an input parameter which specifies the
--     capacity of the @views@ array. The required capacity /must/ be same
--     as defined by the corresponding
--     'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType'.
--
-- -   @viewCountOutput@ is an output parameter which identifies the valid
--     count of @views@.
--
-- -   @views@ is an array of 'View'.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @views@ size.
--
-- = Description
--
-- The 'locateViews' function returns the view and projection info for a
-- particular display time. This time is typically the target display time
-- for a given frame. Repeatedly calling 'locateViews' with the same time
-- /may/ not necessarily return the same result. Instead the prediction
-- gets increasingly accurate as the function is called closer to the given
-- time for which a prediction is made. This allows an application to get
-- the predicted views as late as possible in its pipeline to get the least
-- amount of latency and prediction error.
--
-- 'locateViews' returns an array of 'View' elements, one for each view of
-- the specified view configuration type, along with an 'ViewState'
-- containing additional state data shared across all views. The eye each
-- view corresponds to is statically defined in
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#view_configuration_type >
-- in case the application wants to apply eye-specific rendering traits.
-- The 'ViewState' and 'View' member data may change on subsequent calls to
-- 'locateViews', and so applications /must/ not assume it to be constant.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrLocateViews-session-parameter# @session@ /must/ be a valid
--     'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrLocateViews-viewLocateInfo-parameter# @viewLocateInfo@
--     /must/ be a pointer to a valid 'ViewLocateInfo' structure
--
-- -   #VUID-xrLocateViews-viewState-parameter# @viewState@ /must/ be a
--     pointer to an 'ViewState' structure
--
-- -   #VUID-xrLocateViews-viewCountOutput-parameter# @viewCountOutput@
--     /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrLocateViews-views-parameter# If @viewCapacityInput@ is not
--     @0@, @views@ /must/ be a pointer to an array of @viewCapacityInput@
--     'View' structures
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_TIME_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'View', 'ViewLocateInfo', 'ViewState'
locateViews :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "xrLocateViews" "session"
               Session
            -> -- No documentation found for Nested "xrLocateViews" "viewLocateInfo"
               ViewLocateInfo
            -> io (Result, ViewState, ("views" ::: Vector View))
locateViews session viewLocateInfo = liftIO . evalContT $ do
  let xrLocateViewsPtr = pXrLocateViews (instanceCmds (session :: Session))
  lift $ unless (xrLocateViewsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrLocateViews is null" Nothing Nothing
  let xrLocateViews' = mkXrLocateViews xrLocateViewsPtr
  let session' = sessionHandle (session)
  viewLocateInfo' <- ContT $ withCStruct (viewLocateInfo)
  pViewState <- ContT (withZeroCStruct @ViewState)
  pViewCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrLocateViews" (xrLocateViews' session' viewLocateInfo' (pViewState) (0) (pViewCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  viewCountOutput <- lift $ peek @Word32 pViewCountOutput
  pViews <- ContT $ bracket (callocBytes @View ((fromIntegral (viewCountOutput)) * 64)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pViews `advancePtrBytes` (i * 64) :: Ptr View) . ($ ())) [0..(fromIntegral (viewCountOutput)) - 1]
  r' <- lift $ traceAroundEvent "xrLocateViews" (xrLocateViews' session' viewLocateInfo' (pViewState) ((viewCountOutput)) (pViewCountOutput) ((pViews)))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  viewState <- lift $ peekCStruct @ViewState pViewState
  viewCountOutput' <- lift $ peek @Word32 pViewCountOutput
  views' <- lift $ generateM (fromIntegral (viewCountOutput')) (\i -> peekCStruct @View (((pViews) `advancePtrBytes` (64 * (i)) :: Ptr View)))
  pure $ ((r'), viewState, views')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEndFrame
  :: FunPtr (Ptr Session_T -> Ptr (SomeStruct FrameEndInfo) -> IO Result) -> Ptr Session_T -> Ptr (SomeStruct FrameEndInfo) -> IO Result

-- | xrEndFrame - Marks a frame
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'endFrame' /may/ return immediately to the application.
-- 'FrameEndInfo'::@displayTime@ /should/ be computed using values returned
-- by 'waitFrame'. The runtime /should/ be robust against variations in the
-- timing of calls to 'waitFrame', since a pipelined system may call
-- 'waitFrame' on a separate thread from 'beginFrame' and 'endFrame'
-- without any synchronization guarantees.
--
-- Note
--
-- An accurate predicted display time is very important to avoid black
-- pull-in by reprojection and to reduce motion judder in case the runtime
-- does not implement a translational reprojection. Reprojection should
-- never display images before the display refresh period they were
-- predicted for, even if they are completed early, because this will cause
-- motion judder just the same. In other words, the better the predicted
-- display time, the less latency experienced by the user.
--
-- Every call to 'endFrame' /must/ be preceded by a successful call to
-- 'beginFrame'. Failure to do so /must/ result in
-- 'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID' being returned by
-- 'endFrame'. 'FrameEndInfo' /may/ reference swapchains into which the
-- application has rendered for this frame. From each
-- 'OpenXR.Core10.Handles.Swapchain' only one image index is implicitly
-- referenced per frame, the one corresponding to the last call to
-- 'OpenXR.Core10.Image.releaseSwapchainImage'. However, a specific
-- swapchain (and by extension a specific swapchain image index) /may/ be
-- referenced in 'FrameEndInfo' multiple times. This can be used for
-- example to render a side by side image into a single swapchain image and
-- referencing it twice with differing image rectangles in different
-- layers.
--
-- If no layers are provided then the display /must/ be cleared.
--
-- 'OpenXR.Core10.Enums.Result.ERROR_LAYER_INVALID' /must/ be returned if
-- an unknown, unsupported layer type, or @NULL@ pointer is passed as one
-- of the 'FrameEndInfo'::layers.
--
-- 'OpenXR.Core10.Enums.Result.ERROR_LAYER_INVALID' /must/ be returned if a
-- layer references a swapchain that has no released swapchain image.
--
-- 'OpenXR.Core10.Enums.Result.ERROR_LAYER_LIMIT_EXCEEDED' /must/ be
-- returned if 'FrameEndInfo'::layerCount exceeds
-- 'OpenXR.Core10.Device.SystemGraphicsProperties'::maxLayerCount or if the
-- runtime is unable to composite the specified layers due to resource
-- constraints.
--
-- 'OpenXR.Core10.Enums.Result.ERROR_SWAPCHAIN_RECT_INVALID' /must/ be
-- returned if 'FrameEndInfo'::layers contains a composition layer which
-- references pixels outside of the associated swapchain image or if
-- negatively sized.
--
-- 'OpenXR.Core10.Enums.Result.ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED'
-- /must/ be returned if 'FrameEndInfo'::environmentBlendMode is not
-- supported.
--
-- 'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_RUNNING' /must/ be
-- returned if the @session@
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_not_running is not running>.
--
-- Note
--
-- Applications should discard frames for which 'endFrame' returns a
-- recoverable error over attempting to resubmit the frame with different
-- frame parameters to provide a more consistent experience across
-- different runtime implementations.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_CALL_ORDER_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LAYER_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SWAPCHAIN_RECT_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ENVIRONMENT_BLEND_MODE_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_RUNNING'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LAYER_LIMIT_EXCEEDED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_TIME_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_POSE_INVALID'
--
-- = See Also
--
-- 'FrameEndInfo', 'OpenXR.Core10.Handles.Session', 'beginFrame',
-- 'waitFrame'
endFrame :: forall a io
          . (Extendss FrameEndInfo a, PokeChain a, MonadIO io)
         => -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
            --
            -- #VUID-xrEndFrame-session-parameter# @session@ /must/ be a valid
            -- 'OpenXR.Core10.Handles.Session' handle
            Session
         -> -- | @frameEndInfo@ is a pointer to a valid 'FrameEndInfo'.
            --
            -- #VUID-xrEndFrame-frameEndInfo-parameter# @frameEndInfo@ /must/ be a
            -- pointer to a valid 'FrameEndInfo' structure
            (FrameEndInfo a)
         -> io (Result)
endFrame session frameEndInfo = liftIO . evalContT $ do
  let xrEndFramePtr = pXrEndFrame (instanceCmds (session :: Session))
  lift $ unless (xrEndFramePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEndFrame is null" Nothing Nothing
  let xrEndFrame' = mkXrEndFrame xrEndFramePtr
  frameEndInfo' <- ContT $ withCStruct (frameEndInfo)
  r <- lift $ traceAroundEvent "xrEndFrame" (xrEndFrame' (sessionHandle (session)) (forgetExtensions frameEndInfo'))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrWaitFrameUnsafe
  :: FunPtr (Ptr Session_T -> Ptr FrameWaitInfo -> Ptr (SomeStruct FrameState) -> IO Result) -> Ptr Session_T -> Ptr FrameWaitInfo -> Ptr (SomeStruct FrameState) -> IO Result

foreign import ccall
  "dynamic" mkXrWaitFrameSafe
  :: FunPtr (Ptr Session_T -> Ptr FrameWaitInfo -> Ptr (SomeStruct FrameState) -> IO Result) -> Ptr Session_T -> Ptr FrameWaitInfo -> Ptr (SomeStruct FrameState) -> IO Result

-- | waitFrame with selectable safeness
waitFrameSafeOrUnsafe :: forall a io
                       . (Extendss FrameState a, PokeChain a, PeekChain a, MonadIO io)
                      => (FunPtr (Ptr Session_T -> Ptr FrameWaitInfo -> Ptr (SomeStruct FrameState) -> IO Result) -> Ptr Session_T -> Ptr FrameWaitInfo -> Ptr (SomeStruct FrameState) -> IO Result)
                      -> -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
                         Session
                      -> -- | @frameWaitInfo@ exists for extensibility purposes, it is @NULL@ or a
                         -- pointer to a valid 'FrameWaitInfo'.
                         ("frameWaitInfo" ::: Maybe FrameWaitInfo)
                      -> io (Result, FrameState a)
waitFrameSafeOrUnsafe mkXrWaitFrame session frameWaitInfo = liftIO . evalContT $ do
  let xrWaitFramePtr = pXrWaitFrame (instanceCmds (session :: Session))
  lift $ unless (xrWaitFramePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrWaitFrame is null" Nothing Nothing
  let xrWaitFrame' = mkXrWaitFrame xrWaitFramePtr
  frameWaitInfo' <- case (frameWaitInfo) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pFrameState <- ContT (withZeroCStruct @(FrameState _))
  r <- lift $ traceAroundEvent "xrWaitFrame" (xrWaitFrame' (sessionHandle (session)) frameWaitInfo' (forgetExtensions (pFrameState)))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  frameState <- lift $ peekCStruct @(FrameState _) pFrameState
  pure $ (r, frameState)

-- | xrWaitFrame - Frame timing function
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'waitFrame' throttles the application frame loop in order to synchronize
-- application frame submissions with the display. 'waitFrame' returns a
-- predicted display time for the next time that the runtime predicts a
-- composited frame will be displayed. The runtime /may/ affect this
-- computation by changing the return values and throttling of 'waitFrame'
-- in response to feedback from frame submission and completion times in
-- 'endFrame'. An application /must/ eventually match each 'waitFrame' call
-- with one call to 'beginFrame'. A subsequent 'waitFrame' call /must/
-- block until the previous frame has been begun with 'beginFrame' and
-- /must/ unblock independently of the corresponding call to 'endFrame'.
-- When less than one frame interval has passed since the previous return
-- from 'waitFrame', the runtime /should/ block until the beginning of the
-- next frame interval. If more than one frame interval has passed since
-- the last return from 'waitFrame', the runtime /may/ return immediately
-- or block until the beginning of the next frame interval.
--
-- In the case that an application has pipelined frame submissions, the
-- application /should/ compute the appropriate target display time using
-- both the predicted display time and predicted display interval. The
-- application /should/ use the computed target display time when
-- requesting space and view locations for rendering.
--
-- The 'FrameState'::@predictedDisplayTime@ returned by 'waitFrame' /must/
-- be monotonically increasing.
--
-- The runtime /may/ dynamically adjust the start time of the frame
-- interval relative to the display hardware’s refresh cycle to minimize
-- graphics processor contention between the application and the
-- compositor.
--
-- 'waitFrame' /must/ be callable from any thread, including a different
-- thread than 'beginFrame'\/'endFrame' are being called from.
--
-- Calling 'waitFrame' /must/ be externally synchronized by the
-- application, concurrent calls /may/ result in undefined behavior.
--
-- The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_SESSION_NOT_RUNNING' if the @session@
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_not_running is not running>.
--
-- Note
--
-- The engine simulation /should/ advance based on the display time. Every
-- stage in the engine pipeline should use the exact same display time for
-- one particular application-generated frame. An accurate and consistent
-- display time across all stages and threads in the engine pipeline is
-- important to avoid object motion judder. If the application has multiple
-- pipeline stages, the application should pass its computed display time
-- through its pipeline, as 'waitFrame' must be called only once per frame.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrWaitFrame-session-parameter# @session@ /must/ be a valid
--     'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrWaitFrame-frameWaitInfo-parameter# If @frameWaitInfo@ is not
--     @NULL@, @frameWaitInfo@ /must/ be a pointer to a valid
--     'FrameWaitInfo' structure
--
-- -   #VUID-xrWaitFrame-frameState-parameter# @frameState@ /must/ be a
--     pointer to an 'FrameState' structure
--
-- == Thread Safety
--
-- -   Access to the @session@ parameter by any other 'waitFrame' call
--     /must/ be externally synchronized
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
-- 'FrameState', 'FrameWaitInfo', 'OpenXR.Core10.Handles.Session',
-- 'beginFrame', 'endFrame'
waitFrame :: forall a io
           . (Extendss FrameState a, PokeChain a, PeekChain a, MonadIO io)
          => -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
             Session
          -> -- | @frameWaitInfo@ exists for extensibility purposes, it is @NULL@ or a
             -- pointer to a valid 'FrameWaitInfo'.
             ("frameWaitInfo" ::: Maybe FrameWaitInfo)
          -> io (Result, FrameState a)
waitFrame = waitFrameSafeOrUnsafe mkXrWaitFrameUnsafe

-- | A variant of 'waitFrame' which makes a *safe* FFI call
waitFrameSafe :: forall a io
               . (Extendss FrameState a, PokeChain a, PeekChain a, MonadIO io)
              => -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
                 Session
              -> -- | @frameWaitInfo@ exists for extensibility purposes, it is @NULL@ or a
                 -- pointer to a valid 'FrameWaitInfo'.
                 ("frameWaitInfo" ::: Maybe FrameWaitInfo)
              -> io (Result, FrameState a)
waitFrameSafe = waitFrameSafeOrUnsafe mkXrWaitFrameSafe


-- | XrView - Struct containing view projection state
--
-- == Member Descriptions
--
-- = Description
--
-- The 'View' structure contains view pose and projection state necessary
-- to render a single projection view in the view configuration.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.Fovf', 'OpenXR.Core10.Space.Posef',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'ViewLocateInfo',
-- 'ViewState', 'locateViews'
data View = View
  { -- | @pose@ is an 'OpenXR.Core10.Space.Posef' defining the location and
    -- orientation of the view in the @space@ specified by the 'locateViews'
    -- function.
    pose :: Posef
  , -- | @fov@ is the 'OpenXR.Core10.OtherTypes.Fovf' for the four sides of the
    -- projection.
    fov :: Fovf
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (View)
#endif
deriving instance Show View

instance ToCStruct View where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p View{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Posef)) (pose)
    poke ((p `plusPtr` 44 :: Ptr Fovf)) (fov)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Posef)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Fovf)) (zero)
    f

instance FromCStruct View where
  peekCStruct p = do
    pose <- peekCStruct @Posef ((p `plusPtr` 16 :: Ptr Posef))
    fov <- peekCStruct @Fovf ((p `plusPtr` 44 :: Ptr Fovf))
    pure $ View
             pose fov

instance Storable View where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero View where
  zero = View
           zero
           zero


-- | XrViewLocateInfo - Struct containing view locate information
--
-- == Member Descriptions
--
-- = Description
--
-- The 'ViewLocateInfo' structure contains the display time and space used
-- to locate the view 'View' structures.
--
-- The runtime /must/ return error
-- 'OpenXR.Core10.Enums.Result.ERROR_VIEW_CONFIGURATION_TYPE_UNSUPPORTED'
-- if the given @viewConfigurationType@ is not one of the supported type
-- reported by
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurations'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'View',
-- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType',
-- 'ViewState', 'locateViews'
data ViewLocateInfo = ViewLocateInfo
  { -- | @viewConfigurationType@ is
    -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' to
    -- query for.
    --
    -- #VUID-XrViewLocateInfo-viewConfigurationType-parameter#
    -- @viewConfigurationType@ /must/ be a valid
    -- 'OpenXR.Core10.Enums.ViewConfigurationType.ViewConfigurationType' value
    viewConfigurationType :: ViewConfigurationType
  , -- | @displayTime@ is the time for which the view poses are predicted.
    displayTime :: Time
  , -- | @space@ is the 'OpenXR.Core10.Handles.Space' in which the @pose@ in each
    -- 'View' is expressed.
    --
    -- #VUID-XrViewLocateInfo-space-parameter# @space@ /must/ be a valid
    -- 'OpenXR.Core10.Handles.Space' handle
    space :: Ptr Space_T
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViewLocateInfo)
#endif
deriving instance Show ViewLocateInfo

instance ToCStruct ViewLocateInfo where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ViewLocateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_LOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (viewConfigurationType)
    poke ((p `plusPtr` 24 :: Ptr Time)) (displayTime)
    poke ((p `plusPtr` 32 :: Ptr (Ptr Space_T))) (space)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_LOCATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViewConfigurationType)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Time)) (zero)
    poke ((p `plusPtr` 32 :: Ptr (Ptr Space_T))) (zero)
    f

instance FromCStruct ViewLocateInfo where
  peekCStruct p = do
    viewConfigurationType <- peek @ViewConfigurationType ((p `plusPtr` 16 :: Ptr ViewConfigurationType))
    displayTime <- peek @Time ((p `plusPtr` 24 :: Ptr Time))
    space <- peek @(Ptr Space_T) ((p `plusPtr` 32 :: Ptr (Ptr Space_T)))
    pure $ ViewLocateInfo
             viewConfigurationType displayTime space

instance Storable ViewLocateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ViewLocateInfo where
  zero = ViewLocateInfo
           zero
           zero
           zero


-- | XrViewState - Struct containing additional view state
--
-- == Member Descriptions
--
-- = Description
--
-- The 'ViewState' contains additional view state from 'locateViews' common
-- to all views of the active view configuration.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'View',
-- 'OpenXR.Core10.Enums.ViewStateFlagBits.ViewStateFlags', 'locateViews'
data ViewState = ViewState
  { -- | @viewStateFlags@ is a bitmask of
    -- 'OpenXR.Core10.Enums.ViewStateFlagBits.ViewStateFlagBits' indicating
    -- state for all views.
    --
    -- #VUID-XrViewState-viewStateFlags-parameter# @viewStateFlags@ /must/ be
    -- @0@ or a valid combination of
    -- 'OpenXR.Core10.Enums.ViewStateFlagBits.ViewStateFlagBits' values
    viewStateFlags :: ViewStateFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ViewState)
#endif
deriving instance Show ViewState

instance ToCStruct ViewState where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ViewState{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_STATE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ViewStateFlags)) (viewStateFlags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_VIEW_STATE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ViewState where
  peekCStruct p = do
    viewStateFlags <- peek @ViewStateFlags ((p `plusPtr` 16 :: Ptr ViewStateFlags))
    pure $ ViewState
             viewStateFlags

instance Storable ViewState where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ViewState where
  zero = ViewState
           zero


-- | XrFrameBeginInfo - Begin frame information
--
-- == Member Descriptions
--
-- = Description
--
-- Because this structure only exists to support extension-specific
-- structures, 'beginFrame' will accept a @NULL@ argument for
-- @frameBeginInfo@ for applications that are not using any relevant
-- extensions.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'beginFrame',
-- 'waitFrame'
data FrameBeginInfo = FrameBeginInfo
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FrameBeginInfo)
#endif
deriving instance Show FrameBeginInfo

instance ToCStruct FrameBeginInfo where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FrameBeginInfo f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_FRAME_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_FRAME_BEGIN_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct FrameBeginInfo where
  peekCStruct _ = pure $ FrameBeginInfo
                           

instance Storable FrameBeginInfo where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FrameBeginInfo where
  zero = FrameBeginInfo
           


-- | XrFrameEndInfo - End frame information
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrFrameEndInfo-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_FRAME_END_INFO'
--
-- -   #VUID-XrFrameEndInfo-next-next# @next@ /must/ be @NULL@ or a valid
--     pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
--     See also:
--     'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationFrameEndInfoMSFT'
--
-- -   #VUID-XrFrameEndInfo-environmentBlendMode-parameter#
--     @environmentBlendMode@ /must/ be a valid
--     'OpenXR.Core10.Enums.EnvironmentBlendMode.EnvironmentBlendMode'
--     value
--
-- -   #VUID-XrFrameEndInfo-layers-parameter# If @layerCount@ is not @0@,
--     @layers@ /must/ be a pointer to an array of @layerCount@ valid
--     'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader'-based
--     structures. See also:
--     'OpenXR.Extensions.XR_KHR_composition_layer_cube.CompositionLayerCubeKHR',
--     'OpenXR.Extensions.XR_KHR_composition_layer_cylinder.CompositionLayerCylinderKHR',
--     'OpenXR.Extensions.XR_KHR_composition_layer_equirect2.CompositionLayerEquirect2KHR',
--     'OpenXR.Extensions.XR_KHR_composition_layer_equirect.CompositionLayerEquirectKHR',
--     'OpenXR.Core10.OtherTypes.CompositionLayerProjection',
--     'OpenXR.Core10.OtherTypes.CompositionLayerQuad'
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader',
-- 'OpenXR.Core10.Enums.EnvironmentBlendMode.EnvironmentBlendMode',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'endFrame'
data FrameEndInfo (es :: [Type]) = FrameEndInfo
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    next :: Chain es
  , -- | @displayTime@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- at which this frame /should/ be displayed.
    displayTime :: Time
  , -- | @environmentBlendMode@ is the
    -- 'OpenXR.Core10.Enums.EnvironmentBlendMode.EnvironmentBlendMode' value
    -- representing the desired
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#environment_blend_mode environment blend mode>
    -- for this frame.
    environmentBlendMode :: EnvironmentBlendMode
  , -- | @layerCount@ is the number of composition layers in this frame. The
    -- maximum supported layer count is identified by
    -- 'OpenXR.Core10.Device.SystemGraphicsProperties'::maxLayerCount. If
    -- layerCount is greater than the maximum supported layer count then
    -- 'OpenXR.Core10.Enums.Result.ERROR_LAYER_LIMIT_EXCEEDED' /must/ be
    -- returned.
    layerCount :: Word32
  , -- | @layers@ is a pointer to an array of
    -- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader' pointers.
    layers :: Vector (SomeChild (CompositionLayerBaseHeader '[]))
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FrameEndInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (FrameEndInfo es)

instance Extensible FrameEndInfo where
  extensibleTypeName = "FrameEndInfo"
  setNext x next = x{next = next}
  getNext FrameEndInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends FrameEndInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SecondaryViewConfigurationFrameEndInfoMSFT = Just f
    | otherwise = Nothing

instance (Extendss FrameEndInfo es, PokeChain es) => ToCStruct (FrameEndInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FrameEndInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_FRAME_END_INFO)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr Time)) (displayTime)
    lift $ poke ((p `plusPtr` 24 :: Ptr EnvironmentBlendMode)) (environmentBlendMode)
    let layersLength = Data.Vector.length $ (layers)
    layerCount'' <- lift $ if (layerCount) == 0
      then pure $ fromIntegral layersLength
      else do
        unless (fromIntegral layersLength == (layerCount) || layersLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "layers must be empty or have 'layerCount' elements" Nothing Nothing
        pure (layerCount)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (layerCount'')
    layers'' <- if Data.Vector.null (layers)
      then pure nullPtr
      else do
        pLayers <- ContT $ allocaBytesAligned @(Ptr _) (((Data.Vector.length (layers))) * 8) 8
        Data.Vector.imapM_ (\i e -> do
          layers' <- ContT $ withSomeChild (e)
          lift $ poke (pLayers `plusPtr` (8 * (i)) :: Ptr (Ptr _)) layers') ((layers))
        pure $ pLayers
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (Ptr _)))) layers''
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_FRAME_END_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Time)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr EnvironmentBlendMode)) (zero)
    lift $ f

instance (Extendss FrameEndInfo es, PeekChain es) => FromCStruct (FrameEndInfo es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    displayTime <- peek @Time ((p `plusPtr` 16 :: Ptr Time))
    environmentBlendMode <- peek @EnvironmentBlendMode ((p `plusPtr` 24 :: Ptr EnvironmentBlendMode))
    layerCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    layers <- peek @(Ptr (Ptr _)) ((p `plusPtr` 32 :: Ptr (Ptr (Ptr _))))
    let layersLength = if layers == nullPtr then 0 else (fromIntegral layerCount)
    layers' <- generateM layersLength (\i -> peekSomeCChild =<< peek ((layers `advancePtrBytes` (8 * (i)) :: Ptr (Ptr _))))
    pure $ FrameEndInfo
             next' displayTime environmentBlendMode layerCount layers'

instance es ~ '[] => Zero (FrameEndInfo es) where
  zero = FrameEndInfo
           ()
           zero
           zero
           zero
           mempty


-- | XrFrameWaitInfo - Wait frame information structure
--
-- == Member Descriptions
--
-- = Description
--
-- Because this structure only exists to support extension-specific
-- structures, 'waitFrame' /must/ accept a @NULL@ argument for
-- @frameWaitInfo@ for applications that are not using any relevant
-- extensions.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'FrameState', 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'waitFrame'
data FrameWaitInfo = FrameWaitInfo
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FrameWaitInfo)
#endif
deriving instance Show FrameWaitInfo

instance ToCStruct FrameWaitInfo where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FrameWaitInfo f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_FRAME_WAIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_FRAME_WAIT_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct FrameWaitInfo where
  peekCStruct _ = pure $ FrameWaitInfo
                           

instance Storable FrameWaitInfo where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero FrameWaitInfo where
  zero = FrameWaitInfo
           


-- | XrFrameState - Frame prediction structure
--
-- == Member Descriptions
--
-- = Description
--
-- 'FrameState' describes the time at which the next frame will be
-- displayed to the user. @predictedDisplayTime@ /must/ refer to the
-- midpoint of the interval during which the frame is displayed. The
-- runtime /may/ report a different @predictedDisplayPeriod@ from the
-- hardware’s refresh cycle.
--
-- For any frame where @shouldRender@ is
-- 'OpenXR.Core10.FundamentalTypes.FALSE', the application /should/ avoid
-- heavy GPU work for that frame, for example by not rendering its layers.
-- This typically happens when the application is transitioning into or out
-- of a running session, or when some system UI is fully covering the
-- application at the moment. As long as the session
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session_running is running>,
-- the application /should/ keep running the frame loop to maintain the
-- frame synchronization to the runtime, even if this requires calling
-- 'endFrame' with all layers omitted.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >,
-- 'FrameWaitInfo', 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'waitFrame'
data FrameState (es :: [Type]) = FrameState
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    --
    -- #VUID-XrFrameState-next-next# @next@ /must/ be @NULL@ or a valid pointer
    -- to the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
    -- See also:
    -- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationFrameStateMSFT'
    next :: Chain es
  , -- | @predictedDisplayTime@ is the anticipated display
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- for the next application-generated frame.
    predictedDisplayTime :: Time
  , -- | @predictedDisplayPeriod@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrDuration >
    -- of the display period for the next application-generated frame, for use
    -- in predicting display times beyond the next one.
    predictedDisplayPeriod :: Duration
  , -- | @shouldRender@ is 'OpenXR.Core10.FundamentalTypes.TRUE' if the
    -- application /should/ render its layers as normal and submit them to
    -- 'endFrame'. When this value is 'OpenXR.Core10.FundamentalTypes.FALSE',
    -- the application /should/ avoid heavy GPU work where possible, for
    -- example by skipping layer rendering and then omitting those layers when
    -- calling 'endFrame'.
    shouldRender :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (FrameState (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (FrameState es)

instance Extensible FrameState where
  extensibleTypeName = "FrameState"
  setNext x next = x{next = next}
  getNext FrameState{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends FrameState e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SecondaryViewConfigurationFrameStateMSFT = Just f
    | otherwise = Nothing

instance (Extendss FrameState es, PokeChain es) => ToCStruct (FrameState es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p FrameState{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_FRAME_STATE)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr Time)) (predictedDisplayTime)
    lift $ poke ((p `plusPtr` 24 :: Ptr Duration)) (predictedDisplayPeriod)
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (shouldRender))
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_FRAME_STATE)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Time)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Duration)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance (Extendss FrameState es, PeekChain es) => FromCStruct (FrameState es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    predictedDisplayTime <- peek @Time ((p `plusPtr` 16 :: Ptr Time))
    predictedDisplayPeriod <- peek @Duration ((p `plusPtr` 24 :: Ptr Duration))
    shouldRender <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ FrameState
             next' predictedDisplayTime predictedDisplayPeriod (bool32ToBool shouldRender)

instance es ~ '[] => Zero (FrameState es) where
  zero = FrameState
           ()
           zero
           zero
           zero

