{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_perception_anchor_interop - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_perception_anchor_interop  XR_MSFT_perception_anchor_interop>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 57
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- -   Requires @XR_MSFT_spatial_anchor@
--
-- = See Also
--
-- 'createSpatialAnchorFromPerceptionAnchorMSFT',
-- 'tryGetPerceptionAnchorFromSpatialAnchorMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_perception_anchor_interop OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_perception_anchor_interop  ( createSpatialAnchorFromPerceptionAnchorMSFT
                                                            , withSpatialAnchorFromPerceptionAnchorMSFT
                                                            , tryGetPerceptionAnchorFromSpatialAnchorMSFT
                                                            , MSFT_perception_anchor_interop_SPEC_VERSION
                                                            , pattern MSFT_perception_anchor_interop_SPEC_VERSION
                                                            , MSFT_PERCEPTION_ANCHOR_INTEROP_EXTENSION_NAME
                                                            , pattern MSFT_PERCEPTION_ANCHOR_INTEROP_EXTENSION_NAME
                                                            , IUnknown
                                                            , SpatialAnchorMSFT(..)
                                                            , destroySpatialAnchorMSFT
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
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Foreign.Storable (Storable(peek))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.Extensions.XR_MSFT_spatial_anchor (destroySpatialAnchorMSFT)
import OpenXR.NamedType ((:::))
import OpenXR.Dynamic (InstanceCmds(pXrCreateSpatialAnchorFromPerceptionAnchorMSFT))
import OpenXR.Dynamic (InstanceCmds(pXrTryGetPerceptionAnchorFromSpatialAnchorMSFT))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session(Session))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Extensions.Handles (SpatialAnchorMSFT)
import OpenXR.Extensions.Handles (SpatialAnchorMSFT(..))
import OpenXR.Extensions.Handles (SpatialAnchorMSFT(SpatialAnchorMSFT))
import OpenXR.Extensions.Handles (SpatialAnchorMSFT_T)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Extensions.XR_MSFT_spatial_anchor (destroySpatialAnchorMSFT)
import OpenXR.Extensions.Handles (SpatialAnchorMSFT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateSpatialAnchorFromPerceptionAnchorMSFT
  :: FunPtr (Ptr Session_T -> Ptr IUnknown -> Ptr (Ptr SpatialAnchorMSFT_T) -> IO Result) -> Ptr Session_T -> Ptr IUnknown -> Ptr (Ptr SpatialAnchorMSFT_T) -> IO Result

-- | xrCreateSpatialAnchorFromPerceptionAnchorMSFT - Create a
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT' from a Windows
-- SpatialAnchor pointer
--
-- == Parameter Descriptions
--
-- = Description
--
-- The input @perceptionAnchor@ /must/ support successful @QueryInterface@
-- to
-- <https://docs.microsoft.com/uwp/api/Windows.Perception.Spatial.SpatialAnchor Windows.Perception.Spatial.SpatialAnchor>
-- , otherwise the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'.
--
-- If the function successfully returned, the output @anchor@ /must/ be a
-- valid handle. This also increments the refcount of the
-- @perceptionAnchor@ object.
--
-- When application is done with the @anchor@ handle, it /can/ be destroyed
-- using
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.destroySpatialAnchorMSFT'
-- function. This also decrements the refcount of underlying windows
-- perception anchor object.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrCreateSpatialAnchorFromPerceptionAnchorMSFT-extension-notenabled#
--     The @XR_MSFT_perception_anchor_interop@ extension /must/ be enabled
--     prior to calling 'createSpatialAnchorFromPerceptionAnchorMSFT'
--
-- -   #VUID-xrCreateSpatialAnchorFromPerceptionAnchorMSFT-session-parameter#
--     @session@ /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrCreateSpatialAnchorFromPerceptionAnchorMSFT-perceptionAnchor-parameter#
--     @perceptionAnchor@ /must/ be a pointer to an 'IUnknown' value
--
-- -   #VUID-xrCreateSpatialAnchorFromPerceptionAnchorMSFT-anchor-parameter#
--     @anchor@ /must/ be a pointer to an
--     'OpenXR.Extensions.Handles.SpatialAnchorMSFT' handle
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session',
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT'
createSpatialAnchorFromPerceptionAnchorMSFT :: forall io
                                             . (MonadIO io)
                                            => -- | @session@ is the specified 'OpenXR.Core10.Handles.Session'.
                                               Session
                                            -> -- | @perceptionAnchor@ is an IUnknown pointer to a
                                               -- <https://docs.microsoft.com/uwp/api/Windows.Perception.Spatial.SpatialAnchor Windows.Perception.Spatial.SpatialAnchor>
                                               -- object.
                                               ("perceptionAnchor" ::: Ptr IUnknown)
                                            -> io (SpatialAnchorMSFT)
createSpatialAnchorFromPerceptionAnchorMSFT session perceptionAnchor = liftIO . evalContT $ do
  let cmds = case session of Session{instanceCmds} -> instanceCmds
  let xrCreateSpatialAnchorFromPerceptionAnchorMSFTPtr = pXrCreateSpatialAnchorFromPerceptionAnchorMSFT cmds
  lift $ unless (xrCreateSpatialAnchorFromPerceptionAnchorMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateSpatialAnchorFromPerceptionAnchorMSFT is null" Nothing Nothing
  let xrCreateSpatialAnchorFromPerceptionAnchorMSFT' = mkXrCreateSpatialAnchorFromPerceptionAnchorMSFT xrCreateSpatialAnchorFromPerceptionAnchorMSFTPtr
  pAnchor <- ContT $ bracket (callocBytes @(Ptr SpatialAnchorMSFT_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateSpatialAnchorFromPerceptionAnchorMSFT" (xrCreateSpatialAnchorFromPerceptionAnchorMSFT' (sessionHandle (session)) (perceptionAnchor) (pAnchor))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  anchor <- lift $ peek @(Ptr SpatialAnchorMSFT_T) pAnchor
  pure $ (((\h -> SpatialAnchorMSFT h cmds ) anchor))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSpatialAnchorFromPerceptionAnchorMSFT' and
-- 'destroySpatialAnchorMSFT'
--
-- To ensure that 'destroySpatialAnchorMSFT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSpatialAnchorFromPerceptionAnchorMSFT :: forall io r . MonadIO io => Session -> Ptr IUnknown -> (io SpatialAnchorMSFT -> (SpatialAnchorMSFT -> io ()) -> r) -> r
withSpatialAnchorFromPerceptionAnchorMSFT session perceptionAnchor b =
  b (createSpatialAnchorFromPerceptionAnchorMSFT session perceptionAnchor)
    (\(o0) -> destroySpatialAnchorMSFT o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrTryGetPerceptionAnchorFromSpatialAnchorMSFT
  :: FunPtr (Ptr Session_T -> Ptr SpatialAnchorMSFT_T -> Ptr (Ptr IUnknown) -> IO Result) -> Ptr Session_T -> Ptr SpatialAnchorMSFT_T -> Ptr (Ptr IUnknown) -> IO Result

-- | xrTryGetPerceptionAnchorFromSpatialAnchorMSFT - Convert a
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT' to a Windows SpatialAnchor
--
-- == Parameter Descriptions
--
-- = Description
--
-- If the runtime can convert the @anchor@ to a
-- <https://docs.microsoft.com/uwp/api/Windows.Perception.Spatial.SpatialAnchor Windows.Perception.Spatial.SpatialAnchor>
-- object, this function /must/ return
-- 'OpenXR.Core10.Enums.Result.SUCCESS', and the output 'IUnknown' in the
-- pointer of @perceptionAnchor@ /must/ be not @NULL@. This also increments
-- the refcount of the object. The application /can/ then use
-- @QueryInterface@ to get the pointer for
-- <https://docs.microsoft.com/uwp/api/Windows.Perception.Spatial.SpatialAnchor Windows.Perception.Spatial.SpatialAnchor>
-- object. The application /should/ release the COM pointer after done with
-- the object, or attach it to a smart COM pointer such as
-- @winrt::com_ptr@.
--
-- If the runtime cannot convert the @anchor@ to a
-- <https://docs.microsoft.com/uwp/api/Windows.Perception.Spatial.SpatialAnchor Windows.Perception.Spatial.SpatialAnchor>
-- object, the function /must/ return 'OpenXR.Core10.Enums.Result.SUCCESS',
-- and the output 'IUnknown' in the pointer of @perceptionAnchor@ /must/ be
-- @NULL@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrTryGetPerceptionAnchorFromSpatialAnchorMSFT-extension-notenabled#
--     The @XR_MSFT_perception_anchor_interop@ extension /must/ be enabled
--     prior to calling 'tryGetPerceptionAnchorFromSpatialAnchorMSFT'
--
-- -   #VUID-xrTryGetPerceptionAnchorFromSpatialAnchorMSFT-session-parameter#
--     @session@ /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrTryGetPerceptionAnchorFromSpatialAnchorMSFT-anchor-parameter#
--     @anchor@ /must/ be a valid
--     'OpenXR.Extensions.Handles.SpatialAnchorMSFT' handle
--
-- -   #VUID-xrTryGetPerceptionAnchorFromSpatialAnchorMSFT-perceptionAnchor-parameter#
--     @perceptionAnchor@ /must/ be a pointer to a pointer to an 'IUnknown'
--     value
--
-- -   #VUID-xrTryGetPerceptionAnchorFromSpatialAnchorMSFT-anchor-parent#
--     @anchor@ /must/ have been created, allocated, or retrieved from
--     @session@
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session',
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT'
tryGetPerceptionAnchorFromSpatialAnchorMSFT :: forall io
                                             . (MonadIO io)
                                            => -- | @session@ is the specified 'OpenXR.Core10.Handles.Session'.
                                               Session
                                            -> -- | @anchor@ is a valid 'OpenXR.Extensions.Handles.SpatialAnchorMSFT'
                                               -- handle.
                                               SpatialAnchorMSFT
                                            -> io (("perceptionAnchor" ::: Ptr IUnknown))
tryGetPerceptionAnchorFromSpatialAnchorMSFT session anchor = liftIO . evalContT $ do
  let xrTryGetPerceptionAnchorFromSpatialAnchorMSFTPtr = pXrTryGetPerceptionAnchorFromSpatialAnchorMSFT (case session of Session{instanceCmds} -> instanceCmds)
  lift $ unless (xrTryGetPerceptionAnchorFromSpatialAnchorMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrTryGetPerceptionAnchorFromSpatialAnchorMSFT is null" Nothing Nothing
  let xrTryGetPerceptionAnchorFromSpatialAnchorMSFT' = mkXrTryGetPerceptionAnchorFromSpatialAnchorMSFT xrTryGetPerceptionAnchorFromSpatialAnchorMSFTPtr
  pPerceptionAnchor <- ContT $ bracket (callocBytes @(Ptr IUnknown) 8) free
  r <- lift $ traceAroundEvent "xrTryGetPerceptionAnchorFromSpatialAnchorMSFT" (xrTryGetPerceptionAnchorFromSpatialAnchorMSFT' (sessionHandle (session)) (spatialAnchorMSFTHandle (anchor)) (pPerceptionAnchor))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  perceptionAnchor <- lift $ peek @(Ptr IUnknown) pPerceptionAnchor
  pure $ (perceptionAnchor)


type MSFT_perception_anchor_interop_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_MSFT_perception_anchor_interop_SPEC_VERSION"
pattern MSFT_perception_anchor_interop_SPEC_VERSION :: forall a . Integral a => a
pattern MSFT_perception_anchor_interop_SPEC_VERSION = 1


type MSFT_PERCEPTION_ANCHOR_INTEROP_EXTENSION_NAME = "XR_MSFT_perception_anchor_interop"

-- No documentation found for TopLevel "XR_MSFT_PERCEPTION_ANCHOR_INTEROP_EXTENSION_NAME"
pattern MSFT_PERCEPTION_ANCHOR_INTEROP_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MSFT_PERCEPTION_ANCHOR_INTEROP_EXTENSION_NAME = "XR_MSFT_perception_anchor_interop"


data IUnknown

