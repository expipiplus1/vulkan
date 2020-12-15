{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_spatial_anchor - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_spatial_anchor  XR_MSFT_spatial_anchor>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 40
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'createSpatialAnchorMSFT', 'createSpatialAnchorSpaceMSFT',
-- 'destroySpatialAnchorMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_spatial_anchor OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_spatial_anchor  ( createSpatialAnchorMSFT
                                                 , withSpatialAnchorMSFT
                                                 , createSpatialAnchorSpaceMSFT
                                                 , withSpatialAnchorSpaceMSFT
                                                 , destroySpatialAnchorMSFT
                                                 , SpatialAnchorCreateInfoMSFT(..)
                                                 , SpatialAnchorSpaceCreateInfoMSFT(..)
                                                 , MSFT_spatial_anchor_SPEC_VERSION
                                                 , pattern MSFT_spatial_anchor_SPEC_VERSION
                                                 , MSFT_SPATIAL_ANCHOR_EXTENSION_NAME
                                                 , pattern MSFT_SPATIAL_ANCHOR_EXTENSION_NAME
                                                 , SpatialAnchorMSFT(..)
                                                 ) where

import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
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
import OpenXR.Core10.Space (destroySpace)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.Dynamic (InstanceCmds(pXrCreateSpatialAnchorMSFT))
import OpenXR.Dynamic (InstanceCmds(pXrCreateSpatialAnchorSpaceMSFT))
import OpenXR.Dynamic (InstanceCmds(pXrDestroySpatialAnchorMSFT))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Space (Posef)
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Core10.Handles (Space)
import OpenXR.Core10.Handles (Space(Space))
import OpenXR.Core10.Handles (Space_T)
import OpenXR.Extensions.Handles (SpatialAnchorMSFT)
import OpenXR.Extensions.Handles (SpatialAnchorMSFT(..))
import OpenXR.Extensions.Handles (SpatialAnchorMSFT(SpatialAnchorMSFT))
import OpenXR.Extensions.Handles (SpatialAnchorMSFT_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.FundamentalTypes (Time)
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT))
import OpenXR.Extensions.Handles (SpatialAnchorMSFT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateSpatialAnchorMSFT
  :: FunPtr (Ptr Session_T -> Ptr SpatialAnchorCreateInfoMSFT -> Ptr (Ptr SpatialAnchorMSFT_T) -> IO Result) -> Ptr Session_T -> Ptr SpatialAnchorCreateInfoMSFT -> Ptr (Ptr SpatialAnchorMSFT_T) -> IO Result

-- | xrCreateSpatialAnchorMSFT - Creates a spatial anchor
--
-- == Parameter Descriptions
--
-- = Description
--
-- Creates an 'OpenXR.Extensions.Handles.SpatialAnchorMSFT' handle
-- representing a spatial anchor that will track a fixed location in the
-- physical world over time. That real-world location is specified by the
-- position and orientation of the specified @pose@ within @space@ at
-- @time@.
--
-- If @space@ cannot be located relative to the environment at the moment
-- of the call to 'createSpatialAnchorMSFT', the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT'.
--
-- After the anchor is created, the runtime /should/ then adjust its
-- position and orientation over time relative to other spaces so as to
-- maintain maximum alignment to its original real-world location, even if
-- that changes the anchor’s relationship to the original @space@ used to
-- initialize it.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrCreateSpatialAnchorMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'createSpatialAnchorMSFT'
--
-- -   #VUID-xrCreateSpatialAnchorMSFT-session-parameter# @session@ /must/
--     be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrCreateSpatialAnchorMSFT-createInfo-parameter# @createInfo@
--     /must/ be a pointer to a valid 'SpatialAnchorCreateInfoMSFT'
--     structure
--
-- -   #VUID-xrCreateSpatialAnchorMSFT-anchor-parameter# @anchor@ /must/ be
--     a pointer to an 'OpenXR.Extensions.Handles.SpatialAnchorMSFT' handle
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_POSE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_CREATE_SPATIAL_ANCHOR_FAILED_MSFT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_TIME_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'SpatialAnchorCreateInfoMSFT',
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT'
createSpatialAnchorMSFT :: forall io
                         . (MonadIO io)
                        => -- | @session@ is a handle to an 'OpenXR.Core10.Handles.Session'.
                           Session
                        -> -- | @createInfo@ is a pointer to an 'SpatialAnchorCreateInfoMSFT' structure
                           -- containing information about how to create the anchor.
                           SpatialAnchorCreateInfoMSFT
                        -> io (SpatialAnchorMSFT)
createSpatialAnchorMSFT session createInfo = liftIO . evalContT $ do
  let cmds = instanceCmds (session :: Session)
  let xrCreateSpatialAnchorMSFTPtr = pXrCreateSpatialAnchorMSFT cmds
  lift $ unless (xrCreateSpatialAnchorMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateSpatialAnchorMSFT is null" Nothing Nothing
  let xrCreateSpatialAnchorMSFT' = mkXrCreateSpatialAnchorMSFT xrCreateSpatialAnchorMSFTPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pAnchor <- ContT $ bracket (callocBytes @(Ptr SpatialAnchorMSFT_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateSpatialAnchorMSFT" (xrCreateSpatialAnchorMSFT' (sessionHandle (session)) createInfo' (pAnchor))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  anchor <- lift $ peek @(Ptr SpatialAnchorMSFT_T) pAnchor
  pure $ (((\h -> SpatialAnchorMSFT h cmds ) anchor))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSpatialAnchorMSFT' and 'destroySpatialAnchorMSFT'
--
-- To ensure that 'destroySpatialAnchorMSFT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSpatialAnchorMSFT :: forall io r . MonadIO io => Session -> SpatialAnchorCreateInfoMSFT -> (io SpatialAnchorMSFT -> (SpatialAnchorMSFT -> io ()) -> r) -> r
withSpatialAnchorMSFT session createInfo b =
  b (createSpatialAnchorMSFT session createInfo)
    (\(o0) -> destroySpatialAnchorMSFT o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateSpatialAnchorSpaceMSFT
  :: FunPtr (Ptr Session_T -> Ptr SpatialAnchorSpaceCreateInfoMSFT -> Ptr (Ptr Space_T) -> IO Result) -> Ptr Session_T -> Ptr SpatialAnchorSpaceCreateInfoMSFT -> Ptr (Ptr Space_T) -> IO Result

-- | xrCreateSpatialAnchorSpaceMSFT - Creates a space from a spatial anchor
--
-- == Parameter Descriptions
--
-- = Description
--
-- Creates an 'OpenXR.Core10.Handles.Space' handle based on a spatial
-- anchor. Application /can/ provide an 'OpenXR.Core10.Space.Posef' to
-- define the position and orientation of the new space’s origin relative
-- to the anchor’s natural origin.
--
-- Multiple 'OpenXR.Core10.Handles.Space' handles may exist for a given
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT' simultaneously, up to some
-- limit imposed by the runtime. The 'OpenXR.Core10.Handles.Space' handle
-- must be eventually freed via the 'OpenXR.Core10.Space.destroySpace'
-- function or by destroying the parent
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT' handle.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrCreateSpatialAnchorSpaceMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'createSpatialAnchorSpaceMSFT'
--
-- -   #VUID-xrCreateSpatialAnchorSpaceMSFT-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrCreateSpatialAnchorSpaceMSFT-createInfo-parameter#
--     @createInfo@ /must/ be a pointer to a valid
--     'SpatialAnchorSpaceCreateInfoMSFT' structure
--
-- -   #VUID-xrCreateSpatialAnchorSpaceMSFT-space-parameter# @space@ /must/
--     be a pointer to an 'OpenXR.Core10.Handles.Space' handle
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_POSE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT',
-- 'SpatialAnchorSpaceCreateInfoMSFT'
createSpatialAnchorSpaceMSFT :: forall io
                              . (MonadIO io)
                             => -- | @session@ is a handle to an 'OpenXR.Core10.Handles.Session'.
                                Session
                             -> -- | @createInfo@ is a pointer to an 'SpatialAnchorSpaceCreateInfoMSFT'
                                -- structure containing information about how to create the anchor.
                                SpatialAnchorSpaceCreateInfoMSFT
                             -> io (Space)
createSpatialAnchorSpaceMSFT session createInfo = liftIO . evalContT $ do
  let cmds = instanceCmds (session :: Session)
  let xrCreateSpatialAnchorSpaceMSFTPtr = pXrCreateSpatialAnchorSpaceMSFT cmds
  lift $ unless (xrCreateSpatialAnchorSpaceMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateSpatialAnchorSpaceMSFT is null" Nothing Nothing
  let xrCreateSpatialAnchorSpaceMSFT' = mkXrCreateSpatialAnchorSpaceMSFT xrCreateSpatialAnchorSpaceMSFTPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pSpace <- ContT $ bracket (callocBytes @(Ptr Space_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateSpatialAnchorSpaceMSFT" (xrCreateSpatialAnchorSpaceMSFT' (sessionHandle (session)) createInfo' (pSpace))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  space <- lift $ peek @(Ptr Space_T) pSpace
  pure $ (((\h -> Space h cmds ) space))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createSpatialAnchorSpaceMSFT' and 'destroySpace'
--
-- To ensure that 'destroySpace' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withSpatialAnchorSpaceMSFT :: forall io r . MonadIO io => Session -> SpatialAnchorSpaceCreateInfoMSFT -> (io Space -> (Space -> io ()) -> r) -> r
withSpatialAnchorSpaceMSFT session createInfo b =
  b (createSpatialAnchorSpaceMSFT session createInfo)
    (\(o0) -> destroySpace o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrDestroySpatialAnchorMSFT
  :: FunPtr (Ptr SpatialAnchorMSFT_T -> IO Result) -> Ptr SpatialAnchorMSFT_T -> IO Result

-- | xrDestroySpatialAnchorMSFT - Destroys a spatial anchor
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT' handles are destroyed
-- using 'destroySpatialAnchorMSFT'. By destroying an anchor, the runtime
-- /can/ stop spending resources used to maintain tracking for that
-- anchor’s origin.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrDestroySpatialAnchorMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'destroySpatialAnchorMSFT'
--
-- -   #VUID-xrDestroySpatialAnchorMSFT-anchor-parameter# @anchor@ /must/
--     be a valid 'OpenXR.Extensions.Handles.SpatialAnchorMSFT' handle
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT', 'createSpatialAnchorMSFT'
destroySpatialAnchorMSFT :: forall io
                          . (MonadIO io)
                         => -- | @anchor@ is a handle to an 'OpenXR.Extensions.Handles.SpatialAnchorMSFT'
                            -- previously created by 'createSpatialAnchorMSFT'.
                            SpatialAnchorMSFT
                         -> io ()
destroySpatialAnchorMSFT anchor = liftIO $ do
  let xrDestroySpatialAnchorMSFTPtr = pXrDestroySpatialAnchorMSFT (instanceCmds (anchor :: SpatialAnchorMSFT))
  unless (xrDestroySpatialAnchorMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrDestroySpatialAnchorMSFT is null" Nothing Nothing
  let xrDestroySpatialAnchorMSFT' = mkXrDestroySpatialAnchorMSFT xrDestroySpatialAnchorMSFTPtr
  r <- traceAroundEvent "xrDestroySpatialAnchorMSFT" (xrDestroySpatialAnchorMSFT' (spatialAnchorMSFTHandle (anchor)))
  when (r < SUCCESS) (throwIO (OpenXrException r))


-- | XrSpatialAnchorCreateInfoMSFT - Information to create a spatial anchor
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSpatialAnchorCreateInfoMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'SpatialAnchorCreateInfoMSFT'
--
-- -   #VUID-XrSpatialAnchorCreateInfoMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT'
--
-- -   #VUID-XrSpatialAnchorCreateInfoMSFT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSpatialAnchorCreateInfoMSFT-space-parameter# @space@ /must/
--     be a valid 'OpenXR.Core10.Handles.Space' handle
--
-- = See Also
--
-- 'OpenXR.Core10.Space.Posef', 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'createSpatialAnchorMSFT'
data SpatialAnchorCreateInfoMSFT = SpatialAnchorCreateInfoMSFT
  { -- | @space@ is a handle to the 'OpenXR.Core10.Handles.Space' in which @pose@
    -- is specified.
    space :: Ptr Space_T
  , -- | @pose@ is the 'OpenXR.Core10.Space.Posef' within @space@ at @time@ that
    -- specifies the point in the real world used to initialize the new anchor.
    pose :: Posef
  , -- | @time@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- at which @pose@ will be evaluated within @space@.
    time :: Time
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SpatialAnchorCreateInfoMSFT)
#endif
deriving instance Show SpatialAnchorCreateInfoMSFT

instance ToCStruct SpatialAnchorCreateInfoMSFT where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SpatialAnchorCreateInfoMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Space_T))) (space)
    poke ((p `plusPtr` 24 :: Ptr Posef)) (pose)
    poke ((p `plusPtr` 56 :: Ptr Time)) (time)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SPATIAL_ANCHOR_CREATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Space_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Posef)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Time)) (zero)
    f

instance FromCStruct SpatialAnchorCreateInfoMSFT where
  peekCStruct p = do
    space <- peek @(Ptr Space_T) ((p `plusPtr` 16 :: Ptr (Ptr Space_T)))
    pose <- peekCStruct @Posef ((p `plusPtr` 24 :: Ptr Posef))
    time <- peek @Time ((p `plusPtr` 56 :: Ptr Time))
    pure $ SpatialAnchorCreateInfoMSFT
             space pose time

instance Storable SpatialAnchorCreateInfoMSFT where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SpatialAnchorCreateInfoMSFT where
  zero = SpatialAnchorCreateInfoMSFT
           zero
           zero
           zero


-- | XrSpatialAnchorSpaceCreateInfoMSFT - Information to create a space from
-- a spatial anchor
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSpatialAnchorSpaceCreateInfoMSFT-extension-notenabled# The
--     @@ extension /must/ be enabled prior to using
--     'SpatialAnchorSpaceCreateInfoMSFT'
--
-- -   #VUID-XrSpatialAnchorSpaceCreateInfoMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT'
--
-- -   #VUID-XrSpatialAnchorSpaceCreateInfoMSFT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSpatialAnchorSpaceCreateInfoMSFT-anchor-parameter# @anchor@
--     /must/ be a valid 'OpenXR.Extensions.Handles.SpatialAnchorMSFT'
--     handle
--
-- = See Also
--
-- 'OpenXR.Core10.Space.Posef', 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Extensions.Handles.SpatialAnchorMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'createSpatialAnchorSpaceMSFT'
data SpatialAnchorSpaceCreateInfoMSFT = SpatialAnchorSpaceCreateInfoMSFT
  { -- | @anchor@ is a handle to an 'OpenXR.Extensions.Handles.SpatialAnchorMSFT'
    -- previously created with 'createSpatialAnchorMSFT'.
    anchor :: Ptr SpatialAnchorMSFT_T
  , -- | @poseInAnchorSpace@ is an 'OpenXR.Core10.Space.Posef' defining the
    -- position and orientation of the new space’s origin relative to the
    -- anchor’s natural origin.
    poseInAnchorSpace :: Posef
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SpatialAnchorSpaceCreateInfoMSFT)
#endif
deriving instance Show SpatialAnchorSpaceCreateInfoMSFT

instance ToCStruct SpatialAnchorSpaceCreateInfoMSFT where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SpatialAnchorSpaceCreateInfoMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr SpatialAnchorMSFT_T))) (anchor)
    poke ((p `plusPtr` 24 :: Ptr Posef)) (poseInAnchorSpace)
    f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SPATIAL_ANCHOR_SPACE_CREATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr SpatialAnchorMSFT_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Posef)) (zero)
    f

instance FromCStruct SpatialAnchorSpaceCreateInfoMSFT where
  peekCStruct p = do
    anchor <- peek @(Ptr SpatialAnchorMSFT_T) ((p `plusPtr` 16 :: Ptr (Ptr SpatialAnchorMSFT_T)))
    poseInAnchorSpace <- peekCStruct @Posef ((p `plusPtr` 24 :: Ptr Posef))
    pure $ SpatialAnchorSpaceCreateInfoMSFT
             anchor poseInAnchorSpace

instance Storable SpatialAnchorSpaceCreateInfoMSFT where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SpatialAnchorSpaceCreateInfoMSFT where
  zero = SpatialAnchorSpaceCreateInfoMSFT
           zero
           zero


type MSFT_spatial_anchor_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_MSFT_spatial_anchor_SPEC_VERSION"
pattern MSFT_spatial_anchor_SPEC_VERSION :: forall a . Integral a => a
pattern MSFT_spatial_anchor_SPEC_VERSION = 1


type MSFT_SPATIAL_ANCHOR_EXTENSION_NAME = "XR_MSFT_spatial_anchor"

-- No documentation found for TopLevel "XR_MSFT_SPATIAL_ANCHOR_EXTENSION_NAME"
pattern MSFT_SPATIAL_ANCHOR_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MSFT_SPATIAL_ANCHOR_EXTENSION_NAME = "XR_MSFT_spatial_anchor"

