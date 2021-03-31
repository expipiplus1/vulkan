{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_hand_tracking_mesh - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_hand_tracking_mesh  XR_MSFT_hand_tracking_mesh>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 53
--
-- = Revision
--
-- 2
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- -   Requires @@
--
-- = See Also
--
-- 'HandMeshIndexBufferMSFT', 'HandMeshMSFT',
-- 'HandMeshSpaceCreateInfoMSFT', 'HandMeshUpdateInfoMSFT',
-- 'HandMeshVertexBufferMSFT', 'HandMeshVertexMSFT',
-- 'HandPoseTypeInfoMSFT', 'HandPoseTypeMSFT',
-- 'SystemHandTrackingMeshPropertiesMSFT', 'createHandMeshSpaceMSFT',
-- 'updateHandMeshMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_hand_tracking_mesh OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_hand_tracking_mesh  ( createHandMeshSpaceMSFT
                                                     , withHandMeshSpaceMSFT
                                                     , updateHandMeshMSFT
                                                     , HandMeshSpaceCreateInfoMSFT(..)
                                                     , HandMeshUpdateInfoMSFT(..)
                                                     , HandMeshMSFT(..)
                                                     , HandMeshIndexBufferMSFT(..)
                                                     , HandMeshVertexBufferMSFT(..)
                                                     , HandMeshVertexMSFT(..)
                                                     , SystemHandTrackingMeshPropertiesMSFT(..)
                                                     , HandPoseTypeInfoMSFT(..)
                                                     , HandPoseTypeMSFT( HAND_POSE_TYPE_TRACKED_MSFT
                                                                       , HAND_POSE_TYPE_REFERENCE_OPEN_PALM_MSFT
                                                                       , ..
                                                                       )
                                                     , MSFT_hand_tracking_mesh_SPEC_VERSION
                                                     , pattern MSFT_hand_tracking_mesh_SPEC_VERSION
                                                     , MSFT_HAND_TRACKING_MESH_EXTENSION_NAME
                                                     , pattern MSFT_HAND_TRACKING_MESH_EXTENSION_NAME
                                                     , HandTrackerEXT(..)
                                                     ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero)
import OpenXR.Zero (Zero(..))
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.Core10.FundamentalTypes (bool32ToBool)
import OpenXR.Core10.FundamentalTypes (boolToBool32)
import OpenXR.Core10.Space (destroySpace)
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.Extensions.Handles (HandTrackerEXT)
import OpenXR.Extensions.Handles (HandTrackerEXT(..))
import OpenXR.Extensions.Handles (HandTrackerEXT_T)
import OpenXR.Dynamic (InstanceCmds(pXrCreateHandMeshSpaceMSFT))
import OpenXR.Dynamic (InstanceCmds(pXrUpdateHandMeshMSFT))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Space (Posef)
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Space)
import OpenXR.Core10.Handles (Space(Space))
import OpenXR.Core10.Handles (Space_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.FundamentalTypes (Time)
import OpenXR.Core10.Space (Vector3f)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAND_MESH_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAND_MESH_UPDATE_INFO_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAND_POSE_TYPE_INFO_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT))
import OpenXR.Extensions.Handles (HandTrackerEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateHandMeshSpaceMSFT
  :: FunPtr (Ptr HandTrackerEXT_T -> Ptr HandMeshSpaceCreateInfoMSFT -> Ptr (Ptr Space_T) -> IO Result) -> Ptr HandTrackerEXT_T -> Ptr HandMeshSpaceCreateInfoMSFT -> Ptr (Ptr Space_T) -> IO Result

-- | xrCreateHandMeshSpaceMSFT - Create a space for hand mesh tracking
--
-- == Parameter Descriptions
--
-- = Description
--
-- A hand mesh space location is specified by runtime preference to
-- effectively represent hand mesh vertices without unnecessary
-- transformations. For example, an optical hand tracking system /can/
-- define the hand mesh space origin at the depth camera’s optical center.
--
-- An application should create separate hand mesh space handles for each
-- hand to retrieve the corresponding hand mesh data. The runtime /may/ use
-- the lifetime of this hand mesh space handle to manage the underlying
-- device resources. Therefore, the application /should/ destroy the hand
-- mesh handle after it is finished using the hand mesh.
--
-- The hand mesh space can be related to other spaces in the session, such
-- as view reference space, or grip action space from the
-- \/interaction_profiles\/khr\/simple_controller interaction profile. The
-- hand mesh space may be not locatable when the hand is outside of the
-- tracking range, or if focus is removed from the application. In these
-- cases, the runtime /must/ not set the
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- and
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_ORIENTATION_VALID_BIT'
-- bits on calls to 'OpenXR.Core10.Space.locateSpace' with the hand mesh
-- space, and the application /should/ avoid using the returned poses or
-- query for hand mesh data.
--
-- If the underlying 'OpenXR.Extensions.Handles.HandTrackerEXT' is
-- destroyed, the runtime /must/ continue to support
-- 'OpenXR.Core10.Space.locateSpace' using the hand mesh space, and it
-- /must/ return space location with
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- and
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_ORIENTATION_VALID_BIT'
-- unset.
--
-- The application /may/ create a mesh space for the reference hand by
-- setting @handPoseType@ to 'HAND_POSE_TYPE_REFERENCE_OPEN_PALM_MSFT'.
-- Hand mesh spaces for the reference hand /must/ only be locatable in
-- reference to mesh spaces or joint spaces of the reference hand.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrCreateHandMeshSpaceMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'createHandMeshSpaceMSFT'
--
-- -   #VUID-xrCreateHandMeshSpaceMSFT-handTracker-parameter# @handTracker@
--     /must/ be a valid 'OpenXR.Extensions.Handles.HandTrackerEXT' handle
--
-- -   #VUID-xrCreateHandMeshSpaceMSFT-createInfo-parameter# @createInfo@
--     /must/ be a pointer to a valid 'HandMeshSpaceCreateInfoMSFT'
--     structure
--
-- -   #VUID-xrCreateHandMeshSpaceMSFT-space-parameter# @space@ /must/ be a
--     pointer to an 'OpenXR.Core10.Handles.Space' handle
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_POSE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED'
--
-- = See Also
--
-- 'HandMeshSpaceCreateInfoMSFT',
-- 'OpenXR.Extensions.Handles.HandTrackerEXT',
-- 'OpenXR.Core10.Handles.Space'
createHandMeshSpaceMSFT :: forall io
                         . (MonadIO io)
                        => -- | @handTracker@ is an 'OpenXR.Extensions.Handles.HandTrackerEXT' handle
                           -- previously created with the
                           -- 'OpenXR.Extensions.XR_EXT_hand_tracking.createHandTrackerEXT' function.
                           HandTrackerEXT
                        -> -- | @createInfo@ is the 'HandMeshSpaceCreateInfoMSFT' used to specify the
                           -- hand mesh space.
                           HandMeshSpaceCreateInfoMSFT
                        -> io (Result, Space)
createHandMeshSpaceMSFT handTracker createInfo = liftIO . evalContT $ do
  let cmds = instanceCmds (handTracker :: HandTrackerEXT)
  let xrCreateHandMeshSpaceMSFTPtr = pXrCreateHandMeshSpaceMSFT cmds
  lift $ unless (xrCreateHandMeshSpaceMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateHandMeshSpaceMSFT is null" Nothing Nothing
  let xrCreateHandMeshSpaceMSFT' = mkXrCreateHandMeshSpaceMSFT xrCreateHandMeshSpaceMSFTPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pSpace <- ContT $ bracket (callocBytes @(Ptr Space_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateHandMeshSpaceMSFT" (xrCreateHandMeshSpaceMSFT' (handTrackerEXTHandle (handTracker)) createInfo' (pSpace))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  space <- lift $ peek @(Ptr Space_T) pSpace
  pure $ (r, ((\h -> Space h cmds ) space))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createHandMeshSpaceMSFT' and 'destroySpace'
--
-- To ensure that 'destroySpace' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withHandMeshSpaceMSFT :: forall io r . MonadIO io => HandTrackerEXT -> HandMeshSpaceCreateInfoMSFT -> (io (Result, Space) -> ((Result, Space) -> io ()) -> r) -> r
withHandMeshSpaceMSFT handTracker createInfo b =
  b (createHandMeshSpaceMSFT handTracker createInfo)
    (\(_, o1) -> destroySpace o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrUpdateHandMeshMSFT
  :: FunPtr (Ptr HandTrackerEXT_T -> Ptr HandMeshUpdateInfoMSFT -> Ptr HandMeshMSFT -> IO Result) -> Ptr HandTrackerEXT_T -> Ptr HandMeshUpdateInfoMSFT -> Ptr HandMeshMSFT -> IO Result

-- | xrUpdateHandMeshMSFT - Update hand mesh buffers
--
-- == Parameter Descriptions
--
-- = Description
--
-- The application /should/ preallocate the index buffer and vertex buffer
-- in 'HandMeshMSFT' using the @maxHandMeshIndexCount@ and
-- @maxHandMeshVertexCount@ from the 'SystemHandTrackingMeshPropertiesMSFT'
-- returned from the 'OpenXR.Core10.Device.getSystemProperties' function.
--
-- The application /should/ preallocate the 'HandMeshMSFT' structure and
-- reuse it for each frame so as to reduce the copies of data when
-- underlying tracking data is not changed. The application should use
-- @indexBufferChanged@ and @vertexBufferChanged@ in 'HandMeshMSFT' to
-- detect changes and avoid unnecessary data processing when there is no
-- changes.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrUpdateHandMeshMSFT-extension-notenabled# The @@ extension
--     /must/ be enabled prior to calling 'updateHandMeshMSFT'
--
-- -   #VUID-xrUpdateHandMeshMSFT-handTracker-parameter# @handTracker@
--     /must/ be a valid 'OpenXR.Extensions.Handles.HandTrackerEXT' handle
--
-- -   #VUID-xrUpdateHandMeshMSFT-updateInfo-parameter# @updateInfo@ /must/
--     be a pointer to a valid 'HandMeshUpdateInfoMSFT' structure
--
-- -   #VUID-xrUpdateHandMeshMSFT-handMesh-parameter# @handMesh@ /must/ be
--     a pointer to an 'HandMeshMSFT' structure
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_TIME_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED'
--
-- = See Also
--
-- 'HandMeshMSFT', 'HandMeshUpdateInfoMSFT',
-- 'OpenXR.Extensions.Handles.HandTrackerEXT'
updateHandMeshMSFT :: forall io
                    . (MonadIO io)
                   => -- | @handTracker@ is an 'OpenXR.Extensions.Handles.HandTrackerEXT' handle
                      -- previously created with
                      -- 'OpenXR.Extensions.XR_EXT_hand_tracking.createHandTrackerEXT'.
                      HandTrackerEXT
                   -> -- | @updateInfo@ is a 'HandMeshUpdateInfoMSFT' which contains information to
                      -- query the hand mesh.
                      HandMeshUpdateInfoMSFT
                   -> io (Result, HandMeshMSFT)
updateHandMeshMSFT handTracker updateInfo = liftIO . evalContT $ do
  let xrUpdateHandMeshMSFTPtr = pXrUpdateHandMeshMSFT (instanceCmds (handTracker :: HandTrackerEXT))
  lift $ unless (xrUpdateHandMeshMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrUpdateHandMeshMSFT is null" Nothing Nothing
  let xrUpdateHandMeshMSFT' = mkXrUpdateHandMeshMSFT xrUpdateHandMeshMSFTPtr
  updateInfo' <- ContT $ withCStruct (updateInfo)
  pHandMesh <- ContT (withZeroCStruct @HandMeshMSFT)
  r <- lift $ traceAroundEvent "xrUpdateHandMeshMSFT" (xrUpdateHandMeshMSFT' (handTrackerEXTHandle (handTracker)) updateInfo' (pHandMesh))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  handMesh <- lift $ peekCStruct @HandMeshMSFT pHandMesh
  pure $ (r, handMesh)


-- | XrHandMeshSpaceCreateInfoMSFT - The information to create a hand mesh
-- space
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandMeshSpaceCreateInfoMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'HandMeshSpaceCreateInfoMSFT'
--
-- -   #VUID-XrHandMeshSpaceCreateInfoMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT'
--
-- -   #VUID-XrHandMeshSpaceCreateInfoMSFT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrHandMeshSpaceCreateInfoMSFT-handPoseType-parameter#
--     @handPoseType@ /must/ be a valid 'HandPoseTypeMSFT' value
--
-- = See Also
--
-- 'HandPoseTypeMSFT', 'OpenXR.Core10.Space.Posef',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'createHandMeshSpaceMSFT'
data HandMeshSpaceCreateInfoMSFT = HandMeshSpaceCreateInfoMSFT
  { -- | @handPoseType@ is an 'HandPoseTypeMSFT' used to specify the type of hand
    -- this mesh is tracking. Indices and vertices returned from
    -- 'updateHandMeshMSFT' for a hand type will be relative to the
    -- corresponding space create with the same hand type.
    handPoseType :: HandPoseTypeMSFT
  , -- | @poseInHandMeshSpace@ is an 'OpenXR.Core10.Space.Posef' defining the
    -- position and orientation of the new space’s origin within the natural
    -- reference frame of the hand mesh space.
    poseInHandMeshSpace :: Posef
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandMeshSpaceCreateInfoMSFT)
#endif
deriving instance Show HandMeshSpaceCreateInfoMSFT

instance ToCStruct HandMeshSpaceCreateInfoMSFT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandMeshSpaceCreateInfoMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HandPoseTypeMSFT)) (handPoseType)
    poke ((p `plusPtr` 20 :: Ptr Posef)) (poseInHandMeshSpace)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_MESH_SPACE_CREATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HandPoseTypeMSFT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Posef)) (zero)
    f

instance FromCStruct HandMeshSpaceCreateInfoMSFT where
  peekCStruct p = do
    handPoseType <- peek @HandPoseTypeMSFT ((p `plusPtr` 16 :: Ptr HandPoseTypeMSFT))
    poseInHandMeshSpace <- peekCStruct @Posef ((p `plusPtr` 20 :: Ptr Posef))
    pure $ HandMeshSpaceCreateInfoMSFT
             handPoseType poseInHandMeshSpace

instance Storable HandMeshSpaceCreateInfoMSFT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandMeshSpaceCreateInfoMSFT where
  zero = HandMeshSpaceCreateInfoMSFT
           zero
           zero


-- | XrHandMeshUpdateInfoMSFT - The information to update a hand mesh
--
-- == Member Descriptions
--
-- = Description
--
-- A runtime /may/ not maintain a full history of hand mesh data, therefore
-- the returned 'HandMeshMSFT' might return data that’s not exactly
-- corresponding to the @time@ input. If the runtime cannot return any
-- tracking data for the given @time@ at all, it /must/ set @isActive@ to
-- 'OpenXR.Core10.FundamentalTypes.FALSE' for the call to
-- 'updateHandMeshMSFT'. Otherwise, if the runtime returns @isActive@ as
-- 'OpenXR.Core10.FundamentalTypes.TRUE', the data in 'HandMeshMSFT' must
-- be valid to use.
--
-- An application can choose different @handPoseType@ values to query the
-- hand mesh data. The returned hand mesh /must/ be consistent to the hand
-- joint space location on the same
-- 'OpenXR.Extensions.Handles.HandTrackerEXT' when using the same
-- 'HandPoseTypeMSFT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandMeshUpdateInfoMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using 'HandMeshUpdateInfoMSFT'
--
-- -   #VUID-XrHandMeshUpdateInfoMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_HAND_MESH_UPDATE_INFO_MSFT'
--
-- -   #VUID-XrHandMeshUpdateInfoMSFT-next-next# @next@ /must/ be @NULL@ or
--     a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrHandMeshUpdateInfoMSFT-handPoseType-parameter#
--     @handPoseType@ /must/ be a valid 'HandPoseTypeMSFT' value
--
-- = See Also
--
-- 'HandPoseTypeMSFT', 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'updateHandMeshMSFT'
data HandMeshUpdateInfoMSFT = HandMeshUpdateInfoMSFT
  { -- | @time@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- that describes the time for which the application wishes to query the
    -- hand mesh state.
    time :: Time
  , -- | @handPoseType@ is an 'HandPoseTypeMSFT' which describes the type of hand
    -- pose of the hand mesh to update.
    handPoseType :: HandPoseTypeMSFT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandMeshUpdateInfoMSFT)
#endif
deriving instance Show HandMeshUpdateInfoMSFT

instance ToCStruct HandMeshUpdateInfoMSFT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandMeshUpdateInfoMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_MESH_UPDATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Time)) (time)
    poke ((p `plusPtr` 24 :: Ptr HandPoseTypeMSFT)) (handPoseType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_MESH_UPDATE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Time)) (zero)
    poke ((p `plusPtr` 24 :: Ptr HandPoseTypeMSFT)) (zero)
    f

instance FromCStruct HandMeshUpdateInfoMSFT where
  peekCStruct p = do
    time <- peek @Time ((p `plusPtr` 16 :: Ptr Time))
    handPoseType <- peek @HandPoseTypeMSFT ((p `plusPtr` 24 :: Ptr HandPoseTypeMSFT))
    pure $ HandMeshUpdateInfoMSFT
             time handPoseType

instance Storable HandMeshUpdateInfoMSFT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandMeshUpdateInfoMSFT where
  zero = HandMeshUpdateInfoMSFT
           zero
           zero


-- | XrHandMeshMSFT - The data of a hand mesh
--
-- == Member Descriptions
--
-- = Description
--
-- When the returned @isActive@ value is
-- 'OpenXR.Core10.FundamentalTypes.FALSE', the runtime indicates the hand
-- is not actively tracked, for example, the hand is outside of sensor’s
-- range, or the input focus is taken away from the application. When the
-- runtime returns 'OpenXR.Core10.FundamentalTypes.FALSE' to @isActive@, it
-- /must/ set @indexBufferChanged@ and @vertexBufferChanged@ to
-- 'OpenXR.Core10.FundamentalTypes.FALSE', and /must/ not change the
-- content in @indexBuffer@ or @vertexBuffer@,
--
-- When the returned @isActive@ value is
-- 'OpenXR.Core10.FundamentalTypes.TRUE', the hand tracking mesh
-- represented in @indexBuffer@ and @vertexBuffer@ are updated to the
-- latest data of the @time@ given to the 'updateHandMeshMSFT' function.
-- The runtime /must/ set @indexBufferChanged@ and @vertexBufferChanged@ to
-- reflect whether the index or vertex buffer’s content are changed during
-- the update. In this way, the application can easily avoid unnecessary
-- processing of buffers when there’s no new data.
--
-- The hand mesh is represented in triangle lists and each triangle’s
-- vertices are in counter-clockwise order when looking from outside of the
-- hand. When hand tracking is active, i.e. when @isActive@ is returned as
-- 'OpenXR.Core10.FundamentalTypes.TRUE', the returned
-- @indexBuffer.indexCountOutput@ value /must/ be positive and multiple of
-- 3, and @vertexBuffer.vertexCountOutput@ value /must/ be equal to or
-- larger than 3.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandMeshMSFT-extension-notenabled# The @@ extension /must/
--     be enabled prior to using 'HandMeshMSFT'
--
-- -   #VUID-XrHandMeshMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_HAND_MESH_MSFT'
--
-- -   #VUID-XrHandMeshMSFT-next-next# @next@ /must/ be @NULL@ or a valid
--     pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrHandMeshMSFT-indexBuffer-parameter# @indexBuffer@ /must/ be
--     a valid 'HandMeshIndexBufferMSFT' structure
--
-- -   #VUID-XrHandMeshMSFT-vertexBuffer-parameter# @vertexBuffer@ /must/
--     be a valid 'HandMeshVertexBufferMSFT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'HandMeshIndexBufferMSFT', 'HandMeshVertexBufferMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'updateHandMeshMSFT'
data HandMeshMSFT = HandMeshMSFT
  { -- | @isActive@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >
    -- indicating if the current hand tracker is active.
    isActive :: Bool
  , -- | @indexBufferChanged@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >
    -- indicating if the @indexBuffer@ content was changed during the update.
    indexBufferChanged :: Bool
  , -- | @vertexBufferChanged@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >
    -- indicating if the @vertexBuffer@ content was changed during the update.
    vertexBufferChanged :: Bool
  , -- | @indexBuffer@ is an 'HandMeshIndexBufferMSFT' returns the index buffer
    -- of the tracked hand mesh.
    indexBuffer :: HandMeshIndexBufferMSFT
  , -- | @vertexBuffer@ is an 'HandMeshVertexBufferMSFT' returns the vertex
    -- buffer of the tracked hand mesh.
    vertexBuffer :: HandMeshVertexBufferMSFT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandMeshMSFT)
#endif
deriving instance Show HandMeshMSFT

instance ToCStruct HandMeshMSFT where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandMeshMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_MESH_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (isActive))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (indexBufferChanged))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (vertexBufferChanged))
    poke ((p `plusPtr` 32 :: Ptr HandMeshIndexBufferMSFT)) (indexBuffer)
    poke ((p `plusPtr` 56 :: Ptr HandMeshVertexBufferMSFT)) (vertexBuffer)
    f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_MESH_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr HandMeshIndexBufferMSFT)) (zero)
    poke ((p `plusPtr` 56 :: Ptr HandMeshVertexBufferMSFT)) (zero)
    f

instance FromCStruct HandMeshMSFT where
  peekCStruct p = do
    isActive <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    indexBufferChanged <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    vertexBufferChanged <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    indexBuffer <- peekCStruct @HandMeshIndexBufferMSFT ((p `plusPtr` 32 :: Ptr HandMeshIndexBufferMSFT))
    vertexBuffer <- peekCStruct @HandMeshVertexBufferMSFT ((p `plusPtr` 56 :: Ptr HandMeshVertexBufferMSFT))
    pure $ HandMeshMSFT
             (bool32ToBool isActive) (bool32ToBool indexBufferChanged) (bool32ToBool vertexBufferChanged) indexBuffer vertexBuffer

instance Storable HandMeshMSFT where
  sizeOf ~_ = 80
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandMeshMSFT where
  zero = HandMeshMSFT
           zero
           zero
           zero
           zero
           zero


-- | XrHandMeshIndexBufferMSFT - The index buffer of a hand mesh
--
-- == Member Descriptions
--
-- = Description
--
-- An application /should/ preallocate the indices array using the
-- @maxHandMeshIndexCount@ in 'SystemHandTrackingMeshPropertiesMSFT'
-- returned from 'OpenXR.Core10.Device.getSystemProperties'. In this way,
-- the application can avoid possible insufficient buffer sizees for each
-- query, and therefore avoid reallocating memory each frame.
--
-- The input @indexCapacityInput@ /must/ not be 0, and @indices@ /must/ not
-- be @NULL@, or else the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE' on calls to the
-- 'updateHandMeshMSFT' function.
--
-- If the input @indexCapacityInput@ is not sufficient to contain all
-- output indices, the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT' on calls to
-- 'updateHandMeshMSFT', not change the content in @indexBufferKey@ and
-- @indices@, and return 0 for @indexCountOutput@.
--
-- If the input @indexCapacityInput@ is equal to or larger than the
-- @maxHandMeshIndexCount@ in 'SystemHandTrackingMeshPropertiesMSFT'
-- returned from 'OpenXR.Core10.Device.getSystemProperties', the runtime
-- /must/ not return 'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
-- error on 'updateHandMeshMSFT' because of insufficient index buffer size.
--
-- If the input @indexBufferKey@ is 0, the capacity of indices array is
-- sufficient, and hand mesh tracking is active, the runtime /must/ return
-- the latest non-zero @indexBufferKey@, and fill in @indexCountOutput@ and
-- @indices@.
--
-- If the input @indexBufferKey@ is not 0, the runtime /can/ either return
-- without changing @indexCountOutput@ or content in @indices@, and return
-- 'OpenXR.Core10.FundamentalTypes.FALSE' for @indexBufferChanged@
-- indicating the indices are not changed; or return a new non-zero
-- @indexBufferKey@ and fill in latest data in @indexCountOutput@ and
-- @indices@, and return 'OpenXR.Core10.FundamentalTypes.TRUE' for
-- @indexBufferChanged@ indicating the indices are updated to a newer
-- version.
--
-- An application /can/ keep the 'HandMeshIndexBufferMSFT' structure for
-- each frame in a frame loop and use the returned @indexBufferKey@ to
-- identify different triangle list topology described in @indices@. The
-- application can therefore avoid unnecessary processing of indices, such
-- as coping them to GPU memory.
--
-- The runtime /must/ return the same @indexBufferKey@ for the same
-- 'OpenXR.Extensions.Handles.HandTrackerEXT' at a given time, regardless
-- of the input 'HandPoseTypeMSFT' in 'HandMeshUpdateInfoMSFT'. This
-- ensures the index buffer has the same mesh topology and allows the
-- application to reason about vertices across different hand pose types.
-- For example, the application /can/ build a procedure to perform UV
-- mapping on vertices of a hand mesh using
-- 'HAND_POSE_TYPE_REFERENCE_OPEN_PALM_MSFT', and apply the resultant UV
-- data on vertices to the mesh returned from the same hand tracker using
-- 'HAND_POSE_TYPE_TRACKED_MSFT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandMeshIndexBufferMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using 'HandMeshIndexBufferMSFT'
--
-- -   #VUID-XrHandMeshIndexBufferMSFT-indices-parameter# @indices@ /must/
--     be a pointer to an array of @indexCapacityInput@ @uint32_t@ values
--
-- -   #VUID-XrHandMeshIndexBufferMSFT-indexCapacityInput-arraylength# The
--     @indexCapacityInput@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- 'HandMeshMSFT'
data HandMeshIndexBufferMSFT = HandMeshIndexBufferMSFT
  { -- | @indexBufferKey@ is a @uint32_t@ serving as the key of the returned
    -- index buffer content or 0 to indicate a request to retrieve the latest
    -- indices regardless of existing content in @indices@.
    indexBufferKey :: Word32
  , -- | @indexCapacityInput@ is a positive @uint32_t@ describes the capacity of
    -- the @indices@ array.
    indexCapacityInput :: Word32
  , -- | @indexCountOutput@ is a @uint32_t@ returned by the runtime with the
    -- count of indices written in @indices@.
    indexCountOutput :: Word32
  , -- | @indices@ is an array of indices filled in by the runtime, specifying
    -- the indices of the triangles list in the vertex buffer.
    indices :: Ptr Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandMeshIndexBufferMSFT)
#endif
deriving instance Show HandMeshIndexBufferMSFT

instance ToCStruct HandMeshIndexBufferMSFT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandMeshIndexBufferMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (indexBufferKey)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (indexCapacityInput)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (indexCountOutput)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Word32))) (indices)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Word32))) (zero)
    f

instance FromCStruct HandMeshIndexBufferMSFT where
  peekCStruct p = do
    indexBufferKey <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    indexCapacityInput <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    indexCountOutput <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    indices <- peek @(Ptr Word32) ((p `plusPtr` 16 :: Ptr (Ptr Word32)))
    pure $ HandMeshIndexBufferMSFT
             indexBufferKey indexCapacityInput indexCountOutput indices

instance Storable HandMeshIndexBufferMSFT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandMeshIndexBufferMSFT where
  zero = HandMeshIndexBufferMSFT
           zero
           zero
           zero
           zero


-- | XrHandMeshVertexBufferMSFT - The vertex buffer of a hand mesh
--
-- == Member Descriptions
--
-- = Description
--
-- An application /should/ preallocate the vertices array using the
-- @maxHandMeshVertexCount@ in 'SystemHandTrackingMeshPropertiesMSFT'
-- returned from 'OpenXR.Core10.Device.getSystemProperties'. In this way,
-- the application can avoid possible insufficient buffer sizes for each
-- query, and therefore avoid reallocating memory each frame.
--
-- The input @vertexCapacityInput@ /must/ not be 0, and @vertices@ /must/
-- not be @NULL@, or else the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE' on calls to the
-- 'updateHandMeshMSFT' function.
--
-- If the input @vertexCapacityInput@ is not sufficient to contain all
-- output vertices, the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT' on calls to the
-- 'updateHandMeshMSFT', do not change content in @vertexUpdateTime@ and
-- @vertices@, and return 0 for @vertexCountOutput@.
--
-- If the input @vertexCapacityInput@ is equal to or larger than the
-- @maxHandMeshVertexCount@ in 'SystemHandTrackingMeshPropertiesMSFT'
-- returned from 'OpenXR.Core10.Device.getSystemProperties', the runtime
-- /must/ not return 'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
-- on calls to the 'updateHandMeshMSFT' because of insufficient vertex
-- buffer size.
--
-- If the input @vertexUpdateTime@ is 0, and the capacity of the vertices
-- array is sufficient, and hand mesh tracking is active, the runtime
-- /must/ return the latest non-zero @vertexUpdateTime@, and fill in the
-- @vertexCountOutput@ and @vertices@ fields.
--
-- If the input @vertexUpdateTime@ is not 0, the runtime /can/ either
-- return without changing @vertexCountOutput@ or the content in
-- @vertices@, and return 'OpenXR.Core10.FundamentalTypes.FALSE' for
-- @vertexBufferChanged@ indicating the vertices are not changed; or return
-- a new non-zero @vertexUpdateTime@ and fill in latest data in
-- @vertexCountOutput@ and @vertices@ and return
-- 'OpenXR.Core10.FundamentalTypes.TRUE' for @vertexBufferChanged@
-- indicating the vertices are updated to a newer version.
--
-- An application /can/ keep the 'HandMeshVertexBufferMSFT' structure for
-- each frame in frame loop and use the returned @vertexUpdateTime@ to
-- detect the changes of the content in @vertices@. The application can
-- therefore avoid unnecessary processing of vertices, such as coping them
-- to GPU memory.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandMeshVertexBufferMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'HandMeshVertexBufferMSFT'
--
-- -   #VUID-XrHandMeshVertexBufferMSFT-vertices-parameter# @vertices@
--     /must/ be a pointer to an array of @vertexCapacityInput@
--     'HandMeshVertexMSFT' structures
--
-- -   #VUID-XrHandMeshVertexBufferMSFT-vertexCapacityInput-arraylength#
--     The @vertexCapacityInput@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- 'HandMeshMSFT', 'HandMeshVertexMSFT',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
data HandMeshVertexBufferMSFT = HandMeshVertexBufferMSFT
  { -- | @vertexUpdateTime@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- representing the time when the runtime receives the vertex buffer
    -- content or 0 to indicate a request to retrieve latest vertices
    -- regardless of existing content in @vertices@.
    vertexUpdateTime :: Time
  , -- | @vertexCapacityInput@ is a positive @uint32_t@ describes the capacity of
    -- the @vertices@ array.
    vertexCapacityInput :: Word32
  , -- | @vertexCountOutput@ is a @uint32_t@ filled in by the runtime with the
    -- count of vertices written in @vertices@.
    vertexCountOutput :: Word32
  , -- | @vertices@ is an array of 'HandMeshVertexMSFT' filled in by the runtime,
    -- specifying the vertices of the hand mesh including the position and
    -- normal vector in the hand mesh space.
    vertices :: Ptr HandMeshVertexMSFT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandMeshVertexBufferMSFT)
#endif
deriving instance Show HandMeshVertexBufferMSFT

instance ToCStruct HandMeshVertexBufferMSFT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandMeshVertexBufferMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Time)) (vertexUpdateTime)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (vertexCapacityInput)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (vertexCountOutput)
    poke ((p `plusPtr` 16 :: Ptr (Ptr HandMeshVertexMSFT))) (vertices)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr (Ptr HandMeshVertexMSFT))) (zero)
    f

instance FromCStruct HandMeshVertexBufferMSFT where
  peekCStruct p = do
    vertexUpdateTime <- peek @Time ((p `plusPtr` 0 :: Ptr Time))
    vertexCapacityInput <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    vertexCountOutput <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    vertices <- peek @(Ptr HandMeshVertexMSFT) ((p `plusPtr` 16 :: Ptr (Ptr HandMeshVertexMSFT)))
    pure $ HandMeshVertexBufferMSFT
             vertexUpdateTime vertexCapacityInput vertexCountOutput vertices

instance Storable HandMeshVertexBufferMSFT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandMeshVertexBufferMSFT where
  zero = HandMeshVertexBufferMSFT
           zero
           zero
           zero
           zero


-- | XrHandMeshVertexMSFT - The vertex of hand mesh
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandMeshVertexMSFT-extension-notenabled# The @@ extension
--     /must/ be enabled prior to using 'HandMeshVertexMSFT'
--
-- = See Also
--
-- 'HandMeshVertexBufferMSFT', 'OpenXR.Core10.Space.Vector3f'
data HandMeshVertexMSFT = HandMeshVertexMSFT
  { -- | @position@ is an 'OpenXR.Core10.Space.Vector3f' structure representing
    -- the position of the vertex in the hand mesh space, measured in meters.
    position :: Vector3f
  , -- | @normal@ is an 'OpenXR.Core10.Space.Vector3f' structure representing the
    -- unweighted normal of the triangle surface at the vertex as a unit vector
    -- in hand mesh space.
    normal :: Vector3f
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandMeshVertexMSFT)
#endif
deriving instance Show HandMeshVertexMSFT

instance ToCStruct HandMeshVertexMSFT where
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandMeshVertexMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Vector3f)) (position)
    poke ((p `plusPtr` 12 :: Ptr Vector3f)) (normal)
    f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Vector3f)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Vector3f)) (zero)
    f

instance FromCStruct HandMeshVertexMSFT where
  peekCStruct p = do
    position <- peekCStruct @Vector3f ((p `plusPtr` 0 :: Ptr Vector3f))
    normal <- peekCStruct @Vector3f ((p `plusPtr` 12 :: Ptr Vector3f))
    pure $ HandMeshVertexMSFT
             position normal

instance Storable HandMeshVertexMSFT where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandMeshVertexMSFT where
  zero = HandMeshVertexMSFT
           zero
           zero


-- | XrSystemHandTrackingMeshPropertiesMSFT - System property for hand
-- tracking mesh
--
-- == Member Descriptions
--
-- = Description
--
-- If a runtime returns 'OpenXR.Core10.FundamentalTypes.FALSE' for
-- @supportsHandTrackingMesh@, the system does not support hand tracking
-- mesh input, and therefore /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED' from
-- 'createHandMeshSpaceMSFT' and 'updateHandMeshMSFT'. The application
-- /should/ avoid using hand mesh functionality when
-- @supportsHandTrackingMesh@ is 'OpenXR.Core10.FundamentalTypes.FALSE'.
--
-- If a runtime returns 'OpenXR.Core10.FundamentalTypes.TRUE' for
-- @supportsHandTrackingMesh@, the system supports hand tracking mesh
-- input. In this case, the runtime /must/ return a positive number for
-- @maxHandMeshIndexCount@ and @maxHandMeshVertexCount@. An application
-- /should/ use @maxHandMeshIndexCount@ and @maxHandMeshVertexCount@ to
-- preallocate hand mesh buffers and reuse them in their render loop when
-- calling 'updateHandMeshMSFT' every frame.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSystemHandTrackingMeshPropertiesMSFT-extension-notenabled#
--     The @@ extension /must/ be enabled prior to using
--     'SystemHandTrackingMeshPropertiesMSFT'
--
-- -   #VUID-XrSystemHandTrackingMeshPropertiesMSFT-type-type# @type@
--     /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT'
--
-- -   #VUID-XrSystemHandTrackingMeshPropertiesMSFT-next-next# @next@
--     /must/ be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data SystemHandTrackingMeshPropertiesMSFT = SystemHandTrackingMeshPropertiesMSFT
  { -- | @supportsHandTrackingMesh@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
    -- indicating if current system is capable of hand tracking mesh input.
    supportsHandTrackingMesh :: Bool
  , -- | @maxHandMeshIndexCount@ is a @uint32_t@ returns the maximum count of
    -- indices that will be returned from the hand tracker.
    maxHandMeshIndexCount :: Word32
  , -- | @maxHandMeshVertexCount@ is a @uint32_t@ returns the maximum count of
    -- vertices that will be returned from the hand tracker.
    maxHandMeshVertexCount :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SystemHandTrackingMeshPropertiesMSFT)
#endif
deriving instance Show SystemHandTrackingMeshPropertiesMSFT

instance ToCStruct SystemHandTrackingMeshPropertiesMSFT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SystemHandTrackingMeshPropertiesMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (supportsHandTrackingMesh))
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxHandMeshIndexCount)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxHandMeshVertexCount)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_HAND_TRACKING_MESH_PROPERTIES_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct SystemHandTrackingMeshPropertiesMSFT where
  peekCStruct p = do
    supportsHandTrackingMesh <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    maxHandMeshIndexCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxHandMeshVertexCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ SystemHandTrackingMeshPropertiesMSFT
             (bool32ToBool supportsHandTrackingMesh) maxHandMeshIndexCount maxHandMeshVertexCount

instance Storable SystemHandTrackingMeshPropertiesMSFT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SystemHandTrackingMeshPropertiesMSFT where
  zero = SystemHandTrackingMeshPropertiesMSFT
           zero
           zero
           zero


-- | XrHandPoseTypeInfoMSFT - Describes what hand pose type for the hand
-- joint tracking.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandPoseTypeInfoMSFT-extension-notenabled# The @@ extension
--     /must/ be enabled prior to using 'HandPoseTypeInfoMSFT'
--
-- -   #VUID-XrHandPoseTypeInfoMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_HAND_POSE_TYPE_INFO_MSFT'
--
-- -   #VUID-XrHandPoseTypeInfoMSFT-next-next# @next@ /must/ be @NULL@ or a
--     valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrHandPoseTypeInfoMSFT-handPoseType-parameter# @handPoseType@
--     /must/ be a valid 'HandPoseTypeMSFT' value
--
-- = See Also
--
-- 'HandPoseTypeMSFT', 'OpenXR.Core10.Enums.StructureType.StructureType'
data HandPoseTypeInfoMSFT = HandPoseTypeInfoMSFT
  { -- | @handPoseType@ is an 'HandPoseTypeMSFT' that describes the type of hand
    -- pose of the hand tracking.
    handPoseType :: HandPoseTypeMSFT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandPoseTypeInfoMSFT)
#endif
deriving instance Show HandPoseTypeInfoMSFT

instance ToCStruct HandPoseTypeInfoMSFT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandPoseTypeInfoMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_POSE_TYPE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HandPoseTypeMSFT)) (handPoseType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_POSE_TYPE_INFO_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr HandPoseTypeMSFT)) (zero)
    f

instance FromCStruct HandPoseTypeInfoMSFT where
  peekCStruct p = do
    handPoseType <- peek @HandPoseTypeMSFT ((p `plusPtr` 16 :: Ptr HandPoseTypeMSFT))
    pure $ HandPoseTypeInfoMSFT
             handPoseType

instance Storable HandPoseTypeInfoMSFT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandPoseTypeInfoMSFT where
  zero = HandPoseTypeInfoMSFT
           zero


-- | XrHandPoseTypeMSFT - Describe type of input hand pose
--
-- == Enumerant Descriptions
--
-- The 'HAND_POSE_TYPE_TRACKED_MSFT' input provides best fidelity to the
-- user’s actual hand motion. When the hand tracking input requires the
-- user to be holding a controller in their hand, the hand tracking input
-- will appear as the user virtually holding the controller. This input can
-- be used to render the hand shape together with the controller in hand.
--
-- The 'HAND_POSE_TYPE_REFERENCE_OPEN_PALM_MSFT' input does not move with
-- the user’s actual hand. Through this reference hand pose, an application
-- /can/ get a stable hand joint and mesh that has the same mesh topology
-- as the tracked hand mesh using the same
-- 'OpenXR.Extensions.Handles.HandTrackerEXT', so that the application can
-- apply the data computed from a reference hand pose to the corresponding
-- tracked hand.
--
-- Although a reference hand pose does not move with user’s hand motion,
-- the bone length and hand thickness /may/ be updated, for example when
-- tracking result refines, or a different user’s hand is detected. The
-- application /should/ update reference hand joints and meshes when the
-- tracked mesh’s @indexBufferKey@ is changed or when the @isActive@ value
-- returned from 'updateHandMeshMSFT' changes from
-- 'OpenXR.Core10.FundamentalTypes.FALSE' to
-- 'OpenXR.Core10.FundamentalTypes.TRUE'. It can use the returned
-- @indexBufferKey@ and @vertexUpdateTime@ from 'updateHandMeshMSFT' to
-- avoid unnecessary CPU or GPU work to process the neutral hand inputs.
--
-- = See Also
--
-- 'HandMeshSpaceCreateInfoMSFT', 'HandMeshUpdateInfoMSFT',
-- 'HandPoseTypeInfoMSFT'
newtype HandPoseTypeMSFT = HandPoseTypeMSFT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'HAND_POSE_TYPE_TRACKED_MSFT' represents a hand pose provided by actual
-- tracking of the user’s hand.
pattern HAND_POSE_TYPE_TRACKED_MSFT             = HandPoseTypeMSFT 0
-- | 'HAND_POSE_TYPE_REFERENCE_OPEN_PALM_MSFT' represents a stable reference
-- hand pose in a relaxed open hand shape.
pattern HAND_POSE_TYPE_REFERENCE_OPEN_PALM_MSFT = HandPoseTypeMSFT 1
{-# complete HAND_POSE_TYPE_TRACKED_MSFT,
             HAND_POSE_TYPE_REFERENCE_OPEN_PALM_MSFT :: HandPoseTypeMSFT #-}

conNameHandPoseTypeMSFT :: String
conNameHandPoseTypeMSFT = "HandPoseTypeMSFT"

enumPrefixHandPoseTypeMSFT :: String
enumPrefixHandPoseTypeMSFT = "HAND_POSE_TYPE_"

showTableHandPoseTypeMSFT :: [(HandPoseTypeMSFT, String)]
showTableHandPoseTypeMSFT =
  [(HAND_POSE_TYPE_TRACKED_MSFT, "TRACKED_MSFT"), (HAND_POSE_TYPE_REFERENCE_OPEN_PALM_MSFT, "REFERENCE_OPEN_PALM_MSFT")]

instance Show HandPoseTypeMSFT where
  showsPrec = enumShowsPrec enumPrefixHandPoseTypeMSFT
                            showTableHandPoseTypeMSFT
                            conNameHandPoseTypeMSFT
                            (\(HandPoseTypeMSFT x) -> x)
                            (showsPrec 11)

instance Read HandPoseTypeMSFT where
  readPrec = enumReadPrec enumPrefixHandPoseTypeMSFT showTableHandPoseTypeMSFT conNameHandPoseTypeMSFT HandPoseTypeMSFT


type MSFT_hand_tracking_mesh_SPEC_VERSION = 2

-- No documentation found for TopLevel "XR_MSFT_hand_tracking_mesh_SPEC_VERSION"
pattern MSFT_hand_tracking_mesh_SPEC_VERSION :: forall a . Integral a => a
pattern MSFT_hand_tracking_mesh_SPEC_VERSION = 2


type MSFT_HAND_TRACKING_MESH_EXTENSION_NAME = "XR_MSFT_hand_tracking_mesh"

-- No documentation found for TopLevel "XR_MSFT_HAND_TRACKING_MESH_EXTENSION_NAME"
pattern MSFT_HAND_TRACKING_MESH_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MSFT_HAND_TRACKING_MESH_EXTENSION_NAME = "XR_MSFT_hand_tracking_mesh"

