{-# language CPP #-}
-- | = Name
--
-- XR_EXT_hand_tracking - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_hand_tracking  XR_EXT_hand_tracking>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 52
--
-- = Revision
--
-- 2
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'OpenXR.Core10.APIConstants.HAND_JOINT_COUNT_EXT', 'HandEXT',
-- 'HandJointEXT', 'HandJointLocationEXT', 'HandJointLocationsEXT',
-- 'HandJointSetEXT', 'HandJointVelocitiesEXT', 'HandJointVelocityEXT',
-- 'HandJointsLocateInfoEXT', 'HandTrackerCreateInfoEXT',
-- 'SystemHandTrackingPropertiesEXT', 'createHandTrackerEXT',
-- 'destroyHandTrackerEXT', 'locateHandJointsEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_hand_tracking OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_hand_tracking  ( createHandTrackerEXT
                                               , withHandTrackerEXT
                                               , destroyHandTrackerEXT
                                               , locateHandJointsEXT
                                               , SystemHandTrackingPropertiesEXT(..)
                                               , HandTrackerCreateInfoEXT(..)
                                               , HandJointsLocateInfoEXT(..)
                                               , HandJointLocationEXT(..)
                                               , HandJointVelocityEXT(..)
                                               , HandJointLocationsEXT(..)
                                               , HandJointVelocitiesEXT(..)
                                               , HandEXT( HAND_LEFT_EXT
                                                        , HAND_RIGHT_EXT
                                                        , ..
                                                        )
                                               , HandJointEXT( HAND_JOINT_PALM_EXT
                                                             , HAND_JOINT_WRIST_EXT
                                                             , HAND_JOINT_THUMB_METACARPAL_EXT
                                                             , HAND_JOINT_THUMB_PROXIMAL_EXT
                                                             , HAND_JOINT_THUMB_DISTAL_EXT
                                                             , HAND_JOINT_THUMB_TIP_EXT
                                                             , HAND_JOINT_INDEX_METACARPAL_EXT
                                                             , HAND_JOINT_INDEX_PROXIMAL_EXT
                                                             , HAND_JOINT_INDEX_INTERMEDIATE_EXT
                                                             , HAND_JOINT_INDEX_DISTAL_EXT
                                                             , HAND_JOINT_INDEX_TIP_EXT
                                                             , HAND_JOINT_MIDDLE_METACARPAL_EXT
                                                             , HAND_JOINT_MIDDLE_PROXIMAL_EXT
                                                             , HAND_JOINT_MIDDLE_INTERMEDIATE_EXT
                                                             , HAND_JOINT_MIDDLE_DISTAL_EXT
                                                             , HAND_JOINT_MIDDLE_TIP_EXT
                                                             , HAND_JOINT_RING_METACARPAL_EXT
                                                             , HAND_JOINT_RING_PROXIMAL_EXT
                                                             , HAND_JOINT_RING_INTERMEDIATE_EXT
                                                             , HAND_JOINT_RING_DISTAL_EXT
                                                             , HAND_JOINT_RING_TIP_EXT
                                                             , HAND_JOINT_LITTLE_METACARPAL_EXT
                                                             , HAND_JOINT_LITTLE_PROXIMAL_EXT
                                                             , HAND_JOINT_LITTLE_INTERMEDIATE_EXT
                                                             , HAND_JOINT_LITTLE_DISTAL_EXT
                                                             , HAND_JOINT_LITTLE_TIP_EXT
                                                             , ..
                                                             )
                                               , HandJointSetEXT( HAND_JOINT_SET_DEFAULT_EXT
                                                                , ..
                                                                )
                                               , EXT_hand_tracking_SPEC_VERSION
                                               , pattern EXT_hand_tracking_SPEC_VERSION
                                               , EXT_HAND_TRACKING_EXTENSION_NAME
                                               , pattern EXT_HAND_TRACKING_EXTENSION_NAME
                                               , HandTrackerEXT(..)
                                               , HAND_JOINT_COUNT_EXT
                                               , pattern HAND_JOINT_COUNT_EXT
                                               ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showsPrec)
import Data.Coerce (coerce)
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
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
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
import OpenXR.CStruct.Extends (forgetExtensions)
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.CStruct.Extends (Chain)
import OpenXR.CStruct.Extends (Extends)
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} OpenXR.Extensions.XR_MSFT_hand_tracking_mesh (HandPoseTypeInfoMSFT)
import OpenXR.Extensions.Handles (HandTrackerEXT)
import OpenXR.Extensions.Handles (HandTrackerEXT(..))
import OpenXR.Extensions.Handles (HandTrackerEXT(HandTrackerEXT))
import OpenXR.Extensions.Handles (HandTrackerEXT_T)
import OpenXR.Dynamic (InstanceCmds(pXrCreateHandTrackerEXT))
import OpenXR.Dynamic (InstanceCmds(pXrDestroyHandTrackerEXT))
import OpenXR.Dynamic (InstanceCmds(pXrLocateHandJointsEXT))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.CStruct.Extends (PeekChain)
import OpenXR.CStruct.Extends (PeekChain(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct.Extends (PokeChain(..))
import OpenXR.Core10.Space (Posef)
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Enums.SpaceLocationFlagBits (SpaceLocationFlags)
import OpenXR.Core10.Enums.SpaceVelocityFlagBits (SpaceVelocityFlags)
import OpenXR.Core10.Handles (Space_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.FundamentalTypes (Time)
import OpenXR.Core10.Space (Vector3f)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAND_JOINTS_LOCATE_INFO_EXT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAND_JOINT_LOCATIONS_EXT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAND_JOINT_VELOCITIES_EXT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_HAND_TRACKER_CREATE_INFO_EXT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT))
import OpenXR.Core10.APIConstants (HAND_JOINT_COUNT_EXT)
import OpenXR.Extensions.Handles (HandTrackerEXT(..))
import OpenXR.Core10.APIConstants (pattern HAND_JOINT_COUNT_EXT)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateHandTrackerEXT
  :: FunPtr (Ptr Session_T -> Ptr (SomeStruct HandTrackerCreateInfoEXT) -> Ptr (Ptr HandTrackerEXT_T) -> IO Result) -> Ptr Session_T -> Ptr (SomeStruct HandTrackerCreateInfoEXT) -> Ptr (Ptr HandTrackerEXT_T) -> IO Result

-- | xrCreateHandTrackerEXT - Create a hand joints handle.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrCreateHandTrackerEXT-extension-notenabled# The @@ extension
--     /must/ be enabled prior to calling 'createHandTrackerEXT'
--
-- -   #VUID-xrCreateHandTrackerEXT-session-parameter# @session@ /must/ be
--     a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrCreateHandTrackerEXT-createInfo-parameter# @createInfo@
--     /must/ be a pointer to a valid 'HandTrackerCreateInfoEXT' structure
--
-- -   #VUID-xrCreateHandTrackerEXT-handTracker-parameter# @handTracker@
--     /must/ be a pointer to an 'OpenXR.Extensions.Handles.HandTrackerEXT'
--     handle
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED'
--
-- If the system does not support hand tracking, runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED' from
-- 'createHandTrackerEXT'. In this case, the runtime /must/ return
-- 'OpenXR.Core10.FundamentalTypes.FALSE' for @supportsHandTracking@ in
-- 'SystemHandTrackingPropertiesEXT' when the function
-- 'OpenXR.Core10.Device.getSystemProperties' is called, so that the
-- application /can/ avoid creating a hand tracker.
--
-- = See Also
--
-- 'HandTrackerCreateInfoEXT', 'OpenXR.Extensions.Handles.HandTrackerEXT',
-- 'OpenXR.Core10.Handles.Session'
createHandTrackerEXT :: forall a io
                      . (Extendss HandTrackerCreateInfoEXT a, PokeChain a, MonadIO io)
                     => -- | @session@ is an 'OpenXR.Core10.Handles.Session' in which the hand
                        -- tracker will be active.
                        Session
                     -> -- | @createInfo@ is the 'HandTrackerCreateInfoEXT' used to specify the hand
                        -- tracker.
                        (HandTrackerCreateInfoEXT a)
                     -> io (Result, HandTrackerEXT)
createHandTrackerEXT session createInfo = liftIO . evalContT $ do
  let cmds = instanceCmds (session :: Session)
  let xrCreateHandTrackerEXTPtr = pXrCreateHandTrackerEXT cmds
  lift $ unless (xrCreateHandTrackerEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateHandTrackerEXT is null" Nothing Nothing
  let xrCreateHandTrackerEXT' = mkXrCreateHandTrackerEXT xrCreateHandTrackerEXTPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pHandTracker <- ContT $ bracket (callocBytes @(Ptr HandTrackerEXT_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateHandTrackerEXT" (xrCreateHandTrackerEXT' (sessionHandle (session)) (forgetExtensions createInfo') (pHandTracker))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  handTracker <- lift $ peek @(Ptr HandTrackerEXT_T) pHandTracker
  pure $ (r, ((\h -> HandTrackerEXT h cmds ) handTracker))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createHandTrackerEXT' and 'destroyHandTrackerEXT'
--
-- To ensure that 'destroyHandTrackerEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withHandTrackerEXT :: forall a io r . (Extendss HandTrackerCreateInfoEXT a, PokeChain a, MonadIO io) => Session -> HandTrackerCreateInfoEXT a -> (io (Result, HandTrackerEXT) -> ((Result, HandTrackerEXT) -> io ()) -> r) -> r
withHandTrackerEXT session createInfo b =
  b (createHandTrackerEXT session createInfo)
    (\(_, o1) -> destroyHandTrackerEXT o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrDestroyHandTrackerEXT
  :: FunPtr (Ptr HandTrackerEXT_T -> IO Result) -> Ptr HandTrackerEXT_T -> IO Result

-- | xrDestroyHandTrackerEXT - Destroy a hand joints handle
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrDestroyHandTrackerEXT-extension-notenabled# The @@ extension
--     /must/ be enabled prior to calling 'destroyHandTrackerEXT'
--
-- -   #VUID-xrDestroyHandTrackerEXT-handTracker-parameter# @handTracker@
--     /must/ be a valid 'OpenXR.Extensions.Handles.HandTrackerEXT' handle
--
-- == Thread Safety
--
-- -   Access to @handTracker@, and any child handles, /must/ be externally
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Extensions.Handles.HandTrackerEXT'
destroyHandTrackerEXT :: forall io
                       . (MonadIO io)
                      => -- | @handTracker@ is an 'OpenXR.Extensions.Handles.HandTrackerEXT'
                         -- previously created by 'createHandTrackerEXT'.
                         HandTrackerEXT
                      -> io ()
destroyHandTrackerEXT handTracker = liftIO $ do
  let xrDestroyHandTrackerEXTPtr = pXrDestroyHandTrackerEXT (instanceCmds (handTracker :: HandTrackerEXT))
  unless (xrDestroyHandTrackerEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrDestroyHandTrackerEXT is null" Nothing Nothing
  let xrDestroyHandTrackerEXT' = mkXrDestroyHandTrackerEXT xrDestroyHandTrackerEXTPtr
  r <- traceAroundEvent "xrDestroyHandTrackerEXT" (xrDestroyHandTrackerEXT' (handTrackerEXTHandle (handTracker)))
  when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrLocateHandJointsEXT
  :: FunPtr (Ptr HandTrackerEXT_T -> Ptr HandJointsLocateInfoEXT -> Ptr (SomeStruct HandJointLocationsEXT) -> IO Result) -> Ptr HandTrackerEXT_T -> Ptr HandJointsLocateInfoEXT -> Ptr (SomeStruct HandJointLocationsEXT) -> IO Result

-- | xrLocateHandJointsEXT - Locate hand joint locations
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrLocateHandJointsEXT-extension-notenabled# The @@ extension
--     /must/ be enabled prior to calling 'locateHandJointsEXT'
--
-- -   #VUID-xrLocateHandJointsEXT-handTracker-parameter# @handTracker@
--     /must/ be a valid 'OpenXR.Extensions.Handles.HandTrackerEXT' handle
--
-- -   #VUID-xrLocateHandJointsEXT-locateInfo-parameter# @locateInfo@
--     /must/ be a pointer to a valid 'HandJointsLocateInfoEXT' structure
--
-- -   #VUID-xrLocateHandJointsEXT-locations-parameter# @locations@ /must/
--     be a pointer to an 'HandJointLocationsEXT' structure
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
-- = See Also
--
-- 'HandJointLocationsEXT', 'HandJointsLocateInfoEXT',
-- 'OpenXR.Extensions.Handles.HandTrackerEXT'
locateHandJointsEXT :: forall a io
                     . (Extendss HandJointLocationsEXT a, PokeChain a, PeekChain a, MonadIO io)
                    => -- | @handTracker@ is an 'OpenXR.Extensions.Handles.HandTrackerEXT'
                       -- previously created by 'createHandTrackerEXT'.
                       HandTrackerEXT
                    -> -- | @locateInfo@ is a pointer to 'HandJointsLocateInfoEXT' describing
                       -- information to locate hand joints.
                       HandJointsLocateInfoEXT
                    -> io (Result, HandJointLocationsEXT a)
locateHandJointsEXT handTracker locateInfo = liftIO . evalContT $ do
  let xrLocateHandJointsEXTPtr = pXrLocateHandJointsEXT (instanceCmds (handTracker :: HandTrackerEXT))
  lift $ unless (xrLocateHandJointsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrLocateHandJointsEXT is null" Nothing Nothing
  let xrLocateHandJointsEXT' = mkXrLocateHandJointsEXT xrLocateHandJointsEXTPtr
  locateInfo' <- ContT $ withCStruct (locateInfo)
  pLocations <- ContT (withZeroCStruct @(HandJointLocationsEXT _))
  r <- lift $ traceAroundEvent "xrLocateHandJointsEXT" (xrLocateHandJointsEXT' (handTrackerEXTHandle (handTracker)) locateInfo' (forgetExtensions (pLocations)))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  locations <- lift $ peekCStruct @(HandJointLocationsEXT _) pLocations
  pure $ (r, locations)


-- | XrSystemHandTrackingPropertiesEXT - System property for hand tracking
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSystemHandTrackingPropertiesEXT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'SystemHandTrackingPropertiesEXT'
--
-- -   #VUID-XrSystemHandTrackingPropertiesEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT'
--
-- -   #VUID-XrSystemHandTrackingPropertiesEXT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- If a runtime returns 'OpenXR.Core10.FundamentalTypes.FALSE' for
-- @supportsHandTracking@, the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED' from
-- 'createHandTrackerEXT'.
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data SystemHandTrackingPropertiesEXT = SystemHandTrackingPropertiesEXT
  { -- | @supportsHandTracking@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
    -- indicating if current system is capable of hand tracking input.
    supportsHandTracking :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SystemHandTrackingPropertiesEXT)
#endif
deriving instance Show SystemHandTrackingPropertiesEXT

instance ToCStruct SystemHandTrackingPropertiesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SystemHandTrackingPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (supportsHandTracking))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SYSTEM_HAND_TRACKING_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SystemHandTrackingPropertiesEXT where
  peekCStruct p = do
    supportsHandTracking <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SystemHandTrackingPropertiesEXT
             (bool32ToBool supportsHandTracking)

instance Storable SystemHandTrackingPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SystemHandTrackingPropertiesEXT where
  zero = SystemHandTrackingPropertiesEXT
           zero


-- | XrHandTrackerCreateInfoEXT - Information to create a hand joints handle
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandTrackerCreateInfoEXT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'HandTrackerCreateInfoEXT'
--
-- -   #VUID-XrHandTrackerCreateInfoEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_HAND_TRACKER_CREATE_INFO_EXT'
--
-- -   #VUID-XrHandTrackerCreateInfoEXT-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
--     See also:
--     'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandPoseTypeInfoMSFT'
--
-- -   #VUID-XrHandTrackerCreateInfoEXT-hand-parameter# @hand@ /must/ be a
--     valid 'HandEXT' value
--
-- -   #VUID-XrHandTrackerCreateInfoEXT-handJointSet-parameter#
--     @handJointSet@ /must/ be a valid 'HandJointSetEXT' value
--
-- = See Also
--
-- 'HandEXT', 'HandJointSetEXT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'createHandTrackerEXT'
data HandTrackerCreateInfoEXT (es :: [Type]) = HandTrackerCreateInfoEXT
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR or this extension.
    next :: Chain es
  , -- | @hand@ is an 'HandEXT' which describes which hand the tracker is
    -- tracking.
    hand :: HandEXT
  , -- | @handJointSet@ is an 'HandJointSetEXT' describe the set of hand joints
    -- to retrieve.
    handJointSet :: HandJointSetEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandTrackerCreateInfoEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (HandTrackerCreateInfoEXT es)

instance Extensible HandTrackerCreateInfoEXT where
  extensibleTypeName = "HandTrackerCreateInfoEXT"
  setNext x next = x{next = next}
  getNext HandTrackerCreateInfoEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends HandTrackerCreateInfoEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @HandPoseTypeInfoMSFT = Just f
    | otherwise = Nothing

instance (Extendss HandTrackerCreateInfoEXT es, PokeChain es) => ToCStruct (HandTrackerCreateInfoEXT es) where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandTrackerCreateInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_TRACKER_CREATE_INFO_EXT)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr HandEXT)) (hand)
    lift $ poke ((p `plusPtr` 20 :: Ptr HandJointSetEXT)) (handJointSet)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_TRACKER_CREATE_INFO_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr HandEXT)) (zero)
    lift $ poke ((p `plusPtr` 20 :: Ptr HandJointSetEXT)) (zero)
    lift $ f

instance (Extendss HandTrackerCreateInfoEXT es, PeekChain es) => FromCStruct (HandTrackerCreateInfoEXT es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    hand <- peek @HandEXT ((p `plusPtr` 16 :: Ptr HandEXT))
    handJointSet <- peek @HandJointSetEXT ((p `plusPtr` 20 :: Ptr HandJointSetEXT))
    pure $ HandTrackerCreateInfoEXT
             next' hand handJointSet

instance es ~ '[] => Zero (HandTrackerCreateInfoEXT es) where
  zero = HandTrackerCreateInfoEXT
           ()
           zero
           zero


-- | XrHandJointsLocateInfoEXT - Describes the information to locate hand
-- joints
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandJointsLocateInfoEXT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using 'HandJointsLocateInfoEXT'
--
-- -   #VUID-XrHandJointsLocateInfoEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_HAND_JOINTS_LOCATE_INFO_EXT'
--
-- -   #VUID-XrHandJointsLocateInfoEXT-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrHandJointsLocateInfoEXT-baseSpace-parameter# @baseSpace@
--     /must/ be a valid 'OpenXR.Core10.Handles.Space' handle
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'locateHandJointsEXT'
data HandJointsLocateInfoEXT = HandJointsLocateInfoEXT
  { -- | @baseSpace@ is an 'OpenXR.Core10.Handles.Space' within which the
    -- returned hand joint locations will be represented.
    baseSpace :: Ptr Space_T
  , -- | @time@ is an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- at which to locate the hand joints.
    time :: Time
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandJointsLocateInfoEXT)
#endif
deriving instance Show HandJointsLocateInfoEXT

instance ToCStruct HandJointsLocateInfoEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandJointsLocateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_JOINTS_LOCATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Space_T))) (baseSpace)
    poke ((p `plusPtr` 24 :: Ptr Time)) (time)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_JOINTS_LOCATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Space_T))) (zero)
    poke ((p `plusPtr` 24 :: Ptr Time)) (zero)
    f

instance FromCStruct HandJointsLocateInfoEXT where
  peekCStruct p = do
    baseSpace <- peek @(Ptr Space_T) ((p `plusPtr` 16 :: Ptr (Ptr Space_T)))
    time <- peek @Time ((p `plusPtr` 24 :: Ptr Time))
    pure $ HandJointsLocateInfoEXT
             baseSpace time

instance Storable HandJointsLocateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandJointsLocateInfoEXT where
  zero = HandJointsLocateInfoEXT
           zero
           zero


-- | XrHandJointLocationEXT - Describes the location and radius of a hand
-- joint
--
-- == Member Descriptions
--
-- = Description
--
-- If the returned @locationFlags@ has
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- set, the returned radius /must/ be a positive value.
--
-- If the returned @locationFlags@ has
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- unset, the returned radius value is undefined and should be avoided.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandJointLocationEXT-extension-notenabled# The @@ extension
--     /must/ be enabled prior to using 'HandJointLocationEXT'
--
-- -   #VUID-XrHandJointLocationEXT-locationFlags-parameter#
--     @locationFlags@ /must/ be a valid combination of
--     'OpenXR.Core10.Enums.SpaceLocationFlagBits.SpaceLocationFlagBits'
--     values
--
-- -   #VUID-XrHandJointLocationEXT-locationFlags-requiredbitmask#
--     @locationFlags@ /must/ not be @0@
--
-- = See Also
--
-- 'HandJointLocationsEXT', 'OpenXR.Core10.Space.Posef',
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SpaceLocationFlags'
data HandJointLocationEXT = HandJointLocationEXT
  { -- | @locationFlags@ is a bitfield, with bit masks defined in
    -- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SpaceLocationFlagBits', to
    -- indicate which members contain valid data. If none of the bits are set,
    -- no other fields in this structure /should/ be considered to be valid or
    -- meaningful.
    locationFlags :: SpaceLocationFlags
  , -- | @pose@ is an 'OpenXR.Core10.Space.Posef' defining the position and
    -- orientation of the origin of a hand joint within the reference frame of
    -- the corresponding 'HandJointsLocateInfoEXT'::@baseSpace@.
    pose :: Posef
  , -- | @radius@ is a @float@ value radius of the corresponding joint in units
    -- of meters.
    radius :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandJointLocationEXT)
#endif
deriving instance Show HandJointLocationEXT

instance ToCStruct HandJointLocationEXT where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandJointLocationEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr SpaceLocationFlags)) (locationFlags)
    poke ((p `plusPtr` 8 :: Ptr Posef)) (pose)
    poke ((p `plusPtr` 36 :: Ptr CFloat)) (CFloat (radius))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr SpaceLocationFlags)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Posef)) (zero)
    poke ((p `plusPtr` 36 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct HandJointLocationEXT where
  peekCStruct p = do
    locationFlags <- peek @SpaceLocationFlags ((p `plusPtr` 0 :: Ptr SpaceLocationFlags))
    pose <- peekCStruct @Posef ((p `plusPtr` 8 :: Ptr Posef))
    radius <- peek @CFloat ((p `plusPtr` 36 :: Ptr CFloat))
    pure $ HandJointLocationEXT
             locationFlags pose (coerce @CFloat @Float radius)

instance Storable HandJointLocationEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandJointLocationEXT where
  zero = HandJointLocationEXT
           zero
           zero
           zero


-- | XrHandJointVelocityEXT - Describes the velocity of a hand joint
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandJointVelocityEXT-extension-notenabled# The @@ extension
--     /must/ be enabled prior to using 'HandJointVelocityEXT'
--
-- -   #VUID-XrHandJointVelocityEXT-velocityFlags-parameter#
--     @velocityFlags@ /must/ be a valid combination of
--     'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SpaceVelocityFlagBits'
--     values
--
-- -   #VUID-XrHandJointVelocityEXT-velocityFlags-requiredbitmask#
--     @velocityFlags@ /must/ not be @0@
--
-- = See Also
--
-- 'HandJointVelocitiesEXT',
-- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SpaceVelocityFlags',
-- 'OpenXR.Core10.Space.Vector3f'
data HandJointVelocityEXT = HandJointVelocityEXT
  { -- | @velocityFlags@ is a bitfield, with bit masks defined in
    -- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SpaceVelocityFlagBits', to
    -- indicate which members contain valid data. If none of the bits are set,
    -- no other fields in this structure /should/ be considered to be valid or
    -- meaningful.
    velocityFlags :: SpaceVelocityFlags
  , -- | @linearVelocity@ is the relative linear velocity of the hand joint with
    -- respect to and expressed in the reference frame of the corresponding
    -- 'HandJointsLocateInfoEXT'::@baseSpace@, in units of meters per second.
    linearVelocity :: Vector3f
  , -- | @angularVelocity@ is the relative angular velocity of the hand joint
    -- with respect to the corresponding
    -- 'HandJointsLocateInfoEXT'::@baseSpace@. The vector’s direction is
    -- expressed in the reference frame of the corresponding
    -- 'HandJointsLocateInfoEXT'::@baseSpace@ and is parallel to the rotational
    -- axis of the hand joint. The vector’s magnitude is the relative angular
    -- speed of the hand joint in radians per second. The vector follows the
    -- right-hand rule for torque\/rotation.
    angularVelocity :: Vector3f
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandJointVelocityEXT)
#endif
deriving instance Show HandJointVelocityEXT

instance ToCStruct HandJointVelocityEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandJointVelocityEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr SpaceVelocityFlags)) (velocityFlags)
    poke ((p `plusPtr` 8 :: Ptr Vector3f)) (linearVelocity)
    poke ((p `plusPtr` 20 :: Ptr Vector3f)) (angularVelocity)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr SpaceVelocityFlags)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Vector3f)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Vector3f)) (zero)
    f

instance FromCStruct HandJointVelocityEXT where
  peekCStruct p = do
    velocityFlags <- peek @SpaceVelocityFlags ((p `plusPtr` 0 :: Ptr SpaceVelocityFlags))
    linearVelocity <- peekCStruct @Vector3f ((p `plusPtr` 8 :: Ptr Vector3f))
    angularVelocity <- peekCStruct @Vector3f ((p `plusPtr` 20 :: Ptr Vector3f))
    pure $ HandJointVelocityEXT
             velocityFlags linearVelocity angularVelocity

instance Storable HandJointVelocityEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandJointVelocityEXT where
  zero = HandJointVelocityEXT
           zero
           zero
           zero


-- | XrHandJointLocationsEXT - Returns the hand joint locations
--
-- == Member Descriptions
--
-- = Description
--
-- The application /must/ allocate the memory for the output array
-- @jointLocations@ that can contain at least @jointCount@ of
-- 'HandJointLocationEXT'.
--
-- The application /must/ set @jointCount@ as described by the
-- 'HandJointSetEXT' when creating the
-- 'OpenXR.Extensions.Handles.HandTrackerEXT' otherwise the runtime /must/
-- return 'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'.
--
-- The runtime /must/ update the @jointLocations@ array ordered so that the
-- application can index elements using the corresponding hand joint enum
-- (e.g. 'HandJointEXT') as described by 'HandJointSetEXT' when creating
-- the 'OpenXR.Extensions.Handles.HandTrackerEXT'. For example, when the
-- 'OpenXR.Extensions.Handles.HandTrackerEXT' is created with
-- 'HAND_JOINT_SET_DEFAULT_EXT', the application /must/ set the
-- @jointCount@ to 'OpenXR.Core10.APIConstants.HAND_JOINT_COUNT_EXT', and
-- the runtime /must/ fill the @jointLocations@ array ordered so that it
-- may be indexed by the 'HandJointEXT' enum.
--
-- If the returned @isActive@ is true, the runtime /must/ return all joint
-- locations with both
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- and
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_ORIENTATION_VALID_BIT'
-- set. Although, in this case, some joint space locations /may/ be
-- untracked (i.e.
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_TRACKED_BIT'
-- or
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_ORIENTATION_TRACKED_BIT'
-- is unset).
--
-- If the returned @isActive@ is false, it indicates the hand tracker did
-- not detect the hand input or the application lost input focus. In this
-- case, the runtime /must/ return all @jointLocations@ with neither
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- nor
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_ORIENTATION_VALID_BIT'
-- set.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandJointLocationsEXT-extension-notenabled# The @@ extension
--     /must/ be enabled prior to using 'HandJointLocationsEXT'
--
-- -   #VUID-XrHandJointLocationsEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_HAND_JOINT_LOCATIONS_EXT'
--
-- -   #VUID-XrHandJointLocationsEXT-next-next# @next@ /must/ be @NULL@ or
--     a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
--     See also: 'HandJointVelocitiesEXT'
--
-- -   #VUID-XrHandJointLocationsEXT-jointLocations-parameter#
--     @jointLocations@ /must/ be a pointer to an array of @jointCount@
--     'HandJointLocationEXT' structures
--
-- -   #VUID-XrHandJointLocationsEXT-jointCount-arraylength# The
--     @jointCount@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'HandJointLocationEXT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'locateHandJointsEXT'
data HandJointLocationsEXT (es :: [Type]) = HandJointLocationsEXT
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain, such as 'HandJointVelocitiesEXT'.
    next :: Chain es
  , -- | @isActive@ is a
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >
    -- indicating if the hand tracker is actively tracking.
    isActive :: Bool
  , -- | @jointCount@ is a @uint32_t@ describing the count of elements in
    -- @jointLocations@ array.
    jointCount :: Word32
  , -- | @jointLocations@ is an array of 'HandJointLocationEXT' receiving the
    -- returned hand joint locations.
    jointLocations :: Ptr HandJointLocationEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandJointLocationsEXT (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (HandJointLocationsEXT es)

instance Extensible HandJointLocationsEXT where
  extensibleTypeName = "HandJointLocationsEXT"
  setNext x next = x{next = next}
  getNext HandJointLocationsEXT{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends HandJointLocationsEXT e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @HandJointVelocitiesEXT = Just f
    | otherwise = Nothing

instance (Extendss HandJointLocationsEXT es, PokeChain es) => ToCStruct (HandJointLocationsEXT es) where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandJointLocationsEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_JOINT_LOCATIONS_EXT)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (isActive))
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (jointCount)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr HandJointLocationEXT))) (jointLocations)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_JOINT_LOCATIONS_EXT)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr HandJointLocationEXT))) (zero)
    lift $ f

instance (Extendss HandJointLocationsEXT es, PeekChain es) => FromCStruct (HandJointLocationsEXT es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    isActive <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    jointCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    jointLocations <- peek @(Ptr HandJointLocationEXT) ((p `plusPtr` 24 :: Ptr (Ptr HandJointLocationEXT)))
    pure $ HandJointLocationsEXT
             next' (bool32ToBool isActive) jointCount jointLocations

instance es ~ '[] => Zero (HandJointLocationsEXT es) where
  zero = HandJointLocationsEXT
           ()
           zero
           zero
           zero


-- | XrHandJointVelocitiesEXT - Returns the hand joint velocities
--
-- == Member Descriptions
--
-- = Description
--
-- The application /must/ allocate the memory for the output array
-- @jointVelocities@ that can contain at least @jointCount@ of
-- 'HandJointVelocityEXT'.
--
-- The application /must/ input @jointCount@ as described by the
-- 'HandJointSetEXT' when creating the
-- 'OpenXR.Extensions.Handles.HandTrackerEXT'. Otherwise, the runtime
-- /must/ return 'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'.
--
-- The runtime /must/ update the @jointVelocities@ array in the order so
-- that the application can index elements using the corresponding hand
-- joint enum (e.g. 'HandJointEXT') as described by the 'HandJointSetEXT'
-- when creating the 'OpenXR.Extensions.Handles.HandTrackerEXT'. For
-- example, when the 'OpenXR.Extensions.Handles.HandTrackerEXT' is created
-- with 'HAND_JOINT_SET_DEFAULT_EXT', the application /must/ set the
-- @jointCount@ to 'OpenXR.Core10.APIConstants.HAND_JOINT_COUNT_EXT', and
-- the returned @jointVelocities@ array /must/ be ordered to be indexed by
-- enum 'HandJointEXT' enum.
--
-- If the returned 'HandJointLocationsEXT'::@isActive@ is false, it
-- indicates the hand tracker did not detect a hand input or the
-- application lost input focus. In this case, the runtime /must/ return
-- all @jointVelocities@ with neither
-- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SPACE_VELOCITY_LINEAR_VALID_BIT'
-- nor
-- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SPACE_VELOCITY_ANGULAR_VALID_BIT'
-- set.
--
-- If an 'HandJointVelocitiesEXT' structure is chained to
-- 'HandJointLocationsEXT'::@next@, the returned
-- 'HandJointLocationsEXT'::@isActive@ is true, and the velocity is
-- observed or can be calculated by the runtime, the runtime /must/ fill in
-- the linear velocity of each hand joint within the reference frame of
-- @baseSpace@ and set the
-- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SPACE_VELOCITY_LINEAR_VALID_BIT'.
-- Similarly, if an 'HandJointVelocitiesEXT' structure is chained to
-- 'HandJointLocationsEXT'::@next@, the returned
-- 'HandJointLocationsEXT'::@isActive@ is true, and the /angular velocity/
-- is observed or can be calculated by the runtime, the runtime /must/ fill
-- in the angular velocity of each joint within the reference frame of
-- @baseSpace@ and set the
-- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SPACE_VELOCITY_ANGULAR_VALID_BIT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrHandJointVelocitiesEXT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using 'HandJointVelocitiesEXT'
--
-- -   #VUID-XrHandJointVelocitiesEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_HAND_JOINT_VELOCITIES_EXT'
--
-- -   #VUID-XrHandJointVelocitiesEXT-next-next# @next@ /must/ be @NULL@ or
--     a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrHandJointVelocitiesEXT-jointVelocities-parameter#
--     @jointVelocities@ /must/ be a pointer to an array of @jointCount@
--     'HandJointVelocityEXT' structures
--
-- -   #VUID-XrHandJointVelocitiesEXT-jointCount-arraylength# The
--     @jointCount@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- 'HandJointLocationsEXT', 'HandJointVelocityEXT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data HandJointVelocitiesEXT = HandJointVelocitiesEXT
  { -- | @jointCount@ is a @uint32_t@ describing the number of elements in
    -- @jointVelocities@ array.
    jointCount :: Word32
  , -- | @jointVelocities@ is an array of 'HandJointVelocityEXT' receiving the
    -- returned hand joint velocities.
    jointVelocities :: Ptr HandJointVelocityEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (HandJointVelocitiesEXT)
#endif
deriving instance Show HandJointVelocitiesEXT

instance ToCStruct HandJointVelocitiesEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p HandJointVelocitiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_JOINT_VELOCITIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (jointCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr HandJointVelocityEXT))) (jointVelocities)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_HAND_JOINT_VELOCITIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr HandJointVelocityEXT))) (zero)
    f

instance FromCStruct HandJointVelocitiesEXT where
  peekCStruct p = do
    jointCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    jointVelocities <- peek @(Ptr HandJointVelocityEXT) ((p `plusPtr` 24 :: Ptr (Ptr HandJointVelocityEXT)))
    pure $ HandJointVelocitiesEXT
             jointCount jointVelocities

instance Storable HandJointVelocitiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero HandJointVelocitiesEXT where
  zero = HandJointVelocitiesEXT
           zero
           zero


-- | XrHandEXT - Describes which hand the tracker is tracking.
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'HandTrackerCreateInfoEXT'
newtype HandEXT = HandEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'HAND_LEFT_EXT' specifies the hand tracker will be tracking the user’s
-- left hand.
pattern HAND_LEFT_EXT  = HandEXT 1
-- | 'HAND_RIGHT_EXT' specifies the hand tracker will be tracking the user’s
-- right hand.
pattern HAND_RIGHT_EXT = HandEXT 2
{-# complete HAND_LEFT_EXT,
             HAND_RIGHT_EXT :: HandEXT #-}

conNameHandEXT :: String
conNameHandEXT = "HandEXT"

enumPrefixHandEXT :: String
enumPrefixHandEXT = "HAND_"

showTableHandEXT :: [(HandEXT, String)]
showTableHandEXT = [(HAND_LEFT_EXT, "LEFT_EXT"), (HAND_RIGHT_EXT, "RIGHT_EXT")]

instance Show HandEXT where
  showsPrec = enumShowsPrec enumPrefixHandEXT showTableHandEXT conNameHandEXT (\(HandEXT x) -> x) (showsPrec 11)

instance Read HandEXT where
  readPrec = enumReadPrec enumPrefixHandEXT showTableHandEXT conNameHandEXT HandEXT


-- | XrHandJointEXT - The name of hand joints that can be tracked
--
-- = See Also
--
-- No cross-references are available
newtype HandJointEXT = HandJointEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_PALM_EXT"
pattern HAND_JOINT_PALM_EXT                = HandJointEXT 0
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_WRIST_EXT"
pattern HAND_JOINT_WRIST_EXT               = HandJointEXT 1
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_THUMB_METACARPAL_EXT"
pattern HAND_JOINT_THUMB_METACARPAL_EXT    = HandJointEXT 2
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_THUMB_PROXIMAL_EXT"
pattern HAND_JOINT_THUMB_PROXIMAL_EXT      = HandJointEXT 3
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_THUMB_DISTAL_EXT"
pattern HAND_JOINT_THUMB_DISTAL_EXT        = HandJointEXT 4
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_THUMB_TIP_EXT"
pattern HAND_JOINT_THUMB_TIP_EXT           = HandJointEXT 5
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_INDEX_METACARPAL_EXT"
pattern HAND_JOINT_INDEX_METACARPAL_EXT    = HandJointEXT 6
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_INDEX_PROXIMAL_EXT"
pattern HAND_JOINT_INDEX_PROXIMAL_EXT      = HandJointEXT 7
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_INDEX_INTERMEDIATE_EXT"
pattern HAND_JOINT_INDEX_INTERMEDIATE_EXT  = HandJointEXT 8
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_INDEX_DISTAL_EXT"
pattern HAND_JOINT_INDEX_DISTAL_EXT        = HandJointEXT 9
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_INDEX_TIP_EXT"
pattern HAND_JOINT_INDEX_TIP_EXT           = HandJointEXT 10
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_MIDDLE_METACARPAL_EXT"
pattern HAND_JOINT_MIDDLE_METACARPAL_EXT   = HandJointEXT 11
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_MIDDLE_PROXIMAL_EXT"
pattern HAND_JOINT_MIDDLE_PROXIMAL_EXT     = HandJointEXT 12
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_MIDDLE_INTERMEDIATE_EXT"
pattern HAND_JOINT_MIDDLE_INTERMEDIATE_EXT = HandJointEXT 13
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_MIDDLE_DISTAL_EXT"
pattern HAND_JOINT_MIDDLE_DISTAL_EXT       = HandJointEXT 14
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_MIDDLE_TIP_EXT"
pattern HAND_JOINT_MIDDLE_TIP_EXT          = HandJointEXT 15
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_RING_METACARPAL_EXT"
pattern HAND_JOINT_RING_METACARPAL_EXT     = HandJointEXT 16
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_RING_PROXIMAL_EXT"
pattern HAND_JOINT_RING_PROXIMAL_EXT       = HandJointEXT 17
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_RING_INTERMEDIATE_EXT"
pattern HAND_JOINT_RING_INTERMEDIATE_EXT   = HandJointEXT 18
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_RING_DISTAL_EXT"
pattern HAND_JOINT_RING_DISTAL_EXT         = HandJointEXT 19
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_RING_TIP_EXT"
pattern HAND_JOINT_RING_TIP_EXT            = HandJointEXT 20
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_LITTLE_METACARPAL_EXT"
pattern HAND_JOINT_LITTLE_METACARPAL_EXT   = HandJointEXT 21
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_LITTLE_PROXIMAL_EXT"
pattern HAND_JOINT_LITTLE_PROXIMAL_EXT     = HandJointEXT 22
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_LITTLE_INTERMEDIATE_EXT"
pattern HAND_JOINT_LITTLE_INTERMEDIATE_EXT = HandJointEXT 23
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_LITTLE_DISTAL_EXT"
pattern HAND_JOINT_LITTLE_DISTAL_EXT       = HandJointEXT 24
-- No documentation found for Nested "XrHandJointEXT" "XR_HAND_JOINT_LITTLE_TIP_EXT"
pattern HAND_JOINT_LITTLE_TIP_EXT          = HandJointEXT 25
{-# complete HAND_JOINT_PALM_EXT,
             HAND_JOINT_WRIST_EXT,
             HAND_JOINT_THUMB_METACARPAL_EXT,
             HAND_JOINT_THUMB_PROXIMAL_EXT,
             HAND_JOINT_THUMB_DISTAL_EXT,
             HAND_JOINT_THUMB_TIP_EXT,
             HAND_JOINT_INDEX_METACARPAL_EXT,
             HAND_JOINT_INDEX_PROXIMAL_EXT,
             HAND_JOINT_INDEX_INTERMEDIATE_EXT,
             HAND_JOINT_INDEX_DISTAL_EXT,
             HAND_JOINT_INDEX_TIP_EXT,
             HAND_JOINT_MIDDLE_METACARPAL_EXT,
             HAND_JOINT_MIDDLE_PROXIMAL_EXT,
             HAND_JOINT_MIDDLE_INTERMEDIATE_EXT,
             HAND_JOINT_MIDDLE_DISTAL_EXT,
             HAND_JOINT_MIDDLE_TIP_EXT,
             HAND_JOINT_RING_METACARPAL_EXT,
             HAND_JOINT_RING_PROXIMAL_EXT,
             HAND_JOINT_RING_INTERMEDIATE_EXT,
             HAND_JOINT_RING_DISTAL_EXT,
             HAND_JOINT_RING_TIP_EXT,
             HAND_JOINT_LITTLE_METACARPAL_EXT,
             HAND_JOINT_LITTLE_PROXIMAL_EXT,
             HAND_JOINT_LITTLE_INTERMEDIATE_EXT,
             HAND_JOINT_LITTLE_DISTAL_EXT,
             HAND_JOINT_LITTLE_TIP_EXT :: HandJointEXT #-}

conNameHandJointEXT :: String
conNameHandJointEXT = "HandJointEXT"

enumPrefixHandJointEXT :: String
enumPrefixHandJointEXT = "HAND_JOINT_"

showTableHandJointEXT :: [(HandJointEXT, String)]
showTableHandJointEXT =
  [ (HAND_JOINT_PALM_EXT               , "PALM_EXT")
  , (HAND_JOINT_WRIST_EXT              , "WRIST_EXT")
  , (HAND_JOINT_THUMB_METACARPAL_EXT   , "THUMB_METACARPAL_EXT")
  , (HAND_JOINT_THUMB_PROXIMAL_EXT     , "THUMB_PROXIMAL_EXT")
  , (HAND_JOINT_THUMB_DISTAL_EXT       , "THUMB_DISTAL_EXT")
  , (HAND_JOINT_THUMB_TIP_EXT          , "THUMB_TIP_EXT")
  , (HAND_JOINT_INDEX_METACARPAL_EXT   , "INDEX_METACARPAL_EXT")
  , (HAND_JOINT_INDEX_PROXIMAL_EXT     , "INDEX_PROXIMAL_EXT")
  , (HAND_JOINT_INDEX_INTERMEDIATE_EXT , "INDEX_INTERMEDIATE_EXT")
  , (HAND_JOINT_INDEX_DISTAL_EXT       , "INDEX_DISTAL_EXT")
  , (HAND_JOINT_INDEX_TIP_EXT          , "INDEX_TIP_EXT")
  , (HAND_JOINT_MIDDLE_METACARPAL_EXT  , "MIDDLE_METACARPAL_EXT")
  , (HAND_JOINT_MIDDLE_PROXIMAL_EXT    , "MIDDLE_PROXIMAL_EXT")
  , (HAND_JOINT_MIDDLE_INTERMEDIATE_EXT, "MIDDLE_INTERMEDIATE_EXT")
  , (HAND_JOINT_MIDDLE_DISTAL_EXT      , "MIDDLE_DISTAL_EXT")
  , (HAND_JOINT_MIDDLE_TIP_EXT         , "MIDDLE_TIP_EXT")
  , (HAND_JOINT_RING_METACARPAL_EXT    , "RING_METACARPAL_EXT")
  , (HAND_JOINT_RING_PROXIMAL_EXT      , "RING_PROXIMAL_EXT")
  , (HAND_JOINT_RING_INTERMEDIATE_EXT  , "RING_INTERMEDIATE_EXT")
  , (HAND_JOINT_RING_DISTAL_EXT        , "RING_DISTAL_EXT")
  , (HAND_JOINT_RING_TIP_EXT           , "RING_TIP_EXT")
  , (HAND_JOINT_LITTLE_METACARPAL_EXT  , "LITTLE_METACARPAL_EXT")
  , (HAND_JOINT_LITTLE_PROXIMAL_EXT    , "LITTLE_PROXIMAL_EXT")
  , (HAND_JOINT_LITTLE_INTERMEDIATE_EXT, "LITTLE_INTERMEDIATE_EXT")
  , (HAND_JOINT_LITTLE_DISTAL_EXT      , "LITTLE_DISTAL_EXT")
  , (HAND_JOINT_LITTLE_TIP_EXT         , "LITTLE_TIP_EXT")
  ]

instance Show HandJointEXT where
  showsPrec = enumShowsPrec enumPrefixHandJointEXT
                            showTableHandJointEXT
                            conNameHandJointEXT
                            (\(HandJointEXT x) -> x)
                            (showsPrec 11)

instance Read HandJointEXT where
  readPrec = enumReadPrec enumPrefixHandJointEXT showTableHandJointEXT conNameHandJointEXT HandJointEXT


-- | XrHandJointSetEXT - The set of hand joints to track.
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'HandTrackerCreateInfoEXT'
newtype HandJointSetEXT = HandJointSetEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'HAND_JOINT_SET_DEFAULT_EXT' indicates that the created
-- 'OpenXR.Extensions.Handles.HandTrackerEXT' tracks the set of hand joints
-- described by 'HandJointEXT' enum, i.e. the 'locateHandJointsEXT'
-- function returns an array of joint locations with the count of
-- 'OpenXR.Core10.APIConstants.HAND_JOINT_COUNT_EXT' and can be indexed
-- using 'HandJointEXT'.
pattern HAND_JOINT_SET_DEFAULT_EXT = HandJointSetEXT 0
{-# complete HAND_JOINT_SET_DEFAULT_EXT :: HandJointSetEXT #-}

conNameHandJointSetEXT :: String
conNameHandJointSetEXT = "HandJointSetEXT"

enumPrefixHandJointSetEXT :: String
enumPrefixHandJointSetEXT = "HAND_JOINT_SET_DEFAULT_EXT"

showTableHandJointSetEXT :: [(HandJointSetEXT, String)]
showTableHandJointSetEXT = [(HAND_JOINT_SET_DEFAULT_EXT, "")]

instance Show HandJointSetEXT where
  showsPrec = enumShowsPrec enumPrefixHandJointSetEXT
                            showTableHandJointSetEXT
                            conNameHandJointSetEXT
                            (\(HandJointSetEXT x) -> x)
                            (showsPrec 11)

instance Read HandJointSetEXT where
  readPrec = enumReadPrec enumPrefixHandJointSetEXT showTableHandJointSetEXT conNameHandJointSetEXT HandJointSetEXT


type EXT_hand_tracking_SPEC_VERSION = 2

-- No documentation found for TopLevel "XR_EXT_hand_tracking_SPEC_VERSION"
pattern EXT_hand_tracking_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_hand_tracking_SPEC_VERSION = 2


type EXT_HAND_TRACKING_EXTENSION_NAME = "XR_EXT_hand_tracking"

-- No documentation found for TopLevel "XR_EXT_HAND_TRACKING_EXTENSION_NAME"
pattern EXT_HAND_TRACKING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_HAND_TRACKING_EXTENSION_NAME = "XR_EXT_hand_tracking"

