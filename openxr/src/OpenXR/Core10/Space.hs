{-# language CPP #-}
-- No documentation found for Chapter "Space"
module OpenXR.Core10.Space  ( destroySpace
                            , enumerateReferenceSpaces
                            , createReferenceSpace
                            , withReferenceSpace
                            , createActionSpace
                            , withActionSpace
                            , locateSpace
                            , getReferenceSpaceBoundsRect
                            , Vector3f(..)
                            , Quaternionf(..)
                            , Posef(..)
                            , ReferenceSpaceCreateInfo(..)
                            , ActionSpaceCreateInfo(..)
                            , SpaceLocation(..)
                            , SpaceVelocity(..)
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
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.CStruct.Extends (forgetExtensions)
import OpenXR.NamedType ((:::))
import OpenXR.Core10.Handles (Action_T)
import OpenXR.CStruct.Extends (Chain)
import OpenXR.CStruct.Extends (Extends)
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct.Extends (Extensible(..))
import OpenXR.Core10.FundamentalTypes (Extent2Df)
import {-# SOURCE #-} OpenXR.Extensions.XR_EXT_eye_gaze_interaction (EyeGazeSampleTimeEXT)
import OpenXR.Dynamic (InstanceCmds(pXrCreateActionSpace))
import OpenXR.Dynamic (InstanceCmds(pXrCreateReferenceSpace))
import OpenXR.Dynamic (InstanceCmds(pXrDestroySpace))
import OpenXR.Dynamic (InstanceCmds(pXrEnumerateReferenceSpaces))
import OpenXR.Dynamic (InstanceCmds(pXrGetReferenceSpaceBoundsRect))
import OpenXR.Dynamic (InstanceCmds(pXrLocateSpace))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.SemanticPaths (Path)
import OpenXR.CStruct.Extends (PeekChain)
import OpenXR.CStruct.Extends (PeekChain(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct.Extends (PokeChain(..))
import OpenXR.Core10.Enums.ReferenceSpaceType (ReferenceSpaceType)
import OpenXR.Core10.Enums.ReferenceSpaceType (ReferenceSpaceType(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Handles (Space)
import OpenXR.Core10.Handles (Space(..))
import OpenXR.Core10.Handles (Space(Space))
import OpenXR.Core10.Enums.SpaceLocationFlagBits (SpaceLocationFlags)
import OpenXR.Core10.Enums.SpaceVelocityFlagBits (SpaceVelocityFlags)
import OpenXR.Core10.Handles (Space_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.FundamentalTypes (Time)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_ACTION_SPACE_CREATE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_REFERENCE_SPACE_CREATE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SPACE_LOCATION))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SPACE_VELOCITY))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrDestroySpace
  :: FunPtr (Ptr Space_T -> IO Result) -> Ptr Space_T -> IO Result

-- | xrDestroySpace - Creates a space based on a pose action
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'OpenXR.Core10.Handles.Space' handles are destroyed using
-- 'destroySpace'. The runtime /may/ still use this space if there are
-- active dependencies (e.g, compositions in progress).
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrDestroySpace-space-parameter# @space@ /must/ be a valid
--     'OpenXR.Core10.Handles.Space' handle
--
-- == Thread Safety
--
-- -   Access to @space@, and any child handles, /must/ be externally
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
-- 'OpenXR.Core10.Handles.Space', 'createActionSpace',
-- 'createReferenceSpace'
destroySpace :: forall io
              . (MonadIO io)
             => -- | @space@ is a handle to an 'OpenXR.Core10.Handles.Space' previously
                -- created by a function such as 'createReferenceSpace'.
                Space
             -> io ()
destroySpace space = liftIO $ do
  let xrDestroySpacePtr = pXrDestroySpace (instanceCmds (space :: Space))
  unless (xrDestroySpacePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrDestroySpace is null" Nothing Nothing
  let xrDestroySpace' = mkXrDestroySpace xrDestroySpacePtr
  r <- traceAroundEvent "xrDestroySpace" (xrDestroySpace' (spaceHandle (space)))
  when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateReferenceSpaces
  :: FunPtr (Ptr Session_T -> Word32 -> Ptr Word32 -> Ptr ReferenceSpaceType -> IO Result) -> Ptr Session_T -> Word32 -> Ptr Word32 -> Ptr ReferenceSpaceType -> IO Result

-- | xrEnumerateReferenceSpaces - Enumerate available reference spaces
--
-- == Parameter Descriptions
--
-- -   @session@ is a handle to an 'OpenXR.Core10.Handles.Session'
--     previously created with 'OpenXR.Core10.Device.createSession'.
--
-- -   @spaceCapacityInput@ is the capacity of the spaces array, or 0 to
--     indicate a request to retrieve the required capacity.
--
-- -   @spaceCountOutput@ is a pointer to the count of spaces written, or a
--     pointer to the required capacity in the case that
--     @spaceCapacityInput@ is 0.
--
-- -   @spaces@ is a pointer to an application-allocated array that will be
--     filled with the enumerant of each supported reference space. It
--     /can/ be @NULL@ if @spaceCapacityInput@ is 0.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @spaces@ size.
--
-- = Description
--
-- Enumerates the set of reference space types that this runtime supports
-- for a given session. Runtimes /must/ always return identical buffer
-- contents from this enumeration for the lifetime of the session.
--
-- If a session enumerates support for a given reference space type, calls
-- to 'createReferenceSpace' /must/ succeed for that session, with any
-- transient unavailability of poses expressed later during calls to
-- 'locateSpace'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateReferenceSpaces-session-parameter# @session@ /must/
--     be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrEnumerateReferenceSpaces-spaceCountOutput-parameter#
--     @spaceCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrEnumerateReferenceSpaces-spaces-parameter# If
--     @spaceCapacityInput@ is not @0@, @spaces@ /must/ be a pointer to an
--     array of @spaceCapacityInput@
--     'OpenXR.Core10.Enums.ReferenceSpaceType.ReferenceSpaceType' values
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
-- = See Also
--
-- 'OpenXR.Core10.Enums.ReferenceSpaceType.ReferenceSpaceType',
-- 'OpenXR.Core10.Handles.Session', 'OpenXR.Core10.Handles.Space'
enumerateReferenceSpaces :: forall io
                          . (MonadIO io)
                         => -- No documentation found for Nested "xrEnumerateReferenceSpaces" "session"
                            Session
                         -> io (Result, ("spaces" ::: Vector ReferenceSpaceType))
enumerateReferenceSpaces session = liftIO . evalContT $ do
  let xrEnumerateReferenceSpacesPtr = pXrEnumerateReferenceSpaces (instanceCmds (session :: Session))
  lift $ unless (xrEnumerateReferenceSpacesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateReferenceSpaces is null" Nothing Nothing
  let xrEnumerateReferenceSpaces' = mkXrEnumerateReferenceSpaces xrEnumerateReferenceSpacesPtr
  let session' = sessionHandle (session)
  pSpaceCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateReferenceSpaces" (xrEnumerateReferenceSpaces' session' (0) (pSpaceCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  spaceCountOutput <- lift $ peek @Word32 pSpaceCountOutput
  pSpaces <- ContT $ bracket (callocBytes @ReferenceSpaceType ((fromIntegral (spaceCountOutput)) * 4)) free
  r' <- lift $ traceAroundEvent "xrEnumerateReferenceSpaces" (xrEnumerateReferenceSpaces' session' ((spaceCountOutput)) (pSpaceCountOutput) (pSpaces))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  spaceCountOutput' <- lift $ peek @Word32 pSpaceCountOutput
  spaces' <- lift $ generateM (fromIntegral (spaceCountOutput')) (\i -> peek @ReferenceSpaceType ((pSpaces `advancePtrBytes` (4 * (i)) :: Ptr ReferenceSpaceType)))
  pure $ ((r'), spaces')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateReferenceSpace
  :: FunPtr (Ptr Session_T -> Ptr ReferenceSpaceCreateInfo -> Ptr (Ptr Space_T) -> IO Result) -> Ptr Session_T -> Ptr ReferenceSpaceCreateInfo -> Ptr (Ptr Space_T) -> IO Result

-- | xrCreateReferenceSpace - Creates a reference space
--
-- == Parameter Descriptions
--
-- = Description
--
-- Creates an 'OpenXR.Core10.Handles.Space' handle based on a chosen
-- reference space. Application /can/ provide an 'Posef' to define the
-- position and orientation of the new space’s origin within the natural
-- reference frame of the reference space.
--
-- Multiple 'OpenXR.Core10.Handles.Space' handles may exist simultaneously,
-- up to some limit imposed by the runtime. The
-- 'OpenXR.Core10.Handles.Space' handle /must/ be eventually freed via the
-- 'destroySpace' function.
--
-- The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_REFERENCE_SPACE_UNSUPPORTED' if the
-- given reference space type is not supported by this @session@.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_REFERENCE_SPACE_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_POSE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'ReferenceSpaceCreateInfo', 'OpenXR.Core10.Handles.Session',
-- 'OpenXR.Core10.Handles.Space', 'destroySpace'
createReferenceSpace :: forall io
                      . (MonadIO io)
                     => -- | @session@ is a handle to an 'OpenXR.Core10.Handles.Session' previously
                        -- created with 'OpenXR.Core10.Device.createSession'.
                        --
                        -- #VUID-xrCreateReferenceSpace-session-parameter# @session@ /must/ be a
                        -- valid 'OpenXR.Core10.Handles.Session' handle
                        Session
                     -> -- | @createInfo@ is the 'ReferenceSpaceCreateInfo' used to specify the
                        -- space.
                        --
                        -- #VUID-xrCreateReferenceSpace-createInfo-parameter# @createInfo@ /must/
                        -- be a pointer to a valid 'ReferenceSpaceCreateInfo' structure
                        ReferenceSpaceCreateInfo
                     -> io (Result, Space)
createReferenceSpace session createInfo = liftIO . evalContT $ do
  let cmds = instanceCmds (session :: Session)
  let xrCreateReferenceSpacePtr = pXrCreateReferenceSpace cmds
  lift $ unless (xrCreateReferenceSpacePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateReferenceSpace is null" Nothing Nothing
  let xrCreateReferenceSpace' = mkXrCreateReferenceSpace xrCreateReferenceSpacePtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pSpace <- ContT $ bracket (callocBytes @(Ptr Space_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateReferenceSpace" (xrCreateReferenceSpace' (sessionHandle (session)) createInfo' (pSpace))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  space <- lift $ peek @(Ptr Space_T) pSpace
  pure $ (r, ((\h -> Space h cmds ) space))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createReferenceSpace' and 'destroySpace'
--
-- To ensure that 'destroySpace' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withReferenceSpace :: forall io r . MonadIO io => Session -> ReferenceSpaceCreateInfo -> (io (Result, Space) -> ((Result, Space) -> io ()) -> r) -> r
withReferenceSpace session createInfo b =
  b (createReferenceSpace session createInfo)
    (\(_, o1) -> destroySpace o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateActionSpace
  :: FunPtr (Ptr Session_T -> Ptr ActionSpaceCreateInfo -> Ptr (Ptr Space_T) -> IO Result) -> Ptr Session_T -> Ptr ActionSpaceCreateInfo -> Ptr (Ptr Space_T) -> IO Result

-- | xrCreateActionSpace - Creates a space based on a pose action
--
-- == Parameter Descriptions
--
-- = Description
--
-- Creates an 'OpenXR.Core10.Handles.Space' handle based on a chosen pose
-- action. Application /can/ provide an 'Posef' to define the position and
-- orientation of the new space’s origin within the natural reference frame
-- of the action space.
--
-- Multiple 'OpenXR.Core10.Handles.Space' handles may exist simultaneously,
-- up to some limit imposed by the runtime. The
-- 'OpenXR.Core10.Handles.Space' handle must be eventually freed via the
-- 'destroySpace' function or by destroying the parent
-- 'OpenXR.Core10.Handles.Action' handle.
--
-- The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_ACTION_TYPE_MISMATCH' if the action
-- provided in @action@ is not of type
-- 'OpenXR.Core10.Enums.ActionType.ACTION_TYPE_POSE_INPUT'.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTION_TYPE_MISMATCH'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LIMIT_REACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_POSE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
-- = See Also
--
-- 'ActionSpaceCreateInfo', 'OpenXR.Core10.Handles.Session',
-- 'OpenXR.Core10.Handles.Space', 'destroySpace'
createActionSpace :: forall io
                   . (MonadIO io)
                  => -- | @session@ is the 'OpenXR.Core10.Handles.Session' to create the action
                     -- space in.
                     --
                     -- #VUID-xrCreateActionSpace-session-parameter# @session@ /must/ be a valid
                     -- 'OpenXR.Core10.Handles.Session' handle
                     Session
                  -> -- | @createInfo@ is the 'ActionSpaceCreateInfo' used to specify the space.
                     --
                     -- #VUID-xrCreateActionSpace-createInfo-parameter# @createInfo@ /must/ be a
                     -- pointer to a valid 'ActionSpaceCreateInfo' structure
                     ActionSpaceCreateInfo
                  -> io (Result, Space)
createActionSpace session createInfo = liftIO . evalContT $ do
  let cmds = instanceCmds (session :: Session)
  let xrCreateActionSpacePtr = pXrCreateActionSpace cmds
  lift $ unless (xrCreateActionSpacePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateActionSpace is null" Nothing Nothing
  let xrCreateActionSpace' = mkXrCreateActionSpace xrCreateActionSpacePtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pSpace <- ContT $ bracket (callocBytes @(Ptr Space_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateActionSpace" (xrCreateActionSpace' (sessionHandle (session)) createInfo' (pSpace))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  space <- lift $ peek @(Ptr Space_T) pSpace
  pure $ (r, ((\h -> Space h cmds ) space))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createActionSpace' and 'destroySpace'
--
-- To ensure that 'destroySpace' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withActionSpace :: forall io r . MonadIO io => Session -> ActionSpaceCreateInfo -> (io (Result, Space) -> ((Result, Space) -> io ()) -> r) -> r
withActionSpace session createInfo b =
  b (createActionSpace session createInfo)
    (\(_, o1) -> destroySpace o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrLocateSpace
  :: FunPtr (Ptr Space_T -> Ptr Space_T -> Time -> Ptr (SomeStruct SpaceLocation) -> IO Result) -> Ptr Space_T -> Ptr Space_T -> Time -> Ptr (SomeStruct SpaceLocation) -> IO Result

-- | xrLocateSpace - Locates a space with reference to another space
--
-- == Parameter Descriptions
--
-- = Description
--
-- For a @time@ in the past, the runtime /should/ locate the spaces based
-- on the runtime’s most accurate current understanding of how the world
-- was at that historical time.
--
-- For a @time@ in the future, the runtime /should/ locate the spaces based
-- on the runtime’s most up-to-date prediction of how the world will be at
-- that future time.
--
-- The minimum valid range of values for @time@ are described in
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#prediction-time-limits>.
-- For values of @time@ outside this range, 'locateSpace' /may/ return a
-- location with no position and
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- unset.
--
-- Some devices improve their understanding of the world as the device is
-- used. The location returned by 'locateSpace' for a given @space@,
-- @baseSpace@ and @time@ /may/ change over time, even for spaces that
-- track static objects, as one or both spaces adjust their origins.
--
-- During tracking loss of @space@ relative to @baseSpace@, runtimes
-- /should/ continue to provide inferred or last-known @position@ and
-- @orientation@ values. These inferred poses can, for example, be based on
-- neck model updates, inertial dead reckoning, or a last-known position,
-- so long as it is still reasonable for the application to use that pose.
-- While a runtime is providing position data, it /must/ continue to set
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- but it /can/ clear
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_TRACKED_BIT'
-- to indicate that the position is inferred or last-known in this way.
--
-- If the runtime has not yet observed even a last-known pose for how to
-- locate @space@ in @baseSpace@ (e.g. one space is an action space bound
-- to a motion controller that has not yet been detected, or the two spaces
-- are in disconnected fragments of the runtime’s tracked volume), the
-- runtime /should/ return a location with no position and
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- unset.
--
-- The runtime /must/ return a location with both
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- and
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_TRACKED_BIT'
-- set when locating @space@ and @baseSpace@ if both spaces were created
-- relative to the same entity (e.g. two action spaces for the same
-- action), even if the entity is currently untracked. The location in this
-- case is the difference in the two spaces\' application-specified
-- transforms relative to that common entity.
--
-- The runtime /should/ return a location with
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_VALID_BIT'
-- set and
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SPACE_LOCATION_POSITION_TRACKED_BIT'
-- unset for spaces tracking two static entities in the world when their
-- relative pose is known to the runtime. This enables applications to make
-- use of the runtime’s latest knowledge of the world, even during tracking
-- loss.
--
-- If an 'SpaceVelocity' structure is chained to the @next@ pointer of
-- 'SpaceLocation' and the velocity is observed or can be calculated by the
-- runtime, the runtime /must/ fill in the linear velocity of the origin of
-- space within the reference frame of @baseSpace@ and set the
-- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SPACE_VELOCITY_LINEAR_VALID_BIT'.
-- Similarly, if an 'SpaceVelocity' structure is chained to the @next@
-- pointer of 'SpaceLocation' and the angular velocity is observed or can
-- be calculated by the runtime, the runtime /must/ fill in the angular
-- velocity of the origin of space within the reference frame of
-- @baseSpace@ and set the
-- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SPACE_VELOCITY_ANGULAR_VALID_BIT'.
--
-- The following example code shows how an application can get both the
-- location and velocity of a space within a base space using the
-- 'locateSpace' function by chaining an 'SpaceVelocity' to the next
-- pointer of 'SpaceLocation' and calling 'locateSpace'.
--
-- > XrSpace space;      // previously initialized
-- > XrSpace baseSpace;  // previously initialized
-- > XrTime time;        // previously initialized
-- >
-- > XrSpaceVelocity velocity {XR_TYPE_SPACE_VELOCITY};
-- > XrSpaceLocation location {XR_TYPE_SPACE_LOCATION, &velocity};
-- > xrLocateSpace(space, baseSpace, time, &location);
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrLocateSpace-space-parameter# @space@ /must/ be a valid
--     'OpenXR.Core10.Handles.Space' handle
--
-- -   #VUID-xrLocateSpace-baseSpace-parameter# @baseSpace@ /must/ be a
--     valid 'OpenXR.Core10.Handles.Space' handle
--
-- -   #VUID-xrLocateSpace-location-parameter# @location@ /must/ be a
--     pointer to an 'SpaceLocation' structure
--
-- -   #VUID-xrLocateSpace-commonparent# Both of @baseSpace@ and @space@
--     /must/ have been created, allocated, or retrieved from the same
--     'OpenXR.Core10.Handles.Session'
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
-- = See Also
--
-- 'OpenXR.Core10.Handles.Space', 'SpaceLocation',
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SpaceLocationFlagBits',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
locateSpace :: forall a io
             . (Extendss SpaceLocation a, PokeChain a, PeekChain a, MonadIO io)
            => -- | @space@ identifies the target space to locate.
               Space
            -> -- | @baseSpace@ identifies the underlying space in which to locate @space@.
               ("baseSpace" ::: Space)
            -> -- | @time@ is the time for which the location should be provided.
               Time
            -> io (Result, SpaceLocation a)
locateSpace space baseSpace time = liftIO . evalContT $ do
  let xrLocateSpacePtr = pXrLocateSpace (instanceCmds (space :: Space))
  lift $ unless (xrLocateSpacePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrLocateSpace is null" Nothing Nothing
  let xrLocateSpace' = mkXrLocateSpace xrLocateSpacePtr
  pLocation <- ContT (withZeroCStruct @(SpaceLocation _))
  r <- lift $ traceAroundEvent "xrLocateSpace" (xrLocateSpace' (spaceHandle (space)) (spaceHandle (baseSpace)) (time) (forgetExtensions (pLocation)))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  location <- lift $ peekCStruct @(SpaceLocation _) pLocation
  pure $ (r, location)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetReferenceSpaceBoundsRect
  :: FunPtr (Ptr Session_T -> ReferenceSpaceType -> Ptr Extent2Df -> IO Result) -> Ptr Session_T -> ReferenceSpaceType -> Ptr Extent2Df -> IO Result

-- | xrGetReferenceSpaceBoundsRect - Gets the bounds rectangle of a reference
-- space
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.SPACE_BOUNDS_UNAVAILABLE'
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_REFERENCE_SPACE_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.FundamentalTypes.Extent2Df',
-- 'OpenXR.Core10.Enums.ReferenceSpaceType.ReferenceSpaceType',
-- 'OpenXR.Core10.Handles.Session', 'createReferenceSpace'
getReferenceSpaceBoundsRect :: forall io
                             . (MonadIO io)
                            => -- | @session@ is a handle to an 'OpenXR.Core10.Handles.Session' previously
                               -- created with 'OpenXR.Core10.Device.createSession'.
                               --
                               -- #VUID-xrGetReferenceSpaceBoundsRect-session-parameter# @session@ /must/
                               -- be a valid 'OpenXR.Core10.Handles.Session' handle
                               Session
                            -> -- | @referenceSpaceType@ is the reference space type whose bounds should be
                               -- retrieved.
                               --
                               -- #VUID-xrGetReferenceSpaceBoundsRect-referenceSpaceType-parameter#
                               -- @referenceSpaceType@ /must/ be a valid
                               -- 'OpenXR.Core10.Enums.ReferenceSpaceType.ReferenceSpaceType' value
                               ReferenceSpaceType
                            -> io (Result, ("bounds" ::: Extent2Df))
getReferenceSpaceBoundsRect session referenceSpaceType = liftIO . evalContT $ do
  let xrGetReferenceSpaceBoundsRectPtr = pXrGetReferenceSpaceBoundsRect (instanceCmds (session :: Session))
  lift $ unless (xrGetReferenceSpaceBoundsRectPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetReferenceSpaceBoundsRect is null" Nothing Nothing
  let xrGetReferenceSpaceBoundsRect' = mkXrGetReferenceSpaceBoundsRect xrGetReferenceSpaceBoundsRectPtr
  pBounds <- ContT (withZeroCStruct @Extent2Df)
  r <- lift $ traceAroundEvent "xrGetReferenceSpaceBoundsRect" (xrGetReferenceSpaceBoundsRect' (sessionHandle (session)) (referenceSpaceType) (pBounds))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  bounds <- lift $ peekCStruct @Extent2Df pBounds
  pure $ (r, bounds)


-- | XrVector3f - Three-dimensional vector
--
-- == Member Descriptions
--
-- = Description
--
-- If used to represent physical distances (rather than e.g. velocity or
-- angular velocity) and not otherwise specified, values /must/ be in
-- meters.
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointVelocityEXT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandMeshVertexMSFT',
-- 'Posef', 'Quaternionf', 'SpaceVelocity', 'OpenXR.Core10.Input.Vector2f',
-- 'OpenXR.Core10.OtherTypes.Vector4f'
data Vector3f = Vector3f
  { -- | @x@ is the x coordinate of the vector.
    x :: Float
  , -- | @y@ is the y coordinate of the vector.
    y :: Float
  , -- | @z@ is the z coordinate of the vector.
    z :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Vector3f)
#endif
deriving instance Show Vector3f

instance ToCStruct Vector3f where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Vector3f{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (x))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (y))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (z))
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct Vector3f where
  peekCStruct p = do
    x <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    y <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    z <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    pure $ Vector3f
             (coerce @CFloat @Float x) (coerce @CFloat @Float y) (coerce @CFloat @Float z)

instance Storable Vector3f where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Vector3f where
  zero = Vector3f
           zero
           zero
           zero


-- | XrQuaternionf - Unit Quaternion
--
-- == Member Descriptions
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cube.CompositionLayerCubeKHR',
-- 'Posef', 'OpenXR.Core10.Input.Vector2f', 'Vector3f',
-- 'OpenXR.Core10.OtherTypes.Vector4f'
data Quaternionf = Quaternionf
  { -- | @x@ is the x coordinate of the quaternion.
    x :: Float
  , -- | @y@ is the y coordinate of the quaternion.
    y :: Float
  , -- | @z@ is the z coordinate of the quaternion.
    z :: Float
  , -- | @w@ is the w coordinate of the quaternion.
    w :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Quaternionf)
#endif
deriving instance Show Quaternionf

instance ToCStruct Quaternionf where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Quaternionf{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (x))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (y))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (z))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (w))
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct Quaternionf where
  peekCStruct p = do
    x <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    y <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    z <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    w <- peek @CFloat ((p `plusPtr` 12 :: Ptr CFloat))
    pure $ Quaternionf
             (coerce @CFloat @Float x) (coerce @CFloat @Float y) (coerce @CFloat @Float z) (coerce @CFloat @Float w)

instance Storable Quaternionf where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Quaternionf where
  zero = Quaternionf
           zero
           zero
           zero
           zero


-- | XrPosef - Location and orientation in a space.
--
-- == Member Descriptions
--
-- A construct representing a position and orientation within a space, with
-- position expressed in meters, and orientation represented as a unit
-- quaternion. When using 'Posef' the rotation described by @orientation@
-- is always applied before the translation described by @position@.
--
-- = Description
--
-- A runtime /must/ return 'OpenXR.Core10.Enums.Result.ERROR_POSE_INVALID'
-- if the @orientation@ norm deviates by more than 1% from unit length.
--
-- = See Also
--
-- 'ActionSpaceCreateInfo',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cylinder.CompositionLayerCylinderKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect2.CompositionLayerEquirect2KHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect.CompositionLayerEquirectKHR',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerProjectionView',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerQuad',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.ControllerModelNodeStateMSFT',
-- 'OpenXR.Core10.OtherTypes.EventDataReferenceSpaceChangePending',
-- 'OpenXR.Extensions.XR_EXT_hand_tracking.HandJointLocationEXT',
-- 'OpenXR.Extensions.XR_MSFT_hand_tracking_mesh.HandMeshSpaceCreateInfoMSFT',
-- 'Quaternionf', 'ReferenceSpaceCreateInfo', 'SpaceLocation',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.SpatialAnchorCreateInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_anchor.SpatialAnchorSpaceCreateInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_spatial_graph_bridge.SpatialGraphNodeSpaceCreateInfoMSFT',
-- 'OpenXR.Core10.Input.Vector2f', 'Vector3f',
-- 'OpenXR.Core10.OtherTypes.Vector4f', 'OpenXR.Core10.DisplayTiming.View',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceLocationEXT xrSetInputDeviceLocationEXT>
data Posef = Posef
  { -- | @orientation@ is an 'Quaternionf' representing the orientation within a
    -- space.
    orientation :: Quaternionf
  , -- | @position@ is an 'Vector3f' representing position within a space.
    position :: Vector3f
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Posef)
#endif
deriving instance Show Posef

instance ToCStruct Posef where
  withCStruct x f = allocaBytesAligned 28 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Posef{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Quaternionf)) (orientation)
    poke ((p `plusPtr` 16 :: Ptr Vector3f)) (position)
    f
  cStructSize = 28
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Quaternionf)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Vector3f)) (zero)
    f

instance FromCStruct Posef where
  peekCStruct p = do
    orientation <- peekCStruct @Quaternionf ((p `plusPtr` 0 :: Ptr Quaternionf))
    position <- peekCStruct @Vector3f ((p `plusPtr` 16 :: Ptr Vector3f))
    pure $ Posef
             orientation position

instance Storable Posef where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Posef where
  zero = Posef
           zero
           zero


-- | XrReferenceSpaceCreateInfo - Creation info for a reference space
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Posef', 'OpenXR.Core10.Enums.ReferenceSpaceType.ReferenceSpaceType',
-- 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'createReferenceSpace'
data ReferenceSpaceCreateInfo = ReferenceSpaceCreateInfo
  { -- | @referenceSpaceType@ is the chosen
    -- 'OpenXR.Core10.Enums.ReferenceSpaceType.ReferenceSpaceType'.
    --
    -- #VUID-XrReferenceSpaceCreateInfo-referenceSpaceType-parameter#
    -- @referenceSpaceType@ /must/ be a valid
    -- 'OpenXR.Core10.Enums.ReferenceSpaceType.ReferenceSpaceType' value
    referenceSpaceType :: ReferenceSpaceType
  , -- | @poseInReferenceSpace@ is an 'Posef' defining the position and
    -- orientation of the new space’s origin within the natural reference frame
    -- of the reference space.
    poseInReferenceSpace :: Posef
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ReferenceSpaceCreateInfo)
#endif
deriving instance Show ReferenceSpaceCreateInfo

instance ToCStruct ReferenceSpaceCreateInfo where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ReferenceSpaceCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_REFERENCE_SPACE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ReferenceSpaceType)) (referenceSpaceType)
    poke ((p `plusPtr` 20 :: Ptr Posef)) (poseInReferenceSpace)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_REFERENCE_SPACE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ReferenceSpaceType)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Posef)) (zero)
    f

instance FromCStruct ReferenceSpaceCreateInfo where
  peekCStruct p = do
    referenceSpaceType <- peek @ReferenceSpaceType ((p `plusPtr` 16 :: Ptr ReferenceSpaceType))
    poseInReferenceSpace <- peekCStruct @Posef ((p `plusPtr` 20 :: Ptr Posef))
    pure $ ReferenceSpaceCreateInfo
             referenceSpaceType poseInReferenceSpace

instance Storable ReferenceSpaceCreateInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ReferenceSpaceCreateInfo where
  zero = ReferenceSpaceCreateInfo
           zero
           zero


-- | XrActionSpaceCreateInfo - Creation info for an action space
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'Posef', 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'createActionSpace'
data ActionSpaceCreateInfo = ActionSpaceCreateInfo
  { -- | @action@ is a handle to a pose 'OpenXR.Core10.Handles.Action' previously
    -- created with 'OpenXR.Core10.Input.createAction'.
    --
    -- #VUID-XrActionSpaceCreateInfo-action-parameter# @action@ /must/ be a
    -- valid 'OpenXR.Core10.Handles.Action' handle
    action :: Ptr Action_T
  , -- | @subactionPath@ is 'OpenXR.Core10.APIConstants.NULL_PATH' or an
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
    -- that was specified when the action was created. If @subactionPath@ is a
    -- valid path not specified when the action was created the runtime /must/
    -- return 'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'. If this
    -- parameter is set, the runtime /must/ create a space that is relative to
    -- only that subaction’s pose binding.
    subactionPath :: Path
  , -- | @poseInActionSpace@ is an 'Posef' defining the position and orientation
    -- of the new space’s origin within the natural reference frame of the pose
    -- action.
    poseInActionSpace :: Posef
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActionSpaceCreateInfo)
#endif
deriving instance Show ActionSpaceCreateInfo

instance ToCStruct ActionSpaceCreateInfo where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActionSpaceCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_SPACE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Action_T))) (action)
    poke ((p `plusPtr` 24 :: Ptr Path)) (subactionPath)
    poke ((p `plusPtr` 32 :: Ptr Posef)) (poseInActionSpace)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_SPACE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Action_T))) (zero)
    poke ((p `plusPtr` 32 :: Ptr Posef)) (zero)
    f

instance FromCStruct ActionSpaceCreateInfo where
  peekCStruct p = do
    action <- peek @(Ptr Action_T) ((p `plusPtr` 16 :: Ptr (Ptr Action_T)))
    subactionPath <- peek @Path ((p `plusPtr` 24 :: Ptr Path))
    poseInActionSpace <- peekCStruct @Posef ((p `plusPtr` 32 :: Ptr Posef))
    pure $ ActionSpaceCreateInfo
             action subactionPath poseInActionSpace

instance Storable ActionSpaceCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ActionSpaceCreateInfo where
  zero = ActionSpaceCreateInfo
           zero
           zero
           zero


-- | XrSpaceLocation - Contains info about a space
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Posef', 'OpenXR.Core10.Handles.Space',
-- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SpaceLocationFlags',
-- 'SpaceVelocity', 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'locateSpace'
data SpaceLocation (es :: [Type]) = SpaceLocation
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain, such as 'SpaceVelocity'.
    --
    -- #VUID-XrSpaceLocation-next-next# @next@ /must/ be @NULL@ or a valid
    -- pointer to the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
    -- See also:
    -- 'OpenXR.Extensions.XR_EXT_eye_gaze_interaction.EyeGazeSampleTimeEXT',
    -- 'SpaceVelocity'
    next :: Chain es
  , -- | @locationFlags@ is a bitfield, with bit masks defined in
    -- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SpaceLocationFlagBits', to
    -- indicate which members contain valid data. If none of the bits are set,
    -- no other fields in this structure /should/ be considered to be valid or
    -- meaningful.
    --
    -- #VUID-XrSpaceLocation-locationFlags-parameter# @locationFlags@ /must/ be
    -- @0@ or a valid combination of
    -- 'OpenXR.Core10.Enums.SpaceLocationFlagBits.SpaceLocationFlagBits' values
    locationFlags :: SpaceLocationFlags
  , -- | @pose@ is an 'Posef' defining the position and orientation of the origin
    -- of 'locateSpace'::@space@ within the reference frame of
    -- 'locateSpace'::@baseSpace@.
    pose :: Posef
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SpaceLocation (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SpaceLocation es)

instance Extensible SpaceLocation where
  extensibleTypeName = "SpaceLocation"
  setNext x next = x{next = next}
  getNext SpaceLocation{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SpaceLocation e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @EyeGazeSampleTimeEXT = Just f
    | Just Refl <- eqT @e @SpaceVelocity = Just f
    | otherwise = Nothing

instance (Extendss SpaceLocation es, PokeChain es) => ToCStruct (SpaceLocation es) where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SpaceLocation{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SPACE_LOCATION)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr SpaceLocationFlags)) (locationFlags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Posef)) (pose)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SPACE_LOCATION)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr Posef)) (zero)
    lift $ f

instance (Extendss SpaceLocation es, PeekChain es) => FromCStruct (SpaceLocation es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    locationFlags <- peek @SpaceLocationFlags ((p `plusPtr` 16 :: Ptr SpaceLocationFlags))
    pose <- peekCStruct @Posef ((p `plusPtr` 24 :: Ptr Posef))
    pure $ SpaceLocation
             next' locationFlags pose

instance es ~ '[] => Zero (SpaceLocation es) where
  zero = SpaceLocation
           ()
           zero
           zero


-- | XrSpaceVelocity - Contains info about a space
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Space', 'SpaceLocation',
-- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SpaceVelocityFlags',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'Vector3f',
-- 'locateSpace'
data SpaceVelocity = SpaceVelocity
  { -- | @velocityFlags@ is a bitfield, with bit masks defined in
    -- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SpaceVelocityFlagBits', to
    -- indicate which members contain valid data. If none of the bits are set,
    -- no other fields in this structure /should/ be considered to be valid or
    -- meaningful.
    --
    -- #VUID-XrSpaceVelocity-velocityFlags-parameter# @velocityFlags@ /must/ be
    -- @0@ or a valid combination of
    -- 'OpenXR.Core10.Enums.SpaceVelocityFlagBits.SpaceVelocityFlagBits' values
    velocityFlags :: SpaceVelocityFlags
  , -- | @linearVelocity@ is the relative linear velocity of the origin of
    -- 'locateSpace'::@space@ with respect to and expressed in the reference
    -- frame of 'locateSpace'::@baseSpace@, in units of meters per second.
    linearVelocity :: Vector3f
  , -- | @angularVelocity@ is the relative angular velocity of
    -- 'locateSpace'::@space@ with respect to 'locateSpace'::@baseSpace@. The
    -- vector’s direction is expressed in the reference frame of
    -- 'locateSpace'::@baseSpace@ and is parallel to the rotational axis of
    -- 'locateSpace'::@space@. The vector’s magnitude is the relative angular
    -- speed of 'locateSpace'::@space@ in radians per second. The vector
    -- follows the right-hand rule for torque\/rotation.
    angularVelocity :: Vector3f
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SpaceVelocity)
#endif
deriving instance Show SpaceVelocity

instance ToCStruct SpaceVelocity where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SpaceVelocity{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SPACE_VELOCITY)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SpaceVelocityFlags)) (velocityFlags)
    poke ((p `plusPtr` 24 :: Ptr Vector3f)) (linearVelocity)
    poke ((p `plusPtr` 36 :: Ptr Vector3f)) (angularVelocity)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SPACE_VELOCITY)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Vector3f)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Vector3f)) (zero)
    f

instance FromCStruct SpaceVelocity where
  peekCStruct p = do
    velocityFlags <- peek @SpaceVelocityFlags ((p `plusPtr` 16 :: Ptr SpaceVelocityFlags))
    linearVelocity <- peekCStruct @Vector3f ((p `plusPtr` 24 :: Ptr Vector3f))
    angularVelocity <- peekCStruct @Vector3f ((p `plusPtr` 36 :: Ptr Vector3f))
    pure $ SpaceVelocity
             velocityFlags linearVelocity angularVelocity

instance Storable SpaceVelocity where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SpaceVelocity where
  zero = SpaceVelocity
           zero
           zero
           zero

