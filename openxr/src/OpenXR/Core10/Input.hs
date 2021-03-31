{-# language CPP #-}
-- No documentation found for Chapter "Input"
module OpenXR.Core10.Input  ( getActionStateBoolean
                            , getActionStateFloat
                            , getActionStateVector2f
                            , getActionStatePose
                            , createActionSet
                            , withActionSet
                            , destroyActionSet
                            , createAction
                            , withAction
                            , destroyAction
                            , suggestInteractionProfileBindings
                            , attachSessionActionSets
                            , getCurrentInteractionProfile
                            , syncActions
                            , enumerateBoundSourcesForAction
                            , getInputSourceLocalizedName
                            , Vector2f(..)
                            , ActionStateBoolean(..)
                            , ActionStateFloat(..)
                            , ActionStateVector2f(..)
                            , ActionStatePose(..)
                            , ActionStateGetInfo(..)
                            , ActionSetCreateInfo(..)
                            , ActionSuggestedBinding(..)
                            , InteractionProfileSuggestedBinding(..)
                            , ActiveActionSet(..)
                            , SessionActionSetsAttachInfo(..)
                            , ActionsSyncInfo(..)
                            , BoundSourcesForActionEnumerateInfo(..)
                            , InputSourceLocalizedNameGetInfo(..)
                            , InteractionProfileState(..)
                            , ActionCreateInfo(..)
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
import Data.ByteString (packCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Foreign.C.Types (CChar(..))
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
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
import OpenXR.Core10.Handles (Action)
import OpenXR.Core10.Handles (Action(..))
import OpenXR.Core10.Handles (Action(Action))
import OpenXR.Core10.Handles (ActionSet)
import OpenXR.Core10.Handles (ActionSet(..))
import OpenXR.Core10.Handles (ActionSet(ActionSet))
import OpenXR.Core10.Handles (ActionSet_T)
import OpenXR.Core10.Enums.ActionType (ActionType)
import OpenXR.Core10.Handles (Action_T)
import {-# SOURCE #-} OpenXR.Extensions.XR_KHR_binding_modification (BindingModificationsKHR)
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.CStruct.Extends (Chain)
import OpenXR.CStruct.Extends (Extends)
import OpenXR.CStruct.Extends (Extendss)
import OpenXR.CStruct.Extends (Extensible(..))
import OpenXR.Core10.Enums.InputSourceLocalizedNameFlagBits (InputSourceLocalizedNameFlags)
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Dynamic (InstanceCmds(pXrAttachSessionActionSets))
import OpenXR.Dynamic (InstanceCmds(pXrCreateAction))
import OpenXR.Dynamic (InstanceCmds(pXrCreateActionSet))
import OpenXR.Dynamic (InstanceCmds(pXrDestroyAction))
import OpenXR.Dynamic (InstanceCmds(pXrDestroyActionSet))
import OpenXR.Dynamic (InstanceCmds(pXrEnumerateBoundSourcesForAction))
import OpenXR.Dynamic (InstanceCmds(pXrGetActionStateBoolean))
import OpenXR.Dynamic (InstanceCmds(pXrGetActionStateFloat))
import OpenXR.Dynamic (InstanceCmds(pXrGetActionStatePose))
import OpenXR.Dynamic (InstanceCmds(pXrGetActionStateVector2f))
import OpenXR.Dynamic (InstanceCmds(pXrGetCurrentInteractionProfile))
import OpenXR.Dynamic (InstanceCmds(pXrGetInputSourceLocalizedName))
import OpenXR.Dynamic (InstanceCmds(pXrSuggestInteractionProfileBindings))
import OpenXR.Dynamic (InstanceCmds(pXrSyncActions))
import OpenXR.Core10.Handles (Instance_T)
import {-# SOURCE #-} OpenXR.Extensions.XR_VALVE_analog_threshold (InteractionProfileAnalogThresholdVALVE)
import OpenXR.Core10.APIConstants (MAX_ACTION_NAME_SIZE)
import OpenXR.Core10.APIConstants (MAX_ACTION_SET_NAME_SIZE)
import OpenXR.Core10.APIConstants (MAX_LOCALIZED_ACTION_NAME_SIZE)
import OpenXR.Core10.APIConstants (MAX_LOCALIZED_ACTION_SET_NAME_SIZE)
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.SemanticPaths (Path)
import OpenXR.Core10.SemanticPaths (Path(..))
import OpenXR.CStruct.Extends (PeekChain)
import OpenXR.CStruct.Extends (PeekChain(..))
import OpenXR.CStruct.Extends (PokeChain)
import OpenXR.CStruct.Extends (PokeChain(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.CStruct.Extends (SomeStruct)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.FundamentalTypes (Time)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_ACTIONS_SYNC_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_ACTION_CREATE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_ACTION_SET_CREATE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_ACTION_STATE_BOOLEAN))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_ACTION_STATE_FLOAT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_ACTION_STATE_GET_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_ACTION_STATE_POSE))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_ACTION_STATE_VECTOR2F))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_INPUT_SOURCE_LOCALIZED_NAME_GET_INFO))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_INTERACTION_PROFILE_STATE))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_SESSION_ACTION_SETS_ATTACH_INFO))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetActionStateBoolean
  :: FunPtr (Ptr Session_T -> Ptr ActionStateGetInfo -> Ptr ActionStateBoolean -> IO Result) -> Ptr Session_T -> Ptr ActionStateGetInfo -> Ptr ActionStateBoolean -> IO Result

-- | xrGetActionStateBoolean - Gets boolean action state
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTION_TYPE_MISMATCH'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
-- = See Also
--
-- 'ActionStateBoolean', 'ActionStateGetInfo',
-- 'OpenXR.Core10.Handles.Session', 'createAction'
getActionStateBoolean :: forall io
                       . (MonadIO io)
                      => -- | @session@ is the 'OpenXR.Core10.Handles.Session' to query.
                         --
                         -- #VUID-xrGetActionStateBoolean-session-parameter# @session@ /must/ be a
                         -- valid 'OpenXR.Core10.Handles.Session' handle
                         Session
                      -> -- | @getInfo@ is a pointer to 'ActionStateGetInfo' to provide action and
                         -- subaction paths information.
                         --
                         -- #VUID-xrGetActionStateBoolean-getInfo-parameter# @getInfo@ /must/ be a
                         -- pointer to a valid 'ActionStateGetInfo' structure
                         ActionStateGetInfo
                      -> io (Result, ActionStateBoolean)
getActionStateBoolean session getInfo = liftIO . evalContT $ do
  let xrGetActionStateBooleanPtr = pXrGetActionStateBoolean (instanceCmds (session :: Session))
  lift $ unless (xrGetActionStateBooleanPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetActionStateBoolean is null" Nothing Nothing
  let xrGetActionStateBoolean' = mkXrGetActionStateBoolean xrGetActionStateBooleanPtr
  getInfo' <- ContT $ withCStruct (getInfo)
  pState <- ContT (withZeroCStruct @ActionStateBoolean)
  r <- lift $ traceAroundEvent "xrGetActionStateBoolean" (xrGetActionStateBoolean' (sessionHandle (session)) getInfo' (pState))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  state <- lift $ peekCStruct @ActionStateBoolean pState
  pure $ (r, state)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetActionStateFloat
  :: FunPtr (Ptr Session_T -> Ptr ActionStateGetInfo -> Ptr ActionStateFloat -> IO Result) -> Ptr Session_T -> Ptr ActionStateGetInfo -> Ptr ActionStateFloat -> IO Result

-- | xrGetActionStateFloat - Gets a floating point action state
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTION_TYPE_MISMATCH'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
-- = See Also
--
-- 'ActionStateFloat', 'ActionStateGetInfo',
-- 'OpenXR.Core10.Handles.Session', 'createAction'
getActionStateFloat :: forall io
                     . (MonadIO io)
                    => -- | @session@ is the 'OpenXR.Core10.Handles.Session' to query.
                       --
                       -- #VUID-xrGetActionStateFloat-session-parameter# @session@ /must/ be a
                       -- valid 'OpenXR.Core10.Handles.Session' handle
                       Session
                    -> -- | @getInfo@ is a pointer to 'ActionStateGetInfo' to provide action and
                       -- subaction paths information.
                       --
                       -- #VUID-xrGetActionStateFloat-getInfo-parameter# @getInfo@ /must/ be a
                       -- pointer to a valid 'ActionStateGetInfo' structure
                       ActionStateGetInfo
                    -> io (Result, ActionStateFloat)
getActionStateFloat session getInfo = liftIO . evalContT $ do
  let xrGetActionStateFloatPtr = pXrGetActionStateFloat (instanceCmds (session :: Session))
  lift $ unless (xrGetActionStateFloatPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetActionStateFloat is null" Nothing Nothing
  let xrGetActionStateFloat' = mkXrGetActionStateFloat xrGetActionStateFloatPtr
  getInfo' <- ContT $ withCStruct (getInfo)
  pState <- ContT (withZeroCStruct @ActionStateFloat)
  r <- lift $ traceAroundEvent "xrGetActionStateFloat" (xrGetActionStateFloat' (sessionHandle (session)) getInfo' (pState))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  state <- lift $ peekCStruct @ActionStateFloat pState
  pure $ (r, state)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetActionStateVector2f
  :: FunPtr (Ptr Session_T -> Ptr ActionStateGetInfo -> Ptr ActionStateVector2f -> IO Result) -> Ptr Session_T -> Ptr ActionStateGetInfo -> Ptr ActionStateVector2f -> IO Result

-- | xrGetActionStateVector2f - Gets 2D float vector action state
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTION_TYPE_MISMATCH'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
-- = See Also
--
-- 'ActionStateGetInfo', 'ActionStateVector2f',
-- 'OpenXR.Core10.Handles.Session', 'createAction'
getActionStateVector2f :: forall io
                        . (MonadIO io)
                       => -- | @session@ is the 'OpenXR.Core10.Handles.Session' to query.
                          --
                          -- #VUID-xrGetActionStateVector2f-session-parameter# @session@ /must/ be a
                          -- valid 'OpenXR.Core10.Handles.Session' handle
                          Session
                       -> -- | @getInfo@ is a pointer to 'ActionStateGetInfo' to provide action and
                          -- subaction paths information.
                          --
                          -- #VUID-xrGetActionStateVector2f-getInfo-parameter# @getInfo@ /must/ be a
                          -- pointer to a valid 'ActionStateGetInfo' structure
                          ActionStateGetInfo
                       -> io (Result, ActionStateVector2f)
getActionStateVector2f session getInfo = liftIO . evalContT $ do
  let xrGetActionStateVector2fPtr = pXrGetActionStateVector2f (instanceCmds (session :: Session))
  lift $ unless (xrGetActionStateVector2fPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetActionStateVector2f is null" Nothing Nothing
  let xrGetActionStateVector2f' = mkXrGetActionStateVector2f xrGetActionStateVector2fPtr
  getInfo' <- ContT $ withCStruct (getInfo)
  pState <- ContT (withZeroCStruct @ActionStateVector2f)
  r <- lift $ traceAroundEvent "xrGetActionStateVector2f" (xrGetActionStateVector2f' (sessionHandle (session)) getInfo' (pState))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  state <- lift $ peekCStruct @ActionStateVector2f pState
  pure $ (r, state)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetActionStatePose
  :: FunPtr (Ptr Session_T -> Ptr ActionStateGetInfo -> Ptr ActionStatePose -> IO Result) -> Ptr Session_T -> Ptr ActionStateGetInfo -> Ptr ActionStatePose -> IO Result

-- | xrGetActionStatePose - Gets metadata from a pose action
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'getActionStatePose' returns information about the binding and active
-- state for the specified action. To determine the pose of this action at
-- a historical or predicted time, the application /can/ create an action
-- space using 'OpenXR.Core10.Space.createActionSpace'. Then, after each
-- sync, the application /can/ locate the pose of this action space within
-- a base space using 'OpenXR.Core10.Space.locateSpace'.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTION_TYPE_MISMATCH'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
-- = See Also
--
-- 'ActionStateGetInfo', 'ActionStatePose',
-- 'OpenXR.Core10.Handles.Session', 'createAction',
-- 'OpenXR.Core10.Space.createActionSpace'
getActionStatePose :: forall io
                    . (MonadIO io)
                   => -- | @session@ is the 'OpenXR.Core10.Handles.Session' to query.
                      --
                      -- #VUID-xrGetActionStatePose-session-parameter# @session@ /must/ be a
                      -- valid 'OpenXR.Core10.Handles.Session' handle
                      Session
                   -> -- | @getInfo@ is a pointer to 'ActionStateGetInfo' to provide action and
                      -- subaction paths information.
                      --
                      -- #VUID-xrGetActionStatePose-getInfo-parameter# @getInfo@ /must/ be a
                      -- pointer to a valid 'ActionStateGetInfo' structure
                      ActionStateGetInfo
                   -> io (Result, ActionStatePose)
getActionStatePose session getInfo = liftIO . evalContT $ do
  let xrGetActionStatePosePtr = pXrGetActionStatePose (instanceCmds (session :: Session))
  lift $ unless (xrGetActionStatePosePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetActionStatePose is null" Nothing Nothing
  let xrGetActionStatePose' = mkXrGetActionStatePose xrGetActionStatePosePtr
  getInfo' <- ContT $ withCStruct (getInfo)
  pState <- ContT (withZeroCStruct @ActionStatePose)
  r <- lift $ traceAroundEvent "xrGetActionStatePose" (xrGetActionStatePose' (sessionHandle (session)) getInfo' (pState))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  state <- lift $ peekCStruct @ActionStatePose pState
  pure $ (r, state)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateActionSet
  :: FunPtr (Ptr Instance_T -> Ptr ActionSetCreateInfo -> Ptr (Ptr ActionSet_T) -> IO Result) -> Ptr Instance_T -> Ptr ActionSetCreateInfo -> Ptr (Ptr ActionSet_T) -> IO Result

-- | xrCreateActionSet - Creates an XrActionSet
--
-- == Parameter Descriptions
--
-- = Description
--
-- The 'createActionSet' function creates an action set and returns a
-- handle to the created action set.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_LIMIT_REACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_NAME_DUPLICATED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LOCALIZED_NAME_DUPLICATED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_NAME_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LOCALIZED_NAME_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_FORMAT_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.ActionSet', 'ActionSetCreateInfo',
-- 'OpenXR.Core10.Handles.Instance', 'destroyActionSet'
createActionSet :: forall io
                 . (MonadIO io)
                => -- | @instance@ is a handle to an 'OpenXR.Core10.Handles.Instance'.
                   --
                   -- #VUID-xrCreateActionSet-instance-parameter# @instance@ /must/ be a valid
                   -- 'OpenXR.Core10.Handles.Instance' handle
                   Instance
                -> -- | @createInfo@ is a pointer to a valid 'ActionSetCreateInfo' structure
                   -- that defines the action set being created.
                   --
                   -- #VUID-xrCreateActionSet-createInfo-parameter# @createInfo@ /must/ be a
                   -- pointer to a valid 'ActionSetCreateInfo' structure
                   ActionSetCreateInfo
                -> io (ActionSet)
createActionSet instance' createInfo = liftIO . evalContT $ do
  let cmds = instanceCmds (instance' :: Instance)
  let xrCreateActionSetPtr = pXrCreateActionSet cmds
  lift $ unless (xrCreateActionSetPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateActionSet is null" Nothing Nothing
  let xrCreateActionSet' = mkXrCreateActionSet xrCreateActionSetPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pActionSet <- ContT $ bracket (callocBytes @(Ptr ActionSet_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateActionSet" (xrCreateActionSet' (instanceHandle (instance')) createInfo' (pActionSet))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  actionSet <- lift $ peek @(Ptr ActionSet_T) pActionSet
  pure $ (((\h -> ActionSet h cmds ) actionSet))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createActionSet' and 'destroyActionSet'
--
-- To ensure that 'destroyActionSet' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withActionSet :: forall io r . MonadIO io => Instance -> ActionSetCreateInfo -> (io ActionSet -> (ActionSet -> io ()) -> r) -> r
withActionSet instance' createInfo b =
  b (createActionSet instance' createInfo)
    (\(o0) -> destroyActionSet o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrDestroyActionSet
  :: FunPtr (Ptr ActionSet_T -> IO Result) -> Ptr ActionSet_T -> IO Result

-- | xrDestroyActionSet - Destroys an XrActionSet
--
-- == Parameter Descriptions
--
-- = Description
--
-- Action set handles /can/ be destroyed by calling 'destroyActionSet'.
-- When an action set handle is destroyed, all handles of actions in that
-- action set are also destroyed.
--
-- The implementation /must/ not free underlying resources for the action
-- set while there are other valid handles that refer to those resources.
-- The implementation /may/ release resources for an action set when all of
-- the action spaces for actions in that action set have been destroyed.
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#spaces-action-spaces-lifetime Action Spaces Lifetime>
-- for details.
--
-- Resources for all action sets in an instance /must/ be freed when the
-- instance containing those actions sets is destroyed.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrDestroyActionSet-actionSet-parameter# @actionSet@ /must/ be
--     a valid 'OpenXR.Core10.Handles.ActionSet' handle
--
-- == Thread Safety
--
-- -   Access to @actionSet@, and any child handles, /must/ be externally
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
-- 'OpenXR.Core10.Handles.ActionSet', 'createActionSet'
destroyActionSet :: forall io
                  . (MonadIO io)
                 => -- | @actionSet@ is the action set to destroy.
                    ActionSet
                 -> io ()
destroyActionSet actionSet = liftIO $ do
  let xrDestroyActionSetPtr = pXrDestroyActionSet (instanceCmds (actionSet :: ActionSet))
  unless (xrDestroyActionSetPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrDestroyActionSet is null" Nothing Nothing
  let xrDestroyActionSet' = mkXrDestroyActionSet xrDestroyActionSetPtr
  r <- traceAroundEvent "xrDestroyActionSet" (xrDestroyActionSet' (actionSetHandle (actionSet)))
  when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateAction
  :: FunPtr (Ptr ActionSet_T -> Ptr ActionCreateInfo -> Ptr (Ptr Action_T) -> IO Result) -> Ptr ActionSet_T -> Ptr ActionCreateInfo -> Ptr (Ptr Action_T) -> IO Result

-- | xrCreateAction - Creates an XrAction
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'createAction' creates an action and returns its handle.
--
-- If @actionSet@ has been included in a call to 'attachSessionActionSets',
-- the implementation /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_ACTIONSETS_ALREADY_ATTACHED'.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSETS_ALREADY_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LIMIT_REACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_NAME_DUPLICATED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LOCALIZED_NAME_DUPLICATED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_NAME_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LOCALIZED_NAME_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_FORMAT_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action', 'ActionCreateInfo',
-- 'OpenXR.Core10.Handles.ActionSet',
-- 'OpenXR.Core10.Enums.ActionType.ActionType', 'destroyAction'
createAction :: forall io
              . (MonadIO io)
             => -- | @actionSet@ is a handle to an 'OpenXR.Core10.Handles.ActionSet'.
                --
                -- #VUID-xrCreateAction-actionSet-parameter# @actionSet@ /must/ be a valid
                -- 'OpenXR.Core10.Handles.ActionSet' handle
                ActionSet
             -> -- | @createInfo@ is a pointer to a valid 'ActionCreateInfo' structure that
                -- defines the action being created.
                --
                -- #VUID-xrCreateAction-createInfo-parameter# @createInfo@ /must/ be a
                -- pointer to a valid 'ActionCreateInfo' structure
                ActionCreateInfo
             -> io (Action)
createAction actionSet createInfo = liftIO . evalContT $ do
  let cmds = instanceCmds (actionSet :: ActionSet)
  let xrCreateActionPtr = pXrCreateAction cmds
  lift $ unless (xrCreateActionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateAction is null" Nothing Nothing
  let xrCreateAction' = mkXrCreateAction xrCreateActionPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pAction <- ContT $ bracket (callocBytes @(Ptr Action_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateAction" (xrCreateAction' (actionSetHandle (actionSet)) createInfo' (pAction))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  action <- lift $ peek @(Ptr Action_T) pAction
  pure $ (((\h -> Action h cmds ) action))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createAction' and 'destroyAction'
--
-- To ensure that 'destroyAction' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withAction :: forall io r . MonadIO io => ActionSet -> ActionCreateInfo -> (io Action -> (Action -> io ()) -> r) -> r
withAction actionSet createInfo b =
  b (createAction actionSet createInfo)
    (\(o0) -> destroyAction o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrDestroyAction
  :: FunPtr (Ptr Action_T -> IO Result) -> Ptr Action_T -> IO Result

-- | xrDestroyAction - Destroys an XrAction
--
-- == Parameter Descriptions
--
-- = Description
--
-- Action handles /can/ be destroyed by calling 'destroyAction'. Handles
-- for actions that are part of an action set are automatically destroyed
-- when the action set’s handle is destroyed.
--
-- The implementation /must/ not destroy the underlying resources for an
-- action when 'destroyAction' is called. Those resources are still used to
-- make
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#spaces-action-spaces-lifetime action spaces locatable>
-- and when processing action priority in 'syncActions'. Destroying the
-- action handle removes the application’s access to these resources, but
-- has no other change on actions.
--
-- Resources for all actions in an instance /must/ be freed when the
-- instance containing those actions sets is destroyed.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrDestroyAction-action-parameter# @action@ /must/ be a valid
--     'OpenXR.Core10.Handles.Action' handle
--
-- == Thread Safety
--
-- -   Access to @action@, and any child handles, /must/ be externally
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
-- 'OpenXR.Core10.Handles.Action', 'createAction'
destroyAction :: forall io
               . (MonadIO io)
              => -- | @action@ is the action to destroy.
                 Action
              -> io ()
destroyAction action = liftIO $ do
  let xrDestroyActionPtr = pXrDestroyAction (instanceCmds (action :: Action))
  unless (xrDestroyActionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrDestroyAction is null" Nothing Nothing
  let xrDestroyAction' = mkXrDestroyAction xrDestroyActionPtr
  r <- traceAroundEvent "xrDestroyAction" (xrDestroyAction' (actionHandle (action)))
  when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrSuggestInteractionProfileBindings
  :: FunPtr (Ptr Instance_T -> Ptr (SomeStruct InteractionProfileSuggestedBinding) -> IO Result) -> Ptr Instance_T -> Ptr (SomeStruct InteractionProfileSuggestedBinding) -> IO Result

-- | xrSuggestInteractionProfileBindings - Sets the application-suggested
-- bindings for the interaction profile
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'suggestInteractionProfileBindings' sets an interaction profile for
-- which the application can provide default bindings. The application
-- /can/ call 'suggestInteractionProfileBindings' once per interaction
-- profile that it supports.
--
-- The application /can/ provide any number of bindings for each action.
--
-- If the application successfully calls
-- 'suggestInteractionProfileBindings' more than once for an interaction
-- profile, the runtime /must/ discard the previous suggested bindings and
-- replace them with the new suggested bindings for that profile.
--
-- If the interaction profile path does not follow the structure defined in
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#semantic-path-interaction-profiles Interaction Profiles>
-- or suggested bindings contain paths that do not follow the format
-- defined in
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#semantic-path-input Device input subpaths>,
-- the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'. If the interaction
-- profile or input source for any of the suggested bindings does not exist
-- in the allowlist defined in
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#semantic-path-interaction-profiles Interaction Profile Paths>,
-- the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'. A runtime /must/
-- accept every valid binding in the allowlist though it is free to ignore
-- any of them.
--
-- If the action set for any action referenced in the @suggestedBindings@
-- parameter has been included in a call to 'attachSessionActionSets', the
-- implementation /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_ACTIONSETS_ALREADY_ATTACHED'.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSETS_ALREADY_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance', 'InteractionProfileSuggestedBinding'
suggestInteractionProfileBindings :: forall a io
                                   . (Extendss InteractionProfileSuggestedBinding a, PokeChain a, MonadIO io)
                                  => -- | @instance@ is the 'OpenXR.Core10.Handles.Instance' for which the
                                     -- application would like to set suggested bindings
                                     --
                                     -- #VUID-xrSuggestInteractionProfileBindings-instance-parameter# @instance@
                                     -- /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
                                     Instance
                                  -> -- | @suggestedBindings@ is the 'InteractionProfileSuggestedBinding' that the
                                     -- application would like to set
                                     --
                                     -- #VUID-xrSuggestInteractionProfileBindings-suggestedBindings-parameter#
                                     -- @suggestedBindings@ /must/ be a pointer to a valid
                                     -- 'InteractionProfileSuggestedBinding' structure
                                     ("suggestedBindings" ::: InteractionProfileSuggestedBinding a)
                                  -> io ()
suggestInteractionProfileBindings instance' suggestedBindings = liftIO . evalContT $ do
  let xrSuggestInteractionProfileBindingsPtr = pXrSuggestInteractionProfileBindings (instanceCmds (instance' :: Instance))
  lift $ unless (xrSuggestInteractionProfileBindingsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrSuggestInteractionProfileBindings is null" Nothing Nothing
  let xrSuggestInteractionProfileBindings' = mkXrSuggestInteractionProfileBindings xrSuggestInteractionProfileBindingsPtr
  suggestedBindings' <- ContT $ withCStruct (suggestedBindings)
  r <- lift $ traceAroundEvent "xrSuggestInteractionProfileBindings" (xrSuggestInteractionProfileBindings' (instanceHandle (instance')) (forgetExtensions suggestedBindings'))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrAttachSessionActionSets
  :: FunPtr (Ptr Session_T -> Ptr SessionActionSetsAttachInfo -> IO Result) -> Ptr Session_T -> Ptr SessionActionSetsAttachInfo -> IO Result

-- | xrAttachSessionActionSets - Attaches action sets to a given session
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'attachSessionActionSets' attaches the 'OpenXR.Core10.Handles.ActionSet'
-- handles in @attachInfo.actionSets@ to the @session@. Action sets /must/
-- be attached in order to be synchronized with 'syncActions'.
--
-- When an action set is attached to a session, that action set becomes
-- immutable. See 'createAction' and 'suggestInteractionProfileBindings'
-- for details.
--
-- The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_ACTIONSETS_ALREADY_ATTACHED' if
-- 'attachSessionActionSets' is called more than once for a given
-- @session@. The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED' for any action
-- created after 'attachSessionActionSets' is called for a given @session@
-- if that handle is used with any call for the same @session@.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSETS_ALREADY_ATTACHED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session', 'SessionActionSetsAttachInfo'
attachSessionActionSets :: forall io
                         . (MonadIO io)
                        => -- | @session@ is the 'OpenXR.Core10.Handles.Session' to attach the action
                           -- sets to.
                           --
                           -- #VUID-xrAttachSessionActionSets-session-parameter# @session@ /must/ be a
                           -- valid 'OpenXR.Core10.Handles.Session' handle
                           Session
                        -> -- | @attachInfo@ is the 'SessionActionSetsAttachInfo' to provide information
                           -- to attach action sets to the session.
                           --
                           -- #VUID-xrAttachSessionActionSets-attachInfo-parameter# @attachInfo@
                           -- /must/ be a pointer to a valid 'SessionActionSetsAttachInfo' structure
                           SessionActionSetsAttachInfo
                        -> io (Result)
attachSessionActionSets session attachInfo = liftIO . evalContT $ do
  let xrAttachSessionActionSetsPtr = pXrAttachSessionActionSets (instanceCmds (session :: Session))
  lift $ unless (xrAttachSessionActionSetsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrAttachSessionActionSets is null" Nothing Nothing
  let xrAttachSessionActionSets' = mkXrAttachSessionActionSets xrAttachSessionActionSetsPtr
  attachInfo' <- ContT $ withCStruct (attachInfo)
  r <- lift $ traceAroundEvent "xrAttachSessionActionSets" (xrAttachSessionActionSets' (sessionHandle (session)) attachInfo')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetCurrentInteractionProfile
  :: FunPtr (Ptr Session_T -> Path -> Ptr InteractionProfileState -> IO Result) -> Ptr Session_T -> Path -> Ptr InteractionProfileState -> IO Result

-- | xrGetCurrentInteractionProfile - Gets the current interaction profile
-- for a top level user paths
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'getCurrentInteractionProfile' asks the runtime for the active
-- interaction profiles for a top level user path.
--
-- The runtime /must/ return only interaction profiles for which the
-- application has provided bindings with
-- 'suggestInteractionProfileBindings'. The runtime /may/ return
-- interaction profiles that do not represent physically present hardware,
-- for example if the runtime is using a known interaction profile to bind
-- to hardware that the application is not aware of. The runtime /may/
-- return the last-known interaction profile in the event that no
-- controllers are active.
--
-- If 'attachSessionActionSets' has not yet been called for the @session@,
-- the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'. If
-- @topLevelUserPath@ is not one of the device input subpaths described in
-- section
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#semantic-path-user \/user paths>,
-- the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
-- = See Also
--
-- 'InteractionProfileState',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'OpenXR.Core10.Handles.Session'
getCurrentInteractionProfile :: forall io
                              . (MonadIO io)
                             => -- | @session@ is the 'OpenXR.Core10.Handles.Session' for which the
                                -- application would like to retrieve the current interaction profile.
                                --
                                -- #VUID-xrGetCurrentInteractionProfile-session-parameter# @session@ /must/
                                -- be a valid 'OpenXR.Core10.Handles.Session' handle
                                Session
                             -> -- | @topLevelUserPath@ is the top level user path the application would like
                                -- to retrieve the interaction profile for.
                                ("topLevelUserPath" ::: Path)
                             -> io (Result, InteractionProfileState)
getCurrentInteractionProfile session topLevelUserPath = liftIO . evalContT $ do
  let xrGetCurrentInteractionProfilePtr = pXrGetCurrentInteractionProfile (instanceCmds (session :: Session))
  lift $ unless (xrGetCurrentInteractionProfilePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetCurrentInteractionProfile is null" Nothing Nothing
  let xrGetCurrentInteractionProfile' = mkXrGetCurrentInteractionProfile xrGetCurrentInteractionProfilePtr
  pInteractionProfile <- ContT (withZeroCStruct @InteractionProfileState)
  r <- lift $ traceAroundEvent "xrGetCurrentInteractionProfile" (xrGetCurrentInteractionProfile' (sessionHandle (session)) (topLevelUserPath) (pInteractionProfile))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  interactionProfile <- lift $ peekCStruct @InteractionProfileState pInteractionProfile
  pure $ (r, interactionProfile)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrSyncActions
  :: FunPtr (Ptr Session_T -> Ptr ActionsSyncInfo -> IO Result) -> Ptr Session_T -> Ptr ActionsSyncInfo -> IO Result

-- | xrSyncActions - Updates the current state of input actions
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'syncActions' updates the current state of input actions. Repeated input
-- action state queries between subsequent synchronization calls /must/
-- return the same values. The 'OpenXR.Core10.Handles.ActionSet' structures
-- referenced in the @syncInfo.activeActionSets@ /must/ have been
-- previously attached to the session via 'attachSessionActionSets'. If any
-- action sets not attached to this session are passed to 'syncActions' it
-- /must/ return 'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'.
-- Subsets of the bound action sets /can/ be synchronized in order to
-- control which actions are seen as active.
--
-- If @session@ is not focused, the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.SESSION_NOT_FOCUSED', and all action states
-- in the session /must/ be inactive.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_NOT_FOCUSED'
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.ActionSet', 'ActionsSyncInfo', 'ActiveActionSet',
-- 'OpenXR.Core10.Handles.Session'
syncActions :: forall io
             . (MonadIO io)
            => -- | @session@ is a handle to the 'OpenXR.Core10.Handles.Session' that all
               -- provided action set handles belong to.
               --
               -- #VUID-xrSyncActions-session-parameter# @session@ /must/ be a valid
               -- 'OpenXR.Core10.Handles.Session' handle
               Session
            -> -- | @syncInfo@ is an 'ActionsSyncInfo' providing information to synchronize
               -- action states.
               --
               -- #VUID-xrSyncActions-syncInfo-parameter# @syncInfo@ /must/ be a pointer
               -- to a valid 'ActionsSyncInfo' structure
               ActionsSyncInfo
            -> io (Result)
syncActions session syncInfo = liftIO . evalContT $ do
  let xrSyncActionsPtr = pXrSyncActions (instanceCmds (session :: Session))
  lift $ unless (xrSyncActionsPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrSyncActions is null" Nothing Nothing
  let xrSyncActions' = mkXrSyncActions xrSyncActionsPtr
  syncInfo' <- ContT $ withCStruct (syncInfo)
  r <- lift $ traceAroundEvent "xrSyncActions" (xrSyncActions' (sessionHandle (session)) syncInfo')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrEnumerateBoundSourcesForAction
  :: FunPtr (Ptr Session_T -> Ptr BoundSourcesForActionEnumerateInfo -> Word32 -> Ptr Word32 -> Ptr Path -> IO Result) -> Ptr Session_T -> Ptr BoundSourcesForActionEnumerateInfo -> Word32 -> Ptr Word32 -> Ptr Path -> IO Result

-- | xrEnumerateBoundSourcesForAction - Queries the bound input sources for
-- an action
--
-- == Parameter Descriptions
--
-- -   @session@ is the 'OpenXR.Core10.Handles.Session' being queried.
--
-- -   @enumerateInfo@ is an 'BoundSourcesForActionEnumerateInfo' providing
--     the query information.
--
-- -   @sourceCapacityInput@ is the capacity of the array, or 0 to indicate
--     a request to retrieve the required capacity.
--
-- -   @sourceCountOutput@ is a pointer to the count of sources, or a
--     pointer to the required capacity in the case that
--     @sourceCapacityInput@ is 0.
--
-- -   @sources@ is a pointer to an application-allocated array that will
--     be filled with the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
--     values for all sources. It /can/ be @NULL@ if @sourceCapacityInput@
--     is 0.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @sources@ size.
--
-- = Description
--
-- If an action is unbound, 'enumerateBoundSourcesForAction' /must/ assign
-- @0@ to the value pointed-to by @sourceCountOutput@ and not modify the
-- array.
--
-- 'enumerateBoundSourcesForAction' /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED' if passed an
-- action in an action set never attached to the session with
-- 'attachSessionActionSets'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrEnumerateBoundSourcesForAction-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrEnumerateBoundSourcesForAction-enumerateInfo-parameter#
--     @enumerateInfo@ /must/ be a pointer to a valid
--     'BoundSourcesForActionEnumerateInfo' structure
--
-- -   #VUID-xrEnumerateBoundSourcesForAction-sourceCountOutput-parameter#
--     @sourceCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrEnumerateBoundSourcesForAction-sources-parameter# If
--     @sourceCapacityInput@ is not @0@, @sources@ /must/ be a pointer to
--     an array of @sourceCapacityInput@
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
--     values
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action', 'BoundSourcesForActionEnumerateInfo',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'OpenXR.Core10.Handles.Session'
enumerateBoundSourcesForAction :: forall io
                                . (MonadIO io)
                               => -- No documentation found for Nested "xrEnumerateBoundSourcesForAction" "session"
                                  Session
                               -> -- No documentation found for Nested "xrEnumerateBoundSourcesForAction" "enumerateInfo"
                                  BoundSourcesForActionEnumerateInfo
                               -> io (Result, ("sources" ::: Vector Path))
enumerateBoundSourcesForAction session enumerateInfo = liftIO . evalContT $ do
  let xrEnumerateBoundSourcesForActionPtr = pXrEnumerateBoundSourcesForAction (instanceCmds (session :: Session))
  lift $ unless (xrEnumerateBoundSourcesForActionPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrEnumerateBoundSourcesForAction is null" Nothing Nothing
  let xrEnumerateBoundSourcesForAction' = mkXrEnumerateBoundSourcesForAction xrEnumerateBoundSourcesForActionPtr
  let session' = sessionHandle (session)
  enumerateInfo' <- ContT $ withCStruct (enumerateInfo)
  pSourceCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrEnumerateBoundSourcesForAction" (xrEnumerateBoundSourcesForAction' session' enumerateInfo' (0) (pSourceCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  sourceCountOutput <- lift $ peek @Word32 pSourceCountOutput
  pSources <- ContT $ bracket (callocBytes @Path ((fromIntegral (sourceCountOutput)) * 8)) free
  r' <- lift $ traceAroundEvent "xrEnumerateBoundSourcesForAction" (xrEnumerateBoundSourcesForAction' session' enumerateInfo' ((sourceCountOutput)) (pSourceCountOutput) (pSources))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  sourceCountOutput' <- lift $ peek @Word32 pSourceCountOutput
  sources' <- lift $ generateM (fromIntegral (sourceCountOutput')) (\i -> peek @Path ((pSources `advancePtrBytes` (8 * (i)) :: Ptr Path)))
  pure $ ((r'), sources')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetInputSourceLocalizedName
  :: FunPtr (Ptr Session_T -> Ptr InputSourceLocalizedNameGetInfo -> Word32 -> Ptr Word32 -> Ptr CChar -> IO Result) -> Ptr Session_T -> Ptr InputSourceLocalizedNameGetInfo -> Word32 -> Ptr Word32 -> Ptr CChar -> IO Result

-- | xrGetInputSourceLocalizedName - Gets a localized source name
--
-- == Parameter Descriptions
--
-- -   @session@ is a handle to the 'OpenXR.Core10.Handles.Session'
--     associated with the action that reported this source.
--
-- -   @getInfo@ is an 'InputSourceLocalizedNameGetInfo' providing the
--     query information.
--
-- -   @bufferCapacityInput@ is the capacity of the buffer, or 0 to
--     indicate a request to retrieve the required capacity.
--
-- -   @bufferCountOutput@ is a pointer to the count of name characters
--     written (including the terminating @\\0@), or a pointer to the
--     required capacity in the case that @bufferCapacityInput@ is 0.
--
-- -   @buffer@ is a pointer to an application-allocated buffer that will
--     be filled with the source name. It /can/ be @NULL@ if
--     @bufferCapacityInput@ is 0.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @buffer@ size.
--
-- = Description
--
-- 'getInputSourceLocalizedName' returns a string for the input source in
-- the current system locale.
--
-- If 'attachSessionActionSets' has not yet been called for the session,
-- the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetInputSourceLocalizedName-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrGetInputSourceLocalizedName-getInfo-parameter# @getInfo@
--     /must/ be a pointer to a valid 'InputSourceLocalizedNameGetInfo'
--     structure
--
-- -   #VUID-xrGetInputSourceLocalizedName-bufferCountOutput-parameter#
--     @bufferCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrGetInputSourceLocalizedName-buffer-parameter# If
--     @bufferCapacityInput@ is not @0@, @buffer@ /must/ be a pointer to an
--     array of @bufferCapacityInput@ char values
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ACTIONSET_NOT_ATTACHED'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action', 'InputSourceLocalizedNameGetInfo',
-- 'OpenXR.Core10.Handles.Session'
getInputSourceLocalizedName :: forall io
                             . (MonadIO io)
                            => -- No documentation found for Nested "xrGetInputSourceLocalizedName" "session"
                               Session
                            -> -- No documentation found for Nested "xrGetInputSourceLocalizedName" "getInfo"
                               InputSourceLocalizedNameGetInfo
                            -> io (Result, ("buffer" ::: ByteString))
getInputSourceLocalizedName session getInfo = liftIO . evalContT $ do
  let xrGetInputSourceLocalizedNamePtr = pXrGetInputSourceLocalizedName (instanceCmds (session :: Session))
  lift $ unless (xrGetInputSourceLocalizedNamePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetInputSourceLocalizedName is null" Nothing Nothing
  let xrGetInputSourceLocalizedName' = mkXrGetInputSourceLocalizedName xrGetInputSourceLocalizedNamePtr
  let session' = sessionHandle (session)
  getInfo' <- ContT $ withCStruct (getInfo)
  pBufferCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrGetInputSourceLocalizedName" (xrGetInputSourceLocalizedName' session' getInfo' (0) (pBufferCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  bufferCountOutput <- lift $ peek @Word32 pBufferCountOutput
  pBuffer <- ContT $ bracket (callocBytes @CChar (fromIntegral (bufferCountOutput))) free
  r' <- lift $ traceAroundEvent "xrGetInputSourceLocalizedName" (xrGetInputSourceLocalizedName' session' getInfo' ((bufferCountOutput)) (pBufferCountOutput) (pBuffer))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  buffer' <- lift $ packCString pBuffer
  pure $ ((r'), buffer')


-- | XrVector2f - Two-dimensional vector
--
-- == Member Descriptions
--
-- = Description
--
-- If used to represent physical distances (rather than e.g. normalized
-- direction) and not otherwise specified, values /must/ be in meters.
--
-- = See Also
--
-- 'ActionStateVector2f',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect.CompositionLayerEquirectKHR',
-- 'OpenXR.Core10.Space.Posef', 'OpenXR.Core10.Space.Quaternionf',
-- 'OpenXR.Core10.Space.Vector3f', 'OpenXR.Core10.OtherTypes.Vector4f',
-- 'OpenXR.Extensions.XR_KHR_visibility_mask.VisibilityMaskKHR',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceStateVector2fEXT xrSetInputDeviceStateVector2fEXT>
data Vector2f = Vector2f
  { -- | @x@ is the x coordinate of the vector.
    x :: Float
  , -- | @y@ is the y coordinate of the vector.
    y :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Vector2f)
#endif
deriving instance Show Vector2f

instance ToCStruct Vector2f where
  withCStruct x f = allocaBytesAligned 8 4 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Vector2f{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (x))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (y))
    f
  cStructSize = 8
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct Vector2f where
  peekCStruct p = do
    x <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    y <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    pure $ Vector2f
             (coerce @CFloat @Float x) (coerce @CFloat @Float y)

instance Storable Vector2f where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Vector2f where
  zero = Vector2f
           zero
           zero


-- | XrActionStateBoolean - Boolean action state
--
-- == Member Descriptions
--
-- = Description
--
-- When multiple input sources are bound to this action, the @currentState@
-- follows
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#multiple_inputs the previously defined rule to resolve ambiguity>.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'getActionStateBoolean'
data ActionStateBoolean = ActionStateBoolean
  { -- | @currentState@ is the current state of the action.
    currentState :: Bool
  , -- | @changedSinceLastSync@ is 'OpenXR.Core10.FundamentalTypes.TRUE' if the
    -- value of @currentState@ is different than it was before the most recent
    -- call to 'syncActions'. This parameter can be combined with
    -- @currentState@ to detect rising and falling edges since the previous
    -- call to 'syncActions'. E.g. if both @changedSinceLastSync@ and
    -- @currentState@ are 'OpenXR.Core10.FundamentalTypes.TRUE' then a rising
    -- edge ('OpenXR.Core10.FundamentalTypes.FALSE' to
    -- 'OpenXR.Core10.FundamentalTypes.TRUE') has taken place.
    changedSinceLastSync :: Bool
  , -- | @lastChangeTime@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- when this action’s value last changed.
    lastChangeTime :: Time
  , -- | @isActive@ is 'OpenXR.Core10.FundamentalTypes.TRUE' if and only if there
    -- exists an input source that is contributing to the current state of this
    -- action.
    isActive :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActionStateBoolean)
#endif
deriving instance Show ActionStateBoolean

instance ToCStruct ActionStateBoolean where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActionStateBoolean{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_STATE_BOOLEAN)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (currentState))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (changedSinceLastSync))
    poke ((p `plusPtr` 24 :: Ptr Time)) (lastChangeTime)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (isActive))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_STATE_BOOLEAN)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Time)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct ActionStateBoolean where
  peekCStruct p = do
    currentState <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    changedSinceLastSync <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    lastChangeTime <- peek @Time ((p `plusPtr` 24 :: Ptr Time))
    isActive <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ ActionStateBoolean
             (bool32ToBool currentState) (bool32ToBool changedSinceLastSync) lastChangeTime (bool32ToBool isActive)

instance Storable ActionStateBoolean where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ActionStateBoolean where
  zero = ActionStateBoolean
           zero
           zero
           zero
           zero


-- | XrActionStateFloat - Floating point action state
--
-- == Member Descriptions
--
-- = Description
--
-- When multiple input sources are bound to this action, the @currentState@
-- follows
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#multiple_inputs the previously defined rule to resolve ambiguity>.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'getActionStateFloat'
data ActionStateFloat = ActionStateFloat
  { -- | @currentState@ is the current state of the Action.
    currentState :: Float
  , -- | @changedSinceLastSync@ is 'OpenXR.Core10.FundamentalTypes.TRUE' if the
    -- value of @currentState@ is different than it was before the most recent
    -- call to 'syncActions'.
    changedSinceLastSync :: Bool
  , -- | @lastChangeTime@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- in nanoseconds since this action’s value last changed.
    lastChangeTime :: Time
  , -- | @isActive@ is 'OpenXR.Core10.FundamentalTypes.TRUE' if and only if there
    -- exists an input source that is contributing to the current state of this
    -- action.
    isActive :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActionStateFloat)
#endif
deriving instance Show ActionStateFloat

instance ToCStruct ActionStateFloat where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActionStateFloat{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_STATE_FLOAT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (currentState))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (changedSinceLastSync))
    poke ((p `plusPtr` 24 :: Ptr Time)) (lastChangeTime)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (isActive))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_STATE_FLOAT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Time)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct ActionStateFloat where
  peekCStruct p = do
    currentState <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    changedSinceLastSync <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    lastChangeTime <- peek @Time ((p `plusPtr` 24 :: Ptr Time))
    isActive <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ ActionStateFloat
             (coerce @CFloat @Float currentState) (bool32ToBool changedSinceLastSync) lastChangeTime (bool32ToBool isActive)

instance Storable ActionStateFloat where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ActionStateFloat where
  zero = ActionStateFloat
           zero
           zero
           zero
           zero


-- | XrActionStateVector2f - 2D float vector action state
--
-- == Member Descriptions
--
-- = Description
--
-- When multiple input sources are bound to this action, the @currentState@
-- follows
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#multiple_inputs the previously defined rule to resolve ambiguity>.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >,
-- 'Vector2f', 'getActionStateVector2f'
data ActionStateVector2f = ActionStateVector2f
  { -- | @currentState@ is the current 'Vector2f' state of the Action.
    currentState :: Vector2f
  , -- | @changedSinceLastSync@ is 'OpenXR.Core10.FundamentalTypes.TRUE' if the
    -- value of @currentState@ is different than it was before the most recent
    -- call to 'syncActions'.
    changedSinceLastSync :: Bool
  , -- | @lastChangeTime@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrTime >
    -- in nanoseconds since this action’s value last changed.
    lastChangeTime :: Time
  , -- | @isActive@ is 'OpenXR.Core10.FundamentalTypes.TRUE' if and only if there
    -- exists an input source that is contributing to the current state of this
    -- action.
    isActive :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActionStateVector2f)
#endif
deriving instance Show ActionStateVector2f

instance ToCStruct ActionStateVector2f where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActionStateVector2f{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_STATE_VECTOR2F)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Vector2f)) (currentState)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (changedSinceLastSync))
    poke ((p `plusPtr` 32 :: Ptr Time)) (lastChangeTime)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (isActive))
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_STATE_VECTOR2F)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Vector2f)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Time)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct ActionStateVector2f where
  peekCStruct p = do
    currentState <- peekCStruct @Vector2f ((p `plusPtr` 16 :: Ptr Vector2f))
    changedSinceLastSync <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    lastChangeTime <- peek @Time ((p `plusPtr` 32 :: Ptr Time))
    isActive <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    pure $ ActionStateVector2f
             currentState (bool32ToBool changedSinceLastSync) lastChangeTime (bool32ToBool isActive)

instance Storable ActionStateVector2f where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ActionStateVector2f where
  zero = ActionStateVector2f
           zero
           zero
           zero
           zero


-- | XrActionStatePose - Pose action metadata
--
-- == Member Descriptions
--
-- = Description
--
-- A pose action /must/ not be bound to multiple input sources, according
-- to
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#multiple_inputs the previously defined rule>.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'getActionStatePose'
data ActionStatePose = ActionStatePose
  { -- | @isActive@ is 'OpenXR.Core10.FundamentalTypes.TRUE' if and only if there
    -- exists an input source that is being tracked by this pose action.
    isActive :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActionStatePose)
#endif
deriving instance Show ActionStatePose

instance ToCStruct ActionStatePose where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActionStatePose{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_STATE_POSE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (isActive))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_STATE_POSE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct ActionStatePose where
  peekCStruct p = do
    isActive <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ ActionStatePose
             (bool32ToBool isActive)

instance Storable ActionStatePose where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ActionStatePose where
  zero = ActionStatePose
           zero


-- | XrActionStateGetInfo - Information to get action state
--
-- == Member Descriptions
--
-- = Description
--
-- See 'ActionCreateInfo' for a description of subaction paths, and the
-- restrictions on their use.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'getActionStateBoolean', 'getActionStateFloat', 'getActionStatePose',
-- 'getActionStateVector2f'
data ActionStateGetInfo = ActionStateGetInfo
  { -- | @action@ is the 'OpenXR.Core10.Handles.Action' being queried.
    --
    -- #VUID-XrActionStateGetInfo-action-parameter# @action@ /must/ be a valid
    -- 'OpenXR.Core10.Handles.Action' handle
    action :: Ptr Action_T
  , -- | @subactionPath@ is the subaction path
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
    -- to query data from, or 'OpenXR.Core10.APIConstants.NULL_PATH' to specify
    -- all subaction paths. If the subaction path is specified, it is one of
    -- the subaction paths that were specified when the action was created. If
    -- the subaction path was not specified when the action was created, the
    -- runtime /must/ return
    -- 'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'. If this parameter
    -- is specified, the runtime /must/ return data that originates only from
    -- the subaction paths specified.
    subactionPath :: Path
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActionStateGetInfo)
#endif
deriving instance Show ActionStateGetInfo

instance ToCStruct ActionStateGetInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActionStateGetInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_STATE_GET_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Action_T))) (action)
    poke ((p `plusPtr` 24 :: Ptr Path)) (subactionPath)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_STATE_GET_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Action_T))) (zero)
    f

instance FromCStruct ActionStateGetInfo where
  peekCStruct p = do
    action <- peek @(Ptr Action_T) ((p `plusPtr` 16 :: Ptr (Ptr Action_T)))
    subactionPath <- peek @Path ((p `plusPtr` 24 :: Ptr Path))
    pure $ ActionStateGetInfo
             action subactionPath

instance Storable ActionStateGetInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ActionStateGetInfo where
  zero = ActionStateGetInfo
           zero
           zero


-- | XrActionSetCreateInfo - XrActionSet creation info
--
-- == Member Descriptions
--
-- = Description
--
-- When multiple actions are bound to the same input source, the @priority@
-- of each action set determines which bindings are suppressed. Runtimes
-- /must/ ignore input sources from action sets with a lower priority
-- number if those specific input sources are also present in active
-- actions within a higher priority action set. If multiple action sets
-- with the same priority are bound to the same input source and that is
-- the highest priority number, runtimes /must/ process all those bindings
-- at the same time.
--
-- Two actions are considered to be bound to the same input source if they
-- use the same
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#semantic-path-input identifier and optional location>
-- path segments, even if they have different component segments.
--
-- When runtimes are ignoring bindings because of priority, they /must/
-- treat the binding to that input source as though they do not exist. That
-- means the @isActive@ field /must/ be
-- 'OpenXR.Core10.FundamentalTypes.FALSE' when retrieving action data, and
-- that the runtime /must/ not provide any visual, haptic, or other
-- feedback related to the binding of that action to that input source.
-- Other actions in the same action set which are bound to input sources
-- that do not collide are not affected and are processed as normal.
--
-- If @actionSetName@ or @localizedActionSetName@ are empty strings, the
-- runtime /must/ return 'OpenXR.Core10.Enums.Result.ERROR_NAME_INVALID' or
-- 'OpenXR.Core10.Enums.Result.ERROR_LOCALIZED_NAME_INVALID' respectively.
-- If @actionSetName@ or @localizedActionSetName@ are duplicates of the
-- corresponding field for any existing action set in the specified
-- instance, the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_NAME_DUPLICATED' or
-- 'OpenXR.Core10.Enums.Result.ERROR_LOCALIZED_NAME_DUPLICATED'
-- respectively. If the conflicting action set is destroyed, the
-- conflicting field is no longer considered duplicated. If @actionSetName@
-- contains characters which are not allowed in a single level of a
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#well-formed-path-strings well-formed path string>,
-- the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_PATH_FORMAT_INVALID'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.ActionSet',
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'createActionSet'
data ActionSetCreateInfo = ActionSetCreateInfo
  { -- | @actionSetName@ is an array containing a @NULL@ terminated non-empty
    -- string with the name of this action set.
    --
    -- #VUID-XrActionSetCreateInfo-actionSetName-parameter# @actionSetName@
    -- /must/ be a null-terminated UTF-8 string whose length is less than or
    -- equal to 'OpenXR.Core10.APIConstants.MAX_ACTION_SET_NAME_SIZE'
    actionSetName :: ByteString
  , -- | @localizedActionSetName@ is an array containing a @NULL@ terminated
    -- @UTF@-8 string that can be presented to the user as a description of the
    -- action set. This string should be presented in the system’s current
    -- active locale.
    --
    -- #VUID-XrActionSetCreateInfo-localizedActionSetName-parameter#
    -- @localizedActionSetName@ /must/ be a null-terminated UTF-8 string whose
    -- length is less than or equal to
    -- 'OpenXR.Core10.APIConstants.MAX_LOCALIZED_ACTION_SET_NAME_SIZE'
    localizedActionSetName :: ByteString
  , -- | @priority@ defines which action sets\' actions are active on a given
    -- input source when actions on multiple active action sets are bound to
    -- the same input source. Larger priority numbers take precedence over
    -- smaller priority numbers.
    priority :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActionSetCreateInfo)
#endif
deriving instance Show ActionSetCreateInfo

instance ToCStruct ActionSetCreateInfo where
  withCStruct x f = allocaBytesAligned 216 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActionSetCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_SET_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_ACTION_SET_NAME_SIZE CChar))) (actionSetName)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 80 :: Ptr (FixedArray MAX_LOCALIZED_ACTION_SET_NAME_SIZE CChar))) (localizedActionSetName)
    poke ((p `plusPtr` 208 :: Ptr Word32)) (priority)
    f
  cStructSize = 216
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_SET_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_ACTION_SET_NAME_SIZE CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 80 :: Ptr (FixedArray MAX_LOCALIZED_ACTION_SET_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 208 :: Ptr Word32)) (zero)
    f

instance FromCStruct ActionSetCreateInfo where
  peekCStruct p = do
    actionSetName <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_ACTION_SET_NAME_SIZE CChar))))
    localizedActionSetName <- packCString (lowerArrayPtr ((p `plusPtr` 80 :: Ptr (FixedArray MAX_LOCALIZED_ACTION_SET_NAME_SIZE CChar))))
    priority <- peek @Word32 ((p `plusPtr` 208 :: Ptr Word32))
    pure $ ActionSetCreateInfo
             actionSetName localizedActionSetName priority

instance Storable ActionSetCreateInfo where
  sizeOf ~_ = 216
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ActionSetCreateInfo where
  zero = ActionSetCreateInfo
           mempty
           mempty
           zero


-- | XrActionSuggestedBinding - Suggested binding for a single action
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action', 'InteractionProfileSuggestedBinding',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'suggestInteractionProfileBindings'
data ActionSuggestedBinding = ActionSuggestedBinding
  { -- | @action@ is the 'OpenXR.Core10.Handles.Action' handle for an action
    --
    -- #VUID-XrActionSuggestedBinding-action-parameter# @action@ /must/ be a
    -- valid 'OpenXR.Core10.Handles.Action' handle
    action :: Ptr Action_T
  , -- | @binding@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
    -- of a binding for the action specified in @action@. This path is any top
    -- level user path plus input source path, for example
    -- \/user\/hand\/right\/input\/trigger\/click. See
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#input-suggested-bindings suggested bindings>
    -- for more details.
    binding :: Path
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActionSuggestedBinding)
#endif
deriving instance Show ActionSuggestedBinding

instance ToCStruct ActionSuggestedBinding where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActionSuggestedBinding{..} f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr Action_T))) (action)
    poke ((p `plusPtr` 8 :: Ptr Path)) (binding)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr Action_T))) (zero)
    poke ((p `plusPtr` 8 :: Ptr Path)) (zero)
    f

instance FromCStruct ActionSuggestedBinding where
  peekCStruct p = do
    action <- peek @(Ptr Action_T) ((p `plusPtr` 0 :: Ptr (Ptr Action_T)))
    binding <- peek @Path ((p `plusPtr` 8 :: Ptr Path))
    pure $ ActionSuggestedBinding
             action binding

instance Storable ActionSuggestedBinding where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ActionSuggestedBinding where
  zero = ActionSuggestedBinding
           zero
           zero


-- | XrInteractionProfileSuggestedBinding - Suggested bindings for a
-- interaction profile
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrInteractionProfileSuggestedBinding-type-type# @type@ /must/
--     be
--     'OpenXR.Core10.Enums.StructureType.TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING'
--
-- -   #VUID-XrInteractionProfileSuggestedBinding-next-next# @next@ /must/
--     be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>.
--     See also:
--     'OpenXR.Extensions.XR_KHR_binding_modification.BindingModificationsKHR',
--     'OpenXR.Extensions.XR_VALVE_analog_threshold.InteractionProfileAnalogThresholdVALVE'
--
-- -   #VUID-XrInteractionProfileSuggestedBinding-suggestedBindings-parameter#
--     @suggestedBindings@ /must/ be a pointer to an array of
--     @countSuggestedBindings@ valid 'ActionSuggestedBinding' structures
--
-- -   #VUID-XrInteractionProfileSuggestedBinding-countSuggestedBindings-arraylength#
--     The @countSuggestedBindings@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- 'ActionSuggestedBinding',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'suggestInteractionProfileBindings'
data InteractionProfileSuggestedBinding (es :: [Type]) = InteractionProfileSuggestedBinding
  { -- | @next@ is @NULL@ or a pointer to the next structure in a structure
    -- chain. No such structures are defined in core OpenXR.
    next :: Chain es
  , -- | @interactionProfile@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
    -- of an interaction profile.
    interactionProfile :: Path
  , -- | @suggestedBindings@ is a pointer to an array of 'ActionSuggestedBinding'
    -- structures that define all of the application’s suggested bindings for
    -- the specified interaction profile.
    suggestedBindings :: Vector ActionSuggestedBinding
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InteractionProfileSuggestedBinding (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (InteractionProfileSuggestedBinding es)

instance Extensible InteractionProfileSuggestedBinding where
  extensibleTypeName = "InteractionProfileSuggestedBinding"
  setNext x next = x{next = next}
  getNext InteractionProfileSuggestedBinding{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends InteractionProfileSuggestedBinding e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @BindingModificationsKHR = Just f
    | Just Refl <- eqT @e @InteractionProfileAnalogThresholdVALVE = Just f
    | otherwise = Nothing

instance (Extendss InteractionProfileSuggestedBinding es, PokeChain es) => ToCStruct (InteractionProfileSuggestedBinding es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p InteractionProfileSuggestedBinding{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING)
    next'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) next''
    lift $ poke ((p `plusPtr` 16 :: Ptr Path)) (interactionProfile)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (suggestedBindings)) :: Word32))
    pSuggestedBindings' <- ContT $ allocaBytesAligned @ActionSuggestedBinding ((Data.Vector.length (suggestedBindings)) * 16) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pSuggestedBindings' `plusPtr` (16 * (i)) :: Ptr ActionSuggestedBinding) (e)) (suggestedBindings)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr ActionSuggestedBinding))) (pSuggestedBindings')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INTERACTION_PROFILE_SUGGESTED_BINDING)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Path)) (zero)
    lift $ f

instance (Extendss InteractionProfileSuggestedBinding es, PeekChain es) => FromCStruct (InteractionProfileSuggestedBinding es) where
  peekCStruct p = do
    next <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next' <- peekChain (castPtr next)
    interactionProfile <- peek @Path ((p `plusPtr` 16 :: Ptr Path))
    countSuggestedBindings <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    suggestedBindings <- peek @(Ptr ActionSuggestedBinding) ((p `plusPtr` 32 :: Ptr (Ptr ActionSuggestedBinding)))
    suggestedBindings' <- generateM (fromIntegral countSuggestedBindings) (\i -> peekCStruct @ActionSuggestedBinding ((suggestedBindings `advancePtrBytes` (16 * (i)) :: Ptr ActionSuggestedBinding)))
    pure $ InteractionProfileSuggestedBinding
             next' interactionProfile suggestedBindings'

instance es ~ '[] => Zero (InteractionProfileSuggestedBinding es) where
  zero = InteractionProfileSuggestedBinding
           ()
           zero
           mempty


-- | XrActiveActionSet - Describes an active action set
--
-- == Member Descriptions
--
-- = Description
--
-- This structure defines a single active action set and subaction path
-- combination. Applications /can/ provide a list of these structures to
-- the 'syncActions' function.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.ActionSet', 'ActionsSyncInfo',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'syncActions'
data ActiveActionSet = ActiveActionSet
  { -- | @actionSet@ is the handle of the action set to activate.
    --
    -- #VUID-XrActiveActionSet-actionSet-parameter# @actionSet@ /must/ be a
    -- valid 'OpenXR.Core10.Handles.ActionSet' handle
    actionSet :: Ptr ActionSet_T
  , -- | @subactionPath@ is a subaction path that was declared when one or more
    -- actions in the action set was created or
    -- 'OpenXR.Core10.APIConstants.NULL_PATH'. If the application wants to
    -- activate the action set on more than one subaction path, it /can/
    -- include additional 'ActiveActionSet' structs with the other
    -- @subactionPath@ values. Using 'OpenXR.Core10.APIConstants.NULL_PATH' as
    -- the value for @subactionPath@, acts as a wildcard for all subaction
    -- paths on the actions in the action set. If the subaction path was not
    -- specified on any of the actions in the actionSet when that action was
    -- created, the runtime /must/ return
    -- 'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'.
    subactionPath :: Path
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActiveActionSet)
#endif
deriving instance Show ActiveActionSet

instance ToCStruct ActiveActionSet where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActiveActionSet{..} f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr ActionSet_T))) (actionSet)
    poke ((p `plusPtr` 8 :: Ptr Path)) (subactionPath)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr ActionSet_T))) (zero)
    poke ((p `plusPtr` 8 :: Ptr Path)) (zero)
    f

instance FromCStruct ActiveActionSet where
  peekCStruct p = do
    actionSet <- peek @(Ptr ActionSet_T) ((p `plusPtr` 0 :: Ptr (Ptr ActionSet_T)))
    subactionPath <- peek @Path ((p `plusPtr` 8 :: Ptr Path))
    pure $ ActiveActionSet
             actionSet subactionPath

instance Storable ActiveActionSet where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ActiveActionSet where
  zero = ActiveActionSet
           zero
           zero


-- | XrSessionActionSetsAttachInfo - Information to attach action sets to a
-- session
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrSessionActionSetsAttachInfo-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_SESSION_ACTION_SETS_ATTACH_INFO'
--
-- -   #VUID-XrSessionActionSetsAttachInfo-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrSessionActionSetsAttachInfo-actionSets-parameter#
--     @actionSets@ /must/ be a pointer to an array of @countActionSets@
--     valid 'OpenXR.Core10.Handles.ActionSet' handles
--
-- -   #VUID-XrSessionActionSetsAttachInfo-countActionSets-arraylength# The
--     @countActionSets@ parameter /must/ be greater than @0@
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.ActionSet',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'attachSessionActionSets'
data SessionActionSetsAttachInfo = SessionActionSetsAttachInfo
  { -- | @actionSets@ is a pointer to an array of one or more
    -- 'OpenXR.Core10.Handles.ActionSet' handles to be attached to the session.
    actionSets :: Vector (Ptr ActionSet_T) }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SessionActionSetsAttachInfo)
#endif
deriving instance Show SessionActionSetsAttachInfo

instance ToCStruct SessionActionSetsAttachInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SessionActionSetsAttachInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SESSION_ACTION_SETS_ATTACH_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (actionSets)) :: Word32))
    pActionSets' <- ContT $ allocaBytesAligned @(Ptr ActionSet_T) ((Data.Vector.length (actionSets)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pActionSets' `plusPtr` (8 * (i)) :: Ptr (Ptr ActionSet_T)) (e)) (actionSets)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (Ptr ActionSet_T)))) (pActionSets')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_SESSION_ACTION_SETS_ATTACH_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SessionActionSetsAttachInfo where
  peekCStruct p = do
    countActionSets <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    actionSets <- peek @(Ptr (Ptr ActionSet_T)) ((p `plusPtr` 24 :: Ptr (Ptr (Ptr ActionSet_T))))
    actionSets' <- generateM (fromIntegral countActionSets) (\i -> peek @(Ptr ActionSet_T) ((actionSets `advancePtrBytes` (8 * (i)) :: Ptr (Ptr ActionSet_T))))
    pure $ SessionActionSetsAttachInfo
             actionSets'

instance Zero SessionActionSetsAttachInfo where
  zero = SessionActionSetsAttachInfo
           mempty


-- | XrActionsSyncInfo - Information to sync actions
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrActionsSyncInfo-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_ACTIONS_SYNC_INFO'
--
-- -   #VUID-XrActionsSyncInfo-next-next# @next@ /must/ be @NULL@ or a
--     valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrActionsSyncInfo-activeActionSets-parameter# If
--     @countActiveActionSets@ is not @0@, @activeActionSets@ /must/ be a
--     pointer to an array of @countActiveActionSets@ valid
--     'ActiveActionSet' structures
--
-- = See Also
--
-- 'ActiveActionSet', 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'syncActions'
data ActionsSyncInfo = ActionsSyncInfo
  { -- | @countActiveActionSets@ is an integer specifying the number of valid
    -- elements in the @activeActionSets@ array.
    countActiveActionSets :: Word32
  , -- | @activeActionSets@ is @NULL@ or a pointer to an array of one or more
    -- 'ActiveActionSet' structures that should be synchronized.
    activeActionSets :: Vector ActiveActionSet
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActionsSyncInfo)
#endif
deriving instance Show ActionsSyncInfo

instance ToCStruct ActionsSyncInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActionsSyncInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTIONS_SYNC_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let activeActionSetsLength = Data.Vector.length $ (activeActionSets)
    countActiveActionSets'' <- lift $ if (countActiveActionSets) == 0
      then pure $ fromIntegral activeActionSetsLength
      else do
        unless (fromIntegral activeActionSetsLength == (countActiveActionSets) || activeActionSetsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "activeActionSets must be empty or have 'countActiveActionSets' elements" Nothing Nothing
        pure (countActiveActionSets)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (countActiveActionSets'')
    activeActionSets'' <- if Data.Vector.null (activeActionSets)
      then pure nullPtr
      else do
        pActiveActionSets <- ContT $ allocaBytesAligned @ActiveActionSet (((Data.Vector.length (activeActionSets))) * 16) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pActiveActionSets `plusPtr` (16 * (i)) :: Ptr ActiveActionSet) (e)) ((activeActionSets))
        pure $ pActiveActionSets
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ActiveActionSet))) activeActionSets''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTIONS_SYNC_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ActionsSyncInfo where
  peekCStruct p = do
    countActiveActionSets <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    activeActionSets <- peek @(Ptr ActiveActionSet) ((p `plusPtr` 24 :: Ptr (Ptr ActiveActionSet)))
    let activeActionSetsLength = if activeActionSets == nullPtr then 0 else (fromIntegral countActiveActionSets)
    activeActionSets' <- generateM activeActionSetsLength (\i -> peekCStruct @ActiveActionSet ((activeActionSets `advancePtrBytes` (16 * (i)) :: Ptr ActiveActionSet)))
    pure $ ActionsSyncInfo
             countActiveActionSets activeActionSets'

instance Zero ActionsSyncInfo where
  zero = ActionsSyncInfo
           zero
           mempty


-- | XrBoundSourcesForActionEnumerateInfo - Information to query the bound
-- input sources for an action
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'enumerateBoundSourcesForAction'
data BoundSourcesForActionEnumerateInfo = BoundSourcesForActionEnumerateInfo
  { -- | @action@ is the handle of the action to query.
    --
    -- #VUID-XrBoundSourcesForActionEnumerateInfo-action-parameter# @action@
    -- /must/ be a valid 'OpenXR.Core10.Handles.Action' handle
    action :: Ptr Action_T }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BoundSourcesForActionEnumerateInfo)
#endif
deriving instance Show BoundSourcesForActionEnumerateInfo

instance ToCStruct BoundSourcesForActionEnumerateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BoundSourcesForActionEnumerateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Action_T))) (action)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_BOUND_SOURCES_FOR_ACTION_ENUMERATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr Action_T))) (zero)
    f

instance FromCStruct BoundSourcesForActionEnumerateInfo where
  peekCStruct p = do
    action <- peek @(Ptr Action_T) ((p `plusPtr` 16 :: Ptr (Ptr Action_T)))
    pure $ BoundSourcesForActionEnumerateInfo
             action

instance Storable BoundSourcesForActionEnumerateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BoundSourcesForActionEnumerateInfo where
  zero = BoundSourcesForActionEnumerateInfo
           zero


-- | XrInputSourceLocalizedNameGetInfo - Information to query the bound input
-- sources for an action
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.InputSourceLocalizedNameFlagBits.InputSourceLocalizedNameFlags',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'enumerateBoundSourcesForAction', 'getInputSourceLocalizedName'
data InputSourceLocalizedNameGetInfo = InputSourceLocalizedNameGetInfo
  { -- No documentation found for Nested "XrInputSourceLocalizedNameGetInfo" "sourcePath"
    sourcePath :: Path
  , -- | @whichComponents@ is any set of flags from
    -- 'OpenXR.Core10.Enums.InputSourceLocalizedNameFlagBits.InputSourceLocalizedNameFlagBits'.
    --
    -- #VUID-XrInputSourceLocalizedNameGetInfo-whichComponents-parameter#
    -- @whichComponents@ /must/ be a valid combination of
    -- 'OpenXR.Core10.Enums.InputSourceLocalizedNameFlagBits.InputSourceLocalizedNameFlagBits'
    -- values
    --
    -- #VUID-XrInputSourceLocalizedNameGetInfo-whichComponents-requiredbitmask#
    -- @whichComponents@ /must/ not be @0@
    whichComponents :: InputSourceLocalizedNameFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InputSourceLocalizedNameGetInfo)
#endif
deriving instance Show InputSourceLocalizedNameGetInfo

instance ToCStruct InputSourceLocalizedNameGetInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p InputSourceLocalizedNameGetInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INPUT_SOURCE_LOCALIZED_NAME_GET_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Path)) (sourcePath)
    poke ((p `plusPtr` 24 :: Ptr InputSourceLocalizedNameFlags)) (whichComponents)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INPUT_SOURCE_LOCALIZED_NAME_GET_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Path)) (zero)
    poke ((p `plusPtr` 24 :: Ptr InputSourceLocalizedNameFlags)) (zero)
    f

instance FromCStruct InputSourceLocalizedNameGetInfo where
  peekCStruct p = do
    sourcePath <- peek @Path ((p `plusPtr` 16 :: Ptr Path))
    whichComponents <- peek @InputSourceLocalizedNameFlags ((p `plusPtr` 24 :: Ptr InputSourceLocalizedNameFlags))
    pure $ InputSourceLocalizedNameGetInfo
             sourcePath whichComponents

instance Storable InputSourceLocalizedNameGetInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero InputSourceLocalizedNameGetInfo where
  zero = InputSourceLocalizedNameGetInfo
           zero
           zero


-- | XrInteractionProfileState - Receives active interaction profile for a
-- top level path
--
-- == Member Descriptions
--
-- = Description
--
-- The runtime /must/ only include interaction profiles that the
-- application has provided bindings for via
-- 'suggestInteractionProfileBindings' or
-- 'OpenXR.Core10.APIConstants.NULL_PATH'. If the runtime is rebinding an
-- interaction profile provided by the application to a device that the
-- application did not provide bindings for, it /must/ return the
-- interaction profile path that it is emulating. If the runtime is unable
-- to provide input because it cannot emulate any of the
-- application-provided interaction profiles, it /must/ return
-- 'OpenXR.Core10.APIConstants.NULL_PATH'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'ActionSuggestedBinding',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'getCurrentInteractionProfile', 'suggestInteractionProfileBindings'
data InteractionProfileState = InteractionProfileState
  { -- | @interactionProfile@ is the
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
    -- of the interaction profile path for the @topLevelUserPath@ used to
    -- retrieve this state, or 'OpenXR.Core10.APIConstants.NULL_PATH' if there
    -- is no active interaction profile at that top level user path.
    interactionProfile :: Path }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InteractionProfileState)
#endif
deriving instance Show InteractionProfileState

instance ToCStruct InteractionProfileState where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p InteractionProfileState{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INTERACTION_PROFILE_STATE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Path)) (interactionProfile)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INTERACTION_PROFILE_STATE)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Path)) (zero)
    f

instance FromCStruct InteractionProfileState where
  peekCStruct p = do
    interactionProfile <- peek @Path ((p `plusPtr` 16 :: Ptr Path))
    pure $ InteractionProfileState
             interactionProfile

instance Storable InteractionProfileState where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero InteractionProfileState where
  zero = InteractionProfileState
           zero


-- | XrActionCreateInfo - XrAction creation info
--
-- == Member Descriptions
--
-- = Description
--
-- Subaction paths are a mechanism that enables applications to use the
-- same action name and handle on multiple devices. Applications can query
-- action state using subaction paths that differentiate data coming from
-- each device. This allows the runtime to group logically equivalent
-- actions together in system UI. For instance, an application could create
-- a single @pick_up@ action with the \/user\/hand\/left and
-- \/user\/hand\/right subaction paths and use the subaction paths to
-- independently query the state of @pick_up_with_left_hand@ and
-- @pick_up_with_right_hand@.
--
-- Applications /can/ create actions with or without the @subactionPaths@
-- set to a list of paths. If this list of paths is omitted (i.e.
-- @subactionPaths@ is set to @NULL@, and @countSubactionPaths@ is set to
-- @0@), the application is opting out of filtering action results by
-- subaction paths and any call to get action data must also omit subaction
-- paths.
--
-- If @subactionPaths@ is specified and any of the following conditions are
-- not satisfied, the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED':
--
-- -   Each path provided is one of:
--
--     -   \/user\/head
--
--     -   \/user\/hand\/left
--
--     -   \/user\/hand\/right
--
--     -   \/user\/gamepad
--
-- -   No path appears in the list more than once
--
-- Extensions /may/ append additional top level user paths to the above
-- list.
--
-- Note
--
-- Earlier revisions of the spec mentioned \/user but it could not be
-- implemented as specified and was removed as errata.
--
-- The runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED' in the following
-- circumstances:
--
-- -   The application specified subaction paths at action creation and the
--     application called @xrGetActionState*@ or a haptic function with an
--     empty subaction path array.
--
-- -   The application called @xrGetActionState*@ or a haptic function with
--     a subaction path that was not specified when the action was created.
--
-- If @actionName@ or @localizedActionName@ are empty strings, the runtime
-- /must/ return 'OpenXR.Core10.Enums.Result.ERROR_NAME_INVALID' or
-- 'OpenXR.Core10.Enums.Result.ERROR_LOCALIZED_NAME_INVALID' respectively.
-- If @actionName@ or @localizedActionName@ are duplicates of the
-- corresponding field for any existing action in the specified action set,
-- the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_NAME_DUPLICATED' or
-- 'OpenXR.Core10.Enums.Result.ERROR_LOCALIZED_NAME_DUPLICATED'
-- respectively. If the conflicting action is destroyed, the conflicting
-- field is no longer considered duplicated. If @actionName@ contains
-- characters which are not allowed in a single level of a
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#well-formed-path-strings well-formed path string>,
-- the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_PATH_FORMAT_INVALID'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrActionCreateInfo-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_ACTION_CREATE_INFO'
--
-- -   #VUID-XrActionCreateInfo-next-next# @next@ /must/ be @NULL@ or a
--     valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrActionCreateInfo-actionName-parameter# @actionName@ /must/
--     be a null-terminated UTF-8 string whose length is less than or equal
--     to 'OpenXR.Core10.APIConstants.MAX_ACTION_NAME_SIZE'
--
-- -   #VUID-XrActionCreateInfo-actionType-parameter# @actionType@ /must/
--     be a valid 'OpenXR.Core10.Enums.ActionType.ActionType' value
--
-- -   #VUID-XrActionCreateInfo-subactionPaths-parameter# If
--     @countSubactionPaths@ is not @0@, @subactionPaths@ /must/ be a
--     pointer to an array of @countSubactionPaths@ valid
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
--     values
--
-- -   #VUID-XrActionCreateInfo-localizedActionName-parameter#
--     @localizedActionName@ /must/ be a null-terminated UTF-8 string whose
--     length is less than or equal to
--     'OpenXR.Core10.APIConstants.MAX_LOCALIZED_ACTION_NAME_SIZE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Action',
-- 'OpenXR.Core10.Enums.ActionType.ActionType',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType', 'createAction',
-- 'createActionSet'
data ActionCreateInfo = ActionCreateInfo
  { -- | @actionName@ is an array containing a @NULL@ terminated string with the
    -- name of this action.
    actionName :: ByteString
  , -- | @actionType@ is the 'OpenXR.Core10.Enums.ActionType.ActionType' of the
    -- action to be created.
    actionType :: ActionType
  , -- | @countSubactionPaths@ is the number of elements in the @subactionPaths@
    -- array. If @subactionPaths@ is NULL, this parameter must be 0.
    countSubactionPaths :: Word32
  , -- | @subactionPaths@ is an array of
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
    -- or @NULL@. If this array is specified, it contains one or more subaction
    -- paths that the application intends to query action state for.
    subactionPaths :: Vector Path
  , -- | @localizedActionName@ is an array containing a @NULL@ terminated @UTF@-8
    -- string that can be presented to the user as a description of the action.
    -- This string should be in the system’s current active locale.
    localizedActionName :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ActionCreateInfo)
#endif
deriving instance Show ActionCreateInfo

instance ToCStruct ActionCreateInfo where
  withCStruct x f = allocaBytesAligned 224 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ActionCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_ACTION_NAME_SIZE CChar))) (actionName)
    lift $ poke ((p `plusPtr` 80 :: Ptr ActionType)) (actionType)
    let subactionPathsLength = Data.Vector.length $ (subactionPaths)
    countSubactionPaths'' <- lift $ if (countSubactionPaths) == 0
      then pure $ fromIntegral subactionPathsLength
      else do
        unless (fromIntegral subactionPathsLength == (countSubactionPaths) || subactionPathsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "subactionPaths must be empty or have 'countSubactionPaths' elements" Nothing Nothing
        pure (countSubactionPaths)
    lift $ poke ((p `plusPtr` 84 :: Ptr Word32)) (countSubactionPaths'')
    subactionPaths'' <- if Data.Vector.null (subactionPaths)
      then pure nullPtr
      else do
        pSubactionPaths <- ContT $ allocaBytesAligned @Path (((Data.Vector.length (subactionPaths))) * 8) 8
        lift $ Data.Vector.imapM_ (\i e -> poke (pSubactionPaths `plusPtr` (8 * (i)) :: Ptr Path) (e)) ((subactionPaths))
        pure $ pSubactionPaths
    lift $ poke ((p `plusPtr` 88 :: Ptr (Ptr Path))) subactionPaths''
    lift $ pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 96 :: Ptr (FixedArray MAX_LOCALIZED_ACTION_NAME_SIZE CChar))) (localizedActionName)
    lift $ f
  cStructSize = 224
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_ACTION_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_ACTION_NAME_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 80 :: Ptr ActionType)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 96 :: Ptr (FixedArray MAX_LOCALIZED_ACTION_NAME_SIZE CChar))) (mempty)
    f

instance FromCStruct ActionCreateInfo where
  peekCStruct p = do
    actionName <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_ACTION_NAME_SIZE CChar))))
    actionType <- peek @ActionType ((p `plusPtr` 80 :: Ptr ActionType))
    countSubactionPaths <- peek @Word32 ((p `plusPtr` 84 :: Ptr Word32))
    subactionPaths <- peek @(Ptr Path) ((p `plusPtr` 88 :: Ptr (Ptr Path)))
    let subactionPathsLength = if subactionPaths == nullPtr then 0 else (fromIntegral countSubactionPaths)
    subactionPaths' <- generateM subactionPathsLength (\i -> peek @Path ((subactionPaths `advancePtrBytes` (8 * (i)) :: Ptr Path)))
    localizedActionName <- packCString (lowerArrayPtr ((p `plusPtr` 96 :: Ptr (FixedArray MAX_LOCALIZED_ACTION_NAME_SIZE CChar))))
    pure $ ActionCreateInfo
             actionName actionType countSubactionPaths subactionPaths' localizedActionName

instance Zero ActionCreateInfo where
  zero = ActionCreateInfo
           mempty
           zero
           zero
           mempty
           mempty

