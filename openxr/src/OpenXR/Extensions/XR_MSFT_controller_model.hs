{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_controller_model - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_controller_model  XR_MSFT_controller_model>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 56
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
-- 'MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT',
-- 'OpenXR.Core10.APIConstants.NULL_CONTROLLER_MODEL_KEY_MSFT',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrControllerModelKeyMSFT >,
-- 'ControllerModelKeyStateMSFT', 'ControllerModelNodePropertiesMSFT',
-- 'ControllerModelNodeStateMSFT', 'ControllerModelPropertiesMSFT',
-- 'ControllerModelStateMSFT', 'getControllerModelKeyMSFT',
-- 'getControllerModelPropertiesMSFT', 'getControllerModelStateMSFT',
-- 'loadControllerModelMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_controller_model OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_controller_model  ( getControllerModelKeyMSFT
                                                   , loadControllerModelMSFT
                                                   , getControllerModelPropertiesMSFT
                                                   , getControllerModelStateMSFT
                                                   , ControllerModelKeyMSFT(..)
                                                   , ControllerModelKeyStateMSFT(..)
                                                   , ControllerModelNodePropertiesMSFT(..)
                                                   , ControllerModelPropertiesMSFT(..)
                                                   , ControllerModelNodeStateMSFT(..)
                                                   , ControllerModelStateMSFT(..)
                                                   , MSFT_controller_model_SPEC_VERSION
                                                   , pattern MSFT_controller_model_SPEC_VERSION
                                                   , MSFT_CONTROLLER_MODEL_EXTENSION_NAME
                                                   , pattern MSFT_CONTROLLER_MODEL_EXTENSION_NAME
                                                   , MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT
                                                   , pattern MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT
                                                   , NULL_CONTROLLER_MODEL_KEY_MSFT
                                                   , pattern NULL_CONTROLLER_MODEL_KEY_MSFT
                                                   ) where

import OpenXR.CStruct.Utils (FixedArray)
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
import Data.String (IsString)
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
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import OpenXR.CStruct.Utils (advancePtrBytes)
import OpenXR.CStruct.Utils (lowerArrayPtr)
import OpenXR.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import OpenXR.NamedType ((:::))
import OpenXR.Dynamic (InstanceCmds(pXrGetControllerModelKeyMSFT))
import OpenXR.Dynamic (InstanceCmds(pXrGetControllerModelPropertiesMSFT))
import OpenXR.Dynamic (InstanceCmds(pXrGetControllerModelStateMSFT))
import OpenXR.Dynamic (InstanceCmds(pXrLoadControllerModelMSFT))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.SemanticPaths (Path)
import OpenXR.Core10.SemanticPaths (Path(..))
import OpenXR.Core10.Space (Posef)
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_CONTROLLER_MODEL_STATE_MSFT))
import OpenXR.Core10.APIConstants (NULL_CONTROLLER_MODEL_KEY_MSFT)
import OpenXR.Core10.APIConstants (pattern NULL_CONTROLLER_MODEL_KEY_MSFT)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetControllerModelKeyMSFT
  :: FunPtr (Ptr Session_T -> Path -> Ptr ControllerModelKeyStateMSFT -> IO Result) -> Ptr Session_T -> Path -> Ptr ControllerModelKeyStateMSFT -> IO Result

-- | xrGetControllerModelKeyMSFT - Retrieve the model key for the controller
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetControllerModelKeyMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'getControllerModelKeyMSFT'
--
-- -   #VUID-xrGetControllerModelKeyMSFT-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrGetControllerModelKeyMSFT-controllerModelKeyState-parameter#
--     @controllerModelKeyState@ /must/ be a pointer to an
--     'ControllerModelKeyStateMSFT' structure
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_UNSUPPORTED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
-- = See Also
--
-- 'ControllerModelKeyStateMSFT',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'OpenXR.Core10.Handles.Session'
getControllerModelKeyMSFT :: forall io
                           . (MonadIO io)
                          => -- | @session@ is the specified 'OpenXR.Core10.Handles.Session'.
                             Session
                          -> -- | @topLevelUserPath@ is the top level user path corresponding to the
                             -- controller render model being queried (e.g. \/user\/hand\/left or
                             -- \/user\/hand\/right).
                             ("topLevelUserPath" ::: Path)
                          -> io (ControllerModelKeyStateMSFT)
getControllerModelKeyMSFT session topLevelUserPath = liftIO . evalContT $ do
  let xrGetControllerModelKeyMSFTPtr = pXrGetControllerModelKeyMSFT (instanceCmds (session :: Session))
  lift $ unless (xrGetControllerModelKeyMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetControllerModelKeyMSFT is null" Nothing Nothing
  let xrGetControllerModelKeyMSFT' = mkXrGetControllerModelKeyMSFT xrGetControllerModelKeyMSFTPtr
  pControllerModelKeyState <- ContT (withZeroCStruct @ControllerModelKeyStateMSFT)
  r <- lift $ traceAroundEvent "xrGetControllerModelKeyMSFT" (xrGetControllerModelKeyMSFT' (sessionHandle (session)) (topLevelUserPath) (pControllerModelKeyState))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  controllerModelKeyState <- lift $ peekCStruct @ControllerModelKeyStateMSFT pControllerModelKeyState
  pure $ (controllerModelKeyState)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrLoadControllerModelMSFT
  :: FunPtr (Ptr Session_T -> ControllerModelKeyMSFT -> Word32 -> Ptr Word32 -> Ptr Word8 -> IO Result) -> Ptr Session_T -> ControllerModelKeyMSFT -> Word32 -> Ptr Word32 -> Ptr Word8 -> IO Result

-- | xrLoadControllerModelMSFT - Load controller render model
--
-- == Parameter Descriptions
--
-- -   @session@ is the specified 'OpenXR.Core10.Handles.Session'.
--
-- -   @modelKey@ is the model key corresponding to the controller render
--     model being queried.
--
-- -   @bufferCapacityInput@ is the capacity of the @buffer@ array, or 0 to
--     indicate a request to retrieve the required capacity.
--
-- -   @bufferCountOutput@ filled in by the runtime with the count of
--     elements in @buffer@ array, or returns the required capacity in the
--     case that @bufferCapacityInput@ is 0.
--
-- -   @buffer@ is a pointer to an application-allocated array of the model
--     for the device that will be filled with the @uint8_t@ values by the
--     runtime. It /can/ be @NULL@ if @bufferCapacityInput@ is 0.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @buffer@ size.
--
-- = Description
--
-- The 'loadControllerModelMSFT' function /may/ be a slow operation and
-- therefore /should/ be invoked from a non-timing critical thread.
--
-- If the input @modelKey@ is invalid, i.e. it is
-- 'OpenXR.Core10.APIConstants.NULL_CONTROLLER_MODEL_KEY_MSFT' or not a key
-- returned from 'ControllerModelKeyStateMSFT', the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrLoadControllerModelMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'loadControllerModelMSFT'
--
-- -   #VUID-xrLoadControllerModelMSFT-session-parameter# @session@ /must/
--     be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrLoadControllerModelMSFT-bufferCountOutput-parameter#
--     @bufferCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrLoadControllerModelMSFT-buffer-parameter# If
--     @bufferCapacityInput@ is not @0@, @buffer@ /must/ be a pointer to an
--     array of @bufferCapacityInput@ @uint8_t@ values
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrControllerModelKeyMSFT >,
-- 'OpenXR.Core10.Handles.Session'
loadControllerModelMSFT :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "xrLoadControllerModelMSFT" "session"
                           Session
                        -> -- No documentation found for Nested "xrLoadControllerModelMSFT" "modelKey"
                           ControllerModelKeyMSFT
                        -> io (("buffer" ::: Vector Word8))
loadControllerModelMSFT session modelKey = liftIO . evalContT $ do
  let xrLoadControllerModelMSFTPtr = pXrLoadControllerModelMSFT (instanceCmds (session :: Session))
  lift $ unless (xrLoadControllerModelMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrLoadControllerModelMSFT is null" Nothing Nothing
  let xrLoadControllerModelMSFT' = mkXrLoadControllerModelMSFT xrLoadControllerModelMSFTPtr
  let session' = sessionHandle (session)
  pBufferCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrLoadControllerModelMSFT" (xrLoadControllerModelMSFT' session' (modelKey) (0) (pBufferCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  bufferCountOutput <- lift $ peek @Word32 pBufferCountOutput
  pBuffer <- ContT $ bracket (callocBytes @Word8 (fromIntegral (bufferCountOutput))) free
  r' <- lift $ traceAroundEvent "xrLoadControllerModelMSFT" (xrLoadControllerModelMSFT' session' (modelKey) ((bufferCountOutput)) (pBufferCountOutput) (pBuffer))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  bufferCountOutput' <- lift $ peek @Word32 pBufferCountOutput
  buffer' <- lift $ generateM (fromIntegral (bufferCountOutput')) (\i -> peek @Word8 ((pBuffer `advancePtrBytes` (1 * (i)) :: Ptr Word8)))
  pure $ (buffer')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetControllerModelPropertiesMSFT
  :: FunPtr (Ptr Session_T -> ControllerModelKeyMSFT -> Ptr ControllerModelPropertiesMSFT -> IO Result) -> Ptr Session_T -> ControllerModelKeyMSFT -> Ptr ControllerModelPropertiesMSFT -> IO Result

-- | xrGetControllerModelPropertiesMSFT - Get controller model properties
--
-- == Parameter Descriptions
--
-- = Description
--
-- The runtime /must/ return the same data in
-- 'ControllerModelPropertiesMSFT' for a valid @modelKey@. Therefore, the
-- application /can/ cache the returned 'ControllerModelPropertiesMSFT'
-- using @modelKey@ and reuse the data for each frame.
--
-- If the input @modelKey@ is invalid, i.e. it is
-- 'OpenXR.Core10.APIConstants.NULL_CONTROLLER_MODEL_KEY_MSFT' or not a key
-- returned from 'ControllerModelKeyStateMSFT', the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetControllerModelPropertiesMSFT-extension-notenabled# The
--     @@ extension /must/ be enabled prior to calling
--     'getControllerModelPropertiesMSFT'
--
-- -   #VUID-xrGetControllerModelPropertiesMSFT-session-parameter#
--     @session@ /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrGetControllerModelPropertiesMSFT-properties-parameter#
--     @properties@ /must/ be a pointer to an
--     'ControllerModelPropertiesMSFT' structure
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrControllerModelKeyMSFT >,
-- 'ControllerModelPropertiesMSFT', 'OpenXR.Core10.Handles.Session'
getControllerModelPropertiesMSFT :: forall io
                                  . (MonadIO io)
                                 => -- | @session@ is the specified 'OpenXR.Core10.Handles.Session'.
                                    Session
                                 -> -- | @modelKey@ is a valid model key obtained from
                                    -- 'ControllerModelKeyStateMSFT'
                                    ControllerModelKeyMSFT
                                 -> io (ControllerModelPropertiesMSFT)
getControllerModelPropertiesMSFT session modelKey = liftIO . evalContT $ do
  let xrGetControllerModelPropertiesMSFTPtr = pXrGetControllerModelPropertiesMSFT (instanceCmds (session :: Session))
  lift $ unless (xrGetControllerModelPropertiesMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetControllerModelPropertiesMSFT is null" Nothing Nothing
  let xrGetControllerModelPropertiesMSFT' = mkXrGetControllerModelPropertiesMSFT xrGetControllerModelPropertiesMSFTPtr
  pProperties <- ContT (withZeroCStruct @ControllerModelPropertiesMSFT)
  r <- lift $ traceAroundEvent "xrGetControllerModelPropertiesMSFT" (xrGetControllerModelPropertiesMSFT' (sessionHandle (session)) (modelKey) (pProperties))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  properties <- lift $ peekCStruct @ControllerModelPropertiesMSFT pProperties
  pure $ (properties)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrGetControllerModelStateMSFT
  :: FunPtr (Ptr Session_T -> ControllerModelKeyMSFT -> Ptr ControllerModelStateMSFT -> IO Result) -> Ptr Session_T -> ControllerModelKeyMSFT -> Ptr ControllerModelStateMSFT -> IO Result

-- | xrGetControllerModelStateMSFT - Get controller model state
--
-- == Parameter Descriptions
--
-- = Description
--
-- The runtime /may/ return different state for a model key after each call
-- to 'OpenXR.Core10.Input.syncActions', which represents the latest state
-- of the user interactions.
--
-- If the input @modelKey@ is invalid, i.e. it is
-- 'OpenXR.Core10.APIConstants.NULL_CONTROLLER_MODEL_KEY_MSFT' or not a key
-- returned from 'ControllerModelKeyStateMSFT', the runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrGetControllerModelStateMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'getControllerModelStateMSFT'
--
-- -   #VUID-xrGetControllerModelStateMSFT-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrGetControllerModelStateMSFT-state-parameter# @state@ /must/
--     be a pointer to an 'ControllerModelStateMSFT' structure
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_CONTROLLER_MODEL_KEY_INVALID_MSFT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.SESSION_LOSS_PENDING'
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrControllerModelKeyMSFT >,
-- 'ControllerModelStateMSFT', 'OpenXR.Core10.Handles.Session'
getControllerModelStateMSFT :: forall io
                             . (MonadIO io)
                            => -- | @session@ is the specified 'OpenXR.Core10.Handles.Session'.
                               Session
                            -> -- | @modelKey@ is the model key corresponding to the controller model being
                               -- queried.
                               ControllerModelKeyMSFT
                            -> io (ControllerModelStateMSFT)
getControllerModelStateMSFT session modelKey = liftIO . evalContT $ do
  let xrGetControllerModelStateMSFTPtr = pXrGetControllerModelStateMSFT (instanceCmds (session :: Session))
  lift $ unless (xrGetControllerModelStateMSFTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrGetControllerModelStateMSFT is null" Nothing Nothing
  let xrGetControllerModelStateMSFT' = mkXrGetControllerModelStateMSFT xrGetControllerModelStateMSFTPtr
  pState <- ContT (withZeroCStruct @ControllerModelStateMSFT)
  r <- lift $ traceAroundEvent "xrGetControllerModelStateMSFT" (xrGetControllerModelStateMSFT' (sessionHandle (session)) (modelKey) (pState))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  state <- lift $ peekCStruct @ControllerModelStateMSFT pState
  pure $ (state)


-- | XrControllerModelKeyMSFT - Controller renderable model key
--
-- = Description
--
-- The controller model key used to retrieve the data for the renderable
-- controller model and associated properties and state.
--
-- = See Also
--
-- 'OpenXR.Core10.APIConstants.NULL_CONTROLLER_MODEL_KEY_MSFT',
-- 'ControllerModelKeyStateMSFT', 'getControllerModelPropertiesMSFT',
-- 'getControllerModelStateMSFT', 'loadControllerModelMSFT'
newtype ControllerModelKeyMSFT = ControllerModelKeyMSFT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
instance Show ControllerModelKeyMSFT where
  showsPrec p (ControllerModelKeyMSFT x) = showParen (p >= 11) (showString "ControllerModelKeyMSFT 0x" . showHex x)


-- | XrControllerModelKeyStateMSFT - The model key state for a controller
--
-- == Parameter Descriptions
--
-- = Description
--
-- The @modelKey@ value for the session represents a unique controller
-- model that can be retrieved from 'loadControllerModelMSFT' function.
-- Therefore, the application /can/ use @modelKey@ to cache the returned
-- data from 'loadControllerModelMSFT' for the session.
--
-- A @modelKey@ value of
-- 'OpenXR.Core10.APIConstants.NULL_CONTROLLER_MODEL_KEY_MSFT', represents
-- an invalid model key and indicates there is no controller model yet
-- available. The application /should/ keep calling
-- 'getControllerModelKeyMSFT' because the model /may/ become available at
-- a later point.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrControllerModelKeyStateMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'ControllerModelKeyStateMSFT'
--
-- -   #VUID-XrControllerModelKeyStateMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT'
--
-- -   #VUID-XrControllerModelKeyStateMSFT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrControllerModelKeyMSFT >,
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'getControllerModelKeyMSFT'
data ControllerModelKeyStateMSFT = ControllerModelKeyStateMSFT
  { -- | @modelKey@ is the model key corresponding to the controller render model
    -- being queried.
    modelKey :: ControllerModelKeyMSFT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ControllerModelKeyStateMSFT)
#endif
deriving instance Show ControllerModelKeyStateMSFT

instance ToCStruct ControllerModelKeyStateMSFT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ControllerModelKeyStateMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ControllerModelKeyMSFT)) (modelKey)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_CONTROLLER_MODEL_KEY_STATE_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ControllerModelKeyMSFT)) (zero)
    f

instance FromCStruct ControllerModelKeyStateMSFT where
  peekCStruct p = do
    modelKey <- peek @ControllerModelKeyMSFT ((p `plusPtr` 16 :: Ptr ControllerModelKeyMSFT))
    pure $ ControllerModelKeyStateMSFT
             modelKey

instance Storable ControllerModelKeyStateMSFT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ControllerModelKeyStateMSFT where
  zero = ControllerModelKeyStateMSFT
           zero


-- | XrControllerModelNodePropertiesMSFT - Describes the controller model
-- node properties
--
-- == Parameter Descriptions
--
-- = Description
--
-- The node can be located in the glTF node hierarchy by finding the
-- node(s) with the matching node name and parent node name. If the
-- @parentNodeName@ is empty, the matching will be solely based on the
-- @nodeName@.
--
-- If there are multiple nodes in the glTF file matches the condition
-- above, the first matching node using depth-first traversal in the glTF
-- scene /should/ be animated and the rest /should/ be ignored.
--
-- The runtime /must/ not return any @nodeName@ or @parentName@ that
-- doesn’t match any gltTF nodes in the corresponding controller model.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrControllerModelNodePropertiesMSFT-extension-notenabled# The
--     @@ extension /must/ be enabled prior to using
--     'ControllerModelNodePropertiesMSFT'
--
-- -   #VUID-XrControllerModelNodePropertiesMSFT-type-type# @type@ /must/
--     be
--     'OpenXR.Core10.Enums.StructureType.TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT'
--
-- -   #VUID-XrControllerModelNodePropertiesMSFT-next-next# @next@ /must/
--     be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrControllerModelNodePropertiesMSFT-parentNodeName-parameter#
--     @parentNodeName@ /must/ be a null-terminated UTF-8 string whose
--     length is less than or equal to
--     XR_MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT
--
-- -   #VUID-XrControllerModelNodePropertiesMSFT-nodeName-parameter#
--     @nodeName@ /must/ be a null-terminated UTF-8 string whose length is
--     less than or equal to XR_MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT
--
-- = See Also
--
-- 'ControllerModelPropertiesMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'getControllerModelPropertiesMSFT', 'loadControllerModelMSFT'
data ControllerModelNodePropertiesMSFT = ControllerModelNodePropertiesMSFT
  { -- | @parentNodeName@ is the name of the parent node in the provided glTF
    -- file. The parent name /may/ be empty if it should not be used to locate
    -- this node.
    parentNodeName :: ByteString
  , -- | @nodeName@ is the name of this node in the provided glTF file.
    nodeName :: ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ControllerModelNodePropertiesMSFT)
#endif
deriving instance Show ControllerModelNodePropertiesMSFT

instance ToCStruct ControllerModelNodePropertiesMSFT where
  withCStruct x f = allocaBytesAligned 144 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ControllerModelNodePropertiesMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT CChar))) (parentNodeName)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 80 :: Ptr (FixedArray MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT CChar))) (nodeName)
    f
  cStructSize = 144
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_CONTROLLER_MODEL_NODE_PROPERTIES_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT CChar))) (mempty)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 80 :: Ptr (FixedArray MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT CChar))) (mempty)
    f

instance FromCStruct ControllerModelNodePropertiesMSFT where
  peekCStruct p = do
    parentNodeName <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT CChar))))
    nodeName <- packCString (lowerArrayPtr ((p `plusPtr` 80 :: Ptr (FixedArray MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT CChar))))
    pure $ ControllerModelNodePropertiesMSFT
             parentNodeName nodeName

instance Storable ControllerModelNodePropertiesMSFT where
  sizeOf ~_ = 144
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ControllerModelNodePropertiesMSFT where
  zero = ControllerModelNodePropertiesMSFT
           mempty
           mempty


-- | XrControllerModelPropertiesMSFT - Describes the properties of a
-- controller model
--
-- == Parameter Descriptions
--
-- -   @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of
--     this structure.
--
-- -   @next@ is @NULL@ or a pointer to the next structure in a structure
--     chain.
--
-- -   @nodeCapacityInput@ is the capacity of the @nodeProperties@ array,
--     or 0 to indicate a request to retrieve the required capacity.
--
-- -   @nodeCountOutput@ filled in by the runtime with the count of
--     elements in @nodeProperties@ array, or returns the required capacity
--     in the case that @nodeCapacityInput@ is 0.
--
-- -   @nodeProperties@ is a pointer to an application-allocated array that
--     will be filled with the 'ControllerModelNodePropertiesMSFT' values.
--     It /can/ be @NULL@ if @nodeCapacityInput@ is 0.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @nodeProperties@ size.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrControllerModelPropertiesMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'ControllerModelPropertiesMSFT'
--
-- -   #VUID-XrControllerModelPropertiesMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT'
--
-- -   #VUID-XrControllerModelPropertiesMSFT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrControllerModelPropertiesMSFT-nodeProperties-parameter# If
--     @nodeCapacityInput@ is not @0@, @nodeProperties@ /must/ be a pointer
--     to an array of @nodeCapacityInput@
--     'ControllerModelNodePropertiesMSFT' structures
--
-- = See Also
--
-- 'ControllerModelNodePropertiesMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'getControllerModelPropertiesMSFT', 'loadControllerModelMSFT'
data ControllerModelPropertiesMSFT = ControllerModelPropertiesMSFT
  { -- No documentation found for Nested "XrControllerModelPropertiesMSFT" "nodeCapacityInput"
    nodeCapacityInput :: Word32
  , -- No documentation found for Nested "XrControllerModelPropertiesMSFT" "nodeCountOutput"
    nodeCountOutput :: Word32
  , -- No documentation found for Nested "XrControllerModelPropertiesMSFT" "nodeProperties"
    nodeProperties :: Ptr ControllerModelNodePropertiesMSFT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ControllerModelPropertiesMSFT)
#endif
deriving instance Show ControllerModelPropertiesMSFT

instance ToCStruct ControllerModelPropertiesMSFT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ControllerModelPropertiesMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (nodeCapacityInput)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (nodeCountOutput)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ControllerModelNodePropertiesMSFT))) (nodeProperties)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_CONTROLLER_MODEL_PROPERTIES_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ControllerModelPropertiesMSFT where
  peekCStruct p = do
    nodeCapacityInput <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    nodeCountOutput <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    nodeProperties <- peek @(Ptr ControllerModelNodePropertiesMSFT) ((p `plusPtr` 24 :: Ptr (Ptr ControllerModelNodePropertiesMSFT)))
    pure $ ControllerModelPropertiesMSFT
             nodeCapacityInput nodeCountOutput nodeProperties

instance Storable ControllerModelPropertiesMSFT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ControllerModelPropertiesMSFT where
  zero = ControllerModelPropertiesMSFT
           zero
           zero
           zero


-- | XrControllerModelNodeStateMSFT - Describes the state of a node in a
-- controller model
--
-- == Parameter Descriptions
--
-- = Description
--
-- The state is corresponding to the glTF node identified by the @nodeName@
-- and @nodeParentName@ of the node property at the same array index in the
-- @nodeProperties@ in 'ControllerModelPropertiesMSFT'.
--
-- The @nodePose@ is based on the user’s interaction on the controller at
-- the latest 'OpenXR.Core10.Input.syncActions', represented as the
-- 'OpenXR.Core10.Space.Posef' of the node in it’s parent node space.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrControllerModelNodeStateMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'ControllerModelNodeStateMSFT'
--
-- -   #VUID-XrControllerModelNodeStateMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT'
--
-- -   #VUID-XrControllerModelNodeStateMSFT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- = See Also
--
-- 'ControllerModelStateMSFT', 'OpenXR.Core10.Space.Posef',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'getControllerModelStateMSFT'
data ControllerModelNodeStateMSFT = ControllerModelNodeStateMSFT
  { -- | @nodePose@ is an 'OpenXR.Core10.Space.Posef' of the node in its parent
    -- node space.
    nodePose :: Posef }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ControllerModelNodeStateMSFT)
#endif
deriving instance Show ControllerModelNodeStateMSFT

instance ToCStruct ControllerModelNodeStateMSFT where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ControllerModelNodeStateMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Posef)) (nodePose)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_CONTROLLER_MODEL_NODE_STATE_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Posef)) (zero)
    f

instance FromCStruct ControllerModelNodeStateMSFT where
  peekCStruct p = do
    nodePose <- peekCStruct @Posef ((p `plusPtr` 16 :: Ptr Posef))
    pure $ ControllerModelNodeStateMSFT
             nodePose

instance Storable ControllerModelNodeStateMSFT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ControllerModelNodeStateMSFT where
  zero = ControllerModelNodeStateMSFT
           zero


-- | XrControllerModelStateMSFT - Describes the state of a controller model
--
-- == Parameter Descriptions
--
-- -   @type@ is the 'OpenXR.Core10.Enums.StructureType.StructureType' of
--     this structure.
--
-- -   @next@ is @NULL@ or a pointer to the next structure in a structure
--     chain.
--
-- -   @nodeCapacityInput@ is the capacity of the @nodeStates@ array, or 0
--     to indicate a request to retrieve the required capacity.
--
-- -   @nodeCountOutput@ filled in by the runtime with the count of
--     elements in @nodeStates@ array, or returns the required capacity in
--     the case that @nodeCapacityInput@ is 0.
--
-- -   @nodeStates@ is a pointer to an application-allocated array that
--     will be filled with the 'ControllerModelNodeStateMSFT' values. It
--     /can/ be @NULL@ if @sourceCapacityInput@ is 0.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @nodeStates@ size.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrControllerModelStateMSFT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'ControllerModelStateMSFT'
--
-- -   #VUID-XrControllerModelStateMSFT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_CONTROLLER_MODEL_STATE_MSFT'
--
-- -   #VUID-XrControllerModelStateMSFT-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrControllerModelStateMSFT-nodeStates-parameter# If
--     @nodeCapacityInput@ is not @0@, @nodeStates@ /must/ be a pointer to
--     an array of @nodeCapacityInput@ 'ControllerModelNodeStateMSFT'
--     structures
--
-- = See Also
--
-- 'ControllerModelNodeStateMSFT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'getControllerModelStateMSFT'
data ControllerModelStateMSFT = ControllerModelStateMSFT
  { -- No documentation found for Nested "XrControllerModelStateMSFT" "nodeCapacityInput"
    nodeCapacityInput :: Word32
  , -- No documentation found for Nested "XrControllerModelStateMSFT" "nodeCountOutput"
    nodeCountOutput :: Word32
  , -- No documentation found for Nested "XrControllerModelStateMSFT" "nodeStates"
    nodeStates :: Ptr ControllerModelNodeStateMSFT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ControllerModelStateMSFT)
#endif
deriving instance Show ControllerModelStateMSFT

instance ToCStruct ControllerModelStateMSFT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ControllerModelStateMSFT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_CONTROLLER_MODEL_STATE_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (nodeCapacityInput)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (nodeCountOutput)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ControllerModelNodeStateMSFT))) (nodeStates)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_CONTROLLER_MODEL_STATE_MSFT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct ControllerModelStateMSFT where
  peekCStruct p = do
    nodeCapacityInput <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    nodeCountOutput <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    nodeStates <- peek @(Ptr ControllerModelNodeStateMSFT) ((p `plusPtr` 24 :: Ptr (Ptr ControllerModelNodeStateMSFT)))
    pure $ ControllerModelStateMSFT
             nodeCapacityInput nodeCountOutput nodeStates

instance Storable ControllerModelStateMSFT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ControllerModelStateMSFT where
  zero = ControllerModelStateMSFT
           zero
           zero
           zero


type MSFT_controller_model_SPEC_VERSION = 2

-- No documentation found for TopLevel "XR_MSFT_controller_model_SPEC_VERSION"
pattern MSFT_controller_model_SPEC_VERSION :: forall a . Integral a => a
pattern MSFT_controller_model_SPEC_VERSION = 2


type MSFT_CONTROLLER_MODEL_EXTENSION_NAME = "XR_MSFT_controller_model"

-- No documentation found for TopLevel "XR_MSFT_CONTROLLER_MODEL_EXTENSION_NAME"
pattern MSFT_CONTROLLER_MODEL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MSFT_CONTROLLER_MODEL_EXTENSION_NAME = "XR_MSFT_controller_model"


type MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT = 64

-- No documentation found for TopLevel "XR_MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT"
pattern MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT :: forall a . Integral a => a
pattern MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT = 64

