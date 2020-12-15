{-# language CPP #-}
-- No documentation found for Chapter "SemanticPaths"
module OpenXR.Core10.SemanticPaths  ( stringToPath
                                    , pathToString
                                    , Path(..)
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
import Foreign.Ptr (nullPtr)
import GHC.Show (showParen)
import Numeric (showHex)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Foreign.C.Types (CChar(..))
import Control.Monad.IO.Class (MonadIO)
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.NamedType ((:::))
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Dynamic (InstanceCmds(pXrPathToString))
import OpenXR.Dynamic (InstanceCmds(pXrStringToPath))
import OpenXR.Core10.Handles (Instance_T)
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Zero (Zero)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrStringToPath
  :: FunPtr (Ptr Instance_T -> Ptr CChar -> Ptr Path -> IO Result) -> Ptr Instance_T -> Ptr CChar -> Ptr Path -> IO Result

-- | xrStringToPath - Converts a string to a semantic path
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'stringToPath' retrieves the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- value for a well-formed path string. If such a value had not yet been
-- assigned by the runtime to the provided path string in this
-- 'OpenXR.Core10.Handles.Instance', one /must/ be assigned at this point.
-- All calls to this function with the same
-- 'OpenXR.Core10.Handles.Instance' and path string /must/ retrieve the
-- same
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- value. Upon failure, 'stringToPath' /must/ return an appropriate
-- 'OpenXR.Core10.Enums.Result.Result', and /may/ set the output parameter
-- to 'OpenXR.Core10.APIConstants.NULL_PATH'. See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#path-atom-type Path Atom Type>
-- for the conditions under which an error /may/ be returned when this
-- function is given a valid 'OpenXR.Core10.Handles.Instance' and a
-- well-formed path string.
--
-- If the runtime’s resources are exhausted and it cannot create the path,
-- a return value of 'OpenXR.Core10.Enums.Result.ERROR_PATH_COUNT_EXCEEDED'
-- /must/ be returned. If the application specifies a string that is not a
-- well-formed path string,
-- 'OpenXR.Core10.Enums.Result.ERROR_PATH_FORMAT_INVALID' /must/ be
-- returned.
--
-- A return value of 'OpenXR.Core10.Enums.Result.SUCCESS' from
-- 'stringToPath' /may/ not necessarily imply that the runtime has a
-- component or other source of data that will be accessible through that
-- semantic path. It only means that the path string supplied was
-- well-formed and that the retrieved
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- maps to the given path string within and during the lifetime of the
-- 'OpenXR.Core10.Handles.Instance' given.
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_FORMAT_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_COUNT_EXCEEDED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'pathToString'
stringToPath :: forall io
              . (MonadIO io)
             => -- | @instance@ is an instance previously created.
                --
                -- #VUID-xrStringToPath-instance-parameter# @instance@ /must/ be a valid
                -- 'OpenXR.Core10.Handles.Instance' handle
                Instance
             -> -- | @pathString@ is the path name string to retrieve the associated
                -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
                -- for.
                --
                -- #VUID-xrStringToPath-pathString-parameter# @pathString@ /must/ be a
                -- null-terminated UTF-8 string
                ("pathString" ::: ByteString)
             -> io (Path)
stringToPath instance' pathString = liftIO . evalContT $ do
  let xrStringToPathPtr = pXrStringToPath (instanceCmds (instance' :: Instance))
  lift $ unless (xrStringToPathPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrStringToPath is null" Nothing Nothing
  let xrStringToPath' = mkXrStringToPath xrStringToPathPtr
  pathString' <- ContT $ useAsCString (pathString)
  pPath <- ContT $ bracket (callocBytes @Path 8) free
  r <- lift $ traceAroundEvent "xrStringToPath" (xrStringToPath' (instanceHandle (instance')) pathString' (pPath))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  path <- lift $ peek @Path pPath
  pure $ (path)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrPathToString
  :: FunPtr (Ptr Instance_T -> Path -> Word32 -> Ptr Word32 -> Ptr CChar -> IO Result) -> Ptr Instance_T -> Path -> Word32 -> Ptr Word32 -> Ptr CChar -> IO Result

-- | xrPathToString - Converts a semantic path to a string
--
-- == Parameter Descriptions
--
-- -   @instance@ is an instance previously created.
--
-- -   @path@ is the valid
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
--     value to retrieve the path string for.
--
-- -   @bufferCapacityInput@ is the capacity of the buffer, or 0 to
--     indicate a request to retrieve the required capacity.
--
-- -   @bufferCountOutput@ is a pointer to the count of characters written
--     (including the terminating \'\\0\'), or a pointer to the required
--     capacity in the case that @bufferCapacityInput@ is 0.
--
-- -   @buffer@ is a pointer to an application-allocated buffer that will
--     be filled with the semantic path string. It /can/ be @NULL@ if
--     @bufferCapacityInput@ is 0.
--
-- -   See
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#buffer-size-parameters Buffer Size Parameters>
--     chapter for a detailed description of retrieving the required
--     @buffer@ size.
--
-- = Description
--
-- 'pathToString' retrieves the path name string associated with an
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- in the context of a given 'OpenXR.Core10.Handles.Instance', in the form
-- of a @NULL@ terminated string placed into a /caller-allocated/ buffer.
-- Since the mapping between a well-formed path name string and an
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- is bijective, there will always be exactly one string for each valid
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- value. This can be useful if the calling application receives an
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- value that they had not previously retrieved via 'stringToPath'. During
-- the lifetime of the given 'OpenXR.Core10.Handles.Instance', the path
-- name string retrieved by this function for a given valid
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- will not change. For invalid paths, including
-- 'OpenXR.Core10.APIConstants.NULL_PATH',
-- 'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID' /must/ be returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrPathToString-instance-parameter# @instance@ /must/ be a
--     valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrPathToString-bufferCountOutput-parameter#
--     @bufferCountOutput@ /must/ be a pointer to a @uint32_t@ value
--
-- -   #VUID-xrPathToString-buffer-parameter# If @bufferCapacityInput@ is
--     not @0@, @buffer@ /must/ be a pointer to an array of
--     @bufferCapacityInput@ char values
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_PATH_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_SIZE_INSUFFICIENT'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Instance',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >,
-- 'stringToPath'
pathToString :: forall io
              . (MonadIO io)
             => -- No documentation found for Nested "xrPathToString" "instance"
                Instance
             -> -- No documentation found for Nested "xrPathToString" "path"
                Path
             -> io (("buffer" ::: ByteString))
pathToString instance' path = liftIO . evalContT $ do
  let xrPathToStringPtr = pXrPathToString (instanceCmds (instance' :: Instance))
  lift $ unless (xrPathToStringPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrPathToString is null" Nothing Nothing
  let xrPathToString' = mkXrPathToString xrPathToStringPtr
  let instance'' = instanceHandle (instance')
  pBufferCountOutput <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "xrPathToString" (xrPathToString' instance'' (path) (0) (pBufferCountOutput) (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  bufferCountOutput <- lift $ peek @Word32 pBufferCountOutput
  pBuffer <- ContT $ bracket (callocBytes @CChar (fromIntegral (bufferCountOutput))) free
  r' <- lift $ traceAroundEvent "xrPathToString" (xrPathToString' instance'' (path) ((bufferCountOutput)) (pBufferCountOutput) (pBuffer))
  lift $ when (r' < SUCCESS) (throwIO (OpenXrException r'))
  buffer' <- lift $ packCString pBuffer
  pure $ (buffer')


-- | XrPath - A semantic path
--
-- = Description
--
-- The
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- is an atom that connects an application with a single path, within the
-- context of a single instance. There is a bijective mapping between
-- well-formed path strings and atoms in use. This atom is used — in place
-- of the path name string it corresponds to — to retrieve state and
-- perform other operations.
--
-- As an
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- is only shorthand for a well-formed path string, they have no explicit
-- life cycle.
--
-- Lifetime is implicitly managed by the 'OpenXR.Core10.Handles.Instance'.
-- An
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- /must/ not be used unless it is received at execution time from the
-- runtime in the context of a particular 'OpenXR.Core10.Handles.Instance'.
-- Therefore, with the exception of 'OpenXR.Core10.APIConstants.NULL_PATH',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- values /must/ not be specified as constant values in applications: the
-- corresponding path string /should/ be used instead. During the lifetime
-- of a given 'OpenXR.Core10.Handles.Instance', the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- associated with that instance with any given well-formed path /must/ not
-- vary, and similarly the well-formed path string that corresponds to a
-- given
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- in that instance /must/ not vary. An
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrPath >
-- that is received from one 'OpenXR.Core10.Handles.Instance' /may/ not be
-- used with another. Such an invalid use /may/ be detected and result in
-- an error being returned, or it /may/ result in undefined behavior.
--
-- = See Also
--
-- 'OpenXR.Core10.APIConstants.NULL_PATH',
-- 'OpenXR.Core10.Input.ActionCreateInfo',
-- 'OpenXR.Core10.Space.ActionSpaceCreateInfo',
-- 'OpenXR.Core10.Input.ActionStateGetInfo',
-- 'OpenXR.Core10.Input.ActionSuggestedBinding',
-- 'OpenXR.Core10.Input.ActiveActionSet',
-- 'OpenXR.Core10.Haptics.HapticActionInfo',
-- 'OpenXR.Core10.Input.InputSourceLocalizedNameGetInfo',
-- 'OpenXR.Extensions.XR_VALVE_analog_threshold.InteractionProfileAnalogThresholdVALVE',
-- 'OpenXR.Core10.Input.InteractionProfileState',
-- 'OpenXR.Core10.Input.InteractionProfileSuggestedBinding',
-- 'OpenXR.Core10.Input.enumerateBoundSourcesForAction',
-- 'OpenXR.Extensions.XR_MSFT_controller_model.getControllerModelKeyMSFT',
-- 'OpenXR.Core10.Input.getCurrentInteractionProfile', 'pathToString',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceActiveEXT xrSetInputDeviceActiveEXT>,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceLocationEXT xrSetInputDeviceLocationEXT>,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceStateBoolEXT xrSetInputDeviceStateBoolEXT>,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceStateFloatEXT xrSetInputDeviceStateFloatEXT>,
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#xrSetInputDeviceStateVector2fEXT xrSetInputDeviceStateVector2fEXT>,
-- 'stringToPath'
newtype Path = Path Word64
  deriving newtype (Eq, Ord, Storable, Zero)
instance Show Path where
  showsPrec p (Path x) = showParen (p >= 11) (showString "Path 0x" . showHex x)

