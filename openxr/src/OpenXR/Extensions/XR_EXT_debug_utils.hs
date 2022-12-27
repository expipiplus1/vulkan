{-# language CPP #-}
-- | = Name
--
-- XR_EXT_debug_utils - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_debug_utils  XR_EXT_debug_utils>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 20
--
-- = Revision
--
-- 3
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'PFN_xrDebugUtilsMessengerCallbackEXT', 'DebugUtilsLabelEXT',
-- 'DebugUtilsMessengerCallbackDataEXT',
-- 'DebugUtilsMessengerCreateInfoEXT', 'DebugUtilsObjectNameInfoEXT',
-- 'createDebugUtilsMessengerEXT', 'destroyDebugUtilsMessengerEXT',
-- 'sessionBeginDebugUtilsLabelRegionEXT',
-- 'sessionEndDebugUtilsLabelRegionEXT', 'sessionInsertDebugUtilsLabelEXT',
-- 'setDebugUtilsObjectNameEXT', 'submitDebugUtilsMessageEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_debug_utils OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_debug_utils  ( setDebugUtilsObjectNameEXT
                                             , createDebugUtilsMessengerEXT
                                             , withDebugUtilsMessengerEXT
                                             , destroyDebugUtilsMessengerEXT
                                             , submitDebugUtilsMessageEXT
                                             , sessionBeginDebugUtilsLabelRegionEXT
                                             , sessionEndDebugUtilsLabelRegionEXT
                                             , sessionInsertDebugUtilsLabelEXT
                                             , DebugUtilsObjectNameInfoEXT(..)
                                             , DebugUtilsLabelEXT(..)
                                             , DebugUtilsMessengerCallbackDataEXT(..)
                                             , DebugUtilsMessengerCreateInfoEXT(..)
                                             , DebugUtilsMessageSeverityFlagsEXT
                                             , DebugUtilsMessageSeverityFlagBitsEXT( DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
                                                                                   , DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
                                                                                   , ..
                                                                                   )
                                             , DebugUtilsMessageTypeFlagsEXT
                                             , DebugUtilsMessageTypeFlagBitsEXT( DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                                                                               , DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                                                                               , DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
                                                                               , DEBUG_UTILS_MESSAGE_TYPE_CONFORMANCE_BIT_EXT
                                                                               , ..
                                                                               )
                                             , PFN_xrDebugUtilsMessengerCallbackEXT
                                             , FN_xrDebugUtilsMessengerCallbackEXT
                                             , EXT_debug_utils_SPEC_VERSION
                                             , pattern EXT_debug_utils_SPEC_VERSION
                                             , EXT_DEBUG_UTILS_EXTENSION_NAME
                                             , pattern EXT_DEBUG_UTILS_EXTENSION_NAME
                                             , DebugUtilsMessengerEXT(..)
                                             ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
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
import Foreign.C.Types (CChar)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Bits (Bits)
import GHC.Bits (FiniteBits)
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.NamedType ((:::))
import OpenXR.Core10.FundamentalTypes (Bool32)
import OpenXR.Extensions.Handles (DebugUtilsMessengerEXT)
import OpenXR.Extensions.Handles (DebugUtilsMessengerEXT(..))
import OpenXR.Extensions.Handles (DebugUtilsMessengerEXT(DebugUtilsMessengerEXT))
import OpenXR.Extensions.Handles (DebugUtilsMessengerEXT_T)
import OpenXR.Core10.FundamentalTypes (Flags64)
import OpenXR.Core10.Handles (Instance)
import OpenXR.Core10.Handles (Instance(..))
import OpenXR.Core10.Handles (Instance(Instance))
import OpenXR.Dynamic (InstanceCmds(pXrCreateDebugUtilsMessengerEXT))
import OpenXR.Dynamic (InstanceCmds(pXrDestroyDebugUtilsMessengerEXT))
import OpenXR.Dynamic (InstanceCmds(pXrSessionBeginDebugUtilsLabelRegionEXT))
import OpenXR.Dynamic (InstanceCmds(pXrSessionEndDebugUtilsLabelRegionEXT))
import OpenXR.Dynamic (InstanceCmds(pXrSessionInsertDebugUtilsLabelEXT))
import OpenXR.Dynamic (InstanceCmds(pXrSetDebugUtilsObjectNameEXT))
import OpenXR.Dynamic (InstanceCmds(pXrSubmitDebugUtilsMessageEXT))
import OpenXR.Core10.Handles (Instance_T)
import OpenXR.Core10.Enums.ObjectType (ObjectType)
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session(Session))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_DEBUG_UTILS_LABEL_EXT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT))
import OpenXR.Extensions.Handles (DebugUtilsMessengerEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrSetDebugUtilsObjectNameEXT
  :: FunPtr (Ptr Instance_T -> Ptr DebugUtilsObjectNameInfoEXT -> IO Result) -> Ptr Instance_T -> Ptr DebugUtilsObjectNameInfoEXT -> IO Result

-- | xrSetDebugUtilsObjectNameEXT - Sets debug utils object name
--
-- == Valid Usage
--
-- -   In the structure pointed to by @nameInfo@,
--     'DebugUtilsObjectNameInfoEXT'::@objectType@ /must/ not be
--     'OpenXR.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- -   In the structure pointed to by @nameInfo@,
--     'DebugUtilsObjectNameInfoEXT'::@objectHandle@ /must/ not be
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_NULL_HANDLE XR_NULL_HANDLE>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrSetDebugUtilsObjectNameEXT-extension-notenabled# The
--     @XR_EXT_debug_utils@ extension /must/ be enabled prior to calling
--     'setDebugUtilsObjectNameEXT'
--
-- -   #VUID-xrSetDebugUtilsObjectNameEXT-instance-parameter# @instance@
--     /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrSetDebugUtilsObjectNameEXT-nameInfo-parameter# @nameInfo@
--     /must/ be a pointer to a valid 'DebugUtilsObjectNameInfoEXT'
--     structure
--
-- == Thread Safety
--
-- -   Access to the @objectHandle@ member of the @nameInfo@ parameter
--     /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-successcodes Success>]
--
--     -   'OpenXR.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#fundamentals-errorcodes Failure>]
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- Applications /may/ change the name associated with an object simply by
-- calling 'setDebugUtilsObjectNameEXT' again with a new string. If
-- 'DebugUtilsObjectNameInfoEXT'::@objectName@ is an empty string, then any
-- previously set name is removed.
--
-- = See Also
--
-- 'DebugUtilsObjectNameInfoEXT', 'OpenXR.Core10.Handles.Instance'
setDebugUtilsObjectNameEXT :: forall io
                            . (MonadIO io)
                           => -- | @instance@ is the 'OpenXR.Core10.Handles.Instance' that the object was
                              -- created under.
                              Instance
                           -> -- | @nameInfo@ is a pointer to an instance of the
                              -- 'DebugUtilsObjectNameInfoEXT' structure specifying the parameters of the
                              -- name to set on the object.
                              DebugUtilsObjectNameInfoEXT
                           -> io ()
setDebugUtilsObjectNameEXT instance' nameInfo = liftIO . evalContT $ do
  let xrSetDebugUtilsObjectNameEXTPtr = pXrSetDebugUtilsObjectNameEXT (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (xrSetDebugUtilsObjectNameEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrSetDebugUtilsObjectNameEXT is null" Nothing Nothing
  let xrSetDebugUtilsObjectNameEXT' = mkXrSetDebugUtilsObjectNameEXT xrSetDebugUtilsObjectNameEXTPtr
  nameInfo' <- ContT $ withCStruct (nameInfo)
  r <- lift $ traceAroundEvent "xrSetDebugUtilsObjectNameEXT" (xrSetDebugUtilsObjectNameEXT'
                                                                 (instanceHandle (instance'))
                                                                 nameInfo')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrCreateDebugUtilsMessengerEXT
  :: FunPtr (Ptr Instance_T -> Ptr DebugUtilsMessengerCreateInfoEXT -> Ptr (Ptr DebugUtilsMessengerEXT_T) -> IO Result) -> Ptr Instance_T -> Ptr DebugUtilsMessengerCreateInfoEXT -> Ptr (Ptr DebugUtilsMessengerEXT_T) -> IO Result

-- | xrCreateDebugUtilsMessengerEXT - Creates a debug messenger
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrCreateDebugUtilsMessengerEXT-extension-notenabled# The
--     @XR_EXT_debug_utils@ extension /must/ be enabled prior to calling
--     'createDebugUtilsMessengerEXT'
--
-- -   #VUID-xrCreateDebugUtilsMessengerEXT-instance-parameter# @instance@
--     /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrCreateDebugUtilsMessengerEXT-createInfo-parameter#
--     @createInfo@ /must/ be a pointer to a valid
--     'DebugUtilsMessengerCreateInfoEXT' structure
--
-- -   #VUID-xrCreateDebugUtilsMessengerEXT-messenger-parameter#
--     @messenger@ /must/ be a pointer to an
--     'OpenXR.Extensions.Handles.DebugUtilsMessengerEXT' handle
--
-- == Thread Safety
--
-- -   Access to @instance@, and any child handles, /must/ be externally
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_OUT_OF_MEMORY'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_LIMIT_REACHED'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_INSTANCE_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- The application /must/ ensure that 'createDebugUtilsMessengerEXT' is not
-- executed in parallel with any OpenXR function that is also called with
-- @instance@ or child of @instance@.
--
-- When an event of interest occurs a debug messenger calls its
-- @createInfo@->@userCallback@ with a debug message from the producer of
-- the event. Additionally, the debug messenger /must/ filter out any debug
-- messages that the application’s callback is not interested in based on
-- 'DebugUtilsMessengerCreateInfoEXT' flags, as described below.
--
-- = See Also
--
-- 'DebugUtilsMessengerCreateInfoEXT',
-- 'OpenXR.Extensions.Handles.DebugUtilsMessengerEXT',
-- 'OpenXR.Core10.Handles.Instance', 'destroyDebugUtilsMessengerEXT'
createDebugUtilsMessengerEXT :: forall io
                              . (MonadIO io)
                             => -- | @instance@ is the instance the messenger will be used with.
                                Instance
                             -> -- | @createInfo@ points to an 'DebugUtilsMessengerCreateInfoEXT' structure,
                                -- which contains the callback pointer as well as defines the conditions
                                -- under which this messenger will trigger the callback.
                                DebugUtilsMessengerCreateInfoEXT
                             -> io (DebugUtilsMessengerEXT)
createDebugUtilsMessengerEXT instance' createInfo = liftIO . evalContT $ do
  let cmds = case instance' of Instance{instanceCmds} -> instanceCmds
  let xrCreateDebugUtilsMessengerEXTPtr = pXrCreateDebugUtilsMessengerEXT cmds
  lift $ unless (xrCreateDebugUtilsMessengerEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrCreateDebugUtilsMessengerEXT is null" Nothing Nothing
  let xrCreateDebugUtilsMessengerEXT' = mkXrCreateDebugUtilsMessengerEXT xrCreateDebugUtilsMessengerEXTPtr
  createInfo' <- ContT $ withCStruct (createInfo)
  pMessenger <- ContT $ bracket (callocBytes @(Ptr DebugUtilsMessengerEXT_T) 8) free
  r <- lift $ traceAroundEvent "xrCreateDebugUtilsMessengerEXT" (xrCreateDebugUtilsMessengerEXT'
                                                                   (instanceHandle (instance'))
                                                                   createInfo'
                                                                   (pMessenger))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  messenger <- lift $ peek @(Ptr DebugUtilsMessengerEXT_T) pMessenger
  pure $ (((\h -> DebugUtilsMessengerEXT h cmds ) messenger))

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDebugUtilsMessengerEXT' and 'destroyDebugUtilsMessengerEXT'
--
-- To ensure that 'destroyDebugUtilsMessengerEXT' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDebugUtilsMessengerEXT :: forall io r . MonadIO io => Instance -> DebugUtilsMessengerCreateInfoEXT -> (io DebugUtilsMessengerEXT -> (DebugUtilsMessengerEXT -> io ()) -> r) -> r
withDebugUtilsMessengerEXT instance' createInfo b =
  b (createDebugUtilsMessengerEXT instance' createInfo)
    (\(o0) -> destroyDebugUtilsMessengerEXT o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrDestroyDebugUtilsMessengerEXT
  :: FunPtr (Ptr DebugUtilsMessengerEXT_T -> IO Result) -> Ptr DebugUtilsMessengerEXT_T -> IO Result

-- | xrDestroyDebugUtilsMessengerEXT - Destroys a debug messenger
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrDestroyDebugUtilsMessengerEXT-extension-notenabled# The
--     @XR_EXT_debug_utils@ extension /must/ be enabled prior to calling
--     'destroyDebugUtilsMessengerEXT'
--
-- -   #VUID-xrDestroyDebugUtilsMessengerEXT-messenger-parameter#
--     @messenger@ /must/ be a valid
--     'OpenXR.Extensions.Handles.DebugUtilsMessengerEXT' handle
--
-- == Thread Safety
--
-- -   Access to @messenger@ /must/ be externally synchronized
--
-- -   Access to the 'OpenXR.Core10.Handles.Instance' used to create
--     @messenger@, and all of its child handles /must/ be externally
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
-- The application /must/ ensure that 'destroyDebugUtilsMessengerEXT' is
-- not executed in parallel with any OpenXR function that is also called
-- with the @instance@ or child of @instance@ that it was created with.
--
-- = See Also
--
-- 'OpenXR.Extensions.Handles.DebugUtilsMessengerEXT',
-- 'createDebugUtilsMessengerEXT'
destroyDebugUtilsMessengerEXT :: forall io
                               . (MonadIO io)
                              => -- | @messenger@ the 'OpenXR.Extensions.Handles.DebugUtilsMessengerEXT'
                                 -- object to destroy. @messenger@ is an externally synchronized object and
                                 -- /must/ not be used on more than one thread at a time. This means that
                                 -- 'destroyDebugUtilsMessengerEXT' /must/ not be called when a callback is
                                 -- active.
                                 DebugUtilsMessengerEXT
                              -> io ()
destroyDebugUtilsMessengerEXT messenger = liftIO $ do
  let xrDestroyDebugUtilsMessengerEXTPtr = pXrDestroyDebugUtilsMessengerEXT (case messenger of DebugUtilsMessengerEXT{instanceCmds} -> instanceCmds)
  unless (xrDestroyDebugUtilsMessengerEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrDestroyDebugUtilsMessengerEXT is null" Nothing Nothing
  let xrDestroyDebugUtilsMessengerEXT' = mkXrDestroyDebugUtilsMessengerEXT xrDestroyDebugUtilsMessengerEXTPtr
  r <- traceAroundEvent "xrDestroyDebugUtilsMessengerEXT" (xrDestroyDebugUtilsMessengerEXT'
                                                             (debugUtilsMessengerEXTHandle (messenger)))
  when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrSubmitDebugUtilsMessageEXT
  :: FunPtr (Ptr Instance_T -> DebugUtilsMessageSeverityFlagsEXT -> DebugUtilsMessageTypeFlagsEXT -> Ptr DebugUtilsMessengerCallbackDataEXT -> IO Result) -> Ptr Instance_T -> DebugUtilsMessageSeverityFlagsEXT -> DebugUtilsMessageTypeFlagsEXT -> Ptr DebugUtilsMessengerCallbackDataEXT -> IO Result

-- | xrSubmitDebugUtilsMessageEXT - Submits debug utils message
--
-- == Valid Usage
--
-- -   For each structure in @objects@ found in @callbackData@, the value
--     of 'DebugUtilsObjectNameInfoEXT'::@objectType@ /must/ not be
--     'OpenXR.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrSubmitDebugUtilsMessageEXT-extension-notenabled# The
--     @XR_EXT_debug_utils@ extension /must/ be enabled prior to calling
--     'submitDebugUtilsMessageEXT'
--
-- -   #VUID-xrSubmitDebugUtilsMessageEXT-instance-parameter# @instance@
--     /must/ be a valid 'OpenXR.Core10.Handles.Instance' handle
--
-- -   #VUID-xrSubmitDebugUtilsMessageEXT-messageSeverity-parameter#
--     @messageSeverity@ /must/ be a valid combination of
--     'DebugUtilsMessageSeverityFlagBitsEXT' values
--
-- -   #VUID-xrSubmitDebugUtilsMessageEXT-messageSeverity-requiredbitmask#
--     @messageSeverity@ /must/ not be @0@
--
-- -   #VUID-xrSubmitDebugUtilsMessageEXT-messageTypes-parameter#
--     @messageTypes@ /must/ be a valid combination of
--     'DebugUtilsMessageTypeFlagBitsEXT' values
--
-- -   #VUID-xrSubmitDebugUtilsMessageEXT-messageTypes-requiredbitmask#
--     @messageTypes@ /must/ not be @0@
--
-- -   #VUID-xrSubmitDebugUtilsMessageEXT-callbackData-parameter#
--     @callbackData@ /must/ be a pointer to a valid
--     'DebugUtilsMessengerCallbackDataEXT' structure
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- The application /can/ also produce a debug message, and submit it into
-- the OpenXR messaging system.
--
-- The call will propagate through the layers and generate callback(s) as
-- indicated by the message’s flags. The parameters are passed on to the
-- callback in addition to the userData value that was defined at the time
-- the messenger was created.
--
-- = See Also
--
-- 'DebugUtilsMessageSeverityFlagsEXT', 'DebugUtilsMessageTypeFlagsEXT',
-- 'DebugUtilsMessengerCallbackDataEXT', 'OpenXR.Core10.Handles.Instance'
submitDebugUtilsMessageEXT :: forall io
                            . (MonadIO io)
                           => -- | @instance@ is the debug stream’s 'OpenXR.Core10.Handles.Instance'.
                              Instance
                           -> -- | @messageSeverity@ is a single bit value of
                              -- 'DebugUtilsMessageSeverityFlagsEXT' severity of this event\/message.
                              DebugUtilsMessageSeverityFlagsEXT
                           -> -- | @messageTypes@ is an 'DebugUtilsMessageTypeFlagsEXT' bitmask of
                              -- 'DebugUtilsMessageTypeFlagBitsEXT' specifying which types of event to
                              -- identify this message with.
                              ("messageTypes" ::: DebugUtilsMessageTypeFlagsEXT)
                           -> -- | @callbackData@ contains all the callback related data in the
                              -- 'DebugUtilsMessengerCallbackDataEXT' structure.
                              DebugUtilsMessengerCallbackDataEXT
                           -> io ()
submitDebugUtilsMessageEXT instance'
                             messageSeverity
                             messageTypes
                             callbackData = liftIO . evalContT $ do
  let xrSubmitDebugUtilsMessageEXTPtr = pXrSubmitDebugUtilsMessageEXT (case instance' of Instance{instanceCmds} -> instanceCmds)
  lift $ unless (xrSubmitDebugUtilsMessageEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrSubmitDebugUtilsMessageEXT is null" Nothing Nothing
  let xrSubmitDebugUtilsMessageEXT' = mkXrSubmitDebugUtilsMessageEXT xrSubmitDebugUtilsMessageEXTPtr
  callbackData' <- ContT $ withCStruct (callbackData)
  r <- lift $ traceAroundEvent "xrSubmitDebugUtilsMessageEXT" (xrSubmitDebugUtilsMessageEXT'
                                                                 (instanceHandle (instance'))
                                                                 (messageSeverity)
                                                                 (messageTypes)
                                                                 callbackData')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrSessionBeginDebugUtilsLabelRegionEXT
  :: FunPtr (Ptr Session_T -> Ptr DebugUtilsLabelEXT -> IO Result) -> Ptr Session_T -> Ptr DebugUtilsLabelEXT -> IO Result

-- | xrSessionBeginDebugUtilsLabelRegionEXT - Session begin debug utils label
-- region
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrSessionBeginDebugUtilsLabelRegionEXT-extension-notenabled#
--     The @XR_EXT_debug_utils@ extension /must/ be enabled prior to
--     calling 'sessionBeginDebugUtilsLabelRegionEXT'
--
-- -   #VUID-xrSessionBeginDebugUtilsLabelRegionEXT-session-parameter#
--     @session@ /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrSessionBeginDebugUtilsLabelRegionEXT-labelInfo-parameter#
--     @labelInfo@ /must/ be a pointer to a valid 'DebugUtilsLabelEXT'
--     structure
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- The 'sessionBeginDebugUtilsLabelRegionEXT' function begins a label
-- region within @session@.
--
-- = See Also
--
-- 'DebugUtilsLabelEXT', 'OpenXR.Core10.Handles.Session'
sessionBeginDebugUtilsLabelRegionEXT :: forall io
                                      . (MonadIO io)
                                     => -- | @session@ is the 'OpenXR.Core10.Handles.Session' that a label region
                                        -- should be associated with.
                                        Session
                                     -> -- | @labelInfo@ is the 'DebugUtilsLabelEXT' containing the label information
                                        -- for the region that should be begun.
                                        ("labelInfo" ::: DebugUtilsLabelEXT)
                                     -> io (Result)
sessionBeginDebugUtilsLabelRegionEXT session labelInfo = liftIO . evalContT $ do
  let xrSessionBeginDebugUtilsLabelRegionEXTPtr = pXrSessionBeginDebugUtilsLabelRegionEXT (case session of Session{instanceCmds} -> instanceCmds)
  lift $ unless (xrSessionBeginDebugUtilsLabelRegionEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrSessionBeginDebugUtilsLabelRegionEXT is null" Nothing Nothing
  let xrSessionBeginDebugUtilsLabelRegionEXT' = mkXrSessionBeginDebugUtilsLabelRegionEXT xrSessionBeginDebugUtilsLabelRegionEXTPtr
  labelInfo' <- ContT $ withCStruct (labelInfo)
  r <- lift $ traceAroundEvent "xrSessionBeginDebugUtilsLabelRegionEXT" (xrSessionBeginDebugUtilsLabelRegionEXT'
                                                                           (sessionHandle (session))
                                                                           labelInfo')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrSessionEndDebugUtilsLabelRegionEXT
  :: FunPtr (Ptr Session_T -> IO Result) -> Ptr Session_T -> IO Result

-- | xrSessionEndDebugUtilsLabelRegionEXT - Session end debug utils label
-- region
--
-- == Valid Usage
--
-- -   'sessionEndDebugUtilsLabelRegionEXT' /must/ be called only after a
--     matching 'sessionBeginDebugUtilsLabelRegionEXT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrSessionEndDebugUtilsLabelRegionEXT-extension-notenabled# The
--     @XR_EXT_debug_utils@ extension /must/ be enabled prior to calling
--     'sessionEndDebugUtilsLabelRegionEXT'
--
-- -   #VUID-xrSessionEndDebugUtilsLabelRegionEXT-session-parameter#
--     @session@ /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- This function ends the last label region begun with the
-- 'sessionBeginDebugUtilsLabelRegionEXT' function within the same
-- @session@.
--
-- = See Also
--
-- 'OpenXR.Core10.Handles.Session'
sessionEndDebugUtilsLabelRegionEXT :: forall io
                                    . (MonadIO io)
                                   => -- | @session@ is the 'OpenXR.Core10.Handles.Session' that a label region
                                      -- should be associated with.
                                      Session
                                   -> io (Result)
sessionEndDebugUtilsLabelRegionEXT session = liftIO $ do
  let xrSessionEndDebugUtilsLabelRegionEXTPtr = pXrSessionEndDebugUtilsLabelRegionEXT (case session of Session{instanceCmds} -> instanceCmds)
  unless (xrSessionEndDebugUtilsLabelRegionEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrSessionEndDebugUtilsLabelRegionEXT is null" Nothing Nothing
  let xrSessionEndDebugUtilsLabelRegionEXT' = mkXrSessionEndDebugUtilsLabelRegionEXT xrSessionEndDebugUtilsLabelRegionEXTPtr
  r <- traceAroundEvent "xrSessionEndDebugUtilsLabelRegionEXT" (xrSessionEndDebugUtilsLabelRegionEXT'
                                                                  (sessionHandle (session)))
  when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrSessionInsertDebugUtilsLabelEXT
  :: FunPtr (Ptr Session_T -> Ptr DebugUtilsLabelEXT -> IO Result) -> Ptr Session_T -> Ptr DebugUtilsLabelEXT -> IO Result

-- | xrSessionInsertDebugUtilsLabelEXT - Session insert debug utils label
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrSessionInsertDebugUtilsLabelEXT-extension-notenabled# The
--     @XR_EXT_debug_utils@ extension /must/ be enabled prior to calling
--     'sessionInsertDebugUtilsLabelEXT'
--
-- -   #VUID-xrSessionInsertDebugUtilsLabelEXT-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrSessionInsertDebugUtilsLabelEXT-labelInfo-parameter#
--     @labelInfo@ /must/ be a pointer to a valid 'DebugUtilsLabelEXT'
--     structure
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_SESSION_LOST'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_HANDLE_INVALID'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_RUNTIME_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- The 'sessionInsertDebugUtilsLabelEXT' function inserts an individual
-- label within @session@. The individual labels are useful for different
-- reasons based on the type of debugging scenario. When used with
-- something active like a profiler or debugger, it identifies a single
-- point of time. When used with logging, the individual label identifies
-- that a particular location has been passed at the point the log message
-- is triggered. Because of this usage, individual labels only exist in a
-- log until the next call to any of the label functions:
--
-- -   'sessionBeginDebugUtilsLabelRegionEXT'
--
-- -   'sessionEndDebugUtilsLabelRegionEXT'
--
-- -   'sessionInsertDebugUtilsLabelEXT'
--
-- = See Also
--
-- 'DebugUtilsLabelEXT', 'OpenXR.Core10.Handles.Session',
-- 'sessionBeginDebugUtilsLabelRegionEXT',
-- 'sessionEndDebugUtilsLabelRegionEXT'
sessionInsertDebugUtilsLabelEXT :: forall io
                                 . (MonadIO io)
                                => -- | @session@ is the 'OpenXR.Core10.Handles.Session' that a label region
                                   -- should be associated with.
                                   Session
                                -> -- | @labelInfo@ is the 'DebugUtilsLabelEXT' containing the label information
                                   -- for the region that should be begun.
                                   ("labelInfo" ::: DebugUtilsLabelEXT)
                                -> io (Result)
sessionInsertDebugUtilsLabelEXT session labelInfo = liftIO . evalContT $ do
  let xrSessionInsertDebugUtilsLabelEXTPtr = pXrSessionInsertDebugUtilsLabelEXT (case session of Session{instanceCmds} -> instanceCmds)
  lift $ unless (xrSessionInsertDebugUtilsLabelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrSessionInsertDebugUtilsLabelEXT is null" Nothing Nothing
  let xrSessionInsertDebugUtilsLabelEXT' = mkXrSessionInsertDebugUtilsLabelEXT xrSessionInsertDebugUtilsLabelEXTPtr
  labelInfo' <- ContT $ withCStruct (labelInfo)
  r <- lift $ traceAroundEvent "xrSessionInsertDebugUtilsLabelEXT" (xrSessionInsertDebugUtilsLabelEXT'
                                                                      (sessionHandle (session))
                                                                      labelInfo')
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


-- | XrDebugUtilsObjectNameInfoEXT - Debug utils object name info
--
-- == Valid Usage
--
-- -   If @objectType@ is
--     'OpenXR.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN', @objectHandle@
--     /must/ not be
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_NULL_HANDLE XR_NULL_HANDLE>
--
-- -   If @objectType@ is not
--     'OpenXR.Core10.Enums.ObjectType.OBJECT_TYPE_UNKNOWN', @objectHandle@
--     /must/ be
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_NULL_HANDLE XR_NULL_HANDLE>
--     or an OpenXR handle of the type associated with @objectType@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrDebugUtilsObjectNameInfoEXT-extension-notenabled# The
--     @XR_EXT_debug_utils@ extension /must/ be enabled prior to using
--     'DebugUtilsObjectNameInfoEXT'
--
-- -   #VUID-XrDebugUtilsObjectNameInfoEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT'
--
-- -   #VUID-XrDebugUtilsObjectNameInfoEXT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrDebugUtilsObjectNameInfoEXT-objectType-parameter#
--     @objectType@ /must/ be a valid
--     'OpenXR.Core10.Enums.ObjectType.ObjectType' value
--
-- -   #VUID-XrDebugUtilsObjectNameInfoEXT-objectName-parameter# If
--     @objectName@ is not @NULL@, @objectName@ /must/ be a null-terminated
--     UTF-8 string
--
-- = See Also
--
-- 'DebugUtilsMessengerCallbackDataEXT',
-- 'OpenXR.Core10.Enums.ObjectType.ObjectType',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'setDebugUtilsObjectNameEXT'
data DebugUtilsObjectNameInfoEXT = DebugUtilsObjectNameInfoEXT
  { -- | @objectType@ is an 'OpenXR.Core10.Enums.ObjectType.ObjectType'
    -- specifying the type of the object to be named.
    objectType :: ObjectType
  , -- | @objectHandle@ is the object to be named.
    objectHandle :: Word64
  , -- | @objectName@ is a @NULL@ terminated UTF-8 string specifying the name to
    -- apply to objectHandle.
    objectName :: Maybe ByteString
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsObjectNameInfoEXT)
#endif
deriving instance Show DebugUtilsObjectNameInfoEXT

instance ToCStruct DebugUtilsObjectNameInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsObjectNameInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr ObjectType)) (objectType)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word64)) (objectHandle)
    objectName'' <- case (objectName) of
      Nothing -> pure nullPtr
      Just j -> ContT $ useAsCString (j)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) objectName''
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ObjectType)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct DebugUtilsObjectNameInfoEXT where
  peekCStruct p = do
    objectType <- peek @ObjectType ((p `plusPtr` 16 :: Ptr ObjectType))
    objectHandle <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    objectName <- peek @(Ptr CChar) ((p `plusPtr` 32 :: Ptr (Ptr CChar)))
    objectName' <- maybePeek (\j -> packCString (j)) objectName
    pure $ DebugUtilsObjectNameInfoEXT
             objectType objectHandle objectName'

instance Zero DebugUtilsObjectNameInfoEXT where
  zero = DebugUtilsObjectNameInfoEXT
           zero
           zero
           Nothing


-- | XrDebugUtilsLabelEXT - Debug Utils Label
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrDebugUtilsLabelEXT-extension-notenabled# The
--     @XR_EXT_debug_utils@ extension /must/ be enabled prior to using
--     'DebugUtilsLabelEXT'
--
-- -   #VUID-XrDebugUtilsLabelEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_DEBUG_UTILS_LABEL_EXT'
--
-- -   #VUID-XrDebugUtilsLabelEXT-next-next# @next@ /must/ be @NULL@ or a
--     valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrDebugUtilsLabelEXT-labelName-parameter# @labelName@ /must/
--     be a null-terminated UTF-8 string
--
-- = See Also
--
-- 'DebugUtilsMessengerCallbackDataEXT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'sessionBeginDebugUtilsLabelRegionEXT',
-- 'sessionInsertDebugUtilsLabelEXT'
data DebugUtilsLabelEXT = DebugUtilsLabelEXT
  { -- | @labelName@ is a @NULL@ terminated UTF-8 string specifying the label
    -- name.
    labelName :: ByteString }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsLabelEXT)
#endif
deriving instance Show DebugUtilsLabelEXT

instance ToCStruct DebugUtilsLabelEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsLabelEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_DEBUG_UTILS_LABEL_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    labelName'' <- ContT $ useAsCString (labelName)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) labelName''
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_DEBUG_UTILS_LABEL_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    labelName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) labelName''
    lift $ f

instance FromCStruct DebugUtilsLabelEXT where
  peekCStruct p = do
    labelName <- packCString =<< peek ((p `plusPtr` 16 :: Ptr (Ptr CChar)))
    pure $ DebugUtilsLabelEXT
             labelName

instance Zero DebugUtilsLabelEXT where
  zero = DebugUtilsLabelEXT
           mempty


-- | XrDebugUtilsMessengerCallbackDataEXT - Debug utils messenger callback
-- data
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrDebugUtilsMessengerCallbackDataEXT-extension-notenabled# The
--     @XR_EXT_debug_utils@ extension /must/ be enabled prior to using
--     'DebugUtilsMessengerCallbackDataEXT'
--
-- -   #VUID-XrDebugUtilsMessengerCallbackDataEXT-type-type# @type@ /must/
--     be
--     'OpenXR.Core10.Enums.StructureType.TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT'
--
-- -   #VUID-XrDebugUtilsMessengerCallbackDataEXT-next-next# @next@ /must/
--     be @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrDebugUtilsMessengerCallbackDataEXT-messageId-parameter#
--     @messageId@ /must/ be a null-terminated UTF-8 string
--
-- -   #VUID-XrDebugUtilsMessengerCallbackDataEXT-functionName-parameter#
--     @functionName@ /must/ be a null-terminated UTF-8 string
--
-- -   #VUID-XrDebugUtilsMessengerCallbackDataEXT-message-parameter#
--     @message@ /must/ be a null-terminated UTF-8 string
--
-- An 'DebugUtilsMessengerCallbackDataEXT' is a messenger object that
-- handles passing along debug messages to a provided debug callback.
--
-- Note
--
-- This structure should only be considered valid during the lifetime of
-- the triggered callback.
--
-- The labels listed inside @sessionLabels@ are organized in time order,
-- with the most recently generated label appearing first, and the oldest
-- label appearing last.
--
-- = See Also
--
-- 'DebugUtilsLabelEXT', 'DebugUtilsObjectNameInfoEXT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'submitDebugUtilsMessageEXT'
data DebugUtilsMessengerCallbackDataEXT = DebugUtilsMessengerCallbackDataEXT
  { -- | @messageId@ is a @NULL@ terminated string that identifies the message in
    -- a unique way. If the callback is triggered by a validation layer, this
    -- string corresponds the Valid Usage ID (VUID) that can be used to jump to
    -- the appropriate location in the OpenXR specification. This value /may/
    -- be @NULL@ if no unique message identifier is associated with the
    -- message.
    messageId :: ByteString
  , -- | @functionName@ is a @NULL@ terminated string that identifies the OpenXR
    -- function that was executing at the time the message callback was
    -- triggered. This value /may/ be @NULL@ in cases where it is difficult to
    -- determine the originating OpenXR function.
    functionName :: ByteString
  , -- | @message@ is a @NULL@ terminated string detailing the trigger
    -- conditions.
    message :: ByteString
  , -- | @objectCount@ is a count of items contained in the @objects@ array. This
    -- may be @0@.
    objectCount :: Word32
  , -- | @objects@ is a pointer to an array of 'DebugUtilsObjectNameInfoEXT'
    -- objects related to the detected issue. The array is roughly in order or
    -- importance, but the 0th element is always guaranteed to be the most
    -- important object for this message.
    objects :: Ptr DebugUtilsObjectNameInfoEXT
  , -- | @sessionLabelCount@ is a count of items contained in the @sessionLabels@
    -- array. This may be @0@.
    sessionLabelCount :: Word32
  , -- | @sessionLabels@ is a pointer to an array of 'DebugUtilsLabelEXT' objects
    -- related to the detected issue. The array is roughly in order or
    -- importance, but the 0th element is always guaranteed to be the most
    -- important object for this message.
    --
    -- @sessionLabels@ is NULL or a pointer to an array of 'DebugUtilsLabelEXT'
    -- active in the current 'OpenXR.Core10.Handles.Session' at the time the
    -- callback was triggered. Refer to
    -- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#session-labels Session Labels>
    -- for more information.
    sessionLabels :: Ptr DebugUtilsLabelEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsMessengerCallbackDataEXT)
#endif
deriving instance Show DebugUtilsMessengerCallbackDataEXT

instance ToCStruct DebugUtilsMessengerCallbackDataEXT where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsMessengerCallbackDataEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    messageId'' <- ContT $ useAsCString (messageId)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) messageId''
    functionName'' <- ContT $ useAsCString (functionName)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CChar))) functionName''
    message'' <- ContT $ useAsCString (message)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) message''
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) (objectCount)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr DebugUtilsObjectNameInfoEXT))) (objects)
    lift $ poke ((p `plusPtr` 56 :: Ptr Word32)) (sessionLabelCount)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr DebugUtilsLabelEXT))) (sessionLabels)
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    messageId'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr CChar))) messageId''
    functionName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr CChar))) functionName''
    message'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) message''
    lift $ f

instance FromCStruct DebugUtilsMessengerCallbackDataEXT where
  peekCStruct p = do
    messageId <- packCString =<< peek ((p `plusPtr` 16 :: Ptr (Ptr CChar)))
    functionName <- packCString =<< peek ((p `plusPtr` 24 :: Ptr (Ptr CChar)))
    message <- packCString =<< peek ((p `plusPtr` 32 :: Ptr (Ptr CChar)))
    objectCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    objects <- peek @(Ptr DebugUtilsObjectNameInfoEXT) ((p `plusPtr` 48 :: Ptr (Ptr DebugUtilsObjectNameInfoEXT)))
    sessionLabelCount <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    sessionLabels <- peek @(Ptr DebugUtilsLabelEXT) ((p `plusPtr` 64 :: Ptr (Ptr DebugUtilsLabelEXT)))
    pure $ DebugUtilsMessengerCallbackDataEXT
             messageId
             functionName
             message
             objectCount
             objects
             sessionLabelCount
             sessionLabels

instance Zero DebugUtilsMessengerCallbackDataEXT where
  zero = DebugUtilsMessengerCallbackDataEXT
           mempty
           mempty
           mempty
           zero
           zero
           zero
           zero


-- | XrDebugUtilsMessengerCreateInfoEXT - Debug utils messenger create info
--
-- == Valid Usage
--
-- -   @userCallback@ /must/ be a valid
--     PFN_xrDebugUtilsMessengerCallbackEXT
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrDebugUtilsMessengerCreateInfoEXT-extension-notenabled# The
--     @XR_EXT_debug_utils@ extension /must/ be enabled prior to using
--     'DebugUtilsMessengerCreateInfoEXT'
--
-- -   #VUID-XrDebugUtilsMessengerCreateInfoEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT'
--
-- -   #VUID-XrDebugUtilsMessengerCreateInfoEXT-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrDebugUtilsMessengerCreateInfoEXT-messageSeverities-parameter#
--     @messageSeverities@ /must/ be a valid combination of
--     'DebugUtilsMessageSeverityFlagBitsEXT' values
--
-- -   #VUID-XrDebugUtilsMessengerCreateInfoEXT-messageSeverities-requiredbitmask#
--     @messageSeverities@ /must/ not be @0@
--
-- -   #VUID-XrDebugUtilsMessengerCreateInfoEXT-messageTypes-parameter#
--     @messageTypes@ /must/ be a valid combination of
--     'DebugUtilsMessageTypeFlagBitsEXT' values
--
-- -   #VUID-XrDebugUtilsMessengerCreateInfoEXT-messageTypes-requiredbitmask#
--     @messageTypes@ /must/ not be @0@
--
-- -   #VUID-XrDebugUtilsMessengerCreateInfoEXT-userCallback-parameter#
--     @userCallback@ /must/ be a valid
--     'PFN_xrDebugUtilsMessengerCallbackEXT' value
--
-- For each 'OpenXR.Extensions.Handles.DebugUtilsMessengerEXT' that is
-- created the 'DebugUtilsMessengerCreateInfoEXT'::@messageSeverities@ and
-- 'DebugUtilsMessengerCreateInfoEXT'::@messageTypes@ determine when that
-- 'DebugUtilsMessengerCreateInfoEXT'::@userCallback@ is called. The
-- process to determine if the user’s userCallback is triggered when an
-- event occurs is as follows:
--
-- -   The runtime will perform a bitwise AND of the event’s
--     'DebugUtilsMessageSeverityFlagBitsEXT' with the
--     'DebugUtilsMessengerCreateInfoEXT'::@messageSeverities@ provided
--     during creation of the
--     'OpenXR.Extensions.Handles.DebugUtilsMessengerEXT' object.
--
-- -   If this results in @0@, the message is skipped.
--
-- -   The runtime will perform bitwise AND of the event’s
--     'DebugUtilsMessageTypeFlagBitsEXT' with the
--     'DebugUtilsMessengerCreateInfoEXT'::@messageTypes@ provided during
--     the creation of the
--     'OpenXR.Extensions.Handles.DebugUtilsMessengerEXT' object.
--
-- -   If this results in @0@, the message is skipped.
--
-- -   If the message of the current event is not skipped, the callback
--     will be called with the message.
--
-- The callback will come directly from the component that detected the
-- event, unless some other layer intercepts the calls for its own purposes
-- (filter them in a different way, log to a system error log, etc.).
--
-- = See Also
--
-- 'PFN_xrDebugUtilsMessengerCallbackEXT',
-- 'DebugUtilsMessageSeverityFlagsEXT', 'DebugUtilsMessageTypeFlagsEXT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'createDebugUtilsMessengerEXT'
data DebugUtilsMessengerCreateInfoEXT = DebugUtilsMessengerCreateInfoEXT
  { -- | @messageSeverities@ is a bitmask of
    -- 'DebugUtilsMessageSeverityFlagBitsEXT' specifying which severity of
    -- event(s) that will cause this callback to be called.
    messageSeverities :: DebugUtilsMessageSeverityFlagsEXT
  , -- | @messageTypes@ is a combination of 'DebugUtilsMessageTypeFlagBitsEXT'
    -- specifying which type of event(s) will cause this callback to be called.
    messageTypes :: DebugUtilsMessageTypeFlagsEXT
  , -- | @userCallback@ is the application defined callback function to call.
    userCallback :: PFN_xrDebugUtilsMessengerCallbackEXT
  , -- | @userData@ is arbitrary user data to be passed to the callback.
    userData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DebugUtilsMessengerCreateInfoEXT)
#endif
deriving instance Show DebugUtilsMessengerCreateInfoEXT

instance ToCStruct DebugUtilsMessengerCreateInfoEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DebugUtilsMessengerCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DebugUtilsMessageSeverityFlagsEXT)) (messageSeverities)
    poke ((p `plusPtr` 24 :: Ptr DebugUtilsMessageTypeFlagsEXT)) (messageTypes)
    poke ((p `plusPtr` 32 :: Ptr PFN_xrDebugUtilsMessengerCallbackEXT)) (userCallback)
    poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (userData)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DebugUtilsMessageSeverityFlagsEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DebugUtilsMessageTypeFlagsEXT)) (zero)
    poke ((p `plusPtr` 32 :: Ptr PFN_xrDebugUtilsMessengerCallbackEXT)) (zero)
    f

instance FromCStruct DebugUtilsMessengerCreateInfoEXT where
  peekCStruct p = do
    messageSeverities <- peek @DebugUtilsMessageSeverityFlagsEXT ((p `plusPtr` 16 :: Ptr DebugUtilsMessageSeverityFlagsEXT))
    messageTypes <- peek @DebugUtilsMessageTypeFlagsEXT ((p `plusPtr` 24 :: Ptr DebugUtilsMessageTypeFlagsEXT))
    userCallback <- peek @PFN_xrDebugUtilsMessengerCallbackEXT ((p `plusPtr` 32 :: Ptr PFN_xrDebugUtilsMessengerCallbackEXT))
    userData <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pure $ DebugUtilsMessengerCreateInfoEXT
             messageSeverities messageTypes userCallback userData

instance Storable DebugUtilsMessengerCreateInfoEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DebugUtilsMessengerCreateInfoEXT where
  zero = DebugUtilsMessengerCreateInfoEXT
           zero
           zero
           zero
           zero


type DebugUtilsMessageSeverityFlagsEXT = DebugUtilsMessageSeverityFlagBitsEXT

-- | XrDebugUtilsMessageSeverityFlagBitsEXT -
-- XrDebugUtilsMessageSeverityFlagBitsEXT
--
-- = See Also
--
-- No cross-references are available
newtype DebugUtilsMessageSeverityFlagBitsEXT = DebugUtilsMessageSeverityFlagBitsEXT Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrDebugUtilsMessageSeverityFlagBitsEXT" "XR_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x0000000000000001

-- No documentation found for Nested "XrDebugUtilsMessageSeverityFlagBitsEXT" "XR_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x0000000000000010

-- No documentation found for Nested "XrDebugUtilsMessageSeverityFlagBitsEXT" "XR_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x0000000000000100

-- No documentation found for Nested "XrDebugUtilsMessageSeverityFlagBitsEXT" "XR_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT = DebugUtilsMessageSeverityFlagBitsEXT 0x0000000000001000

conNameDebugUtilsMessageSeverityFlagBitsEXT :: String
conNameDebugUtilsMessageSeverityFlagBitsEXT = "DebugUtilsMessageSeverityFlagBitsEXT"

enumPrefixDebugUtilsMessageSeverityFlagBitsEXT :: String
enumPrefixDebugUtilsMessageSeverityFlagBitsEXT = "DEBUG_UTILS_MESSAGE_SEVERITY_"

showTableDebugUtilsMessageSeverityFlagBitsEXT :: [(DebugUtilsMessageSeverityFlagBitsEXT, String)]
showTableDebugUtilsMessageSeverityFlagBitsEXT =
  [
    ( DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT
    , "VERBOSE_BIT_EXT"
    )
  ,
    ( DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT
    , "INFO_BIT_EXT"
    )
  ,
    ( DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT
    , "WARNING_BIT_EXT"
    )
  ,
    ( DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
    , "ERROR_BIT_EXT"
    )
  ]

instance Show DebugUtilsMessageSeverityFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixDebugUtilsMessageSeverityFlagBitsEXT
      showTableDebugUtilsMessageSeverityFlagBitsEXT
      conNameDebugUtilsMessageSeverityFlagBitsEXT
      (\(DebugUtilsMessageSeverityFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DebugUtilsMessageSeverityFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixDebugUtilsMessageSeverityFlagBitsEXT
      showTableDebugUtilsMessageSeverityFlagBitsEXT
      conNameDebugUtilsMessageSeverityFlagBitsEXT
      DebugUtilsMessageSeverityFlagBitsEXT

type DebugUtilsMessageTypeFlagsEXT = DebugUtilsMessageTypeFlagBitsEXT

-- | XrDebugUtilsMessageTypeFlagBitsEXT - XrDebugUtilsMessageTypeFlagBitsEXT
--
-- = See Also
--
-- No cross-references are available
newtype DebugUtilsMessageTypeFlagBitsEXT = DebugUtilsMessageTypeFlagBitsEXT Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrDebugUtilsMessageTypeFlagBitsEXT" "XR_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT = DebugUtilsMessageTypeFlagBitsEXT 0x0000000000000001

-- No documentation found for Nested "XrDebugUtilsMessageTypeFlagBitsEXT" "XR_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT = DebugUtilsMessageTypeFlagBitsEXT 0x0000000000000002

-- No documentation found for Nested "XrDebugUtilsMessageTypeFlagBitsEXT" "XR_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT = DebugUtilsMessageTypeFlagBitsEXT 0x0000000000000004

-- No documentation found for Nested "XrDebugUtilsMessageTypeFlagBitsEXT" "XR_DEBUG_UTILS_MESSAGE_TYPE_CONFORMANCE_BIT_EXT"
pattern DEBUG_UTILS_MESSAGE_TYPE_CONFORMANCE_BIT_EXT = DebugUtilsMessageTypeFlagBitsEXT 0x0000000000000008

conNameDebugUtilsMessageTypeFlagBitsEXT :: String
conNameDebugUtilsMessageTypeFlagBitsEXT = "DebugUtilsMessageTypeFlagBitsEXT"

enumPrefixDebugUtilsMessageTypeFlagBitsEXT :: String
enumPrefixDebugUtilsMessageTypeFlagBitsEXT = "DEBUG_UTILS_MESSAGE_TYPE_"

showTableDebugUtilsMessageTypeFlagBitsEXT :: [(DebugUtilsMessageTypeFlagBitsEXT, String)]
showTableDebugUtilsMessageTypeFlagBitsEXT =
  [
    ( DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
    , "GENERAL_BIT_EXT"
    )
  ,
    ( DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
    , "VALIDATION_BIT_EXT"
    )
  ,
    ( DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT
    , "PERFORMANCE_BIT_EXT"
    )
  ,
    ( DEBUG_UTILS_MESSAGE_TYPE_CONFORMANCE_BIT_EXT
    , "CONFORMANCE_BIT_EXT"
    )
  ]

instance Show DebugUtilsMessageTypeFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixDebugUtilsMessageTypeFlagBitsEXT
      showTableDebugUtilsMessageTypeFlagBitsEXT
      conNameDebugUtilsMessageTypeFlagBitsEXT
      (\(DebugUtilsMessageTypeFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DebugUtilsMessageTypeFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixDebugUtilsMessageTypeFlagBitsEXT
      showTableDebugUtilsMessageTypeFlagBitsEXT
      conNameDebugUtilsMessageTypeFlagBitsEXT
      DebugUtilsMessageTypeFlagBitsEXT

type FN_xrDebugUtilsMessengerCallbackEXT = DebugUtilsMessageSeverityFlagsEXT -> ("messageTypes" ::: DebugUtilsMessageTypeFlagsEXT) -> Ptr DebugUtilsMessengerCallbackDataEXT -> ("userData" ::: Ptr ()) -> IO Bool32
-- | PFN_xrDebugUtilsMessengerCallbackEXT - Type of callback function invoked
-- by the debug utils
--
-- == Parameter Descriptions
--
-- = Description
--
-- The callback /must/ not call 'destroyDebugUtilsMessengerEXT'.
--
-- The callback returns an
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrBool32 >
-- that indicates to the calling layer the application’s desire to abort
-- the call. A value of 'OpenXR.Core10.FundamentalTypes.TRUE' indicates
-- that the application wants to abort this call. If the application
-- returns 'OpenXR.Core10.FundamentalTypes.FALSE', the function /must/ not
-- be aborted. Applications /should/ always return
-- 'OpenXR.Core10.FundamentalTypes.FALSE' so that they see the same
-- behavior with and without validation layers enabled.
--
-- If the application returns 'OpenXR.Core10.FundamentalTypes.TRUE' from
-- its callback and the OpenXR call being aborted returns an
-- 'OpenXR.Core10.Enums.Result.Result', the layer will return
-- 'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'.
--
-- The object pointed to by @callbackData@ (and any pointers in it
-- recursively) /must/ be valid during the lifetime of the triggered
-- callback. It /may/ become invalid afterwards.
--
-- = See Also
--
-- 'DebugUtilsMessengerCreateInfoEXT', 'createDebugUtilsMessengerEXT'
type PFN_xrDebugUtilsMessengerCallbackEXT = FunPtr FN_xrDebugUtilsMessengerCallbackEXT


type EXT_debug_utils_SPEC_VERSION = 3

-- No documentation found for TopLevel "XR_EXT_debug_utils_SPEC_VERSION"
pattern EXT_debug_utils_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_debug_utils_SPEC_VERSION = 3


type EXT_DEBUG_UTILS_EXTENSION_NAME = "XR_EXT_debug_utils"

-- No documentation found for TopLevel "XR_EXT_DEBUG_UTILS_EXTENSION_NAME"
pattern EXT_DEBUG_UTILS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEBUG_UTILS_EXTENSION_NAME = "XR_EXT_debug_utils"

