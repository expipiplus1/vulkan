{-# language CPP #-}
-- | = Name
--
-- XR_KHR_android_thread_settings - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_android_thread_settings  XR_KHR_android_thread_settings>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 4
--
-- = Revision
--
-- 5
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'AndroidThreadTypeKHR', 'setAndroidApplicationThreadKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_android_thread_settings OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_android_thread_settings  ( setAndroidApplicationThreadKHR
                                                         , AndroidThreadTypeKHR( ANDROID_THREAD_TYPE_APPLICATION_MAIN_KHR
                                                                               , ANDROID_THREAD_TYPE_APPLICATION_WORKER_KHR
                                                                               , ANDROID_THREAD_TYPE_RENDERER_MAIN_KHR
                                                                               , ANDROID_THREAD_TYPE_RENDERER_WORKER_KHR
                                                                               , ..
                                                                               )
                                                         , KHR_android_thread_settings_SPEC_VERSION
                                                         , pattern KHR_android_thread_settings_SPEC_VERSION
                                                         , KHR_ANDROID_THREAD_SETTINGS_EXTENSION_NAME
                                                         , pattern KHR_ANDROID_THREAD_SETTINGS_EXTENSION_NAME
                                                         ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import GHC.Show (showsPrec)
import OpenXR.Zero (Zero)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Foreign.Storable (Storable)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import OpenXR.NamedType ((:::))
import OpenXR.Dynamic (InstanceCmds(pXrSetAndroidApplicationThreadKHR))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session(Session))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrSetAndroidApplicationThreadKHR
  :: FunPtr (Ptr Session_T -> AndroidThreadTypeKHR -> Word32 -> IO Result) -> Ptr Session_T -> AndroidThreadTypeKHR -> Word32 -> IO Result

-- | xrSetAndroidApplicationThreadKHR - declare threads to be of a certain
-- priority type
--
-- == Parameter Descriptions
--
-- = Description
--
-- 'setAndroidApplicationThreadKHR' allows to declare an XR-critical thread
-- and to classify it.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrSetAndroidApplicationThreadKHR-extension-notenabled# The
--     @XR_KHR_android_thread_settings@ extension /must/ be enabled prior
--     to calling 'setAndroidApplicationThreadKHR'
--
-- -   #VUID-xrSetAndroidApplicationThreadKHR-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrSetAndroidApplicationThreadKHR-threadType-parameter#
--     @threadType@ /must/ be a valid 'AndroidThreadTypeKHR' value
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
--     -   'OpenXR.Core10.Enums.Result.ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_VALIDATION_FAILURE'
--
--     -   'OpenXR.Core10.Enums.Result.ERROR_FUNCTION_UNSUPPORTED'
--
-- = See Also
--
-- 'AndroidThreadTypeKHR', 'OpenXR.Core10.Handles.Session'
setAndroidApplicationThreadKHR :: forall io
                                . (MonadIO io)
                               => -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
                                  Session
                               -> -- | @threadType@ is a classification of the declared thread allowing the XR
                                  -- runtime to apply the relevant priority and attributes. If such settings
                                  -- fail, the runtime /must/ return
                                  -- 'OpenXR.Core10.Enums.Result.ERROR_ANDROID_THREAD_SETTINGS_FAILURE_KHR'.
                                  AndroidThreadTypeKHR
                               -> -- | @threadId@ is the kernel thread ID of the declared thread, as returned
                                  -- by @gettid()@ or @android.os.process.myTid()@. If the thread ID is
                                  -- invalid, the runtime /must/ return
                                  -- 'OpenXR.Core10.Enums.Result.ERROR_ANDROID_THREAD_SETTINGS_ID_INVALID_KHR'.
                                  ("threadId" ::: Word32)
                               -> io (Result)
setAndroidApplicationThreadKHR session threadType threadId = liftIO $ do
  let xrSetAndroidApplicationThreadKHRPtr = pXrSetAndroidApplicationThreadKHR (case session of Session{instanceCmds} -> instanceCmds)
  unless (xrSetAndroidApplicationThreadKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrSetAndroidApplicationThreadKHR is null" Nothing Nothing
  let xrSetAndroidApplicationThreadKHR' = mkXrSetAndroidApplicationThreadKHR xrSetAndroidApplicationThreadKHRPtr
  r <- traceAroundEvent "xrSetAndroidApplicationThreadKHR" (xrSetAndroidApplicationThreadKHR' (sessionHandle (session)) (threadType) (threadId))
  when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


-- | XrAndroidThreadTypeKHR - Enum describing Android thread types
--
-- == Enumerants
--
-- = See Also
--
-- 'setAndroidApplicationThreadKHR'
newtype AndroidThreadTypeKHR = AndroidThreadTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'ANDROID_THREAD_TYPE_APPLICATION_MAIN_KHR'
-- hints the XR runtime that the thread is doing background CPU tasks
pattern ANDROID_THREAD_TYPE_APPLICATION_MAIN_KHR   = AndroidThreadTypeKHR 1
-- | 'ANDROID_THREAD_TYPE_APPLICATION_WORKER_KHR'
-- hints the XR runtime that the thread is doing time critical CPU tasks
pattern ANDROID_THREAD_TYPE_APPLICATION_WORKER_KHR = AndroidThreadTypeKHR 2
-- | 'ANDROID_THREAD_TYPE_RENDERER_MAIN_KHR'
-- hints the XR runtime that the thread is doing background graphics device
-- tasks
pattern ANDROID_THREAD_TYPE_RENDERER_MAIN_KHR      = AndroidThreadTypeKHR 3
-- | 'ANDROID_THREAD_TYPE_RENDERER_WORKER_KHR'
-- hints the XR runtime that the thread is doing time critical graphics
-- device tasks
pattern ANDROID_THREAD_TYPE_RENDERER_WORKER_KHR    = AndroidThreadTypeKHR 4
{-# complete ANDROID_THREAD_TYPE_APPLICATION_MAIN_KHR,
             ANDROID_THREAD_TYPE_APPLICATION_WORKER_KHR,
             ANDROID_THREAD_TYPE_RENDERER_MAIN_KHR,
             ANDROID_THREAD_TYPE_RENDERER_WORKER_KHR :: AndroidThreadTypeKHR #-}

conNameAndroidThreadTypeKHR :: String
conNameAndroidThreadTypeKHR = "AndroidThreadTypeKHR"

enumPrefixAndroidThreadTypeKHR :: String
enumPrefixAndroidThreadTypeKHR = "ANDROID_THREAD_TYPE_"

showTableAndroidThreadTypeKHR :: [(AndroidThreadTypeKHR, String)]
showTableAndroidThreadTypeKHR =
  [ (ANDROID_THREAD_TYPE_APPLICATION_MAIN_KHR  , "APPLICATION_MAIN_KHR")
  , (ANDROID_THREAD_TYPE_APPLICATION_WORKER_KHR, "APPLICATION_WORKER_KHR")
  , (ANDROID_THREAD_TYPE_RENDERER_MAIN_KHR     , "RENDERER_MAIN_KHR")
  , (ANDROID_THREAD_TYPE_RENDERER_WORKER_KHR   , "RENDERER_WORKER_KHR")
  ]

instance Show AndroidThreadTypeKHR where
  showsPrec = enumShowsPrec enumPrefixAndroidThreadTypeKHR
                            showTableAndroidThreadTypeKHR
                            conNameAndroidThreadTypeKHR
                            (\(AndroidThreadTypeKHR x) -> x)
                            (showsPrec 11)

instance Read AndroidThreadTypeKHR where
  readPrec = enumReadPrec enumPrefixAndroidThreadTypeKHR
                          showTableAndroidThreadTypeKHR
                          conNameAndroidThreadTypeKHR
                          AndroidThreadTypeKHR


type KHR_android_thread_settings_SPEC_VERSION = 5

-- No documentation found for TopLevel "XR_KHR_android_thread_settings_SPEC_VERSION"
pattern KHR_android_thread_settings_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_android_thread_settings_SPEC_VERSION = 5


type KHR_ANDROID_THREAD_SETTINGS_EXTENSION_NAME = "XR_KHR_android_thread_settings"

-- No documentation found for TopLevel "XR_KHR_ANDROID_THREAD_SETTINGS_EXTENSION_NAME"
pattern KHR_ANDROID_THREAD_SETTINGS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ANDROID_THREAD_SETTINGS_EXTENSION_NAME = "XR_KHR_android_thread_settings"

