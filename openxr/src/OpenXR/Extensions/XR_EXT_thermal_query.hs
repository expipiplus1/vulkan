{-# language CPP #-}
-- | = Name
--
-- XR_EXT_thermal_query - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_thermal_query  XR_EXT_thermal_query>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 17
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
-- 'OpenXR.Extensions.XR_EXT_performance_settings.PerfSettingsDomainEXT',
-- 'OpenXR.Extensions.XR_EXT_performance_settings.PerfSettingsNotificationLevelEXT',
-- 'thermalGetTemperatureTrendEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_thermal_query OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_thermal_query  ( thermalGetTemperatureTrendEXT
                                               , EXT_thermal_query_SPEC_VERSION
                                               , pattern EXT_thermal_query_SPEC_VERSION
                                               , EXT_THERMAL_QUERY_EXTENSION_NAME
                                               , pattern EXT_THERMAL_QUERY_EXTENSION_NAME
                                               , PerfSettingsDomainEXT(..)
                                               , PerfSettingsNotificationLevelEXT(..)
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
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Foreign.C.Types (CFloat(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.Storable (Storable(peek))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Control.Monad.Trans.Cont (ContT(..))
import OpenXR.NamedType ((:::))
import OpenXR.Dynamic (InstanceCmds(pXrThermalGetTemperatureTrendEXT))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Extensions.XR_EXT_performance_settings (PerfSettingsDomainEXT)
import OpenXR.Extensions.XR_EXT_performance_settings (PerfSettingsDomainEXT(..))
import OpenXR.Extensions.XR_EXT_performance_settings (PerfSettingsNotificationLevelEXT)
import OpenXR.Extensions.XR_EXT_performance_settings (PerfSettingsNotificationLevelEXT(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Extensions.XR_EXT_performance_settings (PerfSettingsDomainEXT(..))
import OpenXR.Extensions.XR_EXT_performance_settings (PerfSettingsNotificationLevelEXT(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrThermalGetTemperatureTrendEXT
  :: FunPtr (Ptr Session_T -> PerfSettingsDomainEXT -> Ptr PerfSettingsNotificationLevelEXT -> Ptr CFloat -> Ptr CFloat -> IO Result) -> Ptr Session_T -> PerfSettingsDomainEXT -> Ptr PerfSettingsNotificationLevelEXT -> Ptr CFloat -> Ptr CFloat -> IO Result

-- | xrThermalGetTemperatureTrendEXT - xrThermalGetTemperatureTrendEXT
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrThermalGetTemperatureTrendEXT-extension-notenabled# The @@
--     extension /must/ be enabled prior to calling
--     'thermalGetTemperatureTrendEXT'
--
-- -   #VUID-xrThermalGetTemperatureTrendEXT-session-parameter# @session@
--     /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrThermalGetTemperatureTrendEXT-domain-parameter# @domain@
--     /must/ be a valid
--     'OpenXR.Extensions.XR_EXT_performance_settings.PerfSettingsDomainEXT'
--     value
--
-- -   #VUID-xrThermalGetTemperatureTrendEXT-notificationLevel-parameter#
--     @notificationLevel@ /must/ be a pointer to an
--     'OpenXR.Extensions.XR_EXT_performance_settings.PerfSettingsNotificationLevelEXT'
--     value
--
-- -   #VUID-xrThermalGetTemperatureTrendEXT-tempHeadroom-parameter#
--     @tempHeadroom@ /must/ be a pointer to a @float@ value
--
-- -   #VUID-xrThermalGetTemperatureTrendEXT-tempSlope-parameter#
--     @tempSlope@ /must/ be a pointer to a @float@ value
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
-- = See Also
--
-- 'OpenXR.Extensions.XR_EXT_performance_settings.PerfSettingsDomainEXT',
-- 'OpenXR.Extensions.XR_EXT_performance_settings.PerfSettingsNotificationLevelEXT',
-- 'OpenXR.Core10.Handles.Session'
thermalGetTemperatureTrendEXT :: forall io
                               . (MonadIO io)
                              => -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
                                 Session
                              -> -- | @domain@ : the processing domain
                                 PerfSettingsDomainEXT
                              -> io (Result, PerfSettingsNotificationLevelEXT, ("tempHeadroom" ::: Float), ("tempSlope" ::: Float))
thermalGetTemperatureTrendEXT session domain = liftIO . evalContT $ do
  let xrThermalGetTemperatureTrendEXTPtr = pXrThermalGetTemperatureTrendEXT (instanceCmds (session :: Session))
  lift $ unless (xrThermalGetTemperatureTrendEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrThermalGetTemperatureTrendEXT is null" Nothing Nothing
  let xrThermalGetTemperatureTrendEXT' = mkXrThermalGetTemperatureTrendEXT xrThermalGetTemperatureTrendEXTPtr
  pNotificationLevel <- ContT $ bracket (callocBytes @PerfSettingsNotificationLevelEXT 4) free
  pTempHeadroom <- ContT $ bracket (callocBytes @CFloat 4) free
  pTempSlope <- ContT $ bracket (callocBytes @CFloat 4) free
  r <- lift $ traceAroundEvent "xrThermalGetTemperatureTrendEXT" (xrThermalGetTemperatureTrendEXT' (sessionHandle (session)) (domain) (pNotificationLevel) (pTempHeadroom) (pTempSlope))
  lift $ when (r < SUCCESS) (throwIO (OpenXrException r))
  notificationLevel <- lift $ peek @PerfSettingsNotificationLevelEXT pNotificationLevel
  tempHeadroom <- lift $ peek @CFloat pTempHeadroom
  tempSlope <- lift $ peek @CFloat pTempSlope
  pure $ (r, notificationLevel, (coerce @CFloat @Float tempHeadroom), (coerce @CFloat @Float tempSlope))


type EXT_thermal_query_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_EXT_thermal_query_SPEC_VERSION"
pattern EXT_thermal_query_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_thermal_query_SPEC_VERSION = 1


type EXT_THERMAL_QUERY_EXTENSION_NAME = "XR_EXT_thermal_query"

-- No documentation found for TopLevel "XR_EXT_THERMAL_QUERY_EXTENSION_NAME"
pattern EXT_THERMAL_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_THERMAL_QUERY_EXTENSION_NAME = "XR_EXT_thermal_query"

