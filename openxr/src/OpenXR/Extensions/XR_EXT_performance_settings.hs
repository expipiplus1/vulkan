{-# language CPP #-}
-- | = Name
--
-- XR_EXT_performance_settings - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_performance_settings  XR_EXT_performance_settings>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 16
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
-- 'EventDataPerfSettingsEXT', 'PerfSettingsDomainEXT',
-- 'PerfSettingsLevelEXT', 'PerfSettingsNotificationLevelEXT',
-- 'PerfSettingsSubDomainEXT', 'perfSettingsSetPerformanceLevelEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_performance_settings OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_performance_settings  ( perfSettingsSetPerformanceLevelEXT
                                                      , EventDataPerfSettingsEXT(..)
                                                      , PerfSettingsLevelEXT( PERF_SETTINGS_LEVEL_POWER_SAVINGS_EXT
                                                                            , PERF_SETTINGS_LEVEL_SUSTAINED_LOW_EXT
                                                                            , PERF_SETTINGS_LEVEL_SUSTAINED_HIGH_EXT
                                                                            , PERF_SETTINGS_LEVEL_BOOST_EXT
                                                                            , ..
                                                                            )
                                                      , PerfSettingsDomainEXT( PERF_SETTINGS_DOMAIN_CPU_EXT
                                                                             , PERF_SETTINGS_DOMAIN_GPU_EXT
                                                                             , ..
                                                                             )
                                                      , PerfSettingsSubDomainEXT( PERF_SETTINGS_SUB_DOMAIN_COMPOSITING_EXT
                                                                                , PERF_SETTINGS_SUB_DOMAIN_RENDERING_EXT
                                                                                , PERF_SETTINGS_SUB_DOMAIN_THERMAL_EXT
                                                                                , ..
                                                                                )
                                                      , PerfSettingsNotificationLevelEXT( PERF_SETTINGS_NOTIF_LEVEL_NORMAL_EXT
                                                                                        , PERF_SETTINGS_NOTIF_LEVEL_WARNING_EXT
                                                                                        , PERF_SETTINGS_NOTIF_LEVEL_IMPAIRED_EXT
                                                                                        , ..
                                                                                        )
                                                      , EXT_performance_settings_SPEC_VERSION
                                                      , pattern EXT_performance_settings_SPEC_VERSION
                                                      , EXT_PERFORMANCE_SETTINGS_EXTENSION_NAME
                                                      , pattern EXT_PERFORMANCE_SETTINGS_EXTENSION_NAME
                                                      ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import OpenXR.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
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
import Data.Kind (Type)
import OpenXR.Core10.OtherTypes (EventDataBaseHeader(..))
import OpenXR.Dynamic (InstanceCmds(pXrPerfSettingsSetPerformanceLevelEXT))
import OpenXR.Core10.OtherTypes (IsEventData(..))
import OpenXR.Exception (OpenXrException(..))
import OpenXR.Core10.Enums.Result (Result)
import OpenXR.Core10.Enums.Result (Result(..))
import OpenXR.Core10.Handles (Session)
import OpenXR.Core10.Handles (Session(..))
import OpenXR.Core10.Handles (Session_T)
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.Result (Result(SUCCESS))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_EVENT_DATA_PERF_SETTINGS_EXT))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkXrPerfSettingsSetPerformanceLevelEXT
  :: FunPtr (Ptr Session_T -> PerfSettingsDomainEXT -> PerfSettingsLevelEXT -> IO Result) -> Ptr Session_T -> PerfSettingsDomainEXT -> PerfSettingsLevelEXT -> IO Result

-- | xrPerfSettingsSetPerformanceLevelEXT -
-- xrPerfSettingsSetPerformanceLevelEXT
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-xrPerfSettingsSetPerformanceLevelEXT-extension-notenabled# The
--     @@ extension /must/ be enabled prior to calling
--     'perfSettingsSetPerformanceLevelEXT'
--
-- -   #VUID-xrPerfSettingsSetPerformanceLevelEXT-session-parameter#
--     @session@ /must/ be a valid 'OpenXR.Core10.Handles.Session' handle
--
-- -   #VUID-xrPerfSettingsSetPerformanceLevelEXT-domain-parameter#
--     @domain@ /must/ be a valid 'PerfSettingsDomainEXT' value
--
-- -   #VUID-xrPerfSettingsSetPerformanceLevelEXT-level-parameter# @level@
--     /must/ be a valid 'PerfSettingsLevelEXT' value
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
-- 'PerfSettingsDomainEXT', 'PerfSettingsLevelEXT',
-- 'OpenXR.Core10.Handles.Session'
perfSettingsSetPerformanceLevelEXT :: forall io
                                    . (MonadIO io)
                                   => -- | @session@ is a valid 'OpenXR.Core10.Handles.Session' handle.
                                      Session
                                   -> -- | @domain@: the processing domain for which the level hint is applied
                                      PerfSettingsDomainEXT
                                   -> -- | @level@: the level hint to be applied
                                      PerfSettingsLevelEXT
                                   -> io (Result)
perfSettingsSetPerformanceLevelEXT session domain level = liftIO $ do
  let xrPerfSettingsSetPerformanceLevelEXTPtr = pXrPerfSettingsSetPerformanceLevelEXT (instanceCmds (session :: Session))
  unless (xrPerfSettingsSetPerformanceLevelEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for xrPerfSettingsSetPerformanceLevelEXT is null" Nothing Nothing
  let xrPerfSettingsSetPerformanceLevelEXT' = mkXrPerfSettingsSetPerformanceLevelEXT xrPerfSettingsSetPerformanceLevelEXTPtr
  r <- traceAroundEvent "xrPerfSettingsSetPerformanceLevelEXT" (xrPerfSettingsSetPerformanceLevelEXT' (sessionHandle (session)) (domain) (level))
  when (r < SUCCESS) (throwIO (OpenXrException r))
  pure $ (r)


-- | XrEventDataPerfSettingsEXT - XrEventDataPerfSettingsEXT
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrEventDataPerfSettingsEXT-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'EventDataPerfSettingsEXT'
--
-- -   #VUID-XrEventDataPerfSettingsEXT-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_EVENT_DATA_PERF_SETTINGS_EXT'
--
-- -   #VUID-XrEventDataPerfSettingsEXT-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrEventDataPerfSettingsEXT-domain-parameter# @domain@ /must/
--     be a valid 'PerfSettingsDomainEXT' value
--
-- -   #VUID-XrEventDataPerfSettingsEXT-subDomain-parameter# @subDomain@
--     /must/ be a valid 'PerfSettingsSubDomainEXT' value
--
-- -   #VUID-XrEventDataPerfSettingsEXT-fromLevel-parameter# @fromLevel@
--     /must/ be a valid 'PerfSettingsNotificationLevelEXT' value
--
-- -   #VUID-XrEventDataPerfSettingsEXT-toLevel-parameter# @toLevel@ /must/
--     be a valid 'PerfSettingsNotificationLevelEXT' value
--
-- = See Also
--
-- 'PerfSettingsDomainEXT', 'PerfSettingsNotificationLevelEXT',
-- 'PerfSettingsSubDomainEXT',
-- 'OpenXR.Core10.Enums.StructureType.StructureType'
data EventDataPerfSettingsEXT = EventDataPerfSettingsEXT
  { -- | @domain@ : processing domain in which a threshold has been crossed
    domain :: PerfSettingsDomainEXT
  , -- | @subDomain@ : system area in which a threshold has been crossed
    subDomain :: PerfSettingsSubDomainEXT
  , -- | @fromLevel@ : enumerated notification level which has been exited
    fromLevel :: PerfSettingsNotificationLevelEXT
  , -- | @toLevel@ : enumerated notification level which has been entered
    toLevel :: PerfSettingsNotificationLevelEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventDataPerfSettingsEXT)
#endif
deriving instance Show EventDataPerfSettingsEXT

instance IsEventData EventDataPerfSettingsEXT where
  toEventDataBaseHeader EventDataPerfSettingsEXT{} = EventDataBaseHeader{type' = TYPE_EVENT_DATA_PERF_SETTINGS_EXT}

instance ToCStruct EventDataPerfSettingsEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventDataPerfSettingsEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_PERF_SETTINGS_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerfSettingsDomainEXT)) (domain)
    poke ((p `plusPtr` 20 :: Ptr PerfSettingsSubDomainEXT)) (subDomain)
    poke ((p `plusPtr` 24 :: Ptr PerfSettingsNotificationLevelEXT)) (fromLevel)
    poke ((p `plusPtr` 28 :: Ptr PerfSettingsNotificationLevelEXT)) (toLevel)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_EVENT_DATA_PERF_SETTINGS_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PerfSettingsDomainEXT)) (zero)
    poke ((p `plusPtr` 20 :: Ptr PerfSettingsSubDomainEXT)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PerfSettingsNotificationLevelEXT)) (zero)
    poke ((p `plusPtr` 28 :: Ptr PerfSettingsNotificationLevelEXT)) (zero)
    f

instance FromCStruct EventDataPerfSettingsEXT where
  peekCStruct p = do
    domain <- peek @PerfSettingsDomainEXT ((p `plusPtr` 16 :: Ptr PerfSettingsDomainEXT))
    subDomain <- peek @PerfSettingsSubDomainEXT ((p `plusPtr` 20 :: Ptr PerfSettingsSubDomainEXT))
    fromLevel <- peek @PerfSettingsNotificationLevelEXT ((p `plusPtr` 24 :: Ptr PerfSettingsNotificationLevelEXT))
    toLevel <- peek @PerfSettingsNotificationLevelEXT ((p `plusPtr` 28 :: Ptr PerfSettingsNotificationLevelEXT))
    pure $ EventDataPerfSettingsEXT
             domain subDomain fromLevel toLevel

instance Storable EventDataPerfSettingsEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventDataPerfSettingsEXT where
  zero = EventDataPerfSettingsEXT
           zero
           zero
           zero
           zero


-- | XrPerfSettingsLevelEXT - XrPerfSettingsLevelEXT
--
-- = See Also
--
-- 'perfSettingsSetPerformanceLevelEXT'
newtype PerfSettingsLevelEXT = PerfSettingsLevelEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "XrPerfSettingsLevelEXT" "XR_PERF_SETTINGS_LEVEL_POWER_SAVINGS_EXT"
pattern PERF_SETTINGS_LEVEL_POWER_SAVINGS_EXT  = PerfSettingsLevelEXT 0
-- No documentation found for Nested "XrPerfSettingsLevelEXT" "XR_PERF_SETTINGS_LEVEL_SUSTAINED_LOW_EXT"
pattern PERF_SETTINGS_LEVEL_SUSTAINED_LOW_EXT  = PerfSettingsLevelEXT 25
-- No documentation found for Nested "XrPerfSettingsLevelEXT" "XR_PERF_SETTINGS_LEVEL_SUSTAINED_HIGH_EXT"
pattern PERF_SETTINGS_LEVEL_SUSTAINED_HIGH_EXT = PerfSettingsLevelEXT 50
-- No documentation found for Nested "XrPerfSettingsLevelEXT" "XR_PERF_SETTINGS_LEVEL_BOOST_EXT"
pattern PERF_SETTINGS_LEVEL_BOOST_EXT          = PerfSettingsLevelEXT 75
{-# complete PERF_SETTINGS_LEVEL_POWER_SAVINGS_EXT,
             PERF_SETTINGS_LEVEL_SUSTAINED_LOW_EXT,
             PERF_SETTINGS_LEVEL_SUSTAINED_HIGH_EXT,
             PERF_SETTINGS_LEVEL_BOOST_EXT :: PerfSettingsLevelEXT #-}

conNamePerfSettingsLevelEXT :: String
conNamePerfSettingsLevelEXT = "PerfSettingsLevelEXT"

enumPrefixPerfSettingsLevelEXT :: String
enumPrefixPerfSettingsLevelEXT = "PERF_SETTINGS_LEVEL_"

showTablePerfSettingsLevelEXT :: [(PerfSettingsLevelEXT, String)]
showTablePerfSettingsLevelEXT =
  [ (PERF_SETTINGS_LEVEL_POWER_SAVINGS_EXT , "POWER_SAVINGS_EXT")
  , (PERF_SETTINGS_LEVEL_SUSTAINED_LOW_EXT , "SUSTAINED_LOW_EXT")
  , (PERF_SETTINGS_LEVEL_SUSTAINED_HIGH_EXT, "SUSTAINED_HIGH_EXT")
  , (PERF_SETTINGS_LEVEL_BOOST_EXT         , "BOOST_EXT")
  ]

instance Show PerfSettingsLevelEXT where
  showsPrec = enumShowsPrec enumPrefixPerfSettingsLevelEXT
                            showTablePerfSettingsLevelEXT
                            conNamePerfSettingsLevelEXT
                            (\(PerfSettingsLevelEXT x) -> x)
                            (showsPrec 11)

instance Read PerfSettingsLevelEXT where
  readPrec = enumReadPrec enumPrefixPerfSettingsLevelEXT
                          showTablePerfSettingsLevelEXT
                          conNamePerfSettingsLevelEXT
                          PerfSettingsLevelEXT


-- | XrPerfSettingsDomainEXT - XrPerfSettingsDomainEXT
--
-- = See Also
--
-- 'EventDataPerfSettingsEXT', 'perfSettingsSetPerformanceLevelEXT',
-- 'OpenXR.Extensions.XR_EXT_thermal_query.thermalGetTemperatureTrendEXT'
newtype PerfSettingsDomainEXT = PerfSettingsDomainEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "XrPerfSettingsDomainEXT" "XR_PERF_SETTINGS_DOMAIN_CPU_EXT"
pattern PERF_SETTINGS_DOMAIN_CPU_EXT = PerfSettingsDomainEXT 1
-- No documentation found for Nested "XrPerfSettingsDomainEXT" "XR_PERF_SETTINGS_DOMAIN_GPU_EXT"
pattern PERF_SETTINGS_DOMAIN_GPU_EXT = PerfSettingsDomainEXT 2
{-# complete PERF_SETTINGS_DOMAIN_CPU_EXT,
             PERF_SETTINGS_DOMAIN_GPU_EXT :: PerfSettingsDomainEXT #-}

conNamePerfSettingsDomainEXT :: String
conNamePerfSettingsDomainEXT = "PerfSettingsDomainEXT"

enumPrefixPerfSettingsDomainEXT :: String
enumPrefixPerfSettingsDomainEXT = "PERF_SETTINGS_DOMAIN_"

showTablePerfSettingsDomainEXT :: [(PerfSettingsDomainEXT, String)]
showTablePerfSettingsDomainEXT = [(PERF_SETTINGS_DOMAIN_CPU_EXT, "CPU_EXT"), (PERF_SETTINGS_DOMAIN_GPU_EXT, "GPU_EXT")]

instance Show PerfSettingsDomainEXT where
  showsPrec = enumShowsPrec enumPrefixPerfSettingsDomainEXT
                            showTablePerfSettingsDomainEXT
                            conNamePerfSettingsDomainEXT
                            (\(PerfSettingsDomainEXT x) -> x)
                            (showsPrec 11)

instance Read PerfSettingsDomainEXT where
  readPrec = enumReadPrec enumPrefixPerfSettingsDomainEXT
                          showTablePerfSettingsDomainEXT
                          conNamePerfSettingsDomainEXT
                          PerfSettingsDomainEXT


-- | XrPerfSettingsSubDomainEXT - XrPerfSettingsSubDomainEXT
--
-- = See Also
--
-- 'EventDataPerfSettingsEXT'
newtype PerfSettingsSubDomainEXT = PerfSettingsSubDomainEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "XrPerfSettingsSubDomainEXT" "XR_PERF_SETTINGS_SUB_DOMAIN_COMPOSITING_EXT"
pattern PERF_SETTINGS_SUB_DOMAIN_COMPOSITING_EXT = PerfSettingsSubDomainEXT 1
-- No documentation found for Nested "XrPerfSettingsSubDomainEXT" "XR_PERF_SETTINGS_SUB_DOMAIN_RENDERING_EXT"
pattern PERF_SETTINGS_SUB_DOMAIN_RENDERING_EXT   = PerfSettingsSubDomainEXT 2
-- No documentation found for Nested "XrPerfSettingsSubDomainEXT" "XR_PERF_SETTINGS_SUB_DOMAIN_THERMAL_EXT"
pattern PERF_SETTINGS_SUB_DOMAIN_THERMAL_EXT     = PerfSettingsSubDomainEXT 3
{-# complete PERF_SETTINGS_SUB_DOMAIN_COMPOSITING_EXT,
             PERF_SETTINGS_SUB_DOMAIN_RENDERING_EXT,
             PERF_SETTINGS_SUB_DOMAIN_THERMAL_EXT :: PerfSettingsSubDomainEXT #-}

conNamePerfSettingsSubDomainEXT :: String
conNamePerfSettingsSubDomainEXT = "PerfSettingsSubDomainEXT"

enumPrefixPerfSettingsSubDomainEXT :: String
enumPrefixPerfSettingsSubDomainEXT = "PERF_SETTINGS_SUB_DOMAIN_"

showTablePerfSettingsSubDomainEXT :: [(PerfSettingsSubDomainEXT, String)]
showTablePerfSettingsSubDomainEXT =
  [ (PERF_SETTINGS_SUB_DOMAIN_COMPOSITING_EXT, "COMPOSITING_EXT")
  , (PERF_SETTINGS_SUB_DOMAIN_RENDERING_EXT  , "RENDERING_EXT")
  , (PERF_SETTINGS_SUB_DOMAIN_THERMAL_EXT    , "THERMAL_EXT")
  ]

instance Show PerfSettingsSubDomainEXT where
  showsPrec = enumShowsPrec enumPrefixPerfSettingsSubDomainEXT
                            showTablePerfSettingsSubDomainEXT
                            conNamePerfSettingsSubDomainEXT
                            (\(PerfSettingsSubDomainEXT x) -> x)
                            (showsPrec 11)

instance Read PerfSettingsSubDomainEXT where
  readPrec = enumReadPrec enumPrefixPerfSettingsSubDomainEXT
                          showTablePerfSettingsSubDomainEXT
                          conNamePerfSettingsSubDomainEXT
                          PerfSettingsSubDomainEXT


-- | XrPerfSettingsNotificationLevelEXT - XrPerfSettingsNotificationLevelEXT
--
-- = See Also
--
-- 'EventDataPerfSettingsEXT',
-- 'OpenXR.Extensions.XR_EXT_thermal_query.thermalGetTemperatureTrendEXT'
newtype PerfSettingsNotificationLevelEXT = PerfSettingsNotificationLevelEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "XrPerfSettingsNotificationLevelEXT" "XR_PERF_SETTINGS_NOTIF_LEVEL_NORMAL_EXT"
pattern PERF_SETTINGS_NOTIF_LEVEL_NORMAL_EXT   = PerfSettingsNotificationLevelEXT 0
-- No documentation found for Nested "XrPerfSettingsNotificationLevelEXT" "XR_PERF_SETTINGS_NOTIF_LEVEL_WARNING_EXT"
pattern PERF_SETTINGS_NOTIF_LEVEL_WARNING_EXT  = PerfSettingsNotificationLevelEXT 25
-- No documentation found for Nested "XrPerfSettingsNotificationLevelEXT" "XR_PERF_SETTINGS_NOTIF_LEVEL_IMPAIRED_EXT"
pattern PERF_SETTINGS_NOTIF_LEVEL_IMPAIRED_EXT = PerfSettingsNotificationLevelEXT 75
{-# complete PERF_SETTINGS_NOTIF_LEVEL_NORMAL_EXT,
             PERF_SETTINGS_NOTIF_LEVEL_WARNING_EXT,
             PERF_SETTINGS_NOTIF_LEVEL_IMPAIRED_EXT :: PerfSettingsNotificationLevelEXT #-}

conNamePerfSettingsNotificationLevelEXT :: String
conNamePerfSettingsNotificationLevelEXT = "PerfSettingsNotificationLevelEXT"

enumPrefixPerfSettingsNotificationLevelEXT :: String
enumPrefixPerfSettingsNotificationLevelEXT = "PERF_SETTINGS_NOTIF_LEVEL_"

showTablePerfSettingsNotificationLevelEXT :: [(PerfSettingsNotificationLevelEXT, String)]
showTablePerfSettingsNotificationLevelEXT =
  [ (PERF_SETTINGS_NOTIF_LEVEL_NORMAL_EXT  , "NORMAL_EXT")
  , (PERF_SETTINGS_NOTIF_LEVEL_WARNING_EXT , "WARNING_EXT")
  , (PERF_SETTINGS_NOTIF_LEVEL_IMPAIRED_EXT, "IMPAIRED_EXT")
  ]

instance Show PerfSettingsNotificationLevelEXT where
  showsPrec = enumShowsPrec enumPrefixPerfSettingsNotificationLevelEXT
                            showTablePerfSettingsNotificationLevelEXT
                            conNamePerfSettingsNotificationLevelEXT
                            (\(PerfSettingsNotificationLevelEXT x) -> x)
                            (showsPrec 11)

instance Read PerfSettingsNotificationLevelEXT where
  readPrec = enumReadPrec enumPrefixPerfSettingsNotificationLevelEXT
                          showTablePerfSettingsNotificationLevelEXT
                          conNamePerfSettingsNotificationLevelEXT
                          PerfSettingsNotificationLevelEXT


type EXT_performance_settings_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_EXT_performance_settings_SPEC_VERSION"
pattern EXT_performance_settings_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_performance_settings_SPEC_VERSION = 1


type EXT_PERFORMANCE_SETTINGS_EXTENSION_NAME = "XR_EXT_performance_settings"

-- No documentation found for TopLevel "XR_EXT_PERFORMANCE_SETTINGS_EXTENSION_NAME"
pattern EXT_PERFORMANCE_SETTINGS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PERFORMANCE_SETTINGS_EXTENSION_NAME = "XR_EXT_performance_settings"

