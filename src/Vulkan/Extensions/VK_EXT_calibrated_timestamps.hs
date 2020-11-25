{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_calibrated_timestamps"
module Vulkan.Extensions.VK_EXT_calibrated_timestamps  ( getPhysicalDeviceCalibrateableTimeDomainsEXT
                                                       , getCalibratedTimestampsEXT
                                                       , CalibratedTimestampInfoEXT(..)
                                                       , TimeDomainEXT( TIME_DOMAIN_DEVICE_EXT
                                                                      , TIME_DOMAIN_CLOCK_MONOTONIC_EXT
                                                                      , TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT
                                                                      , TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT
                                                                      , ..
                                                                      )
                                                       , EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
                                                       , pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION
                                                       , EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
                                                       , pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
                                                       ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkGetCalibratedTimestampsEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceCalibrateableTimeDomainsEXT))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr TimeDomainEXT -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr TimeDomainEXT -> IO Result

-- No documentation found for TopLevel "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT"
getPhysicalDeviceCalibrateableTimeDomainsEXT :: forall io
                                              . (MonadIO io)
                                             => -- No documentation found for Nested "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT" "physicalDevice"
                                                PhysicalDevice
                                             -> io (Result, ("timeDomains" ::: Vector TimeDomainEXT))
getPhysicalDeviceCalibrateableTimeDomainsEXT physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceCalibrateableTimeDomainsEXTPtr = pVkGetPhysicalDeviceCalibrateableTimeDomainsEXT (instanceCmds (physicalDevice :: PhysicalDevice))
  lift $ unless (vkGetPhysicalDeviceCalibrateableTimeDomainsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceCalibrateableTimeDomainsEXT is null" Nothing Nothing
  let vkGetPhysicalDeviceCalibrateableTimeDomainsEXT' = mkVkGetPhysicalDeviceCalibrateableTimeDomainsEXT vkGetPhysicalDeviceCalibrateableTimeDomainsEXTPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPTimeDomainCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ vkGetPhysicalDeviceCalibrateableTimeDomainsEXT' physicalDevice' (pPTimeDomainCount) (nullPtr)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pTimeDomainCount <- lift $ peek @Word32 pPTimeDomainCount
  pPTimeDomains <- ContT $ bracket (callocBytes @TimeDomainEXT ((fromIntegral (pTimeDomainCount)) * 4)) free
  r' <- lift $ vkGetPhysicalDeviceCalibrateableTimeDomainsEXT' physicalDevice' (pPTimeDomainCount) (pPTimeDomains)
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pTimeDomainCount' <- lift $ peek @Word32 pPTimeDomainCount
  pTimeDomains' <- lift $ generateM (fromIntegral (pTimeDomainCount')) (\i -> peek @TimeDomainEXT ((pPTimeDomains `advancePtrBytes` (4 * (i)) :: Ptr TimeDomainEXT)))
  pure $ ((r'), pTimeDomains')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetCalibratedTimestampsEXT
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr CalibratedTimestampInfoEXT -> Ptr Word64 -> Ptr Word64 -> IO Result) -> Ptr Device_T -> Word32 -> Ptr CalibratedTimestampInfoEXT -> Ptr Word64 -> Ptr Word64 -> IO Result

-- No documentation found for TopLevel "vkGetCalibratedTimestampsEXT"
getCalibratedTimestampsEXT :: forall io
                            . (MonadIO io)
                           => -- No documentation found for Nested "vkGetCalibratedTimestampsEXT" "device"
                              Device
                           -> -- No documentation found for Nested "vkGetCalibratedTimestampsEXT" "pTimestampInfos"
                              ("timestampInfos" ::: Vector CalibratedTimestampInfoEXT)
                           -> io (("timestamps" ::: Vector Word64), ("maxDeviation" ::: Word64))
getCalibratedTimestampsEXT device timestampInfos = liftIO . evalContT $ do
  let vkGetCalibratedTimestampsEXTPtr = pVkGetCalibratedTimestampsEXT (deviceCmds (device :: Device))
  lift $ unless (vkGetCalibratedTimestampsEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetCalibratedTimestampsEXT is null" Nothing Nothing
  let vkGetCalibratedTimestampsEXT' = mkVkGetCalibratedTimestampsEXT vkGetCalibratedTimestampsEXTPtr
  pPTimestampInfos <- ContT $ allocaBytesAligned @CalibratedTimestampInfoEXT ((Data.Vector.length (timestampInfos)) * 24) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPTimestampInfos `plusPtr` (24 * (i)) :: Ptr CalibratedTimestampInfoEXT) (e)) (timestampInfos)
  pPTimestamps <- ContT $ bracket (callocBytes @Word64 ((fromIntegral ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))) * 8)) free
  pPMaxDeviation <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ vkGetCalibratedTimestampsEXT' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32)) (pPTimestampInfos) (pPTimestamps) (pPMaxDeviation)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pTimestamps <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))) (\i -> peek @Word64 ((pPTimestamps `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
  pMaxDeviation <- lift $ peek @Word64 pPMaxDeviation
  pure $ (pTimestamps, pMaxDeviation)



-- No documentation found for TopLevel "VkCalibratedTimestampInfoEXT"
data CalibratedTimestampInfoEXT = CalibratedTimestampInfoEXT
  { -- No documentation found for Nested "VkCalibratedTimestampInfoEXT" "timeDomain"
    timeDomain :: TimeDomainEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CalibratedTimestampInfoEXT)
#endif
deriving instance Show CalibratedTimestampInfoEXT

instance ToCStruct CalibratedTimestampInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CalibratedTimestampInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TimeDomainEXT)) (timeDomain)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TimeDomainEXT)) (zero)
    f

instance FromCStruct CalibratedTimestampInfoEXT where
  peekCStruct p = do
    timeDomain <- peek @TimeDomainEXT ((p `plusPtr` 16 :: Ptr TimeDomainEXT))
    pure $ CalibratedTimestampInfoEXT
             timeDomain


instance Storable CalibratedTimestampInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero CalibratedTimestampInfoEXT where
  zero = CalibratedTimestampInfoEXT
           zero


-- No documentation found for TopLevel "VkTimeDomainEXT"
newtype TimeDomainEXT = TimeDomainEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkTimeDomainEXT" "VK_TIME_DOMAIN_DEVICE_EXT"
pattern TIME_DOMAIN_DEVICE_EXT                    = TimeDomainEXT 0
-- No documentation found for Nested "VkTimeDomainEXT" "VK_TIME_DOMAIN_CLOCK_MONOTONIC_EXT"
pattern TIME_DOMAIN_CLOCK_MONOTONIC_EXT           = TimeDomainEXT 1
-- No documentation found for Nested "VkTimeDomainEXT" "VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT"
pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT       = TimeDomainEXT 2
-- No documentation found for Nested "VkTimeDomainEXT" "VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT"
pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT = TimeDomainEXT 3
{-# complete TIME_DOMAIN_DEVICE_EXT,
             TIME_DOMAIN_CLOCK_MONOTONIC_EXT,
             TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT,
             TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT :: TimeDomainEXT #-}

conNameTimeDomainEXT :: String
conNameTimeDomainEXT = "TimeDomainEXT"

enumPrefixTimeDomainEXT :: String
enumPrefixTimeDomainEXT = "TIME_DOMAIN_"

showTableTimeDomainEXT :: [(TimeDomainEXT, String)]
showTableTimeDomainEXT =
  [ (TIME_DOMAIN_DEVICE_EXT                   , "DEVICE_EXT")
  , (TIME_DOMAIN_CLOCK_MONOTONIC_EXT          , "CLOCK_MONOTONIC_EXT")
  , (TIME_DOMAIN_CLOCK_MONOTONIC_RAW_EXT      , "CLOCK_MONOTONIC_RAW_EXT")
  , (TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_EXT, "QUERY_PERFORMANCE_COUNTER_EXT")
  ]


instance Show TimeDomainEXT where
showsPrec = enumShowsPrec enumPrefixTimeDomainEXT
                          showTableTimeDomainEXT
                          conNameTimeDomainEXT
                          (\(TimeDomainEXT x) -> x)
                          (showsPrec 11)


instance Read TimeDomainEXT where
  readPrec = enumReadPrec enumPrefixTimeDomainEXT showTableTimeDomainEXT conNameTimeDomainEXT TimeDomainEXT


type EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION"
pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1


type EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_EXT_calibrated_timestamps"

-- No documentation found for TopLevel "VK_EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME"
pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_EXT_calibrated_timestamps"

