{-# language CPP #-}
-- | = Name
--
-- VK_KHR_calibrated_timestamps - device extension
--
-- = VK_KHR_calibrated_timestamps
--
-- [__Name String__]
--     @VK_KHR_calibrated_timestamps@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     544
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_calibrated_timestamps] @aqnuep%0A*Here describe the issue or question you have about the VK_KHR_calibrated_timestamps extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_calibrated_timestamps.adoc VK_EXT_calibrated_timestamps>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-07-12
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Alan Harrison, AMD
--
--     -   Derrick Owens, AMD
--
--     -   Daniel Rakos, RasterGrid
--
--     -   Faith Ekstrand, Intel
--
--     -   Keith Packard, Valve
--
-- == Description
--
-- This extension provides an interface to query calibrated timestamps
-- obtained quasi simultaneously from two time domains.
--
-- == New Commands
--
-- -   'getCalibratedTimestampsKHR'
--
-- -   'getPhysicalDeviceCalibrateableTimeDomainsKHR'
--
-- == New Structures
--
-- -   'CalibratedTimestampInfoKHR'
--
-- == New Enums
--
-- -   'Vulkan.Extensions.VK_EXT_present_timing.TimeDomainKHR'
--
-- == New Enum Constants
--
-- -   'KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME'
--
-- -   'KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2023-07-12 (Daniel Rakos)
--
--     -   Initial draft.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_calibrated_timestamps Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_calibrated_timestamps  ( getPhysicalDeviceCalibrateableTimeDomainsKHR
                                                       , getCalibratedTimestampsKHR
                                                       , CalibratedTimestampInfoKHR(..)
                                                       , KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION
                                                       , pattern KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION
                                                       , KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
                                                       , pattern KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME
                                                       , TimeDomainKHR(..)
                                                       ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetCalibratedTimestampsKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Dynamic (InstanceCmds(pVkGetPhysicalDeviceCalibrateableTimeDomainsKHR))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (PhysicalDevice(PhysicalDevice))
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_present_timing (SwapchainCalibratedTimestampInfoEXT)
import Vulkan.Extensions.VK_EXT_present_timing (TimeDomainKHR)
import Vulkan.Extensions.VK_EXT_present_timing (TimeDomainKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_EXT_present_timing (TimeDomainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCalibrateableTimeDomainsKHR
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr TimeDomainKHR -> IO Result) -> Ptr PhysicalDevice_T -> Ptr Word32 -> Ptr TimeDomainKHR -> IO Result

-- | vkGetPhysicalDeviceCalibrateableTimeDomainsKHR - Query calibrateable
-- time domains
--
-- = Description
--
-- If @pTimeDomains@ is @NULL@, then the number of calibrateable time
-- domains supported for the given @physicalDevice@ is returned in
-- @pTimeDomainCount@. Otherwise, @pTimeDomainCount@ /must/ point to a
-- variable set by the application to the number of elements in the
-- @pTimeDomains@ array, and on return the variable is overwritten with the
-- number of values actually written to @pTimeDomains@. If the value of
-- @pTimeDomainCount@ is less than the number of calibrateable time domains
-- supported, at most @pTimeDomainCount@ values will be written to
-- @pTimeDomains@, and 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be
-- returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate
-- that not all the available time domains were returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPhysicalDeviceCalibrateableTimeDomainsKHR-physicalDevice-parameter#
--     @physicalDevice@ /must/ be a valid
--     'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkGetPhysicalDeviceCalibrateableTimeDomainsKHR-pTimeDomainCount-parameter#
--     @pTimeDomainCount@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetPhysicalDeviceCalibrateableTimeDomainsKHR-pTimeDomains-parameter#
--     If the value referenced by @pTimeDomainCount@ is not @0@, and
--     @pTimeDomains@ is not @NULL@, @pTimeDomains@ /must/ be a valid
--     pointer to an array of @pTimeDomainCount@
--     'Vulkan.Extensions.VK_EXT_present_timing.TimeDomainKHR' values
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps VK_EXT_calibrated_timestamps>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps VK_KHR_calibrated_timestamps>,
-- 'Vulkan.Core10.Handles.PhysicalDevice',
-- 'Vulkan.Extensions.VK_EXT_present_timing.TimeDomainKHR'
getPhysicalDeviceCalibrateableTimeDomainsKHR :: forall io
                                              . (MonadIO io)
                                             => -- | @physicalDevice@ is the physical device from which to query the set of
                                                -- calibrateable time domains.
                                                PhysicalDevice
                                             -> io (Result, ("timeDomains" ::: Vector TimeDomainKHR))
getPhysicalDeviceCalibrateableTimeDomainsKHR physicalDevice = liftIO . evalContT $ do
  let vkGetPhysicalDeviceCalibrateableTimeDomainsKHRPtr = pVkGetPhysicalDeviceCalibrateableTimeDomainsKHR (case physicalDevice of PhysicalDevice{instanceCmds} -> instanceCmds)
  lift $ unless (vkGetPhysicalDeviceCalibrateableTimeDomainsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPhysicalDeviceCalibrateableTimeDomainsKHR is null" Nothing Nothing
  let vkGetPhysicalDeviceCalibrateableTimeDomainsKHR' = mkVkGetPhysicalDeviceCalibrateableTimeDomainsKHR vkGetPhysicalDeviceCalibrateableTimeDomainsKHRPtr
  let physicalDevice' = physicalDeviceHandle (physicalDevice)
  pPTimeDomainCount <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetPhysicalDeviceCalibrateableTimeDomainsKHR" (vkGetPhysicalDeviceCalibrateableTimeDomainsKHR'
                                                                                   physicalDevice'
                                                                                   (pPTimeDomainCount)
                                                                                   (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pTimeDomainCount <- lift $ peek @Word32 pPTimeDomainCount
  pPTimeDomains <- ContT $ bracket (callocBytes @TimeDomainKHR ((fromIntegral (pTimeDomainCount)) * 4)) free
  r' <- lift $ traceAroundEvent "vkGetPhysicalDeviceCalibrateableTimeDomainsKHR" (vkGetPhysicalDeviceCalibrateableTimeDomainsKHR'
                                                                                    physicalDevice'
                                                                                    (pPTimeDomainCount)
                                                                                    (pPTimeDomains))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pTimeDomainCount' <- lift $ peek @Word32 pPTimeDomainCount
  pTimeDomains' <- lift $ generateM (fromIntegral (pTimeDomainCount')) (\i -> peek @TimeDomainKHR ((pPTimeDomains `advancePtrBytes` (4 * (i)) :: Ptr TimeDomainKHR)))
  pure $ ((r'), pTimeDomains')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetCalibratedTimestampsKHR
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct CalibratedTimestampInfoKHR) -> Ptr Word64 -> Ptr Word64 -> IO Result) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct CalibratedTimestampInfoKHR) -> Ptr Word64 -> Ptr Word64 -> IO Result

-- | vkGetCalibratedTimestampsKHR - Query calibrated timestamps
--
-- = Description
--
-- The maximum deviation /may/ vary between calls to
-- 'getCalibratedTimestampsKHR' even for the same set of time domains due
-- to implementation and platform specific reasons. It is the application’s
-- responsibility to assess whether the returned maximum deviation makes
-- the timestamp values suitable for any particular purpose and /can/
-- choose to re-issue the timestamp calibration call pursuing a lower
-- deviation value.
--
-- Calibrated timestamp values /can/ be extrapolated to estimate future
-- coinciding timestamp values, however, depending on the nature of the
-- time domains and other properties of the platform extrapolating values
-- over a sufficiently long period of time /may/ no longer be accurate
-- enough to fit any particular purpose, so applications are expected to
-- re-calibrate the timestamps on a regular basis.
--
-- == Valid Usage
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-timeDomain-09246# The
--     @timeDomain@ value of each 'CalibratedTimestampInfoKHR' in
--     @pTimestampInfos@ /must/ be unique
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-pTimestampInfos-parameter#
--     @pTimestampInfos@ /must/ be a valid pointer to an array of
--     @timestampCount@ valid 'CalibratedTimestampInfoKHR' structures
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-pTimestamps-parameter#
--     @pTimestamps@ /must/ be a valid pointer to an array of
--     @timestampCount@ @uint64_t@ values
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-pMaxDeviation-parameter#
--     @pMaxDeviation@ /must/ be a valid pointer to a @uint64_t@ value
--
-- -   #VUID-vkGetCalibratedTimestampsKHR-timestampCount-arraylength#
--     @timestampCount@ /must/ be greater than @0@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps VK_EXT_calibrated_timestamps>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps VK_KHR_calibrated_timestamps>,
-- 'CalibratedTimestampInfoKHR', 'Vulkan.Core10.Handles.Device'
getCalibratedTimestampsKHR :: forall io
                            . (MonadIO io)
                           => -- | @device@ is the logical device used to perform the query.
                              Device
                           -> -- | @pTimestampInfos@ is a pointer to an array of @timestampCount@
                              -- 'CalibratedTimestampInfoKHR' structures, describing the time domains the
                              -- calibrated timestamps should be captured from.
                              ("timestampInfos" ::: Vector (SomeStruct CalibratedTimestampInfoKHR))
                           -> io (("timestamps" ::: Vector Word64), ("maxDeviation" ::: Word64))
getCalibratedTimestampsKHR device timestampInfos = liftIO . evalContT $ do
  let vkGetCalibratedTimestampsKHRPtr = pVkGetCalibratedTimestampsKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetCalibratedTimestampsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetCalibratedTimestampsKHR is null" Nothing Nothing
  let vkGetCalibratedTimestampsKHR' = mkVkGetCalibratedTimestampsKHR vkGetCalibratedTimestampsKHRPtr
  pPTimestampInfos <- ContT $ allocaBytes @(CalibratedTimestampInfoKHR _) ((Data.Vector.length (timestampInfos)) * 24)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPTimestampInfos `plusPtr` (24 * (i)) :: Ptr (CalibratedTimestampInfoKHR _))) (e) . ($ ())) (timestampInfos)
  pPTimestamps <- ContT $ bracket (callocBytes @Word64 ((fromIntegral ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))) * 8)) free
  pPMaxDeviation <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ traceAroundEvent "vkGetCalibratedTimestampsKHR" (vkGetCalibratedTimestampsKHR'
                                                                 (deviceHandle (device))
                                                                 ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))
                                                                 (forgetExtensions (pPTimestampInfos))
                                                                 (pPTimestamps)
                                                                 (pPMaxDeviation))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pTimestamps <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (timestampInfos)) :: Word32))) (\i -> peek @Word64 ((pPTimestamps `advancePtrBytes` (8 * (i)) :: Ptr Word64)))
  pMaxDeviation <- lift $ peek @Word64 pPMaxDeviation
  pure $ (pTimestamps, pMaxDeviation)


-- | VkCalibratedTimestampInfoKHR - Structure specifying the input parameters
-- of a calibrated timestamp query
--
-- == Valid Usage
--
-- -   #VUID-VkCalibratedTimestampInfoKHR-timeDomain-02354# @timeDomain@
--     /must/ be one of the
--     'Vulkan.Extensions.VK_EXT_present_timing.TimeDomainKHR' values
--     returned by 'getPhysicalDeviceCalibrateableTimeDomainsKHR'
--
-- -   #VUID-VkCalibratedTimestampInfoKHR-timeDomain-12227# If @timeDomain@
--     is
--     'Vulkan.Extensions.VK_EXT_present_timing.TIME_DOMAIN_SWAPCHAIN_LOCAL_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_present_timing.TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT',
--     the @pNext@ chain /must/ include a
--     'Vulkan.Extensions.VK_EXT_present_timing.SwapchainCalibratedTimestampInfoEXT'
--     structure
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkCalibratedTimestampInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR'
--
-- -   #VUID-VkCalibratedTimestampInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_present_timing.SwapchainCalibratedTimestampInfoEXT'
--
-- -   #VUID-VkCalibratedTimestampInfoKHR-sType-unique# The @sType@ value
--     of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkCalibratedTimestampInfoKHR-timeDomain-parameter#
--     @timeDomain@ /must/ be a valid
--     'Vulkan.Extensions.VK_EXT_present_timing.TimeDomainKHR' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps VK_EXT_calibrated_timestamps>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps VK_KHR_calibrated_timestamps>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.VK_EXT_present_timing.TimeDomainKHR',
-- 'getCalibratedTimestampsKHR', 'getCalibratedTimestampsKHR'
data CalibratedTimestampInfoKHR (es :: [Type]) = CalibratedTimestampInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @timeDomain@ is a
    -- 'Vulkan.Extensions.VK_EXT_present_timing.TimeDomainKHR' value specifying
    -- the time domain from which the calibrated timestamp value should be
    -- returned.
    timeDomain :: TimeDomainKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (CalibratedTimestampInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (CalibratedTimestampInfoKHR es)

instance Extensible CalibratedTimestampInfoKHR where
  extensibleTypeName = "CalibratedTimestampInfoKHR"
  setNext CalibratedTimestampInfoKHR{..} next' = CalibratedTimestampInfoKHR{next = next', ..}
  getNext CalibratedTimestampInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends CalibratedTimestampInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @SwapchainCalibratedTimestampInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss CalibratedTimestampInfoKHR es
         , PokeChain es ) => ToCStruct (CalibratedTimestampInfoKHR es) where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p CalibratedTimestampInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr TimeDomainKHR)) (timeDomain)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr TimeDomainKHR)) (zero)
    lift $ f

instance ( Extendss CalibratedTimestampInfoKHR es
         , PeekChain es ) => FromCStruct (CalibratedTimestampInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    timeDomain <- peek @TimeDomainKHR ((p `plusPtr` 16 :: Ptr TimeDomainKHR))
    pure $ CalibratedTimestampInfoKHR
             next timeDomain

instance es ~ '[] => Zero (CalibratedTimestampInfoKHR es) where
  zero = CalibratedTimestampInfoKHR
           ()
           zero


type KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION"
pattern KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_CALIBRATED_TIMESTAMPS_SPEC_VERSION = 1


type KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_KHR_calibrated_timestamps"

-- No documentation found for TopLevel "VK_KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME"
pattern KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_CALIBRATED_TIMESTAMPS_EXTENSION_NAME = "VK_KHR_calibrated_timestamps"

