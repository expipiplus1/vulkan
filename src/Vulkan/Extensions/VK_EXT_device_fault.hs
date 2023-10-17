{-# language CPP #-}
-- | = Name
--
-- VK_EXT_device_fault - device extension
--
-- == VK_EXT_device_fault
--
-- [__Name String__]
--     @VK_EXT_device_fault@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     342
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Ralph Potter <<data:image/png;base64, GitLab>>r_potter
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_device_fault.adoc VK_EXT_device_fault>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-03-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ralph Potter, Samsung
--
--     -   Stuart Smith, AMD
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Mark Bellamy, ARM
--
--     -   Andrew Ellem, Google
--
--     -   Alex Walters, IMG
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Baldur Karlsson, Valve
--
-- == Description
--
-- Device loss can be triggered by a variety of issues, including invalid
-- API usage, implementation errors, or hardware failures.
--
-- This extension introduces a new command: 'getDeviceFaultInfoEXT', which
-- may be called subsequent to a
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' error code having been
-- returned by the implementation. This command allows developers to query
-- for additional information on GPU faults which may have caused device
-- loss, and to generate binary crash dumps, which may be loaded into
-- external tools for further diagnosis.
--
-- == New Commands
--
-- -   'getDeviceFaultInfoEXT'
--
-- == New Structures
--
-- -   'DeviceFaultAddressInfoEXT'
--
-- -   'DeviceFaultCountsEXT'
--
-- -   'DeviceFaultInfoEXT'
--
-- -   'DeviceFaultVendorBinaryHeaderVersionOneEXT'
--
-- -   'DeviceFaultVendorInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFaultFeaturesEXT'
--
-- == New Enums
--
-- -   'DeviceFaultAddressTypeEXT'
--
-- -   'DeviceFaultVendorBinaryHeaderVersionEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEVICE_FAULT_EXTENSION_NAME'
--
-- -   'EXT_DEVICE_FAULT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_COUNTS_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-10-19 (Ralph Potter)
--
--     -   Initial revision
--
-- == See Also
--
-- 'DeviceFaultAddressInfoEXT', 'DeviceFaultAddressTypeEXT',
-- 'DeviceFaultCountsEXT', 'DeviceFaultInfoEXT',
-- 'DeviceFaultVendorBinaryHeaderVersionEXT',
-- 'DeviceFaultVendorBinaryHeaderVersionOneEXT',
-- 'DeviceFaultVendorInfoEXT', 'PhysicalDeviceFaultFeaturesEXT',
-- 'getDeviceFaultInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_device_fault Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_device_fault  ( getDeviceFaultInfoEXT
                                              , PhysicalDeviceFaultFeaturesEXT(..)
                                              , DeviceFaultAddressInfoEXT(..)
                                              , DeviceFaultVendorInfoEXT(..)
                                              , DeviceFaultCountsEXT(..)
                                              , DeviceFaultInfoEXT(..)
                                              , DeviceFaultVendorBinaryHeaderVersionOneEXT(..)
                                              , DeviceFaultAddressTypeEXT( DEVICE_FAULT_ADDRESS_TYPE_NONE_EXT
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_EXT
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_EXT
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_EXT
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_EXT
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_EXT
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_EXT
                                                                         , ..
                                                                         )
                                              , DeviceFaultVendorBinaryHeaderVersionEXT( DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_EXT
                                                                                       , ..
                                                                                       )
                                              , EXT_DEVICE_FAULT_SPEC_VERSION
                                              , pattern EXT_DEVICE_FAULT_SPEC_VERSION
                                              , EXT_DEVICE_FAULT_EXTENSION_NAME
                                              , pattern EXT_DEVICE_FAULT_EXTENSION_NAME
                                              ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceFaultInfoEXT))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_FAULT_COUNTS_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_FAULT_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceFaultInfoEXT
  :: FunPtr (Ptr Device_T -> Ptr DeviceFaultCountsEXT -> Ptr DeviceFaultInfoEXT -> IO Result) -> Ptr Device_T -> Ptr DeviceFaultCountsEXT -> Ptr DeviceFaultInfoEXT -> IO Result

-- | vkGetDeviceFaultInfoEXT - Reports diagnostic fault information on the
-- specified logical device
--
-- = Description
--
-- If @pFaultInfo@ is @NULL@, then the counts of corresponding additional
-- fault information structures available are returned in the
-- @addressInfoCount@ and @vendorInfoCount@ members of @pFaultCounts@.
-- Additionally, the size of any vendor-specific binary crash dump is
-- returned in the @vendorBinarySize@ member of @pFaultCounts@.
--
-- If @pFaultInfo@ is not @NULL@, @pFaultCounts@ /must/ point to a
-- 'DeviceFaultCountsEXT' structure with each structure count or size
-- member (@addressInfoCount@, @vendorInfoCount@, @vendorBinarySize@) set
-- by the user to the number of elements in the corresponding output array
-- member of @pFaultInfo@ (@pAddressInfos@ and @pVendorInfos@), or to the
-- size of the output buffer in bytes (@pVendorBinaryData@). On return,
-- each structure count member is overwritten with the number of structures
-- actually written to the corresponding output array member of
-- @pFaultInfo@. Similarly, @vendorBinarySize@ is overwritten with the
-- number of bytes actually written to the @pVendorBinaryData@ member of
-- @pFaultInfo@.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-deviceFaultVendorBinary vendor-specific crash dumps>
-- feature is not enabled, then implementations /must/ set
-- @pFaultCounts@->vendorBinarySize to zero and /must/ not modify
-- @pFaultInfo@->pVendorBinaryData.
--
-- If any @pFaultCounts@ structure count member is less than the number of
-- corresponding fault properties available, at most structure count
-- (@addressInfoCount@, @vendorInfoCount@) elements will be written to the
-- associated @pFaultInfo@ output array. Similarly, if @vendorBinarySize@
-- is less than the size in bytes of the available crash dump data, at most
-- @vendorBinarySize@ elements will be written to @pVendorBinaryData@.
--
-- If @pFaultInfo@ is @NULL@, then subsequent calls to
-- 'getDeviceFaultInfoEXT' for the same @device@ /must/ return identical
-- values in the @addressInfoCount@, @vendorInfoCount@ and
-- @vendorBinarySize@ members of @pFaultCounts@.
--
-- If @pFaultInfo@ is not @NULL@, then subsequent calls to
-- 'getDeviceFaultInfoEXT' for the same @device@ /must/ return identical
-- values in the output members of @pFaultInfo@ (@pAddressInfos@,
-- @pVendorInfos@, @pVendorBinaryData@), up to the limits described by the
-- structure count and buffer size members of @pFaultCounts@
-- (@addressInfoCount@, @vendorInfoCount@, @vendorBinarySize@). If the
-- sizes of the output members of @pFaultInfo@ increase for a subsequent
-- call to 'getDeviceFaultInfoEXT', then supplementary information /may/ be
-- returned in the additional available space.
--
-- If any @pFaultCounts@ structure count member is smaller than the number
-- of corresponding fault properties available, or if
-- @pFaultCounts@->vendorBinarySize is smaller than the size in bytes of
-- the generated binary crash dump data,
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS', to indicate that not all the
-- available properties were returned.
--
-- If @pFaultCounts@->vendorBinarySize is less than what is necessary to
-- store the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#vendor-binary-crash-dumps binary crash dump header>,
-- nothing will be written to @pFaultInfo@->pVendorBinaryData and zero will
-- be written to @pFaultCounts@->vendorBinarySize.
--
-- == Valid Usage
--
-- -   #VUID-vkGetDeviceFaultInfoEXT-device-07336# @device@ /must/ be in
--     the /lost/ state
--
-- -   #VUID-vkGetDeviceFaultInfoEXT-pFaultCounts-07337# If the value
--     referenced by @pFaultCounts->addressInfoCount@ is not @0@, and
--     @pFaultInfo->pAddressInfos@ is not @NULL@,
--     @pFaultInfo->pAddressInfos@ must be a valid pointer to an array of
--     @pFaultCounts->addressInfoCount@ 'DeviceFaultAddressInfoEXT'
--     structures
--
-- -   #VUID-vkGetDeviceFaultInfoEXT-pFaultCounts-07338# If the value
--     referenced by @pFaultCounts->vendorInfoCount@ is not @0@, and
--     @pFaultInfo->pVendorInfos@ is not @NULL@, @pFaultInfo->pVendorInfos@
--     must be a valid pointer to an array of
--     @pFaultCounts->vendorInfoCount@ 'DeviceFaultVendorInfoEXT'
--     structures
--
-- -   #VUID-vkGetDeviceFaultInfoEXT-pFaultCounts-07339# If the value
--     referenced by @pFaultCounts->vendorBinarySize@ is not @0@, and
--     @pFaultInfo->pVendorBinaryData@ is not @NULL@,
--     @pFaultInfo->pVendorBinaryData@ must be a valid pointer to an array
--     of @pFaultCounts->vendorBinarySize@ bytes
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceFaultInfoEXT-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceFaultInfoEXT-pFaultCounts-parameter# @pFaultCounts@
--     /must/ be a valid pointer to a 'DeviceFaultCountsEXT' structure
--
-- -   #VUID-vkGetDeviceFaultInfoEXT-pFaultInfo-parameter# If @pFaultInfo@
--     is not @NULL@, @pFaultInfo@ /must/ be a valid pointer to a
--     'DeviceFaultInfoEXT' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceFaultCountsEXT',
-- 'DeviceFaultInfoEXT'
getDeviceFaultInfoEXT :: forall io
                       . (MonadIO io)
                      => -- | @device@ is the logical device from which to query the diagnostic fault
                         -- information.
                         Device
                      -> io (Result, DeviceFaultCountsEXT, DeviceFaultInfoEXT)
getDeviceFaultInfoEXT device = liftIO . evalContT $ do
  let vkGetDeviceFaultInfoEXTPtr = pVkGetDeviceFaultInfoEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceFaultInfoEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceFaultInfoEXT is null" Nothing Nothing
  let vkGetDeviceFaultInfoEXT' = mkVkGetDeviceFaultInfoEXT vkGetDeviceFaultInfoEXTPtr
  pPFaultCounts <- ContT (withZeroCStruct @DeviceFaultCountsEXT)
  pPFaultInfo <- ContT (withZeroCStruct @DeviceFaultInfoEXT)
  r <- lift $ traceAroundEvent "vkGetDeviceFaultInfoEXT" (vkGetDeviceFaultInfoEXT'
                                                            (deviceHandle (device))
                                                            (pPFaultCounts)
                                                            (pPFaultInfo))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFaultCounts <- lift $ peekCStruct @DeviceFaultCountsEXT pPFaultCounts
  pFaultInfo <- lift $ peekCStruct @DeviceFaultInfoEXT pPFaultInfo
  pure $ (r, pFaultCounts, pFaultInfo)


-- | VkPhysicalDeviceFaultFeaturesEXT - Structure indicating support for
-- device fault reporting
--
-- = Members
--
-- The members of the 'PhysicalDeviceFaultFeaturesEXT' structure describe
-- the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceFaultFeaturesEXT' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceFaultFeaturesEXT' /can/ also be used in the
-- @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to selectively
-- enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFaultFeaturesEXT = PhysicalDeviceFaultFeaturesEXT
  { -- | #features-deviceFault# @deviceFault@ indicates that the implementation
    -- supports the reporting of device fault information.
    deviceFault :: Bool
  , -- | #features-deviceFaultVendorBinary# @deviceFaultVendorBinary@ indicates
    -- that the implementation supports the generation of vendor-specific
    -- binary crash dumps. These may provide additional information when
    -- imported into vendor-specific external tools.
    deviceFaultVendorBinary :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFaultFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceFaultFeaturesEXT

instance ToCStruct PhysicalDeviceFaultFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFaultFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceFault))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (deviceFaultVendorBinary))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFaultFeaturesEXT where
  peekCStruct p = do
    deviceFault <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    deviceFaultVendorBinary <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceFaultFeaturesEXT
             (bool32ToBool deviceFault) (bool32ToBool deviceFaultVendorBinary)

instance Storable PhysicalDeviceFaultFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFaultFeaturesEXT where
  zero = PhysicalDeviceFaultFeaturesEXT
           zero
           zero


-- | VkDeviceFaultAddressInfoEXT - Structure specifying GPU virtual address
-- information
--
-- = Description
--
-- The combination of @reportedAddress@ and @addressPrecision@ allow the
-- possible range of addresses to be calculated, such that:
--
-- > lower_address = (pInfo->reportedAddress & ~(pInfo->addressPrecision-1))
-- > upper_address = (pInfo->reportedAddress |  (pInfo->addressPrecision-1))
--
-- Note
--
-- It is valid for the @reportedAddress@ to contain a more precise address
-- than indicated by @addressPrecision@. In this case, the value of
-- @reportedAddress@ should be treated as an additional hint as to the
-- value of the address that triggered the page fault, or to the value of
-- an instruction pointer.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'DeviceFaultAddressTypeEXT', 'DeviceFaultInfoEXT',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
data DeviceFaultAddressInfoEXT = DeviceFaultAddressInfoEXT
  { -- | @addressType@ is either the type of memory operation that triggered a
    -- page fault, or the type of association between an instruction pointer
    -- and a fault.
    --
    -- #VUID-VkDeviceFaultAddressInfoEXT-addressType-parameter# @addressType@
    -- /must/ be a valid 'DeviceFaultAddressTypeEXT' value
    addressType :: DeviceFaultAddressTypeEXT
  , -- | @reportedAddress@ is the GPU virtual address recorded by the device.
    reportedAddress :: DeviceAddress
  , -- | @addressPrecision@ is a power of two value that specifies how precisely
    -- the device can report the address.
    addressPrecision :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceFaultAddressInfoEXT)
#endif
deriving instance Show DeviceFaultAddressInfoEXT

instance ToCStruct DeviceFaultAddressInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultAddressInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceFaultAddressTypeEXT)) (addressType)
    poke ((p `plusPtr` 8 :: Ptr DeviceAddress)) (reportedAddress)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (addressPrecision)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceFaultAddressTypeEXT)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct DeviceFaultAddressInfoEXT where
  peekCStruct p = do
    addressType <- peek @DeviceFaultAddressTypeEXT ((p `plusPtr` 0 :: Ptr DeviceFaultAddressTypeEXT))
    reportedAddress <- peek @DeviceAddress ((p `plusPtr` 8 :: Ptr DeviceAddress))
    addressPrecision <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ DeviceFaultAddressInfoEXT
             addressType reportedAddress addressPrecision

instance Storable DeviceFaultAddressInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceFaultAddressInfoEXT where
  zero = DeviceFaultAddressInfoEXT
           zero
           zero
           zero


-- | VkDeviceFaultVendorInfoEXT - Structure specifying vendor-specific fault
-- information
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'DeviceFaultInfoEXT'
data DeviceFaultVendorInfoEXT = DeviceFaultVendorInfoEXT
  { -- | @description@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which is a human readable description of
    -- the fault.
    description :: ByteString
  , -- | @vendorFaultCode@ is the vendor-specific fault code for this fault.
    vendorFaultCode :: Word64
  , -- | @vendorFaultData@ is the vendor-specific fault data associated with this
    -- fault.
    vendorFaultData :: Word64
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceFaultVendorInfoEXT)
#endif
deriving instance Show DeviceFaultVendorInfoEXT

instance ToCStruct DeviceFaultVendorInfoEXT where
  withCStruct x f = allocaBytes 272 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultVendorInfoEXT{..} f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    poke ((p `plusPtr` 256 :: Ptr Word64)) (vendorFaultCode)
    poke ((p `plusPtr` 264 :: Ptr Word64)) (vendorFaultData)
    f
  cStructSize = 272
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 0 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    poke ((p `plusPtr` 256 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 264 :: Ptr Word64)) (zero)
    f

instance FromCStruct DeviceFaultVendorInfoEXT where
  peekCStruct p = do
    description <- packCString (lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    vendorFaultCode <- peek @Word64 ((p `plusPtr` 256 :: Ptr Word64))
    vendorFaultData <- peek @Word64 ((p `plusPtr` 264 :: Ptr Word64))
    pure $ DeviceFaultVendorInfoEXT
             description vendorFaultCode vendorFaultData

instance Storable DeviceFaultVendorInfoEXT where
  sizeOf ~_ = 272
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceFaultVendorInfoEXT where
  zero = DeviceFaultVendorInfoEXT
           mempty
           zero
           zero


-- | VkDeviceFaultCountsEXT - Structure specifying device fault information
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceFaultInfoEXT'
data DeviceFaultCountsEXT = DeviceFaultCountsEXT
  { -- | @addressInfoCount@ is the number of 'DeviceFaultAddressInfoEXT'
    -- structures describing either memory accesses which /may/ have caused a
    -- page fault, or the addresses of active instructions at the time of the
    -- fault.
    addressInfoCount :: Word32
  , -- | @vendorInfoCount@ is the number of 'DeviceFaultVendorInfoEXT' structures
    -- describing vendor-specific fault information.
    vendorInfoCount :: Word32
  , -- | @vendorBinarySize@ is the size in bytes of a vendor-specific binary
    -- crash dump, which may provide additional information when imported into
    -- external tools.
    vendorBinarySize :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceFaultCountsEXT)
#endif
deriving instance Show DeviceFaultCountsEXT

instance ToCStruct DeviceFaultCountsEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultCountsEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_FAULT_COUNTS_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (addressInfoCount)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (vendorInfoCount)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (vendorBinarySize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_FAULT_COUNTS_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DeviceFaultCountsEXT where
  peekCStruct p = do
    addressInfoCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    vendorInfoCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    vendorBinarySize <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    pure $ DeviceFaultCountsEXT
             addressInfoCount vendorInfoCount vendorBinarySize

instance Storable DeviceFaultCountsEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceFaultCountsEXT where
  zero = DeviceFaultCountsEXT
           zero
           zero
           zero


-- | VkDeviceFaultInfoEXT - Structure specifying device fault information
--
-- = Description
--
-- An implementation /should/ populate as many members of
-- 'DeviceFaultInfoEXT' as possible, given the information available at the
-- time of the fault and the constraints of the implementation itself.
--
-- Due to hardware limitations, @pAddressInfos@ describes ranges of GPU
-- virtual address space, rather than precise addresses. The precise memory
-- address accessed or the precise value of the instruction pointer /must/
-- lie within the region described.
--
-- Note
--
-- Each element of @pAddressInfos@ describes either:
--
-- -   A memory access which may have triggered a page fault and may have
--     contributed to device loss
--
-- -   The value of an active instruction pointer at the time a fault
--     occurred. This value may be indicative of the active pipeline or
--     shader at the time of device loss
--
-- Comparison of the GPU virtual addresses described by @pAddressInfos@ to
-- GPU virtual address ranges reported by the
-- @VK_EXT_device_address_binding_report@ extension may allow applications
-- to correlate between these addresses and Vulkan objects. Applications
-- should be aware that these addresses may also correspond to resources
-- internal to an implementation, which will not be reported via the
-- @VK_EXT_device_address_binding_report@ extension.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceFaultInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_INFO_EXT'
--
-- -   #VUID-VkDeviceFaultInfoEXT-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkDeviceFaultInfoEXT-pAddressInfos-parameter# If
--     @pAddressInfos@ is not @NULL@, @pAddressInfos@ /must/ be a valid
--     pointer to a 'DeviceFaultAddressInfoEXT' structure
--
-- -   #VUID-VkDeviceFaultInfoEXT-pVendorInfos-parameter# If @pVendorInfos@
--     is not @NULL@, @pVendorInfos@ /must/ be a valid pointer to a
--     'DeviceFaultVendorInfoEXT' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'DeviceFaultAddressInfoEXT', 'DeviceFaultVendorInfoEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceFaultInfoEXT'
data DeviceFaultInfoEXT = DeviceFaultInfoEXT
  { -- | @description@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which is a human readable description of
    -- the fault.
    description :: ByteString
  , -- | @pAddressInfos@ is @NULL@ or a pointer to an array of
    -- 'DeviceFaultAddressInfoEXT' structures describing either memory accesses
    -- which /may/ have caused a page fault, or describing active instruction
    -- pointers at the time of the fault. If not @NULL@, each element of
    -- @pAddressInfos@ describes the a bounded region of GPU virtual address
    -- space containing either the GPU virtual address accessed, or the value
    -- of an active instruction pointer.
    addressInfos :: Ptr DeviceFaultAddressInfoEXT
  , -- | @pVendorInfos@ is @NULL@ or a pointer to an array of
    -- 'DeviceFaultVendorInfoEXT' structures describing vendor-specific fault
    -- information.
    vendorInfos :: Ptr DeviceFaultVendorInfoEXT
  , -- | @pVendorBinaryData@ is @NULL@ or a pointer to @vendorBinarySize@ number
    -- of bytes of data, which will be populated with a vendor-specific binary
    -- crash dump, as described in
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#vendor-binary-crash-dumps Vendor Binary Crash Dumps>.
    vendorBinaryData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceFaultInfoEXT)
#endif
deriving instance Show DeviceFaultInfoEXT

instance ToCStruct DeviceFaultInfoEXT where
  withCStruct x f = allocaBytes 296 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_FAULT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    poke ((p `plusPtr` 272 :: Ptr (Ptr DeviceFaultAddressInfoEXT))) (addressInfos)
    poke ((p `plusPtr` 280 :: Ptr (Ptr DeviceFaultVendorInfoEXT))) (vendorInfos)
    poke ((p `plusPtr` 288 :: Ptr (Ptr ()))) (vendorBinaryData)
    f
  cStructSize = 296
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_FAULT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    f

instance FromCStruct DeviceFaultInfoEXT where
  peekCStruct p = do
    description <- packCString (lowerArrayPtr ((p `plusPtr` 16 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    pAddressInfos <- peek @(Ptr DeviceFaultAddressInfoEXT) ((p `plusPtr` 272 :: Ptr (Ptr DeviceFaultAddressInfoEXT)))
    pVendorInfos <- peek @(Ptr DeviceFaultVendorInfoEXT) ((p `plusPtr` 280 :: Ptr (Ptr DeviceFaultVendorInfoEXT)))
    pVendorBinaryData <- peek @(Ptr ()) ((p `plusPtr` 288 :: Ptr (Ptr ())))
    pure $ DeviceFaultInfoEXT
             description pAddressInfos pVendorInfos pVendorBinaryData

instance Storable DeviceFaultInfoEXT where
  sizeOf ~_ = 296
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceFaultInfoEXT where
  zero = DeviceFaultInfoEXT
           mempty
           zero
           zero
           zero


-- | VkDeviceFaultVendorBinaryHeaderVersionOneEXT - Structure describing the
-- layout of the vendor binary crash dump header
--
-- = Description
--
-- Unlike most structures declared by the Vulkan API, all fields of this
-- structure are written with the least significant byte first, regardless
-- of host byte-order.
--
-- The C language specification does not define the packing of structure
-- members. This layout assumes tight structure member packing, with
-- members laid out in the order listed in the structure, and the intended
-- size of the structure is 56 bytes. If a compiler produces code that
-- diverges from that pattern, applications /must/ employ another method to
-- set values at the correct offsets.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'DeviceFaultVendorBinaryHeaderVersionEXT'
data DeviceFaultVendorBinaryHeaderVersionOneEXT = DeviceFaultVendorBinaryHeaderVersionOneEXT
  { -- | @headerSize@ is the length in bytes of the crash dump header.
    --
    -- #VUID-VkDeviceFaultVendorBinaryHeaderVersionOneEXT-headerSize-07340#
    -- @headerSize@ /must/ be 56
    headerSize :: Word32
  , -- | @headerVersion@ is a 'DeviceFaultVendorBinaryHeaderVersionEXT' enum
    -- value specifying the version of the header. A consumer of the crash dump
    -- /should/ use the header version to interpret the remainder of the
    -- header.
    --
    -- #VUID-VkDeviceFaultVendorBinaryHeaderVersionOneEXT-headerVersion-07341#
    -- @headerVersion@ /must/ be
    -- 'DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_EXT'
    --
    -- #VUID-VkDeviceFaultVendorBinaryHeaderVersionOneEXT-headerVersion-parameter#
    -- @headerVersion@ /must/ be a valid
    -- 'DeviceFaultVendorBinaryHeaderVersionEXT' value
    headerVersion :: DeviceFaultVendorBinaryHeaderVersionEXT
  , -- | @vendorID@ is the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@vendorID@
    -- of the implementation.
    vendorID :: Word32
  , -- | @deviceID@ is the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@deviceID@
    -- of the implementation.
    deviceID :: Word32
  , -- | @driverVersion@ is the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@driverVersion@
    -- of the implementation.
    driverVersion :: Word32
  , -- | @pipelineCacheUUID@ is an array of
    -- 'Vulkan.Core10.APIConstants.UUID_SIZE' @uint8_t@ values matching the
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@pipelineCacheUUID@
    -- property of the implementation.
    pipelineCacheUUID :: ByteString
  , -- | @applicationNameOffset@ is zero, or an offset from the base address of
    -- the crash dump header to a null-terminated UTF-8 string containing the
    -- name of the application. If @applicationNameOffset@ is non-zero, this
    -- string /must/ match the application name specified via
    -- 'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@pApplicationName@
    -- during instance creation.
    applicationNameOffset :: Word32
  , -- | @applicationVersion@ /must/ be zero or the value specified by
    -- 'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@applicationVersion@
    -- during instance creation.
    applicationVersion :: Word32
  , -- | @engineNameOffset@ is zero, or an offset from the base address of the
    -- crash dump header to a null-terminated UTF-8 string containing the name
    -- of the engine (if any) used to create the application. If
    -- @engineNameOffset@ is non-zero, this string /must/ match the engine name
    -- specified via
    -- 'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@pEngineName@
    -- during instance creation.
    engineNameOffset :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceFaultVendorBinaryHeaderVersionOneEXT)
#endif
deriving instance Show DeviceFaultVendorBinaryHeaderVersionOneEXT

instance ToCStruct DeviceFaultVendorBinaryHeaderVersionOneEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultVendorBinaryHeaderVersionOneEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (headerSize)
    poke ((p `plusPtr` 4 :: Ptr DeviceFaultVendorBinaryHeaderVersionEXT)) (headerVersion)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (vendorID)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (deviceID)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (driverVersion)
    pokeFixedLengthByteString ((p `plusPtr` 20 :: Ptr (FixedArray UUID_SIZE Word8))) (pipelineCacheUUID)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (applicationNameOffset)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (applicationVersion)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (engineNameOffset)
    f
  cStructSize = 48
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr DeviceFaultVendorBinaryHeaderVersionEXT)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    pokeFixedLengthByteString ((p `plusPtr` 20 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceFaultVendorBinaryHeaderVersionOneEXT where
  peekCStruct p = do
    headerSize <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    headerVersion <- peek @DeviceFaultVendorBinaryHeaderVersionEXT ((p `plusPtr` 4 :: Ptr DeviceFaultVendorBinaryHeaderVersionEXT))
    vendorID <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    deviceID <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    driverVersion <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pipelineCacheUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 20 :: Ptr (FixedArray UUID_SIZE Word8)))
    applicationNameOffset <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    applicationVersion <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    engineNameOffset <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pure $ DeviceFaultVendorBinaryHeaderVersionOneEXT
             headerSize
             headerVersion
             vendorID
             deviceID
             driverVersion
             pipelineCacheUUID
             applicationNameOffset
             applicationVersion
             engineNameOffset

instance Storable DeviceFaultVendorBinaryHeaderVersionOneEXT where
  sizeOf ~_ = 48
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceFaultVendorBinaryHeaderVersionOneEXT where
  zero = DeviceFaultVendorBinaryHeaderVersionOneEXT
           zero
           zero
           zero
           zero
           zero
           mempty
           zero
           zero
           zero


-- | VkDeviceFaultAddressTypeEXT - Page fault access types
--
-- = Description
--
-- Note
--
-- The instruction pointer values recorded may not identify the specific
-- instruction(s) that triggered the fault. The relationship between the
-- instruction pointer reported and triggering instruction will be
-- vendor-specific.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'DeviceFaultAddressInfoEXT'
newtype DeviceFaultAddressTypeEXT = DeviceFaultAddressTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DEVICE_FAULT_ADDRESS_TYPE_NONE_EXT' specifies that
-- 'DeviceFaultAddressInfoEXT' does not describe a page fault, or an
-- instruction address.
pattern DEVICE_FAULT_ADDRESS_TYPE_NONE_EXT = DeviceFaultAddressTypeEXT 0

-- | 'DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_EXT' specifies that
-- 'DeviceFaultAddressInfoEXT' describes a page fault triggered by an
-- invalid read operation.
pattern DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_EXT = DeviceFaultAddressTypeEXT 1

-- | 'DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_EXT' specifies that
-- 'DeviceFaultAddressInfoEXT' describes a page fault triggered by an
-- invalid write operation.
pattern DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_EXT = DeviceFaultAddressTypeEXT 2

-- | 'DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_EXT' describes a page fault
-- triggered by an attempt to execute non-executable memory.
pattern DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_EXT = DeviceFaultAddressTypeEXT 3

-- | 'DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_EXT' specifies an
-- instruction pointer value at the time the fault occurred. This may or
-- may not be related to a fault.
pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_EXT = DeviceFaultAddressTypeEXT 4

-- | 'DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_EXT' specifies an
-- instruction pointer value associated with an invalid instruction fault.
pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_EXT = DeviceFaultAddressTypeEXT 5

-- | 'DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_EXT' specifies an
-- instruction pointer value associated with a fault.
pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_EXT = DeviceFaultAddressTypeEXT 6

{-# COMPLETE
  DEVICE_FAULT_ADDRESS_TYPE_NONE_EXT
  , DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_EXT
  , DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_EXT
  , DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_EXT
  , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_EXT
  , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_EXT
  , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_EXT ::
    DeviceFaultAddressTypeEXT
  #-}

conNameDeviceFaultAddressTypeEXT :: String
conNameDeviceFaultAddressTypeEXT = "DeviceFaultAddressTypeEXT"

enumPrefixDeviceFaultAddressTypeEXT :: String
enumPrefixDeviceFaultAddressTypeEXT = "DEVICE_FAULT_ADDRESS_TYPE_"

showTableDeviceFaultAddressTypeEXT :: [(DeviceFaultAddressTypeEXT, String)]
showTableDeviceFaultAddressTypeEXT =
  [
    ( DEVICE_FAULT_ADDRESS_TYPE_NONE_EXT
    , "NONE_EXT"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_EXT
    , "READ_INVALID_EXT"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_EXT
    , "WRITE_INVALID_EXT"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_EXT
    , "EXECUTE_INVALID_EXT"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_EXT
    , "INSTRUCTION_POINTER_UNKNOWN_EXT"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_EXT
    , "INSTRUCTION_POINTER_INVALID_EXT"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_EXT
    , "INSTRUCTION_POINTER_FAULT_EXT"
    )
  ]

instance Show DeviceFaultAddressTypeEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixDeviceFaultAddressTypeEXT
      showTableDeviceFaultAddressTypeEXT
      conNameDeviceFaultAddressTypeEXT
      (\(DeviceFaultAddressTypeEXT x) -> x)
      (showsPrec 11)

instance Read DeviceFaultAddressTypeEXT where
  readPrec =
    enumReadPrec
      enumPrefixDeviceFaultAddressTypeEXT
      showTableDeviceFaultAddressTypeEXT
      conNameDeviceFaultAddressTypeEXT
      DeviceFaultAddressTypeEXT

-- | VkDeviceFaultVendorBinaryHeaderVersionEXT - Encode vendor binary crash
-- dump version
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'DeviceFaultVendorBinaryHeaderVersionOneEXT', 'getDeviceFaultInfoEXT'
newtype DeviceFaultVendorBinaryHeaderVersionEXT = DeviceFaultVendorBinaryHeaderVersionEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_EXT' specifies version
-- one of the binary crash dump header.
pattern DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_EXT = DeviceFaultVendorBinaryHeaderVersionEXT 1

{-# COMPLETE DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_EXT :: DeviceFaultVendorBinaryHeaderVersionEXT #-}

conNameDeviceFaultVendorBinaryHeaderVersionEXT :: String
conNameDeviceFaultVendorBinaryHeaderVersionEXT = "DeviceFaultVendorBinaryHeaderVersionEXT"

enumPrefixDeviceFaultVendorBinaryHeaderVersionEXT :: String
enumPrefixDeviceFaultVendorBinaryHeaderVersionEXT = "DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_EXT"

showTableDeviceFaultVendorBinaryHeaderVersionEXT :: [(DeviceFaultVendorBinaryHeaderVersionEXT, String)]
showTableDeviceFaultVendorBinaryHeaderVersionEXT =
  [
    ( DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_EXT
    , ""
    )
  ]

instance Show DeviceFaultVendorBinaryHeaderVersionEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixDeviceFaultVendorBinaryHeaderVersionEXT
      showTableDeviceFaultVendorBinaryHeaderVersionEXT
      conNameDeviceFaultVendorBinaryHeaderVersionEXT
      (\(DeviceFaultVendorBinaryHeaderVersionEXT x) -> x)
      (showsPrec 11)

instance Read DeviceFaultVendorBinaryHeaderVersionEXT where
  readPrec =
    enumReadPrec
      enumPrefixDeviceFaultVendorBinaryHeaderVersionEXT
      showTableDeviceFaultVendorBinaryHeaderVersionEXT
      conNameDeviceFaultVendorBinaryHeaderVersionEXT
      DeviceFaultVendorBinaryHeaderVersionEXT

type EXT_DEVICE_FAULT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEVICE_FAULT_SPEC_VERSION"
pattern EXT_DEVICE_FAULT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEVICE_FAULT_SPEC_VERSION = 1


type EXT_DEVICE_FAULT_EXTENSION_NAME = "VK_EXT_device_fault"

-- No documentation found for TopLevel "VK_EXT_DEVICE_FAULT_EXTENSION_NAME"
pattern EXT_DEVICE_FAULT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEVICE_FAULT_EXTENSION_NAME = "VK_EXT_device_fault"

