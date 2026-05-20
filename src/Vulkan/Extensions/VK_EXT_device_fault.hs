{-# language CPP #-}
-- | = Name
--
-- VK_EXT_device_fault - device extension
--
-- = VK_EXT_device_fault
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
--     2
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>
--         extension
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
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_device_fault.DeviceFaultAddressTypeKHR':
--
--     -   'DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_EXT'
--
--     -   'DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_EXT'
--
--     -   'DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_EXT'
--
--     -   'DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_EXT'
--
--     -   'DEVICE_FAULT_ADDRESS_TYPE_NONE_EXT'
--
--     -   'DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_EXT'
--
--     -   'DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_EXT'
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
-- -   Revision 2, 2023-04-05 (Ralph Potter)
--
--     -   Restored two missing members to the XML definition of
--         VkDeviceFaultVendorBinaryHeaderVersionOneEXT. No functional
--         change to the specification.
--
-- -   Revision 1, 2020-10-19 (Ralph Potter)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_device_fault Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_device_fault  ( getDeviceFaultInfoEXT
                                              , pattern DEVICE_FAULT_ADDRESS_TYPE_NONE_EXT
                                              , pattern DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_EXT
                                              , pattern DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_EXT
                                              , pattern DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_EXT
                                              , pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_EXT
                                              , pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_EXT
                                              , pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_EXT
                                              , PhysicalDeviceFaultFeaturesEXT(..)
                                              , DeviceFaultCountsEXT(..)
                                              , DeviceFaultInfoEXT(..)
                                              , DeviceFaultVendorBinaryHeaderVersionEXT
                                              , DeviceFaultAddressTypeEXT
                                              , DeviceFaultAddressInfoEXT
                                              , DeviceFaultVendorInfoEXT
                                              , DeviceFaultVendorBinaryHeaderVersionOneEXT
                                              , EXT_DEVICE_FAULT_SPEC_VERSION
                                              , pattern EXT_DEVICE_FAULT_SPEC_VERSION
                                              , EXT_DEVICE_FAULT_EXTENSION_NAME
                                              , pattern EXT_DEVICE_FAULT_EXTENSION_NAME
                                              , DeviceFaultAddressInfoKHR(..)
                                              , DeviceFaultVendorInfoKHR(..)
                                              , DeviceFaultVendorBinaryHeaderVersionOneKHR(..)
                                              , DeviceFaultAddressTypeKHR(..)
                                              , DeviceFaultVendorBinaryHeaderVersionKHR(..)
                                              ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceFaultInfoEXT))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressInfoKHR)
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressTypeKHR)
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultVendorBinaryHeaderVersionKHR)
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultVendorBinaryHeaderVersionOneKHR)
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultVendorInfoKHR)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressTypeKHR(DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_KHR))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressTypeKHR(DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_KHR))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressTypeKHR(DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_KHR))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressTypeKHR(DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_KHR))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressTypeKHR(DEVICE_FAULT_ADDRESS_TYPE_NONE_KHR))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressTypeKHR(DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_KHR))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressTypeKHR(DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_FAULT_COUNTS_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_FAULT_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressInfoKHR(..))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultAddressTypeKHR(..))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultVendorBinaryHeaderVersionKHR(..))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultVendorBinaryHeaderVersionOneKHR(..))
import Vulkan.Extensions.VK_KHR_device_fault (DeviceFaultVendorInfoKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceFaultInfoEXT
  :: FunPtr (Ptr Device_T -> Ptr DeviceFaultCountsEXT -> Ptr DeviceFaultInfoEXT -> IO Result) -> Ptr Device_T -> Ptr DeviceFaultCountsEXT -> Ptr DeviceFaultInfoEXT -> IO Result

-- | vkGetDeviceFaultInfoEXT - Reports fault information for the specified
-- device
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
-- by the application to the number of elements in the corresponding output
-- array member of @pFaultInfo@ (@pAddressInfos@ and @pVendorInfos@), or to
-- the size of the output buffer in bytes (@pVendorBinaryData@). On return,
-- each structure count member is overwritten with the number of structures
-- actually written to the corresponding output array member of
-- @pFaultInfo@. Similarly, @vendorBinarySize@ is overwritten with the
-- number of bytes actually written to the @pVendorBinaryData@ member of
-- @pFaultInfo@.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceFaultVendorBinaryEXT vendor-specific crash dumps>
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#vendor-binary-crash-dumps binary crash dump header>,
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
--     @pFaultInfo->pAddressInfos@ /must/ be a valid pointer to an array of
--     @pFaultCounts->addressInfoCount@ 'DeviceFaultAddressInfoEXT'
--     structures
--
-- -   #VUID-vkGetDeviceFaultInfoEXT-pFaultCounts-07338# If the value
--     referenced by @pFaultCounts->vendorInfoCount@ is not @0@, and
--     @pFaultInfo->pVendorInfos@ is not @NULL@, @pFaultInfo->pVendorInfos@
--     /must/ be a valid pointer to an array of
--     @pFaultCounts->vendorInfoCount@ 'DeviceFaultVendorInfoEXT'
--     structures
--
-- -   #VUID-vkGetDeviceFaultInfoEXT-pFaultCounts-07339# If the value
--     referenced by @pFaultCounts->vendorBinarySize@ is not @0@, and
--     @pFaultInfo->pVendorBinaryData@ is not @NULL@,
--     @pFaultInfo->pVendorBinaryData@ /must/ be a valid pointer to an
--     array of @pFaultCounts->vendorBinarySize@ bytes
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
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
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


-- No documentation found for TopLevel "VK_DEVICE_FAULT_ADDRESS_TYPE_NONE_EXT"
pattern DEVICE_FAULT_ADDRESS_TYPE_NONE_EXT = DEVICE_FAULT_ADDRESS_TYPE_NONE_KHR


-- No documentation found for TopLevel "VK_DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_EXT"
pattern DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_EXT = DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_KHR


-- No documentation found for TopLevel "VK_DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_EXT"
pattern DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_EXT = DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_KHR


-- No documentation found for TopLevel "VK_DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_EXT"
pattern DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_EXT = DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_KHR


-- No documentation found for TopLevel "VK_DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_EXT"
pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_EXT = DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_KHR


-- No documentation found for TopLevel "VK_DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_EXT"
pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_EXT = DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_KHR


-- No documentation found for TopLevel "VK_DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_EXT"
pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_EXT = DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_KHR


-- | VkPhysicalDeviceFaultFeaturesEXT - Structure indicating support for
-- device fault reporting
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceFaultFeaturesEXT' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceFaultFeaturesEXT', it /must/ add an instance of the
-- structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structures>]
--
--     -   'Vulkan.Core10.Device.DeviceCreateInfo'
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFaultFeaturesEXT = PhysicalDeviceFaultFeaturesEXT
  { -- | #features-deviceFaultEXT# @deviceFault@ indicates that the
    -- implementation supports the reporting of device fault information.
    deviceFault :: Bool
  , -- | #features-deviceFaultVendorBinaryEXT# @deviceFaultVendorBinary@
    -- indicates that the implementation supports the generation of
    -- vendor-specific binary crash dumps. These may provide additional
    -- information when imported into vendor-specific external tools.
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
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- 'Vulkan.Extensions.VK_KHR_device_fault.DeviceFaultAddressInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_device_fault.DeviceFaultVendorInfoKHR',
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
    -- pointers at the time of the fault. The length of @pAddressInfos@ is
    -- specified by the 'DeviceFaultCountsEXT'::@addressInfoCount@ value passed
    -- to 'getDeviceFaultInfoEXT'. If not @NULL@, each element of
    -- @pAddressInfos@ describes a bounded region of GPU virtual address space
    -- containing either the GPU virtual address accessed, or the value of an
    -- active instruction pointer.
    addressInfos :: Ptr DeviceFaultAddressInfoKHR
  , -- | @pVendorInfos@ is @NULL@ or a pointer to an array of
    -- 'DeviceFaultVendorInfoEXT' structures describing vendor-specific fault
    -- information. The length of @pVendorInfos@ is specified by the
    -- 'DeviceFaultCountsEXT'::@vendorInfoCount@ value passed to
    -- 'getDeviceFaultInfoEXT'.
    vendorInfos :: Ptr DeviceFaultVendorInfoKHR
  , -- | @pVendorBinaryData@ is @NULL@ or a pointer to memory which will be
    -- populated with a vendor-specific binary crash dump, as described in
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#vendor-binary-crash-dumps Vendor Binary Crash Dumps>.
    -- The size of @pVendorBinaryData@ is specified by the
    -- 'DeviceFaultCountsEXT'::@vendorBinarySize@ value passed to
    -- 'getDeviceFaultInfoEXT'.
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
    poke ((p `plusPtr` 272 :: Ptr (Ptr DeviceFaultAddressInfoKHR))) (addressInfos)
    poke ((p `plusPtr` 280 :: Ptr (Ptr DeviceFaultVendorInfoKHR))) (vendorInfos)
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
    pAddressInfos <- peek @(Ptr DeviceFaultAddressInfoKHR) ((p `plusPtr` 272 :: Ptr (Ptr DeviceFaultAddressInfoKHR)))
    pVendorInfos <- peek @(Ptr DeviceFaultVendorInfoKHR) ((p `plusPtr` 280 :: Ptr (Ptr DeviceFaultVendorInfoKHR)))
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


-- No documentation found for TopLevel "VkDeviceFaultVendorBinaryHeaderVersionEXT"
type DeviceFaultVendorBinaryHeaderVersionEXT = DeviceFaultVendorBinaryHeaderVersionKHR


-- No documentation found for TopLevel "VkDeviceFaultAddressTypeEXT"
type DeviceFaultAddressTypeEXT = DeviceFaultAddressTypeKHR


-- No documentation found for TopLevel "VkDeviceFaultAddressInfoEXT"
type DeviceFaultAddressInfoEXT = DeviceFaultAddressInfoKHR


-- No documentation found for TopLevel "VkDeviceFaultVendorInfoEXT"
type DeviceFaultVendorInfoEXT = DeviceFaultVendorInfoKHR


-- No documentation found for TopLevel "VkDeviceFaultVendorBinaryHeaderVersionOneEXT"
type DeviceFaultVendorBinaryHeaderVersionOneEXT = DeviceFaultVendorBinaryHeaderVersionOneKHR


type EXT_DEVICE_FAULT_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_DEVICE_FAULT_SPEC_VERSION"
pattern EXT_DEVICE_FAULT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEVICE_FAULT_SPEC_VERSION = 2


type EXT_DEVICE_FAULT_EXTENSION_NAME = "VK_EXT_device_fault"

-- No documentation found for TopLevel "VK_EXT_DEVICE_FAULT_EXTENSION_NAME"
pattern EXT_DEVICE_FAULT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEVICE_FAULT_EXTENSION_NAME = "VK_EXT_device_fault"

