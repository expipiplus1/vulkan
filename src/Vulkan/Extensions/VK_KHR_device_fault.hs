{-# language CPP #-}
-- | = Name
--
-- VK_KHR_device_fault - device extension
--
-- = VK_KHR_device_fault
--
-- [__Name String__]
--     @VK_KHR_device_fault@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     574
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
--     -   Ralph Potter <<data:image/png;base64, GitLab>>r_potter
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_device_fault.adoc VK_KHR_device_fault>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ralph Potter, Samsung
--
--     -   Craig Graham, Samsung
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
--     -   Adrian Ravai, Samsung
--
--     -   Peter Gal, Samsung
--
--     -   Matthew Netsch, QUALCOMM
--
--     -   Tobias Hector, AMD
--
--     -   Alan Harrison, AMD
--
--     -   Vikram Tarikere, IMG
--
--     -   Jon Leech, Khronos
--
--     -   Samuel Pitoiset, Valve
--
-- == Description
--
-- Device loss can be triggered by a variety of issues, including invalid
-- API usage, implementation errors, or hardware failures. This extension
-- introduces two new commands:
--
-- -   'getDeviceFaultReportsKHR'
--
-- -   'getDeviceFaultDebugInfoKHR'
--
-- 'getDeviceFaultReportsKHR' allows developers to query for additional
-- information on GPU faults which may have caused device loss, and to
-- generate binary crash dumps, which may be loaded into external tools for
-- further diagnosis. Additionally this command allows developers to query
-- for additional information on GPU faults which were internally recovered
-- by the implementation.
--
-- 'getDeviceFaultReportsKHR' differs from
-- 'Vulkan.Extensions.VK_EXT_device_fault.getDeviceFaultInfoEXT' in that it
-- can be called at any time, is able to report faults which do not result
-- in 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' and allows the caller
-- to wait for such an error to occur.
--
-- 'getDeviceFaultDebugInfoKHR' provides a separate interface which /must/
-- only be called when a device loss has occurred to provide extended GPU
-- vendor specific crash post-mortem information.
--
-- == New Commands
--
-- -   'getDeviceFaultDebugInfoKHR'
--
-- -   'getDeviceFaultReportsKHR'
--
-- == New Structures
--
-- -   'DeviceFaultAddressInfoKHR'
--
-- -   'DeviceFaultDebugInfoKHR'
--
-- -   'DeviceFaultInfoKHR'
--
-- -   'DeviceFaultVendorBinaryHeaderVersionOneKHR'
--
-- -   'DeviceFaultVendorInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFaultFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFaultPropertiesKHR'
--
-- == New Enums
--
-- -   'DeviceFaultAddressTypeKHR'
--
-- -   'DeviceFaultFlagBitsKHR'
--
-- -   'DeviceFaultVendorBinaryHeaderVersionKHR'
--
-- == New Bitmasks
--
-- -   'DeviceFaultFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DEVICE_FAULT_EXTENSION_NAME'
--
-- -   'KHR_DEVICE_FAULT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_DEBUG_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_PROPERTIES_KHR'
--
-- == Version History
--
-- -   Revision 0, 2024-03-01 (Ralph Potter)
--
--     -   Internal Revision
--
-- -   Revision 1, 2025-06-10 (Craig Graham)
--
--     -   Revised API to support async fault queries.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_device_fault Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_device_fault  ( getDeviceFaultReportsKHR
                                              , getDeviceFaultDebugInfoKHR
                                              , pattern DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_EXT
                                              , DeviceFaultAddressInfoKHR(..)
                                              , DeviceFaultVendorInfoKHR(..)
                                              , DeviceFaultInfoKHR(..)
                                              , DeviceFaultDebugInfoKHR(..)
                                              , DeviceFaultVendorBinaryHeaderVersionOneKHR(..)
                                              , PhysicalDeviceFaultFeaturesKHR(..)
                                              , PhysicalDeviceFaultPropertiesKHR(..)
                                              , DeviceFaultFlagsKHR
                                              , DeviceFaultFlagBitsKHR( DEVICE_FAULT_FLAG_DEVICE_LOST_KHR
                                                                      , DEVICE_FAULT_FLAG_MEMORY_ADDRESS_KHR
                                                                      , DEVICE_FAULT_FLAG_INSTRUCTION_ADDRESS_KHR
                                                                      , DEVICE_FAULT_FLAG_VENDOR_KHR
                                                                      , DEVICE_FAULT_FLAG_WATCHDOG_TIMEOUT_KHR
                                                                      , DEVICE_FAULT_FLAG_OVERFLOW_KHR
                                                                      , ..
                                                                      )
                                              , DeviceFaultAddressTypeKHR( DEVICE_FAULT_ADDRESS_TYPE_NONE_KHR
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_KHR
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_KHR
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_KHR
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_KHR
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_KHR
                                                                         , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_KHR
                                                                         , ..
                                                                         )
                                              , DeviceFaultVendorBinaryHeaderVersionKHR( DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_KHR
                                                                                       , ..
                                                                                       )
                                              , KHR_DEVICE_FAULT_SPEC_VERSION
                                              , pattern KHR_DEVICE_FAULT_SPEC_VERSION
                                              , KHR_DEVICE_FAULT_EXTENSION_NAME
                                              , pattern KHR_DEVICE_FAULT_EXTENSION_NAME
                                              ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Data.ByteString (packCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
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
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Utils (peekByteStringFromSizedVectorPtr)
import Vulkan.CStruct.Utils (pokeFixedLengthByteString)
import Vulkan.CStruct.Utils (pokeFixedLengthNullTerminatedByteString)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceFaultDebugInfoKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceFaultReportsKHR))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_abort (DeviceFaultShaderAbortMessageInfoKHR)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.APIConstants (MAX_DESCRIPTION_SIZE)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.APIConstants (UUID_SIZE)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_FAULT_DEBUG_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_FAULT_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_PROPERTIES_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceFaultReportsKHR
  :: FunPtr (Ptr Device_T -> Word64 -> Ptr Word32 -> Ptr DeviceFaultInfoKHR -> IO Result) -> Ptr Device_T -> Word64 -> Ptr Word32 -> Ptr DeviceFaultInfoKHR -> IO Result

-- | vkGetDeviceFaultReportsKHR - Reports fault information for the specified
-- device
--
-- = Description
--
-- If at least one fault report is available when
-- 'getDeviceFaultReportsKHR' is called, then 'getDeviceFaultReportsKHR'
-- returns immediately. If no faults have occurred at the time
-- 'getDeviceFaultReportsKHR' is called, then 'getDeviceFaultReportsKHR'
-- will block and wait until a fault occurs and a report becomes available
-- or until the @timeout@ has expired, whichever is sooner.
--
-- If @timeout@ is zero, then 'getDeviceFaultReportsKHR' does not wait, but
-- simply returns the current fault count and reports.
-- 'Vulkan.Core10.Enums.Result.TIMEOUT' will be returned in this case if no
-- faults have occurred, even though no actual wait was performed.
--
-- If a fault has occurred before the @timeout@ has expired,
-- 'getDeviceFaultReportsKHR' returns 'Vulkan.Core10.Enums.Result.SUCCESS'.
-- Otherwise, 'getDeviceFaultReportsKHR' returns
-- 'Vulkan.Core10.Enums.Result.TIMEOUT' after the @timeout@ has expired.
--
-- If @pFaultInfo@ is @NULL@, then the number of fault reports available is
-- returned in @pFaultCount@. Otherwise, @pFaultCount@ /must/ point to a
-- variable set by the application to the number of elements in the
-- @pFaultInfo@ array, and on return the variable is overwritten with the
-- number of handles actually written to @pFaultInfo@. If @pFaultCount@ is
-- less than the number of fault reports available, at most @pFaultCount@
-- structures will be written, and 'Vulkan.Core10.Enums.Result.INCOMPLETE'
-- will be returned instead of 'Vulkan.Core10.Enums.Result.SUCCESS', to
-- indicate that not all the available fault reports were returned.
--
-- If more than one fault report is available, they will be returned in
-- order of occurrence via @pFaultInfo@.
--
-- Each individual fault report is returned exactly once.
--
-- 'getDeviceFaultReportsKHR'() /may/ be invoked in parallel on different
-- threads, in which case each invocation for a given @device@ will return
-- a unique set of reports. No fault report being returned to more than one
-- invocation.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetDeviceFaultReportsKHR-device-parameter# @device@ /must/
--     be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetDeviceFaultReportsKHR-pFaultCounts-parameter#
--     @pFaultCounts@ /must/ be a valid pointer to a @uint32_t@ value
--
-- -   #VUID-vkGetDeviceFaultReportsKHR-pFaultInfo-parameter# If
--     @pFaultInfo@ is not @NULL@, @pFaultInfo@ /must/ be a valid pointer
--     to an array of @pFaultCounts@ 'DeviceFaultInfoKHR' structures
--
-- -   #VUID-vkGetDeviceFaultReportsKHR-pFaultCounts-arraylength# If
--     @pFaultInfo@ is not @NULL@, the value referenced by @pFaultCounts@
--     /must/ be greater than @0@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceFaultInfoKHR'
getDeviceFaultReportsKHR :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device from which to query the diagnostic fault
                            -- information.
                            Device
                         -> -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
                            -- adjusted to the closest value allowed by the implementation-dependent
                            -- timeout accuracy, which /may/ be substantially longer than one
                            -- nanosecond, and /may/ be longer than the requested period.
                            ("timeout" ::: Word64)
                         -> io (Result, ("faultInfo" ::: Vector DeviceFaultInfoKHR))
getDeviceFaultReportsKHR device timeout = liftIO . evalContT $ do
  let vkGetDeviceFaultReportsKHRPtr = pVkGetDeviceFaultReportsKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceFaultReportsKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceFaultReportsKHR is null" Nothing Nothing
  let vkGetDeviceFaultReportsKHR' = mkVkGetDeviceFaultReportsKHR vkGetDeviceFaultReportsKHRPtr
  let device' = deviceHandle (device)
  pPFaultCounts <- ContT $ bracket (callocBytes @Word32 4) free
  r <- lift $ traceAroundEvent "vkGetDeviceFaultReportsKHR" (vkGetDeviceFaultReportsKHR'
                                                               device'
                                                               (timeout)
                                                               (pPFaultCounts)
                                                               (nullPtr))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFaultCounts <- lift $ peek @Word32 pPFaultCounts
  pPFaultInfo <- ContT $ bracket (callocBytes @DeviceFaultInfoKHR ((fromIntegral (pFaultCounts)) * 608)) free
  _ <- traverse (\i -> ContT $ pokeZeroCStruct (pPFaultInfo `advancePtrBytes` (i * 608) :: Ptr DeviceFaultInfoKHR) . ($ ())) [0..(fromIntegral (pFaultCounts)) - 1]
  r' <- lift $ traceAroundEvent "vkGetDeviceFaultReportsKHR" (vkGetDeviceFaultReportsKHR'
                                                                device'
                                                                (timeout)
                                                                (pPFaultCounts)
                                                                ((pPFaultInfo)))
  lift $ when (r' < SUCCESS) (throwIO (VulkanException r'))
  pFaultCounts' <- lift $ peek @Word32 pPFaultCounts
  pFaultInfo' <- lift $ generateM (fromIntegral (pFaultCounts')) (\i -> peekCStruct @DeviceFaultInfoKHR (((pPFaultInfo) `advancePtrBytes` (608 * (i)) :: Ptr DeviceFaultInfoKHR)))
  pure $ ((r'), pFaultInfo')


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceFaultDebugInfoKHR
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct DeviceFaultDebugInfoKHR) -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct DeviceFaultDebugInfoKHR) -> IO Result

-- | vkGetDeviceFaultDebugInfoKHR - Retrieve vendor-specific crash dump data
-- for the specified logical device
--
-- = Description
--
-- If @pDebugInfo->pVendorBinaryData@ is @NULL@, then the size of any
-- vendor-specific binary crash dump is returned in
-- @pDebugInfo->vendorBinarySize@.
--
-- If the @pDebugInfo->pVendorBinaryData@ is not @NULL@ then it /must/
-- point to a buffer of size @pDebugInfo->vendorBinarySize@ bytes.
--
-- On return, @pDebugInfo->vendorBinarySize@ will be overwritten with the
-- number of bytes actually written to the @pDebugInfo->pVendorBinaryData@.
--
-- If @pDebugInfo->vendorBinaryData@ is not @NULL@, then subsequent calls
-- to 'getDeviceFaultDebugInfoKHR' for the same @device@ /must/ return
-- identical binary values in the @pDebugInfo->pVendorBinaryData@ buffer up
-- to the limit defined by @pDebugInfo->vendorBinarySize@.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceFaultVendorBinary vendor-specific crash dumps>
-- feature is not enabled, then implementations /must/ set
-- @pDebugInfo@->vendorBinarySize to zero and /must/ not modify
-- @pDebugInfo@->pVendorBinaryData.
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
--     -   'Vulkan.Core10.Enums.Result.ERROR_NOT_ENOUGH_SPACE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceFaultDebugInfoKHR'
getDeviceFaultDebugInfoKHR :: forall a io
                            . ( Extendss DeviceFaultDebugInfoKHR a
                              , PokeChain a
                              , PeekChain a
                              , MonadIO io )
                           => -- | @device@ is the logical device from which to query the crash dump.
                              --
                              -- #VUID-vkGetDeviceFaultDebugInfoKHR-device-12383# @device@ /must/ be
                              -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#devsandqueues-lost-device lost>
                              --
                              -- #VUID-vkGetDeviceFaultDebugInfoKHR-device-parameter# @device@ /must/ be
                              -- a valid 'Vulkan.Core10.Handles.Device' handle
                              Device
                           -> io (Result, DeviceFaultDebugInfoKHR a)
getDeviceFaultDebugInfoKHR device = liftIO . evalContT $ do
  let vkGetDeviceFaultDebugInfoKHRPtr = pVkGetDeviceFaultDebugInfoKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceFaultDebugInfoKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceFaultDebugInfoKHR is null" Nothing Nothing
  let vkGetDeviceFaultDebugInfoKHR' = mkVkGetDeviceFaultDebugInfoKHR vkGetDeviceFaultDebugInfoKHRPtr
  pPDebugInfo <- ContT (withZeroCStruct @(DeviceFaultDebugInfoKHR _))
  r <- lift $ traceAroundEvent "vkGetDeviceFaultDebugInfoKHR" (vkGetDeviceFaultDebugInfoKHR'
                                                                 (deviceHandle (device))
                                                                 (forgetExtensions (pPDebugInfo)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDebugInfo <- lift $ peekCStruct @(DeviceFaultDebugInfoKHR _) pPDebugInfo
  pure $ (r, pDebugInfo)


-- No documentation found for TopLevel "VK_DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_EXT"
pattern DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_EXT = DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_KHR


-- | VkDeviceFaultAddressInfoKHR - Structure specifying GPU virtual address
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
-- It is valid for the @reportedAddress@ to contain a more precise address
-- than indicated by @addressPrecision@. In this case, the value of
-- @reportedAddress@ should be treated as an additional hint as to the
-- value of the address that triggered the page fault, or to the value of
-- an instruction pointer.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'DeviceFaultAddressTypeKHR',
-- 'Vulkan.Extensions.VK_EXT_device_fault.DeviceFaultInfoEXT',
-- 'DeviceFaultInfoKHR', 'Vulkan.Core10.FundamentalTypes.DeviceSize'
data DeviceFaultAddressInfoKHR = DeviceFaultAddressInfoKHR
  { -- | @addressType@ is either the type of memory operation that triggered a
    -- page fault, or the type of association between an instruction pointer
    -- and a fault.
    addressType :: DeviceFaultAddressTypeKHR
  , -- | @reportedAddress@ is the GPU virtual address recorded by the device.
    reportedAddress :: DeviceAddress
  , -- | @addressPrecision@ is a power of two value that specifies how precisely
    -- the device can report the address.
    addressPrecision :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceFaultAddressInfoKHR)
#endif
deriving instance Show DeviceFaultAddressInfoKHR

instance ToCStruct DeviceFaultAddressInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultAddressInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceFaultAddressTypeKHR)) (addressType)
    poke ((p `plusPtr` 8 :: Ptr DeviceAddress)) (reportedAddress)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (addressPrecision)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceFaultAddressTypeKHR)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceAddress)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct DeviceFaultAddressInfoKHR where
  peekCStruct p = do
    addressType <- peek @DeviceFaultAddressTypeKHR ((p `plusPtr` 0 :: Ptr DeviceFaultAddressTypeKHR))
    reportedAddress <- peek @DeviceAddress ((p `plusPtr` 8 :: Ptr DeviceAddress))
    addressPrecision <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ DeviceFaultAddressInfoKHR
             addressType reportedAddress addressPrecision

instance Storable DeviceFaultAddressInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceFaultAddressInfoKHR where
  zero = DeviceFaultAddressInfoKHR
           zero
           zero
           zero


-- | VkDeviceFaultVendorInfoKHR - Structure specifying vendor-specific fault
-- information
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'Vulkan.Extensions.VK_EXT_device_fault.DeviceFaultInfoEXT',
-- 'DeviceFaultInfoKHR'
data DeviceFaultVendorInfoKHR = DeviceFaultVendorInfoKHR
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
deriving instance Generic (DeviceFaultVendorInfoKHR)
#endif
deriving instance Show DeviceFaultVendorInfoKHR

instance ToCStruct DeviceFaultVendorInfoKHR where
  withCStruct x f = allocaBytes 272 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultVendorInfoKHR{..} f = do
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

instance FromCStruct DeviceFaultVendorInfoKHR where
  peekCStruct p = do
    description <- packCString (lowerArrayPtr ((p `plusPtr` 0 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    vendorFaultCode <- peek @Word64 ((p `plusPtr` 256 :: Ptr Word64))
    vendorFaultData <- peek @Word64 ((p `plusPtr` 264 :: Ptr Word64))
    pure $ DeviceFaultVendorInfoKHR
             description vendorFaultCode vendorFaultData

instance Storable DeviceFaultVendorInfoKHR where
  sizeOf ~_ = 272
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceFaultVendorInfoKHR where
  zero = DeviceFaultVendorInfoKHR
           mempty
           zero
           zero


-- | VkDeviceFaultInfoKHR - Structure specifying device fault information
--
-- = Description
--
-- An implementation /should/ populate as many members of
-- 'DeviceFaultInfoKHR' as possible, given the information available at the
-- time of the fault and the constraints of the implementation itself.
--
-- A single error /may/ generate multiple instances of
-- 'DeviceFaultInfoKHR'. For example, multiple page faults /may/ be
-- generated, with the @faultAddress@ member of each instance identifying a
-- different memory address. In this case, all instances of
-- 'DeviceFaultInfoKHR' /should/ share the same @groupID@ where the
-- implementation can reasonably ascertain this association.
--
-- Due to hardware limitations, @faultAddressInfo@ and
-- @instructionAddressInfo@ describe ranges of GPU virtual address space,
-- rather than precise addresses. The precise memory address accessed or
-- the precise value of the instruction pointer /must/ lie within the
-- region described.
--
-- Each 'DeviceFaultInfoKHR' reported may (depending on the @flags@ set)
-- provide:
--
-- -   A memory access which may have triggered a page fault and may have
--     contributed to device loss
--
-- -   The value of an active instruction pointer at the time a fault
--     occurred. This value may be indicative of the active pipeline or
--     shader at the time of device loss
--
-- Comparison of the GPU virtual addresses described by @faultAddressInfo@
-- or @instructionAddressInfo@ to GPU virtual address ranges reported by
-- the @VK_EXT_device_address_binding_report@ extension may allow
-- applications to correlate between these addresses and Vulkan objects.
-- Applications should be aware that these addresses may also correspond to
-- resources internal to an implementation, which will not be reported via
-- the @VK_EXT_device_address_binding_report@ extension.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'DeviceFaultAddressInfoKHR', 'DeviceFaultFlagsKHR',
-- 'DeviceFaultVendorInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceFaultReportsKHR'
data DeviceFaultInfoKHR = DeviceFaultInfoKHR
  { -- | @flags@ is a bitmask of 'DeviceFaultFlagBitsKHR' values giving
    -- information as to the type of fault and which additional fields have
    -- been populated by the driver provide further information.
    flags :: DeviceFaultFlagsKHR
  , -- No documentation found for Nested "VkDeviceFaultInfoKHR" "groupId"
    groupId :: Word64
  , -- | @description@ is an array of
    -- 'Vulkan.Core10.APIConstants.MAX_DESCRIPTION_SIZE' @char@ containing a
    -- null-terminated UTF-8 string which is a human readable description of
    -- the fault.
    description :: ByteString
  , -- | @faultAddressInfo@ is a 'DeviceFaultAddressInfoKHR' structure providing
    -- details of the memory access which caused a fault.
    faultAddressInfo :: DeviceFaultAddressInfoKHR
  , -- | @instructionAddressInfo@ is a 'DeviceFaultAddressInfoKHR' structure
    -- providing details of the GPU instruction which cause a fault.
    instructionAddressInfo :: DeviceFaultAddressInfoKHR
  , -- | @vendorInfo@ is a 'DeviceFaultVendorInfoKHR' structure providing vendor
    -- specific fault information.
    vendorInfo :: DeviceFaultVendorInfoKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceFaultInfoKHR)
#endif
deriving instance Show DeviceFaultInfoKHR

instance ToCStruct DeviceFaultInfoKHR where
  withCStruct x f = allocaBytes 608 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_FAULT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceFaultFlagsKHR)) (flags)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (groupId)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 32 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (description)
    poke ((p `plusPtr` 288 :: Ptr DeviceFaultAddressInfoKHR)) (faultAddressInfo)
    poke ((p `plusPtr` 312 :: Ptr DeviceFaultAddressInfoKHR)) (instructionAddressInfo)
    poke ((p `plusPtr` 336 :: Ptr DeviceFaultVendorInfoKHR)) (vendorInfo)
    f
  cStructSize = 608
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_FAULT_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceFaultFlagsKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    pokeFixedLengthNullTerminatedByteString ((p `plusPtr` 32 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))) (mempty)
    f

instance FromCStruct DeviceFaultInfoKHR where
  peekCStruct p = do
    flags <- peek @DeviceFaultFlagsKHR ((p `plusPtr` 16 :: Ptr DeviceFaultFlagsKHR))
    groupId <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    description <- packCString (lowerArrayPtr ((p `plusPtr` 32 :: Ptr (FixedArray MAX_DESCRIPTION_SIZE CChar))))
    faultAddressInfo <- peekCStruct @DeviceFaultAddressInfoKHR ((p `plusPtr` 288 :: Ptr DeviceFaultAddressInfoKHR))
    instructionAddressInfo <- peekCStruct @DeviceFaultAddressInfoKHR ((p `plusPtr` 312 :: Ptr DeviceFaultAddressInfoKHR))
    vendorInfo <- peekCStruct @DeviceFaultVendorInfoKHR ((p `plusPtr` 336 :: Ptr DeviceFaultVendorInfoKHR))
    pure $ DeviceFaultInfoKHR
             flags
             groupId
             description
             faultAddressInfo
             instructionAddressInfo
             vendorInfo

instance Storable DeviceFaultInfoKHR where
  sizeOf ~_ = 608
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceFaultInfoKHR where
  zero = DeviceFaultInfoKHR
           zero
           zero
           mempty
           zero
           zero
           zero


-- | VkDeviceFaultDebugInfoKHR - Structure allowing retrieval of the vendor
-- binary crash dump
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceFaultDebugInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_DEBUG_INFO_KHR'
--
-- -   #VUID-VkDeviceFaultDebugInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_KHR_shader_abort.DeviceFaultShaderAbortMessageInfoKHR'
--
-- -   #VUID-VkDeviceFaultDebugInfoKHR-sType-unique# The @sType@ value of
--     each structure in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceFaultDebugInfoKHR'
data DeviceFaultDebugInfoKHR (es :: [Type]) = DeviceFaultDebugInfoKHR
  { -- No documentation found for Nested "VkDeviceFaultDebugInfoKHR" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkDeviceFaultDebugInfoKHR" "vendorBinarySize"
    vendorBinarySize :: Word32
  , -- No documentation found for Nested "VkDeviceFaultDebugInfoKHR" "pVendorBinaryData"
    vendorBinaryData :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceFaultDebugInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DeviceFaultDebugInfoKHR es)

instance Extensible DeviceFaultDebugInfoKHR where
  extensibleTypeName = "DeviceFaultDebugInfoKHR"
  setNext DeviceFaultDebugInfoKHR{..} next' = DeviceFaultDebugInfoKHR{next = next', ..}
  getNext DeviceFaultDebugInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DeviceFaultDebugInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeviceFaultShaderAbortMessageInfoKHR = Just f
    | otherwise = Nothing

instance ( Extendss DeviceFaultDebugInfoKHR es
         , PokeChain es ) => ToCStruct (DeviceFaultDebugInfoKHR es) where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultDebugInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_FAULT_DEBUG_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (vendorBinarySize)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (vendorBinaryData)
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_FAULT_DEBUG_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss DeviceFaultDebugInfoKHR es
         , PeekChain es ) => FromCStruct (DeviceFaultDebugInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    vendorBinarySize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pVendorBinaryData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ DeviceFaultDebugInfoKHR
             next vendorBinarySize pVendorBinaryData

instance es ~ '[] => Zero (DeviceFaultDebugInfoKHR es) where
  zero = DeviceFaultDebugInfoKHR
           ()
           zero
           zero


-- | VkDeviceFaultVendorBinaryHeaderVersionOneKHR - Structure describing the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'DeviceFaultVendorBinaryHeaderVersionKHR'
data DeviceFaultVendorBinaryHeaderVersionOneKHR = DeviceFaultVendorBinaryHeaderVersionOneKHR
  { -- | @headerSize@ is the length in bytes of the crash dump header.
    --
    -- #VUID-VkDeviceFaultVendorBinaryHeaderVersionOneEXT-headerSize-07340#
    -- @headerSize@ /must/ be 56
    headerSize :: Word32
  , -- | @headerVersion@ is a 'DeviceFaultVendorBinaryHeaderVersionKHR' enum
    -- value specifying the version of the header. A consumer of the crash dump
    -- /should/ use the header version to interpret the remainder of the
    -- header. @headerVersion@ /must/ be written as exactly 4 bytes.
    --
    -- #VUID-VkDeviceFaultVendorBinaryHeaderVersionOneEXT-headerVersion-07341#
    -- @headerVersion@ /must/ be
    -- 'DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_KHR'
    --
    -- #VUID-VkDeviceFaultVendorBinaryHeaderVersionOneKHR-headerVersion-parameter#
    -- @headerVersion@ /must/ be a valid
    -- 'DeviceFaultVendorBinaryHeaderVersionKHR' value
    headerVersion :: DeviceFaultVendorBinaryHeaderVersionKHR
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
  , -- | @engineVersion@ /must/ be zero or the value specified by
    -- 'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@engineVersion@
    -- during instance creation.
    engineVersion :: Word32
  , -- | @apiVersion@ /must/ be zero or the value specified by
    -- 'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@
    -- during instance creation.
    apiVersion :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceFaultVendorBinaryHeaderVersionOneKHR)
#endif
deriving instance Show DeviceFaultVendorBinaryHeaderVersionOneKHR

instance ToCStruct DeviceFaultVendorBinaryHeaderVersionOneKHR where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceFaultVendorBinaryHeaderVersionOneKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (headerSize)
    poke ((p `plusPtr` 4 :: Ptr DeviceFaultVendorBinaryHeaderVersionKHR)) (headerVersion)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (vendorID)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (deviceID)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (driverVersion)
    pokeFixedLengthByteString ((p `plusPtr` 20 :: Ptr (FixedArray UUID_SIZE Word8))) (pipelineCacheUUID)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (applicationNameOffset)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (applicationVersion)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (engineNameOffset)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (engineVersion)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (apiVersion)
    f
  cStructSize = 56
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr DeviceFaultVendorBinaryHeaderVersionKHR)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    pokeFixedLengthByteString ((p `plusPtr` 20 :: Ptr (FixedArray UUID_SIZE Word8))) (mempty)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 52 :: Ptr Word32)) (zero)
    f

instance FromCStruct DeviceFaultVendorBinaryHeaderVersionOneKHR where
  peekCStruct p = do
    headerSize <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    headerVersion <- peek @DeviceFaultVendorBinaryHeaderVersionKHR ((p `plusPtr` 4 :: Ptr DeviceFaultVendorBinaryHeaderVersionKHR))
    vendorID <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    deviceID <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    driverVersion <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pipelineCacheUUID <- peekByteStringFromSizedVectorPtr ((p `plusPtr` 20 :: Ptr (FixedArray UUID_SIZE Word8)))
    applicationNameOffset <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    applicationVersion <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    engineNameOffset <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    engineVersion <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    apiVersion <- peek @Word32 ((p `plusPtr` 52 :: Ptr Word32))
    pure $ DeviceFaultVendorBinaryHeaderVersionOneKHR
             headerSize
             headerVersion
             vendorID
             deviceID
             driverVersion
             pipelineCacheUUID
             applicationNameOffset
             applicationVersion
             engineNameOffset
             engineVersion
             apiVersion

instance Storable DeviceFaultVendorBinaryHeaderVersionOneKHR where
  sizeOf ~_ = 56
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceFaultVendorBinaryHeaderVersionOneKHR where
  zero = DeviceFaultVendorBinaryHeaderVersionOneKHR
           zero
           zero
           zero
           zero
           zero
           mempty
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceFaultFeaturesKHR - Structure indicating support for
-- device fault reporting
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- In exceptional circumstances, some implementations /may/ mask faults and
-- attempt to recover from an error. In such circumstances, the device is
-- not lost and further work can be submitted to the device. When such
-- faults occur, the contents of all resources being written to at the time
-- of the fault are undefined.
--
-- If the 'PhysicalDeviceFaultFeaturesKHR' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceFaultFeaturesKHR', it /must/ add an instance of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFaultFeaturesKHR = PhysicalDeviceFaultFeaturesKHR
  { -- | #features-deviceFault# @deviceFault@ indicates that the implementation
    -- supports the reporting of device fault information.
    deviceFault :: Bool
  , -- | #features-deviceFaultVendorBinary# @deviceFaultVendorBinary@ indicates
    -- that the implementation supports the generation of vendor-specific
    -- binary crash dumps. These /may/ provide additional information when
    -- imported into vendor-specific external tools.
    deviceFaultVendorBinary :: Bool
  , -- | #features-deviceFaultReportMasked# @deviceFaultReportMasked@ indicates
    -- that the implementation supports masked faults in normal operation (ie.
    -- automatically recovered by the driver internally without the application
    -- receiving a 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' error) which
    -- /may/ be reported via this extension even if they did not result in a
    -- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' condition being returned
    -- to the application.
    deviceFaultReportMasked :: Bool
  , -- | #features-deviceFaultDeviceLostOnMasked# @deviceFaultDeviceLostOnMasked@
    -- indicates that the implementation supports returning
    -- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' for faults that would be
    -- normally be masked.
    deviceFaultDeviceLostOnMasked :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFaultFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceFaultFeaturesKHR

instance ToCStruct PhysicalDeviceFaultFeaturesKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFaultFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (deviceFault))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (deviceFaultVendorBinary))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (deviceFaultReportMasked))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (deviceFaultDeviceLostOnMasked))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFaultFeaturesKHR where
  peekCStruct p = do
    deviceFault <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    deviceFaultVendorBinary <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    deviceFaultReportMasked <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    deviceFaultDeviceLostOnMasked <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    pure $ PhysicalDeviceFaultFeaturesKHR
             (bool32ToBool deviceFault)
             (bool32ToBool deviceFaultVendorBinary)
             (bool32ToBool deviceFaultReportMasked)
             (bool32ToBool deviceFaultDeviceLostOnMasked)

instance Storable PhysicalDeviceFaultFeaturesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFaultFeaturesKHR where
  zero = PhysicalDeviceFaultFeaturesKHR
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceFaultPropertiesKHR - Structure describing fault
-- reporting properties for the physical device
--
-- = Members
--
-- The members of the 'PhysicalDeviceFaultPropertiesKHR' structure describe
-- the following:
--
-- = Description
--
-- If the 'PhysicalDeviceFaultPropertiesKHR' structure is included in the
-- @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Structure Chaining
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-validusage-pNext Extends the structure>]
--
--     -   'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceFaultPropertiesKHR = PhysicalDeviceFaultPropertiesKHR
  { -- | #limits-maxDeviceFaultCount# @maxDeviceFaultCount@ is the maximum number
    -- of instances of 'DeviceFaultInfoKHR' that will be retained by the
    -- implementation. This /must/ be greater than or equal to 1. If the
    -- application does not retrieve fault reports and overflow occurs, the
    -- oldest fault reports will be overwritten by the most recent record.
    maxDeviceFaultCount :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFaultPropertiesKHR)
#endif
deriving instance Show PhysicalDeviceFaultPropertiesKHR

instance ToCStruct PhysicalDeviceFaultPropertiesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFaultPropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxDeviceFaultCount)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceFaultPropertiesKHR where
  peekCStruct p = do
    maxDeviceFaultCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PhysicalDeviceFaultPropertiesKHR
             maxDeviceFaultCount

instance Storable PhysicalDeviceFaultPropertiesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFaultPropertiesKHR where
  zero = PhysicalDeviceFaultPropertiesKHR
           zero


type DeviceFaultFlagsKHR = DeviceFaultFlagBitsKHR

-- | VkDeviceFaultFlagBitsKHR - Bits which may be set in a
-- VkDeviceFaultFlagsKHR bitmask
--
-- = Description
--
-- -   'DEVICE_FAULT_FLAG_DEVICE_LOST_KHR' specifies that the fault has
--     resulted in a device lost condition. No subsequent entries will be
--     returned for this device.
--
-- -   'DEVICE_FAULT_FLAG_MEMORY_ADDRESS_KHR' specifies that the fault has
--     associated memory access address information which is stored in the
--     faultAddressInfo field (see 'DeviceFaultAddressInfoKHR').
--
-- -   'DEVICE_FAULT_FLAG_INSTRUCTION_ADDRESS_KHR' specifies that the fault
--     has an associated instruction address which is stored in the
--     instructionAddressInfo field (see 'DeviceFaultAddressInfoKHR').
--
-- -   'DEVICE_FAULT_FLAG_VENDOR_KHR' specifies that the fault has
--     associated vendor information stored in the vendorInfo field (see
--     'DeviceFaultVendorInfoKHR').
--
-- -   'DEVICE_FAULT_FLAG_WATCHDOG_TIMEOUT_KHR' specifies that the fault
--     was the result of a GPU timeout. Further information /may/ be made
--     available using other platform specific extensions via the pNext
--     chain of the 'DeviceFaultInfoKHR' structure.
--
-- -   'DEVICE_FAULT_FLAG_OVERFLOW_KHR' specifies that prior faults have
--     occurred, but information about them is no longer available. This
--     typically indicates that faults are occurring more rapidly than the
--     calling application is able to read them back.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'DeviceFaultFlagsKHR'
newtype DeviceFaultFlagBitsKHR = DeviceFaultFlagBitsKHR Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkDeviceFaultFlagBitsKHR" "VK_DEVICE_FAULT_FLAG_DEVICE_LOST_KHR"
pattern DEVICE_FAULT_FLAG_DEVICE_LOST_KHR = DeviceFaultFlagBitsKHR 0x00000001

-- No documentation found for Nested "VkDeviceFaultFlagBitsKHR" "VK_DEVICE_FAULT_FLAG_MEMORY_ADDRESS_KHR"
pattern DEVICE_FAULT_FLAG_MEMORY_ADDRESS_KHR = DeviceFaultFlagBitsKHR 0x00000002

-- No documentation found for Nested "VkDeviceFaultFlagBitsKHR" "VK_DEVICE_FAULT_FLAG_INSTRUCTION_ADDRESS_KHR"
pattern DEVICE_FAULT_FLAG_INSTRUCTION_ADDRESS_KHR = DeviceFaultFlagBitsKHR 0x00000004

-- No documentation found for Nested "VkDeviceFaultFlagBitsKHR" "VK_DEVICE_FAULT_FLAG_VENDOR_KHR"
pattern DEVICE_FAULT_FLAG_VENDOR_KHR = DeviceFaultFlagBitsKHR 0x00000008

-- No documentation found for Nested "VkDeviceFaultFlagBitsKHR" "VK_DEVICE_FAULT_FLAG_WATCHDOG_TIMEOUT_KHR"
pattern DEVICE_FAULT_FLAG_WATCHDOG_TIMEOUT_KHR = DeviceFaultFlagBitsKHR 0x00000010

-- No documentation found for Nested "VkDeviceFaultFlagBitsKHR" "VK_DEVICE_FAULT_FLAG_OVERFLOW_KHR"
pattern DEVICE_FAULT_FLAG_OVERFLOW_KHR = DeviceFaultFlagBitsKHR 0x00000020

conNameDeviceFaultFlagBitsKHR :: String
conNameDeviceFaultFlagBitsKHR = "DeviceFaultFlagBitsKHR"

enumPrefixDeviceFaultFlagBitsKHR :: String
enumPrefixDeviceFaultFlagBitsKHR = "DEVICE_FAULT_FLAG_"

showTableDeviceFaultFlagBitsKHR :: [(DeviceFaultFlagBitsKHR, String)]
showTableDeviceFaultFlagBitsKHR =
  [
    ( DEVICE_FAULT_FLAG_DEVICE_LOST_KHR
    , "DEVICE_LOST_KHR"
    )
  ,
    ( DEVICE_FAULT_FLAG_MEMORY_ADDRESS_KHR
    , "MEMORY_ADDRESS_KHR"
    )
  ,
    ( DEVICE_FAULT_FLAG_INSTRUCTION_ADDRESS_KHR
    , "INSTRUCTION_ADDRESS_KHR"
    )
  , (DEVICE_FAULT_FLAG_VENDOR_KHR, "VENDOR_KHR")
  ,
    ( DEVICE_FAULT_FLAG_WATCHDOG_TIMEOUT_KHR
    , "WATCHDOG_TIMEOUT_KHR"
    )
  ,
    ( DEVICE_FAULT_FLAG_OVERFLOW_KHR
    , "OVERFLOW_KHR"
    )
  ]

instance Show DeviceFaultFlagBitsKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixDeviceFaultFlagBitsKHR
      showTableDeviceFaultFlagBitsKHR
      conNameDeviceFaultFlagBitsKHR
      (\(DeviceFaultFlagBitsKHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read DeviceFaultFlagBitsKHR where
  readPrec =
    enumReadPrec
      enumPrefixDeviceFaultFlagBitsKHR
      showTableDeviceFaultFlagBitsKHR
      conNameDeviceFaultFlagBitsKHR
      DeviceFaultFlagBitsKHR

-- | VkDeviceFaultAddressTypeKHR - Page fault access types
--
-- = Description
--
-- -   'DEVICE_FAULT_ADDRESS_TYPE_NONE_KHR' specifies that
--     'DeviceFaultAddressInfoKHR' does not describe a page fault, or an
--     instruction address.
--
-- -   'DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_KHR' specifies that
--     'DeviceFaultAddressInfoKHR' describes a page fault triggered by an
--     invalid read operation.
--
-- -   'DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_KHR' specifies that
--     'DeviceFaultAddressInfoKHR' describes a page fault triggered by an
--     invalid write operation.
--
-- -   'DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_KHR' specifies that
--     'DeviceFaultAddressInfoKHR' describes a page fault triggered by an
--     attempt to execute non-executable memory.
--
-- -   'DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_KHR'
--     specifies an instruction pointer value at the time the fault
--     occurred. This may or may not be related to a fault.
--
-- -   'DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_KHR'
--     specifies an instruction pointer value associated with an invalid
--     instruction fault.
--
-- -   'DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_KHR' specifies
--     an instruction pointer value associated with a fault.
--
-- The instruction pointer values recorded may not identify the specific
-- instruction(s) that triggered the fault. The relationship between the
-- instruction pointer reported and triggering instruction will be
-- vendor-specific.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'DeviceFaultAddressInfoKHR'
newtype DeviceFaultAddressTypeKHR = DeviceFaultAddressTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDeviceFaultAddressTypeKHR" "VK_DEVICE_FAULT_ADDRESS_TYPE_NONE_KHR"
pattern DEVICE_FAULT_ADDRESS_TYPE_NONE_KHR = DeviceFaultAddressTypeKHR 0

-- No documentation found for Nested "VkDeviceFaultAddressTypeKHR" "VK_DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_KHR"
pattern DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_KHR = DeviceFaultAddressTypeKHR 1

-- No documentation found for Nested "VkDeviceFaultAddressTypeKHR" "VK_DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_KHR"
pattern DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_KHR = DeviceFaultAddressTypeKHR 2

-- No documentation found for Nested "VkDeviceFaultAddressTypeKHR" "VK_DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_KHR"
pattern DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_KHR = DeviceFaultAddressTypeKHR 3

-- No documentation found for Nested "VkDeviceFaultAddressTypeKHR" "VK_DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_KHR"
pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_KHR = DeviceFaultAddressTypeKHR 4

-- No documentation found for Nested "VkDeviceFaultAddressTypeKHR" "VK_DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_KHR"
pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_KHR = DeviceFaultAddressTypeKHR 5

-- No documentation found for Nested "VkDeviceFaultAddressTypeKHR" "VK_DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_KHR"
pattern DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_KHR = DeviceFaultAddressTypeKHR 6

{-# COMPLETE
  DEVICE_FAULT_ADDRESS_TYPE_NONE_KHR
  , DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_KHR
  , DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_KHR
  , DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_KHR
  , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_KHR
  , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_KHR
  , DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_KHR ::
    DeviceFaultAddressTypeKHR
  #-}

conNameDeviceFaultAddressTypeKHR :: String
conNameDeviceFaultAddressTypeKHR = "DeviceFaultAddressTypeKHR"

enumPrefixDeviceFaultAddressTypeKHR :: String
enumPrefixDeviceFaultAddressTypeKHR = "DEVICE_FAULT_ADDRESS_TYPE_"

showTableDeviceFaultAddressTypeKHR :: [(DeviceFaultAddressTypeKHR, String)]
showTableDeviceFaultAddressTypeKHR =
  [
    ( DEVICE_FAULT_ADDRESS_TYPE_NONE_KHR
    , "NONE_KHR"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_READ_INVALID_KHR
    , "READ_INVALID_KHR"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_WRITE_INVALID_KHR
    , "WRITE_INVALID_KHR"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_EXECUTE_INVALID_KHR
    , "EXECUTE_INVALID_KHR"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_UNKNOWN_KHR
    , "INSTRUCTION_POINTER_UNKNOWN_KHR"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_INVALID_KHR
    , "INSTRUCTION_POINTER_INVALID_KHR"
    )
  ,
    ( DEVICE_FAULT_ADDRESS_TYPE_INSTRUCTION_POINTER_FAULT_KHR
    , "INSTRUCTION_POINTER_FAULT_KHR"
    )
  ]

instance Show DeviceFaultAddressTypeKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixDeviceFaultAddressTypeKHR
      showTableDeviceFaultAddressTypeKHR
      conNameDeviceFaultAddressTypeKHR
      (\(DeviceFaultAddressTypeKHR x) -> x)
      (showsPrec 11)

instance Read DeviceFaultAddressTypeKHR where
  readPrec =
    enumReadPrec
      enumPrefixDeviceFaultAddressTypeKHR
      showTableDeviceFaultAddressTypeKHR
      conNameDeviceFaultAddressTypeKHR
      DeviceFaultAddressTypeKHR

-- | VkDeviceFaultVendorBinaryHeaderVersionKHR - Encode vendor binary crash
-- dump version
--
-- = Description
--
-- -   'DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_KHR' specifies
--     version one of the binary crash dump header.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_device_fault VK_EXT_device_fault>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_fault VK_KHR_device_fault>,
-- 'DeviceFaultVendorBinaryHeaderVersionOneKHR',
-- 'Vulkan.Extensions.VK_EXT_device_fault.getDeviceFaultInfoEXT'
newtype DeviceFaultVendorBinaryHeaderVersionKHR = DeviceFaultVendorBinaryHeaderVersionKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- No documentation found for Nested "VkDeviceFaultVendorBinaryHeaderVersionKHR" "VK_DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_KHR"
pattern DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_KHR = DeviceFaultVendorBinaryHeaderVersionKHR 1

{-# COMPLETE DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_KHR :: DeviceFaultVendorBinaryHeaderVersionKHR #-}

conNameDeviceFaultVendorBinaryHeaderVersionKHR :: String
conNameDeviceFaultVendorBinaryHeaderVersionKHR = "DeviceFaultVendorBinaryHeaderVersionKHR"

enumPrefixDeviceFaultVendorBinaryHeaderVersionKHR :: String
enumPrefixDeviceFaultVendorBinaryHeaderVersionKHR = "DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_KHR"

showTableDeviceFaultVendorBinaryHeaderVersionKHR :: [(DeviceFaultVendorBinaryHeaderVersionKHR, String)]
showTableDeviceFaultVendorBinaryHeaderVersionKHR =
  [
    ( DEVICE_FAULT_VENDOR_BINARY_HEADER_VERSION_ONE_KHR
    , ""
    )
  ]

instance Show DeviceFaultVendorBinaryHeaderVersionKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixDeviceFaultVendorBinaryHeaderVersionKHR
      showTableDeviceFaultVendorBinaryHeaderVersionKHR
      conNameDeviceFaultVendorBinaryHeaderVersionKHR
      (\(DeviceFaultVendorBinaryHeaderVersionKHR x) -> x)
      (showsPrec 11)

instance Read DeviceFaultVendorBinaryHeaderVersionKHR where
  readPrec =
    enumReadPrec
      enumPrefixDeviceFaultVendorBinaryHeaderVersionKHR
      showTableDeviceFaultVendorBinaryHeaderVersionKHR
      conNameDeviceFaultVendorBinaryHeaderVersionKHR
      DeviceFaultVendorBinaryHeaderVersionKHR

type KHR_DEVICE_FAULT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DEVICE_FAULT_SPEC_VERSION"
pattern KHR_DEVICE_FAULT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DEVICE_FAULT_SPEC_VERSION = 1


type KHR_DEVICE_FAULT_EXTENSION_NAME = "VK_KHR_device_fault"

-- No documentation found for TopLevel "VK_KHR_DEVICE_FAULT_EXTENSION_NAME"
pattern KHR_DEVICE_FAULT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DEVICE_FAULT_EXTENSION_NAME = "VK_KHR_device_fault"

