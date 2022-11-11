{-# language CPP #-}
-- | = Name
--
-- VK_NV_external_memory_rdma - device extension
--
-- == VK_NV_external_memory_rdma
--
-- [__Name String__]
--     @VK_NV_external_memory_rdma@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     372
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Carsten Rohde
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_external_memory_rdma] @crohde%0A*Here describe the issue or question you have about the VK_NV_external_memory_rdma extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-04-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Carsten Rohde, NVIDIA
--
-- == Description
--
-- This extension adds support for allocating memory which can be used for
-- remote direct memory access (RDMA) from other devices.
--
-- == New Base Types
--
-- -   'RemoteAddressNV'
--
-- == New Commands
--
-- -   'getMemoryRemoteAddressNV'
--
-- == New Structures
--
-- -   'MemoryGetRemoteAddressInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExternalMemoryRDMAFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTERNAL_MEMORY_RDMA_EXTENSION_NAME'
--
-- -   'NV_EXTERNAL_MEMORY_RDMA_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MemoryPropertyFlagBits':
--
--     -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_RDMA_CAPABLE_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_REMOTE_ADDRESS_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_RDMA_FEATURES_NV'
--
-- == Examples
--
-- > VkPhysicalDeviceMemoryBudgetPropertiesEXT memoryBudgetProperties = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT };
-- > VkPhysicalDeviceMemoryProperties2 memoryProperties2 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2, &memoryBudgetProperties };
-- > vkGetPhysicalDeviceMemoryProperties2(physicalDevice, &memoryProperties2);
-- > uint32_t heapIndex = (uint32_t)-1;
-- > for (uint32_t memoryType = 0; memoryType < memoryProperties2.memoryProperties.memoryTypeCount; memoryType++) {
-- >     if (memoryProperties2.memoryProperties.memoryTypes[memoryType].propertyFlags & VK_MEMORY_PROPERTY_RDMA_CAPABLE_BIT_NV) {
-- >         heapIndex = memoryProperties2.memoryProperties.memoryTypes[memoryType].heapIndex;
-- >         break;
-- >     }
-- > }
-- > if ((heapIndex == (uint32_t)-1) ||
-- >     (memoryBudgetProperties.heapBudget[heapIndex] < size)) {
-- >     return;
-- > }
-- >
-- > VkPhysicalDeviceExternalBufferInfo externalBufferInfo = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO };
-- > externalBufferInfo.usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT;
-- > externalBufferInfo.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV;
-- >
-- > VkExternalBufferProperties externalBufferProperties = { VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES };
-- > vkGetPhysicalDeviceExternalBufferProperties(physicalDevice, &externalBufferInfo, &externalBufferProperties);
-- >
-- > if (!(externalBufferProperties.externalMemoryProperties.externalMemoryFeatures & VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT)) {
-- >     return;
-- > }
-- >
-- > VkExternalMemoryBufferCreateInfo externalMemoryBufferCreateInfo = { VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO };
-- > externalMemoryBufferCreateInfo.handleTypes = VK_EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV;
-- >
-- > VkBufferCreateInfo bufferCreateInfo = { VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO, &externalMemoryBufferCreateInfo };
-- > bufferCreateInfo.size = size;
-- > bufferCreateInfo.usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT;
-- >
-- > VkMemoryRequirements mem_reqs;
-- > vkCreateBuffer(device, &bufferCreateInfo, NULL, &buffer);
-- > vkGetBufferMemoryRequirements(device, buffer, &mem_reqs);
-- >
-- > VkExportMemoryAllocateInfo exportMemoryAllocateInfo = { VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO };
-- > exportMemoryAllocateInfo.handleTypes = VK_EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV;
-- >
-- > // Find memory type index
-- > uint32_t i = 0;
-- > for (; i < VK_MAX_MEMORY_TYPES; i++) {
-- >     if ((mem_reqs.memoryTypeBits & (1 << i)) &&
-- >         (memoryProperties.memoryTypes[i].propertyFlags & VK_MEMORY_PROPERTY_RDMA_CAPABLE_BIT_NV)) {
-- >         break;
-- >     }
-- > }
-- >
-- > VkMemoryAllocateInfo memAllocInfo = { VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO, &exportMemoryAllocateInfo };
-- > memAllocInfo.allocationSize = mem_reqs.size;
-- > memAllocInfo.memoryTypeIndex = i;
-- >
-- > vkAllocateMemory(device, &memAllocInfo, NULL, &mem);
-- > vkBindBufferMemory(device, buffer, mem, 0);
-- >
-- > VkMemoryGetRemoteAddressInfoNV getMemoryRemoteAddressInfo = { VK_STRUCTURE_TYPE_MEMORY_GET_REMOTE_ADDRESS_INFO_NV };
-- > getMemoryRemoteAddressInfo.memory = mem;
-- > getMemoryRemoteAddressInfo.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV;
-- >
-- > VkRemoteAddressNV rdmaAddress;
-- > vkGetMemoryRemoteAddressNV(device, &getMemoryRemoteAddressInfo, &rdmaAddress);
-- > // address returned in 'rdmaAddress' can be used by external devices to initiate RDMA transfers
--
-- == Version History
--
-- -   Revision 1, 2020-12-15 (Carsten Rohde)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'MemoryGetRemoteAddressInfoNV',
-- 'PhysicalDeviceExternalMemoryRDMAFeaturesNV', 'RemoteAddressNV',
-- 'getMemoryRemoteAddressNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_external_memory_rdma Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_external_memory_rdma  ( getMemoryRemoteAddressNV
                                                     , PhysicalDeviceExternalMemoryRDMAFeaturesNV(..)
                                                     , MemoryGetRemoteAddressInfoNV(..)
                                                     , NV_EXTERNAL_MEMORY_RDMA_SPEC_VERSION
                                                     , pattern NV_EXTERNAL_MEMORY_RDMA_SPEC_VERSION
                                                     , NV_EXTERNAL_MEMORY_RDMA_EXTENSION_NAME
                                                     , pattern NV_EXTERNAL_MEMORY_RDMA_EXTENSION_NAME
                                                     , RemoteAddressNV
                                                     ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
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
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetMemoryRemoteAddressNV))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_GET_REMOTE_ADDRESS_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_RDMA_FEATURES_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryRemoteAddressNV
  :: FunPtr (Ptr Device_T -> Ptr MemoryGetRemoteAddressInfoNV -> Ptr RemoteAddressNV -> IO Result) -> Ptr Device_T -> Ptr MemoryGetRemoteAddressInfoNV -> Ptr RemoteAddressNV -> IO Result

-- | vkGetMemoryRemoteAddressNV - Get an address for a memory object
-- accessible by remote devices
--
-- = Description
--
-- More communication may be required between the kernel-mode drivers of
-- the devices involved. This information is out of scope of this
-- documentation and should be requested from the vendors of the devices.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_EXTERNAL_HANDLE'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_rdma VK_NV_external_memory_rdma>,
-- 'Vulkan.Core10.Handles.Device', 'MemoryGetRemoteAddressInfoNV',
-- 'RemoteAddressNV'
getMemoryRemoteAddressNV :: forall io
                          . (MonadIO io)
                         => -- | @device@ is the logical device that created the device memory being
                            -- exported.
                            --
                            -- #VUID-vkGetMemoryRemoteAddressNV-device-parameter# @device@ /must/ be a
                            -- valid 'Vulkan.Core10.Handles.Device' handle
                            Device
                         -> -- | @pMemoryGetRemoteAddressInfo@ is a pointer to a
                            -- 'MemoryGetRemoteAddressInfoNV' structure containing parameters of the
                            -- export operation.
                            --
                            -- #VUID-vkGetMemoryRemoteAddressNV-pMemoryGetRemoteAddressInfo-parameter#
                            -- @pMemoryGetRemoteAddressInfo@ /must/ be a valid pointer to a valid
                            -- 'MemoryGetRemoteAddressInfoNV' structure
                            MemoryGetRemoteAddressInfoNV
                         -> io (RemoteAddressNV)
getMemoryRemoteAddressNV device
                           memoryGetRemoteAddressInfo = liftIO . evalContT $ do
  let vkGetMemoryRemoteAddressNVPtr = pVkGetMemoryRemoteAddressNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetMemoryRemoteAddressNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetMemoryRemoteAddressNV is null" Nothing Nothing
  let vkGetMemoryRemoteAddressNV' = mkVkGetMemoryRemoteAddressNV vkGetMemoryRemoteAddressNVPtr
  pMemoryGetRemoteAddressInfo <- ContT $ withCStruct (memoryGetRemoteAddressInfo)
  pPAddress <- ContT $ bracket (callocBytes @RemoteAddressNV 8) free
  r <- lift $ traceAroundEvent "vkGetMemoryRemoteAddressNV" (vkGetMemoryRemoteAddressNV'
                                                               (deviceHandle (device))
                                                               pMemoryGetRemoteAddressInfo
                                                               (pPAddress))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAddress <- lift $ peek @RemoteAddressNV pPAddress
  pure $ (pAddress)


-- | VkPhysicalDeviceExternalMemoryRDMAFeaturesNV - Structure describing the
-- external memory RDMA features supported by the implementation
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceExternalMemoryRDMAFeaturesNV' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceExternalMemoryRDMAFeaturesNV' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_rdma VK_NV_external_memory_rdma>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExternalMemoryRDMAFeaturesNV = PhysicalDeviceExternalMemoryRDMAFeaturesNV
  { -- | #features-externalMemoryRDMA# @externalMemoryRDMA@ indicates whether the
    -- implementation has support for the
    -- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_RDMA_CAPABLE_BIT_NV'
    -- memory property and the
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV'
    -- external memory handle type.
    externalMemoryRDMA :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExternalMemoryRDMAFeaturesNV)
#endif
deriving instance Show PhysicalDeviceExternalMemoryRDMAFeaturesNV

instance ToCStruct PhysicalDeviceExternalMemoryRDMAFeaturesNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExternalMemoryRDMAFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_RDMA_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (externalMemoryRDMA))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_RDMA_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExternalMemoryRDMAFeaturesNV where
  peekCStruct p = do
    externalMemoryRDMA <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceExternalMemoryRDMAFeaturesNV
             (bool32ToBool externalMemoryRDMA)

instance Storable PhysicalDeviceExternalMemoryRDMAFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExternalMemoryRDMAFeaturesNV where
  zero = PhysicalDeviceExternalMemoryRDMAFeaturesNV
           zero


-- | VkMemoryGetRemoteAddressInfoNV - Structure describing a remote
-- accessible address export operation
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_rdma VK_NV_external_memory_rdma>,
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getMemoryRemoteAddressNV'
data MemoryGetRemoteAddressInfoNV = MemoryGetRemoteAddressInfoNV
  { -- | @memory@ is the memory object from which the remote accessible address
    -- will be exported.
    --
    -- #VUID-VkMemoryGetRemoteAddressInfoNV-memory-parameter# @memory@ /must/
    -- be a valid 'Vulkan.Core10.Handles.DeviceMemory' handle
    memory :: DeviceMemory
  , -- | @handleType@ is the type of handle requested.
    --
    -- #VUID-VkMemoryGetRemoteAddressInfoNV-handleType-04966# @handleType@
    -- /must/ have been included in
    -- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
    -- when @memory@ was created
    --
    -- #VUID-VkMemoryGetRemoteAddressInfoNV-handleType-parameter# @handleType@
    -- /must/ be a valid
    -- 'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits'
    -- value
    handleType :: ExternalMemoryHandleTypeFlagBits
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (MemoryGetRemoteAddressInfoNV)
#endif
deriving instance Show MemoryGetRemoteAddressInfoNV

instance ToCStruct MemoryGetRemoteAddressInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryGetRemoteAddressInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_REMOTE_ADDRESS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (memory)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (handleType)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_MEMORY_GET_REMOTE_ADDRESS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits)) (zero)
    f

instance FromCStruct MemoryGetRemoteAddressInfoNV where
  peekCStruct p = do
    memory <- peek @DeviceMemory ((p `plusPtr` 16 :: Ptr DeviceMemory))
    handleType <- peek @ExternalMemoryHandleTypeFlagBits ((p `plusPtr` 24 :: Ptr ExternalMemoryHandleTypeFlagBits))
    pure $ MemoryGetRemoteAddressInfoNV
             memory handleType

instance Storable MemoryGetRemoteAddressInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryGetRemoteAddressInfoNV where
  zero = MemoryGetRemoteAddressInfoNV
           zero
           zero


type NV_EXTERNAL_MEMORY_RDMA_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_RDMA_SPEC_VERSION"
pattern NV_EXTERNAL_MEMORY_RDMA_SPEC_VERSION :: forall a . Integral a => a
pattern NV_EXTERNAL_MEMORY_RDMA_SPEC_VERSION = 1


type NV_EXTERNAL_MEMORY_RDMA_EXTENSION_NAME = "VK_NV_external_memory_rdma"

-- No documentation found for TopLevel "VK_NV_EXTERNAL_MEMORY_RDMA_EXTENSION_NAME"
pattern NV_EXTERNAL_MEMORY_RDMA_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_EXTERNAL_MEMORY_RDMA_EXTENSION_NAME = "VK_NV_external_memory_rdma"


-- | VkRemoteAddressNV - Remote device address type
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_external_memory_rdma VK_NV_external_memory_rdma>,
-- 'getMemoryRemoteAddressNV'
type RemoteAddressNV = Ptr ()

