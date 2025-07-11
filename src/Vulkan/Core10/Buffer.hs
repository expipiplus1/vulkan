{-# language CPP #-}
-- No documentation found for Chapter "Buffer"
module Vulkan.Core10.Buffer  ( createBuffer
                             , withBuffer
                             , destroyBuffer
                             , BufferCreateInfo(..)
                             , Buffer(..)
                             , SharingMode(..)
                             , BufferUsageFlagBits(..)
                             , BufferUsageFlags
                             , BufferCreateFlagBits(..)
                             , BufferCreateFlags
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
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferCollectionBufferCreateInfoFUCHSIA)
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_buffer_device_address (BufferDeviceAddressCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferOpaqueCaptureAddressCreateInfo)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_maintenance5 (BufferUsageFlags2CreateInfoKHR)
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationBufferCreateInfoNV)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateBuffer))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyBuffer))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryBufferCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (OpaqueCaptureDescriptorDataCreateInfoEXT)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SharingMode (SharingMode)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlagBits(..))
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlagBits(..))
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core10.Enums.SharingMode (SharingMode(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateBuffer
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct BufferCreateInfo) -> Ptr AllocationCallbacks -> Ptr Buffer -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct BufferCreateInfo) -> Ptr AllocationCallbacks -> Ptr Buffer -> IO Result

-- | vkCreateBuffer - Create a new buffer object
--
-- == Valid Usage
--
-- -   #VUID-vkCreateBuffer-flags-00911# If the @flags@ member of
--     @pCreateInfo@ includes
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT',
--     and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-extendedSparseAddressSpace extendedSparseAddressSpace>
--     feature is not enabled, creating this 'Vulkan.Core10.Handles.Buffer'
--     /must/ not cause the total required sparse memory for all currently
--     valid sparse resources on the device to exceed
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@
--
-- -   #VUID-vkCreateBuffer-flags-09383# If the @flags@ member of
--     @pCreateInfo@ includes
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT',
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-extendedSparseAddressSpace extendedSparseAddressSpace>
--     feature is enabled, and the @usage@ member of @pCreateInfo@ contains
--     bits not in
--     'Vulkan.Extensions.VK_NV_extended_sparse_address_space.PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'::@extendedSparseBufferUsageFlags@,
--     creating this 'Vulkan.Core10.Handles.Buffer' /must/ not cause the
--     total required sparse memory for all currently valid sparse
--     resources on the device, excluding 'Vulkan.Core10.Handles.Buffer'
--     created with @usage@ member of @pCreateInfo@ containing bits in
--     'Vulkan.Extensions.VK_NV_extended_sparse_address_space.PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'::@extendedSparseBufferUsageFlags@
--     and 'Vulkan.Core10.Handles.Image' created with @usage@ member of
--     @pCreateInfo@ containing bits in
--     'Vulkan.Extensions.VK_NV_extended_sparse_address_space.PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'::@extendedSparseImageUsageFlags@,
--     to exceed
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@sparseAddressSpaceSize@
--
-- -   #VUID-vkCreateBuffer-flags-09384# If the @flags@ member of
--     @pCreateInfo@ includes
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT'
--     and the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-extendedSparseAddressSpace extendedSparseAddressSpace>
--     feature is enabled, creating this 'Vulkan.Core10.Handles.Buffer'
--     /must/ not cause the total required sparse memory for all currently
--     valid sparse resources on the device to exceed
--     'Vulkan.Extensions.VK_NV_extended_sparse_address_space.PhysicalDeviceExtendedSparseAddressSpacePropertiesNV'::@extendedSparseAddressSpaceSize@
--
-- -   #VUID-vkCreateBuffer-pNext-06387# If using the
--     'Vulkan.Core10.Handles.Buffer' for an import operation from a
--     'Vulkan.Extensions.Handles.BufferCollectionFUCHSIA' where a
--     'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferCollectionBufferCreateInfoFUCHSIA'
--     has been chained to @pNext@, @pCreateInfo@ /must/ match the
--     'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferConstraintsInfoFUCHSIA'::@createInfo@
--     used when setting the constraints on the buffer collection with
--     'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.setBufferCollectionBufferConstraintsFUCHSIA'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateBuffer-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateBuffer-pCreateInfo-parameter# @pCreateInfo@ /must/ be
--     a valid pointer to a valid 'BufferCreateInfo' structure
--
-- -   #VUID-vkCreateBuffer-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateBuffer-pBuffer-parameter# @pBuffer@ /must/ be a valid
--     pointer to a 'Vulkan.Core10.Handles.Buffer' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Extensions.VK_KHR_buffer_device_address.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Buffer', 'BufferCreateInfo',
-- 'Vulkan.Core10.Handles.Device'
createBuffer :: forall a io
              . (Extendss BufferCreateInfo a, PokeChain a, MonadIO io)
             => -- | @device@ is the logical device that creates the buffer object.
                Device
             -> -- | @pCreateInfo@ is a pointer to a 'BufferCreateInfo' structure containing
                -- parameters affecting creation of the buffer.
                (BufferCreateInfo a)
             -> -- | @pAllocator@ controls host memory allocation as described in the
                -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                -- chapter.
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io (Buffer)
createBuffer device createInfo allocator = liftIO . evalContT $ do
  let vkCreateBufferPtr = pVkCreateBuffer (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateBuffer is null" Nothing Nothing
  let vkCreateBuffer' = mkVkCreateBuffer vkCreateBufferPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPBuffer <- ContT $ bracket (callocBytes @Buffer 8) free
  r <- lift $ traceAroundEvent "vkCreateBuffer" (vkCreateBuffer'
                                                   (deviceHandle (device))
                                                   (forgetExtensions pCreateInfo)
                                                   pAllocator
                                                   (pPBuffer))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pBuffer <- lift $ peek @Buffer pPBuffer
  pure $ (pBuffer)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createBuffer' and 'destroyBuffer'
--
-- To ensure that 'destroyBuffer' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withBuffer :: forall a io r . (Extendss BufferCreateInfo a, PokeChain a, MonadIO io) => Device -> BufferCreateInfo a -> Maybe AllocationCallbacks -> (io Buffer -> (Buffer -> io ()) -> r) -> r
withBuffer device pCreateInfo pAllocator b =
  b (createBuffer device pCreateInfo pAllocator)
    (\(o0) -> destroyBuffer device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyBuffer
  :: FunPtr (Ptr Device_T -> Buffer -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Buffer -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyBuffer - Destroy a buffer object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyBuffer-buffer-00922# All submitted commands that
--     refer to @buffer@, either directly or via a
--     'Vulkan.Core10.Handles.BufferView', /must/ have completed execution
--
-- -   #VUID-vkDestroyBuffer-buffer-00923# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @buffer@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyBuffer-buffer-00924# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @buffer@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyBuffer-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyBuffer-buffer-parameter# If @buffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @buffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkDestroyBuffer-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyBuffer-buffer-parent# If @buffer@ is a valid handle,
--     it /must/ have been created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @buffer@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.Device'
destroyBuffer :: forall io
               . (MonadIO io)
              => -- | @device@ is the logical device that destroys the buffer.
                 Device
              -> -- | @buffer@ is the buffer to destroy.
                 Buffer
              -> -- | @pAllocator@ controls host memory allocation as described in the
                 -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                 -- chapter.
                 ("allocator" ::: Maybe AllocationCallbacks)
              -> io ()
destroyBuffer device buffer allocator = liftIO . evalContT $ do
  let vkDestroyBufferPtr = pVkDestroyBuffer (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyBufferPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyBuffer is null" Nothing Nothing
  let vkDestroyBuffer' = mkVkDestroyBuffer vkDestroyBufferPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyBuffer" (vkDestroyBuffer'
                                               (deviceHandle (device))
                                               (buffer)
                                               pAllocator)
  pure $ ()


-- | VkBufferCreateInfo - Structure specifying the parameters of a newly
-- created buffer object
--
-- = Description
--
-- If a
-- 'Vulkan.Extensions.VK_KHR_maintenance5.BufferUsageFlags2CreateInfoKHR'
-- structure is present in the @pNext@ chain,
-- 'Vulkan.Extensions.VK_KHR_maintenance5.BufferUsageFlags2CreateInfoKHR'::@usage@
-- from that structure is used instead of @usage@ from this structure.
--
-- == Valid Usage
--
-- -   #VUID-VkBufferCreateInfo-size-00912# @size@ /must/ be greater than
--     @0@
--
-- -   #VUID-VkBufferCreateInfo-sharingMode-00913# If @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   #VUID-VkBufferCreateInfo-sharingMode-00914# If @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT',
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   #VUID-VkBufferCreateInfo-sharingMode-01419# If @sharingMode@ is
--     'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT', each
--     element of @pQueueFamilyIndices@ /must/ be unique and /must/ be less
--     than @pQueueFamilyPropertyCount@ returned by either
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2'
--     or
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   #VUID-VkBufferCreateInfo-flags-00915# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseBinding sparseBinding>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT'
--
-- -   #VUID-VkBufferCreateInfo-flags-00916# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseResidencyBuffer sparseResidencyBuffer>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   #VUID-VkBufferCreateInfo-flags-00917# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-sparseResidencyAliased sparseResidencyAliased>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_ALIASED_BIT'
--
-- -   #VUID-VkBufferCreateInfo-flags-00918# If @flags@ contains
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     or
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_ALIASED_BIT',
--     it /must/ also contain
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT'
--
-- -   #VUID-VkBufferCreateInfo-pNext-00920# If the @pNext@ chain includes
--     a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'
--     structure, its @handleTypes@ member /must/ only contain bits that
--     are also in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties'::@externalMemoryProperties.compatibleHandleTypes@,
--     as returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.getPhysicalDeviceExternalBufferProperties'
--     with @pExternalBufferInfo->handleType@ equal to any one of the
--     handle types specified in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'::@handleTypes@
--
-- -   #VUID-VkBufferCreateInfo-flags-01887# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-protectedMemory protectedMemory>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--
-- -   #VUID-VkBufferCreateInfo-None-01888# If any of the bits
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT',
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_ALIASED_BIT'
--     are set,
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     /must/ not also be set
--
-- -   #VUID-VkBufferCreateInfo-pNext-01571# If the @pNext@ chain includes
--     a
--     'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationBufferCreateInfoNV'
--     structure, and the @dedicatedAllocation@ member of the chained
--     structure is 'Vulkan.Core10.FundamentalTypes.TRUE', then @flags@
--     /must/ not include
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_BINDING_BIT',
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_ALIASED_BIT'
--
-- -   #VUID-VkBufferCreateInfo-deviceAddress-02604# If
--     'Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT'::@deviceAddress@
--     is not zero, @flags@ /must/ include
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
--
-- -   #VUID-VkBufferCreateInfo-opaqueCaptureAddress-03337# If
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo'::@opaqueCaptureAddress@
--     is not zero, @flags@ /must/ include
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT'
--
-- -   #VUID-VkBufferCreateInfo-flags-03338# If @flags@ includes
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT',
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-bufferDeviceAddressCaptureReplayEXT bufferDeviceAddressCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-VkBufferCreateInfo-usage-04813# If @usage@ includes
--     @VK_BUFFER_USAGE_VIDEO_DECODE_SRC_BIT_KHR@ or
--     @VK_BUFFER_USAGE_VIDEO_DECODE_DST_BIT_KHR@, then the @pNext@ chain
--     /must/ include a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--     structure with @profileCount@ greater than @0@ and @pProfiles@
--     including at least one
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileInfoKHR VkVideoProfileInfoKHR>
--     structure with a @videoCodecOperation@ member specifying a decode
--     operation
--
-- -   #VUID-VkBufferCreateInfo-usage-04814# If @usage@ includes
--     @VK_BUFFER_USAGE_VIDEO_ENCODE_SRC_BIT_KHR@ or
--     @VK_BUFFER_USAGE_VIDEO_ENCODE_DST_BIT_KHR@, then the @pNext@ chain
--     /must/ include a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--     structure with @profileCount@ greater than @0@ and @pProfiles@
--     including at least one
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileInfoKHR VkVideoProfileInfoKHR>
--     structure with a @videoCodecOperation@ member specifying an encode
--     operation
--
-- -   #VUID-VkBufferCreateInfo-size-06409# @size@ /must/ be less than or
--     equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_maintenance4.PhysicalDeviceMaintenance4Properties'::@maxBufferSize@
--
-- -   #VUID-VkBufferCreateInfo-usage-08097# If @usage@ includes
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT',
--     creating this 'Vulkan.Core10.Handles.Buffer' /must/ not cause the
--     total required space for all currently valid buffers using this flag
--     on the device to exceed
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferPropertiesEXT'::@samplerDescriptorBufferAddressSpaceSize@
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferPropertiesEXT'::@descriptorBufferAddressSpaceSize@
--
-- -   #VUID-VkBufferCreateInfo-usage-08098# If @usage@ includes
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT',
--     creating this 'Vulkan.Core10.Handles.Buffer' /must/ not cause the
--     total required space for all currently valid buffers using this flag
--     on the device to exceed
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferPropertiesEXT'::@resourceDescriptorBufferAddressSpaceSize@
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.PhysicalDeviceDescriptorBufferPropertiesEXT'::@descriptorBufferAddressSpaceSize@
--
-- -   #VUID-VkBufferCreateInfo-flags-08099# If @flags@ includes
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT',
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBufferCaptureReplay descriptorBufferCaptureReplay>
--     feature /must/ be enabled
--
-- -   #VUID-VkBufferCreateInfo-pNext-08100# If the @pNext@ chain includes
--     a
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT'
--     structure, @flags@ /must/ contain
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--
-- -   #VUID-VkBufferCreateInfo-usage-08101# If @usage@ includes
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT',
--     the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-descriptorBufferPushDescriptors descriptorBufferPushDescriptors>
--     feature /must/ be enabled
--
-- -   #VUID-VkBufferCreateInfo-usage-08102# If @usage@ includes
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT'
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-bufferlessPushDescriptors ::bufferlessPushDescriptors>
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkBufferCreateInfo-usage-08103# If @usage@ includes
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT',
--     @usage@ /must/ contain at least one of
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-VkBufferCreateInfo-None-09205# If the @pNext@ chain does not
--     include a
--     'Vulkan.Extensions.VK_KHR_maintenance5.BufferUsageFlags2CreateInfoKHR'
--     structure, @usage@ must be a valid combination of
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits' values
--
-- -   #VUID-VkBufferCreateInfo-None-09206# If the @pNext@ chain does not
--     include a
--     'Vulkan.Extensions.VK_KHR_maintenance5.BufferUsageFlags2CreateInfoKHR'
--     structure, @usage@ /must/ not be @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBufferCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_CREATE_INFO'
--
-- -   #VUID-VkBufferCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferCollectionBufferCreateInfoFUCHSIA',
--     'Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo',
--     'Vulkan.Extensions.VK_KHR_maintenance5.BufferUsageFlags2CreateInfoKHR',
--     'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationBufferCreateInfoNV',
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo',
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT',
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoProfileListInfoKHR VkVideoProfileListInfoKHR>
--
-- -   #VUID-VkBufferCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkBufferCreateInfo-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlagBits'
--     values
--
-- -   #VUID-VkBufferCreateInfo-sharingMode-parameter# @sharingMode@ /must/
--     be a valid 'Vulkan.Core10.Enums.SharingMode.SharingMode' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferConstraintsInfoFUCHSIA',
-- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlags',
-- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlags',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_maintenance4.DeviceBufferMemoryRequirements',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.SharingMode.SharingMode',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createBuffer'
data BufferCreateInfo (es :: [Type]) = BufferCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlagBits'
    -- specifying additional parameters of the buffer.
    flags :: BufferCreateFlags
  , -- | @size@ is the size in bytes of the buffer to be created.
    size :: DeviceSize
  , -- | @usage@ is a bitmask of
    -- 'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits' specifying
    -- allowed usages of the buffer.
    usage :: BufferUsageFlags
  , -- | @sharingMode@ is a 'Vulkan.Core10.Enums.SharingMode.SharingMode' value
    -- specifying the sharing mode of the buffer when it will be accessed by
    -- multiple queue families.
    sharingMode :: SharingMode
  , -- | @pQueueFamilyIndices@ is a pointer to an array of queue families that
    -- will access this buffer. It is ignored if @sharingMode@ is not
    -- 'Vulkan.Core10.Enums.SharingMode.SHARING_MODE_CONCURRENT'.
    queueFamilyIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BufferCreateInfo es)

instance Extensible BufferCreateInfo where
  extensibleTypeName = "BufferCreateInfo"
  setNext BufferCreateInfo{..} next' = BufferCreateInfo{next = next', ..}
  getNext BufferCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BufferCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @BufferCollectionBufferCreateInfoFUCHSIA = Just f
    | Just Refl <- eqT @e @OpaqueCaptureDescriptorDataCreateInfoEXT = Just f
    | Just Refl <- eqT @e @BufferDeviceAddressCreateInfoEXT = Just f
    | Just Refl <- eqT @e @BufferOpaqueCaptureAddressCreateInfo = Just f
    | Just Refl <- eqT @e @ExternalMemoryBufferCreateInfo = Just f
    | Just Refl <- eqT @e @DedicatedAllocationBufferCreateInfoNV = Just f
    | Just Refl <- eqT @e @BufferUsageFlags2CreateInfoKHR = Just f
    | otherwise = Nothing

instance ( Extendss BufferCreateInfo es
         , PokeChain es ) => ToCStruct (BufferCreateInfo es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr BufferCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (size)
    lift $ poke ((p `plusPtr` 32 :: Ptr BufferUsageFlags)) (usage)
    lift $ poke ((p `plusPtr` 36 :: Ptr SharingMode)) (sharingMode)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueFamilyIndices)) :: Word32))
    pPQueueFamilyIndices' <- ContT $ allocaBytes @Word32 ((Data.Vector.length (queueFamilyIndices)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueueFamilyIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (queueFamilyIndices)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPQueueFamilyIndices')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr BufferUsageFlags)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr SharingMode)) (zero)
    lift $ f

instance ( Extendss BufferCreateInfo es
         , PeekChain es ) => FromCStruct (BufferCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @BufferCreateFlags ((p `plusPtr` 16 :: Ptr BufferCreateFlags))
    size <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    usage <- peek @BufferUsageFlags ((p `plusPtr` 32 :: Ptr BufferUsageFlags))
    sharingMode <- peek @SharingMode ((p `plusPtr` 36 :: Ptr SharingMode))
    queueFamilyIndexCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pQueueFamilyIndices <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pQueueFamilyIndices' <- generateM (fromIntegral queueFamilyIndexCount) (\i -> peek @Word32 ((pQueueFamilyIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ BufferCreateInfo
             next flags size usage sharingMode pQueueFamilyIndices'

instance es ~ '[] => Zero (BufferCreateInfo es) where
  zero = BufferCreateInfo
           ()
           zero
           zero
           zero
           zero
           mempty

