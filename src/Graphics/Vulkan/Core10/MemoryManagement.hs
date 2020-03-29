{-# language CPP #-}
module Graphics.Vulkan.Core10.MemoryManagement  ( getBufferMemoryRequirements
                                                , bindBufferMemory
                                                , getImageMemoryRequirements
                                                , bindImageMemory
                                                , MemoryRequirements(..)
                                                ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.Handles (Buffer)
import Graphics.Vulkan.Core10.Handles (Buffer(..))
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkBindBufferMemory))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkBindImageMemory))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetBufferMemoryRequirements))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetImageMemoryRequirements))
import Graphics.Vulkan.Core10.Handles (DeviceMemory)
import Graphics.Vulkan.Core10.Handles (DeviceMemory(..))
import Graphics.Vulkan.Core10.BaseType (DeviceSize)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Handles (Image)
import Graphics.Vulkan.Core10.Handles (Image(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferMemoryRequirements
  :: FunPtr (Ptr Device_T -> Buffer -> Ptr MemoryRequirements -> IO ()) -> Ptr Device_T -> Buffer -> Ptr MemoryRequirements -> IO ()

-- | vkGetBufferMemoryRequirements - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     owns the buffer.
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' is the buffer to query.
--
-- -   @pMemoryRequirements@ is a pointer to a 'MemoryRequirements'
--     structure in which the memory requirements of the buffer object are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.Device', 'MemoryRequirements'
getBufferMemoryRequirements :: Device -> Buffer -> IO (MemoryRequirements)
getBufferMemoryRequirements device buffer = evalContT $ do
  let vkGetBufferMemoryRequirements' = mkVkGetBufferMemoryRequirements (pVkGetBufferMemoryRequirements (deviceCmds (device :: Device)))
  pPMemoryRequirements <- ContT (withZeroCStruct @MemoryRequirements)
  lift $ vkGetBufferMemoryRequirements' (deviceHandle (device)) (buffer) (pPMemoryRequirements)
  pMemoryRequirements <- lift $ peekCStruct @MemoryRequirements pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindBufferMemory
  :: FunPtr (Ptr Device_T -> Buffer -> DeviceMemory -> DeviceSize -> IO Result) -> Ptr Device_T -> Buffer -> DeviceMemory -> DeviceSize -> IO Result

-- | vkBindBufferMemory - Bind device memory to a buffer object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     owns the buffer and memory.
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' is the buffer to be attached
--     to memory.
--
-- -   @memory@ is a 'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--     describing the device memory to attach.
--
-- -   @memoryOffset@ is the start offset of the region of @memory@ which
--     is to be bound to the buffer. The number of bytes returned in the
--     'MemoryRequirements'::@size@ member in @memory@, starting from
--     @memoryOffset@ bytes, will be bound to the specified buffer.
--
-- = Description
--
-- 'bindBufferMemory' is equivalent to passing the same parameters through
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo'
-- to
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindBufferMemory2'.
--
-- == Valid Usage
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ not already be backed
--     by a memory object
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ not have been created
--     with any sparse memory binding flags
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the 'MemoryRequirements'
--     structure returned from a call to 'getBufferMemoryRequirements' with
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the 'MemoryRequirements' structure returned from a call to
--     'getBufferMemoryRequirements' with
--     'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   The @size@ member of the 'MemoryRequirements' structure returned
--     from a call to 'getBufferMemoryRequirements' with
--     'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be less than or equal
--     to the size of @memory@ minus @memoryOffset@
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' requires a dedicated
--     allocation(as reported by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2'
--     in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for 'Graphics.Vulkan.Core10.Handles.Buffer'), @memory@ /must/ have
--     been created with
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::'Graphics.Vulkan.Core10.Handles.Buffer'
--     equal to 'Graphics.Vulkan.Core10.Handles.Buffer'
--
-- -   If the 'Graphics.Vulkan.Core10.Memory.MemoryAllocateInfo' provided
--     when @memory@ was allocated included a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'
--     structure in its @pNext@ chain, and
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::'Graphics.Vulkan.Core10.Handles.Buffer'
--     was not 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', then
--     'Graphics.Vulkan.Core10.Handles.Buffer' /must/ equal
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::'Graphics.Vulkan.Core10.Handles.Buffer',
--     and @memoryOffset@ /must/ be zero.
--
-- -   If buffer was created with the
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     bit set, the buffer /must/ be bound to a memory object allocated
--     with a memory type that reports
--     'Graphics.Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   If buffer was created with the
--     'Graphics.Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     bit not set, the buffer /must/ not be bound to a memory object
--     created with a memory type that reports
--     'Graphics.Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Buffer' was created with
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationBufferCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Graphics.Vulkan.Core10.BaseType.TRUE', @memory@ /must/
--     have been created with
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationMemoryAllocateInfoNV'::'Graphics.Vulkan.Core10.Handles.Buffer'
--     equal to a buffer handle created with identical creation parameters
--     to 'Graphics.Vulkan.Core10.Handles.Buffer' and @memoryOffset@ /must/
--     be zero
--
-- -   If the value of
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     used to allocate @memory@ is not @0@, it /must/ include at least one
--     of the handles set in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'::@handleTypes@
--     when 'Graphics.Vulkan.Core10.Handles.Buffer' was created
--
-- -   If @memory@ was created by a memory import operation, the external
--     handle type of the imported memory /must/ also have been set in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'::@handleTypes@
--     when 'Graphics.Vulkan.Core10.Handles.Buffer' was created
--
-- -   If the
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures'::@bufferDeviceAddress@
--     feature is enabled and 'Graphics.Vulkan.Core10.Handles.Buffer' was
--     created with the
--     'Graphics.Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT'
--     bit set, @memory@ /must/ have been allocated with the
--     'Graphics.Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT'
--     bit set
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Buffer' handle
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Buffer' /must/ have been created,
--     allocated, or retrieved from 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   @memory@ /must/ have been created, allocated, or retrieved from
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.Buffer' /must/ be
--     externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Extensions.VK_KHR_buffer_device_address.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Buffer',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.DeviceMemory',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize'
bindBufferMemory :: Device -> Buffer -> DeviceMemory -> ("memoryOffset" ::: DeviceSize) -> IO ()
bindBufferMemory device buffer memory memoryOffset = do
  let vkBindBufferMemory' = mkVkBindBufferMemory (pVkBindBufferMemory (deviceCmds (device :: Device)))
  r <- vkBindBufferMemory' (deviceHandle (device)) (buffer) (memory) (memoryOffset)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageMemoryRequirements
  :: FunPtr (Ptr Device_T -> Image -> Ptr MemoryRequirements -> IO ()) -> Ptr Device_T -> Image -> Ptr MemoryRequirements -> IO ()

-- | vkGetImageMemoryRequirements - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     owns the image.
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' is the image to query.
--
-- -   @pMemoryRequirements@ is a pointer to a 'MemoryRequirements'
--     structure in which the memory requirements of the image object are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Image', 'MemoryRequirements'
getImageMemoryRequirements :: Device -> Image -> IO (MemoryRequirements)
getImageMemoryRequirements device image = evalContT $ do
  let vkGetImageMemoryRequirements' = mkVkGetImageMemoryRequirements (pVkGetImageMemoryRequirements (deviceCmds (device :: Device)))
  pPMemoryRequirements <- ContT (withZeroCStruct @MemoryRequirements)
  lift $ vkGetImageMemoryRequirements' (deviceHandle (device)) (image) (pPMemoryRequirements)
  pMemoryRequirements <- lift $ peekCStruct @MemoryRequirements pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindImageMemory
  :: FunPtr (Ptr Device_T -> Image -> DeviceMemory -> DeviceSize -> IO Result) -> Ptr Device_T -> Image -> DeviceMemory -> DeviceSize -> IO Result

-- | vkBindImageMemory - Bind device memory to an image object
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the logical device that
--     owns the image and memory.
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' is the image.
--
-- -   @memory@ is the 'Graphics.Vulkan.Core10.Handles.DeviceMemory' object
--     describing the device memory to attach.
--
-- -   @memoryOffset@ is the start offset of the region of @memory@ which
--     is to be bound to the image. The number of bytes returned in the
--     'MemoryRequirements'::@size@ member in @memory@, starting from
--     @memoryOffset@ bytes, will be bound to the specified image.
--
-- = Description
--
-- 'bindImageMemory' is equivalent to passing the same parameters through
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo'
-- to
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindImageMemory2'.
--
-- == Valid Usage
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ not have been created
--     with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     set.
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ not already be backed
--     by a memory object
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ not have been created
--     with any sparse memory binding flags
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the 'MemoryRequirements'
--     structure returned from a call to 'getImageMemoryRequirements' with
--     'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the 'MemoryRequirements' structure returned from a call to
--     'getImageMemoryRequirements' with
--     'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   The difference of the size of @memory@ and @memoryOffset@ /must/ be
--     greater than or equal to the @size@ member of the
--     'MemoryRequirements' structure returned from a call to
--     'getImageMemoryRequirements' with the same
--     'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' requires a dedicated
--     allocation (as reported by
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'
--     in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for 'Graphics.Vulkan.Core10.Handles.Image'), @memory@ /must/ have
--     been created with
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::'Graphics.Vulkan.Core10.Handles.Image'
--     equal to 'Graphics.Vulkan.Core10.Handles.Image'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dedicatedAllocationImageAliasing dedicated allocation image aliasing>
--     feature is not enabled, and the
--     'Graphics.Vulkan.Core10.Memory.MemoryAllocateInfo' provided when
--     @memory@ was allocated included a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'
--     structure in its @pNext@ chain, and
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::'Graphics.Vulkan.Core10.Handles.Image'
--     was not 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', then
--     'Graphics.Vulkan.Core10.Handles.Image' /must/ equal
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::'Graphics.Vulkan.Core10.Handles.Image'
--     and @memoryOffset@ /must/ be zero.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dedicatedAllocationImageAliasing dedicated allocation image aliasing>
--     feature is enabled, and the
--     'Graphics.Vulkan.Core10.Memory.MemoryAllocateInfo' provided when
--     @memory@ was allocated included a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'
--     structure in its @pNext@ chain, and
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::'Graphics.Vulkan.Core10.Handles.Image'
--     was not 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', then
--     @memoryOffset@ /must/ be zero, and
--     'Graphics.Vulkan.Core10.Handles.Image' /must/ be either equal to
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::'Graphics.Vulkan.Core10.Handles.Image'
--     or an image that was created using the same parameters in
--     'Graphics.Vulkan.Core10.Image.ImageCreateInfo', with the exception
--     that @extent@ and @arrayLayers@ /may/ differ subject to the
--     following restrictions: every dimension in the @extent@ parameter of
--     the image being bound /must/ be equal to or smaller than the
--     original image for which the allocation was created; and the
--     @arrayLayers@ parameter of the image being bound /must/ be equal to
--     or smaller than the original image for which the allocation was
--     created.
--
-- -   If image was created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT'
--     bit set, the image /must/ be bound to a memory object allocated with
--     a memory type that reports
--     'Graphics.Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   If image was created with the
--     'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT'
--     bit not set, the image /must/ not be bound to a memory object
--     created with a memory type that reports
--     'Graphics.Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   If 'Graphics.Vulkan.Core10.Handles.Image' was created with
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationImageCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Graphics.Vulkan.Core10.BaseType.TRUE', @memory@ /must/
--     have been created with
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationMemoryAllocateInfoNV'::'Graphics.Vulkan.Core10.Handles.Image'
--     equal to an image handle created with identical creation parameters
--     to 'Graphics.Vulkan.Core10.Handles.Image' and @memoryOffset@ /must/
--     be zero
--
-- -   If the value of
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     used to allocate @memory@ is not @0@, it /must/ include at least one
--     of the handles set in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--     when 'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- -   If @memory@ was created by a memory import operation, the external
--     handle type of the imported memory /must/ also have been set in
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--     when 'Graphics.Vulkan.Core10.Handles.Image' was created
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Image' handle
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.DeviceMemory' handle
--
-- -   'Graphics.Vulkan.Core10.Handles.Image' /must/ have been created,
--     allocated, or retrieved from 'Graphics.Vulkan.Core10.Handles.Device'
--
-- -   @memory@ /must/ have been created, allocated, or retrieved from
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to 'Graphics.Vulkan.Core10.Handles.Image' /must/ be
--     externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.DeviceMemory',
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core10.Handles.Image'
bindImageMemory :: Device -> Image -> DeviceMemory -> ("memoryOffset" ::: DeviceSize) -> IO ()
bindImageMemory device image memory memoryOffset = do
  let vkBindImageMemory' = mkVkBindImageMemory (pVkBindImageMemory (deviceCmds (device :: Device)))
  r <- vkBindImageMemory' (deviceHandle (device)) (image) (memory) (memoryOffset)
  when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkMemoryRequirements - Structure specifying memory requirements
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.DeviceSize',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2',
-- 'getBufferMemoryRequirements', 'getImageMemoryRequirements'
data MemoryRequirements = MemoryRequirements
  { -- | @size@ is the size, in bytes, of the memory allocation /required/ for
    -- the resource.
    size :: DeviceSize
  , -- | @alignment@ is the alignment, in bytes, of the offset within the
    -- allocation /required/ for the resource.
    alignment :: DeviceSize
  , -- | @memoryTypeBits@ is a bitmask and contains one bit set for every
    -- supported memory type for the resource. Bit @i@ is set if and only if
    -- the memory type @i@ in the
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceMemoryProperties'
    -- structure for the physical device is supported for the resource.
    memoryTypeBits :: Word32
  }
  deriving (Typeable)
deriving instance Show MemoryRequirements

instance ToCStruct MemoryRequirements where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p MemoryRequirements{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (size)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (alignment)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (memoryTypeBits)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct MemoryRequirements where
  peekCStruct p = do
    size <- peek @DeviceSize ((p `plusPtr` 0 :: Ptr DeviceSize))
    alignment <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    memoryTypeBits <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ MemoryRequirements
             size alignment memoryTypeBits

instance Storable MemoryRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero MemoryRequirements where
  zero = MemoryRequirements
           zero
           zero
           zero

