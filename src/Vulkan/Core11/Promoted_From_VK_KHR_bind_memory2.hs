{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_bind_memory2"
module Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2  ( bindBufferMemory2
                                                        , bindImageMemory2
                                                        , BindBufferMemoryInfo(..)
                                                        , BindImageMemoryInfo(..)
                                                        , StructureType(..)
                                                        , ImageCreateFlagBits(..)
                                                        , ImageCreateFlags
                                                        ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindBufferMemoryDeviceGroupInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindImageMemoryDeviceGroupInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (BindImageMemorySwapchainInfoKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (BindImagePlaneMemoryInfo)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkBindBufferMemory2))
import Vulkan.Dynamic (DeviceCmds(pVkBindImageMemory2))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Handles (Image)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlagBits(..))
import Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindBufferMemory2
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct BindBufferMemoryInfo) -> IO Result) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct BindBufferMemoryInfo) -> IO Result

-- | vkBindBufferMemory2 - Bind device memory to buffer objects
--
-- = Description
--
-- On some implementations, it /may/ be more efficient to batch memory
-- bindings into a single command.
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
-- 'BindBufferMemoryInfo', 'Vulkan.Core10.Handles.Device'
bindBufferMemory2 :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the logical device that owns the buffers and memory.
                     --
                     -- #VUID-vkBindBufferMemory2-device-parameter# @device@ /must/ be a valid
                     -- 'Vulkan.Core10.Handles.Device' handle
                     Device
                  -> -- | @pBindInfos@ is a pointer to an array of @bindInfoCount@
                     -- 'BindBufferMemoryInfo' structures describing buffers and memory to bind.
                     --
                     -- #VUID-vkBindBufferMemory2-pBindInfos-parameter# @pBindInfos@ /must/ be a
                     -- valid pointer to an array of @bindInfoCount@ valid
                     -- 'BindBufferMemoryInfo' structures
                     ("bindInfos" ::: Vector (SomeStruct BindBufferMemoryInfo))
                  -> io ()
bindBufferMemory2 device bindInfos = liftIO . evalContT $ do
  let vkBindBufferMemory2Ptr = pVkBindBufferMemory2 (deviceCmds (device :: Device))
  lift $ unless (vkBindBufferMemory2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindBufferMemory2 is null" Nothing Nothing
  let vkBindBufferMemory2' = mkVkBindBufferMemory2 vkBindBufferMemory2Ptr
  pPBindInfos <- ContT $ allocaBytesAligned @(BindBufferMemoryInfo _) ((Data.Vector.length (bindInfos)) * 40) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPBindInfos `plusPtr` (40 * (i)) :: Ptr (BindBufferMemoryInfo _))) (e) . ($ ())) (bindInfos)
  r <- lift $ traceAroundEvent "vkBindBufferMemory2" (vkBindBufferMemory2' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (bindInfos)) :: Word32)) (forgetExtensions (pPBindInfos)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindImageMemory2
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr (SomeStruct BindImageMemoryInfo) -> IO Result) -> Ptr Device_T -> Word32 -> Ptr (SomeStruct BindImageMemoryInfo) -> IO Result

-- | vkBindImageMemory2 - Bind device memory to image objects
--
-- = Description
--
-- On some implementations, it /may/ be more efficient to batch memory
-- bindings into a single command.
--
-- == Valid Usage
--
-- -   #VUID-vkBindImageMemory2-pBindInfos-02858# If any
--     'BindImageMemoryInfo'::image was created with
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     then all planes of 'BindImageMemoryInfo'::image /must/ be bound
--     individually in separate @pBindInfos@
--
-- -   #VUID-vkBindImageMemory2-pBindInfos-04006# @pBindInfos@ /must/ not
--     refer to the same image subresource more than once
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkBindImageMemory2-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkBindImageMemory2-pBindInfos-parameter# @pBindInfos@ /must/
--     be a valid pointer to an array of @bindInfoCount@ valid
--     'BindImageMemoryInfo' structures
--
-- -   #VUID-vkBindImageMemory2-bindInfoCount-arraylength# @bindInfoCount@
--     /must/ be greater than @0@
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
-- = See Also
--
-- 'BindImageMemoryInfo', 'Vulkan.Core10.Handles.Device'
bindImageMemory2 :: forall io
                  . (MonadIO io)
                 => -- | @device@ is the logical device that owns the images and memory.
                    Device
                 -> -- | @pBindInfos@ is a pointer to an array of 'BindImageMemoryInfo'
                    -- structures, describing images and memory to bind.
                    ("bindInfos" ::: Vector (SomeStruct BindImageMemoryInfo))
                 -> io ()
bindImageMemory2 device bindInfos = liftIO . evalContT $ do
  let vkBindImageMemory2Ptr = pVkBindImageMemory2 (deviceCmds (device :: Device))
  lift $ unless (vkBindImageMemory2Ptr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindImageMemory2 is null" Nothing Nothing
  let vkBindImageMemory2' = mkVkBindImageMemory2 vkBindImageMemory2Ptr
  pPBindInfos <- ContT $ allocaBytesAligned @(BindImageMemoryInfo _) ((Data.Vector.length (bindInfos)) * 40) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPBindInfos `plusPtr` (40 * (i)) :: Ptr (BindImageMemoryInfo _))) (e) . ($ ())) (bindInfos)
  r <- lift $ traceAroundEvent "vkBindImageMemory2" (vkBindImageMemory2' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (bindInfos)) :: Word32)) (forgetExtensions (pPBindInfos)))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkBindBufferMemoryInfo - Structure specifying how to bind a buffer to
-- memory
--
-- == Valid Usage
--
-- -   #VUID-VkBindBufferMemoryInfo-buffer-01029# @buffer@ /must/ not
--     already be backed by a memory object
--
-- -   #VUID-VkBindBufferMemoryInfo-buffer-01030# @buffer@ /must/ not have
--     been created with any sparse memory binding flags
--
-- -   #VUID-VkBindBufferMemoryInfo-memoryOffset-01031# @memoryOffset@
--     /must/ be less than the size of @memory@
--
-- -   #VUID-VkBindBufferMemoryInfo-memory-01035# @memory@ /must/ have been
--     allocated using one of the memory types allowed in the
--     @memoryTypeBits@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'Vulkan.Core10.MemoryManagement.getBufferMemoryRequirements' with
--     @buffer@
--
-- -   #VUID-VkBindBufferMemoryInfo-memoryOffset-01036# @memoryOffset@
--     /must/ be an integer multiple of the @alignment@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'Vulkan.Core10.MemoryManagement.getBufferMemoryRequirements' with
--     @buffer@
--
-- -   #VUID-VkBindBufferMemoryInfo-size-01037# The @size@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'Vulkan.Core10.MemoryManagement.getBufferMemoryRequirements' with
--     @buffer@ /must/ be less than or equal to the size of @memory@ minus
--     @memoryOffset@
--
-- -   #VUID-VkBindBufferMemoryInfo-buffer-01444# If @buffer@ requires a
--     dedicated allocation(as reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2'
--     in
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for @buffer@), @memory@ /must/ have been created with
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::@buffer@
--     equal to @buffer@
--
-- -   #VUID-VkBindBufferMemoryInfo-memory-01508# If the
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' provided when @memory@ was
--     allocated included a
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'
--     structure in its @pNext@ chain, and
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::@buffer@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then @buffer@
--     /must/ equal
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::@buffer@,
--     and @memoryOffset@ /must/ be zero
--
-- -   #VUID-VkBindBufferMemoryInfo-None-01898# If buffer was created with
--     the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     bit set, the buffer /must/ be bound to a memory object allocated
--     with a memory type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   #VUID-VkBindBufferMemoryInfo-None-01899# If buffer was created with
--     the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     bit not set, the buffer /must/ not be bound to a memory object
--     created with a memory type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   #VUID-VkBindBufferMemoryInfo-buffer-01038# If @buffer@ was created
--     with
--     'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationBufferCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Vulkan.Core10.FundamentalTypes.TRUE', @memory@ /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationMemoryAllocateInfoNV'::@buffer@
--     equal to a buffer handle created with identical creation parameters
--     to @buffer@ and @memoryOffset@ /must/ be zero
--
-- -   #VUID-VkBindBufferMemoryInfo-memory-02726# If the value of
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     used to allocate @memory@ is not @0@, it /must/ include at least one
--     of the handles set in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'::@handleTypes@
--     when @buffer@ was created
--
-- -   #VUID-VkBindBufferMemoryInfo-memory-02985# If @memory@ was created
--     by a memory import operation, that is not
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ImportAndroidHardwareBufferInfoANDROID'
--     with a non-@NULL@ @buffer@ value, the external handle type of the
--     imported memory /must/ also have been set in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'::@handleTypes@
--     when @buffer@ was created
--
-- -   #VUID-VkBindBufferMemoryInfo-memory-02986# If @memory@ was created
--     with the
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ImportAndroidHardwareBufferInfoANDROID'
--     memory import operation with a non-@NULL@ @buffer@ value,
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     /must/ also have been set in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo'::@handleTypes@
--     when @buffer@ was created
--
-- -   #VUID-VkBindBufferMemoryInfo-bufferDeviceAddress-03339# If the
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures'::@bufferDeviceAddress@
--     feature is enabled and @buffer@ was created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT'
--     bit set, @memory@ /must/ have been allocated with the
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT'
--     bit set
--
-- -   #VUID-VkBindBufferMemoryInfo-pNext-01605# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindBufferMemoryDeviceGroupInfo'
--     structure, all instances of @memory@ specified by
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindBufferMemoryDeviceGroupInfo'::@pDeviceIndices@
--     /must/ have been allocated
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindBufferMemoryInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO'
--
-- -   #VUID-VkBindBufferMemoryInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindBufferMemoryDeviceGroupInfo'
--
-- -   #VUID-VkBindBufferMemoryInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkBindBufferMemoryInfo-buffer-parameter# @buffer@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkBindBufferMemoryInfo-memory-parameter# @memory@ /must/ be a
--     valid 'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- -   #VUID-VkBindBufferMemoryInfo-commonparent# Both of @buffer@, and
--     @memory@ /must/ have been created, allocated, or retrieved from the
--     same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'bindBufferMemory2',
-- 'Vulkan.Extensions.VK_KHR_bind_memory2.bindBufferMemory2KHR'
data BindBufferMemoryInfo (es :: [Type]) = BindBufferMemoryInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @buffer@ is the buffer to be attached to memory.
    buffer :: Buffer
  , -- | @memory@ is a 'Vulkan.Core10.Handles.DeviceMemory' object describing the
    -- device memory to attach.
    memory :: DeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of @memory@ which is to
    -- be bound to the buffer. The number of bytes returned in the
    -- 'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ member in
    -- @memory@, starting from @memoryOffset@ bytes, will be bound to the
    -- specified buffer.
    memoryOffset :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindBufferMemoryInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BindBufferMemoryInfo es)

instance Extensible BindBufferMemoryInfo where
  extensibleTypeName = "BindBufferMemoryInfo"
  setNext x next = x{next = next}
  getNext BindBufferMemoryInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BindBufferMemoryInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @BindBufferMemoryDeviceGroupInfo = Just f
    | otherwise = Nothing

instance (Extendss BindBufferMemoryInfo es, PokeChain es) => ToCStruct (BindBufferMemoryInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindBufferMemoryInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (buffer)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (memoryOffset)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Buffer)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ f

instance (Extendss BindBufferMemoryInfo es, PeekChain es) => FromCStruct (BindBufferMemoryInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    buffer <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ BindBufferMemoryInfo
             next buffer memory memoryOffset

instance es ~ '[] => Zero (BindBufferMemoryInfo es) where
  zero = BindBufferMemoryInfo
           ()
           zero
           zero
           zero


-- | VkBindImageMemoryInfo - Structure specifying how to bind an image to
-- memory
--
-- == Valid Usage
--
-- -   #VUID-VkBindImageMemoryInfo-image-01044# @image@ /must/ not already
--     be backed by a memory object
--
-- -   #VUID-VkBindImageMemoryInfo-image-01045# @image@ /must/ not have
--     been created with any sparse memory binding flags
--
-- -   #VUID-VkBindImageMemoryInfo-memoryOffset-01046# @memoryOffset@
--     /must/ be less than the size of @memory@
--
-- -   #VUID-VkBindImageMemoryInfo-image-01445# If @image@ requires a
--     dedicated allocation (as reported by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'
--     in
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for @image@), @memory@ /must/ have been created with
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::@image@
--     equal to @image@
--
-- -   #VUID-VkBindImageMemoryInfo-memory-02628# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dedicatedAllocationImageAliasing dedicated allocation image aliasing>
--     feature is not enabled, and the
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' provided when @memory@ was
--     allocated included a
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'
--     structure in its @pNext@ chain, and
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::@image@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then @image@
--     /must/ equal
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::@image@
--     and @memoryOffset@ /must/ be zero
--
-- -   #VUID-VkBindImageMemoryInfo-memory-02629# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dedicatedAllocationImageAliasing dedicated allocation image aliasing>
--     feature is enabled, and the
--     'Vulkan.Core10.Memory.MemoryAllocateInfo' provided when @memory@ was
--     allocated included a
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'
--     structure in its @pNext@ chain, and
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::@image@
--     was not 'Vulkan.Core10.APIConstants.NULL_HANDLE', then
--     @memoryOffset@ /must/ be zero, and @image@ /must/ be either equal to
--     'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo'::@image@
--     or an image that was created using the same parameters in
--     'Vulkan.Core10.Image.ImageCreateInfo', with the exception that
--     @extent@ and @arrayLayers@ /may/ differ subject to the following
--     restrictions: every dimension in the @extent@ parameter of the image
--     being bound /must/ be equal to or smaller than the original image
--     for which the allocation was created; and the @arrayLayers@
--     parameter of the image being bound /must/ be equal to or smaller
--     than the original image for which the allocation was created
--
-- -   #VUID-VkBindImageMemoryInfo-None-01901# If image was created with
--     the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT'
--     bit set, the image /must/ be bound to a memory object allocated with
--     a memory type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   #VUID-VkBindImageMemoryInfo-None-01902# If image was created with
--     the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT'
--     bit not set, the image /must/ not be bound to a memory object
--     created with a memory type that reports
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_PROTECTED_BIT'
--
-- -   #VUID-VkBindImageMemoryInfo-image-01050# If @image@ was created with
--     'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationImageCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Vulkan.Core10.FundamentalTypes.TRUE', @memory@ /must/ have
--     been created with
--     'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationMemoryAllocateInfoNV'::@image@
--     equal to an image handle created with identical creation parameters
--     to @image@ and @memoryOffset@ /must/ be zero
--
-- -   #VUID-VkBindImageMemoryInfo-memory-02728# If the value of
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo'::@handleTypes@
--     used to allocate @memory@ is not @0@, it /must/ include at least one
--     of the handles set in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--     when @image@ was created
--
-- -   #VUID-VkBindImageMemoryInfo-memory-02989# If @memory@ was created by
--     a memory import operation, that is not
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ImportAndroidHardwareBufferInfoANDROID'
--     with a non-@NULL@ @buffer@ value, the external handle type of the
--     imported memory /must/ also have been set in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--     when @image@ was created
--
-- -   #VUID-VkBindImageMemoryInfo-memory-02990# If @memory@ was created
--     with the
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ImportAndroidHardwareBufferInfoANDROID'
--     memory import operation with a non-@NULL@ @buffer@ value,
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     /must/ also have been set in
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'::@handleTypes@
--     when @image@ was created
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01615# If the @pNext@ chain does
--     not include a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'
--     structure, @memory@ /must/ have been allocated using one of the
--     memory types allowed in the @memoryTypeBits@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'
--     with @image@
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01616# If the @pNext@ chain does
--     not include a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'
--     structure, @memoryOffset@ /must/ be an integer multiple of the
--     @alignment@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'
--     with @image@
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01617# If the @pNext@ chain does
--     not include a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'
--     structure, the difference of the size of @memory@ and @memoryOffset@
--     /must/ be greater than or equal to the @size@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'
--     with the same @image@
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01618# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'
--     structure, @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     bit set
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01619# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'
--     structure, @memory@ /must/ have been allocated using one of the
--     memory types allowed in the @memoryTypeBits@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'
--     with @image@ and where
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'::@planeAspect@
--     corresponds to the
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo'::@planeAspect@
--     in the
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.ImageMemoryRequirementsInfo2'
--     structure’s @pNext@ chain
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01620# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'
--     structure, @memoryOffset@ /must/ be an integer multiple of the
--     @alignment@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'
--     with @image@ and where
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'::@planeAspect@
--     corresponds to the
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo'::@planeAspect@
--     in the
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.ImageMemoryRequirementsInfo2'
--     structure’s @pNext@ chain
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01621# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'
--     structure, the difference of the size of @memory@ and @memoryOffset@
--     /must/ be greater than or equal to the @size@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2'
--     with the same @image@ and where
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'::@planeAspect@
--     corresponds to the
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo'::@planeAspect@
--     in the
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.ImageMemoryRequirementsInfo2'
--     structure’s @pNext@ chain
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01626# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'
--     structure, all instances of @memory@ specified by
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'::@pDeviceIndices@
--     /must/ have been allocated
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01627# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'
--     structure, and
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'::@splitInstanceBindRegionCount@
--     is not zero, then @image@ /must/ have been created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT'
--     bit set
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01628# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'
--     structure, all elements of
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'::@pSplitInstanceBindRegions@
--     /must/ be valid rectangles contained within the dimensions of
--     @image@
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01629# If the @pNext@ chain
--     includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'
--     structure, the union of the areas of all elements of
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'::@pSplitInstanceBindRegions@
--     that correspond to the same instance of @image@ /must/ cover the
--     entire image
--
-- -   #VUID-VkBindImageMemoryInfo-image-01630# If @image@ was created with
--     a valid swapchain handle in
--     'Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR'::@swapchain@,
--     then the @pNext@ chain /must/ include a
--     'Vulkan.Extensions.VK_KHR_swapchain.BindImageMemorySwapchainInfoKHR'
--     structure containing the same swapchain handle
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01631# If the @pNext@ chain
--     includes a
--     'Vulkan.Extensions.VK_KHR_swapchain.BindImageMemorySwapchainInfoKHR'
--     structure, @memory@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-01632# If the @pNext@ chain does
--     not include a
--     'Vulkan.Extensions.VK_KHR_swapchain.BindImageMemorySwapchainInfoKHR'
--     structure, @memory@ /must/ be a valid
--     'Vulkan.Core10.Handles.DeviceMemory' handle
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindImageMemoryInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO'
--
-- -   #VUID-VkBindImageMemoryInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo',
--     'Vulkan.Extensions.VK_KHR_swapchain.BindImageMemorySwapchainInfoKHR',
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo'
--
-- -   #VUID-VkBindImageMemoryInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkBindImageMemoryInfo-image-parameter# @image@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-VkBindImageMemoryInfo-commonparent# Both of @image@, and
--     @memory@ that are valid handles of non-ignored parameters /must/
--     have been created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Handles.Image',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'bindImageMemory2',
-- 'Vulkan.Extensions.VK_KHR_bind_memory2.bindImageMemory2KHR'
data BindImageMemoryInfo (es :: [Type]) = BindImageMemoryInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @image@ is the image to be attached to memory.
    image :: Image
  , -- | @memory@ is a 'Vulkan.Core10.Handles.DeviceMemory' object describing the
    -- device memory to attach.
    memory :: DeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of @memory@ which is to
    -- be bound to the image. The number of bytes returned in the
    -- 'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ member in
    -- @memory@, starting from @memoryOffset@ bytes, will be bound to the
    -- specified image.
    memoryOffset :: DeviceSize
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindImageMemoryInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (BindImageMemoryInfo es)

instance Extensible BindImageMemoryInfo where
  extensibleTypeName = "BindImageMemoryInfo"
  setNext x next = x{next = next}
  getNext BindImageMemoryInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends BindImageMemoryInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @BindImagePlaneMemoryInfo = Just f
    | Just Refl <- eqT @e @BindImageMemorySwapchainInfoKHR = Just f
    | Just Refl <- eqT @e @BindImageMemoryDeviceGroupInfo = Just f
    | otherwise = Nothing

instance (Extendss BindImageMemoryInfo es, PokeChain es) => ToCStruct (BindImageMemoryInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindImageMemoryInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (image)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (memoryOffset)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr Image)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    lift $ f

instance (Extendss BindImageMemoryInfo es, PeekChain es) => FromCStruct (BindImageMemoryInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    image <- peek @Image ((p `plusPtr` 16 :: Ptr Image))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ BindImageMemoryInfo
             next image memory memoryOffset

instance es ~ '[] => Zero (BindImageMemoryInfo es) where
  zero = BindImageMemoryInfo
           ()
           zero
           zero
           zero

