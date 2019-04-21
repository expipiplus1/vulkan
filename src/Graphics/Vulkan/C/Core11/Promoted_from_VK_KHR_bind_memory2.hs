{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  , FN_vkBindBufferMemory2
  , PFN_vkBindBufferMemory2
  , vkBindBufferMemory2
  , FN_vkBindImageMemory2
  , PFN_vkBindImageMemory2
  , vkBindImageMemory2
  , pattern VK_IMAGE_CREATE_ALIAS_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | VkBindBufferMemoryInfo - Structure specifying how to bind a buffer to
-- memory
--
-- == Valid Usage
--
-- -   @buffer@ /must/ not already be backed by a memory object
--
-- -   @buffer@ /must/ not have been created with any sparse memory binding
--     flags
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
--     with @buffer@
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
--     with @buffer@
--
-- -   The @size@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
--     with @buffer@ /must/ be less than or equal to the size of @memory@
--     minus @memoryOffset@
--
-- -   If @buffer@ requires a dedicated allocation(as reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2'
--     in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for @buffer@), @memory@ /must/ have been created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@
--     equal to @buffer@ and @memoryOffset@ /must/ be zero
--
-- -   If the 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'
--     provided when @memory@ was allocated included an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     in its @pNext@ chain, and
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@
--     was not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', then
--     @buffer@ /must/ equal
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@
--     and @memoryOffset@ /must/ be zero.
--
-- -   If @buffer@ was created with
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @memory@ /must/
--     have been created with
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV'::@buffer@
--     equal to @buffer@ and @memoryOffset@ /must/ be zero
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindBufferMemoryDeviceGroupInfo',
--     all instances of @memory@ specified by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindBufferMemoryDeviceGroupInfo'::@pDeviceIndices@
--     /must/ have been allocated
--
-- Unresolved directive in VkBindBufferMemoryInfo.txt -
-- include::{generated}\/validity\/structs\/VkBindBufferMemoryInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkBindBufferMemory2'
data VkBindBufferMemoryInfo = VkBindBufferMemoryInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @buffer@ is the buffer to be attached to memory.
  vkBuffer :: VkBuffer
  , -- | @memory@ is a 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
  -- describing the device memory to attach.
  vkMemory :: VkDeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of @memory@ which is to
  -- be bound to the buffer. The number of bytes returned in the
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
  -- member in @memory@, starting from @memoryOffset@ bytes, will be bound to
  -- the specified buffer.
  vkMemoryOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBindBufferMemoryInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkBindBufferMemoryInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindBufferMemoryInfo))

instance Zero VkBindBufferMemoryInfo where
  zero = VkBindBufferMemoryInfo VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
                                zero
                                zero
                                zero
                                zero

-- | VkBindImageMemoryInfo - Structure specifying how to bind an image to
-- memory
--
-- == Valid Usage
--
-- -   @image@ /must/ not already be backed by a memory object
--
-- -   @image@ /must/ not have been created with any sparse memory binding
--     flags
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   If the @pNext@ chain does not include an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, @memory@ /must/ have been allocated using one of the
--     memory types allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with @image@
--
-- -   If the @pNext@ chain does not include an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, @memoryOffset@ /must/ be an integer multiple of the
--     @alignment@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with @image@
--
-- -   If the @pNext@ chain does not include an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, the difference of the size of @memory@ and @memoryOffset@
--     /must/ be greater than or equal to the @size@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with the same @image@
--
-- -   If the @pNext@ chain includes an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, @image@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_CREATE_DISJOINT_BIT'
--     bit set.
--
-- -   If the @pNext@ chain includes an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, @memory@ /must/ have been allocated using one of the
--     memory types allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with @image@ and the correct @planeAspect@ for this plane in the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     structure attached to the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2'’s
--     @pNext@ chain
--
-- -   If the @pNext@ chain includes an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, @memoryOffset@ /must/ be an integer multiple of the
--     @alignment@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with @image@ and the correct @planeAspect@ for this plane in the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     structure attached to the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2'’s
--     @pNext@ chain
--
-- -   If the @pNext@ chain includes an instance of the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, the difference of the size of @memory@ and @memoryOffset@
--     /must/ be greater than or equal to the @size@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with the same @image@ and the correct @planeAspect@ for this plane
--     in the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     structure attached to the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2'’s
--     @pNext@ chain
--
-- -   If @image@ requires a dedicated allocation (as reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for @image@), @memory@ /must/ have been created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     equal to @image@ and @memoryOffset@ /must/ be zero
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-dedicatedAllocationImageAliasing dedicated allocation image aliasing>
--     feature is not enabled, and the
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' provided when
--     @memory@ was allocated included an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     in its @pNext@ chain, and
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     was not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', then
--     @image@ /must/ equal
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     and @memoryOffset@ /must/ be zero.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-dedicatedAllocationImageAliasing dedicated allocation image aliasing>
--     feature is enabled, and the
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' provided when
--     @memory@ was allocated included an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     in its @pNext@ chain, and
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     was not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', then
--     @memoryOffset@ /must/ be zero, and @image@ /must/ be either equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     or an image that was created using the same parameters in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo', with the
--     exception that @extent@ and @arrayLayers@ /may/ differ subject to
--     the following restrictions: every dimension in the @extent@
--     parameter of the image being bound /must/ be equal to or smaller
--     than the original image for which the allocation was created; and
--     the @arrayLayers@ parameter of the image being bound /must/ be equal
--     to or smaller than the original image for which the allocation was
--     created.
--
-- -   If @image@ was created with
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationImageCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @memory@ /must/
--     have been created with
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV'::@image@
--     equal to @image@ and @memoryOffset@ /must/ be zero
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
--     all instances of @memory@ specified by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'::@pDeviceIndices@
--     /must/ have been allocated
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
--     and
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'::@splitInstanceBindRegionCount@
--     is not zero, then @image@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT'
--     bit set
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
--     all elements of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'::@pSplitInstanceBindRegions@
--     /must/ be valid rectangles contained within the dimensions of
--     @image@
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
--     the union of the areas of all elements of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'::@pSplitInstanceBindRegions@
--     that correspond to the same instance of @image@ /must/ cover the
--     entire image.
--
-- -   If @image@ was created with a valid swapchain handle in
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkImageSwapchainCreateInfoKHR'::@swapchain@,
--     then the @pNext@ chain /must/ include a valid instance of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkBindImageMemorySwapchainInfoKHR'
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkBindImageMemorySwapchainInfoKHR',
--     @memory@ /must/ be
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If the @pNext@ chain does not include an instance of
--     'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkBindImageMemorySwapchainInfoKHR',
--     @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
--
-- Unresolved directive in VkBindImageMemoryInfo.txt -
-- include::{generated}\/validity\/structs\/VkBindImageMemoryInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkBindImageMemory2'
data VkBindImageMemoryInfo = VkBindImageMemoryInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @image@ is the image to be attached to memory.
  vkImage :: VkImage
  , -- | @memory@ is a 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
  -- describing the device memory to attach.
  vkMemory :: VkDeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of @memory@ which is to
  -- be bound to the image. The number of bytes returned in the
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
  -- member in @memory@, starting from @memoryOffset@ bytes, will be bound to
  -- the specified image.
  vkMemoryOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBindImageMemoryInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkBindImageMemoryInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindImageMemoryInfo))

instance Zero VkBindImageMemoryInfo where
  zero = VkBindImageMemoryInfo VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
                               zero
                               zero
                               zero
                               zero

-- | vkBindBufferMemory2 - Bind device memory to buffer objects
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the buffers and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of structures of type
--     'VkBindBufferMemoryInfo', describing buffers and memory to bind.
--
-- = Description
--
-- On some implementations, it /may/ be more efficient to batch memory
-- bindings into a single command.
--
-- Unresolved directive in vkBindBufferMemory2.txt -
-- include::{generated}\/validity\/protos\/vkBindBufferMemory2.txt[]
--
-- = See Also
--
-- 'VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindBufferMemory2" vkBindBufferMemory2 :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
#else
vkBindBufferMemory2 :: DeviceCmds -> ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
vkBindBufferMemory2 deviceCmds = mkVkBindBufferMemory2 (pVkBindBufferMemory2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindBufferMemory2
  :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult) -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult)
#endif

type FN_vkBindBufferMemory2 = ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
type PFN_vkBindBufferMemory2 = FunPtr FN_vkBindBufferMemory2

-- | vkBindImageMemory2 - Bind device memory to image objects
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the images and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of structures of type
--     'VkBindImageMemoryInfo', describing images and memory to bind.
--
-- = Description
--
-- On some implementations, it /may/ be more efficient to batch memory
-- bindings into a single command.
--
-- Unresolved directive in vkBindImageMemory2.txt -
-- include::{generated}\/validity\/protos\/vkBindImageMemory2.txt[]
--
-- = See Also
--
-- 'VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindImageMemory2" vkBindImageMemory2 :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
#else
vkBindImageMemory2 :: DeviceCmds -> ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
vkBindImageMemory2 deviceCmds = mkVkBindImageMemory2 (pVkBindImageMemory2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindImageMemory2
  :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult) -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult)
#endif

type FN_vkBindImageMemory2 = ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
type PFN_vkBindImageMemory2 = FunPtr FN_vkBindImageMemory2

-- | 'VK_IMAGE_CREATE_ALIAS_BIT' specifies that two images created with the
-- same creation parameters and aliased to the same memory /can/ interpret
-- the contents of the memory consistently with each other, subject to the
-- rules described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-memory-aliasing Memory Aliasing>
-- section. This flag further specifies that each plane of a /disjoint/
-- image /can/ share an in-memory non-linear representation with
-- single-plane images, and that a single-plane image /can/ share an
-- in-memory non-linear representation with a plane of a multi-planar
-- disjoint image, according to the rules in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatible-planes>.
-- If the @pNext@ chain includes a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'
-- or
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV'
-- structure whose @handleTypes@ member is not @0@, it is as if
-- 'VK_IMAGE_CREATE_ALIAS_BIT' is set.
pattern VK_IMAGE_CREATE_ALIAS_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_ALIAS_BIT = VkImageCreateFlagBits 0x00000400

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO = VkStructureType 1000157000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO = VkStructureType 1000157001
