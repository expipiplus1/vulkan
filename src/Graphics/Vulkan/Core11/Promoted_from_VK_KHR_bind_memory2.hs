{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( withCStructBindBufferMemoryInfo
  , fromCStructBindBufferMemoryInfo
  , BindBufferMemoryInfo(..)
  , withCStructBindImageMemoryInfo
  , fromCStructBindImageMemoryInfo
  , BindImageMemoryInfo(..)
  , bindBufferMemory2
  , bindImageMemory2
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern VK_IMAGE_CREATE_ALIAS_BIT
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  , vkBindBufferMemory2
  , vkBindImageMemory2
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_ALIAS_BIT
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
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindBufferMemory2'
data BindBufferMemoryInfo = BindBufferMemoryInfo
  { -- Univalued member elided
  -- No documentation found for Nested "BindBufferMemoryInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindBufferMemoryInfo" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "BindBufferMemoryInfo" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "BindBufferMemoryInfo" "memoryOffset"
  memoryOffset :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBindBufferMemoryInfo' and
-- marshal a 'BindBufferMemoryInfo' into it. The 'VkBindBufferMemoryInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBindBufferMemoryInfo :: BindBufferMemoryInfo -> (VkBindBufferMemoryInfo -> IO a) -> IO a
withCStructBindBufferMemoryInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BindBufferMemoryInfo)) (\pPNext -> cont (VkBindBufferMemoryInfo VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO pPNext (buffer (marshalled :: BindBufferMemoryInfo)) (memory (marshalled :: BindBufferMemoryInfo)) (memoryOffset (marshalled :: BindBufferMemoryInfo))))

-- | A function to read a 'VkBindBufferMemoryInfo' and all additional
-- structures in the pointer chain into a 'BindBufferMemoryInfo'.
fromCStructBindBufferMemoryInfo :: VkBindBufferMemoryInfo -> IO BindBufferMemoryInfo
fromCStructBindBufferMemoryInfo c = BindBufferMemoryInfo <$> -- Univalued Member elided
                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindBufferMemoryInfo)))
                                                         <*> pure (vkBuffer (c :: VkBindBufferMemoryInfo))
                                                         <*> pure (vkMemory (c :: VkBindBufferMemoryInfo))
                                                         <*> pure (vkMemoryOffset (c :: VkBindBufferMemoryInfo))

instance Zero BindBufferMemoryInfo where
  zero = BindBufferMemoryInfo Nothing
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
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2'
data BindImageMemoryInfo = BindImageMemoryInfo
  { -- Univalued member elided
  -- No documentation found for Nested "BindImageMemoryInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BindImageMemoryInfo" "image"
  image :: Image
  , -- No documentation found for Nested "BindImageMemoryInfo" "memory"
  memory :: DeviceMemory
  , -- No documentation found for Nested "BindImageMemoryInfo" "memoryOffset"
  memoryOffset :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBindImageMemoryInfo' and
-- marshal a 'BindImageMemoryInfo' into it. The 'VkBindImageMemoryInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBindImageMemoryInfo :: BindImageMemoryInfo -> (VkBindImageMemoryInfo -> IO a) -> IO a
withCStructBindImageMemoryInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BindImageMemoryInfo)) (\pPNext -> cont (VkBindImageMemoryInfo VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO pPNext (image (marshalled :: BindImageMemoryInfo)) (memory (marshalled :: BindImageMemoryInfo)) (memoryOffset (marshalled :: BindImageMemoryInfo))))

-- | A function to read a 'VkBindImageMemoryInfo' and all additional
-- structures in the pointer chain into a 'BindImageMemoryInfo'.
fromCStructBindImageMemoryInfo :: VkBindImageMemoryInfo -> IO BindImageMemoryInfo
fromCStructBindImageMemoryInfo c = BindImageMemoryInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindImageMemoryInfo)))
                                                       <*> pure (vkImage (c :: VkBindImageMemoryInfo))
                                                       <*> pure (vkMemory (c :: VkBindImageMemoryInfo))
                                                       <*> pure (vkMemoryOffset (c :: VkBindImageMemoryInfo))

instance Zero BindImageMemoryInfo where
  zero = BindImageMemoryInfo Nothing
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
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
--     describing buffers and memory to bind.
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
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
bindBufferMemory2 :: Device ->  Vector BindBufferMemoryInfo ->  IO ()
bindBufferMemory2 = \(Device device' commandTable) -> \bindInfos' -> withVec withCStructBindBufferMemoryInfo bindInfos' (\pBindInfos' -> vkBindBufferMemory2 commandTable device' (fromIntegral $ Data.Vector.length bindInfos') pBindInfos' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkBindImageMemory2 - Bind device memory to image objects
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the images and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of structures of type
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
--     describing images and memory to bind.
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
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
bindImageMemory2 :: Device ->  Vector BindImageMemoryInfo ->  IO ()
bindImageMemory2 = \(Device device' commandTable) -> \bindInfos' -> withVec withCStructBindImageMemoryInfo bindInfos' (\pBindInfos' -> vkBindImageMemory2 commandTable device' (fromIntegral $ Data.Vector.length bindInfos') pBindInfos' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))
