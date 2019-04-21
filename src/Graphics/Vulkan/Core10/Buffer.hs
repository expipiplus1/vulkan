{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Buffer
  ( BufferCreateFlagBits
  , pattern BUFFER_CREATE_SPARSE_BINDING_BIT
  , pattern BUFFER_CREATE_SPARSE_RESIDENCY_BIT
  , pattern BUFFER_CREATE_SPARSE_ALIASED_BIT
  , BufferCreateFlags
  , withCStructBufferCreateInfo
  , fromCStructBufferCreateInfo
  , BufferCreateInfo(..)
  , BufferUsageFlagBits
  , pattern BUFFER_USAGE_TRANSFER_SRC_BIT
  , pattern BUFFER_USAGE_TRANSFER_DST_BIT
  , pattern BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
  , pattern BUFFER_USAGE_UNIFORM_BUFFER_BIT
  , pattern BUFFER_USAGE_STORAGE_BUFFER_BIT
  , pattern BUFFER_USAGE_INDEX_BUFFER_BIT
  , pattern BUFFER_USAGE_VERTEX_BUFFER_BIT
  , pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT
  , BufferUsageFlags
  , SharingMode
  , pattern SHARING_MODE_EXCLUSIVE
  , pattern SHARING_MODE_CONCURRENT
  , createBuffer
  , destroyBuffer
  , withBuffer
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits(..)
  , VkBufferCreateInfo(..)
  , VkBufferUsageFlagBits(..)
  , VkSharingMode(..)
  , vkCreateBuffer
  , vkDestroyBuffer
  , pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
  , pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT
  , pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
  , pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT
  , pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT
  , pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
  , pattern VK_SHARING_MODE_CONCURRENT
  , pattern VK_SHARING_MODE_EXCLUSIVE
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
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


-- | VkBufferCreateFlagBits - Bitmask specifying additional parameters of a
-- buffer
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features Physical Device Features>
-- for details of the sparse memory features supported on a device.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlags'
type BufferCreateFlagBits = VkBufferCreateFlagBits


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_BINDING_BIT'
-- specifies that the buffer will be backed using sparse memory binding.
pattern BUFFER_CREATE_SPARSE_BINDING_BIT :: (a ~ BufferCreateFlagBits) => a
pattern BUFFER_CREATE_SPARSE_BINDING_BIT = VK_BUFFER_CREATE_SPARSE_BINDING_BIT


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
-- specifies that the buffer /can/ be partially backed using sparse memory
-- binding. Buffers created with this flag /must/ also be created with the
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_BINDING_BIT'
-- flag.
pattern BUFFER_CREATE_SPARSE_RESIDENCY_BIT :: (a ~ BufferCreateFlagBits) => a
pattern BUFFER_CREATE_SPARSE_RESIDENCY_BIT = VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_ALIASED_BIT'
-- specifies that the buffer will be backed using sparse memory binding
-- with memory ranges that might also simultaneously be backing another
-- buffer (or another portion of the same buffer). Buffers created with
-- this flag /must/ also be created with the
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_BINDING_BIT'
-- flag.
pattern BUFFER_CREATE_SPARSE_ALIASED_BIT :: (a ~ BufferCreateFlagBits) => a
pattern BUFFER_CREATE_SPARSE_ALIASED_BIT = VK_BUFFER_CREATE_SPARSE_ALIASED_BIT

-- | VkBufferCreateFlags - Bitmask of VkBufferCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlags' is a bitmask type
-- for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo'
type BufferCreateFlags = BufferCreateFlagBits


-- | VkBufferCreateInfo - Structure specifying the parameters of a newly
-- created buffer object
--
-- == Valid Usage
--
-- -   @size@ /must/ be greater than @0@
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT',
--     @pQueueFamilyIndices@ /must/ be a valid pointer to an array of
--     @queueFamilyIndexCount@ @uint32_t@ values
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT',
--     @queueFamilyIndexCount@ /must/ be greater than @1@
--
-- -   If @sharingMode@ is
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT', each
--     element of @pQueueFamilyIndices@ /must/ be unique and /must/ be less
--     than @pQueueFamilyPropertyCount@ returned by either
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2'
--     for the @physicalDevice@ that was used to create @device@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseBinding sparse bindings>
--     feature is not enabled, @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_BINDING_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseResidencyBuffer sparse buffer residency>
--     feature is not enabled, @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-sparseResidencyAliased sparse aliased residency>
--     feature is not enabled, @flags@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_ALIASED_BIT'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_ALIASED_BIT',
--     it /must/ also contain
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_BINDING_BIT'
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo',
--     its @handleTypes@ member /must/ only contain bits that are also in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'::@externalMemoryProperties.compatibleHandleTypes@,
--     as returned by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferProperties'
--     with @pExternalBufferInfo@->@handleType@ equal to any one of the
--     handle types specified in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo'::@handleTypes@
--
-- -   If the @pNext@ chain contains an instance of
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV',
--     and the @dedicatedAllocation@ member of the chained structure is
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE', then @flags@ /must/ not
--     include
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_ALIASED_BIT'
--
-- -   If
--     'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkBufferDeviceAddressCreateInfoEXT'::@deviceAddress@
--     is not zero, @flags@ /must/ include
--     'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT'
--
-- -   If @flags@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-bufferDeviceAddressCaptureReplay bufferDeviceAddressCaptureReplay>
--     feature /must/ be enabled
--
-- -   If @usage@ includes
--     'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT',
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-bufferDeviceAddress bufferDeviceAddress>
--     feature /must/ be enabled
--
-- Unresolved directive in VkBufferCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkBufferCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkSharingMode',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkCreateBuffer'
data BufferCreateInfo = BufferCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "BufferCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferCreateInfo" "flags"
  flags :: BufferCreateFlags
  , -- No documentation found for Nested "BufferCreateInfo" "size"
  size :: DeviceSize
  , -- No documentation found for Nested "BufferCreateInfo" "usage"
  usage :: BufferUsageFlags
  , -- No documentation found for Nested "BufferCreateInfo" "sharingMode"
  sharingMode :: SharingMode
  -- Length valued member elided
  , -- No documentation found for Nested "BufferCreateInfo" "pQueueFamilyIndices"
  queueFamilyIndices :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBufferCreateInfo' and
-- marshal a 'BufferCreateInfo' into it. The 'VkBufferCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBufferCreateInfo :: BufferCreateInfo -> (VkBufferCreateInfo -> IO a) -> IO a
withCStructBufferCreateInfo marshalled cont = withVec (&) (queueFamilyIndices (marshalled :: BufferCreateInfo)) (\pPQueueFamilyIndices -> maybeWith withSomeVkStruct (next (marshalled :: BufferCreateInfo)) (\pPNext -> cont (VkBufferCreateInfo VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO pPNext (flags (marshalled :: BufferCreateInfo)) (size (marshalled :: BufferCreateInfo)) (usage (marshalled :: BufferCreateInfo)) (sharingMode (marshalled :: BufferCreateInfo)) (fromIntegral (Data.Vector.length (queueFamilyIndices (marshalled :: BufferCreateInfo)))) pPQueueFamilyIndices)))

-- | A function to read a 'VkBufferCreateInfo' and all additional
-- structures in the pointer chain into a 'BufferCreateInfo'.
fromCStructBufferCreateInfo :: VkBufferCreateInfo -> IO BufferCreateInfo
fromCStructBufferCreateInfo c = BufferCreateInfo <$> -- Univalued Member elided
                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferCreateInfo)))
                                                 <*> pure (vkFlags (c :: VkBufferCreateInfo))
                                                 <*> pure (vkSize (c :: VkBufferCreateInfo))
                                                 <*> pure (vkUsage (c :: VkBufferCreateInfo))
                                                 <*> pure (vkSharingMode (c :: VkBufferCreateInfo))
                                                 -- Length valued member elided
                                                 <*> (Data.Vector.generateM (fromIntegral (vkQueueFamilyIndexCount (c :: VkBufferCreateInfo))) (peekElemOff (vkPQueueFamilyIndices (c :: VkBufferCreateInfo))))

instance Zero BufferCreateInfo where
  zero = BufferCreateInfo Nothing
                          zero
                          zero
                          zero
                          zero
                          Data.Vector.empty


-- | VkBufferUsageFlagBits - Bitmask specifying allowed usage of a buffer
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlags'
type BufferUsageFlagBits = VkBufferUsageFlagBits


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_SRC_BIT'
-- specifies that the buffer /can/ be used as the source of a /transfer
-- command/ (see the definition of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-transfer >).
pattern BUFFER_USAGE_TRANSFER_SRC_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_TRANSFER_SRC_BIT = VK_BUFFER_USAGE_TRANSFER_SRC_BIT


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_DST_BIT'
-- specifies that the buffer /can/ be used as the destination of a transfer
-- command.
pattern BUFFER_USAGE_TRANSFER_DST_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_TRANSFER_DST_BIT = VK_BUFFER_USAGE_TRANSFER_DST_BIT


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
-- specifies that the buffer /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferView' suitable for
-- occupying a 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet'
-- slot of type
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'.
pattern BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
-- specifies that the buffer /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferView' suitable for
-- occupying a 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet'
-- slot of type
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'.
pattern BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT'
-- specifies that the buffer /can/ be used in a
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo' suitable
-- for occupying a 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet'
-- slot either of type
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
-- or
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'.
pattern BUFFER_USAGE_UNIFORM_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_UNIFORM_BUFFER_BIT = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_STORAGE_BUFFER_BIT'
-- specifies that the buffer /can/ be used in a
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo' suitable
-- for occupying a 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet'
-- slot either of type
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
-- or
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'.
pattern BUFFER_USAGE_STORAGE_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_STORAGE_BUFFER_BIT = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDEX_BUFFER_BIT'
-- specifies that the buffer is suitable for passing as the @buffer@
-- parameter to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer'.
pattern BUFFER_USAGE_INDEX_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_INDEX_BUFFER_BIT = VK_BUFFER_USAGE_INDEX_BUFFER_BIT


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT'
-- specifies that the buffer is suitable for passing as an element of the
-- @pBuffers@ array to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers'.
pattern BUFFER_USAGE_VERTEX_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_VERTEX_BUFFER_BIT = VK_BUFFER_USAGE_VERTEX_BUFFER_BIT


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
-- specifies that the buffer is suitable for passing as the @buffer@
-- parameter to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectCountNV',
-- or
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect'.
-- It is also suitable for passing as the @buffer@ member of
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsTokenNVX',
-- or @sequencesCountBuffer@ or @sequencesIndexBuffer@ member of
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX'
pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT = VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT

-- | VkBufferUsageFlags - Bitmask of VkBufferUsageFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlags' is a bitmask type
-- for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo'
type BufferUsageFlags = BufferUsageFlagBits

-- | VkSharingMode - Buffer and image sharing modes
--
-- = Description
--
-- __Note__
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT' /may/
-- result in lower performance access to the buffer or image than
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE'.
--
-- Ranges of buffers and image subresources of image objects created using
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE' /must/ only
-- be accessed by queues in the queue family that has /ownership/ of the
-- resource. Upon creation, such resources are not owned by any queue
-- family; ownership is implicitly acquired upon first use within a queue.
-- Once a resource using
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE' is owned by
-- some queue family, the application /must/ perform a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-queue-transfers queue family ownership transfer>
-- to make the memory contents of a range or image subresource accessible
-- to a different queue family.
--
-- __Note__
--
-- Images still require a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-layouts layout transition>
-- from 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED' or
-- 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_PREINITIALIZED' before
-- being used on the first queue.
--
-- A queue family /can/ take ownership of an image subresource or buffer
-- range of a resource created with
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE', without an
-- ownership transfer, in the same way as for a resource that was just
-- created; however, taking ownership in this way has the effect that the
-- contents of the image subresource or buffer range are undefined.
--
-- Ranges of buffers and image subresources of image objects created using
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT' /must/ only
-- be accessed by queues from the queue families specified through the
-- @queueFamilyIndexCount@ and @pQueueFamilyIndices@ members of the
-- corresponding create info structures.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'
type SharingMode = VkSharingMode


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE' specifies
-- that access to any range or image subresource of the object will be
-- exclusive to a single queue family at a time.
pattern SHARING_MODE_EXCLUSIVE :: (a ~ SharingMode) => a
pattern SHARING_MODE_EXCLUSIVE = VK_SHARING_MODE_EXCLUSIVE


-- | 'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT' specifies
-- that concurrent access to any range or image subresource of the object
-- from multiple queue families is supported.
pattern SHARING_MODE_CONCURRENT :: (a ~ SharingMode) => a
pattern SHARING_MODE_CONCURRENT = VK_SHARING_MODE_CONCURRENT


-- | vkCreateBuffer - Create a new buffer object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the buffer object.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo' structure
--     containing parameters affecting creation of the buffer.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pBuffer@ points to a
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle in which
--     the resulting buffer object is returned.
--
-- == Valid Usage
--
-- -   If the @flags@ member of @pCreateInfo@ includes
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_BINDING_BIT',
--     creating this 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer'
--     /must/ not cause the total required sparse memory for all currently
--     valid sparse resources on the device to exceed
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@sparseAddressSpaceSize@
--
-- Unresolved directive in vkCreateBuffer.txt -
-- include::{generated}\/validity\/protos\/vkCreateBuffer.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
createBuffer :: Device ->  BufferCreateInfo ->  Maybe AllocationCallbacks ->  IO (Buffer)
createBuffer = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pBuffer' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructBufferCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateBuffer commandTable device' pCreateInfo' pAllocator pBuffer' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pBuffer')))))


-- | vkDestroyBuffer - Destroy a buffer object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the buffer.
--
-- -   @buffer@ is the buffer to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @buffer@, either directly or
--     via a 'Graphics.Vulkan.C.Core10.BufferView.VkBufferView', /must/
--     have completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @buffer@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @buffer@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroyBuffer.txt -
-- include::{generated}\/validity\/protos\/vkDestroyBuffer.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
destroyBuffer :: Device ->  Buffer ->  Maybe AllocationCallbacks ->  IO ()
destroyBuffer = \(Device device' commandTable) -> \buffer' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyBuffer commandTable device' buffer' pAllocator *> (pure ()))

-- | A safe wrapper for 'createBuffer' and 'destroyBuffer' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withBuffer
  :: Device -> BufferCreateInfo -> Maybe (AllocationCallbacks) -> (Buffer -> IO a) -> IO a
withBuffer device bufferCreateInfo allocationCallbacks = bracket
  (createBuffer device bufferCreateInfo allocationCallbacks)
  (\o -> destroyBuffer device o allocationCallbacks)
