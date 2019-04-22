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
  , pattern BUFFER_CREATE_PROTECTED_BIT
  , pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
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
  , pattern BUFFER_USAGE_RESERVED_15_BIT_KHR
  , pattern BUFFER_USAGE_RESERVED_16_BIT_KHR
  , pattern BUFFER_USAGE_RESERVED_13_BIT_KHR
  , pattern BUFFER_USAGE_RESERVED_14_BIT_KHR
  , pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
  , pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
  , pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern BUFFER_USAGE_RAY_TRACING_BIT_NV
  , pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  , BufferUsageFlags
  , SharingMode
  , pattern SHARING_MODE_EXCLUSIVE
  , pattern SHARING_MODE_CONCURRENT
  , createBuffer
  , destroyBuffer
  , withBuffer
  , pattern VK_BUFFER_USAGE_RESERVED_13_BIT_KHR
  , pattern VK_BUFFER_USAGE_RESERVED_14_BIT_KHR
  , pattern VK_BUFFER_USAGE_RESERVED_15_BIT_KHR
  , pattern VK_BUFFER_USAGE_RESERVED_16_BIT_KHR
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
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( pattern VK_BUFFER_CREATE_PROTECTED_BIT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
  , pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV
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


{-# complete BUFFER_CREATE_SPARSE_BINDING_BIT, BUFFER_CREATE_SPARSE_RESIDENCY_BIT, BUFFER_CREATE_SPARSE_ALIASED_BIT, BUFFER_CREATE_PROTECTED_BIT, BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT :: BufferCreateFlagBits #-}


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


-- No documentation found for Nested "BufferCreateFlagBits" "BUFFER_CREATE_PROTECTED_BIT"
pattern BUFFER_CREATE_PROTECTED_BIT :: (a ~ BufferCreateFlagBits) => a
pattern BUFFER_CREATE_PROTECTED_BIT = VK_BUFFER_CREATE_PROTECTED_BIT


-- No documentation found for Nested "BufferCreateFlagBits" "BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT"
pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT :: (a ~ BufferCreateFlagBits) => a
pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT = VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT

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
--     than @pQueueFamilyPropertyCount@ returned by
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'
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
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkBufferDeviceAddressCreateInfoEXT',
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateFlagBits' values
--
-- -   @usage@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Buffer.VkBufferUsageFlagBits' values
--
-- -   @usage@ /must/ not be @0@
--
-- -   @sharingMode@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Buffer.VkSharingMode' value
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


{-# complete BUFFER_USAGE_TRANSFER_SRC_BIT, BUFFER_USAGE_TRANSFER_DST_BIT, BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT, BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT, BUFFER_USAGE_UNIFORM_BUFFER_BIT, BUFFER_USAGE_STORAGE_BUFFER_BIT, BUFFER_USAGE_INDEX_BUFFER_BIT, BUFFER_USAGE_VERTEX_BUFFER_BIT, BUFFER_USAGE_INDIRECT_BUFFER_BIT, BUFFER_USAGE_RESERVED_15_BIT_KHR, BUFFER_USAGE_RESERVED_16_BIT_KHR, BUFFER_USAGE_RESERVED_13_BIT_KHR, BUFFER_USAGE_RESERVED_14_BIT_KHR, BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT, BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT, BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT, BUFFER_USAGE_RAY_TRACING_BIT_NV, BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT :: BufferUsageFlagBits #-}


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
-- or
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect'.
pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT = VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_RESERVED_15_BIT_KHR"
pattern BUFFER_USAGE_RESERVED_15_BIT_KHR :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_RESERVED_15_BIT_KHR = VK_BUFFER_USAGE_RESERVED_15_BIT_KHR


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_RESERVED_16_BIT_KHR"
pattern BUFFER_USAGE_RESERVED_16_BIT_KHR :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_RESERVED_16_BIT_KHR = VK_BUFFER_USAGE_RESERVED_16_BIT_KHR


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_RESERVED_13_BIT_KHR"
pattern BUFFER_USAGE_RESERVED_13_BIT_KHR :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_RESERVED_13_BIT_KHR = VK_BUFFER_USAGE_RESERVED_13_BIT_KHR


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_RESERVED_14_BIT_KHR"
pattern BUFFER_USAGE_RESERVED_14_BIT_KHR :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_RESERVED_14_BIT_KHR = VK_BUFFER_USAGE_RESERVED_14_BIT_KHR


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT = VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT = VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT"
pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT = VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_RAY_TRACING_BIT_NV"
pattern BUFFER_USAGE_RAY_TRACING_BIT_NV :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_RAY_TRACING_BIT_NV = VK_BUFFER_USAGE_RAY_TRACING_BIT_NV


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT"
pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT = VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT

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
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkPhysicalDeviceImageDrmFormatModifierInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
type SharingMode = VkSharingMode


{-# complete SHARING_MODE_EXCLUSIVE, SHARING_MODE_CONCURRENT :: SharingMode #-}


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
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pBuffer@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VK_ERROR_INVALID_DEVICE_ADDRESS_EXT'
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
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @buffer@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   If @buffer@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @buffer@ /must/ be externally synchronized
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

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_RESERVED_13_BIT_KHR"
pattern VK_BUFFER_USAGE_RESERVED_13_BIT_KHR :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_RESERVED_13_BIT_KHR = VkBufferUsageFlagBits 0x00002000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_RESERVED_14_BIT_KHR"
pattern VK_BUFFER_USAGE_RESERVED_14_BIT_KHR :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_RESERVED_14_BIT_KHR = VkBufferUsageFlagBits 0x00004000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_RESERVED_15_BIT_KHR"
pattern VK_BUFFER_USAGE_RESERVED_15_BIT_KHR :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_RESERVED_15_BIT_KHR = VkBufferUsageFlagBits 0x00008000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_RESERVED_16_BIT_KHR"
pattern VK_BUFFER_USAGE_RESERVED_16_BIT_KHR :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_RESERVED_16_BIT_KHR = VkBufferUsageFlagBits 0x00010000
