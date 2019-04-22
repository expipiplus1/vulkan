{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( withCStructDeviceGroupBindSparseInfo
  , fromCStructDeviceGroupBindSparseInfo
  , DeviceGroupBindSparseInfo(..)
  , withCStructDeviceGroupCommandBufferBeginInfo
  , fromCStructDeviceGroupCommandBufferBeginInfo
  , DeviceGroupCommandBufferBeginInfo(..)
  , withCStructDeviceGroupRenderPassBeginInfo
  , fromCStructDeviceGroupRenderPassBeginInfo
  , DeviceGroupRenderPassBeginInfo(..)
  , withCStructDeviceGroupSubmitInfo
  , fromCStructDeviceGroupSubmitInfo
  , DeviceGroupSubmitInfo(..)
  , MemoryAllocateFlagBits
  , pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , MemoryAllocateFlagBitsKHR
  , MemoryAllocateFlags
  , withCStructMemoryAllocateFlagsInfo
  , fromCStructMemoryAllocateFlagsInfo
  , MemoryAllocateFlagsInfo(..)
  , MemoryAllocateFlagsKHR
  , PeerMemoryFeatureFlagBits
  , pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , PeerMemoryFeatureFlagBitsKHR
  , PeerMemoryFeatureFlags
  , PeerMemoryFeatureFlagsKHR
  , cmdDispatchBase
  , cmdSetDeviceMask
  , getDeviceGroupPeerMemoryFeatures
  , pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern PIPELINE_CREATE_DISPATCH_BASE
  , pattern DEPENDENCY_DEVICE_GROUP_BIT
  ) where

import Control.Monad
  ( (<=<)
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
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkDeviceGroupBindSparseInfo(..)
  , VkDeviceGroupCommandBufferBeginInfo(..)
  , VkDeviceGroupRenderPassBeginInfo(..)
  , VkDeviceGroupSubmitInfo(..)
  , VkMemoryAllocateFlagBits(..)
  , VkMemoryAllocateFlagsInfo(..)
  , VkPeerMemoryFeatureFlagBits(..)
  , vkCmdDispatchBase
  , vkCmdSetDeviceMask
  , vkGetDeviceGroupPeerMemoryFeatures
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , fromCStructRect2D
  , withCStructRect2D
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern DEPENDENCY_DEVICE_GROUP_BIT
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern PIPELINE_CREATE_DISPATCH_BASE
  , pattern PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  )



-- | VkDeviceGroupBindSparseInfo - Structure indicating which instances are
-- bound
--
-- = Description
--
-- These device indices apply to all buffer and image memory binds included
-- in the batch that points to this structure. The semaphore waits and
-- signals for the batch are executed only by the physical device specified
-- by the @resourceDeviceIndex@.
--
-- If this structure is not present, @resourceDeviceIndex@ and
-- @memoryDeviceIndex@ are assumed to be zero.
--
-- == Valid Usage
--
-- -   @resourceDeviceIndex@ and @memoryDeviceIndex@ /must/ both be valid
--     device indices.
--
-- -   Each memory allocation bound in this batch /must/ have allocated an
--     instance for @memoryDeviceIndex@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DeviceGroupBindSparseInfo = DeviceGroupBindSparseInfo
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceGroupBindSparseInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupBindSparseInfo" "resourceDeviceIndex"
  resourceDeviceIndex :: Word32
  , -- No documentation found for Nested "DeviceGroupBindSparseInfo" "memoryDeviceIndex"
  memoryDeviceIndex :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceGroupBindSparseInfo' and
-- marshal a 'DeviceGroupBindSparseInfo' into it. The 'VkDeviceGroupBindSparseInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceGroupBindSparseInfo :: DeviceGroupBindSparseInfo -> (VkDeviceGroupBindSparseInfo -> IO a) -> IO a
withCStructDeviceGroupBindSparseInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DeviceGroupBindSparseInfo)) (\pPNext -> cont (VkDeviceGroupBindSparseInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO pPNext (resourceDeviceIndex (marshalled :: DeviceGroupBindSparseInfo)) (memoryDeviceIndex (marshalled :: DeviceGroupBindSparseInfo))))

-- | A function to read a 'VkDeviceGroupBindSparseInfo' and all additional
-- structures in the pointer chain into a 'DeviceGroupBindSparseInfo'.
fromCStructDeviceGroupBindSparseInfo :: VkDeviceGroupBindSparseInfo -> IO DeviceGroupBindSparseInfo
fromCStructDeviceGroupBindSparseInfo c = DeviceGroupBindSparseInfo <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupBindSparseInfo)))
                                                                   <*> pure (vkResourceDeviceIndex (c :: VkDeviceGroupBindSparseInfo))
                                                                   <*> pure (vkMemoryDeviceIndex (c :: VkDeviceGroupBindSparseInfo))

instance Zero DeviceGroupBindSparseInfo where
  zero = DeviceGroupBindSparseInfo Nothing
                                   zero
                                   zero



-- | VkDeviceGroupCommandBufferBeginInfo - Set the initial device mask for a
-- command buffer
--
-- = Description
--
-- The initial device mask also acts as an upper bound on the set of
-- devices that /can/ ever be in the device mask in the command buffer.
--
-- If this structure is not present, the initial value of a command
-- buffer’s device mask is set to include all physical devices in the
-- logical device when the command buffer begins recording.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DeviceGroupCommandBufferBeginInfo = DeviceGroupCommandBufferBeginInfo
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceGroupCommandBufferBeginInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupCommandBufferBeginInfo" "deviceMask"
  deviceMask :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceGroupCommandBufferBeginInfo' and
-- marshal a 'DeviceGroupCommandBufferBeginInfo' into it. The 'VkDeviceGroupCommandBufferBeginInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceGroupCommandBufferBeginInfo :: DeviceGroupCommandBufferBeginInfo -> (VkDeviceGroupCommandBufferBeginInfo -> IO a) -> IO a
withCStructDeviceGroupCommandBufferBeginInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: DeviceGroupCommandBufferBeginInfo)) (\pPNext -> cont (VkDeviceGroupCommandBufferBeginInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO pPNext (deviceMask (marshalled :: DeviceGroupCommandBufferBeginInfo))))

-- | A function to read a 'VkDeviceGroupCommandBufferBeginInfo' and all additional
-- structures in the pointer chain into a 'DeviceGroupCommandBufferBeginInfo'.
fromCStructDeviceGroupCommandBufferBeginInfo :: VkDeviceGroupCommandBufferBeginInfo -> IO DeviceGroupCommandBufferBeginInfo
fromCStructDeviceGroupCommandBufferBeginInfo c = DeviceGroupCommandBufferBeginInfo <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupCommandBufferBeginInfo)))
                                                                                   <*> pure (vkDeviceMask (c :: VkDeviceGroupCommandBufferBeginInfo))

instance Zero DeviceGroupCommandBufferBeginInfo where
  zero = DeviceGroupCommandBufferBeginInfo Nothing
                                           zero



-- | VkDeviceGroupRenderPassBeginInfo - Set the initial device mask and
-- render areas for a render pass instance
--
-- = Description
--
-- The @deviceMask@ serves several purposes. It is an upper bound on the
-- set of physical devices that /can/ be used during the render pass
-- instance, and the initial device mask when the render pass instance
-- begins. In addition, commands transitioning to the next subpass in the
-- render pass instance and commands ending the render pass instance, and,
-- accordingly render pass attachment load, store, and resolve operations
-- and subpass dependencies corresponding to the render pass instance, are
-- executed on the physical devices included in the device mask provided
-- here.
--
-- If @deviceRenderAreaCount@ is not zero, then the elements of
-- @pDeviceRenderAreas@ override the value of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'::@renderArea@,
-- and provide a render area specific to each physical device. These render
-- areas serve the same purpose as
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'::@renderArea@,
-- including controlling the region of attachments that are cleared by
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR' and that are
-- resolved into resolve attachments.
--
-- If this structure is not present, the render pass instance’s device mask
-- is the value of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupCommandBufferBeginInfo'::@deviceMask@.
-- If this structure is not present or if @deviceRenderAreaCount@ is zero,
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'::@renderArea@
-- is used for all physical devices.
--
-- == Valid Usage
--
-- -   @deviceMask@ /must/ be a valid device mask value
--
-- -   @deviceMask@ /must/ not be zero
--
-- -   @deviceMask@ /must/ be a subset of the command buffer’s initial
--     device mask
--
-- -   @deviceRenderAreaCount@ /must/ either be zero or equal to the number
--     of physical devices in the logical device.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO'
--
-- -   If @deviceRenderAreaCount@ is not @0@, @pDeviceRenderAreas@ /must/
--     be a valid pointer to an array of @deviceRenderAreaCount@
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DeviceGroupRenderPassBeginInfo = DeviceGroupRenderPassBeginInfo
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceGroupRenderPassBeginInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupRenderPassBeginInfo" "deviceMask"
  deviceMask :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupRenderPassBeginInfo" "pDeviceRenderAreas"
  deviceRenderAreas :: Vector Rect2D
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceGroupRenderPassBeginInfo' and
-- marshal a 'DeviceGroupRenderPassBeginInfo' into it. The 'VkDeviceGroupRenderPassBeginInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceGroupRenderPassBeginInfo :: DeviceGroupRenderPassBeginInfo -> (VkDeviceGroupRenderPassBeginInfo -> IO a) -> IO a
withCStructDeviceGroupRenderPassBeginInfo marshalled cont = withVec withCStructRect2D (deviceRenderAreas (marshalled :: DeviceGroupRenderPassBeginInfo)) (\pPDeviceRenderAreas -> maybeWith withSomeVkStruct (next (marshalled :: DeviceGroupRenderPassBeginInfo)) (\pPNext -> cont (VkDeviceGroupRenderPassBeginInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO pPNext (deviceMask (marshalled :: DeviceGroupRenderPassBeginInfo)) (fromIntegral (Data.Vector.length (deviceRenderAreas (marshalled :: DeviceGroupRenderPassBeginInfo)))) pPDeviceRenderAreas)))

-- | A function to read a 'VkDeviceGroupRenderPassBeginInfo' and all additional
-- structures in the pointer chain into a 'DeviceGroupRenderPassBeginInfo'.
fromCStructDeviceGroupRenderPassBeginInfo :: VkDeviceGroupRenderPassBeginInfo -> IO DeviceGroupRenderPassBeginInfo
fromCStructDeviceGroupRenderPassBeginInfo c = DeviceGroupRenderPassBeginInfo <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupRenderPassBeginInfo)))
                                                                             <*> pure (vkDeviceMask (c :: VkDeviceGroupRenderPassBeginInfo))
                                                                             -- Length valued member elided
                                                                             <*> (Data.Vector.generateM (fromIntegral (vkDeviceRenderAreaCount (c :: VkDeviceGroupRenderPassBeginInfo))) (((fromCStructRect2D <=<) . peekElemOff) (vkPDeviceRenderAreas (c :: VkDeviceGroupRenderPassBeginInfo))))

instance Zero DeviceGroupRenderPassBeginInfo where
  zero = DeviceGroupRenderPassBeginInfo Nothing
                                        zero
                                        Data.Vector.empty



-- | VkDeviceGroupSubmitInfo - Structure indicating which physical devices
-- execute semaphore operations and command buffers
--
-- = Description
--
-- If this structure is not present, semaphore operations and command
-- buffers execute on device index zero.
--
-- == Valid Usage
--
-- -   @waitSemaphoreCount@ /must/ equal
--     'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo'::@waitSemaphoreCount@
--
-- -   @commandBufferCount@ /must/ equal
--     'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo'::@commandBufferCount@
--
-- -   @signalSemaphoreCount@ /must/ equal
--     'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo'::@signalSemaphoreCount@
--
-- -   All elements of @pWaitSemaphoreDeviceIndices@ and
--     @pSignalSemaphoreDeviceIndices@ /must/ be valid device indices
--
-- -   All elements of @pCommandBufferDeviceMasks@ /must/ be valid device
--     masks
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO'
--
-- -   If @waitSemaphoreCount@ is not @0@, @pWaitSemaphoreDeviceIndices@
--     /must/ be a valid pointer to an array of @waitSemaphoreCount@
--     @uint32_t@ values
--
-- -   If @commandBufferCount@ is not @0@, @pCommandBufferDeviceMasks@
--     /must/ be a valid pointer to an array of @commandBufferCount@
--     @uint32_t@ values
--
-- -   If @signalSemaphoreCount@ is not @0@,
--     @pSignalSemaphoreDeviceIndices@ /must/ be a valid pointer to an
--     array of @signalSemaphoreCount@ @uint32_t@ values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data DeviceGroupSubmitInfo = DeviceGroupSubmitInfo
  { -- Univalued member elided
  -- No documentation found for Nested "DeviceGroupSubmitInfo" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupSubmitInfo" "pWaitSemaphoreDeviceIndices"
  waitSemaphoreDeviceIndices :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupSubmitInfo" "pCommandBufferDeviceMasks"
  commandBufferDeviceMasks :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupSubmitInfo" "pSignalSemaphoreDeviceIndices"
  signalSemaphoreDeviceIndices :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDeviceGroupSubmitInfo' and
-- marshal a 'DeviceGroupSubmitInfo' into it. The 'VkDeviceGroupSubmitInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDeviceGroupSubmitInfo :: DeviceGroupSubmitInfo -> (VkDeviceGroupSubmitInfo -> IO a) -> IO a
withCStructDeviceGroupSubmitInfo marshalled cont = withVec (&) (signalSemaphoreDeviceIndices (marshalled :: DeviceGroupSubmitInfo)) (\pPSignalSemaphoreDeviceIndices -> withVec (&) (commandBufferDeviceMasks (marshalled :: DeviceGroupSubmitInfo)) (\pPCommandBufferDeviceMasks -> withVec (&) (waitSemaphoreDeviceIndices (marshalled :: DeviceGroupSubmitInfo)) (\pPWaitSemaphoreDeviceIndices -> maybeWith withSomeVkStruct (next (marshalled :: DeviceGroupSubmitInfo)) (\pPNext -> cont (VkDeviceGroupSubmitInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO pPNext (fromIntegral (Data.Vector.length (waitSemaphoreDeviceIndices (marshalled :: DeviceGroupSubmitInfo)))) pPWaitSemaphoreDeviceIndices (fromIntegral (Data.Vector.length (commandBufferDeviceMasks (marshalled :: DeviceGroupSubmitInfo)))) pPCommandBufferDeviceMasks (fromIntegral (Data.Vector.length (signalSemaphoreDeviceIndices (marshalled :: DeviceGroupSubmitInfo)))) pPSignalSemaphoreDeviceIndices)))))

-- | A function to read a 'VkDeviceGroupSubmitInfo' and all additional
-- structures in the pointer chain into a 'DeviceGroupSubmitInfo'.
fromCStructDeviceGroupSubmitInfo :: VkDeviceGroupSubmitInfo -> IO DeviceGroupSubmitInfo
fromCStructDeviceGroupSubmitInfo c = DeviceGroupSubmitInfo <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupSubmitInfo)))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkWaitSemaphoreCount (c :: VkDeviceGroupSubmitInfo))) (peekElemOff (vkPWaitSemaphoreDeviceIndices (c :: VkDeviceGroupSubmitInfo))))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkCommandBufferCount (c :: VkDeviceGroupSubmitInfo))) (peekElemOff (vkPCommandBufferDeviceMasks (c :: VkDeviceGroupSubmitInfo))))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkSignalSemaphoreCount (c :: VkDeviceGroupSubmitInfo))) (peekElemOff (vkPSignalSemaphoreDeviceIndices (c :: VkDeviceGroupSubmitInfo))))

instance Zero DeviceGroupSubmitInfo where
  zero = DeviceGroupSubmitInfo Nothing
                               Data.Vector.empty
                               Data.Vector.empty
                               Data.Vector.empty


-- | VkMemoryAllocateFlagBits - Bitmask specifying flags for a device memory
-- allocation
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlags'
type MemoryAllocateFlagBits = VkMemoryAllocateFlagBits


{-# complete MEMORY_ALLOCATE_DEVICE_MASK_BIT :: MemoryAllocateFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT'
-- specifies that memory will be allocated for the devices in
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagsInfo'::@deviceMask@.
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT :: (a ~ MemoryAllocateFlagBits) => a
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT = VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT

-- No documentation found for TopLevel "MemoryAllocateFlagBitsKHR"
type MemoryAllocateFlagBitsKHR = MemoryAllocateFlagBits

-- | VkMemoryAllocateFlags - Bitmask of VkMemoryAllocateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagsInfo'
type MemoryAllocateFlags = MemoryAllocateFlagBits


-- | VkMemoryAllocateFlagsInfo - Structure controlling how many instances of
-- memory will be allocated
--
-- = Description
--
-- If
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT'
-- is not set, the number of instances allocated depends on whether
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VK_MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- is set in the memory heap. If
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VK_MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- is set, then memory is allocated for every physical device in the
-- logical device (as if @deviceMask@ has bits set for all device indices).
-- If
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VK_MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- is not set, then a single instance of memory is allocated (as if
-- @deviceMask@ is set to one).
--
-- On some implementations, allocations from a multi-instance heap /may/
-- consume memory on all physical devices even if the @deviceMask@ excludes
-- some devices. If
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties'::@subsetAllocation@
-- is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', then memory is only consumed
-- for the devices in the device mask.
--
-- __Note__
--
-- In practice, most allocations on a multi-instance heap will be allocated
-- across all physical devices. Unicast allocation support is an optional
-- optimization for a minority of allocations.
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT'
--     is set, @deviceMask@ /must/ be a valid device mask.
--
-- -   If
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT'
--     is set, @deviceMask@ /must/ not be zero
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO'
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data MemoryAllocateFlagsInfo = MemoryAllocateFlagsInfo
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryAllocateFlagsInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryAllocateFlagsInfo" "flags"
  flags :: MemoryAllocateFlags
  , -- No documentation found for Nested "MemoryAllocateFlagsInfo" "deviceMask"
  deviceMask :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryAllocateFlagsInfo' and
-- marshal a 'MemoryAllocateFlagsInfo' into it. The 'VkMemoryAllocateFlagsInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryAllocateFlagsInfo :: MemoryAllocateFlagsInfo -> (VkMemoryAllocateFlagsInfo -> IO a) -> IO a
withCStructMemoryAllocateFlagsInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryAllocateFlagsInfo)) (\pPNext -> cont (VkMemoryAllocateFlagsInfo VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO pPNext (flags (marshalled :: MemoryAllocateFlagsInfo)) (deviceMask (marshalled :: MemoryAllocateFlagsInfo))))

-- | A function to read a 'VkMemoryAllocateFlagsInfo' and all additional
-- structures in the pointer chain into a 'MemoryAllocateFlagsInfo'.
fromCStructMemoryAllocateFlagsInfo :: VkMemoryAllocateFlagsInfo -> IO MemoryAllocateFlagsInfo
fromCStructMemoryAllocateFlagsInfo c = MemoryAllocateFlagsInfo <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryAllocateFlagsInfo)))
                                                               <*> pure (vkFlags (c :: VkMemoryAllocateFlagsInfo))
                                                               <*> pure (vkDeviceMask (c :: VkMemoryAllocateFlagsInfo))

instance Zero MemoryAllocateFlagsInfo where
  zero = MemoryAllocateFlagsInfo Nothing
                                 zero
                                 zero


-- No documentation found for TopLevel "MemoryAllocateFlagsKHR"
type MemoryAllocateFlagsKHR = MemoryAllocateFlags

-- | VkPeerMemoryFeatureFlagBits - Bitmask specifying supported peer memory
-- features
--
-- = Description
--
-- __Note__
--
-- The peer memory features of a memory heap also apply to any accesses
-- that /may/ be performed during
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transitions>.
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_PEER_MEMORY_FEATURE_COPY_DST_BIT'
-- /must/ be supported for all host local heaps and for at least one device
-- local heap.
--
-- If a device does not support a peer memory feature, it is still valid to
-- use a resource that includes both local and peer memory bindings with
-- the corresponding access type as long as only the local bindings are
-- actually accessed. For example, an application doing split-frame
-- rendering would use framebuffer attachments that include both local and
-- peer memory bindings, but would scissor the rendering to only update
-- local memory.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlags'
type PeerMemoryFeatureFlagBits = VkPeerMemoryFeatureFlagBits


{-# complete PEER_MEMORY_FEATURE_COPY_SRC_BIT, PEER_MEMORY_FEATURE_COPY_DST_BIT, PEER_MEMORY_FEATURE_GENERIC_SRC_BIT, PEER_MEMORY_FEATURE_GENERIC_DST_BIT :: PeerMemoryFeatureFlagBits #-}


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT'
-- specifies that the memory /can/ be accessed as the source of a
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- or
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer'
-- command.
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT :: (a ~ PeerMemoryFeatureFlagBits) => a
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT = VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_PEER_MEMORY_FEATURE_COPY_DST_BIT'
-- specifies that the memory /can/ be accessed as the destination of a
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- or
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer'
-- command.
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT :: (a ~ PeerMemoryFeatureFlagBits) => a
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT = VK_PEER_MEMORY_FEATURE_COPY_DST_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT'
-- specifies that the memory /can/ be read as any memory access type.
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT :: (a ~ PeerMemoryFeatureFlagBits) => a
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT = VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT


-- | 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT'
-- specifies that the memory /can/ be written as any memory access type.
-- Shader atomics are considered to be writes.
pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT :: (a ~ PeerMemoryFeatureFlagBits) => a
pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT = VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT

-- No documentation found for TopLevel "PeerMemoryFeatureFlagBitsKHR"
type PeerMemoryFeatureFlagBitsKHR = PeerMemoryFeatureFlagBits

-- | VkPeerMemoryFeatureFlags - Bitmask of VkPeerMemoryFeatureFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkGetDeviceGroupPeerMemoryFeatures',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group.vkGetDeviceGroupPeerMemoryFeaturesKHR'
type PeerMemoryFeatureFlags = PeerMemoryFeatureFlagBits

-- No documentation found for TopLevel "PeerMemoryFeatureFlagsKHR"
type PeerMemoryFeatureFlagsKHR = PeerMemoryFeatureFlags


-- | vkCmdDispatchBase - Dispatch compute work items
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @baseGroupX@ is the start value for the X component of
--     @WorkgroupId@.
--
-- -   @baseGroupY@ is the start value for the Y component of
--     @WorkgroupId@.
--
-- -   @baseGroupZ@ is the start value for the Z component of
--     @WorkgroupId@.
--
-- -   @groupCountX@ is the number of local workgroups to dispatch in the X
--     dimension.
--
-- -   @groupCountY@ is the number of local workgroups to dispatch in the Y
--     dimension.
--
-- -   @groupCountZ@ is the number of local workgroups to dispatch in the Z
--     dimension.
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of
-- @groupCountX@ × @groupCountY@ × @groupCountZ@ local workgroups is
-- assembled, with @WorkgroupId@ values ranging from [@baseGroup*@,
-- @baseGroup*@ + @groupCount*@) in each component.
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatch' is
-- equivalent to
-- @vkCmdDispatchBase(0,0,0,groupCountX,groupCountY,groupCountZ)@.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   @baseGroupX@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   @baseGroupX@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   @baseGroupZ@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- -   @groupCountX@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--     minus @baseGroupX@
--
-- -   @groupCountY@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--     minus @baseGroupY@
--
-- -   @groupCountZ@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--     minus @baseGroupZ@
--
-- -   If any of @baseGroupX@, @baseGroupY@, or @baseGroupZ@ are not zero,
--     then the bound compute pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_PIPELINE_CREATE_DISPATCH_BASE'
--     flag.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Compute         |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdDispatchBase :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdDispatchBase = \(CommandBuffer commandBuffer' commandTable) -> \baseGroupX' -> \baseGroupY' -> \baseGroupZ' -> \groupCountX' -> \groupCountY' -> \groupCountZ' -> vkCmdDispatchBase commandTable commandBuffer' baseGroupX' baseGroupY' baseGroupZ' groupCountX' groupCountY' groupCountZ' *> (pure ())


-- | vkCmdSetDeviceMask - Modify device mask of a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is command buffer whose current device mask is
--     modified.
--
-- -   @deviceMask@ is the new value of the current device mask.
--
-- = Description
--
-- @deviceMask@ is used to filter out subsequent commands from executing on
-- all physical devices whose bit indices are not set in the mask, except
-- commands beginning a render pass instance, commands transitioning to the
-- next subpass in the render pass instance, and commands ending a render
-- pass instance, which always execute on the set of physical devices whose
-- bit indices are included in the @deviceMask@ member of the instance of
-- the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group.VkDeviceGroupRenderPassBeginInfoKHR'
-- structure passed to the command beginning the corresponding render pass
-- instance.
--
-- == Valid Usage
--
-- -   @deviceMask@ /must/ be a valid device mask value
--
-- -   @deviceMask@ /must/ not be zero
--
-- -   @deviceMask@ /must/ not include any set bits that were not in the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupCommandBufferBeginInfo'::@deviceMask@
--     value when the command buffer began recording.
--
-- -   If
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkCmdSetDeviceMask'
--     is called inside a render pass instance, @deviceMask@ /must/ not
--     include any set bits that were not in the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupRenderPassBeginInfo'::@deviceMask@
--     value when the render pass instance began recording.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, compute,
--     or transfer operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > |                 |                 | Transfer        |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdSetDeviceMask :: CommandBuffer ->  Word32 ->  IO ()
cmdSetDeviceMask = \(CommandBuffer commandBuffer' commandTable) -> \deviceMask' -> vkCmdSetDeviceMask commandTable commandBuffer' deviceMask' *> (pure ())


-- | vkGetDeviceGroupPeerMemoryFeatures - Query supported peer memory
-- features of a device
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the memory.
--
-- -   @heapIndex@ is the index of the memory heap from which the memory is
--     allocated.
--
-- -   @localDeviceIndex@ is the device index of the physical device that
--     performs the memory access.
--
-- -   @remoteDeviceIndex@ is the device index of the physical device that
--     the memory is allocated for.
--
-- -   @pPeerMemoryFeatures@ is a pointer to a bitmask of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlagBits'
--     indicating which types of memory accesses are supported for the
--     combination of heap, local, and remote devices.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlags'
getDeviceGroupPeerMemoryFeatures :: Device ->  Word32 ->  Word32 ->  Word32 ->  IO (PeerMemoryFeatureFlags)
getDeviceGroupPeerMemoryFeatures = \(Device device' commandTable) -> \heapIndex' -> \localDeviceIndex' -> \remoteDeviceIndex' -> alloca (\pPeerMemoryFeatures' -> vkGetDeviceGroupPeerMemoryFeatures commandTable device' heapIndex' localDeviceIndex' remoteDeviceIndex' pPeerMemoryFeatures' *> (peek pPeerMemoryFeatures'))
