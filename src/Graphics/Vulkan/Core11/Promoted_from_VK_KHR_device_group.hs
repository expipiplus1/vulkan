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
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
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
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
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
-- Unresolved directive in VkDeviceGroupBindSparseInfo.txt -
-- include::{generated}\/validity\/structs\/VkDeviceGroupBindSparseInfo.txt[]
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
-- == Valid Usage
--
-- Unresolved directive in VkDeviceGroupCommandBufferBeginInfo.txt -
-- include::{generated}\/validity\/structs\/VkDeviceGroupCommandBufferBeginInfo.txt[]
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
-- Unresolved directive in VkDeviceGroupRenderPassBeginInfo.txt -
-- include::{generated}\/validity\/structs\/VkDeviceGroupRenderPassBeginInfo.txt[]
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
-- Unresolved directive in VkDeviceGroupSubmitInfo.txt -
-- include::{generated}\/validity\/structs\/VkDeviceGroupSubmitInfo.txt[]
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
-- Unresolved directive in VkMemoryAllocateFlagsInfo.txt -
-- include::{generated}\/validity\/structs\/VkMemoryAllocateFlagsInfo.txt[]
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
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkGetDeviceGroupPeerMemoryFeatures'
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
-- Unresolved directive in vkCmdDispatchBase.txt -
-- include::{chapters}\/commonvalidity\/draw_dispatch_common.txt[] *
-- @baseGroupX@ /must/ be less than
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
-- * @baseGroupX@ /must/ be less than
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
-- * @baseGroupZ@ /must/ be less than
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
-- * @groupCountX@ /must/ be less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
-- minus @baseGroupX@ * @groupCountY@ /must/ be less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
-- minus @baseGroupY@ * @groupCountZ@ /must/ be less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
-- minus @baseGroupZ@ * If any of @baseGroupX@, @baseGroupY@, or
-- @baseGroupZ@ are not zero, then the bound compute pipeline /must/ have
-- been created with the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VK_PIPELINE_CREATE_DISPATCH_BASE'
-- flag.
--
-- Unresolved directive in vkCmdDispatchBase.txt -
-- include::{generated}\/validity\/protos\/vkCmdDispatchBase.txt[]
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
-- Unresolved directive in vkCmdSetDeviceMask.txt -
-- include::{generated}\/validity\/protos\/vkCmdSetDeviceMask.txt[]
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
-- == Valid Usage
--
-- Unresolved directive in vkGetDeviceGroupPeerMemoryFeatures.txt -
-- include::{generated}\/validity\/protos\/vkGetDeviceGroupPeerMemoryFeatures.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkPeerMemoryFeatureFlags'
getDeviceGroupPeerMemoryFeatures :: Device ->  Word32 ->  Word32 ->  Word32 ->  IO (PeerMemoryFeatureFlags)
getDeviceGroupPeerMemoryFeatures = \(Device device' commandTable) -> \heapIndex' -> \localDeviceIndex' -> \remoteDeviceIndex' -> alloca (\pPeerMemoryFeatures' -> vkGetDeviceGroupPeerMemoryFeatures commandTable device' heapIndex' localDeviceIndex' remoteDeviceIndex' pPeerMemoryFeatures' *> (peek pPeerMemoryFeatures'))
