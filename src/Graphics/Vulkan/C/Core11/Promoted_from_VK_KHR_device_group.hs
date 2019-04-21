{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkDeviceGroupBindSparseInfo(..)
  , VkDeviceGroupCommandBufferBeginInfo(..)
  , VkDeviceGroupRenderPassBeginInfo(..)
  , VkDeviceGroupSubmitInfo(..)
  , VkMemoryAllocateFlagBits(..)
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , VkMemoryAllocateFlags
  , VkMemoryAllocateFlagsInfo(..)
  , VkPeerMemoryFeatureFlagBits(..)
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , VkPeerMemoryFeatureFlags
  , FN_vkCmdDispatchBase
  , PFN_vkCmdDispatchBase
  , vkCmdDispatchBase
  , FN_vkCmdSetDeviceMask
  , PFN_vkCmdSetDeviceMask
  , vkCmdSetDeviceMask
  , FN_vkGetDeviceGroupPeerMemoryFeatures
  , PFN_vkGetDeviceGroupPeerMemoryFeatures
  , vkGetDeviceGroupPeerMemoryFeatures
  , pattern VK_DEPENDENCY_DEVICE_GROUP_BIT
  , pattern VK_PIPELINE_CREATE_DISPATCH_BASE
  , pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
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
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkDependencyFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkPipelineCreateFlagBits(..)
  , VkRect2D(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkCommandBuffer
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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
data VkDeviceGroupBindSparseInfo = VkDeviceGroupBindSparseInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @resourceDeviceIndex@ is a device index indicating which instance of the
  -- resource is bound.
  vkResourceDeviceIndex :: Word32
  , -- | @memoryDeviceIndex@ is a device index indicating which instance of the
  -- memory the resource instance is bound to.
  vkMemoryDeviceIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupBindSparseInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGroupBindSparseInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupBindSparseInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupBindSparseInfo))
                *> poke (ptr `plusPtr` 16) (vkResourceDeviceIndex (poked :: VkDeviceGroupBindSparseInfo))
                *> poke (ptr `plusPtr` 20) (vkMemoryDeviceIndex (poked :: VkDeviceGroupBindSparseInfo))

instance Zero VkDeviceGroupBindSparseInfo where
  zero = VkDeviceGroupBindSparseInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
                                     zero
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
data VkDeviceGroupCommandBufferBeginInfo = VkDeviceGroupCommandBufferBeginInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @deviceMask@ /must/ not be zero
  vkDeviceMask :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupCommandBufferBeginInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDeviceGroupCommandBufferBeginInfo <$> peek (ptr `plusPtr` 0)
                                                 <*> peek (ptr `plusPtr` 8)
                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupCommandBufferBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkDeviceMask (poked :: VkDeviceGroupCommandBufferBeginInfo))

instance Zero VkDeviceGroupCommandBufferBeginInfo where
  zero = VkDeviceGroupCommandBufferBeginInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
                                             zero
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
-- is the value of 'VkDeviceGroupCommandBufferBeginInfo'::@deviceMask@. If
-- this structure is not present or if @deviceRenderAreaCount@ is zero,
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
data VkDeviceGroupRenderPassBeginInfo = VkDeviceGroupRenderPassBeginInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @deviceMask@ /must/ be a subset of the command buffer’s initial device
  -- mask
  vkDeviceMask :: Word32
  , -- | @deviceRenderAreaCount@ /must/ either be zero or equal to the number of
  -- physical devices in the logical device.
  vkDeviceRenderAreaCount :: Word32
  , -- | @pDeviceRenderAreas@ is an array of structures of type
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' defining the render area
  -- for each physical device.
  vkPDeviceRenderAreas :: Ptr VkRect2D
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupRenderPassBeginInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDeviceGroupRenderPassBeginInfo <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 20)
                                              <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkDeviceMask (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 20) (vkDeviceRenderAreaCount (poked :: VkDeviceGroupRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkPDeviceRenderAreas (poked :: VkDeviceGroupRenderPassBeginInfo))

instance Zero VkDeviceGroupRenderPassBeginInfo where
  zero = VkDeviceGroupRenderPassBeginInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
                                          zero
                                          zero
                                          zero
                                          zero

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
data VkDeviceGroupSubmitInfo = VkDeviceGroupSubmitInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @waitSemaphoreCount@ is the number of elements in the
  -- @pWaitSemaphoreDeviceIndices@ array.
  vkWaitSemaphoreCount :: Word32
  , -- | @pWaitSemaphoreDeviceIndices@ is an array of device indices indicating
  -- which physical device executes the semaphore wait operation in the
  -- corresponding element of
  -- 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo'::@pWaitSemaphores@.
  vkPWaitSemaphoreDeviceIndices :: Ptr Word32
  , -- | @commandBufferCount@ is the number of elements in the
  -- @pCommandBufferDeviceMasks@ array.
  vkCommandBufferCount :: Word32
  , -- | @pCommandBufferDeviceMasks@ is an array of device masks indicating which
  -- physical devices execute the command buffer in the corresponding element
  -- of 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo'::@pCommandBuffers@. A
  -- physical device executes the command buffer if the corresponding bit is
  -- set in the mask.
  vkPCommandBufferDeviceMasks :: Ptr Word32
  , -- | @signalSemaphoreCount@ is the number of elements in the
  -- @pSignalSemaphoreDeviceIndices@ array.
  vkSignalSemaphoreCount :: Word32
  , -- | @pSignalSemaphoreDeviceIndices@ is an array of device indices indicating
  -- which physical device executes the semaphore signal operation in the
  -- corresponding element of
  -- 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo'::@pSignalSemaphores@.
  vkPSignalSemaphoreDeviceIndices :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupSubmitInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkDeviceGroupSubmitInfo <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 24)
                                     <*> peek (ptr `plusPtr` 32)
                                     <*> peek (ptr `plusPtr` 40)
                                     <*> peek (ptr `plusPtr` 48)
                                     <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 16) (vkWaitSemaphoreCount (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 24) (vkPWaitSemaphoreDeviceIndices (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 32) (vkCommandBufferCount (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 40) (vkPCommandBufferDeviceMasks (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 48) (vkSignalSemaphoreCount (poked :: VkDeviceGroupSubmitInfo))
                *> poke (ptr `plusPtr` 56) (vkPSignalSemaphoreDeviceIndices (poked :: VkDeviceGroupSubmitInfo))

instance Zero VkDeviceGroupSubmitInfo where
  zero = VkDeviceGroupSubmitInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero
                                 zero

-- ** VkMemoryAllocateFlagBits

-- | VkMemoryAllocateFlagBits - Bitmask specifying flags for a device memory
-- allocation
--
-- = See Also
--
-- 'VkMemoryAllocateFlags'
newtype VkMemoryAllocateFlagBits = VkMemoryAllocateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkMemoryAllocateFlagBits where
  showsPrec _ VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT = showString "VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT"
  showsPrec p (VkMemoryAllocateFlagBits x) = showParen (p >= 11) (showString "VkMemoryAllocateFlagBits " . showsPrec 11 x)

instance Read VkMemoryAllocateFlagBits where
  readPrec = parens ( choose [ ("VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT", pure VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryAllocateFlagBits")
                        v <- step readPrec
                        pure (VkMemoryAllocateFlagBits v)
                        )
                    )

-- | 'VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT' specifies that memory will be
-- allocated for the devices in 'VkMemoryAllocateFlagsInfo'::@deviceMask@.
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT :: VkMemoryAllocateFlagBits
pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT = VkMemoryAllocateFlagBits 0x00000001

-- | VkMemoryAllocateFlags - Bitmask of VkMemoryAllocateFlagBits
--
-- = Description
--
-- 'VkMemoryAllocateFlags' is a bitmask type for setting a mask of zero or
-- more 'VkMemoryAllocateFlagBits'.
--
-- = See Also
--
-- 'VkMemoryAllocateFlagBits', 'VkMemoryAllocateFlagsInfo'
type VkMemoryAllocateFlags = VkMemoryAllocateFlagBits

-- | VkMemoryAllocateFlagsInfo - Structure controlling how many instances of
-- memory will be allocated
--
-- = Description
--
-- If 'VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT' is not set, the number of
-- instances allocated depends on whether
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
-- -   If 'VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT' is set, @deviceMask@ /must/
--     be a valid device mask.
--
-- -   If 'VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT' is set, @deviceMask@ /must/
--     not be zero
--
-- Unresolved directive in VkMemoryAllocateFlagsInfo.txt -
-- include::{generated}\/validity\/structs\/VkMemoryAllocateFlagsInfo.txt[]
--
-- = See Also
--
-- 'VkMemoryAllocateFlags', 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkMemoryAllocateFlagsInfo = VkMemoryAllocateFlagsInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkMemoryAllocateFlagBits' controlling the
  -- allocation.
  vkFlags :: VkMemoryAllocateFlags
  , -- | @deviceMask@ is a mask of physical devices in the logical device,
  -- indicating that memory /must/ be allocated on each device in the mask,
  -- if 'VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT' is set in @flags@.
  vkDeviceMask :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryAllocateFlagsInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryAllocateFlagsInfo <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryAllocateFlagsInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryAllocateFlagsInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkMemoryAllocateFlagsInfo))
                *> poke (ptr `plusPtr` 20) (vkDeviceMask (poked :: VkMemoryAllocateFlagsInfo))

instance Zero VkMemoryAllocateFlagsInfo where
  zero = VkMemoryAllocateFlagsInfo VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
                                   zero
                                   zero
                                   zero

-- ** VkPeerMemoryFeatureFlagBits

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
-- 'VK_PEER_MEMORY_FEATURE_COPY_DST_BIT' /must/ be supported for all host
-- local heaps and for at least one device local heap.
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
-- 'VkPeerMemoryFeatureFlags'
newtype VkPeerMemoryFeatureFlagBits = VkPeerMemoryFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkPeerMemoryFeatureFlagBits where
  showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT = showString "VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT"
  showsPrec _ VK_PEER_MEMORY_FEATURE_COPY_DST_BIT = showString "VK_PEER_MEMORY_FEATURE_COPY_DST_BIT"
  showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT = showString "VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT"
  showsPrec _ VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT = showString "VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT"
  showsPrec p (VkPeerMemoryFeatureFlagBits x) = showParen (p >= 11) (showString "VkPeerMemoryFeatureFlagBits " . showsPrec 11 x)

instance Read VkPeerMemoryFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT",    pure VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT)
                             , ("VK_PEER_MEMORY_FEATURE_COPY_DST_BIT",    pure VK_PEER_MEMORY_FEATURE_COPY_DST_BIT)
                             , ("VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT", pure VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT)
                             , ("VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT", pure VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPeerMemoryFeatureFlagBits")
                        v <- step readPrec
                        pure (VkPeerMemoryFeatureFlagBits v)
                        )
                    )

-- | 'VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT' specifies that the memory /can/ be
-- accessed as the source of a
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- or
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer'
-- command.
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT = VkPeerMemoryFeatureFlagBits 0x00000001

-- | 'VK_PEER_MEMORY_FEATURE_COPY_DST_BIT' specifies that the memory /can/ be
-- accessed as the destination of a
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- or
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer'
-- command.
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT = VkPeerMemoryFeatureFlagBits 0x00000002

-- | 'VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT' specifies that the memory /can/
-- be read as any memory access type.
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT = VkPeerMemoryFeatureFlagBits 0x00000004

-- | 'VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT' specifies that the memory /can/
-- be written as any memory access type. Shader atomics are considered to
-- be writes.
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT :: VkPeerMemoryFeatureFlagBits
pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT = VkPeerMemoryFeatureFlagBits 0x00000008

-- | VkPeerMemoryFeatureFlags - Bitmask of VkPeerMemoryFeatureFlagBits
--
-- = Description
--
-- 'VkPeerMemoryFeatureFlags' is a bitmask type for setting a mask of zero
-- or more 'VkPeerMemoryFeatureFlagBits'.
--
-- = See Also
--
-- 'VkPeerMemoryFeatureFlagBits', 'vkGetDeviceGroupPeerMemoryFeatures'
type VkPeerMemoryFeatureFlags = VkPeerMemoryFeatureFlagBits

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
-- been created with the 'VK_PIPELINE_CREATE_DISPATCH_BASE' flag.
--
-- Unresolved directive in vkCmdDispatchBase.txt -
-- include::{generated}\/validity\/protos\/vkCmdDispatchBase.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDispatchBase" vkCmdDispatchBase :: ("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
#else
vkCmdDispatchBase :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
vkCmdDispatchBase deviceCmds = mkVkCmdDispatchBase (pVkCmdDispatchBase deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchBase
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ())
#endif

type FN_vkCmdDispatchBase = ("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
type PFN_vkCmdDispatchBase = FunPtr FN_vkCmdDispatchBase

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
--     'VkDeviceGroupCommandBufferBeginInfo'::@deviceMask@ value when the
--     command buffer began recording.
--
-- -   If 'vkCmdSetDeviceMask' is called inside a render pass instance,
--     @deviceMask@ /must/ not include any set bits that were not in the
--     'VkDeviceGroupRenderPassBeginInfo'::@deviceMask@ value when the
--     render pass instance began recording.
--
-- Unresolved directive in vkCmdSetDeviceMask.txt -
-- include::{generated}\/validity\/protos\/vkCmdSetDeviceMask.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetDeviceMask" vkCmdSetDeviceMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()
#else
vkCmdSetDeviceMask :: DeviceCmds -> ("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()
vkCmdSetDeviceMask deviceCmds = mkVkCmdSetDeviceMask (pVkCmdSetDeviceMask deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDeviceMask
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ())
#endif

type FN_vkCmdSetDeviceMask = ("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()
type PFN_vkCmdSetDeviceMask = FunPtr FN_vkCmdSetDeviceMask

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
--     'VkPeerMemoryFeatureFlagBits' indicating which types of memory
--     accesses are supported for the combination of heap, local, and
--     remote devices.
--
-- == Valid Usage
--
-- Unresolved directive in vkGetDeviceGroupPeerMemoryFeatures.txt -
-- include::{generated}\/validity\/protos\/vkGetDeviceGroupPeerMemoryFeatures.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkPeerMemoryFeatureFlags'
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceGroupPeerMemoryFeatures" vkGetDeviceGroupPeerMemoryFeatures :: ("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()
#else
vkGetDeviceGroupPeerMemoryFeatures :: DeviceCmds -> ("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()
vkGetDeviceGroupPeerMemoryFeatures deviceCmds = mkVkGetDeviceGroupPeerMemoryFeatures (pVkGetDeviceGroupPeerMemoryFeatures deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupPeerMemoryFeatures
  :: FunPtr (("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()) -> (("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ())
#endif

type FN_vkGetDeviceGroupPeerMemoryFeatures = ("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()
type PFN_vkGetDeviceGroupPeerMemoryFeatures = FunPtr FN_vkGetDeviceGroupPeerMemoryFeatures

-- | 'VK_DEPENDENCY_DEVICE_GROUP_BIT' specifies that dependencies are
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-device-local-dependencies non-device-local dependency>.
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT :: VkDependencyFlagBits
pattern VK_DEPENDENCY_DEVICE_GROUP_BIT = VkDependencyFlagBits 0x00000004

-- | 'VK_PIPELINE_CREATE_DISPATCH_BASE' specifies that a compute pipeline
-- /can/ be used with 'vkCmdDispatchBase' with a non-zero base workgroup.
pattern VK_PIPELINE_CREATE_DISPATCH_BASE :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_DISPATCH_BASE = VkPipelineCreateFlagBits 0x00000010

-- | 'VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT' specifies that any
-- shader input variables decorated as @ViewIndex@ will be assigned values
-- as if they were decorated as @DeviceIndex@.
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT :: VkPipelineCreateFlagBits
pattern VK_PIPELINE_CREATE_VIEW_INDEX_FROM_DEVICE_INDEX_BIT = VkPipelineCreateFlagBits 0x00000008

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO = VkStructureType 1000060006

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO = VkStructureType 1000060004

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO = VkStructureType 1000060003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO = VkStructureType 1000060005

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO"
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO = VkStructureType 1000060000
