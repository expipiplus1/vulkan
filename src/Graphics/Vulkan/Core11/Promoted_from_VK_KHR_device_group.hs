{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DeviceGroupBindSparseInfo(..)
  , 
  DeviceGroupCommandBufferBeginInfo(..)
  , DeviceGroupRenderPassBeginInfo(..)
  , DeviceGroupSubmitInfo(..)
#endif
  , MemoryAllocateFlagBits
  , pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , MemoryAllocateFlagBitsKHR
  , MemoryAllocateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , MemoryAllocateFlagsInfo(..)
#endif
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


#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkMemoryAllocateFlagBits(..)
  , VkPeerMemoryFeatureFlagBits(..)
  , vkCmdDispatchBase
  , vkCmdSetDeviceMask
  , vkGetDeviceGroupPeerMemoryFeatures
  , pattern VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT
  , pattern VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  )
#endif
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
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



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceGroupBindSparseInfo"
data DeviceGroupBindSparseInfo = DeviceGroupBindSparseInfo
  { -- No documentation found for Nested "DeviceGroupBindSparseInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupBindSparseInfo" "resourceDeviceIndex"
  resourceDeviceIndex :: Word32
  , -- No documentation found for Nested "DeviceGroupBindSparseInfo" "memoryDeviceIndex"
  memoryDeviceIndex :: Word32
  }
  deriving (Show, Eq)

instance Zero DeviceGroupBindSparseInfo where
  zero = DeviceGroupBindSparseInfo Nothing
                                   zero
                                   zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceGroupCommandBufferBeginInfo"
data DeviceGroupCommandBufferBeginInfo = DeviceGroupCommandBufferBeginInfo
  { -- No documentation found for Nested "DeviceGroupCommandBufferBeginInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupCommandBufferBeginInfo" "deviceMask"
  deviceMask :: Word32
  }
  deriving (Show, Eq)

instance Zero DeviceGroupCommandBufferBeginInfo where
  zero = DeviceGroupCommandBufferBeginInfo Nothing
                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceGroupRenderPassBeginInfo"
data DeviceGroupRenderPassBeginInfo = DeviceGroupRenderPassBeginInfo
  { -- No documentation found for Nested "DeviceGroupRenderPassBeginInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupRenderPassBeginInfo" "deviceMask"
  deviceMask :: Word32
  , -- No documentation found for Nested "DeviceGroupRenderPassBeginInfo" "pDeviceRenderAreas"
  deviceRenderAreas :: Vector Rect2D
  }
  deriving (Show, Eq)

instance Zero DeviceGroupRenderPassBeginInfo where
  zero = DeviceGroupRenderPassBeginInfo Nothing
                                        zero
                                        mempty

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDeviceGroupSubmitInfo"
data DeviceGroupSubmitInfo = DeviceGroupSubmitInfo
  { -- No documentation found for Nested "DeviceGroupSubmitInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupSubmitInfo" "pWaitSemaphoreDeviceIndices"
  waitSemaphoreDeviceIndices :: Vector Word32
  , -- No documentation found for Nested "DeviceGroupSubmitInfo" "pCommandBufferDeviceMasks"
  commandBufferDeviceMasks :: Vector Word32
  , -- No documentation found for Nested "DeviceGroupSubmitInfo" "pSignalSemaphoreDeviceIndices"
  signalSemaphoreDeviceIndices :: Vector Word32
  }
  deriving (Show, Eq)

instance Zero DeviceGroupSubmitInfo where
  zero = DeviceGroupSubmitInfo Nothing
                               mempty
                               mempty
                               mempty

#endif

-- No documentation found for TopLevel "MemoryAllocateFlagBits"
type MemoryAllocateFlagBits = VkMemoryAllocateFlagBits


{-# complete MEMORY_ALLOCATE_DEVICE_MASK_BIT :: MemoryAllocateFlagBits #-}


-- No documentation found for Nested "MemoryAllocateFlagBits" "MEMORY_ALLOCATE_DEVICE_MASK_BIT"
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT :: (a ~ MemoryAllocateFlagBits) => a
pattern MEMORY_ALLOCATE_DEVICE_MASK_BIT = VK_MEMORY_ALLOCATE_DEVICE_MASK_BIT

-- No documentation found for TopLevel "MemoryAllocateFlagBitsKHR"
type MemoryAllocateFlagBitsKHR = MemoryAllocateFlagBits

-- No documentation found for TopLevel "MemoryAllocateFlags"
type MemoryAllocateFlags = MemoryAllocateFlagBits


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkMemoryAllocateFlagsInfo"
data MemoryAllocateFlagsInfo = MemoryAllocateFlagsInfo
  { -- No documentation found for Nested "MemoryAllocateFlagsInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryAllocateFlagsInfo" "flags"
  flags :: MemoryAllocateFlags
  , -- No documentation found for Nested "MemoryAllocateFlagsInfo" "deviceMask"
  deviceMask :: Word32
  }
  deriving (Show, Eq)

instance Zero MemoryAllocateFlagsInfo where
  zero = MemoryAllocateFlagsInfo Nothing
                                 zero
                                 zero

#endif

-- No documentation found for TopLevel "MemoryAllocateFlagsKHR"
type MemoryAllocateFlagsKHR = MemoryAllocateFlags

-- No documentation found for TopLevel "PeerMemoryFeatureFlagBits"
type PeerMemoryFeatureFlagBits = VkPeerMemoryFeatureFlagBits


{-# complete PEER_MEMORY_FEATURE_COPY_SRC_BIT, PEER_MEMORY_FEATURE_COPY_DST_BIT, PEER_MEMORY_FEATURE_GENERIC_SRC_BIT, PEER_MEMORY_FEATURE_GENERIC_DST_BIT :: PeerMemoryFeatureFlagBits #-}


-- No documentation found for Nested "PeerMemoryFeatureFlagBits" "PEER_MEMORY_FEATURE_COPY_SRC_BIT"
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT :: (a ~ PeerMemoryFeatureFlagBits) => a
pattern PEER_MEMORY_FEATURE_COPY_SRC_BIT = VK_PEER_MEMORY_FEATURE_COPY_SRC_BIT


-- No documentation found for Nested "PeerMemoryFeatureFlagBits" "PEER_MEMORY_FEATURE_COPY_DST_BIT"
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT :: (a ~ PeerMemoryFeatureFlagBits) => a
pattern PEER_MEMORY_FEATURE_COPY_DST_BIT = VK_PEER_MEMORY_FEATURE_COPY_DST_BIT


-- No documentation found for Nested "PeerMemoryFeatureFlagBits" "PEER_MEMORY_FEATURE_GENERIC_SRC_BIT"
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT :: (a ~ PeerMemoryFeatureFlagBits) => a
pattern PEER_MEMORY_FEATURE_GENERIC_SRC_BIT = VK_PEER_MEMORY_FEATURE_GENERIC_SRC_BIT


-- No documentation found for Nested "PeerMemoryFeatureFlagBits" "PEER_MEMORY_FEATURE_GENERIC_DST_BIT"
pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT :: (a ~ PeerMemoryFeatureFlagBits) => a
pattern PEER_MEMORY_FEATURE_GENERIC_DST_BIT = VK_PEER_MEMORY_FEATURE_GENERIC_DST_BIT

-- No documentation found for TopLevel "PeerMemoryFeatureFlagBitsKHR"
type PeerMemoryFeatureFlagBitsKHR = PeerMemoryFeatureFlagBits

-- No documentation found for TopLevel "PeerMemoryFeatureFlags"
type PeerMemoryFeatureFlags = PeerMemoryFeatureFlagBits

-- No documentation found for TopLevel "PeerMemoryFeatureFlagsKHR"
type PeerMemoryFeatureFlagsKHR = PeerMemoryFeatureFlags


-- No documentation found for TopLevel "vkCmdDispatchBase"
cmdDispatchBase :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdDispatchBase = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCmdSetDeviceMask"
cmdSetDeviceMask :: CommandBuffer ->  Word32 ->  IO ()
cmdSetDeviceMask = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetDeviceGroupPeerMemoryFeatures"
getDeviceGroupPeerMemoryFeatures :: Device ->  Word32 ->  Word32 ->  Word32 ->  IO (PeerMemoryFeatureFlags)
getDeviceGroupPeerMemoryFeatures = undefined {- {wrapped (pretty cName) :: Doc ()} -}
