{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

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
  , MemoryAllocateFlagBitsKHR
  , MemoryAllocateFlags
  , withCStructMemoryAllocateFlagsInfo
  , fromCStructMemoryAllocateFlagsInfo
  , MemoryAllocateFlagsInfo(..)
  , MemoryAllocateFlagsKHR
  , PeerMemoryFeatureFlagBits
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
  ( generateM
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
import qualified Graphics.Vulkan.C.Dynamic
  ( cmdDispatchBase
  , cmdSetDeviceMask
  , getDeviceGroupPeerMemoryFeatures
  )


import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkDeviceGroupBindSparseInfo(..)
  , VkDeviceGroupCommandBufferBeginInfo(..)
  , VkDeviceGroupRenderPassBeginInfo(..)
  , VkDeviceGroupSubmitInfo(..)
  , VkMemoryAllocateFlagBits(..)
  , VkMemoryAllocateFlagsInfo(..)
  , VkPeerMemoryFeatureFlagBits(..)
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


-- No documentation found for TopLevel "DeviceGroupBindSparseInfo"
data DeviceGroupBindSparseInfo = DeviceGroupBindSparseInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceGroupBindSparseInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupBindSparseInfo" "resourceDeviceIndex"
  vkResourceDeviceIndex :: Word32
  , -- No documentation found for Nested "DeviceGroupBindSparseInfo" "memoryDeviceIndex"
  vkMemoryDeviceIndex :: Word32
  }
  deriving (Show, Eq)
withCStructDeviceGroupBindSparseInfo :: DeviceGroupBindSparseInfo -> (VkDeviceGroupBindSparseInfo -> IO a) -> IO a
withCStructDeviceGroupBindSparseInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: DeviceGroupBindSparseInfo)) (\pPNext -> cont (VkDeviceGroupBindSparseInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO pPNext (vkResourceDeviceIndex (from :: DeviceGroupBindSparseInfo)) (vkMemoryDeviceIndex (from :: DeviceGroupBindSparseInfo))))
fromCStructDeviceGroupBindSparseInfo :: VkDeviceGroupBindSparseInfo -> IO DeviceGroupBindSparseInfo
fromCStructDeviceGroupBindSparseInfo c = DeviceGroupBindSparseInfo <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupBindSparseInfo)))
                                                                   <*> pure (vkResourceDeviceIndex (c :: VkDeviceGroupBindSparseInfo))
                                                                   <*> pure (vkMemoryDeviceIndex (c :: VkDeviceGroupBindSparseInfo))
-- No documentation found for TopLevel "DeviceGroupCommandBufferBeginInfo"
data DeviceGroupCommandBufferBeginInfo = DeviceGroupCommandBufferBeginInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceGroupCommandBufferBeginInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupCommandBufferBeginInfo" "deviceMask"
  vkDeviceMask :: Word32
  }
  deriving (Show, Eq)
withCStructDeviceGroupCommandBufferBeginInfo :: DeviceGroupCommandBufferBeginInfo -> (VkDeviceGroupCommandBufferBeginInfo -> IO a) -> IO a
withCStructDeviceGroupCommandBufferBeginInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: DeviceGroupCommandBufferBeginInfo)) (\pPNext -> cont (VkDeviceGroupCommandBufferBeginInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO pPNext (vkDeviceMask (from :: DeviceGroupCommandBufferBeginInfo))))
fromCStructDeviceGroupCommandBufferBeginInfo :: VkDeviceGroupCommandBufferBeginInfo -> IO DeviceGroupCommandBufferBeginInfo
fromCStructDeviceGroupCommandBufferBeginInfo c = DeviceGroupCommandBufferBeginInfo <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupCommandBufferBeginInfo)))
                                                                                   <*> pure (vkDeviceMask (c :: VkDeviceGroupCommandBufferBeginInfo))
-- No documentation found for TopLevel "DeviceGroupRenderPassBeginInfo"
data DeviceGroupRenderPassBeginInfo = DeviceGroupRenderPassBeginInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceGroupRenderPassBeginInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DeviceGroupRenderPassBeginInfo" "deviceMask"
  vkDeviceMask :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupRenderPassBeginInfo" "pDeviceRenderAreas"
  vkPDeviceRenderAreas :: Vector Rect2D
  }
  deriving (Show, Eq)
withCStructDeviceGroupRenderPassBeginInfo :: DeviceGroupRenderPassBeginInfo -> (VkDeviceGroupRenderPassBeginInfo -> IO a) -> IO a
withCStructDeviceGroupRenderPassBeginInfo from cont = withVec withCStructRect2D (vkPDeviceRenderAreas (from :: DeviceGroupRenderPassBeginInfo)) (\pDeviceRenderAreas -> maybeWith withSomeVkStruct (vkPNext (from :: DeviceGroupRenderPassBeginInfo)) (\pPNext -> cont (VkDeviceGroupRenderPassBeginInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO pPNext (vkDeviceMask (from :: DeviceGroupRenderPassBeginInfo)) (fromIntegral (Data.Vector.length (vkPDeviceRenderAreas (from :: DeviceGroupRenderPassBeginInfo)))) pDeviceRenderAreas)))
fromCStructDeviceGroupRenderPassBeginInfo :: VkDeviceGroupRenderPassBeginInfo -> IO DeviceGroupRenderPassBeginInfo
fromCStructDeviceGroupRenderPassBeginInfo c = DeviceGroupRenderPassBeginInfo <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupRenderPassBeginInfo)))
                                                                             <*> pure (vkDeviceMask (c :: VkDeviceGroupRenderPassBeginInfo))
                                                                             -- Length valued member elided
                                                                             <*> (Data.Vector.generateM (fromIntegral (vkDeviceRenderAreaCount (c :: VkDeviceGroupRenderPassBeginInfo))) (((fromCStructRect2D <=<) . peekElemOff) (vkPDeviceRenderAreas (c :: VkDeviceGroupRenderPassBeginInfo))))
-- No documentation found for TopLevel "DeviceGroupSubmitInfo"
data DeviceGroupSubmitInfo = DeviceGroupSubmitInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DeviceGroupSubmitInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupSubmitInfo" "pWaitSemaphoreDeviceIndices"
  vkPWaitSemaphoreDeviceIndices :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupSubmitInfo" "pCommandBufferDeviceMasks"
  vkPCommandBufferDeviceMasks :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "DeviceGroupSubmitInfo" "pSignalSemaphoreDeviceIndices"
  vkPSignalSemaphoreDeviceIndices :: Vector Word32
  }
  deriving (Show, Eq)
withCStructDeviceGroupSubmitInfo :: DeviceGroupSubmitInfo -> (VkDeviceGroupSubmitInfo -> IO a) -> IO a
withCStructDeviceGroupSubmitInfo from cont = withVec (&) (vkPSignalSemaphoreDeviceIndices (from :: DeviceGroupSubmitInfo)) (\pSignalSemaphoreDeviceIndices -> withVec (&) (vkPCommandBufferDeviceMasks (from :: DeviceGroupSubmitInfo)) (\pCommandBufferDeviceMasks -> withVec (&) (vkPWaitSemaphoreDeviceIndices (from :: DeviceGroupSubmitInfo)) (\pWaitSemaphoreDeviceIndices -> maybeWith withSomeVkStruct (vkPNext (from :: DeviceGroupSubmitInfo)) (\pPNext -> cont (VkDeviceGroupSubmitInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO pPNext (fromIntegral (Data.Vector.length (vkPWaitSemaphoreDeviceIndices (from :: DeviceGroupSubmitInfo)))) pWaitSemaphoreDeviceIndices (fromIntegral (Data.Vector.length (vkPCommandBufferDeviceMasks (from :: DeviceGroupSubmitInfo)))) pCommandBufferDeviceMasks (fromIntegral (Data.Vector.length (vkPSignalSemaphoreDeviceIndices (from :: DeviceGroupSubmitInfo)))) pSignalSemaphoreDeviceIndices)))))
fromCStructDeviceGroupSubmitInfo :: VkDeviceGroupSubmitInfo -> IO DeviceGroupSubmitInfo
fromCStructDeviceGroupSubmitInfo c = DeviceGroupSubmitInfo <$> -- Univalued Member elided
                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDeviceGroupSubmitInfo)))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkWaitSemaphoreCount (c :: VkDeviceGroupSubmitInfo))) (peekElemOff (vkPWaitSemaphoreDeviceIndices (c :: VkDeviceGroupSubmitInfo))))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkCommandBufferCount (c :: VkDeviceGroupSubmitInfo))) (peekElemOff (vkPCommandBufferDeviceMasks (c :: VkDeviceGroupSubmitInfo))))
                                                           -- Length valued member elided
                                                           <*> (Data.Vector.generateM (fromIntegral (vkSignalSemaphoreCount (c :: VkDeviceGroupSubmitInfo))) (peekElemOff (vkPSignalSemaphoreDeviceIndices (c :: VkDeviceGroupSubmitInfo))))
-- No documentation found for TopLevel "MemoryAllocateFlagBits"
type MemoryAllocateFlagBits = VkMemoryAllocateFlagBits
-- No documentation found for TopLevel "MemoryAllocateFlagBitsKHR"
type MemoryAllocateFlagBitsKHR = MemoryAllocateFlagBits
-- No documentation found for TopLevel "MemoryAllocateFlags"
type MemoryAllocateFlags = MemoryAllocateFlagBits
-- No documentation found for TopLevel "MemoryAllocateFlagsInfo"
data MemoryAllocateFlagsInfo = MemoryAllocateFlagsInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryAllocateFlagsInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryAllocateFlagsInfo" "flags"
  vkFlags :: MemoryAllocateFlags
  , -- No documentation found for Nested "MemoryAllocateFlagsInfo" "deviceMask"
  vkDeviceMask :: Word32
  }
  deriving (Show, Eq)
withCStructMemoryAllocateFlagsInfo :: MemoryAllocateFlagsInfo -> (VkMemoryAllocateFlagsInfo -> IO a) -> IO a
withCStructMemoryAllocateFlagsInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryAllocateFlagsInfo)) (\pPNext -> cont (VkMemoryAllocateFlagsInfo VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO pPNext (vkFlags (from :: MemoryAllocateFlagsInfo)) (vkDeviceMask (from :: MemoryAllocateFlagsInfo))))
fromCStructMemoryAllocateFlagsInfo :: VkMemoryAllocateFlagsInfo -> IO MemoryAllocateFlagsInfo
fromCStructMemoryAllocateFlagsInfo c = MemoryAllocateFlagsInfo <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryAllocateFlagsInfo)))
                                                               <*> pure (vkFlags (c :: VkMemoryAllocateFlagsInfo))
                                                               <*> pure (vkDeviceMask (c :: VkMemoryAllocateFlagsInfo))
-- No documentation found for TopLevel "MemoryAllocateFlagsKHR"
type MemoryAllocateFlagsKHR = MemoryAllocateFlags
-- No documentation found for TopLevel "PeerMemoryFeatureFlagBits"
type PeerMemoryFeatureFlagBits = VkPeerMemoryFeatureFlagBits
-- No documentation found for TopLevel "PeerMemoryFeatureFlagBitsKHR"
type PeerMemoryFeatureFlagBitsKHR = PeerMemoryFeatureFlagBits
-- No documentation found for TopLevel "PeerMemoryFeatureFlags"
type PeerMemoryFeatureFlags = PeerMemoryFeatureFlagBits
-- No documentation found for TopLevel "PeerMemoryFeatureFlagsKHR"
type PeerMemoryFeatureFlagsKHR = PeerMemoryFeatureFlags

-- | Wrapper for vkCmdDispatchBase
cmdDispatchBase :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdDispatchBase = \(CommandBuffer commandBuffer commandTable) -> \baseGroupX -> \baseGroupY -> \baseGroupZ -> \groupCountX -> \groupCountY -> \groupCountZ -> Graphics.Vulkan.C.Dynamic.cmdDispatchBase commandTable commandBuffer baseGroupX baseGroupY baseGroupZ groupCountX groupCountY groupCountZ *> (pure ())

-- | Wrapper for vkCmdSetDeviceMask
cmdSetDeviceMask :: CommandBuffer ->  Word32 ->  IO ()
cmdSetDeviceMask = \(CommandBuffer commandBuffer commandTable) -> \deviceMask -> Graphics.Vulkan.C.Dynamic.cmdSetDeviceMask commandTable commandBuffer deviceMask *> (pure ())

-- | Wrapper for vkGetDeviceGroupPeerMemoryFeatures
getDeviceGroupPeerMemoryFeatures :: Device ->  Word32 ->  Word32 ->  Word32 ->  IO (PeerMemoryFeatureFlags)
getDeviceGroupPeerMemoryFeatures = \(Device device commandTable) -> \heapIndex -> \localDeviceIndex -> \remoteDeviceIndex -> alloca (\pPeerMemoryFeatures -> Graphics.Vulkan.C.Dynamic.getDeviceGroupPeerMemoryFeatures commandTable device heapIndex localDeviceIndex remoteDeviceIndex pPeerMemoryFeatures *> (peek pPeerMemoryFeatures))
