{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  , withCStructMappedMemoryRange
  , fromCStructMappedMemoryRange
  , MappedMemoryRange(..)
  , withCStructMemoryAllocateInfo
  , fromCStructMemoryAllocateInfo
  , MemoryAllocateInfo(..)
  , MemoryMapFlags
  , allocateMemory
  , flushMappedMemoryRanges
  , freeMemory
  , getDeviceMemoryCommitment
  , invalidateMappedMemoryRanges
  , mapMemory
  , unmapMemory
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
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( allocateMemory
  , flushMappedMemoryRanges
  , freeMemory
  , getDeviceMemoryCommitment
  , invalidateMappedMemoryRanges
  , mapMemory
  , unmapMemory
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkMappedMemoryRange(..)
  , VkMemoryAllocateInfo(..)
  , VkMemoryMapFlags(..)
  , VkDeviceMemory
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  , withCStructAllocationCallbacks
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


-- No documentation found for TopLevel "DeviceMemory"
type DeviceMemory = VkDeviceMemory
-- No documentation found for TopLevel "MappedMemoryRange"
data MappedMemoryRange = MappedMemoryRange
  { -- Univalued Member elided
  -- No documentation found for Nested "MappedMemoryRange" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MappedMemoryRange" "memory"
  vkMemory :: DeviceMemory
  , -- No documentation found for Nested "MappedMemoryRange" "offset"
  vkOffset :: DeviceSize
  , -- No documentation found for Nested "MappedMemoryRange" "size"
  vkSize :: DeviceSize
  }
  deriving (Show, Eq)
withCStructMappedMemoryRange :: MappedMemoryRange -> (VkMappedMemoryRange -> IO a) -> IO a
withCStructMappedMemoryRange from cont = maybeWith withSomeVkStruct (vkPNext (from :: MappedMemoryRange)) (\pPNext -> cont (VkMappedMemoryRange VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE pPNext (vkMemory (from :: MappedMemoryRange)) (vkOffset (from :: MappedMemoryRange)) (vkSize (from :: MappedMemoryRange))))
fromCStructMappedMemoryRange :: VkMappedMemoryRange -> IO MappedMemoryRange
fromCStructMappedMemoryRange c = MappedMemoryRange <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMappedMemoryRange)))
                                                   <*> pure (vkMemory (c :: VkMappedMemoryRange))
                                                   <*> pure (vkOffset (c :: VkMappedMemoryRange))
                                                   <*> pure (vkSize (c :: VkMappedMemoryRange))
-- No documentation found for TopLevel "MemoryAllocateInfo"
data MemoryAllocateInfo = MemoryAllocateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "MemoryAllocateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryAllocateInfo" "allocationSize"
  vkAllocationSize :: DeviceSize
  , -- No documentation found for Nested "MemoryAllocateInfo" "memoryTypeIndex"
  vkMemoryTypeIndex :: Word32
  }
  deriving (Show, Eq)
withCStructMemoryAllocateInfo :: MemoryAllocateInfo -> (VkMemoryAllocateInfo -> IO a) -> IO a
withCStructMemoryAllocateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: MemoryAllocateInfo)) (\pPNext -> cont (VkMemoryAllocateInfo VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO pPNext (vkAllocationSize (from :: MemoryAllocateInfo)) (vkMemoryTypeIndex (from :: MemoryAllocateInfo))))
fromCStructMemoryAllocateInfo :: VkMemoryAllocateInfo -> IO MemoryAllocateInfo
fromCStructMemoryAllocateInfo c = MemoryAllocateInfo <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryAllocateInfo)))
                                                     <*> pure (vkAllocationSize (c :: VkMemoryAllocateInfo))
                                                     <*> pure (vkMemoryTypeIndex (c :: VkMemoryAllocateInfo))
-- No documentation found for TopLevel "MemoryMapFlags"
type MemoryMapFlags = VkMemoryMapFlags

-- | Wrapper for 'vkAllocateMemory'
allocateMemory :: Device ->  MemoryAllocateInfo ->  Maybe AllocationCallbacks ->  IO ( DeviceMemory )
allocateMemory = \(Device device commandTable) -> \allocateInfo -> \allocator -> alloca (\pMemory -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructMemoryAllocateInfo a . flip with) allocateInfo (\pAllocateInfo -> Graphics.Vulkan.C.Dynamic.allocateMemory commandTable device pAllocateInfo pAllocator pMemory >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pMemory)))))

-- | Wrapper for 'vkFlushMappedMemoryRanges'
flushMappedMemoryRanges :: Device ->  Vector MappedMemoryRange ->  IO ()
flushMappedMemoryRanges = \(Device device commandTable) -> \memoryRanges -> withVec withCStructMappedMemoryRange memoryRanges (\pMemoryRanges -> Graphics.Vulkan.C.Dynamic.flushMappedMemoryRanges commandTable device (fromIntegral $ Data.Vector.length memoryRanges) pMemoryRanges >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for 'vkFreeMemory'
freeMemory :: Device ->  DeviceMemory ->  Maybe AllocationCallbacks ->  IO ()
freeMemory = \(Device device commandTable) -> \memory -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.freeMemory commandTable device memory pAllocator *> (pure ()))

-- | Wrapper for 'vkGetDeviceMemoryCommitment'
getDeviceMemoryCommitment :: Device ->  DeviceMemory ->  IO (DeviceSize)
getDeviceMemoryCommitment = \(Device device commandTable) -> \memory -> alloca (\pCommittedMemoryInBytes -> Graphics.Vulkan.C.Dynamic.getDeviceMemoryCommitment commandTable device memory pCommittedMemoryInBytes *> (peek pCommittedMemoryInBytes))

-- | Wrapper for 'vkInvalidateMappedMemoryRanges'
invalidateMappedMemoryRanges :: Device ->  Vector MappedMemoryRange ->  IO ()
invalidateMappedMemoryRanges = \(Device device commandTable) -> \memoryRanges -> withVec withCStructMappedMemoryRange memoryRanges (\pMemoryRanges -> Graphics.Vulkan.C.Dynamic.invalidateMappedMemoryRanges commandTable device (fromIntegral $ Data.Vector.length memoryRanges) pMemoryRanges >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for 'vkMapMemory'
mapMemory :: Device ->  DeviceMemory ->  DeviceSize ->  DeviceSize ->  MemoryMapFlags ->  IO ( Ptr () )
mapMemory = \(Device device commandTable) -> \memory -> \offset -> \size -> \flags -> alloca (\pPData -> Graphics.Vulkan.C.Dynamic.mapMemory commandTable device memory offset size flags pPData >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pPData)))

-- | Wrapper for 'vkUnmapMemory'
unmapMemory :: Device ->  DeviceMemory ->  IO ()
unmapMemory = \(Device device commandTable) -> \memory -> Graphics.Vulkan.C.Dynamic.unmapMemory commandTable device memory *> (pure ())
