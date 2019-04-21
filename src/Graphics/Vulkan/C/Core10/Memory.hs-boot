{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  , VkMappedMemoryRange
  , VkMemoryAllocateInfo
  , VkMemoryMapFlags
  , FN_vkAllocateMemory
  , PFN_vkAllocateMemory
  , FN_vkFlushMappedMemoryRanges
  , PFN_vkFlushMappedMemoryRanges
  , FN_vkFreeMemory
  , PFN_vkFreeMemory
  , FN_vkGetDeviceMemoryCommitment
  , PFN_vkGetDeviceMemoryCommitment
  , FN_vkInvalidateMappedMemoryRanges
  , PFN_vkInvalidateMappedMemoryRanges
  , FN_vkMapMemory
  , PFN_vkMapMemory
  , FN_vkUnmapMemory
  , PFN_vkUnmapMemory
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  , VkDeviceSize
  )


-- | Dummy data to tag the 'Ptr' with
data VkDeviceMemory_T
-- | VkDeviceMemory - Opaque handle to a device memory object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'VkMappedMemoryRange',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'vkAllocateMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'vkFreeMemory', 'vkGetDeviceMemoryCommitment', 'vkMapMemory',
-- 'vkUnmapMemory'
type VkDeviceMemory = Ptr VkDeviceMemory_T

data VkMappedMemoryRange

data VkMemoryAllocateInfo

data VkMemoryMapFlags

type FN_vkAllocateMemory = ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult
type PFN_vkAllocateMemory = FunPtr FN_vkAllocateMemory

type FN_vkFlushMappedMemoryRanges = ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
type PFN_vkFlushMappedMemoryRanges = FunPtr FN_vkFlushMappedMemoryRanges

type FN_vkFreeMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkFreeMemory = FunPtr FN_vkFreeMemory

type FN_vkGetDeviceMemoryCommitment = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()
type PFN_vkGetDeviceMemoryCommitment = FunPtr FN_vkGetDeviceMemoryCommitment

type FN_vkInvalidateMappedMemoryRanges = ("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult
type PFN_vkInvalidateMappedMemoryRanges = FunPtr FN_vkInvalidateMappedMemoryRanges

type FN_vkMapMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult
type PFN_vkMapMemory = FunPtr FN_vkMapMemory

type FN_vkUnmapMemory = ("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()
type PFN_vkUnmapMemory = FunPtr FN_vkUnmapMemory
