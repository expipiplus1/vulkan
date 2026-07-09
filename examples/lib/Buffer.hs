-- | VMA buffer helpers for the examples

module Buffer
  ( deviceBuffer
  , readbackBuffer
  , storageBuffer
  , stagingBuffer
  ) where

import Control.Monad.Trans.Resource (MonadResource, ReleaseKey, allocate)
import Data.Bits ((.|.))
import qualified Data.Vector as V
import Data.Word (Word32)
import Foreign.Ptr (Ptr)
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

{- | A GPU-only buffer of the given size and usage. @sharedFamilies@ makes it
CONCURRENT across those queue families (for a cross-queue read with no ownership
transfer); 'Nothing' leaves it EXCLUSIVE.
-}
deviceBuffer :: (MonadResource m) => VMA.Allocator -> Vk.DeviceSize -> Vk.BufferUsageFlags -> Maybe [Word32] -> m (ReleaseKey, Vk.Buffer)
deviceBuffer allocator size usage sharedFamilies = do
  (key, (buffer, _, _)) <- VMA.withBuffer allocator bufferInfo gpuAlloc allocate
  pure (key, buffer)
  where
    bufferInfo =
      zero
        { Vk.size = size
        , Vk.usage = usage
        , Vk.sharingMode = maybe Vk.SHARING_MODE_EXCLUSIVE (const Vk.SHARING_MODE_CONCURRENT) sharedFamilies
        , Vk.queueFamilyIndices = maybe V.empty V.fromList sharedFamilies
        }
        :: Vk.BufferCreateInfo '[]
    gpuAlloc = zero{AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_ONLY}

-- | A CPU-visible, @TRANSFER_DST@ buffer for reading device data back to the host.
readbackBuffer :: (MonadResource m) => VMA.Allocator -> Vk.DeviceSize -> m (ReleaseKey, (Vk.Buffer, VMA.Allocation, Ptr ()))
readbackBuffer allocator size = do
  (key, (buffer, allocation, info)) <- VMA.withBuffer allocator bufferInfo hostAlloc allocate
  pure (key, (buffer, allocation, VMA.mappedData info))
  where
    bufferInfo = zero{Vk.size = size, Vk.usage = Vk.BUFFER_USAGE_TRANSFER_DST_BIT} :: Vk.BufferCreateInfo '[]
    hostAlloc =
      zero
        { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_TO_CPU
        }

{- | A CPU-visible, mapped @STORAGE@ buffer a compute shader writes and the host
reads back directly; returns the buffer, its allocation (GPU→CPU memory may be
non-coherent — 'VMA.invalidateAllocation' before host reads) and a pointer to
its mapped memory.
-}
storageBuffer :: (MonadResource m) => VMA.Allocator -> Vk.DeviceSize -> m (ReleaseKey, (Vk.Buffer, VMA.Allocation, Ptr ()))
storageBuffer allocator size = do
  (key, (buffer, allocation, info)) <- VMA.withBuffer allocator bufferInfo hostAlloc allocate
  pure (key, (buffer, allocation, VMA.mappedData info))
  where
    -- TRANSFER_DST so the buffer can be seeded (cmdFillBuffer) before first use.
    bufferInfo = zero{Vk.size = size, Vk.usage = Vk.BUFFER_USAGE_STORAGE_BUFFER_BIT .|. Vk.BUFFER_USAGE_TRANSFER_DST_BIT} :: Vk.BufferCreateInfo '[]
    hostAlloc =
      zero
        { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_GPU_TO_CPU
        }

{- | A CPU-visible, mapped @TRANSFER_SRC@ staging buffer for bulk CPU → GPU uploads
(host-poke the mapped memory, then 'Vk.cmdCopyBuffer' into a GPU-only buffer).
Returns the buffer and a pointer to its mapped memory.
-}
stagingBuffer :: (MonadResource m) => VMA.Allocator -> Vk.DeviceSize -> m (ReleaseKey, (Vk.Buffer, Ptr ()))
stagingBuffer allocator size = do
  (key, (buffer, _, info)) <- VMA.withBuffer allocator bufferInfo hostAlloc allocate
  pure (key, (buffer, VMA.mappedData info))
  where
    bufferInfo = zero{Vk.size = size, Vk.usage = Vk.BUFFER_USAGE_TRANSFER_SRC_BIT} :: Vk.BufferCreateInfo '[]
    hostAlloc =
      zero
        { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        }
