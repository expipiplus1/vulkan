-- | Persistently mapped buffers the CPU writes and the GPU reads where they lie.
module Mapped
  ( allocateBuffer
  ) where

import Control.Monad.Trans.Resource (ResourceT, allocate)
import Data.Bits ((.|.))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, sizeOf)
import qualified Vulkan.Core10 as Vk
import Vulkan.Zero (zero)
import qualified VulkanMemoryAllocator as AllocationCreateInfo (AllocationCreateInfo (..))
import qualified VulkanMemoryAllocator as VMA

{- | Room for @count@ elements, host-coherent: a write lands before any command
submitted after it, with no barrier of its own.
-}
allocateBuffer :: forall a. (Storable a) => VMA.Allocator -> Vk.BufferUsageFlags -> Int -> ResourceT IO (Vk.Buffer, Ptr a)
allocateBuffer vma usage count = do
  (_, (buffer, _, info)) <-
    VMA.withBuffer
      vma
      zero
        { Vk.size = fromIntegral (count * sizeOf (undefined :: a))
        , Vk.usage = usage
        }
      zero
        { AllocationCreateInfo.flags = VMA.ALLOCATION_CREATE_MAPPED_BIT
        , AllocationCreateInfo.usage = VMA.MEMORY_USAGE_CPU_TO_GPU
        , AllocationCreateInfo.requiredFlags =
            Vk.MEMORY_PROPERTY_HOST_VISIBLE_BIT .|. Vk.MEMORY_PROPERTY_HOST_COHERENT_BIT
        }
      allocate
  pure (buffer, castPtr (VMA.mappedData info))
