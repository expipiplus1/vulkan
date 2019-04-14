{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  , VkMemoryRequirements(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkBindBufferMemory
#endif
  , FN_vkBindBufferMemory
  , PFN_vkBindBufferMemory
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkBindImageMemory
#endif
  , FN_vkBindImageMemory
  , PFN_vkBindImageMemory
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetBufferMemoryRequirements
#endif
  , FN_vkGetBufferMemoryRequirements
  , PFN_vkGetBufferMemoryRequirements
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetImageMemoryRequirements
#endif
  , FN_vkGetImageMemoryRequirements
  , PFN_vkGetImageMemoryRequirements
  ) where

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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkBuffer_T
-- No documentation found for TopLevel "VkBuffer"
type VkBuffer = Ptr VkBuffer_T
-- | Dummy data to tag the 'Ptr' with
data VkImage_T
-- No documentation found for TopLevel "VkImage"
type VkImage = Ptr VkImage_T
-- No documentation found for TopLevel "VkMemoryRequirements"
data VkMemoryRequirements = VkMemoryRequirements
  { -- No documentation found for Nested "VkMemoryRequirements" "size"
  vkSize :: VkDeviceSize
  , -- No documentation found for Nested "VkMemoryRequirements" "alignment"
  vkAlignment :: VkDeviceSize
  , -- No documentation found for Nested "VkMemoryRequirements" "memoryTypeBits"
  vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryRequirements <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSize (poked :: VkMemoryRequirements))
                *> poke (ptr `plusPtr` 8) (vkAlignment (poked :: VkMemoryRequirements))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryRequirements))
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkBindBufferMemory"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindBufferMemory" vkBindBufferMemory :: ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult

#endif
type FN_vkBindBufferMemory = ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult
type PFN_vkBindBufferMemory = FunPtr FN_vkBindBufferMemory
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkBindImageMemory"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindImageMemory" vkBindImageMemory :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult

#endif
type FN_vkBindImageMemory = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult
type PFN_vkBindImageMemory = FunPtr FN_vkBindImageMemory
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkGetBufferMemoryRequirements"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetBufferMemoryRequirements" vkGetBufferMemoryRequirements :: ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()

#endif
type FN_vkGetBufferMemoryRequirements = ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()
type PFN_vkGetBufferMemoryRequirements = FunPtr FN_vkGetBufferMemoryRequirements
#if defined(EXPOSE_CORE10_COMMANDS)
-- No documentation found for TopLevel "vkGetImageMemoryRequirements"
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageMemoryRequirements" vkGetImageMemoryRequirements :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()

#endif
type FN_vkGetImageMemoryRequirements = ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()
type PFN_vkGetImageMemoryRequirements = FunPtr FN_vkGetImageMemoryRequirements
