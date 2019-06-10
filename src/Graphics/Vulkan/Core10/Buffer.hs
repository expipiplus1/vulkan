{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Buffer
  ( BufferCreateFlagBits
  , pattern BUFFER_CREATE_SPARSE_BINDING_BIT
  , pattern BUFFER_CREATE_SPARSE_RESIDENCY_BIT
  , pattern BUFFER_CREATE_SPARSE_ALIASED_BIT
  , pattern BUFFER_CREATE_PROTECTED_BIT
  , pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , BufferCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , BufferCreateInfo(..)
#endif
  , BufferUsageFlagBits
  , pattern BUFFER_USAGE_TRANSFER_SRC_BIT
  , pattern BUFFER_USAGE_TRANSFER_DST_BIT
  , pattern BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
  , pattern BUFFER_USAGE_UNIFORM_BUFFER_BIT
  , pattern BUFFER_USAGE_STORAGE_BUFFER_BIT
  , pattern BUFFER_USAGE_INDEX_BUFFER_BIT
  , pattern BUFFER_USAGE_VERTEX_BUFFER_BIT
  , pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT
  , pattern BUFFER_USAGE_RESERVED_15_BIT_KHR
  , pattern BUFFER_USAGE_RESERVED_16_BIT_KHR
  , pattern BUFFER_USAGE_RESERVED_13_BIT_KHR
  , pattern BUFFER_USAGE_RESERVED_14_BIT_KHR
  , pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
  , pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
  , pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern BUFFER_USAGE_RAY_TRACING_BIT_NV
  , pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  , BufferUsageFlags
  , SharingMode
  , pattern SHARING_MODE_EXCLUSIVE
  , pattern SHARING_MODE_CONCURRENT
  , createBuffer
  , destroyBuffer
  , withBuffer
  , pattern VK_BUFFER_USAGE_RESERVED_13_BIT_KHR
  , pattern VK_BUFFER_USAGE_RESERVED_14_BIT_KHR
  , pattern VK_BUFFER_USAGE_RESERVED_15_BIT_KHR
  , pattern VK_BUFFER_USAGE_RESERVED_16_BIT_KHR
  ) where

import Control.Exception
  ( bracket
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits(..)
  , VkBufferUsageFlagBits(..)
  , VkSharingMode(..)
  , vkCreateBuffer
  , vkDestroyBuffer
  , pattern VK_BUFFER_CREATE_SPARSE_ALIASED_BIT
  , pattern VK_BUFFER_CREATE_SPARSE_BINDING_BIT
  , pattern VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT
  , pattern VK_BUFFER_USAGE_INDEX_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_STORAGE_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_TRANSFER_DST_BIT
  , pattern VK_BUFFER_USAGE_TRANSFER_SRC_BIT
  , pattern VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
  , pattern VK_SHARING_MODE_CONCURRENT
  , pattern VK_SHARING_MODE_EXCLUSIVE
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( pattern VK_BUFFER_CREATE_PROTECTED_BIT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( pattern VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT
  , pattern VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( pattern VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT
  , pattern VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_BUFFER_USAGE_RAY_TRACING_BIT_NV
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( DeviceSize
  )
#endif
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "BufferCreateFlagBits"
type BufferCreateFlagBits = VkBufferCreateFlagBits


{-# complete BUFFER_CREATE_SPARSE_BINDING_BIT, BUFFER_CREATE_SPARSE_RESIDENCY_BIT, BUFFER_CREATE_SPARSE_ALIASED_BIT, BUFFER_CREATE_PROTECTED_BIT, BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT :: BufferCreateFlagBits #-}


-- No documentation found for Nested "BufferCreateFlagBits" "BUFFER_CREATE_SPARSE_BINDING_BIT"
pattern BUFFER_CREATE_SPARSE_BINDING_BIT :: (a ~ BufferCreateFlagBits) => a
pattern BUFFER_CREATE_SPARSE_BINDING_BIT = VK_BUFFER_CREATE_SPARSE_BINDING_BIT


-- No documentation found for Nested "BufferCreateFlagBits" "BUFFER_CREATE_SPARSE_RESIDENCY_BIT"
pattern BUFFER_CREATE_SPARSE_RESIDENCY_BIT :: (a ~ BufferCreateFlagBits) => a
pattern BUFFER_CREATE_SPARSE_RESIDENCY_BIT = VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT


-- No documentation found for Nested "BufferCreateFlagBits" "BUFFER_CREATE_SPARSE_ALIASED_BIT"
pattern BUFFER_CREATE_SPARSE_ALIASED_BIT :: (a ~ BufferCreateFlagBits) => a
pattern BUFFER_CREATE_SPARSE_ALIASED_BIT = VK_BUFFER_CREATE_SPARSE_ALIASED_BIT


-- No documentation found for Nested "BufferCreateFlagBits" "BUFFER_CREATE_PROTECTED_BIT"
pattern BUFFER_CREATE_PROTECTED_BIT :: (a ~ BufferCreateFlagBits) => a
pattern BUFFER_CREATE_PROTECTED_BIT = VK_BUFFER_CREATE_PROTECTED_BIT


-- No documentation found for Nested "BufferCreateFlagBits" "BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT"
pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT :: (a ~ BufferCreateFlagBits) => a
pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT = VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT

-- No documentation found for TopLevel "BufferCreateFlags"
type BufferCreateFlags = BufferCreateFlagBits


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkBufferCreateInfo"
data BufferCreateInfo = BufferCreateInfo
  { -- No documentation found for Nested "BufferCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferCreateInfo" "flags"
  flags :: BufferCreateFlags
  , -- No documentation found for Nested "BufferCreateInfo" "size"
  size :: DeviceSize
  , -- No documentation found for Nested "BufferCreateInfo" "usage"
  usage :: BufferUsageFlags
  , -- No documentation found for Nested "BufferCreateInfo" "sharingMode"
  sharingMode :: SharingMode
  , -- No documentation found for Nested "BufferCreateInfo" "pQueueFamilyIndices"
  queueFamilyIndices :: Vector Word32
  }
  deriving (Show, Eq)

instance Zero BufferCreateInfo where
  zero = BufferCreateInfo Nothing
                          zero
                          zero
                          zero
                          zero
                          mempty

#endif

-- No documentation found for TopLevel "BufferUsageFlagBits"
type BufferUsageFlagBits = VkBufferUsageFlagBits


{-# complete BUFFER_USAGE_TRANSFER_SRC_BIT, BUFFER_USAGE_TRANSFER_DST_BIT, BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT, BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT, BUFFER_USAGE_UNIFORM_BUFFER_BIT, BUFFER_USAGE_STORAGE_BUFFER_BIT, BUFFER_USAGE_INDEX_BUFFER_BIT, BUFFER_USAGE_VERTEX_BUFFER_BIT, BUFFER_USAGE_INDIRECT_BUFFER_BIT, BUFFER_USAGE_RESERVED_15_BIT_KHR, BUFFER_USAGE_RESERVED_16_BIT_KHR, BUFFER_USAGE_RESERVED_13_BIT_KHR, BUFFER_USAGE_RESERVED_14_BIT_KHR, BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT, BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT, BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT, BUFFER_USAGE_RAY_TRACING_BIT_NV, BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT :: BufferUsageFlagBits #-}


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_TRANSFER_SRC_BIT"
pattern BUFFER_USAGE_TRANSFER_SRC_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_TRANSFER_SRC_BIT = VK_BUFFER_USAGE_TRANSFER_SRC_BIT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_TRANSFER_DST_BIT"
pattern BUFFER_USAGE_TRANSFER_DST_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_TRANSFER_DST_BIT = VK_BUFFER_USAGE_TRANSFER_DST_BIT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT"
pattern BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT = VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT"
pattern BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT = VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_UNIFORM_BUFFER_BIT"
pattern BUFFER_USAGE_UNIFORM_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_UNIFORM_BUFFER_BIT = VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_STORAGE_BUFFER_BIT"
pattern BUFFER_USAGE_STORAGE_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_STORAGE_BUFFER_BIT = VK_BUFFER_USAGE_STORAGE_BUFFER_BIT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_INDEX_BUFFER_BIT"
pattern BUFFER_USAGE_INDEX_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_INDEX_BUFFER_BIT = VK_BUFFER_USAGE_INDEX_BUFFER_BIT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_VERTEX_BUFFER_BIT"
pattern BUFFER_USAGE_VERTEX_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_VERTEX_BUFFER_BIT = VK_BUFFER_USAGE_VERTEX_BUFFER_BIT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_INDIRECT_BUFFER_BIT"
pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_INDIRECT_BUFFER_BIT = VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_RESERVED_15_BIT_KHR"
pattern BUFFER_USAGE_RESERVED_15_BIT_KHR :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_RESERVED_15_BIT_KHR = VK_BUFFER_USAGE_RESERVED_15_BIT_KHR


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_RESERVED_16_BIT_KHR"
pattern BUFFER_USAGE_RESERVED_16_BIT_KHR :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_RESERVED_16_BIT_KHR = VK_BUFFER_USAGE_RESERVED_16_BIT_KHR


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_RESERVED_13_BIT_KHR"
pattern BUFFER_USAGE_RESERVED_13_BIT_KHR :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_RESERVED_13_BIT_KHR = VK_BUFFER_USAGE_RESERVED_13_BIT_KHR


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_RESERVED_14_BIT_KHR"
pattern BUFFER_USAGE_RESERVED_14_BIT_KHR :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_RESERVED_14_BIT_KHR = VK_BUFFER_USAGE_RESERVED_14_BIT_KHR


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT = VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT"
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT = VK_BUFFER_USAGE_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT"
pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT = VK_BUFFER_USAGE_CONDITIONAL_RENDERING_BIT_EXT


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_RAY_TRACING_BIT_NV"
pattern BUFFER_USAGE_RAY_TRACING_BIT_NV :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_RAY_TRACING_BIT_NV = VK_BUFFER_USAGE_RAY_TRACING_BIT_NV


-- No documentation found for Nested "BufferUsageFlagBits" "BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT"
pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT :: (a ~ BufferUsageFlagBits) => a
pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT = VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT

-- No documentation found for TopLevel "BufferUsageFlags"
type BufferUsageFlags = BufferUsageFlagBits

-- No documentation found for TopLevel "SharingMode"
type SharingMode = VkSharingMode


{-# complete SHARING_MODE_EXCLUSIVE, SHARING_MODE_CONCURRENT :: SharingMode #-}


-- No documentation found for Nested "SharingMode" "SHARING_MODE_EXCLUSIVE"
pattern SHARING_MODE_EXCLUSIVE :: (a ~ SharingMode) => a
pattern SHARING_MODE_EXCLUSIVE = VK_SHARING_MODE_EXCLUSIVE


-- No documentation found for Nested "SharingMode" "SHARING_MODE_CONCURRENT"
pattern SHARING_MODE_CONCURRENT :: (a ~ SharingMode) => a
pattern SHARING_MODE_CONCURRENT = VK_SHARING_MODE_CONCURRENT


-- No documentation found for TopLevel "vkCreateBuffer"
createBuffer :: Device ->  BufferCreateInfo ->  Maybe AllocationCallbacks ->  IO (Buffer)
createBuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyBuffer"
destroyBuffer :: Device ->  Buffer ->  Maybe AllocationCallbacks ->  IO ()
destroyBuffer = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createBuffer' and 'destroyBuffer' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withBuffer
  :: Device -> BufferCreateInfo -> Maybe AllocationCallbacks -> (Buffer -> IO a) -> IO a
withBuffer device bufferCreateInfo allocationCallbacks = bracket
  (createBuffer device bufferCreateInfo allocationCallbacks)
  (\o -> destroyBuffer device o allocationCallbacks)

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_RESERVED_13_BIT_KHR"
pattern VK_BUFFER_USAGE_RESERVED_13_BIT_KHR :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_RESERVED_13_BIT_KHR = VkBufferUsageFlagBits 0x00002000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_RESERVED_14_BIT_KHR"
pattern VK_BUFFER_USAGE_RESERVED_14_BIT_KHR :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_RESERVED_14_BIT_KHR = VkBufferUsageFlagBits 0x00004000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_RESERVED_15_BIT_KHR"
pattern VK_BUFFER_USAGE_RESERVED_15_BIT_KHR :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_RESERVED_15_BIT_KHR = VkBufferUsageFlagBits 0x00008000

-- No documentation found for Nested "VkBufferUsageFlagBits" "VK_BUFFER_USAGE_RESERVED_16_BIT_KHR"
pattern VK_BUFFER_USAGE_RESERVED_16_BIT_KHR :: VkBufferUsageFlagBits
pattern VK_BUFFER_USAGE_RESERVED_16_BIT_KHR = VkBufferUsageFlagBits 0x00010000
