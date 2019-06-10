{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.DescriptorSet
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  CopyDescriptorSet(..)
  , 
#endif
  DescriptorBufferInfo(..)
  , DescriptorImageInfo(..)
  , DescriptorPool
  , DescriptorPoolCreateFlagBits
  , pattern DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
  , pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , DescriptorPoolCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , DescriptorPoolCreateInfo(..)
#endif
  , DescriptorPoolResetFlags
  , DescriptorPoolSize(..)
  , DescriptorSet
#if defined(VK_USE_PLATFORM_GGP)
  , DescriptorSetAllocateInfo(..)
#endif
  , DescriptorSetLayoutBinding(..)
  , DescriptorSetLayoutCreateFlagBits
  , pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  , pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  , DescriptorSetLayoutCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , DescriptorSetLayoutCreateInfo(..)
#endif
  , DescriptorType
  , pattern DESCRIPTOR_TYPE_SAMPLER
  , pattern DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  , pattern DESCRIPTOR_TYPE_SAMPLED_IMAGE
  , pattern DESCRIPTOR_TYPE_STORAGE_IMAGE
  , pattern DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  , pattern DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  , pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER
  , pattern DESCRIPTOR_TYPE_STORAGE_BUFFER
  , pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
  , pattern DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
  , pattern DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  , pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  , pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
#if defined(VK_USE_PLATFORM_GGP)
  , WriteDescriptorSet(..)
#endif
  , allocateDescriptorSets
  , createDescriptorPool
  , createDescriptorSetLayout
  , destroyDescriptorPool
  , destroyDescriptorSetLayout
  , freeDescriptorSets
  , resetDescriptorPool
  , updateDescriptorSets
  , withDescriptorPool
  , withDescriptorSetLayout
  , withDescriptorSets
  ) where

import Control.Exception
  ( bracket
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
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorPoolCreateFlagBits(..)
  , VkDescriptorPoolResetFlags(..)
  , VkDescriptorSetLayoutCreateFlagBits(..)
  , VkDescriptorType(..)
  , VkDescriptorPool
  , VkDescriptorSet
  , vkAllocateDescriptorSets
  , vkCreateDescriptorPool
  , vkCreateDescriptorSetLayout
  , vkDestroyDescriptorPool
  , vkDestroyDescriptorSetLayout
  , vkFreeDescriptorSets
  , vkResetDescriptorPool
  , vkUpdateDescriptorSets
  , pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
  , pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  , pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  , pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
  , pattern VK_DESCRIPTOR_TYPE_SAMPLER
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block
  ( pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.BufferView
  ( BufferView
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  )
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageView
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( DescriptorSetLayout
  , ShaderStageFlags
  )
import Graphics.Vulkan.Core10.Sampler
  ( Sampler
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkCopyDescriptorSet"
data CopyDescriptorSet = CopyDescriptorSet
  { -- No documentation found for Nested "CopyDescriptorSet" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CopyDescriptorSet" "srcSet"
  srcSet :: DescriptorSet
  , -- No documentation found for Nested "CopyDescriptorSet" "srcBinding"
  srcBinding :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "srcArrayElement"
  srcArrayElement :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "dstSet"
  dstSet :: DescriptorSet
  , -- No documentation found for Nested "CopyDescriptorSet" "dstBinding"
  dstBinding :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "dstArrayElement"
  dstArrayElement :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "descriptorCount"
  descriptorCount :: Word32
  }
  deriving (Show, Eq)

instance Zero CopyDescriptorSet where
  zero = CopyDescriptorSet Nothing
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero

#endif


-- No documentation found for TopLevel "VkDescriptorBufferInfo"
data DescriptorBufferInfo = DescriptorBufferInfo
  { -- No documentation found for Nested "DescriptorBufferInfo" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "DescriptorBufferInfo" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "DescriptorBufferInfo" "range"
  range :: DeviceSize
  }
  deriving (Show, Eq)

instance Zero DescriptorBufferInfo where
  zero = DescriptorBufferInfo zero
                              zero
                              zero



-- No documentation found for TopLevel "VkDescriptorImageInfo"
data DescriptorImageInfo = DescriptorImageInfo
  { -- No documentation found for Nested "DescriptorImageInfo" "sampler"
  sampler :: Sampler
  , -- No documentation found for Nested "DescriptorImageInfo" "imageView"
  imageView :: ImageView
  , -- No documentation found for Nested "DescriptorImageInfo" "imageLayout"
  imageLayout :: ImageLayout
  }
  deriving (Show, Eq)

instance Zero DescriptorImageInfo where
  zero = DescriptorImageInfo zero
                             zero
                             zero


-- No documentation found for TopLevel "DescriptorPool"
type DescriptorPool = VkDescriptorPool

-- No documentation found for TopLevel "DescriptorPoolCreateFlagBits"
type DescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits


{-# complete DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT, DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT :: DescriptorPoolCreateFlagBits #-}


-- No documentation found for Nested "DescriptorPoolCreateFlagBits" "DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT"
pattern DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT :: (a ~ DescriptorPoolCreateFlagBits) => a
pattern DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT


-- No documentation found for Nested "DescriptorPoolCreateFlagBits" "DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT"
pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT :: (a ~ DescriptorPoolCreateFlagBits) => a
pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT = VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT

-- No documentation found for TopLevel "DescriptorPoolCreateFlags"
type DescriptorPoolCreateFlags = DescriptorPoolCreateFlagBits


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDescriptorPoolCreateInfo"
data DescriptorPoolCreateInfo = DescriptorPoolCreateInfo
  { -- No documentation found for Nested "DescriptorPoolCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorPoolCreateInfo" "flags"
  flags :: DescriptorPoolCreateFlags
  , -- No documentation found for Nested "DescriptorPoolCreateInfo" "maxSets"
  maxSets :: Word32
  , -- No documentation found for Nested "DescriptorPoolCreateInfo" "pPoolSizes"
  poolSizes :: Vector DescriptorPoolSize
  }
  deriving (Show, Eq)

instance Zero DescriptorPoolCreateInfo where
  zero = DescriptorPoolCreateInfo Nothing
                                  zero
                                  zero
                                  mempty

#endif

-- No documentation found for TopLevel "DescriptorPoolResetFlags"
type DescriptorPoolResetFlags = VkDescriptorPoolResetFlags


-- No complete pragma for DescriptorPoolResetFlags as it has no patterns


-- No documentation found for TopLevel "VkDescriptorPoolSize"
data DescriptorPoolSize = DescriptorPoolSize
  { -- No documentation found for Nested "DescriptorPoolSize" "type"
  type' :: DescriptorType
  , -- No documentation found for Nested "DescriptorPoolSize" "descriptorCount"
  descriptorCount :: Word32
  }
  deriving (Show, Eq)

instance Zero DescriptorPoolSize where
  zero = DescriptorPoolSize zero
                            zero


-- No documentation found for TopLevel "DescriptorSet"
type DescriptorSet = VkDescriptorSet


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDescriptorSetAllocateInfo"
data DescriptorSetAllocateInfo = DescriptorSetAllocateInfo
  { -- No documentation found for Nested "DescriptorSetAllocateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetAllocateInfo" "descriptorPool"
  descriptorPool :: DescriptorPool
  , -- No documentation found for Nested "DescriptorSetAllocateInfo" "pSetLayouts"
  setLayouts :: Vector DescriptorSetLayout
  }
  deriving (Show, Eq)

instance Zero DescriptorSetAllocateInfo where
  zero = DescriptorSetAllocateInfo Nothing
                                   zero
                                   mempty

#endif


-- No documentation found for TopLevel "VkDescriptorSetLayoutBinding"
data DescriptorSetLayoutBinding = DescriptorSetLayoutBinding
  { -- No documentation found for Nested "DescriptorSetLayoutBinding" "binding"
  binding :: Word32
  , -- No documentation found for Nested "DescriptorSetLayoutBinding" "descriptorType"
  descriptorType :: DescriptorType
  , -- No documentation found for Nested "DescriptorSetLayoutBinding" "stageFlags"
  stageFlags :: ShaderStageFlags
  , -- No documentation found for Nested "DescriptorSetLayoutBinding" "pImmutableSamplers"
  immutableSamplers :: Either Word32 (Vector Sampler)
  }
  deriving (Show, Eq)

instance Zero DescriptorSetLayoutBinding where
  zero = DescriptorSetLayoutBinding zero
                                    zero
                                    zero
                                    (Left 0)


-- No documentation found for TopLevel "DescriptorSetLayoutCreateFlagBits"
type DescriptorSetLayoutCreateFlagBits = VkDescriptorSetLayoutCreateFlagBits


{-# complete DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR, DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT :: DescriptorSetLayoutCreateFlagBits #-}


-- No documentation found for Nested "DescriptorSetLayoutCreateFlagBits" "DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR"
pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR :: (a ~ DescriptorSetLayoutCreateFlagBits) => a
pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR = VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR


-- No documentation found for Nested "DescriptorSetLayoutCreateFlagBits" "DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT"
pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT :: (a ~ DescriptorSetLayoutCreateFlagBits) => a
pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT = VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT

-- No documentation found for TopLevel "DescriptorSetLayoutCreateFlags"
type DescriptorSetLayoutCreateFlags = DescriptorSetLayoutCreateFlagBits


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDescriptorSetLayoutCreateInfo"
data DescriptorSetLayoutCreateInfo = DescriptorSetLayoutCreateInfo
  { -- No documentation found for Nested "DescriptorSetLayoutCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetLayoutCreateInfo" "flags"
  flags :: DescriptorSetLayoutCreateFlags
  , -- No documentation found for Nested "DescriptorSetLayoutCreateInfo" "pBindings"
  bindings :: Vector DescriptorSetLayoutBinding
  }
  deriving (Show, Eq)

instance Zero DescriptorSetLayoutCreateInfo where
  zero = DescriptorSetLayoutCreateInfo Nothing
                                       zero
                                       mempty

#endif

-- No documentation found for TopLevel "DescriptorType"
type DescriptorType = VkDescriptorType


{-# complete DESCRIPTOR_TYPE_SAMPLER, DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, DESCRIPTOR_TYPE_SAMPLED_IMAGE, DESCRIPTOR_TYPE_STORAGE_IMAGE, DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER, DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER, DESCRIPTOR_TYPE_UNIFORM_BUFFER, DESCRIPTOR_TYPE_STORAGE_BUFFER, DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC, DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, DESCRIPTOR_TYPE_INPUT_ATTACHMENT, DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT, DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV :: DescriptorType #-}


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_SAMPLER"
pattern DESCRIPTOR_TYPE_SAMPLER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_SAMPLER = VK_DESCRIPTOR_TYPE_SAMPLER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
pattern DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_SAMPLED_IMAGE"
pattern DESCRIPTOR_TYPE_SAMPLED_IMAGE :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_SAMPLED_IMAGE = VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_STORAGE_IMAGE"
pattern DESCRIPTOR_TYPE_STORAGE_IMAGE :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_STORAGE_IMAGE = VK_DESCRIPTOR_TYPE_STORAGE_IMAGE


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
pattern DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
pattern DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_UNIFORM_BUFFER"
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_STORAGE_BUFFER"
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
pattern DESCRIPTOR_TYPE_INPUT_ATTACHMENT :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_INPUT_ATTACHMENT = VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT"
pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT = VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV = VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkWriteDescriptorSet"
data WriteDescriptorSet = WriteDescriptorSet
  { -- No documentation found for Nested "WriteDescriptorSet" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "WriteDescriptorSet" "dstSet"
  dstSet :: DescriptorSet
  , -- No documentation found for Nested "WriteDescriptorSet" "dstBinding"
  dstBinding :: Word32
  , -- No documentation found for Nested "WriteDescriptorSet" "dstArrayElement"
  dstArrayElement :: Word32
  , -- No documentation found for Nested "WriteDescriptorSet" "descriptorType"
  descriptorType :: DescriptorType
  , -- No documentation found for Nested "WriteDescriptorSet" "pImageInfo"
  imageInfo :: Vector DescriptorImageInfo
  , -- No documentation found for Nested "WriteDescriptorSet" "pBufferInfo"
  bufferInfo :: Vector DescriptorBufferInfo
  , -- No documentation found for Nested "WriteDescriptorSet" "pTexelBufferView"
  texelBufferView :: Vector BufferView
  }
  deriving (Show, Eq)

instance Zero WriteDescriptorSet where
  zero = WriteDescriptorSet Nothing
                            zero
                            zero
                            zero
                            zero
                            mempty
                            mempty
                            mempty

#endif


-- No documentation found for TopLevel "vkAllocateDescriptorSets"
allocateDescriptorSets :: Device ->  DescriptorSetAllocateInfo ->  IO (Vector DescriptorSet)
allocateDescriptorSets = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateDescriptorPool"
createDescriptorPool :: Device ->  DescriptorPoolCreateInfo ->  Maybe AllocationCallbacks ->  IO (DescriptorPool)
createDescriptorPool = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkCreateDescriptorSetLayout"
createDescriptorSetLayout :: Device ->  DescriptorSetLayoutCreateInfo ->  Maybe AllocationCallbacks ->  IO (DescriptorSetLayout)
createDescriptorSetLayout = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyDescriptorPool"
destroyDescriptorPool :: Device ->  DescriptorPool ->  Maybe AllocationCallbacks ->  IO ()
destroyDescriptorPool = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyDescriptorSetLayout"
destroyDescriptorSetLayout :: Device ->  DescriptorSetLayout ->  Maybe AllocationCallbacks ->  IO ()
destroyDescriptorSetLayout = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkFreeDescriptorSets"
freeDescriptorSets :: Device ->  DescriptorPool ->  Vector DescriptorSet ->  IO ()
freeDescriptorSets = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkResetDescriptorPool"
resetDescriptorPool :: Device ->  DescriptorPool ->  DescriptorPoolResetFlags ->  IO ()
resetDescriptorPool = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkUpdateDescriptorSets"
updateDescriptorSets :: Device ->  Vector WriteDescriptorSet ->  Vector CopyDescriptorSet ->  IO ()
updateDescriptorSets = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createDescriptorPool' and 'destroyDescriptorPool' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDescriptorPool
  :: Device -> DescriptorPoolCreateInfo -> Maybe AllocationCallbacks -> (DescriptorPool -> IO a) -> IO a
withDescriptorPool device descriptorPoolCreateInfo allocationCallbacks = bracket
  (createDescriptorPool device descriptorPoolCreateInfo allocationCallbacks)
  (\o -> destroyDescriptorPool device o allocationCallbacks)

-- | A safe wrapper for 'createDescriptorSetLayout' and 'destroyDescriptorSetLayout' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDescriptorSetLayout
  :: Device -> DescriptorSetLayoutCreateInfo -> Maybe AllocationCallbacks -> (DescriptorSetLayout -> IO a) -> IO a
withDescriptorSetLayout device descriptorSetLayoutCreateInfo allocationCallbacks = bracket
  (createDescriptorSetLayout device descriptorSetLayoutCreateInfo allocationCallbacks)
  (\o -> destroyDescriptorSetLayout device o allocationCallbacks)

-- | A safe wrapper for 'allocateDescriptorSets' and 'freeDescriptorSets' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDescriptorSets
  :: Device -> DescriptorSetAllocateInfo -> (Vector DescriptorSet -> IO a) -> IO a
withDescriptorSets device descriptorSetAllocateInfo = bracket
  (allocateDescriptorSets device descriptorSetAllocateInfo)
  (\o -> freeDescriptorSets device (descriptorPool (descriptorSetAllocateInfo :: DescriptorSetAllocateInfo))  o)
