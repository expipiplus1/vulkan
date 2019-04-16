{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.DescriptorSet
  ( withCStructCopyDescriptorSet
  , fromCStructCopyDescriptorSet
  , CopyDescriptorSet(..)
  , withCStructDescriptorBufferInfo
  , fromCStructDescriptorBufferInfo
  , DescriptorBufferInfo(..)
  , withCStructDescriptorImageInfo
  , fromCStructDescriptorImageInfo
  , DescriptorImageInfo(..)
  , DescriptorPool
  , DescriptorPoolCreateFlagBits
  , DescriptorPoolCreateFlags
  , withCStructDescriptorPoolCreateInfo
  , fromCStructDescriptorPoolCreateInfo
  , DescriptorPoolCreateInfo(..)
  , DescriptorPoolResetFlags
  , withCStructDescriptorPoolSize
  , fromCStructDescriptorPoolSize
  , DescriptorPoolSize(..)
  , DescriptorSet
  , withCStructDescriptorSetAllocateInfo
  , fromCStructDescriptorSetAllocateInfo
  , DescriptorSetAllocateInfo(..)
  , withCStructDescriptorSetLayoutBinding
  , fromCStructDescriptorSetLayoutBinding
  , DescriptorSetLayoutBinding(..)
  , DescriptorSetLayoutCreateFlagBits
  , DescriptorSetLayoutCreateFlags
  , withCStructDescriptorSetLayoutCreateInfo
  , fromCStructDescriptorSetLayoutCreateInfo
  , DescriptorSetLayoutCreateInfo(..)
  , DescriptorType
  , withCStructWriteDescriptorSet
  , fromCStructWriteDescriptorSet
  , WriteDescriptorSet(..)
  , allocateDescriptorSets
  , createDescriptorPool
  , createDescriptorSetLayout
  , destroyDescriptorPool
  , destroyDescriptorSetLayout
  , freeDescriptorSets
  , resetDescriptorPool
  , updateDescriptorSets
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
  )
import Data.List
  ( minimum
  )
import Data.Maybe
  ( maybe
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
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( allocateDescriptorSets
  , createDescriptorPool
  , createDescriptorSetLayout
  , destroyDescriptorPool
  , destroyDescriptorSetLayout
  , freeDescriptorSets
  , resetDescriptorPool
  , updateDescriptorSets
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkCopyDescriptorSet(..)
  , VkDescriptorBufferInfo(..)
  , VkDescriptorImageInfo(..)
  , VkDescriptorPoolCreateFlagBits(..)
  , VkDescriptorPoolCreateInfo(..)
  , VkDescriptorPoolResetFlags(..)
  , VkDescriptorPoolSize(..)
  , VkDescriptorSetAllocateInfo(..)
  , VkDescriptorSetLayoutBinding(..)
  , VkDescriptorSetLayoutCreateFlagBits(..)
  , VkDescriptorSetLayoutCreateInfo(..)
  , VkDescriptorType(..)
  , VkWriteDescriptorSet(..)
  , VkDescriptorPool
  , VkDescriptorSet
  )
import Graphics.Vulkan.Core10.BufferView
  ( BufferView
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  , withCStructAllocationCallbacks
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


-- No documentation found for TopLevel "CopyDescriptorSet"
data CopyDescriptorSet = CopyDescriptorSet
  { -- Univalued Member elided
  -- No documentation found for Nested "CopyDescriptorSet" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CopyDescriptorSet" "srcSet"
  vkSrcSet :: DescriptorSet
  , -- No documentation found for Nested "CopyDescriptorSet" "srcBinding"
  vkSrcBinding :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "srcArrayElement"
  vkSrcArrayElement :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "dstSet"
  vkDstSet :: DescriptorSet
  , -- No documentation found for Nested "CopyDescriptorSet" "dstBinding"
  vkDstBinding :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "dstArrayElement"
  vkDstArrayElement :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "descriptorCount"
  vkDescriptorCount :: Word32
  }
  deriving (Show, Eq)
withCStructCopyDescriptorSet :: CopyDescriptorSet -> (VkCopyDescriptorSet -> IO a) -> IO a
withCStructCopyDescriptorSet from cont = maybeWith withSomeVkStruct (vkPNext (from :: CopyDescriptorSet)) (\pPNext -> cont (VkCopyDescriptorSet VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET pPNext (vkSrcSet (from :: CopyDescriptorSet)) (vkSrcBinding (from :: CopyDescriptorSet)) (vkSrcArrayElement (from :: CopyDescriptorSet)) (vkDstSet (from :: CopyDescriptorSet)) (vkDstBinding (from :: CopyDescriptorSet)) (vkDstArrayElement (from :: CopyDescriptorSet)) (vkDescriptorCount (from :: CopyDescriptorSet))))
fromCStructCopyDescriptorSet :: VkCopyDescriptorSet -> IO CopyDescriptorSet
fromCStructCopyDescriptorSet c = CopyDescriptorSet <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCopyDescriptorSet)))
                                                   <*> pure (vkSrcSet (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkSrcBinding (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkSrcArrayElement (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkDstSet (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkDstBinding (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkDstArrayElement (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkDescriptorCount (c :: VkCopyDescriptorSet))
-- No documentation found for TopLevel "DescriptorBufferInfo"
data DescriptorBufferInfo = DescriptorBufferInfo
  { -- No documentation found for Nested "DescriptorBufferInfo" "buffer"
  vkBuffer :: Buffer
  , -- No documentation found for Nested "DescriptorBufferInfo" "offset"
  vkOffset :: DeviceSize
  , -- No documentation found for Nested "DescriptorBufferInfo" "range"
  vkRange :: DeviceSize
  }
  deriving (Show, Eq)
withCStructDescriptorBufferInfo :: DescriptorBufferInfo -> (VkDescriptorBufferInfo -> IO a) -> IO a
withCStructDescriptorBufferInfo from cont = cont (VkDescriptorBufferInfo (vkBuffer (from :: DescriptorBufferInfo)) (vkOffset (from :: DescriptorBufferInfo)) (vkRange (from :: DescriptorBufferInfo)))
fromCStructDescriptorBufferInfo :: VkDescriptorBufferInfo -> IO DescriptorBufferInfo
fromCStructDescriptorBufferInfo c = DescriptorBufferInfo <$> pure (vkBuffer (c :: VkDescriptorBufferInfo))
                                                         <*> pure (vkOffset (c :: VkDescriptorBufferInfo))
                                                         <*> pure (vkRange (c :: VkDescriptorBufferInfo))
-- No documentation found for TopLevel "DescriptorImageInfo"
data DescriptorImageInfo = DescriptorImageInfo
  { -- No documentation found for Nested "DescriptorImageInfo" "sampler"
  vkSampler :: Sampler
  , -- No documentation found for Nested "DescriptorImageInfo" "imageView"
  vkImageView :: ImageView
  , -- No documentation found for Nested "DescriptorImageInfo" "imageLayout"
  vkImageLayout :: ImageLayout
  }
  deriving (Show, Eq)
withCStructDescriptorImageInfo :: DescriptorImageInfo -> (VkDescriptorImageInfo -> IO a) -> IO a
withCStructDescriptorImageInfo from cont = cont (VkDescriptorImageInfo (vkSampler (from :: DescriptorImageInfo)) (vkImageView (from :: DescriptorImageInfo)) (vkImageLayout (from :: DescriptorImageInfo)))
fromCStructDescriptorImageInfo :: VkDescriptorImageInfo -> IO DescriptorImageInfo
fromCStructDescriptorImageInfo c = DescriptorImageInfo <$> pure (vkSampler (c :: VkDescriptorImageInfo))
                                                       <*> pure (vkImageView (c :: VkDescriptorImageInfo))
                                                       <*> pure (vkImageLayout (c :: VkDescriptorImageInfo))
-- No documentation found for TopLevel "DescriptorPool"
type DescriptorPool = VkDescriptorPool
-- No documentation found for TopLevel "DescriptorPoolCreateFlagBits"
type DescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits
-- No documentation found for TopLevel "DescriptorPoolCreateFlags"
type DescriptorPoolCreateFlags = DescriptorPoolCreateFlagBits
-- No documentation found for TopLevel "DescriptorPoolCreateInfo"
data DescriptorPoolCreateInfo = DescriptorPoolCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DescriptorPoolCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorPoolCreateInfo" "flags"
  vkFlags :: DescriptorPoolCreateFlags
  , -- No documentation found for Nested "DescriptorPoolCreateInfo" "maxSets"
  vkMaxSets :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "DescriptorPoolCreateInfo" "pPoolSizes"
  vkPPoolSizes :: Vector DescriptorPoolSize
  }
  deriving (Show, Eq)
withCStructDescriptorPoolCreateInfo :: DescriptorPoolCreateInfo -> (VkDescriptorPoolCreateInfo -> IO a) -> IO a
withCStructDescriptorPoolCreateInfo from cont = withVec withCStructDescriptorPoolSize (vkPPoolSizes (from :: DescriptorPoolCreateInfo)) (\pPoolSizes -> maybeWith withSomeVkStruct (vkPNext (from :: DescriptorPoolCreateInfo)) (\pPNext -> cont (VkDescriptorPoolCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO pPNext (vkFlags (from :: DescriptorPoolCreateInfo)) (vkMaxSets (from :: DescriptorPoolCreateInfo)) (fromIntegral (Data.Vector.length (vkPPoolSizes (from :: DescriptorPoolCreateInfo)))) pPoolSizes)))
fromCStructDescriptorPoolCreateInfo :: VkDescriptorPoolCreateInfo -> IO DescriptorPoolCreateInfo
fromCStructDescriptorPoolCreateInfo c = DescriptorPoolCreateInfo <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorPoolCreateInfo)))
                                                                 <*> pure (vkFlags (c :: VkDescriptorPoolCreateInfo))
                                                                 <*> pure (vkMaxSets (c :: VkDescriptorPoolCreateInfo))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkPoolSizeCount (c :: VkDescriptorPoolCreateInfo))) (((fromCStructDescriptorPoolSize <=<) . peekElemOff) (vkPPoolSizes (c :: VkDescriptorPoolCreateInfo))))
-- No documentation found for TopLevel "DescriptorPoolResetFlags"
type DescriptorPoolResetFlags = VkDescriptorPoolResetFlags
-- No documentation found for TopLevel "DescriptorPoolSize"
data DescriptorPoolSize = DescriptorPoolSize
  { -- No documentation found for Nested "DescriptorPoolSize" "type"
  vkType :: DescriptorType
  , -- No documentation found for Nested "DescriptorPoolSize" "descriptorCount"
  vkDescriptorCount :: Word32
  }
  deriving (Show, Eq)
withCStructDescriptorPoolSize :: DescriptorPoolSize -> (VkDescriptorPoolSize -> IO a) -> IO a
withCStructDescriptorPoolSize from cont = cont (VkDescriptorPoolSize (vkType (from :: DescriptorPoolSize)) (vkDescriptorCount (from :: DescriptorPoolSize)))
fromCStructDescriptorPoolSize :: VkDescriptorPoolSize -> IO DescriptorPoolSize
fromCStructDescriptorPoolSize c = DescriptorPoolSize <$> pure (vkType (c :: VkDescriptorPoolSize))
                                                     <*> pure (vkDescriptorCount (c :: VkDescriptorPoolSize))
-- No documentation found for TopLevel "DescriptorSet"
type DescriptorSet = VkDescriptorSet
-- No documentation found for TopLevel "DescriptorSetAllocateInfo"
data DescriptorSetAllocateInfo = DescriptorSetAllocateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DescriptorSetAllocateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetAllocateInfo" "descriptorPool"
  vkDescriptorPool :: DescriptorPool
  -- Length valued member elided
  , -- No documentation found for Nested "DescriptorSetAllocateInfo" "pSetLayouts"
  vkPSetLayouts :: Vector DescriptorSetLayout
  }
  deriving (Show, Eq)
withCStructDescriptorSetAllocateInfo :: DescriptorSetAllocateInfo -> (VkDescriptorSetAllocateInfo -> IO a) -> IO a
withCStructDescriptorSetAllocateInfo from cont = withVec (&) (vkPSetLayouts (from :: DescriptorSetAllocateInfo)) (\pSetLayouts -> maybeWith withSomeVkStruct (vkPNext (from :: DescriptorSetAllocateInfo)) (\pPNext -> cont (VkDescriptorSetAllocateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO pPNext (vkDescriptorPool (from :: DescriptorSetAllocateInfo)) (fromIntegral (Data.Vector.length (vkPSetLayouts (from :: DescriptorSetAllocateInfo)))) pSetLayouts)))
fromCStructDescriptorSetAllocateInfo :: VkDescriptorSetAllocateInfo -> IO DescriptorSetAllocateInfo
fromCStructDescriptorSetAllocateInfo c = DescriptorSetAllocateInfo <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetAllocateInfo)))
                                                                   <*> pure (vkDescriptorPool (c :: VkDescriptorSetAllocateInfo))
                                                                   -- Length valued member elided
                                                                   <*> (Data.Vector.generateM (fromIntegral (vkDescriptorSetCount (c :: VkDescriptorSetAllocateInfo))) (peekElemOff (vkPSetLayouts (c :: VkDescriptorSetAllocateInfo))))
-- No documentation found for TopLevel "DescriptorSetLayoutBinding"
data DescriptorSetLayoutBinding = DescriptorSetLayoutBinding
  { -- No documentation found for Nested "DescriptorSetLayoutBinding" "binding"
  vkBinding :: Word32
  , -- No documentation found for Nested "DescriptorSetLayoutBinding" "descriptorType"
  vkDescriptorType :: DescriptorType
  -- Optional length valued member elided
  , -- No documentation found for Nested "DescriptorSetLayoutBinding" "stageFlags"
  vkStageFlags :: ShaderStageFlags
  , -- No documentation found for Nested "DescriptorSetLayoutBinding" "pImmutableSamplers"
  vkPImmutableSamplers :: Maybe (Vector Sampler)
  }
  deriving (Show, Eq)
withCStructDescriptorSetLayoutBinding :: DescriptorSetLayoutBinding -> (VkDescriptorSetLayoutBinding -> IO a) -> IO a
withCStructDescriptorSetLayoutBinding from cont = maybeWith (withVec (&)) (vkPImmutableSamplers (from :: DescriptorSetLayoutBinding)) (\pImmutableSamplers -> cont (VkDescriptorSetLayoutBinding (vkBinding (from :: DescriptorSetLayoutBinding)) (vkDescriptorType (from :: DescriptorSetLayoutBinding)) (maybe 0 (fromIntegral . Data.Vector.length) (vkPImmutableSamplers (from :: DescriptorSetLayoutBinding))) (vkStageFlags (from :: DescriptorSetLayoutBinding)) pImmutableSamplers))
fromCStructDescriptorSetLayoutBinding :: VkDescriptorSetLayoutBinding -> IO DescriptorSetLayoutBinding
fromCStructDescriptorSetLayoutBinding c = DescriptorSetLayoutBinding <$> pure (vkBinding (c :: VkDescriptorSetLayoutBinding))
                                                                     <*> pure (vkDescriptorType (c :: VkDescriptorSetLayoutBinding))
                                                                     -- Optional length valued member elided
                                                                     <*> pure (vkStageFlags (c :: VkDescriptorSetLayoutBinding))
                                                                     <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkDescriptorCount (c :: VkDescriptorSetLayoutBinding))) (peekElemOff p)) (vkPImmutableSamplers (c :: VkDescriptorSetLayoutBinding))
-- No documentation found for TopLevel "DescriptorSetLayoutCreateFlagBits"
type DescriptorSetLayoutCreateFlagBits = VkDescriptorSetLayoutCreateFlagBits
-- No documentation found for TopLevel "DescriptorSetLayoutCreateFlags"
type DescriptorSetLayoutCreateFlags = DescriptorSetLayoutCreateFlagBits
-- No documentation found for TopLevel "DescriptorSetLayoutCreateInfo"
data DescriptorSetLayoutCreateInfo = DescriptorSetLayoutCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "DescriptorSetLayoutCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetLayoutCreateInfo" "flags"
  vkFlags :: DescriptorSetLayoutCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "DescriptorSetLayoutCreateInfo" "pBindings"
  vkPBindings :: Vector DescriptorSetLayoutBinding
  }
  deriving (Show, Eq)
withCStructDescriptorSetLayoutCreateInfo :: DescriptorSetLayoutCreateInfo -> (VkDescriptorSetLayoutCreateInfo -> IO a) -> IO a
withCStructDescriptorSetLayoutCreateInfo from cont = withVec withCStructDescriptorSetLayoutBinding (vkPBindings (from :: DescriptorSetLayoutCreateInfo)) (\pBindings -> maybeWith withSomeVkStruct (vkPNext (from :: DescriptorSetLayoutCreateInfo)) (\pPNext -> cont (VkDescriptorSetLayoutCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO pPNext (vkFlags (from :: DescriptorSetLayoutCreateInfo)) (fromIntegral (Data.Vector.length (vkPBindings (from :: DescriptorSetLayoutCreateInfo)))) pBindings)))
fromCStructDescriptorSetLayoutCreateInfo :: VkDescriptorSetLayoutCreateInfo -> IO DescriptorSetLayoutCreateInfo
fromCStructDescriptorSetLayoutCreateInfo c = DescriptorSetLayoutCreateInfo <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetLayoutCreateInfo)))
                                                                           <*> pure (vkFlags (c :: VkDescriptorSetLayoutCreateInfo))
                                                                           -- Length valued member elided
                                                                           <*> (Data.Vector.generateM (fromIntegral (vkBindingCount (c :: VkDescriptorSetLayoutCreateInfo))) (((fromCStructDescriptorSetLayoutBinding <=<) . peekElemOff) (vkPBindings (c :: VkDescriptorSetLayoutCreateInfo))))
-- No documentation found for TopLevel "DescriptorType"
type DescriptorType = VkDescriptorType
-- No documentation found for TopLevel "WriteDescriptorSet"
data WriteDescriptorSet = WriteDescriptorSet
  { -- Univalued Member elided
  -- No documentation found for Nested "WriteDescriptorSet" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "WriteDescriptorSet" "dstSet"
  vkDstSet :: DescriptorSet
  , -- No documentation found for Nested "WriteDescriptorSet" "dstBinding"
  vkDstBinding :: Word32
  , -- No documentation found for Nested "WriteDescriptorSet" "dstArrayElement"
  vkDstArrayElement :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "WriteDescriptorSet" "descriptorType"
  vkDescriptorType :: DescriptorType
  , -- No documentation found for Nested "WriteDescriptorSet" "pImageInfo"
  vkPImageInfo :: Vector DescriptorImageInfo
  , -- No documentation found for Nested "WriteDescriptorSet" "pBufferInfo"
  vkPBufferInfo :: Vector DescriptorBufferInfo
  , -- No documentation found for Nested "WriteDescriptorSet" "pTexelBufferView"
  vkPTexelBufferView :: Vector BufferView
  }
  deriving (Show, Eq)
withCStructWriteDescriptorSet :: WriteDescriptorSet -> (VkWriteDescriptorSet -> IO a) -> IO a
withCStructWriteDescriptorSet from cont = withVec (&) (vkPTexelBufferView (from :: WriteDescriptorSet)) (\pTexelBufferView -> withVec withCStructDescriptorBufferInfo (vkPBufferInfo (from :: WriteDescriptorSet)) (\pBufferInfo -> withVec withCStructDescriptorImageInfo (vkPImageInfo (from :: WriteDescriptorSet)) (\pImageInfo -> maybeWith withSomeVkStruct (vkPNext (from :: WriteDescriptorSet)) (\pPNext -> cont (VkWriteDescriptorSet VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET pPNext (vkDstSet (from :: WriteDescriptorSet)) (vkDstBinding (from :: WriteDescriptorSet)) (vkDstArrayElement (from :: WriteDescriptorSet)) (fromIntegral (minimum ([Data.Vector.length (vkPImageInfo (from :: WriteDescriptorSet)), Data.Vector.length (vkPBufferInfo (from :: WriteDescriptorSet)), Data.Vector.length (vkPTexelBufferView (from :: WriteDescriptorSet))]))) (vkDescriptorType (from :: WriteDescriptorSet)) pImageInfo pBufferInfo pTexelBufferView)))))
fromCStructWriteDescriptorSet :: VkWriteDescriptorSet -> IO WriteDescriptorSet
fromCStructWriteDescriptorSet c = WriteDescriptorSet <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWriteDescriptorSet)))
                                                     <*> pure (vkDstSet (c :: VkWriteDescriptorSet))
                                                     <*> pure (vkDstBinding (c :: VkWriteDescriptorSet))
                                                     <*> pure (vkDstArrayElement (c :: VkWriteDescriptorSet))
                                                     -- Length valued member elided
                                                     <*> pure (vkDescriptorType (c :: VkWriteDescriptorSet))
                                                     <*> (Data.Vector.generateM (fromIntegral (vkDescriptorCount (c :: VkWriteDescriptorSet))) (((fromCStructDescriptorImageInfo <=<) . peekElemOff) (vkPImageInfo (c :: VkWriteDescriptorSet))))
                                                     <*> (Data.Vector.generateM (fromIntegral (vkDescriptorCount (c :: VkWriteDescriptorSet))) (((fromCStructDescriptorBufferInfo <=<) . peekElemOff) (vkPBufferInfo (c :: VkWriteDescriptorSet))))
                                                     <*> (Data.Vector.generateM (fromIntegral (vkDescriptorCount (c :: VkWriteDescriptorSet))) (peekElemOff (vkPTexelBufferView (c :: VkWriteDescriptorSet))))

-- | Wrapper for vkAllocateDescriptorSets
allocateDescriptorSets :: Device ->  DescriptorSetAllocateInfo ->  IO (DescriptorSet)
allocateDescriptorSets = \(Device device commandTable) -> \allocateInfo -> alloca (\pDescriptorSets -> (\a -> withCStructDescriptorSetAllocateInfo a . flip with) allocateInfo (\pAllocateInfo -> Graphics.Vulkan.C.Dynamic.allocateDescriptorSets commandTable device pAllocateInfo pDescriptorSets >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pDescriptorSets))))

-- | Wrapper for vkCreateDescriptorPool
createDescriptorPool :: Device ->  DescriptorPoolCreateInfo ->  Maybe AllocationCallbacks ->  IO (DescriptorPool)
createDescriptorPool = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pDescriptorPool -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructDescriptorPoolCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createDescriptorPool commandTable device pCreateInfo pAllocator pDescriptorPool >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pDescriptorPool)))))

-- | Wrapper for vkCreateDescriptorSetLayout
createDescriptorSetLayout :: Device ->  DescriptorSetLayoutCreateInfo ->  Maybe AllocationCallbacks ->  IO (DescriptorSetLayout)
createDescriptorSetLayout = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pSetLayout -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructDescriptorSetLayoutCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createDescriptorSetLayout commandTable device pCreateInfo pAllocator pSetLayout >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSetLayout)))))

-- | Wrapper for vkDestroyDescriptorPool
destroyDescriptorPool :: Device ->  DescriptorPool ->  Maybe AllocationCallbacks ->  IO ()
destroyDescriptorPool = \(Device device commandTable) -> \descriptorPool -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyDescriptorPool commandTable device descriptorPool pAllocator *> (pure ()))

-- | Wrapper for vkDestroyDescriptorSetLayout
destroyDescriptorSetLayout :: Device ->  DescriptorSetLayout ->  Maybe AllocationCallbacks ->  IO ()
destroyDescriptorSetLayout = \(Device device commandTable) -> \descriptorSetLayout -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyDescriptorSetLayout commandTable device descriptorSetLayout pAllocator *> (pure ()))

-- | Wrapper for vkFreeDescriptorSets
freeDescriptorSets :: Device ->  DescriptorPool ->  Vector DescriptorSet ->  IO ()
freeDescriptorSets = \(Device device commandTable) -> \descriptorPool -> \descriptorSets -> withVec (&) descriptorSets (\pDescriptorSets -> Graphics.Vulkan.C.Dynamic.freeDescriptorSets commandTable device descriptorPool (fromIntegral $ Data.Vector.length descriptorSets) pDescriptorSets >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ())))

-- | Wrapper for vkResetDescriptorPool
resetDescriptorPool :: Device ->  DescriptorPool ->  DescriptorPoolResetFlags ->  IO ()
resetDescriptorPool = \(Device device commandTable) -> \descriptorPool -> \flags -> Graphics.Vulkan.C.Dynamic.resetDescriptorPool commandTable device descriptorPool flags >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (pure ()))

-- | Wrapper for vkUpdateDescriptorSets
updateDescriptorSets :: Device ->  Vector WriteDescriptorSet ->  Vector CopyDescriptorSet ->  IO ()
updateDescriptorSets = \(Device device commandTable) -> \descriptorWrites -> \descriptorCopies -> withVec withCStructCopyDescriptorSet descriptorCopies (\pDescriptorCopies -> withVec withCStructWriteDescriptorSet descriptorWrites (\pDescriptorWrites -> Graphics.Vulkan.C.Dynamic.updateDescriptorSets commandTable device (fromIntegral $ Data.Vector.length descriptorWrites) pDescriptorWrites (fromIntegral $ Data.Vector.length descriptorCopies) pDescriptorCopies *> (pure ())))
