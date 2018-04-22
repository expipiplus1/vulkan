{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorType(..)
  , pattern VK_DESCRIPTOR_TYPE_SAMPLER
  , pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  , pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
  , pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  , VkDescriptorSetLayoutCreateFlagBits(..)
  , VkDescriptorPoolCreateFlagBits(..)
  , pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
  , VkDescriptorPoolResetFlags(..)
  , VkDescriptorSet
  , VkDescriptorPool
  , vkCreateDescriptorSetLayout
  , vkDestroyDescriptorSetLayout
  , vkCreateDescriptorPool
  , vkDestroyDescriptorPool
  , vkResetDescriptorPool
  , vkAllocateDescriptorSets
  , vkFreeDescriptorSets
  , vkUpdateDescriptorSets
  , VkDescriptorBufferInfo(..)
  , VkDescriptorImageInfo(..)
  , VkWriteDescriptorSet(..)
  , VkCopyDescriptorSet(..)
  , VkDescriptorSetLayoutBinding(..)
  , VkDescriptorSetLayoutCreateInfo(..)
  , VkDescriptorPoolSize(..)
  , VkDescriptorPoolCreateInfo(..)
  , VkDescriptorSetAllocateInfo(..)
  , VkDescriptorSetLayoutCreateFlags
  , VkDescriptorPoolCreateFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.BufferView
  ( VkBufferView
  )
import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.Core10.ImageView
  ( VkImageView
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  , VkShaderStageFlags
  )
import Graphics.Vulkan.Core10.Sampler
  ( VkSampler
  )


-- ** VkDescriptorType

-- | VkDescriptorType - Specifies the type of a descriptor in a descriptor
-- set
--
-- = Description
--
-- -   @VK_DESCRIPTOR_TYPE_SAMPLER@ specifies a [sampler
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-sampler).
--
-- -   @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@ specifies a [combined
--     image sampler
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-combinedimagesampler).
--
-- -   @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@ specifies a [sampled image
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-sampledimage).
--
-- -   @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@ specifies a [storage image
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storageimage).
--
-- -   @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ specifies a [uniform texel
--     buffer
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer).
--
-- -   @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@ specifies a [storage texel
--     buffer
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer).
--
-- -   @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ specifies a [uniform buffer
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-uniformbuffer).
--
-- -   @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ specifies a [storage buffer
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storagebuffer).
--
-- -   @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ specifies a [dynamic
--     uniform buffer
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic).
--
-- -   @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ specifies a [dynamic
--     storage buffer
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic).
--
-- -   @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@ specifies a [input attachment
--     descriptor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-inputattachment).
--
-- When a descriptor set is updated via elements of 'VkWriteDescriptorSet',
-- members of @pImageInfo@, @pBufferInfo@ and @pTexelBufferView@ are only
-- accessed by the implementation when they correspond to descriptor type
-- being defined - otherwise they are ignored. The members accessed are as
-- follows for each descriptor type:
--
-- -   For @VK_DESCRIPTOR_TYPE_SAMPLER@, only the @sample@ member of each
--     element of 'VkWriteDescriptorSet'::@pImageInfo@ is accessed.
--
-- -   For @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@,
--     @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@, or
--     @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@, only the @imageView@ and
--     @imageLayout@ members of each element of
--     'VkWriteDescriptorSet'::@pImageInfo@ are accessed.
--
-- -   For @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, all members of each
--     element of 'VkWriteDescriptorSet'::@pImageInfo@ are accessed.
--
-- -   For @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@,
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@,
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@, or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@, all members of each
--     element of 'VkWriteDescriptorSet'::@pBufferInfo@ are accessed.
--
-- -   For @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@, each element of
--     'VkWriteDescriptorSet'::@pTexelBufferView@ is accessed.
--
-- = See Also
--
-- 'VkDescriptorPoolSize', 'VkDescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateEntry',
-- 'VkWriteDescriptorSet'
newtype VkDescriptorType = VkDescriptorType Int32
  deriving (Eq, Ord, Storable)

instance Show VkDescriptorType where
  showsPrec _ VK_DESCRIPTOR_TYPE_SAMPLER = showString "VK_DESCRIPTOR_TYPE_SAMPLER"
  showsPrec _ VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = showString "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
  showsPrec _ VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = showString "VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE"
  showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = showString "VK_DESCRIPTOR_TYPE_STORAGE_IMAGE"
  showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = showString "VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
  showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = showString "VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
  showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = showString "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
  showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = showString "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER"
  showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = showString "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
  showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = showString "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
  showsPrec _ VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = showString "VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
  showsPrec p (VkDescriptorType x) = showParen (p >= 11) (showString "VkDescriptorType " . showsPrec 11 x)

instance Read VkDescriptorType where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_TYPE_SAMPLER",                pure VK_DESCRIPTOR_TYPE_SAMPLER)
                             , ("VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER", pure VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
                             , ("VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE",          pure VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_IMAGE",          pure VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
                             , ("VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER",   pure VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER",   pure VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER",         pure VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_BUFFER",         pure VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC", pure VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC", pure VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC)
                             , ("VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT",       pure VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorType")
                        v <- step readPrec
                        pure (VkDescriptorType v)
                        )
                    )

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLER"
pattern VK_DESCRIPTOR_TYPE_SAMPLER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_SAMPLER = VkDescriptorType 0

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = VkDescriptorType 1

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE"
pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = VkDescriptorType 2

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_IMAGE"
pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = VkDescriptorType 3

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = VkDescriptorType 4

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = VkDescriptorType 5

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = VkDescriptorType 6

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER"
pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = VkDescriptorType 7

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = VkDescriptorType 8

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = VkDescriptorType 9

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = VkDescriptorType 10
-- ** VkDescriptorSetLayoutCreateFlagBits

-- | VkDescriptorSetLayoutCreateFlagBits - Bitmask specifying descriptor set
-- layout properties
--
-- = See Also
--
-- 'VkDescriptorSetLayoutCreateFlags'
newtype VkDescriptorSetLayoutCreateFlagBits = VkDescriptorSetLayoutCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDescriptorSetLayoutCreateFlagBits where
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDescriptorSetLayoutCreateFlagBits 0x00000001) = showString "VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR"
  showsPrec _ (VkDescriptorSetLayoutCreateFlagBits 0x00000002) = showString "VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT"
  showsPrec p (VkDescriptorSetLayoutCreateFlagBits x) = showParen (p >= 11) (showString "VkDescriptorSetLayoutCreateFlagBits " . showsPrec 11 x)

instance Read VkDescriptorSetLayoutCreateFlagBits where
  readPrec = parens ( choose [ -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR",        pure (VkDescriptorSetLayoutCreateFlagBits 0x00000001))
                             , ("VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT", pure (VkDescriptorSetLayoutCreateFlagBits 0x00000002))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorSetLayoutCreateFlagBits")
                        v <- step readPrec
                        pure (VkDescriptorSetLayoutCreateFlagBits v)
                        )
                    )


-- ** VkDescriptorPoolCreateFlagBits

-- | VkDescriptorPoolCreateFlagBits - Bitmask specifying certain supported
-- operations on a descriptor pool
--
-- = See Also
--
-- 'VkDescriptorPoolCreateFlags'
newtype VkDescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDescriptorPoolCreateFlagBits where
  showsPrec _ VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = showString "VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDescriptorPoolCreateFlagBits 0x00000002) = showString "VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT"
  showsPrec p (VkDescriptorPoolCreateFlagBits x) = showParen (p >= 11) (showString "VkDescriptorPoolCreateFlagBits " . showsPrec 11 x)

instance Read VkDescriptorPoolCreateFlagBits where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT", pure VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT", pure (VkDescriptorPoolCreateFlagBits 0x00000002))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorPoolCreateFlagBits")
                        v <- step readPrec
                        pure (VkDescriptorPoolCreateFlagBits v)
                        )
                    )

-- | @VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT@ specifies that
-- descriptor sets /can/ return their individual allocations to the pool,
-- i.e. all of 'vkAllocateDescriptorSets', 'vkFreeDescriptorSets', and
-- 'vkResetDescriptorPool' are allowed. Otherwise, descriptor sets
-- allocated from the pool /must/ not be individually freed back to the
-- pool, i.e. only 'vkAllocateDescriptorSets' and 'vkResetDescriptorPool'
-- are allowed.
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT :: VkDescriptorPoolCreateFlagBits
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = VkDescriptorPoolCreateFlagBits 0x00000001
-- ** VkDescriptorPoolResetFlags

-- | VkDescriptorPoolResetFlags - Reserved for future use
--
-- = Description
--
-- @VkDescriptorPoolResetFlags@ is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- 'vkResetDescriptorPool'
newtype VkDescriptorPoolResetFlags = VkDescriptorPoolResetFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDescriptorPoolResetFlags where
  
  showsPrec p (VkDescriptorPoolResetFlags x) = showParen (p >= 11) (showString "VkDescriptorPoolResetFlags " . showsPrec 11 x)

instance Read VkDescriptorPoolResetFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorPoolResetFlags")
                        v <- step readPrec
                        pure (VkDescriptorPoolResetFlags v)
                        )
                    )


-- | Dummy data to tag the 'Ptr' with
data VkDescriptorSet_T
-- | VkDescriptorSet - Opaque handle to a descriptor set object
--
-- = See Also
--
-- 'VkCopyDescriptorSet',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkObjectTableDescriptorSetEntryNVX',
-- 'VkWriteDescriptorSet', 'vkAllocateDescriptorSets',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'vkFreeDescriptorSets',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplateKHR'
type VkDescriptorSet = Ptr VkDescriptorSet_T
-- | Dummy data to tag the 'Ptr' with
data VkDescriptorPool_T
-- | VkDescriptorPool - Opaque handle to a descriptor pool object
--
-- = See Also
--
-- 'VkDescriptorSetAllocateInfo', 'vkCreateDescriptorPool',
-- 'vkDestroyDescriptorPool', 'vkFreeDescriptorSets',
-- 'vkResetDescriptorPool'
type VkDescriptorPool = Ptr VkDescriptorPool_T
-- | vkCreateDescriptorSetLayout - Create a new descriptor set layout
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the descriptor set
--     layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkDescriptorSetLayoutCreateInfo' structure specifying the state of
--     the descriptor set layout object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pSetLayout@ points to a @VkDescriptorSetLayout@ handle in which the
--     resulting descriptor set layout object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkDescriptorSetLayoutCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pSetLayout@ /must/ be a valid pointer to a @VkDescriptorSetLayout@
--     handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'VkDescriptorSetLayoutCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDescriptorSetLayout" vkCreateDescriptorSetLayout :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult
-- | vkDestroyDescriptorSetLayout - Destroy a descriptor set layout object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the descriptor set
--     layout.
--
-- -   @descriptorSetLayout@ is the descriptor set layout to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   If @VkAllocationCallbacks@ were provided when @descriptorSetLayout@
--     was created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when
--     @descriptorSetLayout@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @descriptorSetLayout@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @descriptorSetLayout@ /must/ be a valid @VkDescriptorSetLayout@
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @descriptorSetLayout@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorSetLayout@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDescriptorSetLayout" vkDestroyDescriptorSetLayout :: ("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkCreateDescriptorPool - Creates a descriptor pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the descriptor pool.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkDescriptorPoolCreateInfo' structure specifying the state of the
--     descriptor pool object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pDescriptorPool@ points to a @VkDescriptorPool@ handle in which the
--     resulting descriptor pool object is returned.
--
-- = Description
--
-- @pAllocator@ controls host memory allocation as described in the [Memory
-- Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
-- chapter.
--
-- The created descriptor pool is returned in @pDescriptorPool@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkDescriptorPoolCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pDescriptorPool@ /must/ be a valid pointer to a @VkDescriptorPool@
--     handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_FRAGMENTATION_EXT@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkDescriptorPool', 'VkDescriptorPoolCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDescriptorPool" vkCreateDescriptorPool :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult
-- | vkDestroyDescriptorPool - Destroy a descriptor pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the descriptor pool.
--
-- -   @descriptorPool@ is the descriptor pool to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- = Description
--
-- When a pool is destroyed, all descriptor sets allocated from the pool
-- are implicitly freed and become invalid. Descriptor sets allocated from
-- a given pool do not need to be freed before destroying that descriptor
-- pool.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @descriptorPool@ (via any
--     allocated descriptor sets) /must/ have completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @descriptorPool@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @descriptorPool@
--     was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @descriptorPool@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @descriptorPool@
--     /must/ be a valid @VkDescriptorPool@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @descriptorPool@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorPool@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkDescriptorPool',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDescriptorPool" vkDestroyDescriptorPool :: ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
-- | vkResetDescriptorPool - Resets a descriptor pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the descriptor pool.
--
-- -   @descriptorPool@ is the descriptor pool to be reset.
--
-- -   @flags@ is reserved for future use.
--
-- = Description
--
-- Resetting a descriptor pool recycles all of the resources from all of
-- the descriptor sets allocated from the descriptor pool back to the
-- descriptor pool, and the descriptor sets are implicitly freed.
--
-- == Valid Usage
--
-- -   All uses of @descriptorPool@ (via any allocated descriptor sets)
--     /must/ have completed execution
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @descriptorPool@ /must/ be a valid @VkDescriptorPool@ handle
--
-- -   @flags@ /must/ be @0@
--
-- -   @descriptorPool@ /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorPool@ /must/ be externally synchronized
--
-- -   Host access to any @VkDescriptorSet@ objects allocated from
--     @descriptorPool@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'VkDescriptorPool', 'VkDescriptorPoolResetFlags',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetDescriptorPool" vkResetDescriptorPool :: ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult
-- | vkAllocateDescriptorSets - Allocate one or more descriptor sets
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the descriptor pool.
--
-- -   @pAllocateInfo@ is a pointer to an instance of the
--     'VkDescriptorSetAllocateInfo' structure describing parameters of the
--     allocation.
--
-- -   @pDescriptorSets@ is a pointer to an array of @VkDescriptorSet@
--     handles in which the resulting descriptor set objects are returned.
--
-- = Description
--
-- The allocated descriptor sets are returned in @pDescriptorSets@.
--
-- When a descriptor set is allocated, the initial state is largely
-- uninitialized and all descriptors are undefined. However, the descriptor
-- set /can/ be bound in a command buffer without causing errors or
-- exceptions. For descriptor set bindings created with the
-- @VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT@ bit set, all descriptors
-- in that binding that are dynamically used /must/ have been populated
-- before the descriptor set is
-- [consumed](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-binding).
-- For descriptor set bindings created without the
-- @VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT@ bit set, all descriptors
-- in that binding that are statically used /must/ have been populated
-- before the descriptor set is
-- [consumed](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-binding).
-- Entries that are not used by a pipeline /can/ have uninitialized
-- descriptors or descriptors of resources that have been destroyed, and
-- executing a draw or dispatch with such a descriptor set bound does not
-- cause undefined behavior. This means applications need not populate
-- unused entries with dummy descriptors.
--
-- If a call to @vkAllocateDescriptorSets@ would cause the total number of
-- descriptor sets allocated from the pool to exceed the value of
-- 'VkDescriptorPoolCreateInfo'::@maxSets@ used to create
-- @pAllocateInfo@→@descriptorPool@, then the allocation /may/ fail due to
-- lack of space in the descriptor pool. Similarly, the allocation /may/
-- fail due to lack of space if the call to @vkAllocateDescriptorSets@
-- would cause the number of any given descriptor type to exceed the sum of
-- all the @descriptorCount@ members of each element of
-- 'VkDescriptorPoolCreateInfo'::@pPoolSizes@ with a @member@ equal to that
-- type. If the allocation fails due to no more space in the descriptor
-- pool, and not because of system or device memory exhaustion, then
-- @VK_ERROR_OUT_OF_POOL_MEMORY@ /must/ be returned.
--
-- @vkAllocateDescriptorSets@ /can/ be used to create multiple descriptor
-- sets. If the creation of any of those descriptor sets fails, then the
-- implementation /must/ destroy all successfully created descriptor set
-- objects from this command, set all entries of the @pDescriptorSets@
-- array to 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE' and return
-- the error.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pAllocateInfo@ /must/ be a valid pointer to a valid
--     @VkDescriptorSetAllocateInfo@ structure
--
-- -   @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @pAllocateInfo@::descriptorSetCount @VkDescriptorSet@ handles
--
-- == Host Synchronization
--
-- -   Host access to @pAllocateInfo@::descriptorPool /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_FRAGMENTED_POOL@
--
--     -   @VK_ERROR_OUT_OF_POOL_MEMORY@
--
-- = See Also
--
-- 'VkDescriptorSet', 'VkDescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAllocateDescriptorSets" vkAllocateDescriptorSets :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
-- | vkFreeDescriptorSets - Free one or more descriptor sets
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the descriptor pool.
--
-- -   @descriptorPool@ is the descriptor pool from which the descriptor
--     sets were allocated.
--
-- -   @descriptorSetCount@ is the number of elements in the
--     @pDescriptorSets@ array.
--
-- -   @pDescriptorSets@ is an array of handles to @VkDescriptorSet@
--     objects.
--
-- = Description
--
-- After a successful call to @vkFreeDescriptorSets@, all descriptor sets
-- in @pDescriptorSets@ are invalid.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to any element of
--     @pDescriptorSets@ /must/ have completed execution
--
-- -   @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ @VkDescriptorSet@ handles, each element of
--     which /must/ either be a valid handle or
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'
--
-- -   Each valid handle in @pDescriptorSets@ /must/ have been allocated
--     from @descriptorPool@
--
-- -   @descriptorPool@ /must/ have been created with the
--     @VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT@ flag
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @descriptorPool@ /must/ be a valid @VkDescriptorPool@ handle
--
-- -   @descriptorSetCount@ /must/ be greater than @0@
--
-- -   @descriptorPool@ /must/ have been created, allocated, or retrieved
--     from @device@
--
-- -   Each element of @pDescriptorSets@ that is a valid handle /must/ have
--     been created, allocated, or retrieved from @descriptorPool@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorPool@ /must/ be externally synchronized
--
-- -   Host access to each member of @pDescriptorSets@ /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'VkDescriptorPool', 'VkDescriptorSet',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFreeDescriptorSets" vkFreeDescriptorSets :: ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
-- | vkUpdateDescriptorSets - Update the contents of a descriptor set object
--
-- = Parameters
--
-- -   @device@ is the logical device that updates the descriptor sets.
--
-- -   @descriptorWriteCount@ is the number of elements in the
--     @pDescriptorWrites@ array.
--
-- -   @pDescriptorWrites@ is a pointer to an array of
--     'VkWriteDescriptorSet' structures describing the descriptor sets to
--     write to.
--
-- -   @descriptorCopyCount@ is the number of elements in the
--     @pDescriptorCopies@ array.
--
-- -   @pDescriptorCopies@ is a pointer to an array of
--     'VkCopyDescriptorSet' structures describing the descriptor sets to
--     copy between.
--
-- = Description
--
-- The operations described by @pDescriptorWrites@ are performed first,
-- followed by the operations described by @pDescriptorCopies@. Within each
-- array, the operations are performed in the order they appear in the
-- array.
--
-- Each element in the @pDescriptorWrites@ array describes an operation
-- updating the descriptor set using descriptors for resources specified in
-- the structure.
--
-- Each element in the @pDescriptorCopies@ array is a 'VkCopyDescriptorSet'
-- structure describing an operation copying descriptors between sets.
--
-- If the @dstSet@ member of any element of @pDescriptorWrites@ or
-- @pDescriptorCopies@ is bound, accessed, or modified by any command that
-- was recorded to a command buffer which is currently in the [recording or
-- executable
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle),
-- and any of the descriptor bindings that are updated were not created
-- with the @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ or
-- @VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT@ bits set,
-- that command buffer becomes
-- [invalid](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- == Valid Usage
--
-- -   Descriptor bindings updated by this command which were created
--     without the @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ or
--     @VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT@ bits set
--     /must/ not be used by any command that was recorded to a command
--     buffer which is in the [pending
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @descriptorWriteCount@ is not @0@, @pDescriptorWrites@ /must/ be
--     a valid pointer to an array of @descriptorWriteCount@ valid
--     @VkWriteDescriptorSet@ structures
--
-- -   If @descriptorCopyCount@ is not @0@, @pDescriptorCopies@ /must/ be a
--     valid pointer to an array of @descriptorCopyCount@ valid
--     @VkCopyDescriptorSet@ structures
--
-- == Host Synchronization
--
-- -   Host access to @pDescriptorWrites@[].dstSet /must/ be externally
--     synchronized
--
-- -   Host access to @pDescriptorCopies@[].dstSet /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'VkCopyDescriptorSet',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkWriteDescriptorSet'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUpdateDescriptorSets" vkUpdateDescriptorSets :: ("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()
-- | VkDescriptorBufferInfo - Structure specifying descriptor buffer info
--
-- = Description
--
-- __Note__
--
-- When setting @range@ to @VK_WHOLE_SIZE@, the effective range /must/ not
-- be larger than the maximum range for the descriptor type
-- ([maxUniformBufferRange](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-maxUniformBufferRange)
-- or
-- [maxStorageBufferRange](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-maxStorageBufferRange)).
-- This means that @VK_WHOLE_SIZE@ is not typically useful in the common
-- case where uniform buffer descriptors are suballocated from a buffer
-- that is much larger than @maxUniformBufferRange@.
--
-- For @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ and
-- @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@ descriptor types, @offset@
-- is the base offset from which the dynamic offset is applied and @range@
-- is the static size used for all dynamic offsets.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @buffer@
--
-- -   If @range@ is not equal to @VK_WHOLE_SIZE@, @range@ /must/ be
--     greater than @0@
--
-- -   If @range@ is not equal to @VK_WHOLE_SIZE@, @range@ /must/ be less
--     than or equal to the size of @buffer@ minus @offset@
--
-- == Valid Usage (Implicit)
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer', @VkDeviceSize@,
-- 'VkWriteDescriptorSet'
data VkDescriptorBufferInfo = VkDescriptorBufferInfo
  { -- | @buffer@ is the buffer resource.
  vkBuffer :: VkBuffer
  , -- | @offset@ is the offset in bytes from the start of @buffer@. Access to
  -- buffer memory via this descriptor uses addressing that is relative to
  -- this starting offset.
  vkOffset :: VkDeviceSize
  , -- | @range@ is the size in bytes that is used for this descriptor update, or
  -- @VK_WHOLE_SIZE@ to use the range from @offset@ to the end of the buffer.
  vkRange :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkDescriptorBufferInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorBufferInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBuffer (poked :: VkDescriptorBufferInfo))
                *> poke (ptr `plusPtr` 8) (vkOffset (poked :: VkDescriptorBufferInfo))
                *> poke (ptr `plusPtr` 16) (vkRange (poked :: VkDescriptorBufferInfo))
-- | VkDescriptorImageInfo - Structure specifying descriptor image info
--
-- = Description
--
-- Members of @VkDescriptorImageInfo@ that are not used in an update (as
-- described above) are ignored.
--
-- == Valid Usage
--
-- -   @imageView@ /must/ not be 2D or 2D array image view created from a
--     3D image
--
-- -   @imageLayout@ /must/ match the actual
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' of each subresource
--     accessible from @imageView@ at the time this descriptor is accessed
--
-- -   If @sampler@ is used and enables [sampler Y’CBCR
--     conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion):
--
--     -   The @format@ of the @imageView@ /must/ be the same as the
--         'Graphics.Vulkan.Core10.Core.VkFormat' of the image
--
--     -   The @aspectMask@ of the @imageView@ /must/ be
--         @VK_IMAGE_ASPECT_COLOR_BIT@
--
--     -   The @components.a@ of the @imageView@ /must/ be
--         @VK_COMPONENT_SWIZZLE_IDENTITY@
--
--     -   The @components.r@ of the @imageView@ /must/ be
--         @VK_COMPONENT_SWIZZLE_IDENTITY@
--
--     -   The @components.g@ of the @imageView@ /must/ be
--         @VK_COMPONENT_SWIZZLE_IDENTITY@
--
--     -   The @components.b@ of the @imageView@ /must/ be
--         @VK_COMPONENT_SWIZZLE_IDENTITY@
--
--     -   The @pNext@ chain of the @imageView@ /must/ contain a
--         @VkSamplerYcbcrConversionInfo@ which has a @conversion@ which is
--         an /identically defined object/ to the @conversion@ of the
--         @VkSamplerYcbcrConversionInfo@ which is in the @pNext@ chain of
--         the @sampler@
--
-- -   If @sampler@ is used and does not enable [sampler Y’CBCR
--     conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#samplers-YCbCr-conversion)
--     and the 'Graphics.Vulkan.Core10.Core.VkFormat' of the image is a
--     [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     the image /must/ have been created with
--     @VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT@, and the @aspectMask@ of the
--     @imageView@ /must/ be @VK_IMAGE_ASPECT_PLANE_0_BIT@,
--     @VK_IMAGE_ASPECT_PLANE_1_BIT@ or (for three-plane formats only)
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@
--
-- == Valid Usage (Implicit)
--
-- -   Both of @imageView@, and @sampler@ that are valid handles /must/
--     have been created, allocated, or retrieved from the same @VkDevice@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageView',
-- 'Graphics.Vulkan.Core10.Sampler.VkSampler', 'VkWriteDescriptorSet'
data VkDescriptorImageInfo = VkDescriptorImageInfo
  { -- | @sampler@ is a sampler handle, and is used in descriptor updates for
  -- types @VK_DESCRIPTOR_TYPE_SAMPLER@ and
  -- @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@ if the binding being updated
  -- does not use immutable samplers.
  vkSampler :: VkSampler
  , -- | @imageView@ is an image view handle, and is used in descriptor updates
  -- for types @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@,
  -- @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@,
  -- @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, and
  -- @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@.
  vkImageView :: VkImageView
  , -- | @imageLayout@ is the layout that the image subresources accessible from
  -- @imageView@ will be in at the time this descriptor is accessed.
  -- @imageLayout@ is used in descriptor updates for types
  -- @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@, @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@,
  -- @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, and
  -- @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@.
  vkImageLayout :: VkImageLayout
  }
  deriving (Eq, Show)

instance Storable VkDescriptorImageInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorImageInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSampler (poked :: VkDescriptorImageInfo))
                *> poke (ptr `plusPtr` 8) (vkImageView (poked :: VkDescriptorImageInfo))
                *> poke (ptr `plusPtr` 16) (vkImageLayout (poked :: VkDescriptorImageInfo))
-- | VkWriteDescriptorSet - Structure specifying the parameters of a
-- descriptor set write operation
--
-- = Description
--
-- Only one of @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@ members
-- is used according to the descriptor type specified in the
-- @descriptorType@ member of the containing @VkWriteDescriptorSet@
-- structure, as specified below.
--
-- If the @dstBinding@ has fewer than @descriptorCount@ array elements
-- remaining starting from @dstArrayElement@, then the remainder will be
-- used to update the subsequent binding - @dstBinding@+1 starting at array
-- element zero. If a binding has a @descriptorCount@ of zero, it is
-- skipped. This behavior applies recursively, with the update affecting
-- consecutive bindings as needed to update all @descriptorCount@
-- descriptors.
--
-- == Valid Usage
--
-- -   @dstBinding@ /must/ be less than or equal to the maximum value of
--     @binding@ of all 'VkDescriptorSetLayoutBinding' structures specified
--     when @dstSet@’s descriptor set layout was created
--
-- -   @dstBinding@ /must/ be a binding with a non-zero @descriptorCount@
--
-- -   All consecutive bindings updated via a single @VkWriteDescriptorSet@
--     structure, except those with a @descriptorCount@ of zero, /must/
--     have identical @descriptorType@ and @stageFlags@.
--
-- -   All consecutive bindings updated via a single @VkWriteDescriptorSet@
--     structure, except those with a @descriptorCount@ of zero, /must/ all
--     either use immutable samplers or /must/ all not use immutable
--     samplers.
--
-- -   @descriptorType@ /must/ match the type of @dstBinding@ within
--     @dstSet@
--
-- -   @dstSet@ /must/ be a valid 'VkDescriptorSet' handle
--
-- -   The sum of @dstArrayElement@ and @descriptorCount@ /must/ be less
--     than or equal to the number of array elements in the descriptor set
--     binding specified by @dstBinding@, and all applicable consecutive
--     bindings, as described by
--     [{html_spec_relative}#descriptorsets-updates-consecutive](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-updates-consecutive)
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_SAMPLER@,
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@,
--     @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@,
--     @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@, or
--     @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@, @pImageInfo@ /must/ be a
--     valid pointer to an array of @descriptorCount@ valid
--     @VkDescriptorImageInfo@ structures
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@, @pTexelBufferView@ /must/
--     be a valid pointer to an array of @descriptorCount@ valid
--     @VkBufferView@ handles
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@,
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@,
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@, or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@, @pBufferInfo@ /must/ be
--     a valid pointer to an array of @descriptorCount@ valid
--     @VkDescriptorBufferInfo@ structures
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_SAMPLER@ or
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, and @dstSet@ was not
--     allocated with a layout that included immutable samplers for
--     @dstBinding@ with @descriptorType@, the @sampler@ member of each
--     element of @pImageInfo@ /must/ be a valid @VkSampler@ object
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@,
--     @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@,
--     @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@, or
--     @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@, the @imageView@ and
--     @imageLayout@ members of each element of @pImageInfo@ /must/ be a
--     valid @VkImageView@ and
--     'Graphics.Vulkan.Core10.Image.VkImageLayout', respectively
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@, for each
--     descriptor that will be accessed via load or store operations the
--     @imageLayout@ member for corresponding elements of @pImageInfo@
--     /must/ be @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@, the @offset@ member of
--     each element of @pBufferInfo@ /must/ be a multiple of
--     @VkPhysicalDeviceLimits@::@minUniformBufferOffsetAlignment@
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@, the @offset@ member of
--     each element of @pBufferInfo@ /must/ be a multiple of
--     @VkPhysicalDeviceLimits@::@minStorageBufferOffsetAlignment@
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@,
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@,
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@, or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@, and the @buffer@ member
--     of any element of @pBufferInfo@ is the handle of a non-sparse
--     buffer, then that buffer /must/ be bound completely and contiguously
--     to a single @VkDeviceMemory@ object
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@, the @buffer@ member of
--     each element of @pBufferInfo@ /must/ have been created with
--     @VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT@ set
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@, the @buffer@ member of
--     each element of @pBufferInfo@ /must/ have been created with
--     @VK_BUFFER_USAGE_STORAGE_BUFFER_BIT@ set
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@, the @range@ member of
--     each element of @pBufferInfo@, or the effective range if @range@ is
--     @VK_WHOLE_SIZE@, /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxUniformBufferRange@
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@, the @range@ member of
--     each element of @pBufferInfo@, or the effective range if @range@ is
--     @VK_WHOLE_SIZE@, /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxStorageBufferRange@
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER@,
--     the @VkBuffer@ that each element of @pTexelBufferView@ was created
--     from /must/ have been created with
--     @VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT@ set
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER@,
--     the @VkBuffer@ that each element of @pTexelBufferView@ was created
--     from /must/ have been created with
--     @VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT@ set
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@ or
--     @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@, the @imageView@ member of
--     each element of @pImageInfo@ /must/ have been created with the
--     identity swizzle
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@ or
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, the @imageView@ member
--     of each element of @pImageInfo@ /must/ have been created with
--     @VK_IMAGE_USAGE_SAMPLED_BIT@ set
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE@ or
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, the @imageLayout@
--     member of each element of @pImageInfo@ /must/ be
--     @VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL@,
--     @VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@, the
--     @imageView@ member of each element of @pImageInfo@ /must/ have been
--     created with @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@ set
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_STORAGE_IMAGE@, the
--     @imageView@ member of each element of @pImageInfo@ /must/ have been
--     created with @VK_IMAGE_USAGE_STORAGE_BIT@ set
--
-- -   All consecutive bindings updated via a single @VkWriteDescriptorSet@
--     structure, except those with a @descriptorCount@ of zero, /must/
--     have identical
--     'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagBitsEXT'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @descriptorType@ /must/ be a valid 'VkDescriptorType' value
--
-- -   @descriptorCount@ /must/ be greater than @0@
--
-- -   Both of @dstSet@, and the elements of @pTexelBufferView@ that are
--     valid handles /must/ have been created, allocated, or retrieved from
--     the same @VkDevice@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BufferView.VkBufferView',
-- 'VkDescriptorBufferInfo', 'VkDescriptorImageInfo', 'VkDescriptorSet',
-- 'VkDescriptorType', 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetKHR',
-- 'vkUpdateDescriptorSets'
data VkWriteDescriptorSet = VkWriteDescriptorSet
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @dstSet@ is the destination descriptor set to update.
  vkDstSet :: VkDescriptorSet
  , -- | @dstBinding@ is the descriptor binding within that set.
  vkDstBinding :: Word32
  , -- | @dstArrayElement@ is the starting element in that array.
  vkDstArrayElement :: Word32
  , -- | @descriptorCount@ is the number of descriptors to update (the number of
  -- elements in @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@).
  vkDescriptorCount :: Word32
  , -- | @descriptorType@ is a 'VkDescriptorType' specifying the type of each
  -- descriptor in @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@, as
  -- described below. It /must/ be the same type as that specified in
  -- @VkDescriptorSetLayoutBinding@ for @dstSet@ at @dstBinding@. The type of
  -- the descriptor also controls which array the descriptors are taken from.
  vkDescriptorType :: VkDescriptorType
  , -- | @pImageInfo@ points to an array of 'VkDescriptorImageInfo' structures or
  -- is ignored, as described below.
  vkPImageInfo :: Ptr VkDescriptorImageInfo
  , -- | @pBufferInfo@ points to an array of 'VkDescriptorBufferInfo' structures
  -- or is ignored, as described below.
  vkPBufferInfo :: Ptr VkDescriptorBufferInfo
  , -- | @pTexelBufferView@ points to an array of
  -- 'Graphics.Vulkan.Core10.BufferView.VkBufferView' handles as described in
  -- the [Buffer
  -- Views](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-buffer-views)
  -- section or is ignored, as described below.
  vkPTexelBufferView :: Ptr VkBufferView
  }
  deriving (Eq, Show)

instance Storable VkWriteDescriptorSet where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkWriteDescriptorSet <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 28)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 36)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
                                  <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 16) (vkDstSet (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 24) (vkDstBinding (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 28) (vkDstArrayElement (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 32) (vkDescriptorCount (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 36) (vkDescriptorType (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 40) (vkPImageInfo (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 48) (vkPBufferInfo (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 56) (vkPTexelBufferView (poked :: VkWriteDescriptorSet))
-- | VkCopyDescriptorSet - Structure specifying a copy descriptor set
-- operation
--
-- == Valid Usage
--
-- -   @srcBinding@ /must/ be a valid binding within @srcSet@
--
-- -   The sum of @srcArrayElement@ and @descriptorCount@ /must/ be less
--     than or equal to the number of array elements in the descriptor set
--     binding specified by @srcBinding@, and all applicable consecutive
--     bindings, as described by
--     [{html_spec_relative}#descriptorsets-updates-consecutive](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-updates-consecutive)
--
-- -   @dstBinding@ /must/ be a valid binding within @dstSet@
--
-- -   The sum of @dstArrayElement@ and @descriptorCount@ /must/ be less
--     than or equal to the number of array elements in the descriptor set
--     binding specified by @dstBinding@, and all applicable consecutive
--     bindings, as described by
--     [{html_spec_relative}#descriptorsets-updates-consecutive](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-updates-consecutive)
--
-- -   If @srcSet@ is equal to @dstSet@, then the source and destination
--     ranges of descriptors /must/ not overlap, where the ranges /may/
--     include array elements from consecutive bindings as described by
--     [{html_spec_relative}#descriptorsets-updates-consecutive](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-updates-consecutive)
--
-- -   If @srcSet@’s layout was created with the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     flag set, then @dstSet@’s layout /must/ also have been created with
--     the @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     flag set
--
-- -   If @srcSet@’s layout was created without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     flag set, then @dstSet@’s layout /must/ also have been created
--     without the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--     flag set
--
-- -   If the descriptor pool from which @srcSet@ was allocated was created
--     with the @VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT@ flag
--     set, then the descriptor pool from which @dstSet@ was allocated
--     /must/ also have been created with the
--     @VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT@ flag set
--
-- -   If the descriptor pool from which @srcSet@ was allocated was created
--     without the @VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT@
--     flag set, then the descriptor pool from which @dstSet@ was allocated
--     /must/ also have been created without the
--     @VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT@ flag set
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @srcSet@ /must/ be a valid @VkDescriptorSet@ handle
--
-- -   @dstSet@ /must/ be a valid @VkDescriptorSet@ handle
--
-- -   Both of @dstSet@, and @srcSet@ /must/ have been created, allocated,
--     or retrieved from the same @VkDevice@
--
-- = See Also
--
-- 'VkDescriptorSet', 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkUpdateDescriptorSets'
data VkCopyDescriptorSet = VkCopyDescriptorSet
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @srcSet@, @srcBinding@, and @srcArrayElement@ are the source set,
  -- binding, and array element, respectively.
  vkSrcSet :: VkDescriptorSet
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcBinding"
  vkSrcBinding :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcArrayElement"
  vkSrcArrayElement :: Word32
  , -- | @dstSet@, @dstBinding@, and @dstArrayElement@ are the destination set,
  -- binding, and array element, respectively.
  vkDstSet :: VkDescriptorSet
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstBinding"
  vkDstBinding :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstArrayElement"
  vkDstArrayElement :: Word32
  , -- | @descriptorCount@ is the number of descriptors to copy from the source
  -- to destination. If @descriptorCount@ is greater than the number of
  -- remaining array elements in the source or destination binding, those
  -- affect consecutive bindings in a manner similar to
  -- 'VkWriteDescriptorSet' above.
  vkDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkCopyDescriptorSet where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkCopyDescriptorSet <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 28)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 40)
                                 <*> peek (ptr `plusPtr` 44)
                                 <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 16) (vkSrcSet (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 24) (vkSrcBinding (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 28) (vkSrcArrayElement (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 32) (vkDstSet (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 40) (vkDstBinding (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 44) (vkDstArrayElement (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 48) (vkDescriptorCount (poked :: VkCopyDescriptorSet))
-- | VkDescriptorSetLayoutBinding - Structure specifying a descriptor set
-- layout binding
--
-- = Members
--
-- -   @binding@ is the binding number of this entry and corresponds to a
--     resource of the same binding number in the shader stages.
--
-- -   @descriptorType@ is a 'VkDescriptorType' specifying which type of
--     resource descriptors are used for this binding.
--
-- -   @descriptorCount@ is the number of descriptors contained in the
--     binding, accessed in a shader as an array. If @descriptorCount@ is
--     zero this binding entry is reserved and the resource /must/ not be
--     accessed from any stage via this binding within any pipeline using
--     the set layout.
--
-- -   @stageFlags@ member is a bitmask of
--     'Graphics.Vulkan.Core10.Pipeline.VkShaderStageFlagBits' specifying
--     which pipeline shader stages /can/ access a resource for this
--     binding. @VK_SHADER_STAGE_ALL@ is a shorthand specifying that all
--     defined shader stages, including any additional stages defined by
--     extensions, /can/ access the resource.
--
--     If a shader stage is not included in @stageFlags@, then a resource
--     /must/ not be accessed from that stage via this binding within any
--     pipeline using the set layout. Other than input attachments which
--     are limited to the fragment shader, there are no limitations on what
--     combinations of stages /can/ use a descriptor binding, and in
--     particular a binding /can/ be used by both graphics stages and the
--     compute stage.
--
-- = Description
--
-- -   @pImmutableSamplers@ affects initialization of samplers. If
--     @descriptorType@ specifies a @VK_DESCRIPTOR_TYPE_SAMPLER@ or
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@ type descriptor, then
--     @pImmutableSamplers@ /can/ be used to initialize a set of /immutable
--     samplers/. Immutable samplers are permanently bound into the set
--     layout; later binding a sampler into an immutable sampler slot in a
--     descriptor set is not allowed. If @pImmutableSamplers@ is not
--     @NULL@, then it is considered to be a pointer to an array of sampler
--     handles that will be consumed by the set layout and used for the
--     corresponding binding. If @pImmutableSamplers@ is @NULL@, then the
--     sampler slots are dynamic and sampler handles /must/ be bound into
--     descriptor sets using this layout. If @descriptorType@ is not one of
--     these descriptor types, then @pImmutableSamplers@ is ignored.
--
-- The above layout definition allows the descriptor bindings to be
-- specified sparsely such that not all binding numbers between 0 and the
-- maximum binding number need to be specified in the @pBindings@ array.
-- Bindings that are not specified have a @descriptorCount@ and
-- @stageFlags@ of zero, and the @descriptorType@ is treated as undefined.
-- However, all binding numbers between 0 and the maximum binding number in
-- the 'VkDescriptorSetLayoutCreateInfo'::@pBindings@ array /may/ consume
-- memory in the descriptor set layout even if not all descriptor bindings
-- are used, though it /should/ not consume additional memory from the
-- descriptor pool.
--
-- __Note__
--
-- The maximum binding number specified /should/ be as compact as possible
-- to avoid wasted memory.
--
-- == Valid Usage
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_SAMPLER@ or
--     @VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER@, and @descriptorCount@
--     is not @0@ and @pImmutableSamplers@ is not @NULL@,
--     @pImmutableSamplers@ /must/ be a valid pointer to an array of
--     @descriptorCount@ valid @VkSampler@ handles
--
-- -   If @descriptorCount@ is not @0@, @stageFlags@ /must/ be a valid
--     combination of
--     'Graphics.Vulkan.Core10.Pipeline.VkShaderStageFlagBits' values
--
-- -   If @descriptorType@ is @VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT@ and
--     @descriptorCount@ is not @0@, then @stageFlags@ /must/ be @0@ or
--     @VK_SHADER_STAGE_FRAGMENT_BIT@
--
-- == Valid Usage (Implicit)
--
-- -   @descriptorType@ /must/ be a valid 'VkDescriptorType' value
--
-- = See Also
--
-- 'VkDescriptorSetLayoutCreateInfo', 'VkDescriptorType',
-- 'Graphics.Vulkan.Core10.Sampler.VkSampler',
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkShaderStageFlags'
data VkDescriptorSetLayoutBinding = VkDescriptorSetLayoutBinding
  { -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "binding"
  vkBinding :: Word32
  , -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "descriptorType"
  vkDescriptorType :: VkDescriptorType
  , -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "descriptorCount"
  vkDescriptorCount :: Word32
  , -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "stageFlags"
  vkStageFlags :: VkShaderStageFlags
  , -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "pImmutableSamplers"
  vkPImmutableSamplers :: Ptr VkSampler
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetLayoutBinding where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutBinding <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 4)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 12)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBinding (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 4) (vkDescriptorType (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 8) (vkDescriptorCount (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 12) (vkStageFlags (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 16) (vkPImmutableSamplers (poked :: VkDescriptorSetLayoutBinding))
-- | VkDescriptorSetLayoutCreateInfo - Structure specifying parameters of a
-- newly created descriptor set layout
--
-- == Valid Usage
--
-- -   The 'VkDescriptorSetLayoutBinding'::@binding@ members of the
--     elements of the @pBindings@ array /must/ each have different values.
--
-- -   If @flags@ contains
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR@, then all
--     elements of @pBindings@ /must/ not have a @descriptorType@ of
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@
--
-- -   If @flags@ contains
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR@, then the
--     total number of elements of all bindings /must/ be less than or
--     equal to
--     'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.VkPhysicalDevicePushDescriptorPropertiesKHR'::@maxPushDescriptors@
--
-- -   If any binding has the
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ bit set, @flags@
--     /must/ include
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@
--
-- -   If any binding has the
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ bit set, then all
--     bindings /must/ not have @descriptorType@ of
--     @VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC@ or
--     @VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkDescriptorSetLayoutBindingFlagsCreateInfoEXT'
--
-- -   @flags@ /must/ be a valid combination of
--     'VkDescriptorSetLayoutCreateFlagBits' values
--
-- -   If @bindingCount@ is not @0@, @pBindings@ /must/ be a valid pointer
--     to an array of @bindingCount@ valid @VkDescriptorSetLayoutBinding@
--     structures
--
-- = See Also
--
-- 'VkDescriptorSetLayoutBinding', 'VkDescriptorSetLayoutCreateFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.Extensions.VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupportKHR'
data VkDescriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkDescriptorSetLayoutCreateFlagBits' specifying
  -- options for descriptor set layout creation.
  vkFlags :: VkDescriptorSetLayoutCreateFlags
  , -- | @bindingCount@ is the number of elements in @pBindings@.
  vkBindingCount :: Word32
  , -- | @pBindings@ is a pointer to an array of 'VkDescriptorSetLayoutBinding'
  -- structures.
  vkPBindings :: Ptr VkDescriptorSetLayoutBinding
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetLayoutCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkBindingCount (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPBindings (poked :: VkDescriptorSetLayoutCreateInfo))
-- | VkDescriptorPoolSize - Structure specifying descriptor pool size
--
-- == Valid Usage
--
-- -   @descriptorCount@ /must/ be greater than @0@
--
-- == Valid Usage (Implicit)
--
-- -   @type@ /must/ be a valid 'VkDescriptorType' value
--
-- = See Also
--
-- 'VkDescriptorPoolCreateInfo', 'VkDescriptorType'
data VkDescriptorPoolSize = VkDescriptorPoolSize
  { -- | @type@ is the type of descriptor.
  vkType :: VkDescriptorType
  , -- | @descriptorCount@ is the number of descriptors of that type to allocate.
  vkDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorPoolSize where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkDescriptorPoolSize <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkDescriptorPoolSize))
                *> poke (ptr `plusPtr` 4) (vkDescriptorCount (poked :: VkDescriptorPoolSize))
-- | VkDescriptorPoolCreateInfo - Structure specifying parameters of a newly
-- created descriptor pool
--
-- = Description
--
-- If multiple @VkDescriptorPoolSize@ structures appear in the @pPoolSizes@
-- array then the pool will be created with enough storage for the total
-- number of descriptors of each type.
--
-- Fragmentation of a descriptor pool is possible and /may/ lead to
-- descriptor set allocation failures. A failure due to fragmentation is
-- defined as failing a descriptor set allocation despite the sum of all
-- outstanding descriptor set allocations from the pool plus the requested
-- allocation requiring no more than the total number of descriptors
-- requested at pool creation. Implementations provide certain guarantees
-- of when fragmentation /must/ not cause allocation failure, as described
-- below.
--
-- If a descriptor pool has not had any descriptor sets freed since it was
-- created or most recently reset then fragmentation /must/ not cause an
-- allocation failure (note that this is always the case for a pool created
-- without the @VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT@ bit
-- set). Additionally, if all sets allocated from the pool since it was
-- created or most recently reset use the same number of descriptors (of
-- each type) and the requested allocation also uses that same number of
-- descriptors (of each type), then fragmentation /must/ not cause an
-- allocation failure.
--
-- If an allocation failure occurs due to fragmentation, an application
-- /can/ create an additional descriptor pool to perform further descriptor
-- set allocations.
--
-- If @flags@ has the @VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT@
-- bit set, descriptor pool creation /may/ fail with the error
-- @VK_ERROR_FRAGMENTATION_EXT@ if the total number of descriptors across
-- all pools (including this one) created with this bit set exceeds
-- @maxUpdateAfterBindDescriptorsInAllPools@, or if fragmentation of the
-- underlying hardware resources occurs.
--
-- == Valid Usage
--
-- -   @maxSets@ /must/ be greater than @0@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be a valid combination of
--     'VkDescriptorPoolCreateFlagBits' values
--
-- -   @pPoolSizes@ /must/ be a valid pointer to an array of
--     @poolSizeCount@ valid @VkDescriptorPoolSize@ structures
--
-- -   @poolSizeCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'VkDescriptorPoolCreateFlags', 'VkDescriptorPoolSize',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCreateDescriptorPool'
data VkDescriptorPoolCreateInfo = VkDescriptorPoolCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkDescriptorPoolCreateFlagBits' specifying
  -- certain supported operations on the pool.
  vkFlags :: VkDescriptorPoolCreateFlags
  , -- | @maxSets@ is the maximum number of descriptor sets that /can/ be
  -- allocated from the pool.
  vkMaxSets :: Word32
  , -- | @poolSizeCount@ is the number of elements in @pPoolSizes@.
  vkPoolSizeCount :: Word32
  , -- | @pPoolSizes@ is a pointer to an array of @VkDescriptorPoolSize@
  -- structures, each containing a descriptor type and number of descriptors
  -- of that type to be allocated in the pool.
  vkPPoolSizes :: Ptr VkDescriptorPoolSize
  }
  deriving (Eq, Show)

instance Storable VkDescriptorPoolCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDescriptorPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkMaxSets (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPoolSizeCount (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPPoolSizes (poked :: VkDescriptorPoolCreateInfo))
-- | VkDescriptorSetAllocateInfo - Structure specifying the allocation
-- parameters for descriptor sets
--
-- == Valid Usage
--
-- -   Each element of @pSetLayouts@ /must/ not have been created with
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR@ set
--
-- -   If any element of @pSetLayouts@ was created with the
--     @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
--     set, @descriptorPool@ /must/ have been created with the
--     @VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT@ flag set
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkDescriptorSetVariableDescriptorCountAllocateInfoEXT'
--
-- -   @descriptorPool@ /must/ be a valid @VkDescriptorPool@ handle
--
-- -   @pSetLayouts@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ valid @VkDescriptorSetLayout@ handles
--
-- -   @descriptorSetCount@ /must/ be greater than @0@
--
-- -   Both of @descriptorPool@, and the elements of @pSetLayouts@ /must/
--     have been created, allocated, or retrieved from the same @VkDevice@
--
-- = See Also
--
-- 'VkDescriptorPool',
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkAllocateDescriptorSets'
data VkDescriptorSetAllocateInfo = VkDescriptorSetAllocateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @descriptorPool@ is the pool which the sets will be allocated from.
  vkDescriptorPool :: VkDescriptorPool
  , -- | @descriptorSetCount@ determines the number of descriptor sets to be
  -- allocated from the pool.
  vkDescriptorSetCount :: Word32
  , -- | @pSetLayouts@ is an array of descriptor set layouts, with each member
  -- specifying how the corresponding descriptor set is allocated.
  vkPSetLayouts :: Ptr VkDescriptorSetLayout
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetAllocateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDescriptorSetAllocateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkDescriptorPool (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkDescriptorSetCount (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 32) (vkPSetLayouts (poked :: VkDescriptorSetAllocateInfo))
-- | VkDescriptorSetLayoutCreateFlags - Bitmask of
-- VkDescriptorSetLayoutCreateFlagBits
--
-- = Description
--
-- @VkDescriptorSetLayoutCreateFlags@ is a bitmask type for setting a mask
-- of zero or more 'VkDescriptorSetLayoutCreateFlagBits'.
--
-- = See Also
--
-- 'VkDescriptorSetLayoutCreateFlagBits', 'VkDescriptorSetLayoutCreateInfo'
type VkDescriptorSetLayoutCreateFlags = VkDescriptorSetLayoutCreateFlagBits
-- | VkDescriptorPoolCreateFlags - Bitmask of VkDescriptorPoolCreateFlagBits
--
-- = Description
--
-- @VkDescriptorPoolCreateFlags@ is a bitmask type for setting a mask of
-- zero or more 'VkDescriptorPoolCreateFlagBits'.
--
-- = See Also
--
-- 'VkDescriptorPoolCreateFlagBits', 'VkDescriptorPoolCreateInfo'
type VkDescriptorPoolCreateFlags = VkDescriptorPoolCreateFlagBits
