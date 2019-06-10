{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkCopyDescriptorSet(..)
  , VkDescriptorBufferInfo(..)
  , VkDescriptorImageInfo(..)
  , VkDescriptorPool
  , VkDescriptorPoolCreateFlagBits(..)
  , pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
  , VkDescriptorPoolCreateFlags
  , VkDescriptorPoolCreateInfo(..)
  , VkDescriptorPoolResetFlags(..)
  , VkDescriptorPoolSize(..)
  , VkDescriptorSet
  , VkDescriptorSetAllocateInfo(..)
  , VkDescriptorSetLayoutBinding(..)
  , VkDescriptorSetLayoutCreateFlagBits(..)
  , VkDescriptorSetLayoutCreateFlags
  , VkDescriptorSetLayoutCreateInfo(..)
  , VkDescriptorType(..)
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
  , VkWriteDescriptorSet(..)
  , FN_vkAllocateDescriptorSets
  , PFN_vkAllocateDescriptorSets
  , vkAllocateDescriptorSets
  , FN_vkCreateDescriptorPool
  , PFN_vkCreateDescriptorPool
  , vkCreateDescriptorPool
  , FN_vkCreateDescriptorSetLayout
  , PFN_vkCreateDescriptorSetLayout
  , vkCreateDescriptorSetLayout
  , FN_vkDestroyDescriptorPool
  , PFN_vkDestroyDescriptorPool
  , vkDestroyDescriptorPool
  , FN_vkDestroyDescriptorSetLayout
  , PFN_vkDestroyDescriptorSetLayout
  , vkDestroyDescriptorSetLayout
  , FN_vkFreeDescriptorSets
  , PFN_vkFreeDescriptorSets
  , vkFreeDescriptorSets
  , FN_vkResetDescriptorPool
  , PFN_vkResetDescriptorPool
  , vkResetDescriptorPool
  , FN_vkUpdateDescriptorSets
  , PFN_vkUpdateDescriptorSets
  , vkUpdateDescriptorSets
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
  ( FunPtr
  , Ptr
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


import Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferView
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageView
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  , VkShaderStageFlags
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkSampler
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkCopyDescriptorSet"
data VkCopyDescriptorSet = VkCopyDescriptorSet
  { -- No documentation found for Nested "VkCopyDescriptorSet" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkCopyDescriptorSet" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcSet"
  vkSrcSet :: VkDescriptorSet
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcBinding"
  vkSrcBinding :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcArrayElement"
  vkSrcArrayElement :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstSet"
  vkDstSet :: VkDescriptorSet
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstBinding"
  vkDstBinding :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstArrayElement"
  vkDstArrayElement :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "descriptorCount"
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

instance Zero VkCopyDescriptorSet where
  zero = VkCopyDescriptorSet VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero

-- No documentation found for TopLevel "VkDescriptorBufferInfo"
data VkDescriptorBufferInfo = VkDescriptorBufferInfo
  { -- No documentation found for Nested "VkDescriptorBufferInfo" "buffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkDescriptorBufferInfo" "offset"
  vkOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkDescriptorBufferInfo" "range"
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

instance Zero VkDescriptorBufferInfo where
  zero = VkDescriptorBufferInfo zero
                                zero
                                zero

-- No documentation found for TopLevel "VkDescriptorImageInfo"
data VkDescriptorImageInfo = VkDescriptorImageInfo
  { -- No documentation found for Nested "VkDescriptorImageInfo" "sampler"
  vkSampler :: VkSampler
  , -- No documentation found for Nested "VkDescriptorImageInfo" "imageView"
  vkImageView :: VkImageView
  , -- No documentation found for Nested "VkDescriptorImageInfo" "imageLayout"
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

instance Zero VkDescriptorImageInfo where
  zero = VkDescriptorImageInfo zero
                               zero
                               zero

-- | Dummy data to tag the 'Ptr' with
data VkDescriptorPool_T
-- No documentation found for TopLevel "VkDescriptorPool"
type VkDescriptorPool = Ptr VkDescriptorPool_T

-- ** VkDescriptorPoolCreateFlagBits

-- No documentation found for TopLevel "VkDescriptorPoolCreateFlagBits"
newtype VkDescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkDescriptorPoolCreateFlagBits" "VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT"
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT :: VkDescriptorPoolCreateFlagBits
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = VkDescriptorPoolCreateFlagBits 0x00000001

-- No documentation found for TopLevel "VkDescriptorPoolCreateFlags"
type VkDescriptorPoolCreateFlags = VkDescriptorPoolCreateFlagBits

-- No documentation found for TopLevel "VkDescriptorPoolCreateInfo"
data VkDescriptorPoolCreateInfo = VkDescriptorPoolCreateInfo
  { -- No documentation found for Nested "VkDescriptorPoolCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDescriptorPoolCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDescriptorPoolCreateInfo" "flags"
  vkFlags :: VkDescriptorPoolCreateFlags
  , -- No documentation found for Nested "VkDescriptorPoolCreateInfo" "maxSets"
  vkMaxSets :: Word32
  , -- No documentation found for Nested "VkDescriptorPoolCreateInfo" "poolSizeCount"
  vkPoolSizeCount :: Word32
  , -- No documentation found for Nested "VkDescriptorPoolCreateInfo" "pPoolSizes"
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

instance Zero VkDescriptorPoolCreateInfo where
  zero = VkDescriptorPoolCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero

-- ** VkDescriptorPoolResetFlags

-- No documentation found for TopLevel "VkDescriptorPoolResetFlags"
newtype VkDescriptorPoolResetFlags = VkDescriptorPoolResetFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- No documentation found for TopLevel "VkDescriptorPoolSize"
data VkDescriptorPoolSize = VkDescriptorPoolSize
  { -- No documentation found for Nested "VkDescriptorPoolSize" "type"
  vkType :: VkDescriptorType
  , -- No documentation found for Nested "VkDescriptorPoolSize" "descriptorCount"
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

instance Zero VkDescriptorPoolSize where
  zero = VkDescriptorPoolSize zero
                              zero

-- | Dummy data to tag the 'Ptr' with
data VkDescriptorSet_T
-- No documentation found for TopLevel "VkDescriptorSet"
type VkDescriptorSet = Ptr VkDescriptorSet_T

-- No documentation found for TopLevel "VkDescriptorSetAllocateInfo"
data VkDescriptorSetAllocateInfo = VkDescriptorSetAllocateInfo
  { -- No documentation found for Nested "VkDescriptorSetAllocateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDescriptorSetAllocateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDescriptorSetAllocateInfo" "descriptorPool"
  vkDescriptorPool :: VkDescriptorPool
  , -- No documentation found for Nested "VkDescriptorSetAllocateInfo" "descriptorSetCount"
  vkDescriptorSetCount :: Word32
  , -- No documentation found for Nested "VkDescriptorSetAllocateInfo" "pSetLayouts"
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

instance Zero VkDescriptorSetAllocateInfo where
  zero = VkDescriptorSetAllocateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
                                     zero
                                     zero
                                     zero
                                     zero

-- No documentation found for TopLevel "VkDescriptorSetLayoutBinding"
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

instance Zero VkDescriptorSetLayoutBinding where
  zero = VkDescriptorSetLayoutBinding zero
                                      zero
                                      zero
                                      zero
                                      zero

-- ** VkDescriptorSetLayoutCreateFlagBits

-- No documentation found for TopLevel "VkDescriptorSetLayoutCreateFlagBits"
newtype VkDescriptorSetLayoutCreateFlagBits = VkDescriptorSetLayoutCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- No documentation found for TopLevel "VkDescriptorSetLayoutCreateFlags"
type VkDescriptorSetLayoutCreateFlags = VkDescriptorSetLayoutCreateFlagBits

-- No documentation found for TopLevel "VkDescriptorSetLayoutCreateInfo"
data VkDescriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo
  { -- No documentation found for Nested "VkDescriptorSetLayoutCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDescriptorSetLayoutCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDescriptorSetLayoutCreateInfo" "flags"
  vkFlags :: VkDescriptorSetLayoutCreateFlags
  , -- No documentation found for Nested "VkDescriptorSetLayoutCreateInfo" "bindingCount"
  vkBindingCount :: Word32
  , -- No documentation found for Nested "VkDescriptorSetLayoutCreateInfo" "pBindings"
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

instance Zero VkDescriptorSetLayoutCreateInfo where
  zero = VkDescriptorSetLayoutCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
                                         zero
                                         zero
                                         zero
                                         zero

-- ** VkDescriptorType

-- No documentation found for TopLevel "VkDescriptorType"
newtype VkDescriptorType = VkDescriptorType Int32
  deriving (Eq, Ord, Storable, Zero)

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
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDescriptorType 1000138000) = showString "VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT"
  showsPrec _ (VkDescriptorType 1000165000) = showString "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
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
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT",  pure (VkDescriptorType 1000138000))
                             , ("VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV", pure (VkDescriptorType 1000165000))
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

-- No documentation found for TopLevel "VkWriteDescriptorSet"
data VkWriteDescriptorSet = VkWriteDescriptorSet
  { -- No documentation found for Nested "VkWriteDescriptorSet" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkWriteDescriptorSet" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkWriteDescriptorSet" "dstSet"
  vkDstSet :: VkDescriptorSet
  , -- No documentation found for Nested "VkWriteDescriptorSet" "dstBinding"
  vkDstBinding :: Word32
  , -- No documentation found for Nested "VkWriteDescriptorSet" "dstArrayElement"
  vkDstArrayElement :: Word32
  , -- No documentation found for Nested "VkWriteDescriptorSet" "descriptorCount"
  vkDescriptorCount :: Word32
  , -- No documentation found for Nested "VkWriteDescriptorSet" "descriptorType"
  vkDescriptorType :: VkDescriptorType
  , -- No documentation found for Nested "VkWriteDescriptorSet" "pImageInfo"
  vkPImageInfo :: Ptr VkDescriptorImageInfo
  , -- No documentation found for Nested "VkWriteDescriptorSet" "pBufferInfo"
  vkPBufferInfo :: Ptr VkDescriptorBufferInfo
  , -- No documentation found for Nested "VkWriteDescriptorSet" "pTexelBufferView"
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

instance Zero VkWriteDescriptorSet where
  zero = VkWriteDescriptorSet VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero

-- No documentation found for TopLevel "vkAllocateDescriptorSets"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAllocateDescriptorSets" vkAllocateDescriptorSets :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
#else
vkAllocateDescriptorSets :: DeviceCmds -> ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
vkAllocateDescriptorSets deviceCmds = mkVkAllocateDescriptorSets (pVkAllocateDescriptorSets deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateDescriptorSets
  :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult)
#endif

type FN_vkAllocateDescriptorSets = ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
type PFN_vkAllocateDescriptorSets = FunPtr FN_vkAllocateDescriptorSets

-- No documentation found for TopLevel "vkCreateDescriptorPool"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDescriptorPool" vkCreateDescriptorPool :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult
#else
vkCreateDescriptorPool :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult
vkCreateDescriptorPool deviceCmds = mkVkCreateDescriptorPool (pVkCreateDescriptorPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorPool
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult)
#endif

type FN_vkCreateDescriptorPool = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult
type PFN_vkCreateDescriptorPool = FunPtr FN_vkCreateDescriptorPool

-- No documentation found for TopLevel "vkCreateDescriptorSetLayout"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDescriptorSetLayout" vkCreateDescriptorSetLayout :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult
#else
vkCreateDescriptorSetLayout :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult
vkCreateDescriptorSetLayout deviceCmds = mkVkCreateDescriptorSetLayout (pVkCreateDescriptorSetLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorSetLayout
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult)
#endif

type FN_vkCreateDescriptorSetLayout = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult
type PFN_vkCreateDescriptorSetLayout = FunPtr FN_vkCreateDescriptorSetLayout

-- No documentation found for TopLevel "vkDestroyDescriptorPool"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDescriptorPool" vkDestroyDescriptorPool :: ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyDescriptorPool :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyDescriptorPool deviceCmds = mkVkDestroyDescriptorPool (pVkDestroyDescriptorPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorPool
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyDescriptorPool = ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDescriptorPool = FunPtr FN_vkDestroyDescriptorPool

-- No documentation found for TopLevel "vkDestroyDescriptorSetLayout"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDescriptorSetLayout" vkDestroyDescriptorSetLayout :: ("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyDescriptorSetLayout :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyDescriptorSetLayout deviceCmds = mkVkDestroyDescriptorSetLayout (pVkDestroyDescriptorSetLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorSetLayout
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyDescriptorSetLayout = ("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDescriptorSetLayout = FunPtr FN_vkDestroyDescriptorSetLayout

-- No documentation found for TopLevel "vkFreeDescriptorSets"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFreeDescriptorSets" vkFreeDescriptorSets :: ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
#else
vkFreeDescriptorSets :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
vkFreeDescriptorSets deviceCmds = mkVkFreeDescriptorSets (pVkFreeDescriptorSets deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeDescriptorSets
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult) -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult)
#endif

type FN_vkFreeDescriptorSets = ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
type PFN_vkFreeDescriptorSets = FunPtr FN_vkFreeDescriptorSets

-- No documentation found for TopLevel "vkResetDescriptorPool"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetDescriptorPool" vkResetDescriptorPool :: ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult
#else
vkResetDescriptorPool :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult
vkResetDescriptorPool deviceCmds = mkVkResetDescriptorPool (pVkResetDescriptorPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetDescriptorPool
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult) -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult)
#endif

type FN_vkResetDescriptorPool = ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult
type PFN_vkResetDescriptorPool = FunPtr FN_vkResetDescriptorPool

-- No documentation found for TopLevel "vkUpdateDescriptorSets"
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUpdateDescriptorSets" vkUpdateDescriptorSets :: ("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()
#else
vkUpdateDescriptorSets :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()
vkUpdateDescriptorSets deviceCmds = mkVkUpdateDescriptorSets (pVkUpdateDescriptorSets deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateDescriptorSets
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ())
#endif

type FN_vkUpdateDescriptorSets = ("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()
type PFN_vkUpdateDescriptorSets = FunPtr FN_vkUpdateDescriptorSets
