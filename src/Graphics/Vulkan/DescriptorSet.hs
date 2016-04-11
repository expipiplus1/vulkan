{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.DescriptorSet where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( Ptr
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void
                )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )

-- ** vkUpdateDescriptorSets
foreign import ccall "vkUpdateDescriptorSets" vkUpdateDescriptorSets ::
  Device ->
  Word32 ->
    Ptr VkWriteDescriptorSet ->
      Word32 -> Ptr VkCopyDescriptorSet -> IO ()

-- ** VkDescriptorPoolResetFlags
-- | Opaque flag
newtype VkDescriptorPoolResetFlags = VkDescriptorPoolResetFlags VkFlags
  deriving (Eq, Storable)

-- ** vkAllocateDescriptorSets
foreign import ccall "vkAllocateDescriptorSets" vkAllocateDescriptorSets ::
  Device ->
  Ptr VkDescriptorSetAllocateInfo -> Ptr DescriptorSet -> IO VkResult


data VkDescriptorBufferInfo =
  VkDescriptorBufferInfo{ buffer :: Buffer 
                        , offset :: VkDeviceSize 
                        , range :: VkDeviceSize 
                        }
  deriving (Eq)

instance Storable VkDescriptorBufferInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorBufferInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (buffer (poked :: VkDescriptorBufferInfo))
                *> poke (ptr `plusPtr` 8) (offset (poked :: VkDescriptorBufferInfo))
                *> poke (ptr `plusPtr` 16) (range (poked :: VkDescriptorBufferInfo))



data VkDescriptorImageInfo =
  VkDescriptorImageInfo{ sampler :: Sampler 
                       , imageView :: ImageView 
                       , imageLayout :: VkImageLayout 
                       }
  deriving (Eq)

instance Storable VkDescriptorImageInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorImageInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (sampler (poked :: VkDescriptorImageInfo))
                *> poke (ptr `plusPtr` 8) (imageView (poked :: VkDescriptorImageInfo))
                *> poke (ptr `plusPtr` 16) (imageLayout (poked :: VkDescriptorImageInfo))



data VkCopyDescriptorSet =
  VkCopyDescriptorSet{ sType :: VkStructureType 
                     , pNext :: Ptr Void 
                     , srcSet :: DescriptorSet 
                     , srcBinding :: Word32 
                     , srcArrayElement :: Word32 
                     , dstSet :: DescriptorSet 
                     , dstBinding :: Word32 
                     , dstArrayElement :: Word32 
                     , descriptorCount :: Word32 
                     }
  deriving (Eq)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 16) (srcSet (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 24) (srcBinding (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 28) (srcArrayElement (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 32) (dstSet (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 40) (dstBinding (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 44) (dstArrayElement (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 48) (descriptorCount (poked :: VkCopyDescriptorSet))


-- ** vkDestroyDescriptorPool
foreign import ccall "vkDestroyDescriptorPool" vkDestroyDescriptorPool ::
  Device -> DescriptorPool -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateDescriptorSetLayout
foreign import ccall "vkCreateDescriptorSetLayout" vkCreateDescriptorSetLayout ::
  Device ->
  Ptr VkDescriptorSetLayoutCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr DescriptorSetLayout -> IO VkResult

newtype DescriptorPool = DescriptorPool Word64
  deriving (Eq, Storable)

-- ** vkResetDescriptorPool
foreign import ccall "vkResetDescriptorPool" vkResetDescriptorPool ::
  Device ->
  DescriptorPool -> VkDescriptorPoolResetFlags -> IO VkResult

newtype DescriptorSetLayout = DescriptorSetLayout Word64
  deriving (Eq, Storable)

-- ** vkFreeDescriptorSets
foreign import ccall "vkFreeDescriptorSets" vkFreeDescriptorSets ::
  Device ->
  DescriptorPool -> Word32 -> Ptr DescriptorSet -> IO VkResult


data VkDescriptorPoolCreateInfo =
  VkDescriptorPoolCreateInfo{ sType :: VkStructureType 
                            , pNext :: Ptr Void 
                            , flags :: VkDescriptorPoolCreateFlags 
                            , maxSets :: Word32 
                            , poolSizeCount :: Word32 
                            , pPoolSizes :: Ptr VkDescriptorPoolSize 
                            }
  deriving (Eq)

instance Storable VkDescriptorPoolCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDescriptorPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (maxSets (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (poolSizeCount (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 32) (pPoolSizes (poked :: VkDescriptorPoolCreateInfo))


-- ** VkDescriptorSetLayoutCreateFlags
-- | Opaque flag
newtype VkDescriptorSetLayoutCreateFlags = VkDescriptorSetLayoutCreateFlags VkFlags
  deriving (Eq, Storable)


data VkDescriptorSetLayoutCreateInfo =
  VkDescriptorSetLayoutCreateInfo{ sType :: VkStructureType 
                                 , pNext :: Ptr Void 
                                 , flags :: VkDescriptorSetLayoutCreateFlags 
                                 , bindingCount :: Word32 
                                 , pBindings :: Ptr VkDescriptorSetLayoutBinding 
                                 }
  deriving (Eq)

instance Storable VkDescriptorSetLayoutCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (bindingCount (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (pBindings (poked :: VkDescriptorSetLayoutCreateInfo))


-- ** VkDescriptorPoolCreateFlags

newtype VkDescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- | Alias for VkDescriptorPoolCreateFlagBits
type VkDescriptorPoolCreateFlags = VkDescriptorPoolCreateFlagBits

instance Show VkDescriptorPoolCreateFlagBits where
  showsPrec _ VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = showString "VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT"
  
  showsPrec p (VkDescriptorPoolCreateFlagBits x) = showParen (p >= 11) (showString "VkDescriptorPoolCreateFlagBits " . showsPrec 11 x)

instance Read VkDescriptorPoolCreateFlagBits where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT", pure VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorPoolCreateFlagBits")
                        v <- step readPrec
                        pure (VkDescriptorPoolCreateFlagBits v)
                        )
                    )

-- | Descriptor sets may be freed individually
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = VkDescriptorPoolCreateFlagBits 0x1



data VkDescriptorPoolSize =
  VkDescriptorPoolSize{ _type :: VkDescriptorType 
                      , descriptorCount :: Word32 
                      }
  deriving (Eq)

instance Storable VkDescriptorPoolSize where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkDescriptorPoolSize <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (_type (poked :: VkDescriptorPoolSize))
                *> poke (ptr `plusPtr` 4) (descriptorCount (poked :: VkDescriptorPoolSize))


newtype DescriptorSet = DescriptorSet Word64
  deriving (Eq, Storable)


data VkWriteDescriptorSet =
  VkWriteDescriptorSet{ sType :: VkStructureType 
                      , pNext :: Ptr Void 
                      , dstSet :: DescriptorSet 
                      , dstBinding :: Word32 
                      , dstArrayElement :: Word32 
                      , descriptorCount :: Word32 
                      , descriptorType :: VkDescriptorType 
                      , pImageInfo :: Ptr VkDescriptorImageInfo 
                      , pBufferInfo :: Ptr VkDescriptorBufferInfo 
                      , pTexelBufferView :: Ptr BufferView 
                      }
  deriving (Eq)

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
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 16) (dstSet (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 24) (dstBinding (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 28) (dstArrayElement (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 32) (descriptorCount (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 36) (descriptorType (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 40) (pImageInfo (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 48) (pBufferInfo (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 56) (pTexelBufferView (poked :: VkWriteDescriptorSet))


-- ** vkCreateDescriptorPool
foreign import ccall "vkCreateDescriptorPool" vkCreateDescriptorPool ::
  Device ->
  Ptr VkDescriptorPoolCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr DescriptorPool -> IO VkResult

-- ** vkDestroyDescriptorSetLayout
foreign import ccall "vkDestroyDescriptorSetLayout" vkDestroyDescriptorSetLayout ::
  Device -> DescriptorSetLayout -> Ptr VkAllocationCallbacks -> IO ()


data VkDescriptorSetLayoutBinding =
  VkDescriptorSetLayoutBinding{ binding :: Word32 
                              , descriptorType :: VkDescriptorType 
                              , descriptorCount :: Word32 
                              , stageFlags :: VkShaderStageFlags 
                              , pImmutableSamplers :: Ptr Sampler 
                              }
  deriving (Eq)

instance Storable VkDescriptorSetLayoutBinding where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutBinding <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 4)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 12)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (binding (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 4) (descriptorType (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 8) (descriptorCount (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 12) (stageFlags (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 16) (pImmutableSamplers (poked :: VkDescriptorSetLayoutBinding))


-- ** VkDescriptorType

newtype VkDescriptorType = VkDescriptorType Int32
  deriving (Eq, Storable)

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
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_TYPE_SAMPLER", pure VK_DESCRIPTOR_TYPE_SAMPLER)
                             , ("VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER", pure VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
                             , ("VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE", pure VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_IMAGE", pure VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
                             , ("VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER", pure VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER", pure VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER", pure VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_BUFFER", pure VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC", pure VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC", pure VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC)
                             , ("VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT", pure VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorType")
                        v <- step readPrec
                        pure (VkDescriptorType v)
                        )
                    )


pattern VK_DESCRIPTOR_TYPE_SAMPLER = VkDescriptorType 0

pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = VkDescriptorType 1

pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = VkDescriptorType 2

pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = VkDescriptorType 3

pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = VkDescriptorType 4

pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = VkDescriptorType 5

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = VkDescriptorType 6

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = VkDescriptorType 7

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = VkDescriptorType 8

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = VkDescriptorType 9

pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = VkDescriptorType 10


data VkDescriptorSetAllocateInfo =
  VkDescriptorSetAllocateInfo{ sType :: VkStructureType 
                             , pNext :: Ptr Void 
                             , descriptorPool :: DescriptorPool 
                             , descriptorSetCount :: Word32 
                             , pSetLayouts :: Ptr DescriptorSetLayout 
                             }
  deriving (Eq)

instance Storable VkDescriptorSetAllocateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDescriptorSetAllocateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 16) (descriptorPool (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 24) (descriptorSetCount (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 32) (pSetLayouts (poked :: VkDescriptorSetAllocateInfo))


