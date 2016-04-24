{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.DescriptorSet where

import Graphics.Vulkan.Device( Device(..)
                             )
import Graphics.Vulkan.Buffer( Buffer(..)
                             )
import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( Ptr(..)
                  , plusPtr
                  )
import Data.Int( Int32
               )
import Data.Bits( Bits
                , FiniteBits
                )
import Foreign.Storable( Storable(..)
                       )
import Data.Void( Void(..)
                )
import Graphics.Vulkan.Memory( AllocationCallbacks(..)
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Shader( ShaderStageFlags(..)
                             )
import Graphics.Vulkan.Sampler( Sampler(..)
                              )
import Graphics.Vulkan.Image( ImageLayout(..)
                            )
import Graphics.Vulkan.ImageView( ImageView(..)
                                )
import Graphics.Vulkan.BufferView( BufferView(..)
                                 )
import Graphics.Vulkan.Core( StructureType(..)
                           , Result(..)
                           , DeviceSize(..)
                           , Flags(..)
                           )

-- ** updateDescriptorSets
foreign import ccall "vkUpdateDescriptorSets" updateDescriptorSets ::
  Device ->
  Word32 ->
    Ptr WriteDescriptorSet -> Word32 -> Ptr CopyDescriptorSet -> IO ()

-- ** DescriptorPoolResetFlags
-- | Opaque flag
newtype DescriptorPoolResetFlags = DescriptorPoolResetFlags Flags
  deriving (Eq, Storable)

-- ** allocateDescriptorSets
foreign import ccall "vkAllocateDescriptorSets" allocateDescriptorSets ::
  Device ->
  Ptr DescriptorSetAllocateInfo -> Ptr DescriptorSet -> IO Result


data DescriptorBufferInfo =
  DescriptorBufferInfo{ buffer :: Buffer 
                      , offset :: DeviceSize 
                      , range :: DeviceSize 
                      }
  deriving (Eq)

instance Storable DescriptorBufferInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = DescriptorBufferInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (buffer (poked :: DescriptorBufferInfo))
                *> poke (ptr `plusPtr` 8) (offset (poked :: DescriptorBufferInfo))
                *> poke (ptr `plusPtr` 16) (range (poked :: DescriptorBufferInfo))



data DescriptorImageInfo =
  DescriptorImageInfo{ sampler :: Sampler 
                     , imageView :: ImageView 
                     , imageLayout :: ImageLayout 
                     }
  deriving (Eq)

instance Storable DescriptorImageInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = DescriptorImageInfo <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (sampler (poked :: DescriptorImageInfo))
                *> poke (ptr `plusPtr` 8) (imageView (poked :: DescriptorImageInfo))
                *> poke (ptr `plusPtr` 16) (imageLayout (poked :: DescriptorImageInfo))



data CopyDescriptorSet =
  CopyDescriptorSet{ sType :: StructureType 
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

instance Storable CopyDescriptorSet where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = CopyDescriptorSet <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 24)
                               <*> peek (ptr `plusPtr` 28)
                               <*> peek (ptr `plusPtr` 32)
                               <*> peek (ptr `plusPtr` 40)
                               <*> peek (ptr `plusPtr` 44)
                               <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: CopyDescriptorSet))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: CopyDescriptorSet))
                *> poke (ptr `plusPtr` 16) (srcSet (poked :: CopyDescriptorSet))
                *> poke (ptr `plusPtr` 24) (srcBinding (poked :: CopyDescriptorSet))
                *> poke (ptr `plusPtr` 28) (srcArrayElement (poked :: CopyDescriptorSet))
                *> poke (ptr `plusPtr` 32) (dstSet (poked :: CopyDescriptorSet))
                *> poke (ptr `plusPtr` 40) (dstBinding (poked :: CopyDescriptorSet))
                *> poke (ptr `plusPtr` 44) (dstArrayElement (poked :: CopyDescriptorSet))
                *> poke (ptr `plusPtr` 48) (descriptorCount (poked :: CopyDescriptorSet))


-- ** destroyDescriptorPool
foreign import ccall "vkDestroyDescriptorPool" destroyDescriptorPool ::
  Device -> DescriptorPool -> Ptr AllocationCallbacks -> IO ()

-- ** createDescriptorSetLayout
foreign import ccall "vkCreateDescriptorSetLayout" createDescriptorSetLayout ::
  Device ->
  Ptr DescriptorSetLayoutCreateInfo ->
    Ptr AllocationCallbacks -> Ptr DescriptorSetLayout -> IO Result

newtype DescriptorPool = DescriptorPool Word64
  deriving (Eq, Storable)

-- ** resetDescriptorPool
foreign import ccall "vkResetDescriptorPool" resetDescriptorPool ::
  Device -> DescriptorPool -> DescriptorPoolResetFlags -> IO Result

newtype DescriptorSetLayout = DescriptorSetLayout Word64
  deriving (Eq, Storable)

-- ** freeDescriptorSets
foreign import ccall "vkFreeDescriptorSets" freeDescriptorSets ::
  Device ->
  DescriptorPool -> Word32 -> Ptr DescriptorSet -> IO Result


data DescriptorPoolCreateInfo =
  DescriptorPoolCreateInfo{ sType :: StructureType 
                          , pNext :: Ptr Void 
                          , flags :: DescriptorPoolCreateFlags 
                          , maxSets :: Word32 
                          , poolSizeCount :: Word32 
                          , pPoolSizes :: Ptr DescriptorPoolSize 
                          }
  deriving (Eq)

instance Storable DescriptorPoolCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = DescriptorPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 20)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: DescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (maxSets (poked :: DescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (poolSizeCount (poked :: DescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 32) (pPoolSizes (poked :: DescriptorPoolCreateInfo))


-- ** DescriptorSetLayoutCreateFlags
-- | Opaque flag
newtype DescriptorSetLayoutCreateFlags = DescriptorSetLayoutCreateFlags Flags
  deriving (Eq, Storable)


data DescriptorSetLayoutCreateInfo =
  DescriptorSetLayoutCreateInfo{ sType :: StructureType 
                               , pNext :: Ptr Void 
                               , flags :: DescriptorSetLayoutCreateFlags 
                               , bindingCount :: Word32 
                               , pBindings :: Ptr DescriptorSetLayoutBinding 
                               }
  deriving (Eq)

instance Storable DescriptorSetLayoutCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = DescriptorSetLayoutCreateInfo <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 20)
                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (flags (poked :: DescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (bindingCount (poked :: DescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (pBindings (poked :: DescriptorSetLayoutCreateInfo))


-- ** VkDescriptorPoolCreateFlags

newtype DescriptorPoolCreateFlags = DescriptorPoolCreateFlags Flags
  deriving (Eq, Storable, Bits, FiniteBits)

instance Show DescriptorPoolCreateFlags where
  showsPrec _ VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = showString "VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT"
  
  showsPrec p (DescriptorPoolCreateFlags x) = showParen (p >= 11) (showString "DescriptorPoolCreateFlags " . showsPrec 11 x)

instance Read DescriptorPoolCreateFlags where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT", pure VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "DescriptorPoolCreateFlags")
                        v <- step readPrec
                        pure (DescriptorPoolCreateFlags v)
                        )
                    )

-- | Descriptor sets may be freed individually
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = DescriptorPoolCreateFlags 0x1



data DescriptorPoolSize =
  DescriptorPoolSize{ _type :: DescriptorType 
                    , descriptorCount :: Word32 
                    }
  deriving (Eq)

instance Storable DescriptorPoolSize where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = DescriptorPoolSize <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (_type (poked :: DescriptorPoolSize))
                *> poke (ptr `plusPtr` 4) (descriptorCount (poked :: DescriptorPoolSize))


newtype DescriptorSet = DescriptorSet Word64
  deriving (Eq, Storable)


data WriteDescriptorSet =
  WriteDescriptorSet{ sType :: StructureType 
                    , pNext :: Ptr Void 
                    , dstSet :: DescriptorSet 
                    , dstBinding :: Word32 
                    , dstArrayElement :: Word32 
                    , descriptorCount :: Word32 
                    , descriptorType :: DescriptorType 
                    , pImageInfo :: Ptr DescriptorImageInfo 
                    , pBufferInfo :: Ptr DescriptorBufferInfo 
                    , pTexelBufferView :: Ptr BufferView 
                    }
  deriving (Eq)

instance Storable WriteDescriptorSet where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = WriteDescriptorSet <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 24)
                                <*> peek (ptr `plusPtr` 28)
                                <*> peek (ptr `plusPtr` 32)
                                <*> peek (ptr `plusPtr` 36)
                                <*> peek (ptr `plusPtr` 40)
                                <*> peek (ptr `plusPtr` 48)
                                <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: WriteDescriptorSet))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: WriteDescriptorSet))
                *> poke (ptr `plusPtr` 16) (dstSet (poked :: WriteDescriptorSet))
                *> poke (ptr `plusPtr` 24) (dstBinding (poked :: WriteDescriptorSet))
                *> poke (ptr `plusPtr` 28) (dstArrayElement (poked :: WriteDescriptorSet))
                *> poke (ptr `plusPtr` 32) (descriptorCount (poked :: WriteDescriptorSet))
                *> poke (ptr `plusPtr` 36) (descriptorType (poked :: WriteDescriptorSet))
                *> poke (ptr `plusPtr` 40) (pImageInfo (poked :: WriteDescriptorSet))
                *> poke (ptr `plusPtr` 48) (pBufferInfo (poked :: WriteDescriptorSet))
                *> poke (ptr `plusPtr` 56) (pTexelBufferView (poked :: WriteDescriptorSet))


-- ** createDescriptorPool
foreign import ccall "vkCreateDescriptorPool" createDescriptorPool ::
  Device ->
  Ptr DescriptorPoolCreateInfo ->
    Ptr AllocationCallbacks -> Ptr DescriptorPool -> IO Result

-- ** destroyDescriptorSetLayout
foreign import ccall "vkDestroyDescriptorSetLayout" destroyDescriptorSetLayout ::
  Device -> DescriptorSetLayout -> Ptr AllocationCallbacks -> IO ()


data DescriptorSetLayoutBinding =
  DescriptorSetLayoutBinding{ binding :: Word32 
                            , descriptorType :: DescriptorType 
                            , descriptorCount :: Word32 
                            , stageFlags :: ShaderStageFlags 
                            , pImmutableSamplers :: Ptr Sampler 
                            }
  deriving (Eq)

instance Storable DescriptorSetLayoutBinding where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = DescriptorSetLayoutBinding <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 4)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 12)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (binding (poked :: DescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 4) (descriptorType (poked :: DescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 8) (descriptorCount (poked :: DescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 12) (stageFlags (poked :: DescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 16) (pImmutableSamplers (poked :: DescriptorSetLayoutBinding))


-- ** DescriptorType

newtype DescriptorType = DescriptorType Int32
  deriving (Eq, Storable)

instance Show DescriptorType where
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
  showsPrec p (DescriptorType x) = showParen (p >= 11) (showString "DescriptorType " . showsPrec 11 x)

instance Read DescriptorType where
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
                        expectP (Ident "DescriptorType")
                        v <- step readPrec
                        pure (DescriptorType v)
                        )
                    )


pattern VK_DESCRIPTOR_TYPE_SAMPLER = DescriptorType 0

pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = DescriptorType 1

pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = DescriptorType 2

pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = DescriptorType 3

pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = DescriptorType 4

pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = DescriptorType 5

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = DescriptorType 6

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = DescriptorType 7

pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = DescriptorType 8

pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = DescriptorType 9

pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = DescriptorType 10


data DescriptorSetAllocateInfo =
  DescriptorSetAllocateInfo{ sType :: StructureType 
                           , pNext :: Ptr Void 
                           , descriptorPool :: DescriptorPool 
                           , descriptorSetCount :: Word32 
                           , pSetLayouts :: Ptr DescriptorSetLayout 
                           }
  deriving (Eq)

instance Storable DescriptorSetAllocateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = DescriptorSetAllocateInfo <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 8)
                                       <*> peek (ptr `plusPtr` 16)
                                       <*> peek (ptr `plusPtr` 24)
                                       <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (sType (poked :: DescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 8) (pNext (poked :: DescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 16) (descriptorPool (poked :: DescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 24) (descriptorSetCount (poked :: DescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 32) (pSetLayouts (poked :: DescriptorSetAllocateInfo))


