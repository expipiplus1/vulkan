{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.DescriptorSet where

import Graphics.Vulkan.Device( VkDevice(..)
                             )
import Graphics.Vulkan.Buffer( VkBuffer(..)
                             )
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
import Graphics.Vulkan.Memory( VkInternalAllocationType(..)
                             , PFN_vkAllocationFunction
                             , PFN_vkReallocationFunction
                             , PFN_vkInternalAllocationNotification
                             , VkAllocationCallbacks(..)
                             , VkSystemAllocationScope(..)
                             , PFN_vkFreeFunction
                             , PFN_vkInternalFreeNotification
                             )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Graphics.Vulkan.Shader( VkShaderStageFlagBits(..)
                             , VkShaderStageFlags(..)
                             )
import Graphics.Vulkan.Sampler( VkSampler(..)
                              )
import Graphics.Vulkan.Image( VkImageLayout(..)
                            )
import Graphics.Vulkan.ImageView( VkImageView(..)
                                )
import Graphics.Vulkan.BufferView( VkBufferView(..)
                                 )
import Graphics.Vulkan.Core( VkResult(..)
                           , VkDeviceSize(..)
                           , VkFlags(..)
                           , VkStructureType(..)
                           )
import Foreign.C.Types( CSize(..)
                      )

-- ** vkUpdateDescriptorSets
foreign import ccall "vkUpdateDescriptorSets" vkUpdateDescriptorSets ::
  VkDevice ->
  Word32 ->
    Ptr VkWriteDescriptorSet ->
      Word32 -> Ptr VkCopyDescriptorSet -> IO ()

-- ** VkDescriptorPoolResetFlags
-- | Opaque flag
newtype VkDescriptorPoolResetFlags = VkDescriptorPoolResetFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)

-- ** vkAllocateDescriptorSets
foreign import ccall "vkAllocateDescriptorSets" vkAllocateDescriptorSets ::
  VkDevice ->
  Ptr VkDescriptorSetAllocateInfo ->
    Ptr VkDescriptorSet -> IO VkResult


data VkDescriptorBufferInfo =
  VkDescriptorBufferInfo{ vkBuffer :: VkBuffer 
                        , vkOffset :: VkDeviceSize 
                        , vkRange :: VkDeviceSize 
                        }
  deriving (Eq)

instance Storable VkDescriptorBufferInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorBufferInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBuffer (poked :: VkDescriptorBufferInfo))
                *> poke (ptr `plusPtr` 8) (vkOffset (poked :: VkDescriptorBufferInfo))
                *> poke (ptr `plusPtr` 16) (vkRange (poked :: VkDescriptorBufferInfo))



data VkDescriptorImageInfo =
  VkDescriptorImageInfo{ vkSampler :: VkSampler 
                       , vkImageView :: VkImageView 
                       , vkImageLayout :: VkImageLayout 
                       }
  deriving (Eq)

instance Storable VkDescriptorImageInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorImageInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSampler (poked :: VkDescriptorImageInfo))
                *> poke (ptr `plusPtr` 8) (vkImageView (poked :: VkDescriptorImageInfo))
                *> poke (ptr `plusPtr` 16) (vkImageLayout (poked :: VkDescriptorImageInfo))



data VkCopyDescriptorSet =
  VkCopyDescriptorSet{ vkSType :: VkStructureType 
                     , vkPNext :: Ptr Void 
                     , vkSrcSet :: VkDescriptorSet 
                     , vkSrcBinding :: Word32 
                     , vkSrcArrayElement :: Word32 
                     , vkDstSet :: VkDescriptorSet 
                     , vkDstBinding :: Word32 
                     , vkDstArrayElement :: Word32 
                     , vkDescriptorCount :: Word32 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 16) (vkSrcSet (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 24) (vkSrcBinding (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 28) (vkSrcArrayElement (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 32) (vkDstSet (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 40) (vkDstBinding (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 44) (vkDstArrayElement (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 48) (vkDescriptorCount (poked :: VkCopyDescriptorSet))


-- ** vkDestroyDescriptorPool
foreign import ccall "vkDestroyDescriptorPool" vkDestroyDescriptorPool ::
  VkDevice -> VkDescriptorPool -> Ptr VkAllocationCallbacks -> IO ()

-- ** vkCreateDescriptorSetLayout
foreign import ccall "vkCreateDescriptorSetLayout" vkCreateDescriptorSetLayout ::
  VkDevice ->
  Ptr VkDescriptorSetLayoutCreateInfo ->
    Ptr VkAllocationCallbacks ->
      Ptr VkDescriptorSetLayout -> IO VkResult

newtype VkDescriptorPool = VkDescriptorPool Word64
  deriving (Eq, Storable)

-- ** vkResetDescriptorPool
foreign import ccall "vkResetDescriptorPool" vkResetDescriptorPool ::
  VkDevice ->
  VkDescriptorPool -> VkDescriptorPoolResetFlags -> IO VkResult

newtype VkDescriptorSetLayout = VkDescriptorSetLayout Word64
  deriving (Eq, Storable)

-- ** vkFreeDescriptorSets
foreign import ccall "vkFreeDescriptorSets" vkFreeDescriptorSets ::
  VkDevice ->
  VkDescriptorPool -> Word32 -> Ptr VkDescriptorSet -> IO VkResult


data VkDescriptorPoolCreateInfo =
  VkDescriptorPoolCreateInfo{ vkSType :: VkStructureType 
                            , vkPNext :: Ptr Void 
                            , vkFlags :: VkDescriptorPoolCreateFlags 
                            , vkMaxSets :: Word32 
                            , vkPoolSizeCount :: Word32 
                            , vkPPoolSizes :: Ptr VkDescriptorPoolSize 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkMaxSets (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPoolSizeCount (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPPoolSizes (poked :: VkDescriptorPoolCreateInfo))


-- ** VkDescriptorSetLayoutCreateFlags
-- | Opaque flag
newtype VkDescriptorSetLayoutCreateFlags = VkDescriptorSetLayoutCreateFlags VkFlags
  deriving (Eq, Storable, Bits, FiniteBits)


data VkDescriptorSetLayoutCreateInfo =
  VkDescriptorSetLayoutCreateInfo{ vkSType :: VkStructureType 
                                 , vkPNext :: Ptr Void 
                                 , vkFlags :: VkDescriptorSetLayoutCreateFlags 
                                 , vkBindingCount :: Word32 
                                 , vkPBindings :: Ptr VkDescriptorSetLayoutBinding 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkBindingCount (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPBindings (poked :: VkDescriptorSetLayoutCreateInfo))


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
  VkDescriptorPoolSize{ vkType :: VkDescriptorType 
                      , vkDescriptorCount :: Word32 
                      }
  deriving (Eq)

instance Storable VkDescriptorPoolSize where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkDescriptorPoolSize <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkDescriptorPoolSize))
                *> poke (ptr `plusPtr` 4) (vkDescriptorCount (poked :: VkDescriptorPoolSize))


newtype VkDescriptorSet = VkDescriptorSet Word64
  deriving (Eq, Storable)


data VkWriteDescriptorSet =
  VkWriteDescriptorSet{ vkSType :: VkStructureType 
                      , vkPNext :: Ptr Void 
                      , vkDstSet :: VkDescriptorSet 
                      , vkDstBinding :: Word32 
                      , vkDstArrayElement :: Word32 
                      , vkDescriptorCount :: Word32 
                      , vkDescriptorType :: VkDescriptorType 
                      , vkPImageInfo :: Ptr VkDescriptorImageInfo 
                      , vkPBufferInfo :: Ptr VkDescriptorBufferInfo 
                      , vkPTexelBufferView :: Ptr VkBufferView 
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


-- ** vkCreateDescriptorPool
foreign import ccall "vkCreateDescriptorPool" vkCreateDescriptorPool ::
  VkDevice ->
  Ptr VkDescriptorPoolCreateInfo ->
    Ptr VkAllocationCallbacks -> Ptr VkDescriptorPool -> IO VkResult

-- ** vkDestroyDescriptorSetLayout
foreign import ccall "vkDestroyDescriptorSetLayout" vkDestroyDescriptorSetLayout ::
  VkDevice ->
  VkDescriptorSetLayout -> Ptr VkAllocationCallbacks -> IO ()


data VkDescriptorSetLayoutBinding =
  VkDescriptorSetLayoutBinding{ vkBinding :: Word32 
                              , vkDescriptorType :: VkDescriptorType 
                              , vkDescriptorCount :: Word32 
                              , vkStageFlags :: VkShaderStageFlags 
                              , vkPImmutableSamplers :: Ptr VkSampler 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBinding (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 4) (vkDescriptorType (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 8) (vkDescriptorCount (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 12) (vkStageFlags (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 16) (vkPImmutableSamplers (poked :: VkDescriptorSetLayoutBinding))


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
  VkDescriptorSetAllocateInfo{ vkSType :: VkStructureType 
                             , vkPNext :: Ptr Void 
                             , vkDescriptorPool :: VkDescriptorPool 
                             , vkDescriptorSetCount :: Word32 
                             , vkPSetLayouts :: Ptr VkDescriptorSetLayout 
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
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkDescriptorPool (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkDescriptorSetCount (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 32) (vkPSetLayouts (poked :: VkDescriptorSetAllocateInfo))


