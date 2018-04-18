{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
  ( VkDescriptorBindingFlagBitsEXT(..)
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT
  , pattern VK_ERROR_FRAGMENTATION_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  , pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
  , pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  , VkPhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , VkPhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  , VkDescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , VkDescriptorBindingFlagsEXT
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
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


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkResult(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorPoolCreateFlagBits(..)
  , VkDescriptorSetLayoutCreateFlagBits(..)
  )


-- ** VkDescriptorBindingFlagBitsEXT

-- | 
newtype VkDescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkDescriptorBindingFlagBitsEXT where
  showsPrec _ VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT"
  showsPrec _ VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT"
  showsPrec _ VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT"
  showsPrec _ VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT = showString "VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT"
  showsPrec p (VkDescriptorBindingFlagBitsEXT x) = showParen (p >= 11) (showString "VkDescriptorBindingFlagBitsEXT " . showsPrec 11 x)

instance Read VkDescriptorBindingFlagBitsEXT where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT",           pure VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT)
                             , ("VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT", pure VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT)
                             , ("VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT",             pure VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT)
                             , ("VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT",   pure VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorBindingFlagBitsEXT")
                        v <- step readPrec
                        pure (VkDescriptorBindingFlagBitsEXT v)
                        )
                    )

-- | 
pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000001

-- | 
pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000002

-- | 
pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000004

-- | 
pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000008
-- | Nothing
pattern VK_ERROR_FRAGMENTATION_EXT :: VkResult
pattern VK_ERROR_FRAGMENTATION_EXT = VkResult (-1000161000)
-- | Nothing
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT = VkStructureType 1000161000
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT = VkStructureType 1000161001
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT = VkStructureType 1000161002
-- | Nothing
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT = VkStructureType 1000161003
-- | Nothing
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT = VkStructureType 1000161004
-- | Nothing
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT :: VkDescriptorSetLayoutCreateFlagBits
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT = VkDescriptorSetLayoutCreateFlagBits 0x00000002
-- | Nothing
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT :: VkDescriptorPoolCreateFlagBits
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT = VkDescriptorPoolCreateFlagBits 0x00000002
pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2
pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME = "VK_EXT_descriptor_indexing"
-- | TODO: Struct comments
data VkPhysicalDeviceDescriptorIndexingFeaturesEXT = VkPhysicalDeviceDescriptorIndexingFeaturesEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkShaderInputAttachmentArrayDynamicIndexing :: VkBool32
  , vkShaderUniformTexelBufferArrayDynamicIndexing :: VkBool32
  , vkShaderStorageTexelBufferArrayDynamicIndexing :: VkBool32
  , vkShaderUniformBufferArrayNonUniformIndexing :: VkBool32
  , vkShaderSampledImageArrayNonUniformIndexing :: VkBool32
  , vkShaderStorageBufferArrayNonUniformIndexing :: VkBool32
  , vkShaderStorageImageArrayNonUniformIndexing :: VkBool32
  , vkShaderInputAttachmentArrayNonUniformIndexing :: VkBool32
  , vkShaderUniformTexelBufferArrayNonUniformIndexing :: VkBool32
  , vkShaderStorageTexelBufferArrayNonUniformIndexing :: VkBool32
  , vkDescriptorBindingUniformBufferUpdateAfterBind :: VkBool32
  , vkDescriptorBindingSampledImageUpdateAfterBind :: VkBool32
  , vkDescriptorBindingStorageImageUpdateAfterBind :: VkBool32
  , vkDescriptorBindingStorageBufferUpdateAfterBind :: VkBool32
  , vkDescriptorBindingUniformTexelBufferUpdateAfterBind :: VkBool32
  , vkDescriptorBindingStorageTexelBufferUpdateAfterBind :: VkBool32
  , vkDescriptorBindingUpdateUnusedWhilePending :: VkBool32
  , vkDescriptorBindingPartiallyBound :: VkBool32
  , vkDescriptorBindingVariableDescriptorCount :: VkBool32
  , vkRuntimeDescriptorArray :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDescriptorIndexingFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
                                                           <*> peek (ptr `plusPtr` 20)
                                                           <*> peek (ptr `plusPtr` 24)
                                                           <*> peek (ptr `plusPtr` 28)
                                                           <*> peek (ptr `plusPtr` 32)
                                                           <*> peek (ptr `plusPtr` 36)
                                                           <*> peek (ptr `plusPtr` 40)
                                                           <*> peek (ptr `plusPtr` 44)
                                                           <*> peek (ptr `plusPtr` 48)
                                                           <*> peek (ptr `plusPtr` 52)
                                                           <*> peek (ptr `plusPtr` 56)
                                                           <*> peek (ptr `plusPtr` 60)
                                                           <*> peek (ptr `plusPtr` 64)
                                                           <*> peek (ptr `plusPtr` 68)
                                                           <*> peek (ptr `plusPtr` 72)
                                                           <*> peek (ptr `plusPtr` 76)
                                                           <*> peek (ptr `plusPtr` 80)
                                                           <*> peek (ptr `plusPtr` 84)
                                                           <*> peek (ptr `plusPtr` 88)
                                                           <*> peek (ptr `plusPtr` 92)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkShaderInputAttachmentArrayDynamicIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkShaderUniformTexelBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 24) (vkShaderStorageTexelBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 28) (vkShaderUniformBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 32) (vkShaderSampledImageArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 36) (vkShaderStorageBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 40) (vkShaderStorageImageArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 44) (vkShaderInputAttachmentArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 48) (vkShaderUniformTexelBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 52) (vkShaderStorageTexelBufferArrayNonUniformIndexing (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 56) (vkDescriptorBindingUniformBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 60) (vkDescriptorBindingSampledImageUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 64) (vkDescriptorBindingStorageImageUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 68) (vkDescriptorBindingStorageBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 72) (vkDescriptorBindingUniformTexelBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 76) (vkDescriptorBindingStorageTexelBufferUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 80) (vkDescriptorBindingUpdateUnusedWhilePending (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 84) (vkDescriptorBindingPartiallyBound (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 88) (vkDescriptorBindingVariableDescriptorCount (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
                *> poke (ptr `plusPtr` 92) (vkRuntimeDescriptorArray (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
-- | TODO: Struct comments
data VkPhysicalDeviceDescriptorIndexingPropertiesEXT = VkPhysicalDeviceDescriptorIndexingPropertiesEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMaxUpdateAfterBindDescriptorsInAllPools :: Word32
  , vkShaderUniformBufferArrayNonUniformIndexingNative :: VkBool32
  , vkShaderSampledImageArrayNonUniformIndexingNative :: VkBool32
  , vkShaderStorageBufferArrayNonUniformIndexingNative :: VkBool32
  , vkShaderStorageImageArrayNonUniformIndexingNative :: VkBool32
  , vkShaderInputAttachmentArrayNonUniformIndexingNative :: VkBool32
  , vkRobustBufferAccessUpdateAfterBind :: VkBool32
  , vkQuadDivergentImplicitLod :: VkBool32
  , vkMaxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , vkMaxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , vkMaxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , vkMaxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , vkMaxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , vkMaxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , vkMaxPerStageUpdateAfterBindResources :: Word32
  , vkMaxDescriptorSetUpdateAfterBindSamplers :: Word32
  , vkMaxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , vkMaxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , vkMaxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , vkMaxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , vkMaxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , vkMaxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , vkMaxDescriptorSetUpdateAfterBindInputAttachments :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
  sizeOf ~_ = 112
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceDescriptorIndexingPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 20)
                                                             <*> peek (ptr `plusPtr` 24)
                                                             <*> peek (ptr `plusPtr` 28)
                                                             <*> peek (ptr `plusPtr` 32)
                                                             <*> peek (ptr `plusPtr` 36)
                                                             <*> peek (ptr `plusPtr` 40)
                                                             <*> peek (ptr `plusPtr` 44)
                                                             <*> peek (ptr `plusPtr` 48)
                                                             <*> peek (ptr `plusPtr` 52)
                                                             <*> peek (ptr `plusPtr` 56)
                                                             <*> peek (ptr `plusPtr` 60)
                                                             <*> peek (ptr `plusPtr` 64)
                                                             <*> peek (ptr `plusPtr` 68)
                                                             <*> peek (ptr `plusPtr` 72)
                                                             <*> peek (ptr `plusPtr` 76)
                                                             <*> peek (ptr `plusPtr` 80)
                                                             <*> peek (ptr `plusPtr` 84)
                                                             <*> peek (ptr `plusPtr` 88)
                                                             <*> peek (ptr `plusPtr` 92)
                                                             <*> peek (ptr `plusPtr` 96)
                                                             <*> peek (ptr `plusPtr` 100)
                                                             <*> peek (ptr `plusPtr` 104)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxUpdateAfterBindDescriptorsInAllPools (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkShaderUniformBufferArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkShaderSampledImageArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkShaderStorageBufferArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkShaderStorageImageArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkShaderInputAttachmentArrayNonUniformIndexingNative (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 40) (vkRobustBufferAccessUpdateAfterBind (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 44) (vkQuadDivergentImplicitLod (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 48) (vkMaxPerStageDescriptorUpdateAfterBindSamplers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 52) (vkMaxPerStageDescriptorUpdateAfterBindUniformBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 56) (vkMaxPerStageDescriptorUpdateAfterBindStorageBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 60) (vkMaxPerStageDescriptorUpdateAfterBindSampledImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 64) (vkMaxPerStageDescriptorUpdateAfterBindStorageImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 68) (vkMaxPerStageDescriptorUpdateAfterBindInputAttachments (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 72) (vkMaxPerStageUpdateAfterBindResources (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 76) (vkMaxDescriptorSetUpdateAfterBindSamplers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 80) (vkMaxDescriptorSetUpdateAfterBindUniformBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 84) (vkMaxDescriptorSetUpdateAfterBindUniformBuffersDynamic (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 88) (vkMaxDescriptorSetUpdateAfterBindStorageBuffers (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 92) (vkMaxDescriptorSetUpdateAfterBindStorageBuffersDynamic (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 96) (vkMaxDescriptorSetUpdateAfterBindSampledImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 100) (vkMaxDescriptorSetUpdateAfterBindStorageImages (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
                *> poke (ptr `plusPtr` 104) (vkMaxDescriptorSetUpdateAfterBindInputAttachments (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
-- | TODO: Struct comments
data VkDescriptorSetLayoutBindingFlagsCreateInfoEXT = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkBindingCount :: Word32
  , vkBindingFlags :: Ptr VkDescriptorBindingFlagsEXT
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkBindingCount (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkBindingFlags (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
-- | TODO: Struct comments
data VkDescriptorSetVariableDescriptorCountAllocateInfoEXT = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkDescriptorSetCount :: Word32
  , vkDescriptorCounts :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetVariableDescriptorCountAllocateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                                   <*> peek (ptr `plusPtr` 8)
                                                                   <*> peek (ptr `plusPtr` 16)
                                                                   <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDescriptorSetCount (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkDescriptorCounts (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
-- | TODO: Struct comments
data VkDescriptorSetVariableDescriptorCountLayoutSupportEXT = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMaxVariableDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT <$> peek (ptr `plusPtr` 0)
                                                                    <*> peek (ptr `plusPtr` 8)
                                                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxVariableDescriptorCount (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
type VkDescriptorBindingFlagsEXT = VkDescriptorBindingFlagBitsEXT
