{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( VkDescriptorBindingFlagBitsEXT(..)
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT
  , pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT
  , VkDescriptorBindingFlagsEXT
  , VkDescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , VkPhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , VkPhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  , pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  , pattern VK_ERROR_FRAGMENTATION_EXT
  , pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME
  , pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorPoolCreateFlagBits(..)
  , VkDescriptorSetLayoutCreateFlagBits(..)
  )


-- ** VkDescriptorBindingFlagBitsEXT

-- No documentation found for TopLevel "VkDescriptorBindingFlagBitsEXT"
newtype VkDescriptorBindingFlagBitsEXT = VkDescriptorBindingFlagBitsEXT VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- No documentation found for Nested "VkDescriptorBindingFlagBitsEXT" "VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT"
pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000001

-- No documentation found for Nested "VkDescriptorBindingFlagBitsEXT" "VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT"
pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000002

-- No documentation found for Nested "VkDescriptorBindingFlagBitsEXT" "VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT"
pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000004

-- No documentation found for Nested "VkDescriptorBindingFlagBitsEXT" "VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT"
pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT :: VkDescriptorBindingFlagBitsEXT
pattern VK_DESCRIPTOR_BINDING_VARIABLE_DESCRIPTOR_COUNT_BIT_EXT = VkDescriptorBindingFlagBitsEXT 0x00000008
-- No documentation found for TopLevel "VkDescriptorBindingFlagsEXT"
type VkDescriptorBindingFlagsEXT = VkDescriptorBindingFlagBitsEXT
-- No documentation found for TopLevel "VkDescriptorSetLayoutBindingFlagsCreateInfoEXT"
data VkDescriptorSetLayoutBindingFlagsCreateInfoEXT = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT
  { -- No documentation found for Nested "VkDescriptorSetLayoutBindingFlagsCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDescriptorSetLayoutBindingFlagsCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDescriptorSetLayoutBindingFlagsCreateInfoEXT" "bindingCount"
  vkBindingCount :: Word32
  , -- No documentation found for Nested "VkDescriptorSetLayoutBindingFlagsCreateInfoEXT" "pBindingFlags"
  vkPBindingFlags :: Ptr VkDescriptorBindingFlagsEXT
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkBindingCount (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPBindingFlags (poked :: VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))

instance Zero VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
  zero = VkDescriptorSetLayoutBindingFlagsCreateInfoEXT zero
                                                        zero
                                                        zero
                                                        zero
-- No documentation found for TopLevel "VkDescriptorSetVariableDescriptorCountAllocateInfoEXT"
data VkDescriptorSetVariableDescriptorCountAllocateInfoEXT = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT
  { -- No documentation found for Nested "VkDescriptorSetVariableDescriptorCountAllocateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDescriptorSetVariableDescriptorCountAllocateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDescriptorSetVariableDescriptorCountAllocateInfoEXT" "descriptorSetCount"
  vkDescriptorSetCount :: Word32
  , -- No documentation found for Nested "VkDescriptorSetVariableDescriptorCountAllocateInfoEXT" "pDescriptorCounts"
  vkPDescriptorCounts :: Ptr Word32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkDescriptorSetCount (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPDescriptorCounts (poked :: VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))

instance Zero VkDescriptorSetVariableDescriptorCountAllocateInfoEXT where
  zero = VkDescriptorSetVariableDescriptorCountAllocateInfoEXT zero
                                                               zero
                                                               zero
                                                               zero
-- No documentation found for TopLevel "VkDescriptorSetVariableDescriptorCountLayoutSupportEXT"
data VkDescriptorSetVariableDescriptorCountLayoutSupportEXT = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT
  { -- No documentation found for Nested "VkDescriptorSetVariableDescriptorCountLayoutSupportEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDescriptorSetVariableDescriptorCountLayoutSupportEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDescriptorSetVariableDescriptorCountLayoutSupportEXT" "maxVariableDescriptorCount"
  vkMaxVariableDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT <$> peek (ptr `plusPtr` 0)
                                                                    <*> peek (ptr `plusPtr` 8)
                                                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxVariableDescriptorCount (poked :: VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))

instance Zero VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  zero = VkDescriptorSetVariableDescriptorCountLayoutSupportEXT zero
                                                                zero
                                                                zero
-- No documentation found for TopLevel "VkPhysicalDeviceDescriptorIndexingFeaturesEXT"
data VkPhysicalDeviceDescriptorIndexingFeaturesEXT = VkPhysicalDeviceDescriptorIndexingFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderInputAttachmentArrayDynamicIndexing"
  vkShaderInputAttachmentArrayDynamicIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformTexelBufferArrayDynamicIndexing"
  vkShaderUniformTexelBufferArrayDynamicIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageTexelBufferArrayDynamicIndexing"
  vkShaderStorageTexelBufferArrayDynamicIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformBufferArrayNonUniformIndexing"
  vkShaderUniformBufferArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderSampledImageArrayNonUniformIndexing"
  vkShaderSampledImageArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageBufferArrayNonUniformIndexing"
  vkShaderStorageBufferArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageImageArrayNonUniformIndexing"
  vkShaderStorageImageArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderInputAttachmentArrayNonUniformIndexing"
  vkShaderInputAttachmentArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderUniformTexelBufferArrayNonUniformIndexing"
  vkShaderUniformTexelBufferArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "shaderStorageTexelBufferArrayNonUniformIndexing"
  vkShaderStorageTexelBufferArrayNonUniformIndexing :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUniformBufferUpdateAfterBind"
  vkDescriptorBindingUniformBufferUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingSampledImageUpdateAfterBind"
  vkDescriptorBindingSampledImageUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageImageUpdateAfterBind"
  vkDescriptorBindingStorageImageUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageBufferUpdateAfterBind"
  vkDescriptorBindingStorageBufferUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUniformTexelBufferUpdateAfterBind"
  vkDescriptorBindingUniformTexelBufferUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingStorageTexelBufferUpdateAfterBind"
  vkDescriptorBindingStorageTexelBufferUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingUpdateUnusedWhilePending"
  vkDescriptorBindingUpdateUnusedWhilePending :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingPartiallyBound"
  vkDescriptorBindingPartiallyBound :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "descriptorBindingVariableDescriptorCount"
  vkDescriptorBindingVariableDescriptorCount :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingFeaturesEXT" "runtimeDescriptorArray"
  vkRuntimeDescriptorArray :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
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

instance Zero VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
  zero = VkPhysicalDeviceDescriptorIndexingFeaturesEXT zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero
-- No documentation found for TopLevel "VkPhysicalDeviceDescriptorIndexingPropertiesEXT"
data VkPhysicalDeviceDescriptorIndexingPropertiesEXT = VkPhysicalDeviceDescriptorIndexingPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxUpdateAfterBindDescriptorsInAllPools"
  vkMaxUpdateAfterBindDescriptorsInAllPools :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderUniformBufferArrayNonUniformIndexingNative"
  vkShaderUniformBufferArrayNonUniformIndexingNative :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderSampledImageArrayNonUniformIndexingNative"
  vkShaderSampledImageArrayNonUniformIndexingNative :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderStorageBufferArrayNonUniformIndexingNative"
  vkShaderStorageBufferArrayNonUniformIndexingNative :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderStorageImageArrayNonUniformIndexingNative"
  vkShaderStorageImageArrayNonUniformIndexingNative :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "shaderInputAttachmentArrayNonUniformIndexingNative"
  vkShaderInputAttachmentArrayNonUniformIndexingNative :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "robustBufferAccessUpdateAfterBind"
  vkRobustBufferAccessUpdateAfterBind :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "quadDivergentImplicitLod"
  vkQuadDivergentImplicitLod :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindSamplers"
  vkMaxPerStageDescriptorUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindUniformBuffers"
  vkMaxPerStageDescriptorUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindStorageBuffers"
  vkMaxPerStageDescriptorUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindSampledImages"
  vkMaxPerStageDescriptorUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindStorageImages"
  vkMaxPerStageDescriptorUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindInputAttachments"
  vkMaxPerStageDescriptorUpdateAfterBindInputAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxPerStageUpdateAfterBindResources"
  vkMaxPerStageUpdateAfterBindResources :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindSamplers"
  vkMaxDescriptorSetUpdateAfterBindSamplers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindUniformBuffers"
  vkMaxDescriptorSetUpdateAfterBindUniformBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindUniformBuffersDynamic"
  vkMaxDescriptorSetUpdateAfterBindUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageBuffers"
  vkMaxDescriptorSetUpdateAfterBindStorageBuffers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageBuffersDynamic"
  vkMaxDescriptorSetUpdateAfterBindStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindSampledImages"
  vkMaxDescriptorSetUpdateAfterBindSampledImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindStorageImages"
  vkMaxDescriptorSetUpdateAfterBindStorageImages :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceDescriptorIndexingPropertiesEXT" "maxDescriptorSetUpdateAfterBindInputAttachments"
  vkMaxDescriptorSetUpdateAfterBindInputAttachments :: Word32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
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

instance Zero VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
  zero = VkPhysicalDeviceDescriptorIndexingPropertiesEXT zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
-- No documentation found for Nested "VkDescriptorPoolCreateFlagBits" "VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT"
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT :: VkDescriptorPoolCreateFlagBits
pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT = VkDescriptorPoolCreateFlagBits 0x00000002
-- No documentation found for Nested "VkDescriptorSetLayoutCreateFlagBits" "VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT"
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT :: VkDescriptorSetLayoutCreateFlagBits
pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT = VkDescriptorSetLayoutCreateFlagBits 0x00000002
-- No documentation found for Nested "VkResult" "VK_ERROR_FRAGMENTATION_EXT"
pattern VK_ERROR_FRAGMENTATION_EXT :: VkResult
pattern VK_ERROR_FRAGMENTATION_EXT = VkResult (-1000161000)
-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME"
pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DESCRIPTOR_INDEXING_EXTENSION_NAME = "VK_EXT_descriptor_indexing"
-- No documentation found for TopLevel "VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION"
pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DESCRIPTOR_INDEXING_SPEC_VERSION = 2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT = VkStructureType 1000161000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT = VkStructureType 1000161003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT = VkStructureType 1000161004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT = VkStructureType 1000161001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT = VkStructureType 1000161002
