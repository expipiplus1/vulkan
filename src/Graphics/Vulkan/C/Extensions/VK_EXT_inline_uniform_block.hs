{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block
  ( VkDescriptorPoolInlineUniformBlockCreateInfoEXT(..)
  , VkPhysicalDeviceInlineUniformBlockFeaturesEXT(..)
  , VkPhysicalDeviceInlineUniformBlockPropertiesEXT(..)
  , VkWriteDescriptorSetInlineUniformBlockEXT(..)
  , pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  , pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
  , pattern VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
  ) where

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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkDescriptorType(..)
  )


-- No documentation found for TopLevel "VkDescriptorPoolInlineUniformBlockCreateInfoEXT"
data VkDescriptorPoolInlineUniformBlockCreateInfoEXT = VkDescriptorPoolInlineUniformBlockCreateInfoEXT
  { -- No documentation found for Nested "VkDescriptorPoolInlineUniformBlockCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDescriptorPoolInlineUniformBlockCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDescriptorPoolInlineUniformBlockCreateInfoEXT" "maxInlineUniformBlockBindings"
  vkMaxInlineUniformBlockBindings :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorPoolInlineUniformBlockCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorPoolInlineUniformBlockCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorPoolInlineUniformBlockCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorPoolInlineUniformBlockCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxInlineUniformBlockBindings (poked :: VkDescriptorPoolInlineUniformBlockCreateInfoEXT))

instance Zero VkDescriptorPoolInlineUniformBlockCreateInfoEXT where
  zero = VkDescriptorPoolInlineUniformBlockCreateInfoEXT VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
                                                         zero
                                                         zero

-- No documentation found for TopLevel "VkPhysicalDeviceInlineUniformBlockFeaturesEXT"
data VkPhysicalDeviceInlineUniformBlockFeaturesEXT = VkPhysicalDeviceInlineUniformBlockFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockFeaturesEXT" "inlineUniformBlock"
  vkInlineUniformBlock :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockFeaturesEXT" "descriptorBindingInlineUniformBlockUpdateAfterBind"
  vkDescriptorBindingInlineUniformBlockUpdateAfterBind :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceInlineUniformBlockFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceInlineUniformBlockFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                           <*> peek (ptr `plusPtr` 8)
                                                           <*> peek (ptr `plusPtr` 16)
                                                           <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkInlineUniformBlock (poked :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkDescriptorBindingInlineUniformBlockUpdateAfterBind (poked :: VkPhysicalDeviceInlineUniformBlockFeaturesEXT))

instance Zero VkPhysicalDeviceInlineUniformBlockFeaturesEXT where
  zero = VkPhysicalDeviceInlineUniformBlockFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
                                                       zero
                                                       zero
                                                       zero

-- No documentation found for TopLevel "VkPhysicalDeviceInlineUniformBlockPropertiesEXT"
data VkPhysicalDeviceInlineUniformBlockPropertiesEXT = VkPhysicalDeviceInlineUniformBlockPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" "maxInlineUniformBlockSize"
  vkMaxInlineUniformBlockSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorInlineUniformBlocks"
  vkMaxPerStageDescriptorInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks"
  vkMaxPerStageDescriptorUpdateAfterBindInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" "maxDescriptorSetInlineUniformBlocks"
  vkMaxDescriptorSetInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" "maxDescriptorSetUpdateAfterBindInlineUniformBlocks"
  vkMaxDescriptorSetUpdateAfterBindInlineUniformBlocks :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceInlineUniformBlockPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceInlineUniformBlockPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                             <*> peek (ptr `plusPtr` 8)
                                                             <*> peek (ptr `plusPtr` 16)
                                                             <*> peek (ptr `plusPtr` 20)
                                                             <*> peek (ptr `plusPtr` 24)
                                                             <*> peek (ptr `plusPtr` 28)
                                                             <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxInlineUniformBlockSize (poked :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkMaxPerStageDescriptorInlineUniformBlocks (poked :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkMaxPerStageDescriptorUpdateAfterBindInlineUniformBlocks (poked :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkMaxDescriptorSetInlineUniformBlocks (poked :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkMaxDescriptorSetUpdateAfterBindInlineUniformBlocks (poked :: VkPhysicalDeviceInlineUniformBlockPropertiesEXT))

instance Zero VkPhysicalDeviceInlineUniformBlockPropertiesEXT where
  zero = VkPhysicalDeviceInlineUniformBlockPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero

-- No documentation found for TopLevel "VkWriteDescriptorSetInlineUniformBlockEXT"
data VkWriteDescriptorSetInlineUniformBlockEXT = VkWriteDescriptorSetInlineUniformBlockEXT
  { -- No documentation found for Nested "VkWriteDescriptorSetInlineUniformBlockEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkWriteDescriptorSetInlineUniformBlockEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkWriteDescriptorSetInlineUniformBlockEXT" "dataSize"
  vkDataSize :: Word32
  , -- No documentation found for Nested "VkWriteDescriptorSetInlineUniformBlockEXT" "pData"
  vkPData :: Ptr ()
  }
  deriving (Eq, Show)

instance Storable VkWriteDescriptorSetInlineUniformBlockEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkWriteDescriptorSetInlineUniformBlockEXT <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
                                                       <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWriteDescriptorSetInlineUniformBlockEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWriteDescriptorSetInlineUniformBlockEXT))
                *> poke (ptr `plusPtr` 16) (vkDataSize (poked :: VkWriteDescriptorSetInlineUniformBlockEXT))
                *> poke (ptr `plusPtr` 24) (vkPData (poked :: VkWriteDescriptorSetInlineUniformBlockEXT))

instance Zero VkWriteDescriptorSetInlineUniformBlockEXT where
  zero = VkWriteDescriptorSetInlineUniformBlockEXT VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
                                                   zero
                                                   zero
                                                   zero

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT"
pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT = VkDescriptorType 1000138000

-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME"
pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME = "VK_EXT_inline_uniform_block"

-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION"
pattern VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION :: Integral a => a
pattern VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT = VkStructureType 1000138003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT = VkStructureType 1000138000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT = VkStructureType 1000138001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT"
pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT = VkStructureType 1000138002
