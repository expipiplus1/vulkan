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


-- | VkDescriptorPoolInlineUniformBlockCreateInfoEXT - Structure specifying
-- the maximum number of inline uniform block bindings of a newly created
-- descriptor pool
--
-- = Description
--
-- Unresolved directive in
-- VkDescriptorPoolInlineUniformBlockCreateInfoEXT.txt -
-- include::..\/validity\/structs\/VkDescriptorPoolInlineUniformBlockCreateInfoEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDescriptorPoolInlineUniformBlockCreateInfoEXT = VkDescriptorPoolInlineUniformBlockCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxInlineUniformBlockBindings@ is the number of inline uniform block
  -- bindings to allocate.
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
  zero = VkDescriptorPoolInlineUniformBlockCreateInfoEXT zero
                                                         zero
                                                         zero
-- | VkPhysicalDeviceInlineUniformBlockFeaturesEXT - Structure describing
-- inline uniform block features that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceInlineUniformBlockFeaturesEXT@
-- structure describe the following features:
--
-- = Description
--
-- If the @VkPhysicalDeviceInlineUniformBlockFeaturesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- @VkPhysicalDeviceInlineUniformBlockFeaturesEXT@ /can/ also be used in
-- the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable features.
--
-- Unresolved directive in
-- VkPhysicalDeviceInlineUniformBlockFeaturesEXT.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceInlineUniformBlockFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceInlineUniformBlockFeaturesEXT = VkPhysicalDeviceInlineUniformBlockFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- | @inlineUniformBlock@ indicates whether the implementation supports
  -- inline uniform block descriptors. If this feature is not enabled,
  -- @VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT@ /must/ not be used.
  vkInlineUniformBlock :: VkBool32
  , -- | @descriptorBindingInlineUniformBlockUpdateAfterBind@ indicates whether
  -- the implementation supports updating inline uniform block descriptors
  -- after a set is bound. If this feature is not enabled,
  -- @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ /must/ not be used
  -- with @VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT@.
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
  zero = VkPhysicalDeviceInlineUniformBlockFeaturesEXT zero
                                                       zero
                                                       zero
                                                       zero
-- | VkPhysicalDeviceInlineUniformBlockPropertiesEXT - Structure describing
-- inline uniform block properties that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceInlineUniformBlockPropertiesEXT@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- If the @VkPhysicalDeviceInlineUniformBlockPropertiesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Unresolved directive in
-- VkPhysicalDeviceInlineUniformBlockPropertiesEXT.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceInlineUniformBlockPropertiesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceInlineUniformBlockPropertiesEXT = VkPhysicalDeviceInlineUniformBlockPropertiesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxInlineUniformBlockSize@ is the maximum size in bytes of an
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-inlineuniformblock inline uniform block>
  -- binding.
  vkMaxInlineUniformBlockSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorInlineUniformBlocks"
  vkMaxPerStageDescriptorInlineUniformBlocks :: Word32
  , -- | @maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks@ is similar to
  -- @maxPerStageDescriptorInlineUniformBlocks@ but counts descriptor
  -- bindings from descriptor sets created with or without the
  -- @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
  -- set.
  vkMaxPerStageDescriptorUpdateAfterBindInlineUniformBlocks :: Word32
  , -- | @maxDescriptorSetInlineUniformBlocks@ is the maximum number of inline
  -- uniform block bindings that /can/ be included in descriptor bindings in
  -- a pipeline layout across all pipeline shader stages and descriptor set
  -- numbers. Descriptor bindings with a descriptor type of
  -- @VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT@ count against this limit.
  -- Only descriptor bindings in descriptor set layouts created without the
  -- @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit set
  -- count against this limit.
  vkMaxDescriptorSetInlineUniformBlocks :: Word32
  , -- | @maxDescriptorSetUpdateAfterBindInlineUniformBlocks@ is similar to
  -- @maxDescriptorSetInlineUniformBlocks@ but counts descriptor bindings
  -- from descriptor sets created with or without the
  -- @VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT@ bit
  -- set.
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
  zero = VkPhysicalDeviceInlineUniformBlockPropertiesEXT zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
                                                         zero
-- | VkWriteDescriptorSetInlineUniformBlockEXT - Structure specifying inline
-- uniform block data
--
-- == Valid Usage
--
-- Unresolved directive in VkWriteDescriptorSetInlineUniformBlockEXT.txt -
-- include::..\/validity\/structs\/VkWriteDescriptorSetInlineUniformBlockEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkWriteDescriptorSetInlineUniformBlockEXT = VkWriteDescriptorSetInlineUniformBlockEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @dataSize@ /must/ be an integer multiple of @4@
  vkDataSize :: Word32
  , -- | @pData@ is a pointer to @dataSize@ number of bytes of data to write to
  -- the inline uniform block.
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
  zero = VkWriteDescriptorSetInlineUniformBlockEXT zero
                                                   zero
                                                   zero
                                                   zero
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT"
pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT = VkDescriptorType 1000138000
-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME"
pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME :: (Eq a ,IsString a) => a
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
