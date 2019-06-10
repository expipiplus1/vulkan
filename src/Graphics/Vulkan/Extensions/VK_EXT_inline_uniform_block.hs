{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  DescriptorPoolInlineUniformBlockCreateInfoEXT(..)
  , 
  PhysicalDeviceInlineUniformBlockFeaturesEXT(..)
  , PhysicalDeviceInlineUniformBlockPropertiesEXT(..)
  , WriteDescriptorSetInlineUniformBlockEXT(..)
#endif
  , pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
  , pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
  , pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Word
  ( Word32
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( Ptr
  , nullPtr
  )
#endif



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block
  ( pattern VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME
  , pattern VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkDescriptorPoolInlineUniformBlockCreateInfoEXT"
data DescriptorPoolInlineUniformBlockCreateInfoEXT = DescriptorPoolInlineUniformBlockCreateInfoEXT
  { -- No documentation found for Nested "DescriptorPoolInlineUniformBlockCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorPoolInlineUniformBlockCreateInfoEXT" "maxInlineUniformBlockBindings"
  maxInlineUniformBlockBindings :: Word32
  }
  deriving (Show, Eq)

instance Zero DescriptorPoolInlineUniformBlockCreateInfoEXT where
  zero = DescriptorPoolInlineUniformBlockCreateInfoEXT Nothing
                                                       zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceInlineUniformBlockFeaturesEXT"
data PhysicalDeviceInlineUniformBlockFeaturesEXT = PhysicalDeviceInlineUniformBlockFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockFeaturesEXT" "inlineUniformBlock"
  inlineUniformBlock :: Bool
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockFeaturesEXT" "descriptorBindingInlineUniformBlockUpdateAfterBind"
  descriptorBindingInlineUniformBlockUpdateAfterBind :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceInlineUniformBlockFeaturesEXT where
  zero = PhysicalDeviceInlineUniformBlockFeaturesEXT Nothing
                                                     False
                                                     False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceInlineUniformBlockPropertiesEXT"
data PhysicalDeviceInlineUniformBlockPropertiesEXT = PhysicalDeviceInlineUniformBlockPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxInlineUniformBlockSize"
  maxInlineUniformBlockSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorInlineUniformBlocks"
  maxPerStageDescriptorInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks"
  maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxDescriptorSetInlineUniformBlocks"
  maxDescriptorSetInlineUniformBlocks :: Word32
  , -- No documentation found for Nested "PhysicalDeviceInlineUniformBlockPropertiesEXT" "maxDescriptorSetUpdateAfterBindInlineUniformBlocks"
  maxDescriptorSetUpdateAfterBindInlineUniformBlocks :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceInlineUniformBlockPropertiesEXT where
  zero = PhysicalDeviceInlineUniformBlockPropertiesEXT Nothing
                                                       zero
                                                       zero
                                                       zero
                                                       zero
                                                       zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkWriteDescriptorSetInlineUniformBlockEXT"
data WriteDescriptorSetInlineUniformBlockEXT = WriteDescriptorSetInlineUniformBlockEXT
  { -- No documentation found for Nested "WriteDescriptorSetInlineUniformBlockEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "WriteDescriptorSetInlineUniformBlockEXT" "dataSize"
  dataSize :: Word32
  , -- No documentation found for Nested "WriteDescriptorSetInlineUniformBlockEXT" "pData"
  data' :: Ptr ()
  }
  deriving (Show, Eq)

instance Zero WriteDescriptorSetInlineUniformBlockEXT where
  zero = WriteDescriptorSetInlineUniformBlockEXT Nothing
                                                 zero
                                                 nullPtr

#endif

-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME"
pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME = VK_EXT_INLINE_UNIFORM_BLOCK_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION"
pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION :: Integral a => a
pattern EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION = VK_EXT_INLINE_UNIFORM_BLOCK_SPEC_VERSION
