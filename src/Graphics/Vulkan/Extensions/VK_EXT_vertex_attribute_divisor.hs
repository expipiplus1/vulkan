{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
  , pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
  , pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
  , VkVertexInputBindingDivisorDescriptionEXT(..)
  , VkPipelineVertexInputDivisorStateCreateInfoEXT(..)
  , VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  ) where

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


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT = VkStructureType 1000190000
-- | Nothing
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT = VkStructureType 1000190001
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION :: Integral a => a
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 1
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_EXT_vertex_attribute_divisor"
-- | TODO: Struct comments
data VkVertexInputBindingDivisorDescriptionEXT = VkVertexInputBindingDivisorDescriptionEXT
  { vkBinding :: Word32
  , vkDivisor :: Word32
  }
  deriving (Eq, Show)

instance Storable VkVertexInputBindingDivisorDescriptionEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkVertexInputBindingDivisorDescriptionEXT <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBinding (poked :: VkVertexInputBindingDivisorDescriptionEXT))
                *> poke (ptr `plusPtr` 4) (vkDivisor (poked :: VkVertexInputBindingDivisorDescriptionEXT))
-- | TODO: Struct comments
data VkPipelineVertexInputDivisorStateCreateInfoEXT = VkPipelineVertexInputDivisorStateCreateInfoEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkVertexBindingDivisorCount :: Word32
  , vkVertexBindingDivisors :: Ptr VkVertexInputBindingDivisorDescriptionEXT
  }
  deriving (Eq, Show)

instance Storable VkPipelineVertexInputDivisorStateCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineVertexInputDivisorStateCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineVertexInputDivisorStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPipelineVertexInputDivisorStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkVertexBindingDivisorCount (poked :: VkPipelineVertexInputDivisorStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkVertexBindingDivisors (poked :: VkPipelineVertexInputDivisorStateCreateInfoEXT))
-- | TODO: Struct comments
data VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT = VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkMaxVertexAttribDivisor :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                                 <*> peek (ptr `plusPtr` 8)
                                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxVertexAttribDivisor (poked :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))
