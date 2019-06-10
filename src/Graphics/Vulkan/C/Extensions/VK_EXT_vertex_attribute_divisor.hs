{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor
  ( VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
  , VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , VkPipelineVertexInputDivisorStateCreateInfoEXT(..)
  , VkVertexInputBindingDivisorDescriptionEXT(..)
  , pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
  , pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
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


-- No documentation found for TopLevel "VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT"
data VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT = VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT" "vertexAttributeInstanceRateDivisor"
  vkVertexAttributeInstanceRateDivisor :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT" "vertexAttributeInstanceRateZeroDivisor"
  vkVertexAttributeInstanceRateZeroDivisor :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                               <*> peek (ptr `plusPtr` 8)
                                                               <*> peek (ptr `plusPtr` 16)
                                                               <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkVertexAttributeInstanceRateDivisor (poked :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT))
                *> poke (ptr `plusPtr` 20) (vkVertexAttributeInstanceRateZeroDivisor (poked :: VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT))

instance Zero VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  zero = VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
                                                           zero
                                                           zero
                                                           zero

-- No documentation found for TopLevel "VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT"
data VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT = VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT" "maxVertexAttribDivisor"
  vkMaxVertexAttribDivisor :: Word32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                                 <*> peek (ptr `plusPtr` 8)
                                                                 <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkMaxVertexAttribDivisor (poked :: VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))

instance Zero VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  zero = VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
                                                             zero
                                                             zero

-- No documentation found for TopLevel "VkPipelineVertexInputDivisorStateCreateInfoEXT"
data VkPipelineVertexInputDivisorStateCreateInfoEXT = VkPipelineVertexInputDivisorStateCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineVertexInputDivisorStateCreateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPipelineVertexInputDivisorStateCreateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPipelineVertexInputDivisorStateCreateInfoEXT" "vertexBindingDivisorCount"
  vkVertexBindingDivisorCount :: Word32
  , -- No documentation found for Nested "VkPipelineVertexInputDivisorStateCreateInfoEXT" "pVertexBindingDivisors"
  vkPVertexBindingDivisors :: Ptr VkVertexInputBindingDivisorDescriptionEXT
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineVertexInputDivisorStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkVertexBindingDivisorCount (poked :: VkPipelineVertexInputDivisorStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkPVertexBindingDivisors (poked :: VkPipelineVertexInputDivisorStateCreateInfoEXT))

instance Zero VkPipelineVertexInputDivisorStateCreateInfoEXT where
  zero = VkPipelineVertexInputDivisorStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
                                                        zero
                                                        zero
                                                        zero

-- No documentation found for TopLevel "VkVertexInputBindingDivisorDescriptionEXT"
data VkVertexInputBindingDivisorDescriptionEXT = VkVertexInputBindingDivisorDescriptionEXT
  { -- No documentation found for Nested "VkVertexInputBindingDivisorDescriptionEXT" "binding"
  vkBinding :: Word32
  , -- No documentation found for Nested "VkVertexInputBindingDivisorDescriptionEXT" "divisor"
  vkDivisor :: Word32
  }
  deriving (Eq, Show)

instance Storable VkVertexInputBindingDivisorDescriptionEXT where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkVertexInputBindingDivisorDescriptionEXT <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBinding (poked :: VkVertexInputBindingDivisorDescriptionEXT))
                *> poke (ptr `plusPtr` 4) (vkDivisor (poked :: VkVertexInputBindingDivisorDescriptionEXT))

instance Zero VkVertexInputBindingDivisorDescriptionEXT where
  zero = VkVertexInputBindingDivisorDescriptionEXT zero
                                                   zero

-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME"
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_EXT_vertex_attribute_divisor"

-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION"
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION :: Integral a => a
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 3

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT = VkStructureType 1000190002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT = VkStructureType 1000190000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT = VkStructureType 1000190001
