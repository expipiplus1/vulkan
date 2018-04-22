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
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT = VkStructureType 1000190000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT = VkStructureType 1000190001
-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION"
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION :: Integral a => a
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME"
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = "VK_EXT_vertex_attribute_divisor"
-- | VkVertexInputBindingDivisorDescriptionEXT - Structure specifying a
-- divisor used in instanced rendering
--
-- = Description
--
-- If this structure is not used to define a divisor value for an attribute
-- then the divisor has a logical default value of 1.
--
-- == Valid Usage
--
-- -   @binding@ /must/ be less than
--     @VkPhysicalDeviceLimits@::@maxVertexInputBindings@
--
-- -   @divisor@ /must/ be a value between @0@ and
--     @VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT@::@maxVertexAttribDivisor@,
--     inclusive.
--
-- -   'Graphics.Vulkan.Core10.Pipeline.VkVertexInputBindingDescription'::@inputRate@
--     /must/ be of type @VK_VERTEX_INPUT_RATE_INSTANCE@ for this
--     @binding@.
--
-- = See Also
--
-- 'VkPipelineVertexInputDivisorStateCreateInfoEXT'
data VkVertexInputBindingDivisorDescriptionEXT = VkVertexInputBindingDivisorDescriptionEXT
  { -- | @binding@ is the binding number for which the divisor is specified.
  vkBinding :: Word32
  , -- | @divisor@ is the the number of successive instances that will use the
  -- same value of the vertex attribute when instanced rendering is enabled.
  -- For example, if the divisor is N, the same vertex attribute will applied
  -- to N successive instances before moving on to the next vertex attribute.
  -- If a value of 0 is used for the divisor, then the first vertex attribute
  -- will be applied to all instances. The maximum value of divisor is
  -- implementation dependent and can be queried using
  -- @VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT@::@maxVertexAttribDivisor@.
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
-- | VkPipelineVertexInputDivisorStateCreateInfoEXT - Structure specifying
-- vertex attributes assignment during instanced rendering
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT@
--
-- -   @pVertexBindingDivisors@ /must/ be a valid pointer to an array of
--     @vertexBindingDivisorCount@
--     @VkVertexInputBindingDivisorDescriptionEXT@ structures
--
-- -   @vertexBindingDivisorCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'VkVertexInputBindingDivisorDescriptionEXT'
data VkPipelineVertexInputDivisorStateCreateInfoEXT = VkPipelineVertexInputDivisorStateCreateInfoEXT
  { -- | @sType@ is the type of this structure
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure
  vkPNext :: Ptr ()
  , -- | @vertexBindingDivisorCount@ is the number of elements in the
  -- @pVertexBindingDivisors@ array.
  vkVertexBindingDivisorCount :: Word32
  , -- | @pVertexBindingDivisors@ is a pointer to an array of
  -- @VkVertexInputBindingDivisorDescriptionEXT@ structures, which specifies
  -- the divisor value for each binding.
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
-- | VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT - Structure
-- describing max value of vertex attribute divisor that can be supported
-- by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @maxVertexAttribDivisor@ is the maximum value of the number of
--     instances that will repeat the value of vertex attribute data when
--     instanced rendering is enabled.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
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
