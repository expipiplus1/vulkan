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


-- | VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT - Structure describing
-- if fetching of vertex attribute may be repeated for instanced rendering
--
-- = Description
--
-- If the @VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating the implementation-dependent
-- behavior. @VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT@ /can/ also
-- be used in @pNext@ chain of
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo' to enable the
-- feature.
--
-- Unresolved directive in
-- VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT.txt -
-- include::..\/validity\/structs\/VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT = VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @vertexAttributeInstanceRateDivisor@ specifies whether vertex attribute
  -- fetching may be repeated in case of instanced rendering.
  vkVertexAttributeInstanceRateDivisor :: VkBool32
  , -- | @vertexAttributeInstanceRateZeroDivisor@ specifies whether a zero value
  -- for @VkVertexInputBindingDivisorDescriptionEXT@::@divisor@ is supported.
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
  zero = VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT zero
                                                           zero
                                                           zero
                                                           zero
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
-- If the @VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT@ structure
-- is included in the @pNext@ chain of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT = VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT
  { -- | @sType@ /must/ be
  -- @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT@
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @maxVertexAttribDivisor@ is the maximum value of the number of instances
  -- that will repeat the value of vertex attribute data when instanced
  -- rendering is enabled.
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
  zero = VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT zero
                                                             zero
                                                             zero
-- | VkPipelineVertexInputDivisorStateCreateInfoEXT - Structure specifying
-- vertex attributes assignment during instanced rendering
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- No cross-references are available
data VkPipelineVertexInputDivisorStateCreateInfoEXT = VkPipelineVertexInputDivisorStateCreateInfoEXT
  { -- | @sType@ /must/ be
  -- @VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT@
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure
  vkPNext :: Ptr ()
  , -- | @vertexBindingDivisorCount@ /must/ be greater than @0@
  vkVertexBindingDivisorCount :: Word32
  , -- | @pVertexBindingDivisors@ /must/ be a valid pointer to an array of
  -- @vertexBindingDivisorCount@ @VkVertexInputBindingDivisorDescriptionEXT@
  -- structures
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
  zero = VkPipelineVertexInputDivisorStateCreateInfoEXT zero
                                                        zero
                                                        zero
                                                        zero
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
-- -   If the @vertexAttributeInstanceRateZeroDivisor@ feature is not
--     enabled, @divisor@ /must/ not be @0@
--
-- -   If the @vertexAttributeInstanceRateDivisor@ feature is not enabled,
--     @divisor@ /must/ be @1@
--
-- -   @divisor@ /must/ be a value between @0@ and
--     @VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT@::@maxVertexAttribDivisor@,
--     inclusive.
--
-- -   'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputBindingDescription'::@inputRate@
--     /must/ be of type @VK_VERTEX_INPUT_RATE_INSTANCE@ for this
--     @binding@.
--
-- = See Also
--
-- No cross-references are available
data VkVertexInputBindingDivisorDescriptionEXT = VkVertexInputBindingDivisorDescriptionEXT
  { -- | @binding@ is the binding number for which the divisor is specified.
  vkBinding :: Word32
  , -- | @divisor@ is the number of successive instances that will use the same
  -- value of the vertex attribute when instanced rendering is enabled. For
  -- example, if the divisor is N, the same vertex attribute will applied to
  -- N successive instances before moving on to the next vertex attribute.
  -- The maximum value of divisor is implementation dependent and can be
  -- queried using
  -- @VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT@::@maxVertexAttribDivisor@.
  -- A value of @0@ /can/ be used for the divisor if the
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-vertexAttributeInstanceRateZeroDivisor vertexAttributeInstanceRateZeroDivisor>
  -- feature is enabled. In this case, the same vertex attribute will be
  -- applied to all instances.
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
pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: (Eq a ,IsString a) => a
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
