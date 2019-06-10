{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
  , 
  PhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , PipelineVertexInputDivisorStateCreateInfoEXT(..)
#endif
  , VertexInputBindingDivisorDescriptionEXT(..)
  , pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
  , pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )

#if defined(VK_USE_PLATFORM_GGP)
import Data.Vector
  ( Vector
  )
#endif
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor
  ( pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME
  , pattern VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT"
data PhysicalDeviceVertexAttributeDivisorFeaturesEXT = PhysicalDeviceVertexAttributeDivisorFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorFeaturesEXT" "vertexAttributeInstanceRateDivisor"
  vertexAttributeInstanceRateDivisor :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorFeaturesEXT" "vertexAttributeInstanceRateZeroDivisor"
  vertexAttributeInstanceRateZeroDivisor :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  zero = PhysicalDeviceVertexAttributeDivisorFeaturesEXT Nothing
                                                         False
                                                         False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT"
data PhysicalDeviceVertexAttributeDivisorPropertiesEXT = PhysicalDeviceVertexAttributeDivisorPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVertexAttributeDivisorPropertiesEXT" "maxVertexAttribDivisor"
  maxVertexAttribDivisor :: Word32
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  zero = PhysicalDeviceVertexAttributeDivisorPropertiesEXT Nothing
                                                           zero

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineVertexInputDivisorStateCreateInfoEXT"
data PipelineVertexInputDivisorStateCreateInfoEXT = PipelineVertexInputDivisorStateCreateInfoEXT
  { -- No documentation found for Nested "PipelineVertexInputDivisorStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineVertexInputDivisorStateCreateInfoEXT" "pVertexBindingDivisors"
  vertexBindingDivisors :: Vector VertexInputBindingDivisorDescriptionEXT
  }
  deriving (Show, Eq)

instance Zero PipelineVertexInputDivisorStateCreateInfoEXT where
  zero = PipelineVertexInputDivisorStateCreateInfoEXT Nothing
                                                      mempty

#endif


-- No documentation found for TopLevel "VkVertexInputBindingDivisorDescriptionEXT"
data VertexInputBindingDivisorDescriptionEXT = VertexInputBindingDivisorDescriptionEXT
  { -- No documentation found for Nested "VertexInputBindingDivisorDescriptionEXT" "binding"
  binding :: Word32
  , -- No documentation found for Nested "VertexInputBindingDivisorDescriptionEXT" "divisor"
  divisor :: Word32
  }
  deriving (Show, Eq)

instance Zero VertexInputBindingDivisorDescriptionEXT where
  zero = VertexInputBindingDivisorDescriptionEXT zero
                                                 zero


-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME"
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME = VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION"
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION :: Integral a => a
pattern EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION = VK_EXT_VERTEX_ATTRIBUTE_DIVISOR_SPEC_VERSION
