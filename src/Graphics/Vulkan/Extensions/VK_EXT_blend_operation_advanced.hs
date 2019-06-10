{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced
  ( BlendOverlapEXT
  , pattern BLEND_OVERLAP_UNCORRELATED_EXT
  , pattern BLEND_OVERLAP_DISJOINT_EXT
  , pattern BLEND_OVERLAP_CONJOINT_EXT
#if defined(VK_USE_PLATFORM_GGP)
  , PhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , PhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , PipelineColorBlendAdvancedStateCreateInfoEXT(..)
#endif
  , pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  , pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
  , pattern BLEND_OP_ZERO_EXT
  , pattern BLEND_OP_SRC_EXT
  , pattern BLEND_OP_DST_EXT
  , pattern BLEND_OP_SRC_OVER_EXT
  , pattern BLEND_OP_DST_OVER_EXT
  , pattern BLEND_OP_SRC_IN_EXT
  , pattern BLEND_OP_DST_IN_EXT
  , pattern BLEND_OP_SRC_OUT_EXT
  , pattern BLEND_OP_DST_OUT_EXT
  , pattern BLEND_OP_SRC_ATOP_EXT
  , pattern BLEND_OP_DST_ATOP_EXT
  , pattern BLEND_OP_XOR_EXT
  , pattern BLEND_OP_MULTIPLY_EXT
  , pattern BLEND_OP_SCREEN_EXT
  , pattern BLEND_OP_OVERLAY_EXT
  , pattern BLEND_OP_DARKEN_EXT
  , pattern BLEND_OP_LIGHTEN_EXT
  , pattern BLEND_OP_COLORDODGE_EXT
  , pattern BLEND_OP_COLORBURN_EXT
  , pattern BLEND_OP_HARDLIGHT_EXT
  , pattern BLEND_OP_SOFTLIGHT_EXT
  , pattern BLEND_OP_DIFFERENCE_EXT
  , pattern BLEND_OP_EXCLUSION_EXT
  , pattern BLEND_OP_INVERT_EXT
  , pattern BLEND_OP_INVERT_RGB_EXT
  , pattern BLEND_OP_LINEARDODGE_EXT
  , pattern BLEND_OP_LINEARBURN_EXT
  , pattern BLEND_OP_VIVIDLIGHT_EXT
  , pattern BLEND_OP_LINEARLIGHT_EXT
  , pattern BLEND_OP_PINLIGHT_EXT
  , pattern BLEND_OP_HARDMIX_EXT
  , pattern BLEND_OP_HSL_HUE_EXT
  , pattern BLEND_OP_HSL_SATURATION_EXT
  , pattern BLEND_OP_HSL_COLOR_EXT
  , pattern BLEND_OP_HSL_LUMINOSITY_EXT
  , pattern BLEND_OP_PLUS_EXT
  , pattern BLEND_OP_PLUS_CLAMPED_EXT
  , pattern BLEND_OP_PLUS_CLAMPED_ALPHA_EXT
  , pattern BLEND_OP_PLUS_DARKER_EXT
  , pattern BLEND_OP_MINUS_EXT
  , pattern BLEND_OP_MINUS_CLAMPED_EXT
  , pattern BLEND_OP_CONTRAST_EXT
  , pattern BLEND_OP_INVERT_OVG_EXT
  , pattern BLEND_OP_RED_EXT
  , pattern BLEND_OP_GREEN_EXT
  , pattern BLEND_OP_BLUE_EXT
  , pattern ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
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
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( VkBlendOverlapEXT(..)
  , pattern VK_BLEND_OVERLAP_CONJOINT_EXT
  , pattern VK_BLEND_OVERLAP_DISJOINT_EXT
  , pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Pass
  ( pattern ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern BLEND_OP_BLUE_EXT
  , pattern BLEND_OP_COLORBURN_EXT
  , pattern BLEND_OP_COLORDODGE_EXT
  , pattern BLEND_OP_CONTRAST_EXT
  , pattern BLEND_OP_DARKEN_EXT
  , pattern BLEND_OP_DIFFERENCE_EXT
  , pattern BLEND_OP_DST_ATOP_EXT
  , pattern BLEND_OP_DST_EXT
  , pattern BLEND_OP_DST_IN_EXT
  , pattern BLEND_OP_DST_OUT_EXT
  , pattern BLEND_OP_DST_OVER_EXT
  , pattern BLEND_OP_EXCLUSION_EXT
  , pattern BLEND_OP_GREEN_EXT
  , pattern BLEND_OP_HARDLIGHT_EXT
  , pattern BLEND_OP_HARDMIX_EXT
  , pattern BLEND_OP_HSL_COLOR_EXT
  , pattern BLEND_OP_HSL_HUE_EXT
  , pattern BLEND_OP_HSL_LUMINOSITY_EXT
  , pattern BLEND_OP_HSL_SATURATION_EXT
  , pattern BLEND_OP_INVERT_EXT
  , pattern BLEND_OP_INVERT_OVG_EXT
  , pattern BLEND_OP_INVERT_RGB_EXT
  , pattern BLEND_OP_LIGHTEN_EXT
  , pattern BLEND_OP_LINEARBURN_EXT
  , pattern BLEND_OP_LINEARDODGE_EXT
  , pattern BLEND_OP_LINEARLIGHT_EXT
  , pattern BLEND_OP_MINUS_CLAMPED_EXT
  , pattern BLEND_OP_MINUS_EXT
  , pattern BLEND_OP_MULTIPLY_EXT
  , pattern BLEND_OP_OVERLAY_EXT
  , pattern BLEND_OP_PINLIGHT_EXT
  , pattern BLEND_OP_PLUS_CLAMPED_ALPHA_EXT
  , pattern BLEND_OP_PLUS_CLAMPED_EXT
  , pattern BLEND_OP_PLUS_DARKER_EXT
  , pattern BLEND_OP_PLUS_EXT
  , pattern BLEND_OP_RED_EXT
  , pattern BLEND_OP_SCREEN_EXT
  , pattern BLEND_OP_SOFTLIGHT_EXT
  , pattern BLEND_OP_SRC_ATOP_EXT
  , pattern BLEND_OP_SRC_EXT
  , pattern BLEND_OP_SRC_IN_EXT
  , pattern BLEND_OP_SRC_OUT_EXT
  , pattern BLEND_OP_SRC_OVER_EXT
  , pattern BLEND_OP_VIVIDLIGHT_EXT
  , pattern BLEND_OP_XOR_EXT
  , pattern BLEND_OP_ZERO_EXT
  )


-- No documentation found for TopLevel "BlendOverlapEXT"
type BlendOverlapEXT = VkBlendOverlapEXT


{-# complete BLEND_OVERLAP_UNCORRELATED_EXT, BLEND_OVERLAP_DISJOINT_EXT, BLEND_OVERLAP_CONJOINT_EXT :: BlendOverlapEXT #-}


-- No documentation found for Nested "BlendOverlapEXT" "BLEND_OVERLAP_UNCORRELATED_EXT"
pattern BLEND_OVERLAP_UNCORRELATED_EXT :: (a ~ BlendOverlapEXT) => a
pattern BLEND_OVERLAP_UNCORRELATED_EXT = VK_BLEND_OVERLAP_UNCORRELATED_EXT


-- No documentation found for Nested "BlendOverlapEXT" "BLEND_OVERLAP_DISJOINT_EXT"
pattern BLEND_OVERLAP_DISJOINT_EXT :: (a ~ BlendOverlapEXT) => a
pattern BLEND_OVERLAP_DISJOINT_EXT = VK_BLEND_OVERLAP_DISJOINT_EXT


-- No documentation found for Nested "BlendOverlapEXT" "BLEND_OVERLAP_CONJOINT_EXT"
pattern BLEND_OVERLAP_CONJOINT_EXT :: (a ~ BlendOverlapEXT) => a
pattern BLEND_OVERLAP_CONJOINT_EXT = VK_BLEND_OVERLAP_CONJOINT_EXT


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT"
data PhysicalDeviceBlendOperationAdvancedFeaturesEXT = PhysicalDeviceBlendOperationAdvancedFeaturesEXT
  { -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedFeaturesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedFeaturesEXT" "advancedBlendCoherentOperations"
  advancedBlendCoherentOperations :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  zero = PhysicalDeviceBlendOperationAdvancedFeaturesEXT Nothing
                                                         False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT"
data PhysicalDeviceBlendOperationAdvancedPropertiesEXT = PhysicalDeviceBlendOperationAdvancedPropertiesEXT
  { -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendMaxColorAttachments"
  advancedBlendMaxColorAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendIndependentBlend"
  advancedBlendIndependentBlend :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendNonPremultipliedSrcColor"
  advancedBlendNonPremultipliedSrcColor :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendNonPremultipliedDstColor"
  advancedBlendNonPremultipliedDstColor :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendCorrelatedOverlap"
  advancedBlendCorrelatedOverlap :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendAllOperations"
  advancedBlendAllOperations :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  zero = PhysicalDeviceBlendOperationAdvancedPropertiesEXT Nothing
                                                           zero
                                                           False
                                                           False
                                                           False
                                                           False
                                                           False

#endif


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPipelineColorBlendAdvancedStateCreateInfoEXT"
data PipelineColorBlendAdvancedStateCreateInfoEXT = PipelineColorBlendAdvancedStateCreateInfoEXT
  { -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "srcPremultiplied"
  srcPremultiplied :: Bool
  , -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "dstPremultiplied"
  dstPremultiplied :: Bool
  , -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "blendOverlap"
  blendOverlap :: BlendOverlapEXT
  }
  deriving (Show, Eq)

instance Zero PipelineColorBlendAdvancedStateCreateInfoEXT where
  zero = PipelineColorBlendAdvancedStateCreateInfoEXT Nothing
                                                      False
                                                      False
                                                      zero

#endif

-- No documentation found for TopLevel "VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME"
pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME = VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION"
pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION :: Integral a => a
pattern EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
