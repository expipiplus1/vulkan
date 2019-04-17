{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced
  ( BlendOverlapEXT
  , withCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT
  , fromCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT
  , PhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , withCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT
  , fromCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT
  , PhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , withCStructPipelineColorBlendAdvancedStateCreateInfoEXT
  , fromCStructPipelineColorBlendAdvancedStateCreateInfoEXT
  , PipelineColorBlendAdvancedStateCreateInfoEXT(..)
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
  , pattern VK_BLEND_OP_ZERO_EXT
  , pattern VK_BLEND_OP_SRC_EXT
  , pattern VK_BLEND_OP_DST_EXT
  , pattern VK_BLEND_OP_SRC_OVER_EXT
  , pattern VK_BLEND_OP_DST_OVER_EXT
  , pattern VK_BLEND_OP_SRC_IN_EXT
  , pattern VK_BLEND_OP_DST_IN_EXT
  , pattern VK_BLEND_OP_SRC_OUT_EXT
  , pattern VK_BLEND_OP_DST_OUT_EXT
  , pattern VK_BLEND_OP_SRC_ATOP_EXT
  , pattern VK_BLEND_OP_DST_ATOP_EXT
  , pattern VK_BLEND_OP_XOR_EXT
  , pattern VK_BLEND_OP_MULTIPLY_EXT
  , pattern VK_BLEND_OP_SCREEN_EXT
  , pattern VK_BLEND_OP_OVERLAY_EXT
  , pattern VK_BLEND_OP_DARKEN_EXT
  , pattern VK_BLEND_OP_LIGHTEN_EXT
  , pattern VK_BLEND_OP_COLORDODGE_EXT
  , pattern VK_BLEND_OP_COLORBURN_EXT
  , pattern VK_BLEND_OP_HARDLIGHT_EXT
  , pattern VK_BLEND_OP_SOFTLIGHT_EXT
  , pattern VK_BLEND_OP_DIFFERENCE_EXT
  , pattern VK_BLEND_OP_EXCLUSION_EXT
  , pattern VK_BLEND_OP_INVERT_EXT
  , pattern VK_BLEND_OP_INVERT_RGB_EXT
  , pattern VK_BLEND_OP_LINEARDODGE_EXT
  , pattern VK_BLEND_OP_LINEARBURN_EXT
  , pattern VK_BLEND_OP_VIVIDLIGHT_EXT
  , pattern VK_BLEND_OP_LINEARLIGHT_EXT
  , pattern VK_BLEND_OP_PINLIGHT_EXT
  , pattern VK_BLEND_OP_HARDMIX_EXT
  , pattern VK_BLEND_OP_HSL_HUE_EXT
  , pattern VK_BLEND_OP_HSL_SATURATION_EXT
  , pattern VK_BLEND_OP_HSL_COLOR_EXT
  , pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT
  , pattern VK_BLEND_OP_PLUS_EXT
  , pattern VK_BLEND_OP_PLUS_CLAMPED_EXT
  , pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT
  , pattern VK_BLEND_OP_PLUS_DARKER_EXT
  , pattern VK_BLEND_OP_MINUS_EXT
  , pattern VK_BLEND_OP_MINUS_CLAMPED_EXT
  , pattern VK_BLEND_OP_CONTRAST_EXT
  , pattern VK_BLEND_OP_INVERT_OVG_EXT
  , pattern VK_BLEND_OP_RED_EXT
  , pattern VK_BLEND_OP_GREEN_EXT
  , pattern VK_BLEND_OP_BLUE_EXT
  , pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( VkBlendOverlapEXT(..)
  , VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , VkPipelineColorBlendAdvancedStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
  , pattern VK_BLEND_OP_BLUE_EXT
  , pattern VK_BLEND_OP_COLORBURN_EXT
  , pattern VK_BLEND_OP_COLORDODGE_EXT
  , pattern VK_BLEND_OP_CONTRAST_EXT
  , pattern VK_BLEND_OP_DARKEN_EXT
  , pattern VK_BLEND_OP_DIFFERENCE_EXT
  , pattern VK_BLEND_OP_DST_ATOP_EXT
  , pattern VK_BLEND_OP_DST_EXT
  , pattern VK_BLEND_OP_DST_IN_EXT
  , pattern VK_BLEND_OP_DST_OUT_EXT
  , pattern VK_BLEND_OP_DST_OVER_EXT
  , pattern VK_BLEND_OP_EXCLUSION_EXT
  , pattern VK_BLEND_OP_GREEN_EXT
  , pattern VK_BLEND_OP_HARDLIGHT_EXT
  , pattern VK_BLEND_OP_HARDMIX_EXT
  , pattern VK_BLEND_OP_HSL_COLOR_EXT
  , pattern VK_BLEND_OP_HSL_HUE_EXT
  , pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT
  , pattern VK_BLEND_OP_HSL_SATURATION_EXT
  , pattern VK_BLEND_OP_INVERT_EXT
  , pattern VK_BLEND_OP_INVERT_OVG_EXT
  , pattern VK_BLEND_OP_INVERT_RGB_EXT
  , pattern VK_BLEND_OP_LIGHTEN_EXT
  , pattern VK_BLEND_OP_LINEARBURN_EXT
  , pattern VK_BLEND_OP_LINEARDODGE_EXT
  , pattern VK_BLEND_OP_LINEARLIGHT_EXT
  , pattern VK_BLEND_OP_MINUS_CLAMPED_EXT
  , pattern VK_BLEND_OP_MINUS_EXT
  , pattern VK_BLEND_OP_MULTIPLY_EXT
  , pattern VK_BLEND_OP_OVERLAY_EXT
  , pattern VK_BLEND_OP_PINLIGHT_EXT
  , pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT
  , pattern VK_BLEND_OP_PLUS_CLAMPED_EXT
  , pattern VK_BLEND_OP_PLUS_DARKER_EXT
  , pattern VK_BLEND_OP_PLUS_EXT
  , pattern VK_BLEND_OP_RED_EXT
  , pattern VK_BLEND_OP_SCREEN_EXT
  , pattern VK_BLEND_OP_SOFTLIGHT_EXT
  , pattern VK_BLEND_OP_SRC_ATOP_EXT
  , pattern VK_BLEND_OP_SRC_EXT
  , pattern VK_BLEND_OP_SRC_IN_EXT
  , pattern VK_BLEND_OP_SRC_OUT_EXT
  , pattern VK_BLEND_OP_SRC_OVER_EXT
  , pattern VK_BLEND_OP_VIVIDLIGHT_EXT
  , pattern VK_BLEND_OP_XOR_EXT
  , pattern VK_BLEND_OP_ZERO_EXT
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
  )


-- No documentation found for TopLevel "BlendOverlapEXT"
type BlendOverlapEXT = VkBlendOverlapEXT
-- No documentation found for TopLevel "PhysicalDeviceBlendOperationAdvancedFeaturesEXT"
data PhysicalDeviceBlendOperationAdvancedFeaturesEXT = PhysicalDeviceBlendOperationAdvancedFeaturesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedFeaturesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedFeaturesEXT" "advancedBlendCoherentOperations"
  vkAdvancedBlendCoherentOperations :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT :: PhysicalDeviceBlendOperationAdvancedFeaturesEXT -> (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT -> IO a) -> IO a
withCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceBlendOperationAdvancedFeaturesEXT)) (\pPNext -> cont (VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT pPNext (boolToBool32 (vkAdvancedBlendCoherentOperations (from :: PhysicalDeviceBlendOperationAdvancedFeaturesEXT)))))
fromCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT -> IO PhysicalDeviceBlendOperationAdvancedFeaturesEXT
fromCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT c = PhysicalDeviceBlendOperationAdvancedFeaturesEXT <$> -- Univalued Member elided
                                                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT)))
                                                                                                               <*> pure (bool32ToBool (vkAdvancedBlendCoherentOperations (c :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT)))
instance Zero PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  zero = PhysicalDeviceBlendOperationAdvancedFeaturesEXT Nothing
                                                         False
-- No documentation found for TopLevel "PhysicalDeviceBlendOperationAdvancedPropertiesEXT"
data PhysicalDeviceBlendOperationAdvancedPropertiesEXT = PhysicalDeviceBlendOperationAdvancedPropertiesEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendMaxColorAttachments"
  vkAdvancedBlendMaxColorAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendIndependentBlend"
  vkAdvancedBlendIndependentBlend :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendNonPremultipliedSrcColor"
  vkAdvancedBlendNonPremultipliedSrcColor :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendNonPremultipliedDstColor"
  vkAdvancedBlendNonPremultipliedDstColor :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendCorrelatedOverlap"
  vkAdvancedBlendCorrelatedOverlap :: Bool
  , -- No documentation found for Nested "PhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendAllOperations"
  vkAdvancedBlendAllOperations :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT -> (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT -> IO a) -> IO a
withCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT)) (\pPNext -> cont (VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT pPNext (vkAdvancedBlendMaxColorAttachments (from :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT)) (boolToBool32 (vkAdvancedBlendIndependentBlend (from :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT))) (boolToBool32 (vkAdvancedBlendNonPremultipliedSrcColor (from :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT))) (boolToBool32 (vkAdvancedBlendNonPremultipliedDstColor (from :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT))) (boolToBool32 (vkAdvancedBlendCorrelatedOverlap (from :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT))) (boolToBool32 (vkAdvancedBlendAllOperations (from :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT)))))
fromCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT -> IO PhysicalDeviceBlendOperationAdvancedPropertiesEXT
fromCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT c = PhysicalDeviceBlendOperationAdvancedPropertiesEXT <$> -- Univalued Member elided
                                                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
                                                                                                                   <*> pure (vkAdvancedBlendMaxColorAttachments (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                                                                                                                   <*> pure (bool32ToBool (vkAdvancedBlendIndependentBlend (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
                                                                                                                   <*> pure (bool32ToBool (vkAdvancedBlendNonPremultipliedSrcColor (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
                                                                                                                   <*> pure (bool32ToBool (vkAdvancedBlendNonPremultipliedDstColor (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
                                                                                                                   <*> pure (bool32ToBool (vkAdvancedBlendCorrelatedOverlap (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
                                                                                                                   <*> pure (bool32ToBool (vkAdvancedBlendAllOperations (c :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT)))
instance Zero PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  zero = PhysicalDeviceBlendOperationAdvancedPropertiesEXT Nothing
                                                           zero
                                                           False
                                                           False
                                                           False
                                                           False
                                                           False
-- No documentation found for TopLevel "PipelineColorBlendAdvancedStateCreateInfoEXT"
data PipelineColorBlendAdvancedStateCreateInfoEXT = PipelineColorBlendAdvancedStateCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "srcPremultiplied"
  vkSrcPremultiplied :: Bool
  , -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "dstPremultiplied"
  vkDstPremultiplied :: Bool
  , -- No documentation found for Nested "PipelineColorBlendAdvancedStateCreateInfoEXT" "blendOverlap"
  vkBlendOverlap :: BlendOverlapEXT
  }
  deriving (Show, Eq)
withCStructPipelineColorBlendAdvancedStateCreateInfoEXT :: PipelineColorBlendAdvancedStateCreateInfoEXT -> (VkPipelineColorBlendAdvancedStateCreateInfoEXT -> IO a) -> IO a
withCStructPipelineColorBlendAdvancedStateCreateInfoEXT from cont = maybeWith withSomeVkStruct (vkPNext (from :: PipelineColorBlendAdvancedStateCreateInfoEXT)) (\pPNext -> cont (VkPipelineColorBlendAdvancedStateCreateInfoEXT VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT pPNext (boolToBool32 (vkSrcPremultiplied (from :: PipelineColorBlendAdvancedStateCreateInfoEXT))) (boolToBool32 (vkDstPremultiplied (from :: PipelineColorBlendAdvancedStateCreateInfoEXT))) (vkBlendOverlap (from :: PipelineColorBlendAdvancedStateCreateInfoEXT))))
fromCStructPipelineColorBlendAdvancedStateCreateInfoEXT :: VkPipelineColorBlendAdvancedStateCreateInfoEXT -> IO PipelineColorBlendAdvancedStateCreateInfoEXT
fromCStructPipelineColorBlendAdvancedStateCreateInfoEXT c = PipelineColorBlendAdvancedStateCreateInfoEXT <$> -- Univalued Member elided
                                                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPipelineColorBlendAdvancedStateCreateInfoEXT)))
                                                                                                         <*> pure (bool32ToBool (vkSrcPremultiplied (c :: VkPipelineColorBlendAdvancedStateCreateInfoEXT)))
                                                                                                         <*> pure (bool32ToBool (vkDstPremultiplied (c :: VkPipelineColorBlendAdvancedStateCreateInfoEXT)))
                                                                                                         <*> pure (vkBlendOverlap (c :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
instance Zero PipelineColorBlendAdvancedStateCreateInfoEXT where
  zero = PipelineColorBlendAdvancedStateCreateInfoEXT Nothing
                                                      False
                                                      False
                                                      zero
