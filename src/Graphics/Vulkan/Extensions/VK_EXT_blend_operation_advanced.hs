{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced
  ( VkBlendOverlapEXT(..)
  , pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT
  , pattern VK_BLEND_OVERLAP_DISJOINT_EXT
  , pattern VK_BLEND_OVERLAP_CONJOINT_EXT
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
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
  , pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION
  , pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME
  , VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , VkPipelineColorBlendAdvancedStateCreateInfoEXT(..)
  ) where

import Data.Int
  ( Int32
  )
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
import GHC.Read
  ( expectP
  , choose
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( VkAccessFlagBits(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkBlendOp(..)
  )


-- ** VkBlendOverlapEXT

-- | 
newtype VkBlendOverlapEXT = VkBlendOverlapEXT Int32
  deriving (Eq, Ord, Storable)

instance Show VkBlendOverlapEXT where
  showsPrec _ VK_BLEND_OVERLAP_UNCORRELATED_EXT = showString "VK_BLEND_OVERLAP_UNCORRELATED_EXT"
  showsPrec _ VK_BLEND_OVERLAP_DISJOINT_EXT = showString "VK_BLEND_OVERLAP_DISJOINT_EXT"
  showsPrec _ VK_BLEND_OVERLAP_CONJOINT_EXT = showString "VK_BLEND_OVERLAP_CONJOINT_EXT"
  showsPrec p (VkBlendOverlapEXT x) = showParen (p >= 11) (showString "VkBlendOverlapEXT " . showsPrec 11 x)

instance Read VkBlendOverlapEXT where
  readPrec = parens ( choose [ ("VK_BLEND_OVERLAP_UNCORRELATED_EXT", pure VK_BLEND_OVERLAP_UNCORRELATED_EXT)
                             , ("VK_BLEND_OVERLAP_DISJOINT_EXT",     pure VK_BLEND_OVERLAP_DISJOINT_EXT)
                             , ("VK_BLEND_OVERLAP_CONJOINT_EXT",     pure VK_BLEND_OVERLAP_CONJOINT_EXT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBlendOverlapEXT")
                        v <- step readPrec
                        pure (VkBlendOverlapEXT v)
                        )
                    )

-- | 
pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT :: VkBlendOverlapEXT
pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT = VkBlendOverlapEXT 0

-- | 
pattern VK_BLEND_OVERLAP_DISJOINT_EXT :: VkBlendOverlapEXT
pattern VK_BLEND_OVERLAP_DISJOINT_EXT = VkBlendOverlapEXT 1

-- | 
pattern VK_BLEND_OVERLAP_CONJOINT_EXT :: VkBlendOverlapEXT
pattern VK_BLEND_OVERLAP_CONJOINT_EXT = VkBlendOverlapEXT 2
-- | Nothing
pattern VK_BLEND_OP_ZERO_EXT :: VkBlendOp
pattern VK_BLEND_OP_ZERO_EXT = VkBlendOp 1000148000
-- | Nothing
pattern VK_BLEND_OP_SRC_EXT :: VkBlendOp
pattern VK_BLEND_OP_SRC_EXT = VkBlendOp 1000148001
-- | Nothing
pattern VK_BLEND_OP_DST_EXT :: VkBlendOp
pattern VK_BLEND_OP_DST_EXT = VkBlendOp 1000148002
-- | Nothing
pattern VK_BLEND_OP_SRC_OVER_EXT :: VkBlendOp
pattern VK_BLEND_OP_SRC_OVER_EXT = VkBlendOp 1000148003
-- | Nothing
pattern VK_BLEND_OP_DST_OVER_EXT :: VkBlendOp
pattern VK_BLEND_OP_DST_OVER_EXT = VkBlendOp 1000148004
-- | Nothing
pattern VK_BLEND_OP_SRC_IN_EXT :: VkBlendOp
pattern VK_BLEND_OP_SRC_IN_EXT = VkBlendOp 1000148005
-- | Nothing
pattern VK_BLEND_OP_DST_IN_EXT :: VkBlendOp
pattern VK_BLEND_OP_DST_IN_EXT = VkBlendOp 1000148006
-- | Nothing
pattern VK_BLEND_OP_SRC_OUT_EXT :: VkBlendOp
pattern VK_BLEND_OP_SRC_OUT_EXT = VkBlendOp 1000148007
-- | Nothing
pattern VK_BLEND_OP_DST_OUT_EXT :: VkBlendOp
pattern VK_BLEND_OP_DST_OUT_EXT = VkBlendOp 1000148008
-- | Nothing
pattern VK_BLEND_OP_SRC_ATOP_EXT :: VkBlendOp
pattern VK_BLEND_OP_SRC_ATOP_EXT = VkBlendOp 1000148009
-- | Nothing
pattern VK_BLEND_OP_DST_ATOP_EXT :: VkBlendOp
pattern VK_BLEND_OP_DST_ATOP_EXT = VkBlendOp 1000148010
-- | Nothing
pattern VK_BLEND_OP_XOR_EXT :: VkBlendOp
pattern VK_BLEND_OP_XOR_EXT = VkBlendOp 1000148011
-- | Nothing
pattern VK_BLEND_OP_MULTIPLY_EXT :: VkBlendOp
pattern VK_BLEND_OP_MULTIPLY_EXT = VkBlendOp 1000148012
-- | Nothing
pattern VK_BLEND_OP_SCREEN_EXT :: VkBlendOp
pattern VK_BLEND_OP_SCREEN_EXT = VkBlendOp 1000148013
-- | Nothing
pattern VK_BLEND_OP_OVERLAY_EXT :: VkBlendOp
pattern VK_BLEND_OP_OVERLAY_EXT = VkBlendOp 1000148014
-- | Nothing
pattern VK_BLEND_OP_DARKEN_EXT :: VkBlendOp
pattern VK_BLEND_OP_DARKEN_EXT = VkBlendOp 1000148015
-- | Nothing
pattern VK_BLEND_OP_LIGHTEN_EXT :: VkBlendOp
pattern VK_BLEND_OP_LIGHTEN_EXT = VkBlendOp 1000148016
-- | Nothing
pattern VK_BLEND_OP_COLORDODGE_EXT :: VkBlendOp
pattern VK_BLEND_OP_COLORDODGE_EXT = VkBlendOp 1000148017
-- | Nothing
pattern VK_BLEND_OP_COLORBURN_EXT :: VkBlendOp
pattern VK_BLEND_OP_COLORBURN_EXT = VkBlendOp 1000148018
-- | Nothing
pattern VK_BLEND_OP_HARDLIGHT_EXT :: VkBlendOp
pattern VK_BLEND_OP_HARDLIGHT_EXT = VkBlendOp 1000148019
-- | Nothing
pattern VK_BLEND_OP_SOFTLIGHT_EXT :: VkBlendOp
pattern VK_BLEND_OP_SOFTLIGHT_EXT = VkBlendOp 1000148020
-- | Nothing
pattern VK_BLEND_OP_DIFFERENCE_EXT :: VkBlendOp
pattern VK_BLEND_OP_DIFFERENCE_EXT = VkBlendOp 1000148021
-- | Nothing
pattern VK_BLEND_OP_EXCLUSION_EXT :: VkBlendOp
pattern VK_BLEND_OP_EXCLUSION_EXT = VkBlendOp 1000148022
-- | Nothing
pattern VK_BLEND_OP_INVERT_EXT :: VkBlendOp
pattern VK_BLEND_OP_INVERT_EXT = VkBlendOp 1000148023
-- | Nothing
pattern VK_BLEND_OP_INVERT_RGB_EXT :: VkBlendOp
pattern VK_BLEND_OP_INVERT_RGB_EXT = VkBlendOp 1000148024
-- | Nothing
pattern VK_BLEND_OP_LINEARDODGE_EXT :: VkBlendOp
pattern VK_BLEND_OP_LINEARDODGE_EXT = VkBlendOp 1000148025
-- | Nothing
pattern VK_BLEND_OP_LINEARBURN_EXT :: VkBlendOp
pattern VK_BLEND_OP_LINEARBURN_EXT = VkBlendOp 1000148026
-- | Nothing
pattern VK_BLEND_OP_VIVIDLIGHT_EXT :: VkBlendOp
pattern VK_BLEND_OP_VIVIDLIGHT_EXT = VkBlendOp 1000148027
-- | Nothing
pattern VK_BLEND_OP_LINEARLIGHT_EXT :: VkBlendOp
pattern VK_BLEND_OP_LINEARLIGHT_EXT = VkBlendOp 1000148028
-- | Nothing
pattern VK_BLEND_OP_PINLIGHT_EXT :: VkBlendOp
pattern VK_BLEND_OP_PINLIGHT_EXT = VkBlendOp 1000148029
-- | Nothing
pattern VK_BLEND_OP_HARDMIX_EXT :: VkBlendOp
pattern VK_BLEND_OP_HARDMIX_EXT = VkBlendOp 1000148030
-- | Nothing
pattern VK_BLEND_OP_HSL_HUE_EXT :: VkBlendOp
pattern VK_BLEND_OP_HSL_HUE_EXT = VkBlendOp 1000148031
-- | Nothing
pattern VK_BLEND_OP_HSL_SATURATION_EXT :: VkBlendOp
pattern VK_BLEND_OP_HSL_SATURATION_EXT = VkBlendOp 1000148032
-- | Nothing
pattern VK_BLEND_OP_HSL_COLOR_EXT :: VkBlendOp
pattern VK_BLEND_OP_HSL_COLOR_EXT = VkBlendOp 1000148033
-- | Nothing
pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT :: VkBlendOp
pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT = VkBlendOp 1000148034
-- | Nothing
pattern VK_BLEND_OP_PLUS_EXT :: VkBlendOp
pattern VK_BLEND_OP_PLUS_EXT = VkBlendOp 1000148035
-- | Nothing
pattern VK_BLEND_OP_PLUS_CLAMPED_EXT :: VkBlendOp
pattern VK_BLEND_OP_PLUS_CLAMPED_EXT = VkBlendOp 1000148036
-- | Nothing
pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT :: VkBlendOp
pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT = VkBlendOp 1000148037
-- | Nothing
pattern VK_BLEND_OP_PLUS_DARKER_EXT :: VkBlendOp
pattern VK_BLEND_OP_PLUS_DARKER_EXT = VkBlendOp 1000148038
-- | Nothing
pattern VK_BLEND_OP_MINUS_EXT :: VkBlendOp
pattern VK_BLEND_OP_MINUS_EXT = VkBlendOp 1000148039
-- | Nothing
pattern VK_BLEND_OP_MINUS_CLAMPED_EXT :: VkBlendOp
pattern VK_BLEND_OP_MINUS_CLAMPED_EXT = VkBlendOp 1000148040
-- | Nothing
pattern VK_BLEND_OP_CONTRAST_EXT :: VkBlendOp
pattern VK_BLEND_OP_CONTRAST_EXT = VkBlendOp 1000148041
-- | Nothing
pattern VK_BLEND_OP_INVERT_OVG_EXT :: VkBlendOp
pattern VK_BLEND_OP_INVERT_OVG_EXT = VkBlendOp 1000148042
-- | Nothing
pattern VK_BLEND_OP_RED_EXT :: VkBlendOp
pattern VK_BLEND_OP_RED_EXT = VkBlendOp 1000148043
-- | Nothing
pattern VK_BLEND_OP_GREEN_EXT :: VkBlendOp
pattern VK_BLEND_OP_GREEN_EXT = VkBlendOp 1000148044
-- | Nothing
pattern VK_BLEND_OP_BLUE_EXT :: VkBlendOp
pattern VK_BLEND_OP_BLUE_EXT = VkBlendOp 1000148045
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT = VkStructureType 1000148000
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT = VkStructureType 1000148001
-- | Nothing
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT = VkStructureType 1000148002
-- | Nothing
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT = VkAccessFlagBits 0x00080000
pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION :: Integral a => a
pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2
pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME = "VK_EXT_blend_operation_advanced"
-- | TODO: Struct comments
data VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT = VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkAdvancedBlendCoherentOperations :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                               <*> peek (ptr `plusPtr` 8)
                                                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkAdvancedBlendCoherentOperations (poked :: VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT))
-- | TODO: Struct comments
data VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT = VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkAdvancedBlendMaxColorAttachments :: Word32
  , vkAdvancedBlendIndependentBlend :: VkBool32
  , vkAdvancedBlendNonPremultipliedSrcColor :: VkBool32
  , vkAdvancedBlendNonPremultipliedDstColor :: VkBool32
  , vkAdvancedBlendCorrelatedOverlap :: VkBool32
  , vkAdvancedBlendAllOperations :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT <$> peek (ptr `plusPtr` 0)
                                                                 <*> peek (ptr `plusPtr` 8)
                                                                 <*> peek (ptr `plusPtr` 16)
                                                                 <*> peek (ptr `plusPtr` 20)
                                                                 <*> peek (ptr `plusPtr` 24)
                                                                 <*> peek (ptr `plusPtr` 28)
                                                                 <*> peek (ptr `plusPtr` 32)
                                                                 <*> peek (ptr `plusPtr` 36)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 16) (vkAdvancedBlendMaxColorAttachments (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 20) (vkAdvancedBlendIndependentBlend (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 24) (vkAdvancedBlendNonPremultipliedSrcColor (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 28) (vkAdvancedBlendNonPremultipliedDstColor (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 32) (vkAdvancedBlendCorrelatedOverlap (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
                *> poke (ptr `plusPtr` 36) (vkAdvancedBlendAllOperations (poked :: VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
-- | TODO: Struct comments
data VkPipelineColorBlendAdvancedStateCreateInfoEXT = VkPipelineColorBlendAdvancedStateCreateInfoEXT
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkSrcPremultiplied :: VkBool32
  , vkDstPremultiplied :: VkBool32
  , vkBlendOverlap :: VkBlendOverlapEXT
  }
  deriving (Eq, Show)

instance Storable VkPipelineColorBlendAdvancedStateCreateInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPipelineColorBlendAdvancedStateCreateInfoEXT <$> peek (ptr `plusPtr` 0)
                                                            <*> peek (ptr `plusPtr` 8)
                                                            <*> peek (ptr `plusPtr` 16)
                                                            <*> peek (ptr `plusPtr` 20)
                                                            <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkSrcPremultiplied (poked :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 20) (vkDstPremultiplied (poked :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
                *> poke (ptr `plusPtr` 24) (vkBlendOverlap (poked :: VkPipelineColorBlendAdvancedStateCreateInfoEXT))
