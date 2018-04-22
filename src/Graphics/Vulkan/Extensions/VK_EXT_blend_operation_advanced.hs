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
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
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

-- | VkBlendOverlapEXT - Enumerant specifying the blend overlap parameter
--
-- = Description
--
-- -   @VK_BLEND_OVERLAP_UNCORRELATED_EXT@ specifies that there is no
--     correlation between the source and destination coverage.
--
-- -   @VK_BLEND_OVERLAP_CONJOINT_EXT@ specifies that the source and
--     destination coverage are considered to have maximal overlap.
--
-- -   @VK_BLEND_OVERLAP_DISJOINT_EXT@ specifies that the source and
--     destination coverage are considered to have minimal overlap.
--
-- +-------------------------------------+--------------------------------------------------------------------------------------+
-- | Overlap Mode                        | Weighting Equations                                                                  |
-- +=====================================+======================================================================================+
-- | @VK_BLEND_OVERLAP_UNCORRELATED_EXT@ | \[                                              \begin{aligned}                      |
-- |                                     |                                                 p_0(A_s,A_d) & = A_sA_d \\           |
-- |                                     |                                                 p_1(A_s,A_d) & = A_s(1-A_d) \\       |
-- |                                     |                                                 p_2(A_s,A_d) & = A_d(1-A_s) \\       |
-- |                                     |                                               \end{aligned}\]                        |
-- +-------------------------------------+--------------------------------------------------------------------------------------+
-- | @VK_BLEND_OVERLAP_CONJOINT_EXT@     | \[                                              \begin{aligned}                      |
-- |                                     |                                                 p_0(A_s,A_d) & = min(A_s,A_d) \\     |
-- |                                     |                                                 p_1(A_s,A_d) & = max(A_s-A_d,0) \\   |
-- |                                     |                                                 p_2(A_s,A_d) & = max(A_d-A_s,0) \\   |
-- |                                     |                                               \end{aligned}\]                        |
-- +-------------------------------------+--------------------------------------------------------------------------------------+
-- | @VK_BLEND_OVERLAP_DISJOINT_EXT@     | \[                                              \begin{aligned}                      |
-- |                                     |                                                 p_0(A_s,A_d) & = max(A_s+A_d-1,0) \\ |
-- |                                     |                                                 p_1(A_s,A_d) & = min(A_s,1-A_d) \\   |
-- |                                     |                                                 p_2(A_s,A_d) & = min(A_d,1-A_s) \\   |
-- |                                     |                                               \end{aligned}\]                        |
-- +-------------------------------------+--------------------------------------------------------------------------------------+
--
-- Advanced Blend Overlap Modes
--
-- = See Also
--
-- 'VkPipelineColorBlendAdvancedStateCreateInfoEXT'
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

-- No documentation found for Nested "VkBlendOverlapEXT" "VK_BLEND_OVERLAP_UNCORRELATED_EXT"
pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT :: VkBlendOverlapEXT
pattern VK_BLEND_OVERLAP_UNCORRELATED_EXT = VkBlendOverlapEXT 0

-- No documentation found for Nested "VkBlendOverlapEXT" "VK_BLEND_OVERLAP_DISJOINT_EXT"
pattern VK_BLEND_OVERLAP_DISJOINT_EXT :: VkBlendOverlapEXT
pattern VK_BLEND_OVERLAP_DISJOINT_EXT = VkBlendOverlapEXT 1

-- No documentation found for Nested "VkBlendOverlapEXT" "VK_BLEND_OVERLAP_CONJOINT_EXT"
pattern VK_BLEND_OVERLAP_CONJOINT_EXT :: VkBlendOverlapEXT
pattern VK_BLEND_OVERLAP_CONJOINT_EXT = VkBlendOverlapEXT 2
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_ZERO_EXT"
pattern VK_BLEND_OP_ZERO_EXT :: VkBlendOp
pattern VK_BLEND_OP_ZERO_EXT = VkBlendOp 1000148000
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SRC_EXT"
pattern VK_BLEND_OP_SRC_EXT :: VkBlendOp
pattern VK_BLEND_OP_SRC_EXT = VkBlendOp 1000148001
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DST_EXT"
pattern VK_BLEND_OP_DST_EXT :: VkBlendOp
pattern VK_BLEND_OP_DST_EXT = VkBlendOp 1000148002
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SRC_OVER_EXT"
pattern VK_BLEND_OP_SRC_OVER_EXT :: VkBlendOp
pattern VK_BLEND_OP_SRC_OVER_EXT = VkBlendOp 1000148003
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DST_OVER_EXT"
pattern VK_BLEND_OP_DST_OVER_EXT :: VkBlendOp
pattern VK_BLEND_OP_DST_OVER_EXT = VkBlendOp 1000148004
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SRC_IN_EXT"
pattern VK_BLEND_OP_SRC_IN_EXT :: VkBlendOp
pattern VK_BLEND_OP_SRC_IN_EXT = VkBlendOp 1000148005
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DST_IN_EXT"
pattern VK_BLEND_OP_DST_IN_EXT :: VkBlendOp
pattern VK_BLEND_OP_DST_IN_EXT = VkBlendOp 1000148006
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SRC_OUT_EXT"
pattern VK_BLEND_OP_SRC_OUT_EXT :: VkBlendOp
pattern VK_BLEND_OP_SRC_OUT_EXT = VkBlendOp 1000148007
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DST_OUT_EXT"
pattern VK_BLEND_OP_DST_OUT_EXT :: VkBlendOp
pattern VK_BLEND_OP_DST_OUT_EXT = VkBlendOp 1000148008
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SRC_ATOP_EXT"
pattern VK_BLEND_OP_SRC_ATOP_EXT :: VkBlendOp
pattern VK_BLEND_OP_SRC_ATOP_EXT = VkBlendOp 1000148009
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DST_ATOP_EXT"
pattern VK_BLEND_OP_DST_ATOP_EXT :: VkBlendOp
pattern VK_BLEND_OP_DST_ATOP_EXT = VkBlendOp 1000148010
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_XOR_EXT"
pattern VK_BLEND_OP_XOR_EXT :: VkBlendOp
pattern VK_BLEND_OP_XOR_EXT = VkBlendOp 1000148011
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_MULTIPLY_EXT"
pattern VK_BLEND_OP_MULTIPLY_EXT :: VkBlendOp
pattern VK_BLEND_OP_MULTIPLY_EXT = VkBlendOp 1000148012
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SCREEN_EXT"
pattern VK_BLEND_OP_SCREEN_EXT :: VkBlendOp
pattern VK_BLEND_OP_SCREEN_EXT = VkBlendOp 1000148013
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_OVERLAY_EXT"
pattern VK_BLEND_OP_OVERLAY_EXT :: VkBlendOp
pattern VK_BLEND_OP_OVERLAY_EXT = VkBlendOp 1000148014
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DARKEN_EXT"
pattern VK_BLEND_OP_DARKEN_EXT :: VkBlendOp
pattern VK_BLEND_OP_DARKEN_EXT = VkBlendOp 1000148015
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_LIGHTEN_EXT"
pattern VK_BLEND_OP_LIGHTEN_EXT :: VkBlendOp
pattern VK_BLEND_OP_LIGHTEN_EXT = VkBlendOp 1000148016
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_COLORDODGE_EXT"
pattern VK_BLEND_OP_COLORDODGE_EXT :: VkBlendOp
pattern VK_BLEND_OP_COLORDODGE_EXT = VkBlendOp 1000148017
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_COLORBURN_EXT"
pattern VK_BLEND_OP_COLORBURN_EXT :: VkBlendOp
pattern VK_BLEND_OP_COLORBURN_EXT = VkBlendOp 1000148018
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HARDLIGHT_EXT"
pattern VK_BLEND_OP_HARDLIGHT_EXT :: VkBlendOp
pattern VK_BLEND_OP_HARDLIGHT_EXT = VkBlendOp 1000148019
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SOFTLIGHT_EXT"
pattern VK_BLEND_OP_SOFTLIGHT_EXT :: VkBlendOp
pattern VK_BLEND_OP_SOFTLIGHT_EXT = VkBlendOp 1000148020
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DIFFERENCE_EXT"
pattern VK_BLEND_OP_DIFFERENCE_EXT :: VkBlendOp
pattern VK_BLEND_OP_DIFFERENCE_EXT = VkBlendOp 1000148021
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_EXCLUSION_EXT"
pattern VK_BLEND_OP_EXCLUSION_EXT :: VkBlendOp
pattern VK_BLEND_OP_EXCLUSION_EXT = VkBlendOp 1000148022
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_INVERT_EXT"
pattern VK_BLEND_OP_INVERT_EXT :: VkBlendOp
pattern VK_BLEND_OP_INVERT_EXT = VkBlendOp 1000148023
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_INVERT_RGB_EXT"
pattern VK_BLEND_OP_INVERT_RGB_EXT :: VkBlendOp
pattern VK_BLEND_OP_INVERT_RGB_EXT = VkBlendOp 1000148024
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_LINEARDODGE_EXT"
pattern VK_BLEND_OP_LINEARDODGE_EXT :: VkBlendOp
pattern VK_BLEND_OP_LINEARDODGE_EXT = VkBlendOp 1000148025
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_LINEARBURN_EXT"
pattern VK_BLEND_OP_LINEARBURN_EXT :: VkBlendOp
pattern VK_BLEND_OP_LINEARBURN_EXT = VkBlendOp 1000148026
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_VIVIDLIGHT_EXT"
pattern VK_BLEND_OP_VIVIDLIGHT_EXT :: VkBlendOp
pattern VK_BLEND_OP_VIVIDLIGHT_EXT = VkBlendOp 1000148027
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_LINEARLIGHT_EXT"
pattern VK_BLEND_OP_LINEARLIGHT_EXT :: VkBlendOp
pattern VK_BLEND_OP_LINEARLIGHT_EXT = VkBlendOp 1000148028
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_PINLIGHT_EXT"
pattern VK_BLEND_OP_PINLIGHT_EXT :: VkBlendOp
pattern VK_BLEND_OP_PINLIGHT_EXT = VkBlendOp 1000148029
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HARDMIX_EXT"
pattern VK_BLEND_OP_HARDMIX_EXT :: VkBlendOp
pattern VK_BLEND_OP_HARDMIX_EXT = VkBlendOp 1000148030
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HSL_HUE_EXT"
pattern VK_BLEND_OP_HSL_HUE_EXT :: VkBlendOp
pattern VK_BLEND_OP_HSL_HUE_EXT = VkBlendOp 1000148031
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HSL_SATURATION_EXT"
pattern VK_BLEND_OP_HSL_SATURATION_EXT :: VkBlendOp
pattern VK_BLEND_OP_HSL_SATURATION_EXT = VkBlendOp 1000148032
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HSL_COLOR_EXT"
pattern VK_BLEND_OP_HSL_COLOR_EXT :: VkBlendOp
pattern VK_BLEND_OP_HSL_COLOR_EXT = VkBlendOp 1000148033
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HSL_LUMINOSITY_EXT"
pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT :: VkBlendOp
pattern VK_BLEND_OP_HSL_LUMINOSITY_EXT = VkBlendOp 1000148034
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_PLUS_EXT"
pattern VK_BLEND_OP_PLUS_EXT :: VkBlendOp
pattern VK_BLEND_OP_PLUS_EXT = VkBlendOp 1000148035
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_PLUS_CLAMPED_EXT"
pattern VK_BLEND_OP_PLUS_CLAMPED_EXT :: VkBlendOp
pattern VK_BLEND_OP_PLUS_CLAMPED_EXT = VkBlendOp 1000148036
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT"
pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT :: VkBlendOp
pattern VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT = VkBlendOp 1000148037
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_PLUS_DARKER_EXT"
pattern VK_BLEND_OP_PLUS_DARKER_EXT :: VkBlendOp
pattern VK_BLEND_OP_PLUS_DARKER_EXT = VkBlendOp 1000148038
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_MINUS_EXT"
pattern VK_BLEND_OP_MINUS_EXT :: VkBlendOp
pattern VK_BLEND_OP_MINUS_EXT = VkBlendOp 1000148039
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_MINUS_CLAMPED_EXT"
pattern VK_BLEND_OP_MINUS_CLAMPED_EXT :: VkBlendOp
pattern VK_BLEND_OP_MINUS_CLAMPED_EXT = VkBlendOp 1000148040
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_CONTRAST_EXT"
pattern VK_BLEND_OP_CONTRAST_EXT :: VkBlendOp
pattern VK_BLEND_OP_CONTRAST_EXT = VkBlendOp 1000148041
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_INVERT_OVG_EXT"
pattern VK_BLEND_OP_INVERT_OVG_EXT :: VkBlendOp
pattern VK_BLEND_OP_INVERT_OVG_EXT = VkBlendOp 1000148042
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_RED_EXT"
pattern VK_BLEND_OP_RED_EXT :: VkBlendOp
pattern VK_BLEND_OP_RED_EXT = VkBlendOp 1000148043
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_GREEN_EXT"
pattern VK_BLEND_OP_GREEN_EXT :: VkBlendOp
pattern VK_BLEND_OP_GREEN_EXT = VkBlendOp 1000148044
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_BLUE_EXT"
pattern VK_BLEND_OP_BLUE_EXT :: VkBlendOp
pattern VK_BLEND_OP_BLUE_EXT = VkBlendOp 1000148045
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT = VkStructureType 1000148000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT = VkStructureType 1000148001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT = VkStructureType 1000148002
-- No documentation found for Nested "VkAccessFlagBits" "VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT"
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT :: VkAccessFlagBits
pattern VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT = VkAccessFlagBits 0x00080000
-- No documentation found for TopLevel "VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION"
pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION :: Integral a => a
pattern VK_EXT_BLEND_OPERATION_ADVANCED_SPEC_VERSION = 2
-- No documentation found for TopLevel "VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME"
pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_BLEND_OPERATION_ADVANCED_EXTENSION_NAME = "VK_EXT_blend_operation_advanced"
-- | VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT - Structure describing
-- advanced blending features that can be supported by an implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT@
-- structure describe the following features:
--
-- = Description
--
-- -   @advancedBlendCoherentOperations@ specifies whether blending using
--     [advanced blend
--     operations](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-blend-advanced)
--     is guaranteed to execute atomically and in [primitive
--     order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#drawing-primitive-order).
--     If this is @VK_TRUE@,
--     @VK_ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT@ is treated the
--     same as @VK_ACCESS_COLOR_ATTACHMENT_READ_BIT@, and advanced blending
--     needs no additional synchronization over basic blending. If this is
--     @VK_FALSE@, then memory dependencies are required to guarantee order
--     between two advanced blending operations that occur on the same
--     sample.
--
-- If the @VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT@ structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- @VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT@ /can/ also be used
-- in @pNext@ chain of 'Graphics.Vulkan.Core10.Device.VkDeviceCreateInfo'
-- to enable the features.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT = VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT" "advancedBlendCoherentOperations"
  vkAdvancedBlendCoherentOperations :: VkBool32
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
-- | VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT - Structure
-- describing advanced blending limits that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the @VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT@
-- structure describe the following implementation-dependent limits:
--
-- = Description
--
-- -   @advancedBlendMaxColorAttachments@ is one greater than the highest
--     color attachment index that /can/ be used in a subpass, for a
--     pipeline that uses an [advanced blend
--     operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-blend-advanced).
--
-- -   @advancedBlendIndependentBlend@ specifies whether advanced blend
--     operations /can/ vary per-attachment.
--
-- -   @advancedBlendNonPremultipliedSrcColor@ specifies whether the source
--     color /can/ be treated as non-premultiplied. If this is @VK_FALSE@,
--     then
--     'VkPipelineColorBlendAdvancedStateCreateInfoEXT'::@srcPremultiplied@
--     /must/ be @VK_TRUE@.
--
-- -   @advancedBlendNonPremultipliedDstColor@ specifies whether the
--     destination color /can/ be treated as non-premultiplied. If this is
--     @VK_FALSE@, then
--     'VkPipelineColorBlendAdvancedStateCreateInfoEXT'::@dstPremultiplied@
--     /must/ be @VK_TRUE@.
--
-- -   @advancedBlendCorrelatedOverlap@ specifies whether the overlap mode
--     /can/ be treated as correlated. If this is @VK_FALSE@, then
--     'VkPipelineColorBlendAdvancedStateCreateInfoEXT'::@blendOverlap@
--     /must/ be @VK_BLEND_OVERLAP_UNCORRELATED_EXT@.
--
-- -   @advancedBlendAllOperations@ specifies whether all advanced blend
--     operation enums are supported. See the valid usage of
--     'Graphics.Vulkan.Core10.Pipeline.VkPipelineColorBlendAttachmentState'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT@
--
-- If the @VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT@ structure
-- is included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT = VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendMaxColorAttachments"
  vkAdvancedBlendMaxColorAttachments :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendIndependentBlend"
  vkAdvancedBlendIndependentBlend :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendNonPremultipliedSrcColor"
  vkAdvancedBlendNonPremultipliedSrcColor :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendNonPremultipliedDstColor"
  vkAdvancedBlendNonPremultipliedDstColor :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendCorrelatedOverlap"
  vkAdvancedBlendCorrelatedOverlap :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT" "advancedBlendAllOperations"
  vkAdvancedBlendAllOperations :: VkBool32
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
-- | VkPipelineColorBlendAdvancedStateCreateInfoEXT - Structure specifying
-- parameters that affect advanced blend operations
--
-- = Description
--
-- If this structure is not present, @srcPremultiplied@ and
-- @dstPremultiplied@ are both considered to be @VK_TRUE@, and
-- @blendOverlap@ is considered to be @VK_BLEND_OVERLAP_UNCORRELATED_EXT@.
--
-- == Valid Usage
--
-- -   If the [non-premultiplied source
--     color](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-advancedBlendNonPremultipliedSrcColor)
--     property is not supported, @srcPremultiplied@ /must/ be @VK_TRUE@
--
-- -   If the [non-premultiplied destination
--     color](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-advancedBlendNonPremultipliedDstColor)
--     property is not supported, @dstPremultiplied@ /must/ be @VK_TRUE@
--
-- -   If the [correlated
--     overlap](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-limits-advancedBlendCorrelatedOverlap)
--     property is not supported, @blendOverlap@ /must/ be
--     @VK_BLEND_OVERLAP_UNCORRELATED_EXT@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT@
--
-- -   @blendOverlap@ /must/ be a valid 'VkBlendOverlapEXT' value
--
-- = See Also
--
-- 'VkBlendOverlapEXT', @VkBool32@,
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPipelineColorBlendAdvancedStateCreateInfoEXT = VkPipelineColorBlendAdvancedStateCreateInfoEXT
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @srcPremultiplied@ specifies whether the source color of the blend
  -- operation is treated as premultiplied.
  vkSrcPremultiplied :: VkBool32
  , -- | @dstPremultiplied@ specifies whether the destination color of the blend
  -- operation is treated as premultiplied.
  vkDstPremultiplied :: VkBool32
  , -- | @blendOverlap@ is a 'VkBlendOverlapEXT' value specifying how the source
  -- and destination sample’s coverage is correlated.
  vkBlendOverlap :: VkBlendOverlapEXT
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
