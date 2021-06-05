{-# language CPP #-}
-- No documentation found for Chapter "BlendOp"
module Vulkan.Core10.Enums.BlendOp  (BlendOp( BLEND_OP_ADD
                                            , BLEND_OP_SUBTRACT
                                            , BLEND_OP_REVERSE_SUBTRACT
                                            , BLEND_OP_MIN
                                            , BLEND_OP_MAX
                                            , BLEND_OP_BLUE_EXT
                                            , BLEND_OP_GREEN_EXT
                                            , BLEND_OP_RED_EXT
                                            , BLEND_OP_INVERT_OVG_EXT
                                            , BLEND_OP_CONTRAST_EXT
                                            , BLEND_OP_MINUS_CLAMPED_EXT
                                            , BLEND_OP_MINUS_EXT
                                            , BLEND_OP_PLUS_DARKER_EXT
                                            , BLEND_OP_PLUS_CLAMPED_ALPHA_EXT
                                            , BLEND_OP_PLUS_CLAMPED_EXT
                                            , BLEND_OP_PLUS_EXT
                                            , BLEND_OP_HSL_LUMINOSITY_EXT
                                            , BLEND_OP_HSL_COLOR_EXT
                                            , BLEND_OP_HSL_SATURATION_EXT
                                            , BLEND_OP_HSL_HUE_EXT
                                            , BLEND_OP_HARDMIX_EXT
                                            , BLEND_OP_PINLIGHT_EXT
                                            , BLEND_OP_LINEARLIGHT_EXT
                                            , BLEND_OP_VIVIDLIGHT_EXT
                                            , BLEND_OP_LINEARBURN_EXT
                                            , BLEND_OP_LINEARDODGE_EXT
                                            , BLEND_OP_INVERT_RGB_EXT
                                            , BLEND_OP_INVERT_EXT
                                            , BLEND_OP_EXCLUSION_EXT
                                            , BLEND_OP_DIFFERENCE_EXT
                                            , BLEND_OP_SOFTLIGHT_EXT
                                            , BLEND_OP_HARDLIGHT_EXT
                                            , BLEND_OP_COLORBURN_EXT
                                            , BLEND_OP_COLORDODGE_EXT
                                            , BLEND_OP_LIGHTEN_EXT
                                            , BLEND_OP_DARKEN_EXT
                                            , BLEND_OP_OVERLAY_EXT
                                            , BLEND_OP_SCREEN_EXT
                                            , BLEND_OP_MULTIPLY_EXT
                                            , BLEND_OP_XOR_EXT
                                            , BLEND_OP_DST_ATOP_EXT
                                            , BLEND_OP_SRC_ATOP_EXT
                                            , BLEND_OP_DST_OUT_EXT
                                            , BLEND_OP_SRC_OUT_EXT
                                            , BLEND_OP_DST_IN_EXT
                                            , BLEND_OP_SRC_IN_EXT
                                            , BLEND_OP_DST_OVER_EXT
                                            , BLEND_OP_SRC_OVER_EXT
                                            , BLEND_OP_DST_EXT
                                            , BLEND_OP_SRC_EXT
                                            , BLEND_OP_ZERO_EXT
                                            , ..
                                            )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkBlendOp - Framebuffer blending operations
--
-- = Description
--
-- The semantics of the basic blend operations are described in the table
-- below:
--
-- +-------------------------------+--------------------+-----------------+
-- | 'BlendOp'                     | RGB Components     | Alpha Component |
-- +===============================+====================+=================+
-- | 'BLEND_OP_ADD'                | R = Rs0 × Sr + Rd  | A = As0 × Sa +  |
-- |                               | × Dr               | Ad × Da         |
-- |                               | G = Gs0 × Sg + Gd  |                 |
-- |                               | × Dg               |                 |
-- |                               | B = Bs0 × Sb + Bd  |                 |
-- |                               | × Db               |                 |
-- +-------------------------------+--------------------+-----------------+
-- | 'BLEND_OP_SUBTRACT'           | R = Rs0 × Sr - Rd  | A = As0 × Sa -  |
-- |                               | × Dr               | Ad × Da         |
-- |                               | G = Gs0 × Sg - Gd  |                 |
-- |                               | × Dg               |                 |
-- |                               | B = Bs0 × Sb - Bd  |                 |
-- |                               | × Db               |                 |
-- +-------------------------------+--------------------+-----------------+
-- | 'BLEND_OP_REVERSE_SUBTRACT'   | R = Rd × Dr - Rs0  | A = Ad × Da -   |
-- |                               | × Sr               | As0 × Sa        |
-- |                               | G = Gd × Dg - Gs0  |                 |
-- |                               | × Sg               |                 |
-- |                               | B = Bd × Db - Bs0  |                 |
-- |                               | × Sb               |                 |
-- +-------------------------------+--------------------+-----------------+
-- | 'BLEND_OP_MIN'                | R = min(Rs0,Rd)    | A = min(As0,Ad) |
-- |                               | G = min(Gs0,Gd)    |                 |
-- |                               | B = min(Bs0,Bd)    |                 |
-- +-------------------------------+--------------------+-----------------+
-- | 'BLEND_OP_MAX'                | R = max(Rs0,Rd)    | A = max(As0,Ad) |
-- |                               | G = max(Gs0,Gd)    |                 |
-- |                               | B = max(Bs0,Bd)    |                 |
-- +-------------------------------+--------------------+-----------------+
--
-- Basic Blend Operations
--
-- In this table, the following conventions are used:
--
-- -   Rs0, Gs0, Bs0 and As0 represent the first source color R, G, B, and
--     A components, respectively.
--
-- -   Rd, Gd, Bd and Ad represent the R, G, B, and A components of the
--     destination color. That is, the color currently in the corresponding
--     color attachment for this fragment\/sample.
--
-- -   Sr, Sg, Sb and Sa represent the source blend factor R, G, B, and A
--     components, respectively.
--
-- -   Dr, Dg, Db and Da represent the destination blend factor R, G, B,
--     and A components, respectively.
--
-- The blending operation produces a new set of values R, G, B and A, which
-- are written to the framebuffer attachment. If blending is not enabled
-- for this attachment, then R, G, B and A are assigned Rs0, Gs0, Bs0 and
-- As0, respectively.
--
-- If the color attachment is fixed-point, the components of the source and
-- destination values and blend factors are each clamped to [0,1] or [-1,1]
-- respectively for an unsigned normalized or signed normalized color
-- attachment prior to evaluating the blend operations. If the color
-- attachment is floating-point, no clamping occurs.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'
newtype BlendOp = BlendOp Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_ADD"
pattern BLEND_OP_ADD                    = BlendOp 0
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SUBTRACT"
pattern BLEND_OP_SUBTRACT               = BlendOp 1
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_REVERSE_SUBTRACT"
pattern BLEND_OP_REVERSE_SUBTRACT       = BlendOp 2
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_MIN"
pattern BLEND_OP_MIN                    = BlendOp 3
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_MAX"
pattern BLEND_OP_MAX                    = BlendOp 4
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_BLUE_EXT"
pattern BLEND_OP_BLUE_EXT               = BlendOp 1000148045
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_GREEN_EXT"
pattern BLEND_OP_GREEN_EXT              = BlendOp 1000148044
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_RED_EXT"
pattern BLEND_OP_RED_EXT                = BlendOp 1000148043
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_INVERT_OVG_EXT"
pattern BLEND_OP_INVERT_OVG_EXT         = BlendOp 1000148042
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_CONTRAST_EXT"
pattern BLEND_OP_CONTRAST_EXT           = BlendOp 1000148041
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_MINUS_CLAMPED_EXT"
pattern BLEND_OP_MINUS_CLAMPED_EXT      = BlendOp 1000148040
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_MINUS_EXT"
pattern BLEND_OP_MINUS_EXT              = BlendOp 1000148039
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_PLUS_DARKER_EXT"
pattern BLEND_OP_PLUS_DARKER_EXT        = BlendOp 1000148038
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_PLUS_CLAMPED_ALPHA_EXT"
pattern BLEND_OP_PLUS_CLAMPED_ALPHA_EXT = BlendOp 1000148037
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_PLUS_CLAMPED_EXT"
pattern BLEND_OP_PLUS_CLAMPED_EXT       = BlendOp 1000148036
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_PLUS_EXT"
pattern BLEND_OP_PLUS_EXT               = BlendOp 1000148035
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HSL_LUMINOSITY_EXT"
pattern BLEND_OP_HSL_LUMINOSITY_EXT     = BlendOp 1000148034
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HSL_COLOR_EXT"
pattern BLEND_OP_HSL_COLOR_EXT          = BlendOp 1000148033
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HSL_SATURATION_EXT"
pattern BLEND_OP_HSL_SATURATION_EXT     = BlendOp 1000148032
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HSL_HUE_EXT"
pattern BLEND_OP_HSL_HUE_EXT            = BlendOp 1000148031
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HARDMIX_EXT"
pattern BLEND_OP_HARDMIX_EXT            = BlendOp 1000148030
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_PINLIGHT_EXT"
pattern BLEND_OP_PINLIGHT_EXT           = BlendOp 1000148029
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_LINEARLIGHT_EXT"
pattern BLEND_OP_LINEARLIGHT_EXT        = BlendOp 1000148028
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_VIVIDLIGHT_EXT"
pattern BLEND_OP_VIVIDLIGHT_EXT         = BlendOp 1000148027
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_LINEARBURN_EXT"
pattern BLEND_OP_LINEARBURN_EXT         = BlendOp 1000148026
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_LINEARDODGE_EXT"
pattern BLEND_OP_LINEARDODGE_EXT        = BlendOp 1000148025
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_INVERT_RGB_EXT"
pattern BLEND_OP_INVERT_RGB_EXT         = BlendOp 1000148024
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_INVERT_EXT"
pattern BLEND_OP_INVERT_EXT             = BlendOp 1000148023
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_EXCLUSION_EXT"
pattern BLEND_OP_EXCLUSION_EXT          = BlendOp 1000148022
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DIFFERENCE_EXT"
pattern BLEND_OP_DIFFERENCE_EXT         = BlendOp 1000148021
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SOFTLIGHT_EXT"
pattern BLEND_OP_SOFTLIGHT_EXT          = BlendOp 1000148020
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_HARDLIGHT_EXT"
pattern BLEND_OP_HARDLIGHT_EXT          = BlendOp 1000148019
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_COLORBURN_EXT"
pattern BLEND_OP_COLORBURN_EXT          = BlendOp 1000148018
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_COLORDODGE_EXT"
pattern BLEND_OP_COLORDODGE_EXT         = BlendOp 1000148017
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_LIGHTEN_EXT"
pattern BLEND_OP_LIGHTEN_EXT            = BlendOp 1000148016
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DARKEN_EXT"
pattern BLEND_OP_DARKEN_EXT             = BlendOp 1000148015
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_OVERLAY_EXT"
pattern BLEND_OP_OVERLAY_EXT            = BlendOp 1000148014
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SCREEN_EXT"
pattern BLEND_OP_SCREEN_EXT             = BlendOp 1000148013
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_MULTIPLY_EXT"
pattern BLEND_OP_MULTIPLY_EXT           = BlendOp 1000148012
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_XOR_EXT"
pattern BLEND_OP_XOR_EXT                = BlendOp 1000148011
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DST_ATOP_EXT"
pattern BLEND_OP_DST_ATOP_EXT           = BlendOp 1000148010
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SRC_ATOP_EXT"
pattern BLEND_OP_SRC_ATOP_EXT           = BlendOp 1000148009
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DST_OUT_EXT"
pattern BLEND_OP_DST_OUT_EXT            = BlendOp 1000148008
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SRC_OUT_EXT"
pattern BLEND_OP_SRC_OUT_EXT            = BlendOp 1000148007
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DST_IN_EXT"
pattern BLEND_OP_DST_IN_EXT             = BlendOp 1000148006
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SRC_IN_EXT"
pattern BLEND_OP_SRC_IN_EXT             = BlendOp 1000148005
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DST_OVER_EXT"
pattern BLEND_OP_DST_OVER_EXT           = BlendOp 1000148004
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SRC_OVER_EXT"
pattern BLEND_OP_SRC_OVER_EXT           = BlendOp 1000148003
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_DST_EXT"
pattern BLEND_OP_DST_EXT                = BlendOp 1000148002
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_SRC_EXT"
pattern BLEND_OP_SRC_EXT                = BlendOp 1000148001
-- No documentation found for Nested "VkBlendOp" "VK_BLEND_OP_ZERO_EXT"
pattern BLEND_OP_ZERO_EXT               = BlendOp 1000148000
{-# complete BLEND_OP_ADD,
             BLEND_OP_SUBTRACT,
             BLEND_OP_REVERSE_SUBTRACT,
             BLEND_OP_MIN,
             BLEND_OP_MAX,
             BLEND_OP_BLUE_EXT,
             BLEND_OP_GREEN_EXT,
             BLEND_OP_RED_EXT,
             BLEND_OP_INVERT_OVG_EXT,
             BLEND_OP_CONTRAST_EXT,
             BLEND_OP_MINUS_CLAMPED_EXT,
             BLEND_OP_MINUS_EXT,
             BLEND_OP_PLUS_DARKER_EXT,
             BLEND_OP_PLUS_CLAMPED_ALPHA_EXT,
             BLEND_OP_PLUS_CLAMPED_EXT,
             BLEND_OP_PLUS_EXT,
             BLEND_OP_HSL_LUMINOSITY_EXT,
             BLEND_OP_HSL_COLOR_EXT,
             BLEND_OP_HSL_SATURATION_EXT,
             BLEND_OP_HSL_HUE_EXT,
             BLEND_OP_HARDMIX_EXT,
             BLEND_OP_PINLIGHT_EXT,
             BLEND_OP_LINEARLIGHT_EXT,
             BLEND_OP_VIVIDLIGHT_EXT,
             BLEND_OP_LINEARBURN_EXT,
             BLEND_OP_LINEARDODGE_EXT,
             BLEND_OP_INVERT_RGB_EXT,
             BLEND_OP_INVERT_EXT,
             BLEND_OP_EXCLUSION_EXT,
             BLEND_OP_DIFFERENCE_EXT,
             BLEND_OP_SOFTLIGHT_EXT,
             BLEND_OP_HARDLIGHT_EXT,
             BLEND_OP_COLORBURN_EXT,
             BLEND_OP_COLORDODGE_EXT,
             BLEND_OP_LIGHTEN_EXT,
             BLEND_OP_DARKEN_EXT,
             BLEND_OP_OVERLAY_EXT,
             BLEND_OP_SCREEN_EXT,
             BLEND_OP_MULTIPLY_EXT,
             BLEND_OP_XOR_EXT,
             BLEND_OP_DST_ATOP_EXT,
             BLEND_OP_SRC_ATOP_EXT,
             BLEND_OP_DST_OUT_EXT,
             BLEND_OP_SRC_OUT_EXT,
             BLEND_OP_DST_IN_EXT,
             BLEND_OP_SRC_IN_EXT,
             BLEND_OP_DST_OVER_EXT,
             BLEND_OP_SRC_OVER_EXT,
             BLEND_OP_DST_EXT,
             BLEND_OP_SRC_EXT,
             BLEND_OP_ZERO_EXT :: BlendOp #-}

conNameBlendOp :: String
conNameBlendOp = "BlendOp"

enumPrefixBlendOp :: String
enumPrefixBlendOp = "BLEND_OP_"

showTableBlendOp :: [(BlendOp, String)]
showTableBlendOp =
  [ (BLEND_OP_ADD                   , "ADD")
  , (BLEND_OP_SUBTRACT              , "SUBTRACT")
  , (BLEND_OP_REVERSE_SUBTRACT      , "REVERSE_SUBTRACT")
  , (BLEND_OP_MIN                   , "MIN")
  , (BLEND_OP_MAX                   , "MAX")
  , (BLEND_OP_BLUE_EXT              , "BLUE_EXT")
  , (BLEND_OP_GREEN_EXT             , "GREEN_EXT")
  , (BLEND_OP_RED_EXT               , "RED_EXT")
  , (BLEND_OP_INVERT_OVG_EXT        , "INVERT_OVG_EXT")
  , (BLEND_OP_CONTRAST_EXT          , "CONTRAST_EXT")
  , (BLEND_OP_MINUS_CLAMPED_EXT     , "MINUS_CLAMPED_EXT")
  , (BLEND_OP_MINUS_EXT             , "MINUS_EXT")
  , (BLEND_OP_PLUS_DARKER_EXT       , "PLUS_DARKER_EXT")
  , (BLEND_OP_PLUS_CLAMPED_ALPHA_EXT, "PLUS_CLAMPED_ALPHA_EXT")
  , (BLEND_OP_PLUS_CLAMPED_EXT      , "PLUS_CLAMPED_EXT")
  , (BLEND_OP_PLUS_EXT              , "PLUS_EXT")
  , (BLEND_OP_HSL_LUMINOSITY_EXT    , "HSL_LUMINOSITY_EXT")
  , (BLEND_OP_HSL_COLOR_EXT         , "HSL_COLOR_EXT")
  , (BLEND_OP_HSL_SATURATION_EXT    , "HSL_SATURATION_EXT")
  , (BLEND_OP_HSL_HUE_EXT           , "HSL_HUE_EXT")
  , (BLEND_OP_HARDMIX_EXT           , "HARDMIX_EXT")
  , (BLEND_OP_PINLIGHT_EXT          , "PINLIGHT_EXT")
  , (BLEND_OP_LINEARLIGHT_EXT       , "LINEARLIGHT_EXT")
  , (BLEND_OP_VIVIDLIGHT_EXT        , "VIVIDLIGHT_EXT")
  , (BLEND_OP_LINEARBURN_EXT        , "LINEARBURN_EXT")
  , (BLEND_OP_LINEARDODGE_EXT       , "LINEARDODGE_EXT")
  , (BLEND_OP_INVERT_RGB_EXT        , "INVERT_RGB_EXT")
  , (BLEND_OP_INVERT_EXT            , "INVERT_EXT")
  , (BLEND_OP_EXCLUSION_EXT         , "EXCLUSION_EXT")
  , (BLEND_OP_DIFFERENCE_EXT        , "DIFFERENCE_EXT")
  , (BLEND_OP_SOFTLIGHT_EXT         , "SOFTLIGHT_EXT")
  , (BLEND_OP_HARDLIGHT_EXT         , "HARDLIGHT_EXT")
  , (BLEND_OP_COLORBURN_EXT         , "COLORBURN_EXT")
  , (BLEND_OP_COLORDODGE_EXT        , "COLORDODGE_EXT")
  , (BLEND_OP_LIGHTEN_EXT           , "LIGHTEN_EXT")
  , (BLEND_OP_DARKEN_EXT            , "DARKEN_EXT")
  , (BLEND_OP_OVERLAY_EXT           , "OVERLAY_EXT")
  , (BLEND_OP_SCREEN_EXT            , "SCREEN_EXT")
  , (BLEND_OP_MULTIPLY_EXT          , "MULTIPLY_EXT")
  , (BLEND_OP_XOR_EXT               , "XOR_EXT")
  , (BLEND_OP_DST_ATOP_EXT          , "DST_ATOP_EXT")
  , (BLEND_OP_SRC_ATOP_EXT          , "SRC_ATOP_EXT")
  , (BLEND_OP_DST_OUT_EXT           , "DST_OUT_EXT")
  , (BLEND_OP_SRC_OUT_EXT           , "SRC_OUT_EXT")
  , (BLEND_OP_DST_IN_EXT            , "DST_IN_EXT")
  , (BLEND_OP_SRC_IN_EXT            , "SRC_IN_EXT")
  , (BLEND_OP_DST_OVER_EXT          , "DST_OVER_EXT")
  , (BLEND_OP_SRC_OVER_EXT          , "SRC_OVER_EXT")
  , (BLEND_OP_DST_EXT               , "DST_EXT")
  , (BLEND_OP_SRC_EXT               , "SRC_EXT")
  , (BLEND_OP_ZERO_EXT              , "ZERO_EXT")
  ]

instance Show BlendOp where
  showsPrec = enumShowsPrec enumPrefixBlendOp showTableBlendOp conNameBlendOp (\(BlendOp x) -> x) (showsPrec 11)

instance Read BlendOp where
  readPrec = enumReadPrec enumPrefixBlendOp showTableBlendOp conNameBlendOp BlendOp

