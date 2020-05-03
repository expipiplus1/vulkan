{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_swapchain_colorspace  ( pattern COLOR_SPACE_DCI_P3_LINEAR_EXT
                                                      , ColorSpaceKHR( COLOR_SPACE_SRGB_NONLINEAR_KHR
                                                                     , COLOR_SPACE_DISPLAY_NATIVE_AMD
                                                                     , COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT
                                                                     , COLOR_SPACE_PASS_THROUGH_EXT
                                                                     , COLOR_SPACE_ADOBERGB_NONLINEAR_EXT
                                                                     , COLOR_SPACE_ADOBERGB_LINEAR_EXT
                                                                     , COLOR_SPACE_HDR10_HLG_EXT
                                                                     , COLOR_SPACE_DOLBYVISION_EXT
                                                                     , COLOR_SPACE_HDR10_ST2084_EXT
                                                                     , COLOR_SPACE_BT2020_LINEAR_EXT
                                                                     , COLOR_SPACE_BT709_NONLINEAR_EXT
                                                                     , COLOR_SPACE_BT709_LINEAR_EXT
                                                                     , COLOR_SPACE_DCI_P3_NONLINEAR_EXT
                                                                     , COLOR_SPACE_DISPLAY_P3_LINEAR_EXT
                                                                     , COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT
                                                                     , COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
                                                                     , ..
                                                                     )
                                                      , EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION
                                                      , pattern EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION
                                                      , EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME
                                                      , pattern EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME
                                                      ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.String (IsString)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VK_COLOR_SPACE_DCI_P3_LINEAR_EXT"
pattern COLOR_SPACE_DCI_P3_LINEAR_EXT = COLOR_SPACE_DISPLAY_P3_LINEAR_EXT


-- | VkColorSpaceKHR - supported color space of the presentation engine
--
-- = Description
--
-- Note
--
-- In the initial release of the @VK_KHR_surface@ and @VK_KHR_swapchain@
-- extensions, the token @VK_COLORSPACE_SRGB_NONLINEAR_KHR@ was used.
-- Starting in the 2016-05-13 updates to the extension branches, matching
-- release 1.0.13 of the core API specification,
-- 'COLOR_SPACE_SRGB_NONLINEAR_KHR' is used instead for consistency with
-- Vulkan naming rules. The older enum is still available for backwards
-- compatibility.
--
-- Note
--
-- In older versions of this extension 'COLOR_SPACE_DISPLAY_P3_LINEAR_EXT'
-- was misnamed 'COLOR_SPACE_DCI_P3_LINEAR_EXT'. This has been updated to
-- indicate that it uses RGB color encoding, not XYZ. The old name is
-- deprecated but is maintained for backwards compatibility.
--
-- The color components of non-linear color space swap chain images /must/
-- have had the appropriate transfer function applied. The color space
-- selected for the swap chain image will not affect the processing of data
-- written into the image by the implementation. Vulkan requires that all
-- implementations support the sRGB transfer function by use of an SRGB
-- pixel format. Other transfer functions, such as SMPTE 170M or SMPTE2084,
-- /can/ be performed by the application shader. This extension defines
-- enums for 'ColorSpaceKHR' that correspond to the following color spaces:
--
-- +--------------+----------+----------+----------+-------------+------------+
-- | Name         | Red      | Green    | Blue     | White-point | Transfer   |
-- |              | Primary  | Primary  | Primary  |             | function   |
-- +==============+==========+==========+==========+=============+============+
-- | DCI-P3       | 1.000,   | 0.000,   | 0.000,   | 0.3333,     | DCI P3     |
-- |              | 0.000    | 1.000    | 0.000    | 0.3333      |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | Display-P3   | 0.680,   | 0.265,   | 0.150,   | 0.3127,     | Display-P3 |
-- |              | 0.320    | 0.690    | 0.060    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | BT709        | 0.640,   | 0.300,   | 0.150,   | 0.3127,     | ITU (SMPTE |
-- |              | 0.330    | 0.600    | 0.060    | 0.3290      | 170M)      |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | sRGB         | 0.640,   | 0.300,   | 0.150,   | 0.3127,     | sRGB       |
-- |              | 0.330    | 0.600    | 0.060    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | extended     | 0.640,   | 0.300,   | 0.150,   | 0.3127,     | extended   |
-- | sRGB         | 0.330    | 0.600    | 0.060    | 0.3290      | sRGB       |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | HDR10_ST2084 | 0.708,   | 0.170,   | 0.131,   | 0.3127,     | ST2084 PQ  |
-- |              | 0.292    | 0.797    | 0.046    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | DOLBYVISION  | 0.708,   | 0.170,   | 0.131,   | 0.3127,     | ST2084 PQ  |
-- |              | 0.292    | 0.797    | 0.046    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | HDR10_HLG    | 0.708,   | 0.170,   | 0.131,   | 0.3127,     | HLG        |
-- |              | 0.292    | 0.797    | 0.046    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
-- | AdobeRGB     | 0.640,   | 0.210,   | 0.150,   | 0.3127,     | AdobeRGB   |
-- |              | 0.330    | 0.710    | 0.060    | 0.3290      |            |
-- |              |          |          |          | (D65)       |            |
-- +--------------+----------+----------+----------+-------------+------------+
--
-- Color Spaces and Attributes
--
-- The transfer functions are described in the “Transfer Functions” chapter
-- of the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#data-format Khronos Data Format Specification>.
--
-- Except Display-P3 OETF, which is:
--
-- \[\begin{aligned}
-- E & =
--   \begin{cases}
--     1.055 \times L^{1 \over 2.4} - 0.055 & \text{for}\  0.0030186 \leq L \leq 1 \\
--     12.92 \times L                       & \text{for}\  0 \leq L < 0.0030186
--   \end{cases}
-- \end{aligned}\]
--
-- where L is the linear value of a color channel and E is the encoded
-- value (as stored in the image in memory).
--
-- Note
--
-- For most uses, the sRGB OETF is equivalent.
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceFormatKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'
newtype ColorSpaceKHR = ColorSpaceKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'COLOR_SPACE_SRGB_NONLINEAR_KHR' specifies support for the sRGB color
-- space.
pattern COLOR_SPACE_SRGB_NONLINEAR_KHR = ColorSpaceKHR 0
-- | 'COLOR_SPACE_DISPLAY_NATIVE_AMD' specifies support for the display’s
-- native color space. This matches the color space expectations of AMD’s
-- FreeSync2 standard, for displays supporting it.
pattern COLOR_SPACE_DISPLAY_NATIVE_AMD = ColorSpaceKHR 1000213000
-- | 'COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT' specifies support for the
-- extended sRGB color space to be displayed using an sRGB EOTF.
pattern COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT = ColorSpaceKHR 1000104014
-- | 'COLOR_SPACE_PASS_THROUGH_EXT' specifies that color components are used
-- “as is”. This is intended to allow applications to supply data for color
-- spaces not described here.
pattern COLOR_SPACE_PASS_THROUGH_EXT = ColorSpaceKHR 1000104013
-- | 'COLOR_SPACE_ADOBERGB_NONLINEAR_EXT' specifies support for the AdobeRGB
-- color space to be displayed using the Gamma 2.2 EOTF.
pattern COLOR_SPACE_ADOBERGB_NONLINEAR_EXT = ColorSpaceKHR 1000104012
-- | 'COLOR_SPACE_ADOBERGB_LINEAR_EXT' specifies support for the AdobeRGB
-- color space to be displayed using a linear EOTF.
pattern COLOR_SPACE_ADOBERGB_LINEAR_EXT = ColorSpaceKHR 1000104011
-- | 'COLOR_SPACE_HDR10_HLG_EXT' specifies support for the HDR10 (BT2020
-- color space) to be displayed using the Hybrid Log Gamma (HLG) EOTF.
pattern COLOR_SPACE_HDR10_HLG_EXT = ColorSpaceKHR 1000104010
-- | 'COLOR_SPACE_DOLBYVISION_EXT' specifies support for the Dolby Vision
-- (BT2020 color space), proprietary encoding, to be displayed using the
-- SMPTE ST2084 EOTF.
pattern COLOR_SPACE_DOLBYVISION_EXT = ColorSpaceKHR 1000104009
-- | 'COLOR_SPACE_HDR10_ST2084_EXT' specifies support for the HDR10 (BT2020
-- color) space to be displayed using the SMPTE ST2084 Perceptual Quantizer
-- (PQ) EOTF.
pattern COLOR_SPACE_HDR10_ST2084_EXT = ColorSpaceKHR 1000104008
-- | 'COLOR_SPACE_BT2020_LINEAR_EXT' specifies support for the BT2020 color
-- space to be displayed using a linear EOTF.
pattern COLOR_SPACE_BT2020_LINEAR_EXT = ColorSpaceKHR 1000104007
-- | 'COLOR_SPACE_BT709_NONLINEAR_EXT' specifies support for the BT709 color
-- space to be displayed using the SMPTE 170M EOTF.
pattern COLOR_SPACE_BT709_NONLINEAR_EXT = ColorSpaceKHR 1000104006
-- | 'COLOR_SPACE_BT709_LINEAR_EXT' specifies support for the BT709 color
-- space to be displayed using a linear EOTF.
pattern COLOR_SPACE_BT709_LINEAR_EXT = ColorSpaceKHR 1000104005
-- | 'COLOR_SPACE_DCI_P3_NONLINEAR_EXT' specifies support for the DCI-P3
-- color space to be displayed using the DCI-P3 EOTF. Note that values in
-- such an image are interpreted as XYZ encoded color data by the
-- presentation engine.
pattern COLOR_SPACE_DCI_P3_NONLINEAR_EXT = ColorSpaceKHR 1000104004
-- | 'COLOR_SPACE_DISPLAY_P3_LINEAR_EXT' specifies support for the Display-P3
-- color space to be displayed using a linear EOTF.
pattern COLOR_SPACE_DISPLAY_P3_LINEAR_EXT = ColorSpaceKHR 1000104003
-- | 'COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT' specifies support for the
-- extended sRGB color space to be displayed using a linear EOTF.
pattern COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT = ColorSpaceKHR 1000104002
-- | 'COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT' specifies support for the
-- Display-P3 color space to be displayed using an sRGB-like EOTF (defined
-- below).
pattern COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT = ColorSpaceKHR 1000104001
{-# complete COLOR_SPACE_SRGB_NONLINEAR_KHR,
             COLOR_SPACE_DISPLAY_NATIVE_AMD,
             COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT,
             COLOR_SPACE_PASS_THROUGH_EXT,
             COLOR_SPACE_ADOBERGB_NONLINEAR_EXT,
             COLOR_SPACE_ADOBERGB_LINEAR_EXT,
             COLOR_SPACE_HDR10_HLG_EXT,
             COLOR_SPACE_DOLBYVISION_EXT,
             COLOR_SPACE_HDR10_ST2084_EXT,
             COLOR_SPACE_BT2020_LINEAR_EXT,
             COLOR_SPACE_BT709_NONLINEAR_EXT,
             COLOR_SPACE_BT709_LINEAR_EXT,
             COLOR_SPACE_DCI_P3_NONLINEAR_EXT,
             COLOR_SPACE_DISPLAY_P3_LINEAR_EXT,
             COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT,
             COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT :: ColorSpaceKHR #-}

instance Show ColorSpaceKHR where
  showsPrec p = \case
    COLOR_SPACE_SRGB_NONLINEAR_KHR -> showString "COLOR_SPACE_SRGB_NONLINEAR_KHR"
    COLOR_SPACE_DISPLAY_NATIVE_AMD -> showString "COLOR_SPACE_DISPLAY_NATIVE_AMD"
    COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT -> showString "COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT"
    COLOR_SPACE_PASS_THROUGH_EXT -> showString "COLOR_SPACE_PASS_THROUGH_EXT"
    COLOR_SPACE_ADOBERGB_NONLINEAR_EXT -> showString "COLOR_SPACE_ADOBERGB_NONLINEAR_EXT"
    COLOR_SPACE_ADOBERGB_LINEAR_EXT -> showString "COLOR_SPACE_ADOBERGB_LINEAR_EXT"
    COLOR_SPACE_HDR10_HLG_EXT -> showString "COLOR_SPACE_HDR10_HLG_EXT"
    COLOR_SPACE_DOLBYVISION_EXT -> showString "COLOR_SPACE_DOLBYVISION_EXT"
    COLOR_SPACE_HDR10_ST2084_EXT -> showString "COLOR_SPACE_HDR10_ST2084_EXT"
    COLOR_SPACE_BT2020_LINEAR_EXT -> showString "COLOR_SPACE_BT2020_LINEAR_EXT"
    COLOR_SPACE_BT709_NONLINEAR_EXT -> showString "COLOR_SPACE_BT709_NONLINEAR_EXT"
    COLOR_SPACE_BT709_LINEAR_EXT -> showString "COLOR_SPACE_BT709_LINEAR_EXT"
    COLOR_SPACE_DCI_P3_NONLINEAR_EXT -> showString "COLOR_SPACE_DCI_P3_NONLINEAR_EXT"
    COLOR_SPACE_DISPLAY_P3_LINEAR_EXT -> showString "COLOR_SPACE_DISPLAY_P3_LINEAR_EXT"
    COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT -> showString "COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT"
    COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT -> showString "COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT"
    ColorSpaceKHR x -> showParen (p >= 11) (showString "ColorSpaceKHR " . showsPrec 11 x)

instance Read ColorSpaceKHR where
  readPrec = parens (choose [("COLOR_SPACE_SRGB_NONLINEAR_KHR", pure COLOR_SPACE_SRGB_NONLINEAR_KHR)
                            , ("COLOR_SPACE_DISPLAY_NATIVE_AMD", pure COLOR_SPACE_DISPLAY_NATIVE_AMD)
                            , ("COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT", pure COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT)
                            , ("COLOR_SPACE_PASS_THROUGH_EXT", pure COLOR_SPACE_PASS_THROUGH_EXT)
                            , ("COLOR_SPACE_ADOBERGB_NONLINEAR_EXT", pure COLOR_SPACE_ADOBERGB_NONLINEAR_EXT)
                            , ("COLOR_SPACE_ADOBERGB_LINEAR_EXT", pure COLOR_SPACE_ADOBERGB_LINEAR_EXT)
                            , ("COLOR_SPACE_HDR10_HLG_EXT", pure COLOR_SPACE_HDR10_HLG_EXT)
                            , ("COLOR_SPACE_DOLBYVISION_EXT", pure COLOR_SPACE_DOLBYVISION_EXT)
                            , ("COLOR_SPACE_HDR10_ST2084_EXT", pure COLOR_SPACE_HDR10_ST2084_EXT)
                            , ("COLOR_SPACE_BT2020_LINEAR_EXT", pure COLOR_SPACE_BT2020_LINEAR_EXT)
                            , ("COLOR_SPACE_BT709_NONLINEAR_EXT", pure COLOR_SPACE_BT709_NONLINEAR_EXT)
                            , ("COLOR_SPACE_BT709_LINEAR_EXT", pure COLOR_SPACE_BT709_LINEAR_EXT)
                            , ("COLOR_SPACE_DCI_P3_NONLINEAR_EXT", pure COLOR_SPACE_DCI_P3_NONLINEAR_EXT)
                            , ("COLOR_SPACE_DISPLAY_P3_LINEAR_EXT", pure COLOR_SPACE_DISPLAY_P3_LINEAR_EXT)
                            , ("COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT", pure COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT)
                            , ("COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT", pure COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ColorSpaceKHR")
                       v <- step readPrec
                       pure (ColorSpaceKHR v)))


type EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION"
pattern EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION = 4


type EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME = "VK_EXT_swapchain_colorspace"

-- No documentation found for TopLevel "VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME"
pattern EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME = "VK_EXT_swapchain_colorspace"

