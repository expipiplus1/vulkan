{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_swapchain_colorspace
  ( pattern VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT
  , pattern VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_BT2020_LINEAR_EXT
  , pattern VK_COLOR_SPACE_BT709_LINEAR_EXT
  , pattern VK_COLOR_SPACE_BT709_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_DCI_P3_LINEAR_EXT
  , pattern VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_DOLBYVISION_EXT
  , pattern VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT
  , pattern VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT
  , pattern VK_COLOR_SPACE_HDR10_HLG_EXT
  , pattern VK_COLOR_SPACE_HDR10_ST2084_EXT
  , pattern VK_COLOR_SPACE_PASS_THROUGH_EXT
  , pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME
  , pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR(..)
  )


-- | @VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT@ specifies support for the AdobeRGB
-- color space and applies a linear OETF.
pattern VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_ADOBERGB_LINEAR_EXT = VkColorSpaceKHR 1000104011
-- | @VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT@ specifies support for the
-- AdobeRGB color space and applies the Gamma 2.2 OETF.
pattern VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_ADOBERGB_NONLINEAR_EXT = VkColorSpaceKHR 1000104012
-- | @VK_COLOR_SPACE_BT2020_LINEAR_EXT@ specifies support for the BT2020
-- color space and applies a linear OETF.
pattern VK_COLOR_SPACE_BT2020_LINEAR_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_BT2020_LINEAR_EXT = VkColorSpaceKHR 1000104007
-- | @VK_COLOR_SPACE_BT709_LINEAR_EXT@ specifies support for the BT709 color
-- space and applies a linear OETF.
pattern VK_COLOR_SPACE_BT709_LINEAR_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_BT709_LINEAR_EXT = VkColorSpaceKHR 1000104005
-- | @VK_COLOR_SPACE_BT709_NONLINEAR_EXT@ specifies support for the BT709
-- color space and applies the SMPTE 170M OETF.
pattern VK_COLOR_SPACE_BT709_NONLINEAR_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_BT709_NONLINEAR_EXT = VkColorSpaceKHR 1000104006
-- | @VK_COLOR_SPACE_DCI_P3_LINEAR_EXT@ specifies support for the DCI-P3
-- color space and applies a linear OETF.
pattern VK_COLOR_SPACE_DCI_P3_LINEAR_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_DCI_P3_LINEAR_EXT = VkColorSpaceKHR 1000104003
-- | @VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT@ specifies support for the DCI-P3
-- color space and applies the Gamma 2.6 OETF.
pattern VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_DCI_P3_NONLINEAR_EXT = VkColorSpaceKHR 1000104004
-- | @VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT@ specifies support for the
-- Display-P3 color space and applies an sRGB-like transfer function
-- (defined below).
pattern VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT = VkColorSpaceKHR 1000104001
-- | @VK_COLOR_SPACE_DOLBYVISION_EXT@ specifies support for the Dolby Vision
-- (BT2020 color space), proprietary encoding, and applies the SMPTE ST2084
-- OETF.
pattern VK_COLOR_SPACE_DOLBYVISION_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_DOLBYVISION_EXT = VkColorSpaceKHR 1000104009
-- | @VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT@ specifies support for the
-- extended sRGB color space and applies a linear transfer function.
pattern VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT = VkColorSpaceKHR 1000104002
-- | @VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT@ specifies support for the
-- extended sRGB color space and applies an sRGB transfer function.
pattern VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT = VkColorSpaceKHR 1000104014
-- | @VK_COLOR_SPACE_HDR10_HLG_EXT@ specifies support for the HDR10 (BT2020
-- color space) and applies the Hybrid Log Gamma (HLG) OETF.
pattern VK_COLOR_SPACE_HDR10_HLG_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_HDR10_HLG_EXT = VkColorSpaceKHR 1000104010
-- | @VK_COLOR_SPACE_HDR10_ST2084_EXT@ specifies support for the HDR10
-- (BT2020 color) space and applies the SMPTE ST2084 Perceptual Quantizer
-- (PQ) OETF.
pattern VK_COLOR_SPACE_HDR10_ST2084_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_HDR10_ST2084_EXT = VkColorSpaceKHR 1000104008
-- | @VK_COLOR_SPACE_PASS_THROUGH_EXT@ specifies that color components are
-- used “as is”. This is intended to allow applications to supply data for
-- color spaces not described here.
pattern VK_COLOR_SPACE_PASS_THROUGH_EXT :: VkColorSpaceKHR
pattern VK_COLOR_SPACE_PASS_THROUGH_EXT = VkColorSpaceKHR 1000104013
-- No documentation found for TopLevel "VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME"
pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME = "VK_EXT_swapchain_colorspace"
-- No documentation found for TopLevel "VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION"
pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION = 3
