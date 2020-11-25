{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_swapchain_colorspace"
module Vulkan.Extensions.VK_EXT_swapchain_colorspace  ( pattern COLOR_SPACE_DCI_P3_LINEAR_EXT
                                                      , EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION
                                                      , pattern EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION
                                                      , EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME
                                                      , pattern EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME
                                                      , ColorSpaceKHR(..)
                                                      ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR(COLOR_SPACE_DISPLAY_P3_LINEAR_EXT))
import Vulkan.Extensions.VK_KHR_surface (ColorSpaceKHR(..))
-- No documentation found for TopLevel "VK_COLOR_SPACE_DCI_P3_LINEAR_EXT"
pattern COLOR_SPACE_DCI_P3_LINEAR_EXT = COLOR_SPACE_DISPLAY_P3_LINEAR_EXT


type EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION"
pattern EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION = 4


type EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME = "VK_EXT_swapchain_colorspace"

-- No documentation found for TopLevel "VK_EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME"
pattern EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME = "VK_EXT_swapchain_colorspace"

