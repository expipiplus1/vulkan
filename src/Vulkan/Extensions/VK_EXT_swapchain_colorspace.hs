{-# language CPP #-}
-- | = Name
--
-- VK_EXT_swapchain_colorspace - instance extension
--
-- == VK_EXT_swapchain_colorspace
--
-- [__Name String__]
--     @VK_EXT_swapchain_colorspace@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     105
--
-- [__Revision__]
--     4
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@ to be enabled
--
-- [__Contact__]
--
--     -   Courtney Goeltzenleuchter
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_swapchain_colorspace] @courtney-g%0A*Here describe the issue or question you have about the VK_EXT_swapchain_colorspace extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-04-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Courtney Goeltzenleuchter, Google
--
-- == Description
--
-- To be done.
--
-- == New Enum Constants
--
-- -   'EXT_SWAPCHAIN_COLOR_SPACE_EXTENSION_NAME'
--
-- -   'EXT_SWAPCHAIN_COLOR_SPACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_surface.ColorSpaceKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_ADOBERGB_LINEAR_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_ADOBERGB_NONLINEAR_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_BT2020_LINEAR_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_BT709_LINEAR_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_BT709_NONLINEAR_EXT'
--
--     -   'COLOR_SPACE_DCI_P3_LINEAR_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_DCI_P3_NONLINEAR_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_DISPLAY_P3_LINEAR_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_DISPLAY_P3_NONLINEAR_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_DOLBYVISION_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_EXTENDED_SRGB_LINEAR_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_EXTENDED_SRGB_NONLINEAR_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_HDR10_HLG_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_HDR10_ST2084_EXT'
--
--     -   'Vulkan.Extensions.VK_KHR_surface.COLOR_SPACE_PASS_THROUGH_EXT'
--
-- == Issues
--
-- 1) Does the spec need to specify which kinds of image formats support
-- the color spaces?
--
-- __RESOLVED__: Pixel format is independent of color space (though some
-- color spaces really want \/ need floating point color components to be
-- useful). Therefore, do not plan on documenting what formats support
-- which colorspaces. An application /can/ call
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR' to
-- query what a particular implementation supports.
--
-- 2) How does application determine if HW supports appropriate transfer
-- function for a colorspace?
--
-- __RESOLVED__: Extension indicates that implementation /must/ not do the
-- OETF encoding if it is not sRGB. That responsibility falls to the
-- application shaders. Any other native OETF \/ EOTF functions supported
-- by an implementation can be described by separate extension.
--
-- == Version History
--
-- -   Revision 1, 2016-12-27 (Courtney Goeltzenleuchter)
--
--     -   Initial version
--
-- -   Revision 2, 2017-01-19 (Courtney Goeltzenleuchter)
--
--     -   Add pass through and multiple options for BT2020.
--
--     -   Clean up some issues with equations not displaying properly.
--
-- -   Revision 3, 2017-06-23 (Courtney Goeltzenleuchter)
--
--     -   Add extended sRGB non-linear enum.
--
-- -   Revision 4, 2019-04-26 (Graeme Leese)
--
--     -   Clarify colorspace transfer function usage.
--
--     -   Refer to normative definitions in the Data Format Specification.
--
--     -   Clarify DCI-P3 and Display P3 usage.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_swapchain_colorspace Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

