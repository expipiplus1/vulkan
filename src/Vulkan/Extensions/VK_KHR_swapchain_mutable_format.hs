{-# language CPP #-}
-- | = Name
--
-- VK_KHR_swapchain_mutable_format - device extension
--
-- == VK_KHR_swapchain_mutable_format
--
-- [__Name String__]
--     @VK_KHR_swapchain_mutable_format@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     201
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@
--
--     -   Requires @VK_KHR_maintenance2@
--
--     -   Requires @VK_KHR_image_format_list@
--
-- [__Contact__]
--
--     -   Daniel Rakos
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_swapchain_mutable_format] @drakos-arm%0A<<Here describe the issue or question you have about the VK_KHR_swapchain_mutable_format extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-03-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jason Ekstrand, Intel
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Jesse Hall, Google
--
--     -   Daniel Rakos, AMD
--
--     -   Ray Smith, ARM
--
-- == Description
--
-- This extension allows processing of swapchain images as different
-- formats to that used by the window system, which is particularly useful
-- for switching between sRGB and linear RGB formats.
--
-- It adds a new swapchain creation flag that enables creating image views
-- from presentable images with a different format than the one used to
-- create the swapchain.
--
-- == New Enum Constants
--
-- -   'KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME'
--
-- -   'KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR'
--
-- == Issues
--
-- 1) Are there any new capabilities needed?
--
-- __RESOLVED__: No. It is expected that all implementations exposing this
-- extension support swapchain image format mutability.
--
-- 2) Do we need a separate @VK_SWAPCHAIN_CREATE_EXTENDED_USAGE_BIT_KHR@?
--
-- __RESOLVED__: No. This extension requires @VK_KHR_maintenance2@ and
-- presentable images of swapchains created with
-- 'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR'
-- are created internally in a way equivalent to specifying both
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- and
-- 'Vulkan.Extensions.VK_KHR_maintenance2.IMAGE_CREATE_EXTENDED_USAGE_BIT_KHR'.
--
-- 3) Do we need a separate structure to allow specifying an image format
-- list for swapchains?
--
-- __RESOLVED__: No. We simply use the same
-- 'Vulkan.Extensions.VK_KHR_image_format_list.ImageFormatListCreateInfoKHR'
-- structure introduced by @VK_KHR_image_format_list@. The structure is
-- required to be included in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR' for
-- swapchains created with
-- 'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_MUTABLE_FORMAT_BIT_KHR'.
--
-- == Version History
--
-- -   Revision 1, 2018-03-28 (Daniel Rakos)
--
--     -   Internal revisions.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_swapchain_mutable_format Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_swapchain_mutable_format  ( KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION
                                                          , pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION
                                                          , KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME
                                                          , pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME
                                                          , SwapchainCreateFlagBitsKHR(..)
                                                          , SwapchainCreateFlagsKHR
                                                          ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
type KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION"
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_SPEC_VERSION = 1


type KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME = "VK_KHR_swapchain_mutable_format"

-- No documentation found for TopLevel "VK_KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME"
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SWAPCHAIN_MUTABLE_FORMAT_EXTENSION_NAME = "VK_KHR_swapchain_mutable_format"

