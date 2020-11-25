{-# language CPP #-}
-- | = Name
--
-- VK_EXT_depth_range_unrestricted - device extension
--
-- == VK_EXT_depth_range_unrestricted
--
-- [__Name String__]
--     @VK_EXT_depth_range_unrestricted@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     14
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_depth_range_unrestricted:%20&body=@pdaniell-nv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-06-22
--
-- [__Contributors__]
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension removes the 'Vulkan.Core10.Pipeline.Viewport' @minDepth@
-- and @maxDepth@ restrictions that the values must be between @0.0@ and
-- @1.0@, inclusive. It also removes the same restriction on
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
-- @minDepthBounds@ and @maxDepthBounds@. Finally it removes the
-- restriction on the @depth@ value in
-- 'Vulkan.Core10.CommandBufferBuilding.ClearDepthStencilValue'.
--
-- == New Enum Constants
--
-- -   'EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME'
--
-- -   'EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION'
--
-- == Issues
--
-- 1) How do 'Vulkan.Core10.Pipeline.Viewport' @minDepth@ and @maxDepth@
-- values outside of the @0.0@ to @1.0@ range interact with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>?
--
-- __RESOLVED__: The behavior described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>
-- still applies. If depth clamping is disabled the depth values are still
-- clipped to 0 ≤ zc ≤ wc before the viewport transform. If depth clamping
-- is enabled the above equation is ignored and the depth values are
-- instead clamped to the 'Vulkan.Core10.Pipeline.Viewport' @minDepth@ and
-- @maxDepth@ values, which in the case of this extension can be outside of
-- the @0.0@ to @1.0@ range.
--
-- 2) What happens if a resulting depth fragment is outside of the @0.0@ to
-- @1.0@ range and the depth buffer is fixed-point rather than
-- floating-point?
--
-- __RESOLVED__: The supported range of a fixed-point depth buffer is @0.0@
-- to @1.0@ and depth fragments are clamped to this range.
--
-- == Version History
--
-- -   Revision 1, 2017-06-22 (Piers Daniell)
--
--     -   Internal revisions
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_depth_range_unrestricted Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_depth_range_unrestricted  ( EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION
                                                          , pattern EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION
                                                          , EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME
                                                          , pattern EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)

type EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION"
pattern EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION = 1


type EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME = "VK_EXT_depth_range_unrestricted"

-- No documentation found for TopLevel "VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME"
pattern EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME = "VK_EXT_depth_range_unrestricted"

