{-# language CPP #-}
-- | = Name
--
-- VK_KHR_incremental_present - device extension
--
-- == VK_KHR_incremental_present
--
-- [__Name String__]
--     @VK_KHR_incremental_present@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     85
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@ to be enabled for any device-level
--         functionality
--
-- [__Contact__]
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_incremental_present] @ianelliottus%0A<<Here describe the issue or question you have about the VK_KHR_incremental_present extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-11-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ian Elliott, Google
--
--     -   Jesse Hall, Google
--
--     -   Alon Or-bach, Samsung
--
--     -   James Jones, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Ray Smith, ARM
--
--     -   Mika Isojarvi, Google
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This device extension extends
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', from the
-- @VK_KHR_swapchain@ extension, allowing an application to specify a list
-- of rectangular, modified regions of each image to present. This should
-- be used in situations where an application is only changing a small
-- portion of the presentable images within a swapchain, since it enables
-- the presentation engine to avoid wasting time presenting parts of the
-- surface that have not changed.
--
-- This extension is leveraged from the @EGL_KHR_swap_buffers_with_damage@
-- extension.
--
-- == New Structures
--
-- -   'PresentRegionKHR'
--
-- -   'RectLayerKHR'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'PresentRegionsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_INCREMENTAL_PRESENT_EXTENSION_NAME'
--
-- -   'KHR_INCREMENTAL_PRESENT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_REGIONS_KHR'
--
-- == Issues
--
-- 1) How should we handle steroescopic-3D swapchains? We need to add a
-- layer for each rectangle. One approach is to create another struct
-- containing the 'Vulkan.Core10.FundamentalTypes.Rect2D' plus layer, and
-- have 'PresentRegionsKHR' point to an array of that struct. Another
-- approach is to have two parallel arrays, @pRectangles@ and @pLayers@,
-- where @pRectangles@[i] and @pLayers@[i] must be used together. Which
-- approach should we use, and if the array of a new structure, what should
-- that be called?
--
-- __RESOLVED__: Create a new structure, which is a
-- 'Vulkan.Core10.FundamentalTypes.Rect2D' plus a layer, and will be called
-- 'RectLayerKHR'.
--
-- 2) Where is the origin of the 'RectLayerKHR'?
--
-- __RESOLVED__: The upper left corner of the presentable image(s) of the
-- swapchain, per the definition of framebuffer coordinates.
--
-- 3) Does the rectangular region, 'RectLayerKHR', specify pixels of the
-- swapchain’s image(s), or of the surface?
--
-- __RESOLVED__: Of the image(s). Some presentation engines may scale the
-- pixels of a swapchain’s image(s) to the size of the surface. The size of
-- the swapchain’s image(s) will be consistent, where the size of the
-- surface may vary over time.
--
-- 4) What if all of the rectangles for a given swapchain contain a width
-- and\/or height of zero?
--
-- __RESOLVED__: The application is indicating that no pixels changed since
-- the last present. The presentation engine may use such a hint and not
-- update any pixels for the swapchain. However, all other semantics of
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' must still be
-- honored, including waiting for semaphores to signal.
--
-- 5) When the swapchain is created with
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@preTransform@
-- set to a value other than
-- 'Vulkan.Extensions.VK_KHR_surface.SURFACE_TRANSFORM_IDENTITY_BIT_KHR',
-- should the rectangular region, 'RectLayerKHR', be transformed to align
-- with the @preTransform@?
--
-- __RESOLVED__: No. The rectangular region in 'RectLayerKHR' should not be
-- transformed. As such, it may not align with the extents of the
-- swapchain’s image(s). It is the responsibility of the presentation
-- engine to transform the rectangular region. This matches the behavior of
-- the Android presentation engine, which set the precedent.
--
-- == Version History
--
-- -   Revision 1, 2016-11-02 (Ian Elliott)
--
--     -   Internal revisions
--
-- -   Revision 2, 2021-03-18 (Ian Elliott)
--
--     -   Clarified alignment of rectangles for presentation engines that
--         support transformed swapchains.
--
-- == See Also
--
-- 'PresentRegionKHR', 'PresentRegionsKHR', 'RectLayerKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_incremental_present Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_incremental_present  ( PresentRegionKHR
                                                     , PresentRegionsKHR
                                                     , RectLayerKHR
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PresentRegionKHR

instance ToCStruct PresentRegionKHR
instance Show PresentRegionKHR

instance FromCStruct PresentRegionKHR


data PresentRegionsKHR

instance ToCStruct PresentRegionsKHR
instance Show PresentRegionsKHR

instance FromCStruct PresentRegionsKHR


data RectLayerKHR

instance ToCStruct RectLayerKHR
instance Show RectLayerKHR

instance FromCStruct RectLayerKHR

