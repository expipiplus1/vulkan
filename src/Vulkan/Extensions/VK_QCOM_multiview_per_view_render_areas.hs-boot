{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_multiview_per_view_render_areas - device extension
--
-- == VK_QCOM_multiview_per_view_render_areas
--
-- [__Name String__]
--     @VK_QCOM_multiview_per_view_render_areas@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     511
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_multiview_per_view_render_areas] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_multiview_per_view_render_areas extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-01-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_KHR_dynamic_rendering@
--
--     -   This extension interacts with @VK_QCOM_render_pass_transform@
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm
--
--     -   Jonathan Tinkham, Qualcomm
--
--     -   Jonathan Wicks, Qualcomm
--
-- == Description
--
-- Certain use cases (e.g., side-by-side VR rendering) use multiview and
-- render to distinct regions of the framebuffer for each view. On some
-- implementations, there may be a performance benefit for providing
-- per-view render areas to the implementation. Such per-view render areas
-- can be used by the implementation to reduce the pixels that are affected
-- by attachment load, store, and multisample resolve operations.
--
-- The extension enables a multiview render pass instance to define
-- per-view render areas. For each view of a multiview render pass
-- instance, only those pixels in the per-view render area are affected by
-- load, store and resolve operations.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM'
--
-- -   Extending 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo':
--
--     -   'MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_EXTENSION_NAME'
--
-- -   'QCOM_MULTIVIEW_PER_VIEW_RENDER_AREAS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MULTIVIEW_PER_VIEW_RENDER_AREAS_RENDER_PASS_BEGIN_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_RENDER_AREAS_FEATURES_QCOM'
--
-- == Issues
--
-- 1) Do the per-view @renderAreas@ interact with
-- 'Vulkan.Core10.Pass.getRenderAreaGranularity' ?
--
-- __RESOLVED__: There is no change. The granularity returned by
-- 'Vulkan.Core10.Pass.getRenderAreaGranularity' also applies to the
-- per-view @renderAreas@.
--
-- 2) How does this extension interact with
-- @VK_QCOM_render_pass_transform@?
--
-- __RESOLVED__: When @VK_QCOM_render_pass_transform@ is enabled, the
-- application provides render area in non-rotated coordinates which is
-- rotated by the implementation to the rotated coordinate system. When
-- this extension is used in combination with
-- @VK_QCOM_render_pass_transform@, then the @renderArea@ provided in
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@renderArea@,
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@,
-- or
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM'::@renderArea@
-- is rotated by the implementation. The per-view render areas are not
-- rotated.
--
-- 3) How does this extension interact with
-- @VK_QCOM_multiview_per_view_viewports@
--
-- __RESOLVED__: There is no direct interaction. The per-view viewports and
-- the per-view renderAreas are orthogonal features.
--
-- 4) When a per-view @renderArea@ is specified, must multiview rendering
-- for each view of a multiview render pass be contained within the
-- per-view @renderArea@?
--
-- __RESOLVED__: Yes, and the @VK_QCOM_multiview_per_view_viewports@ may
-- help here since it provides per-view scissors.
--
-- 5) When per-view render areas are specified, what purpose if any do
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'::@renderArea@
-- and
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo'::@renderArea@
-- serve?
--
-- __RESOLVED__: The per-view @renderArea@ effectively overrides the
-- per-renderpass @renderArea@. The per-view @renderArea@ defines the
-- regions of the attachments that are effected by load, store, and
-- multisample resolve operations. A valid implementation could ignore the
-- per-renderpass @renderArea@. However, as an aid to the implementation,
-- the application must set the per-renderpass @renderArea@ to an area that
-- is at least as large as the union of all the per-view render areas.
-- Pixels that are within the per-renderpass @renderArea@ but not within
-- any per-view render area must not be affected by load, store, or
-- multisample resolve operations.
--
-- == Version History
--
-- -   Revision 1, 2023-01-10 (Jeff Leger)
--
-- == See Also
--
-- 'MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM',
-- 'PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_multiview_per_view_render_areas Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_multiview_per_view_render_areas  ( MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM
                                                                  , PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM
                                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM

instance ToCStruct MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM
instance Show MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM

instance FromCStruct MultiviewPerViewRenderAreasRenderPassBeginInfoQCOM


data PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM

instance ToCStruct PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM
instance Show PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM

instance FromCStruct PhysicalDeviceMultiviewPerViewRenderAreasFeaturesQCOM

