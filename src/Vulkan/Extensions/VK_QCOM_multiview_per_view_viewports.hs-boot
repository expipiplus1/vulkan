{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_multiview_per_view_viewports - device extension
--
-- == VK_QCOM_multiview_per_view_viewports
--
-- [__Name String__]
--     @VK_QCOM_multiview_per_view_viewports@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     489
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_multiview_per_view_viewports] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_multiview_per_view_viewports extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-11-22
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_KHR_dynamic_rendering@
--
--     -   This extension interacts with @VK_EXT_extended_dynamic_state@
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
-- Certain use cases for multiview have a need for specifying a separate
-- viewport and scissor for each view, without using shader-based viewport
-- indexing as introduced with @VK_EXT_shader_viewport_index_layer@.
--
-- This extension adds a new way to control ViewportIndex with multiview.
-- When the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiview-per-view-viewports multiviewPerViewViewports>
-- feature is enabled and if the last pre-rasterization shader entry
-- point’s interface does not use the @ViewportIndex@ built-in decoration,
-- then each view of a multiview render pass instance will use a viewport
-- and scissor index equal to the @ViewIndex@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_EXTENSION_NAME'
--
-- -   'QCOM_MULTIVIEW_PER_VIEW_VIEWPORTS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_VIEWPORTS_FEATURES_QCOM'
--
-- == Issues
--
-- 1) Is is possible to enable\/disable the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-multiview-per-view-viewports multiviewPerViewViewports>
-- feature for individual render pass instances?
--
-- __RESOLVED__: No, when the multiviewPerViewViewports feature is enabled
-- during vkCreateDevice, then all created render pass instances (including
-- dynamic render passes from @VK_KHR_dynamic_rendering@) and all created
-- VkPipelines will have the feature enabled. This approach was chosen
-- because it simplifies application code and there is no known use case
-- enable\/disable the feature for individual render passes or pipelines.
--
-- 2) When this extension is used, is the value of @ViewportIndex@
-- implicitly written by the last pre-rasterization shader stage and can
-- the value of @ViewportIndex@ be read in the fragment shader?
--
-- __RESOLVED__: No, use of the extension extension does not add an
-- implicit write to @ViewportIndex@ in any shader stage, and additionally,
-- the value of @ViewportIndex@ in the fragment shader is undefined.
--
-- == Version History
--
-- -   Revision 1, 2022-11-22 (Jeff Leger)
--
-- == See Also
--
-- 'PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_multiview_per_view_viewports Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_multiview_per_view_viewports  (PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM

instance ToCStruct PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM
instance Show PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM

instance FromCStruct PhysicalDeviceMultiviewPerViewViewportsFeaturesQCOM

