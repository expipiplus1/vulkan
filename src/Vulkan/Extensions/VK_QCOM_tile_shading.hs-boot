{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_tile_shading - device extension
--
-- = VK_QCOM_tile_shading
--
-- [__Name String__]
--     @VK_QCOM_tile_shading@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     310
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/QCOM/SPV_QCOM_tile_shading.html SPV_QCOM_tile_shading>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_tile_shading] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_tile_shading extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_tile_shading.adoc VK_QCOM_tile_shading>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-8-13
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_KHR_dynamic_rendering@
--
--     -   This extension interacts with @VK_EXT_transform_feedback@
--
--     -   This extension interacts with @VK_EXT_debug_marker@
--
--     -   This extension interacts with
--         @VK_EXT_attachment_feedback_loop_layout@
--
--     -   This extension interacts with
--         @VK_KHR_dynamic_rendering_local_read@
--
--     -   This extension interacts with @VK_QCOM_image_processing@
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm
--
--     -   Matt Netsch, Qualcomm
--
--     -   Srihari Babu Alla, Qualcomm
--
--     -   Matthew Smith, Qualcomm
--
--     -   Kevin Matlage, Qualcomm
--
--     -   Alex Bourd, Qualcomm
--
-- == Description
--
-- This extension exposes tile shading in Vulkan. Many mobile GPUs utilize
-- Tile-Based Deferred Rendering (TBDR) to optimize for power and
-- performance. Conversely, most desktop GPUs use immediate-mode rendering
-- (IM). Adreno ™ GPUs uniquely have the ability to operate in either mode,
-- and when this extension is not enabled, the Adreno driver will select
-- the most optimal mode (TBDR or IM) based on the workload; this feature
-- is called FlexRender ™. When this extension is in use, FlexRender is
-- disabled and the GPU operates exclusively in TBDR wherever possible.
--
-- The TBDR mode divides the color and depth\/stencil buffer attachments
-- into a regular grid of smaller regions called “tiles”. When a render
-- pass instance is submitted for execution on an Adreno GPU, the rendering
-- is split into two phases: a single “visibility pass” followed by
-- multiple “rendering passes” where a separate render pass is issued for
-- each tile in the framebuffer.
--
-- The “visibility pass” processes the geometry: identifies which tiles are
-- covered by each primitive, eliminates occluded primitives and unneeded
-- state changes, and performs other tile-specific optimizations. The
-- primitive coverage information collected during the visibility pass is
-- used in the subsequent “rendering pass” for each tile. During the
-- rendering pass for each tile, any primitives that were determined not to
-- cover the current tile are skipped.
--
-- This deferred rasterization additionally utilizes a specialized
-- high-bandwidth on-die memory, “tile memory”. Tile memory is dramatically
-- more efficient than other device memory. The tile memory temporarily
-- stores the color and other attachments for each tile during
-- rasterization. After each tile is fully rasterized, the resulting tile
-- is typically copied to device memory backing the attachment as specified
-- by the render pass STORE_OP. The per-tile rendering passes occur
-- independently for each tile, with multiple tiles potentially being
-- processed in parallel.
--
-- This extension enables applications to leverage the power and
-- performance of tile memory in new ways:
--
-- -   Adds a mechanism for recording dispatches or draws that are
--     guaranteed to be executed per-tile.
--
-- -   Such draws bypass the above-mentioned visibility-based skipping and
--     are guaranteed to be executed for every tile in the rendering pass.
--
-- -   Shaders can declare “tile attachments” variables, providing shader
--     access to color, depth\/stencil, and input attachment pixels.
--
-- -   Fragment and compute shaders can read these render pass attachments
--     at any location within the tile. Compute shaders can also write to
--     color attachments at any location within the tile.
--
-- -   Shaders can use new built-in variables that provide the location,
--     size, and apron region of the tile.
--
-- -   A new tile dispatch command automatically scales workgroup sizes and
--     counts to the tile size, given a desired shading rate.
--
-- -   Framebuffer-local dependencies are expanded to tile-sized regions,
--     rather than a single pixel or sample.
--
-- -   A tile shading render pass can also enable tiling “aprons”. This is
--     a specialized rendering mode where the GPU renders overlapping tiles
--     that enable specific use cases.
--
-- == New Commands
--
-- -   'cmdBeginPerTileExecutionQCOM'
--
-- -   'cmdDispatchTileQCOM'
--
-- -   'cmdEndPerTileExecutionQCOM'
--
-- == New Structures
--
-- -   'DispatchTileInfoQCOM'
--
-- -   'PerTileBeginInfoQCOM'
--
-- -   'PerTileEndInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceTileShadingFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceTileShadingPropertiesQCOM'
--
-- -   Extending 'Vulkan.Core10.Pass.RenderPassCreateInfo',
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingInfo',
--     'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo':
--
--     -   'RenderPassTileShadingCreateInfoQCOM'
--
-- == New Enums
--
-- -   'TileShadingRenderPassFlagBitsQCOM'
--
-- == New Bitmasks
--
-- -   'TileShadingRenderPassFlagsQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_TILE_SHADING_EXTENSION_NAME'
--
-- -   'QCOM_TILE_SHADING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_TILE_ATTACHMENT_READ_BIT_QCOM'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_TILE_ATTACHMENT_WRITE_BIT_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPATCH_TILE_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PER_TILE_BEGIN_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PER_TILE_END_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_PROPERTIES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_TILE_SHADING_CREATE_INFO_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SubpassDescriptionFlagBits':
--
--     -   'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_TILE_SHADING_APRON_BIT_QCOM'
--
-- == New or Modified Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-tileoffset TileOffsetQCOM>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-tilesize TileDimensionQCOM>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-tileapronsize TileApronSizeQCOM>
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-TileShadingQCOM TileShadingQCOM>
--
-- == Issues
--
-- 1) Some early Adreno drivers advertised support for version 1 of this
-- extension without supporting the required
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tileShadingApron tileShadingApron>
-- feature. To cover all Adreno devices on the market, applications should
-- not assume any version of this extension supports the @tileShadingApron@
-- feature without performing a feature query.
--
-- == Version History
--
-- -   Revision 2, 2025-08-13 (Matthew Netsch)
--
--     -   Make the @tileShadingApron@ feature optional
--
-- -   Revision 1, 2023-10-12 (Jeff Leger)
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_tile_shading Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_tile_shading  ( DispatchTileInfoQCOM
                                               , PerTileBeginInfoQCOM
                                               , PerTileEndInfoQCOM
                                               , PhysicalDeviceTileShadingFeaturesQCOM
                                               , PhysicalDeviceTileShadingPropertiesQCOM
                                               , RenderPassTileShadingCreateInfoQCOM
                                               ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DispatchTileInfoQCOM

instance ToCStruct DispatchTileInfoQCOM
instance Show DispatchTileInfoQCOM

instance FromCStruct DispatchTileInfoQCOM


data PerTileBeginInfoQCOM

instance ToCStruct PerTileBeginInfoQCOM
instance Show PerTileBeginInfoQCOM

instance FromCStruct PerTileBeginInfoQCOM


data PerTileEndInfoQCOM

instance ToCStruct PerTileEndInfoQCOM
instance Show PerTileEndInfoQCOM

instance FromCStruct PerTileEndInfoQCOM


data PhysicalDeviceTileShadingFeaturesQCOM

instance ToCStruct PhysicalDeviceTileShadingFeaturesQCOM
instance Show PhysicalDeviceTileShadingFeaturesQCOM

instance FromCStruct PhysicalDeviceTileShadingFeaturesQCOM


data PhysicalDeviceTileShadingPropertiesQCOM

instance ToCStruct PhysicalDeviceTileShadingPropertiesQCOM
instance Show PhysicalDeviceTileShadingPropertiesQCOM

instance FromCStruct PhysicalDeviceTileShadingPropertiesQCOM


data RenderPassTileShadingCreateInfoQCOM

instance ToCStruct RenderPassTileShadingCreateInfoQCOM
instance Show RenderPassTileShadingCreateInfoQCOM

instance FromCStruct RenderPassTileShadingCreateInfoQCOM

