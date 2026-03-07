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
module Vulkan.Extensions.VK_QCOM_tile_shading  ( cmdDispatchTileQCOM
                                               , cmdBeginPerTileExecutionQCOM
                                               , cmdEndPerTileExecutionQCOM
                                               , PhysicalDeviceTileShadingFeaturesQCOM(..)
                                               , PhysicalDeviceTileShadingPropertiesQCOM(..)
                                               , RenderPassTileShadingCreateInfoQCOM(..)
                                               , PerTileBeginInfoQCOM(..)
                                               , PerTileEndInfoQCOM(..)
                                               , DispatchTileInfoQCOM(..)
                                               , TileShadingRenderPassFlagsQCOM
                                               , TileShadingRenderPassFlagBitsQCOM( TILE_SHADING_RENDER_PASS_ENABLE_BIT_QCOM
                                                                                  , TILE_SHADING_RENDER_PASS_PER_TILE_EXECUTION_BIT_QCOM
                                                                                  , ..
                                                                                  )
                                               , QCOM_TILE_SHADING_SPEC_VERSION
                                               , pattern QCOM_TILE_SHADING_SPEC_VERSION
                                               , QCOM_TILE_SHADING_EXTENSION_NAME
                                               , pattern QCOM_TILE_SHADING_EXTENSION_NAME
                                               ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Dynamic (DeviceCmds(pVkCmdBeginPerTileExecutionQCOM))
import Vulkan.Dynamic (DeviceCmds(pVkCmdDispatchTileQCOM))
import Vulkan.Dynamic (DeviceCmds(pVkCmdEndPerTileExecutionQCOM))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPATCH_TILE_INFO_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PER_TILE_BEGIN_INFO_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PER_TILE_END_INFO_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_FEATURES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_PROPERTIES_QCOM))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDER_PASS_TILE_SHADING_CREATE_INFO_QCOM))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchTileQCOM
  :: FunPtr (Ptr CommandBuffer_T -> Ptr DispatchTileInfoQCOM -> IO ()) -> Ptr CommandBuffer_T -> Ptr DispatchTileInfoQCOM -> IO ()

-- | vkCmdDispatchTileQCOM - Dispatch per-tile work items
--
-- = Description
--
-- This command operates in the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>,
-- invoking a separate dispatch for each /covered tile/. The global
-- workgroup count and local workgroup size of each dispatch are defined by
-- the implementation to efficiently iterate over a uniform grid of pixel
-- blocks within the area of its /active tile/.
--
-- Each shader invocation operates on a single pixel block and its size is
-- determined by the shader’s tiling rate, which /must/ be defined by
-- shaders executed by this command. The @TileShadingRateQCOM@ execution
-- mode operand defines the shader’s tiling rate. Its @x@ and @y@ /must/ be
-- a power of two and less than or equal to the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxTileShadingRate maxTileShadingRate>
-- limit. Its @z@ /must/ be less than or equal to the active tile’s depth
-- as reported by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_properties VK_QCOM_tile_properties>,
-- and
-- 'Vulkan.Extensions.VK_QCOM_tile_properties.TilePropertiesQCOM'.tileSize.z
-- % @TileShadingRateQCOM@::@z@ /must/ equal @0@.
--
-- The start location of the shader invocation’s pixel block is
-- vec3(@TileOffsetQCOM@, 0) + (@GlobalInvocationId@ *
-- @TileShadingRateQCOM@)
--
-- Shader invocations /can/ perform tile attachment load\/store operations
-- at any location within the /active tile/, but the most efficient access
-- /may/ be limited to fragment locations within and local to the shader
-- invocation’s pixel block.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdDispatchTileQCOM-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR',
--     @reductionMode@ equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE',
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-magFilter-09598# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @reductionMode@ equal to either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR',
--     @reductionMode@ equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE',
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-mipmapMode-09599# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @reductionMode@ equal to either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-unnormalizedCoordinates-09635# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s @levelCount@ and @layerCount@ /must/ be 1
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08609# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s @viewType@ /must/ be
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08610# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08611# If a
--     'Vulkan.Core10.Handles.Sampler' created with
--     @unnormalizedCoordinates@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE' is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-07888# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor is accessed using atomic operations as a result of this
--     command, then the storage texel buffer’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-buffer-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-02693# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     extension is not enabled and any 'Vulkan.Core10.Handles.ImageView'
--     is sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a
--     result of this command, it /must/ not have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' of
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE', or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'
--
-- -   #VUID-vkCmdDispatchTileQCOM-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as a result of this
--     command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatchTileQCOM-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' with a reduction mode
--     of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdDispatchTileQCOM-cubicRangeClamp-09212# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-cubicRangeClamp cubicRangeClamp>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ not have a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   #VUID-vkCmdDispatchTileQCOM-reductionMode-09213# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with a
--     'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'::@reductionMode@
--     equal to
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--     as a result of this command /must/ sample with
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-selectableCubicWeights-09214# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-selectableCubicWeights selectableCubicWeights>
--     feature is not enabled, then any 'Vulkan.Core10.Handles.ImageView'
--     being sampled with 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT' as
--     a result of this command /must/ have
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.SamplerCubicWeightsCreateInfoQCOM'::@cubicWeights@
--     equal to
--     'Vulkan.Extensions.VK_QCOM_filter_cubic_weights.CUBIC_FILTER_WEIGHTS_CATMULL_ROM_QCOM'
--
-- -   #VUID-vkCmdDispatchTileQCOM-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpTypeImage-07027# For any
--     'Vulkan.Core10.Handles.ImageView' being written as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpTypeImage-07028# For any
--     'Vulkan.Core10.Handles.ImageView' being read as a storage image
--     where the image format field of the @OpTypeImage@ is @Unknown@, the
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpTypeImage-07029# For any
--     'Vulkan.Core10.Handles.BufferView' being written as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@, the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpTypeImage-07030# Any
--     'Vulkan.Core10.Handles.BufferView' being read as a storage texel
--     buffer where the image format field of the @OpTypeImage@ is
--     @Unknown@ then the view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkFormatProperties3 buffer features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08600# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline without the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a set /n/, a descriptor set
--     /must/ have been bound to /n/ at the same pipeline bind point, with
--     a 'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT' , as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08601# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline without the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a push constant value, that
--     value /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility compatible for push constants>
--     with the 'Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' array used to create the
--     current 'Vulkan.Extensions.Handles.ShaderEXT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-10068# For each array of resources
--     that is used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     the indices used to access members of the array /must/ be less than
--     the descriptor count for the identified binding in the descriptor
--     sets used by this command
--
-- -   #VUID-vkCmdDispatchTileQCOM-maintenance4-08602# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' without the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline without the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a push constant value, that
--     value /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility compatible for push constants>
--     with the 'Vulkan.Core10.Handles.PipelineLayout' used to create the
--     current 'Vulkan.Core10.Handles.Pipeline' or the
--     'Vulkan.Core10.Handles.DescriptorSetLayout' and
--     'Vulkan.Core10.PipelineLayout.PushConstantRange' arrays used to
--     create the current 'Vulkan.Extensions.Handles.ShaderEXT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08114# Descriptors in each bound
--     descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are accessed as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptor-validity descriptor validity>
--     by the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point used by this command and the bound
--     'Vulkan.Core10.Handles.Pipeline' was not created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-imageLayout-00344# If an image
--     descriptor is accessed by a shader, the
--     'Vulkan.Core10.Enums.ImageLayout.ImageLayout' /must/ match the
--     subresource accessible from the 'Vulkan.Core10.Handles.ImageView' as
--     defined by the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-layouts-matching-rule image layout matching rules>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08115# If the descriptors used by
--     the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point were specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', the
--     bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     without
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08116# Descriptors in bound
--     descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command and the bound 'Vulkan.Core10.Handles.Pipeline'
--     was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08604# Descriptors in bound
--     descriptor buffers, specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     /must/ be valid if they are dynamically used by any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08117# If the descriptors used by
--     the 'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind
--     point were specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.cmdSetDescriptorBufferOffsetsEXT',
--     the bound 'Vulkan.Core10.Handles.Pipeline' /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08119# If a descriptor is
--     dynamically used with a 'Vulkan.Core10.Handles.Pipeline' created
--     with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08605# If a descriptor is
--     dynamically used with a 'Vulkan.Extensions.Handles.ShaderEXT'
--     created with a 'Vulkan.Core10.Handles.DescriptorSetLayout' that was
--     created with
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     the descriptor memory /must/ be resident
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08606# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--     feature is not enabled, a valid pipeline /must/ be bound to the
--     pipeline bind point used by this command
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08608# If a pipeline is bound to
--     the pipeline bind point used by this command, there /must/ not have
--     been any calls to dynamic state setting commands for any state
--     specified statically in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command, since that
--     pipeline was bound
--
-- -   #VUID-vkCmdDispatchTileQCOM-uniformBuffers-06935# If any stage of
--     the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a uniform buffer, and that
--     stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @uniformBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08612# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDispatchTileQCOM-storageBuffers-06936# If any stage of
--     the 'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline
--     bind point used by this command accesses a storage buffer, and that
--     stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @storageBuffers@, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the buffer as specified in the descriptor set bound
--     to the same pipeline bind point
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-08613# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a storage
--     buffer, it /must/ not access values outside of the range of the
--     buffer as specified in the descriptor set bound to the same pipeline
--     bind point
--
-- -   #VUID-vkCmdDispatchTileQCOM-commandBuffer-02707# If @commandBuffer@
--     is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shaders>
--     /must/ not be a protected resource
--
-- -   #VUID-vkCmdDispatchTileQCOM-viewType-07752# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed as a result of this
--     command, then the image view’s @viewType@ /must/ match the @Dim@
--     operand of the @OpTypeImage@ as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-image-dimensions ???>
--
-- -   #VUID-vkCmdDispatchTileQCOM-format-07753# If a
--     'Vulkan.Core10.Handles.ImageView' or
--     'Vulkan.Core10.Handles.BufferView' is accessed as a result of this
--     command, then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-numericformat numeric type>
--     of the view’s @format@ and the @Sampled@ @Type@ operand of the
--     @OpTypeImage@ /must/ match
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageWrite-08795# If a
--     'Vulkan.Core10.Handles.ImageView' created with a format other than
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have at least as many
--     components as the image view’s format
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageWrite-08796# If a
--     'Vulkan.Core10.Handles.ImageView' created with the format
--     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM' is accessed using
--     @OpImageWrite@ as a result of this command, then the @Type@ of the
--     @Texel@ operand of that instruction /must/ have four components
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdDispatchTileQCOM-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchTileQCOM-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchTileQCOM-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdDispatchTileQCOM-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdDispatchTileQCOM-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchTileQCOM-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageSampleWeightedQCOM-06971# If
--     @OpImageSampleWeightedQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageSampleWeightedQCOM-06972# If
--     @OpImageSampleWeightedQCOM@ uses a 'Vulkan.Core10.Handles.ImageView'
--     as a sample weight image as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageBoxFilterQCOM-06973# If
--     @OpImageBoxFilterQCOM@ is used to sample a
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageBlockMatchSSDQCOM-06974# If
--     @OpImageBlockMatchSSDQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageBlockMatchSADQCOM-06975# If
--     @OpImageBlockMatchSADQCOM@ is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageBlockMatchSADQCOM-06976# If
--     @OpImageBlockMatchSADQCOM@ or OpImageBlockMatchSSDQCOM is used to
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageSampleWeightedQCOM-06977# If
--     @OpImageSampleWeightedQCOM@, @OpImageBoxFilterQCOM@,
--     @OpImageBlockMatchWindowSSDQCOM@, @OpImageBlockMatchWindowSADQCOM@,
--     @OpImageBlockMatchGatherSSDQCOM@, @OpImageBlockMatchGatherSADQCOM@,
--     @OpImageBlockMatchSSDQCOM@, or @OpImageBlockMatchSADQCOM@ uses a
--     'Vulkan.Core10.Handles.Sampler' as a result of this command, then
--     the sampler /must/ have been created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageSampleWeightedQCOM-06978# If any
--     command other than @OpImageSampleWeightedQCOM@,
--     @OpImageBoxFilterQCOM@, @OpImageBlockMatchWindowSSDQCOM@,
--     @OpImageBlockMatchWindowSADQCOM@, @OpImageBlockMatchGatherSSDQCOM@,
--     @OpImageBlockMatchGatherSADQCOM@, @OpImageBlockMatchSSDQCOM@, or
--     @OpImageBlockMatchSADQCOM@ uses a 'Vulkan.Core10.Handles.Sampler' as
--     a result of this command, then the sampler /must/ not have been
--     created with
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageBlockMatchWindow-09215# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM'
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageBlockMatchWindow-09216# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     instruction is used to read from an
--     'Vulkan.Core10.Handles.ImageView' as a result of this command, then
--     the image view’s format /must/ be a single-component format
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpImageBlockMatchWindow-09217# If a
--     @OpImageBlockMatchWindow*QCOM@ or @OpImageBlockMatchGather*QCOM@
--     read from a reference image as result of this command, then the
--     specified reference coordinates /must/ not fail
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-integer-coordinate-validation integer texel coordinate validation>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-07288# Any shader invocation
--     executed by this command /must/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-termination terminate>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-09600# If a descriptor with type
--     equal to any of
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     is accessed as a result of this command, all image subresources
--     identified by that descriptor /must/ be in the image layout
--     identified when the descriptor was written
--
-- -   #VUID-vkCmdDispatchTileQCOM-commandBuffer-10746# The
--     'Vulkan.Core10.Handles.DeviceMemory' object allocated from a
--     'Vulkan.Core10.DeviceInitialization.MemoryHeap' with the
--     'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
--     property that is bound to a resource accessed as a result of this
--     command /must/ be the active bound
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-bind-tile-memory bound tile memory object>
--     in @commandBuffer@
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-10678# If this command is recorded
--     inside a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-tile-shading tile shading render pass>
--     instance, the stages corresponding to the pipeline bind point used
--     by this command /must/ only include
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and\/or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-10679# If this command is recorded
--     where
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is enabled, there /must/ be no access to any image while the image
--     was be transitioned to the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     layout
--
-- -   #VUID-vkCmdDispatchTileQCOM-pDescription-09900# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the
--     underlying 'Vulkan.Extensions.Handles.TensorARM' object /must/ have
--     been created with the
--     'Vulkan.Extensions.VK_ARM_tensors.TENSOR_USAGE_SHADER_BIT_ARM' usage
--     flag set
--
-- -   #VUID-vkCmdDispatchTileQCOM-dimensionCount-09905# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the @Rank@
--     of the @OpTypeTensorARM@ of the tensor resource variable /must/ be
--     equal to the @dimensionCount@ provided via
--     'Vulkan.Extensions.VK_ARM_tensors.TensorCreateInfoARM'::@pDescription@
--     when creating the underlying 'Vulkan.Extensions.Handles.TensorARM'
--     object
--
-- -   #VUID-vkCmdDispatchTileQCOM-OpTypeTensorARM-09906# If a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_TENSOR_ARM'
--     descriptor is accessed as a result of this command, then the element
--     type of the @OpTypeTensorARM@ of the tensor resource variable /must/
--     be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-tensor-formats compatible>
--     with the 'Vulkan.Core10.Enums.Format.Format' of the
--     'Vulkan.Extensions.Handles.TensorViewARM' used for the access
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11297# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses a @OpTypeStruct@ decorated with @Block@ or
--     @BufferBlock@ using that mapping, the calculated offset for the
--     resource heap /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-bufferDescriptorAlignment bufferDescriptorAlignment>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11298# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses an @OpTypeImage@ or @OpTypeSampledImage@ using
--     that mapping, the calculated offset for the resource heap /must/ be
--     a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-imageDescriptorAlignment imageDescriptorAlignment>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11299# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses an @OpTypeSampler@ or @OpTypeSampledImage@
--     using that mapping, the calculated offset for the sampler heap
--     /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-samplerDescriptorAlignment samplerDescriptorAlignment>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11397# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses an @OpTypeTensorARM@ using that mapping, the
--     calculated offset for the resource heap /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-tensorDescriptorAlignment tensorDescriptorAlignment>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11300# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a
--     multiple of 4
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11301# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' backed by physical
--     memory at every offset specified by each mapping
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11302# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11304# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a
--     multiple of 8
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11305# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address at the expected location in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress' backed by physical
--     memory at every offset specified by each mapping
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11306# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the value of
--     the address pointed to by the address in push data /must/ be a valid
--     'Vulkan.Core10.FundamentalTypes.DeviceAddress'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11308# For each
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps descriptor heap>
--     that is statically used by
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding a bound shader>,
--     either directly or via a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>,
--     a valid descriptor heap /must/ be bound
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11309# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline with the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, execution of this command /must/ not result in any descriptor
--     read accessing data outside of the user range of the respective heap
--     bound by @vkCmdBind*HeapEXT@ commands
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11372# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a uniform buffer or uniform
--     texel buffer through a descriptor in the bound resource heap, that
--     stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @uniformBuffers@, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the descriptor specified via
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT'
--     when the descriptor was written
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11373# If any stage of the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a storage buffer or storage
--     texel buffer through a descriptor in the bound resource heap, that
--     stage was created without enabling either
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS'
--     or
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2'
--     for @storageBuffers@, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not enabled, and the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, that stage /must/ not access values outside
--     of the range of the descriptor specified by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT'
--     when the descriptor was written
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11374# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess2 robustBufferAccess2>
--     feature is not enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
--     feature is not enabled, and any
--     'Vulkan.Extensions.Handles.ShaderEXT' bound to a stage corresponding
--     to the pipeline bind point used by this command accesses a uniform
--     buffer, uniform texel buffer, storage buffer, or storage texel
--     buffer, that shader /must/ not access values outside of the range of
--     the buffer as specified by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DeviceAddressRangeEXT'
--     when the descriptor was written
--
-- -   #VUID-vkCmdDispatchTileQCOM-pBindInfo-11375# If any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     uses an embedded sampler via a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>,
--     the value of @pBindInfo->reservedRangeSize@ set for
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.cmdBindSamplerHeapEXT'
--     /must/ be greater than or equal to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minSamplerHeapReservedRangeWithEmbedded minSamplerHeapReservedRangeWithEmbedded>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11376# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created as a 'Vulkan.Extensions.Handles.ShaderEXT' with the
--     'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_DESCRIPTOR_HEAP_BIT_EXT'
--     flag or as part of a pipeline with the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     flag, and that shader statically uses a push constant value, that
--     value /must/ have been set by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.cmdPushDataEXT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11398# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-binding bound shader>
--     was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the access
--     /must/ not be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-execution-memory-access-bounds out of bounds>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11437# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a resource using that mapping, the buffer from
--     which the address in push data was queried /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11438# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a uniform buffer using that mapping, the
--     address that the uniform buffer is mapped to /must/ have been
--     queried from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_UNIFORM_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11441# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a uniform buffer using that mapping, the
--     address that the uniform buffer is mapped to /must/ be aligned to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minUniformBufferOffsetAlignment minUniformBufferOffsetAlignment>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11439# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a storage buffer using that mapping, the
--     address that the storage buffer is mapped to /must/ have been
--     queried from a buffer created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_BUFFER_BIT'
--     usage flag set
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11442# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses a storage buffer using that mapping, the
--     address that the storage buffer is mapped to /must/ be aligned to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minStorageBufferOffsetAlignment minStorageBufferOffsetAlignment>
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-11485# If a pipeline is bound to
--     the pipeline bind point used by this command, or shader is bound to
--     a shader stage used by this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     and a shader accesses an acceleration structure using that mapping,
--     the address that the acceleration structure is mapped to /must/ be
--     an acceleration structure address retrieved from a
--     'Vulkan.Extensions.Handles.AccelerationStructureKHR' object via
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.getAccelerationStructureDeviceAddressKHR'
--     or handle retrieved from a
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' object via
--     'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
--
-- -   #VUID-vkCmdDispatchTileQCOM-index-11450# If a shader uses a sampler
--     descriptor to sample an image as a result of this command, and that
--     sampler descriptor uses a custom border color with an index defined
--     by
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.SamplerCustomBorderColorIndexCreateInfoEXT',
--     the value of
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.SamplerCustomBorderColorIndexCreateInfoEXT'::@index@
--     /must/ have been registered before this command was recorded, and
--     still be registered during the sampling operation, with an
--     identically defined color
--
-- -   #VUID-vkCmdDispatchTileQCOM-protectedNoFault-11455# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, a pipeline is bound to the pipeline bind point
--     used by this command, or a shader is bound to a shader stage used by
--     this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     the address that the resource is mapped to /must/ have been queried
--     from a buffer created without the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     create flag set
--
-- -   #VUID-vkCmdDispatchTileQCOM-protectedNoFault-11456# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, a pipeline is bound to the pipeline bind point
--     used by this command, or a shader is bound to a shader stage used by
--     this command, and it was created with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     the address of the indirect memory /must/ have been queried from a
--     buffer created without the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'
--     create flag set
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-10672# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is not enabled, this command /must/ be called outside of a render
--     pass instance
--
-- -   #VUID-vkCmdDispatchTileQCOM-aspectMask-10673# If this command is
--     recorded where
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is enabled, and if the 'Vulkan.Core10.Handles.Pipeline' object bound
--     to the pipeline bind point used by this command writes to a variable
--     of storage class @Storage@ @Class@ @TileAttachmentQCOM@, the
--     corresponding 'Vulkan.Core10.Handles.ImageView' using /must/ not
--     have been created with an @aspectMask@ that contains
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-10674# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     is enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tileShadingPerTileDispatch tileShadingPerTileDispatch>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-10675# Memory backing image
--     subresources used as
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-tile-shading-attachment-access tile attachments>
--     in the current render pass /must/ not be written in any way other
--     than as a tile attachment by this command
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-10676# If any recorded command in
--     the current subpass will write to an image subresource as a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-tile-shading-attachment-access tile attachment>,
--     this command /must/ not read from the memory backing that image
--     subresource in any other way than as a tile attachment
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-10743# If there is no bound compute
--     pipeline, a valid 'Vulkan.Extensions.Handles.ShaderEXT' /must/ be
--     bound to the
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--     stage
--
-- -   #VUID-vkCmdDispatchTileQCOM-commandBuffer-02712# If @commandBuffer@
--     is a protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource written to by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be an unprotected resource
--
-- -   #VUID-vkCmdDispatchTileQCOM-commandBuffer-02713# If @commandBuffer@
--     is a protected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, pipeline stages other than the framebuffer-space
--     and compute stages in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command /must/ not
--     write to any resource
--
-- -   #VUID-vkCmdDispatchTileQCOM-commandBuffer-04617# If any of the
--     shader stages of the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command uses the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RayQueryKHR RayQueryKHR>
--     capability, then @commandBuffer@ /must/ not be a protected command
--     buffer
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-10668# When this command is
--     recorded
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
--     /must/ be enabled
--
-- -   #VUID-vkCmdDispatchTileQCOM-None-10669# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tileShadingDispatchTile tileShadingDispatchTile>
--     /must/ enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdDispatchTileQCOM-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdDispatchTileQCOM-pDispatchTileInfo-parameter#
--     @pDispatchTileInfo@ /must/ be a valid pointer to a valid
--     'DispatchTileInfoQCOM' structure
--
-- -   #VUID-vkCmdDispatchTileQCOM-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdDispatchTileQCOM-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT' operations
--
-- -   #VUID-vkCmdDispatchTileQCOM-renderpass# This command /must/ only be
--     called inside of a render pass instance
--
-- -   #VUID-vkCmdDispatchTileQCOM-suspended# This command /must/ not be
--     called between suspended render pass instances
--
-- -   #VUID-vkCmdDispatchTileQCOM-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | Action                                                                                                                                 |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdDispatchTileQCOM is affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_shading VK_QCOM_tile_shading>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'DispatchTileInfoQCOM'
cmdDispatchTileQCOM :: forall io
                     . (MonadIO io)
                    => -- | @commandBuffer@ is the command buffer into which the command will be
                       -- recorded.
                       CommandBuffer
                    -> -- | @pDispatchTileInfo@ is a pointer to a 'DispatchTileInfoQCOM' structure
                       -- containing information about the area-based dispatch.
                       DispatchTileInfoQCOM
                    -> io ()
cmdDispatchTileQCOM commandBuffer dispatchTileInfo = liftIO . evalContT $ do
  let vkCmdDispatchTileQCOMPtr = pVkCmdDispatchTileQCOM (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdDispatchTileQCOMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdDispatchTileQCOM is null" Nothing Nothing
  let vkCmdDispatchTileQCOM' = mkVkCmdDispatchTileQCOM vkCmdDispatchTileQCOMPtr
  pDispatchTileInfo <- ContT $ withCStruct (dispatchTileInfo)
  lift $ traceAroundEvent "vkCmdDispatchTileQCOM" (vkCmdDispatchTileQCOM'
                                                     (commandBufferHandle (commandBuffer))
                                                     pDispatchTileInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginPerTileExecutionQCOM
  :: FunPtr (Ptr CommandBuffer_T -> Ptr PerTileBeginInfoQCOM -> IO ()) -> Ptr CommandBuffer_T -> Ptr PerTileBeginInfoQCOM -> IO ()

-- | vkCmdBeginPerTileExecutionQCOM - Begin per-tile execution mode
--
-- = Description
--
-- When /per-tile execution model/ is enabled, recorded @vkCmdDraw*@ or
-- @vkCmdDispatch*@ commands are invoked per tile. That is, the recorded
-- draw or dispatch is invoked exactly once for each /covered tile/. The
-- set of /covered tiles/ for a given render pass instance consists of the
-- set of render pass tiles, which /can/ be queried with
-- @VK_QCOM_tile_properties@, that are completely or partially covered by
-- the @renderArea@ for the render pass instance. The draw or dispatch
-- commands /may/ be invoked for uncovered tiles.
--
-- Each per-tile command invocation is associated with a single tile, the
-- /active tile/. These per-tile invocations are not specified to execute
-- in any particular order, but the size and offset of the /active tile/ is
-- available via shader built-ins.
--
-- When /per-tile execution model/ is enabled, the following restrictions
-- apply:
--
-- -   Transform feedback commands such as
--     'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT',
--     'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT',
--     'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginQueryIndexedEXT',
--     and
--     'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT',
--     /must/ not be recorded.
--
-- -   Query commands such as
--     'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp',
--     'Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerBeginEXT',
--     'Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerEndEXT',
--     'Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerInsertEXT',
--     'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery', and
--     'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery', /must/ not be
--     recorded.
--
-- -   Event commands such as
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWaitEvents2'
--     and 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents' /must/ not
--     be recorded.
--
-- -   Render pass clears like
--     'Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments' /must/ not
--     be recorded
--
-- -   Access of an attachment with layout
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_ATTACHMENT_FEEDBACK_LOOP_OPTIMAL_EXT'
--     as provided by @VK_EXT_attachment_feedback_loop_layout@ is
--     disallowed
--
-- -   Any commands that would cause a invocations of one of the following
--     shader stages are not allowed
--
--     -   tessellation
--
--     -   geometry
--
--     -   ray tracing
--
--     -   mesh shading
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBeginPerTileExecutionQCOM-None-10664# The current render
--     pass /must/ be a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-tile-shading tile shading render pass>
--
-- -   #VUID-vkCmdBeginPerTileExecutionQCOM-None-10665# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tileShadingPerTileDispatch tileShadingPerTileDispatch>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tileShadingPerTileDraw tileShadingPerTileDraw>
--     feature must be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBeginPerTileExecutionQCOM-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBeginPerTileExecutionQCOM-pPerTileBeginInfo-parameter#
--     @pPerTileBeginInfo@ /must/ be a valid pointer to a valid
--     'PerTileBeginInfoQCOM' structure
--
-- -   #VUID-vkCmdBeginPerTileExecutionQCOM-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBeginPerTileExecutionQCOM-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdBeginPerTileExecutionQCOM-renderpass# This command /must/
--     only be called inside of a render pass instance
--
-- -   #VUID-vkCmdBeginPerTileExecutionQCOM-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdBeginPerTileExecutionQCOM is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_shading VK_QCOM_tile_shading>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'PerTileBeginInfoQCOM'
cmdBeginPerTileExecutionQCOM :: forall io
                              . (MonadIO io)
                             => -- | @commandBuffer@ is the command buffer in which to record the command.
                                CommandBuffer
                             -> -- | @pPerTileBeginInfo@ is a pointer to a 'PerTileBeginInfoQCOM' structure
                                -- containing information about how the /per-tile execution model/ is
                                -- started.
                                PerTileBeginInfoQCOM
                             -> io ()
cmdBeginPerTileExecutionQCOM commandBuffer
                               perTileBeginInfo = liftIO . evalContT $ do
  let vkCmdBeginPerTileExecutionQCOMPtr = pVkCmdBeginPerTileExecutionQCOM (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdBeginPerTileExecutionQCOMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBeginPerTileExecutionQCOM is null" Nothing Nothing
  let vkCmdBeginPerTileExecutionQCOM' = mkVkCmdBeginPerTileExecutionQCOM vkCmdBeginPerTileExecutionQCOMPtr
  pPerTileBeginInfo <- ContT $ withCStruct (perTileBeginInfo)
  lift $ traceAroundEvent "vkCmdBeginPerTileExecutionQCOM" (vkCmdBeginPerTileExecutionQCOM'
                                                              (commandBufferHandle (commandBuffer))
                                                              pPerTileBeginInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndPerTileExecutionQCOM
  :: FunPtr (Ptr CommandBuffer_T -> Ptr PerTileEndInfoQCOM -> IO ()) -> Ptr CommandBuffer_T -> Ptr PerTileEndInfoQCOM -> IO ()

-- | vkCmdEndPerTileExecutionQCOM - End per-tile execution mode
--
-- = Description
--
-- This command disables /per-tile execution model/.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdEndPerTileExecutionQCOM-None-10666# The /per-tile
--     execution model/ /must/ have been enabled in the current render pass
--
-- -   #VUID-vkCmdEndPerTileExecutionQCOM-None-10667# The current render
--     pass /must/ be a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-tile-shading tile shading render pass>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdEndPerTileExecutionQCOM-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdEndPerTileExecutionQCOM-pPerTileEndInfo-parameter#
--     @pPerTileEndInfo@ /must/ be a valid pointer to a valid
--     'PerTileEndInfoQCOM' structure
--
-- -   #VUID-vkCmdEndPerTileExecutionQCOM-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdEndPerTileExecutionQCOM-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT', or
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' operations
--
-- -   #VUID-vkCmdEndPerTileExecutionQCOM-renderpass# This command /must/
--     only be called inside of a render pass instance
--
-- -   #VUID-vkCmdEndPerTileExecutionQCOM-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Inside                                                                                                                 | Outside                                                                                                                     | VK_QUEUE_COMPUTE_BIT                                                                                                  | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             | VK_QUEUE_GRAPHICS_BIT                                                                                                 |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- == Conditional Rendering
--
-- vkCmdEndPerTileExecutionQCOM is not affected by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing-conditional-rendering conditional rendering>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_shading VK_QCOM_tile_shading>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'PerTileEndInfoQCOM'
cmdEndPerTileExecutionQCOM :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer in which to record the command.
                              CommandBuffer
                           -> -- | @pPerTileEndInfo@ is a pointer to a 'PerTileEndInfoQCOM' structure
                              -- containing information about how the /per-tile execution model/ is
                              -- ended.
                              PerTileEndInfoQCOM
                           -> io ()
cmdEndPerTileExecutionQCOM commandBuffer
                             perTileEndInfo = liftIO . evalContT $ do
  let vkCmdEndPerTileExecutionQCOMPtr = pVkCmdEndPerTileExecutionQCOM (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdEndPerTileExecutionQCOMPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdEndPerTileExecutionQCOM is null" Nothing Nothing
  let vkCmdEndPerTileExecutionQCOM' = mkVkCmdEndPerTileExecutionQCOM vkCmdEndPerTileExecutionQCOMPtr
  pPerTileEndInfo <- ContT $ withCStruct (perTileEndInfo)
  lift $ traceAroundEvent "vkCmdEndPerTileExecutionQCOM" (vkCmdEndPerTileExecutionQCOM'
                                                            (commandBufferHandle (commandBuffer))
                                                            pPerTileEndInfo)
  pure $ ()


-- | VkPhysicalDeviceTileShadingFeaturesQCOM - Structure describing tile
-- shading features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceTileShadingFeaturesQCOM' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDeviceTileShadingFeaturesQCOM', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_shading VK_QCOM_tile_shading>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTileShadingFeaturesQCOM = PhysicalDeviceTileShadingFeaturesQCOM
  { -- | #features-tileShading# @tileShading@ indicates that the implementation
    -- supports
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-tile-shading tile shading render pass>
    -- instances.
    tileShading :: Bool
  , -- | #features-tileShadingFragmentStage# @tileShadingFragmentStage@ indicates
    -- that the implementation supports tile shading in the fragment stage.
    tileShadingFragmentStage :: Bool
  , -- | #features-tileShadingColorAttachments# @tileShadingColorAttachments@
    -- indicates that the implementation supports access to color attachments
    -- in a tile shader.
    tileShadingColorAttachments :: Bool
  , -- | #features-tileShadingDepthAttachments# @tileShadingDepthAttachments@
    -- indicates that the implementation supports access to depth aspect of
    -- depth stencil attachments.
    tileShadingDepthAttachments :: Bool
  , -- | #features-tileShadingStencilAttachments# @tileShadingStencilAttachments@
    -- indicates that the implementation supports access to stencil aspect of
    -- depth stencil attachments.
    tileShadingStencilAttachments :: Bool
  , -- | #features-tileShadingInputAttachments# @tileShadingInputAttachments@
    -- indicates that the implementation supports access to input attachments.
    tileShadingInputAttachments :: Bool
  , -- | #features-tileShadingSampledAttachments# @tileShadingSampledAttachments@
    -- indicates that the implementation supports access to sampling of tile
    -- attachments.
    tileShadingSampledAttachments :: Bool
  , -- | #features-tileShadingPerTileDraw# @tileShadingPerTileDraw@ indicates
    -- that the implementation supports the recording of vkCmdDraw* commands
    -- when
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
    -- is enabled.
    tileShadingPerTileDraw :: Bool
  , -- | #features-tileShadingPerTileDispatch# @tileShadingPerTileDispatch@
    -- indicates that the implementation supports the recording of
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatch'* commands within those
    -- regions of a command buffer where the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution model>
    -- is enabled.
    tileShadingPerTileDispatch :: Bool
  , -- | #features-tileShadingDispatchTile# @tileShadingDispatchTile@ indicates
    -- that the implementation supports the recording of 'cmdDispatchTileQCOM'
    -- commands.
    tileShadingDispatchTile :: Bool
  , -- | #features-tileShadingApron# @tileShadingApron@ indicates that the
    -- implementation supports
    -- 'RenderPassTileShadingCreateInfoQCOM'::@apronSize@ value other than
    -- (0,0). See
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-tile-shading-aprons Tiling Aprons>
    -- for more information.
    tileShadingApron :: Bool
  , -- | #features-tileShadingAnisotropicApron# @tileShadingAnisotropicApron@
    -- indicates that the implementation supports
    -- 'RenderPassTileShadingCreateInfoQCOM'::@apronSize@ set to a value where
    -- @apronSize.width@ differs from @apronSize.height@.
    tileShadingAnisotropicApron :: Bool
  , -- | #features-tileShadingAtomicOps# @tileShadingAtomicOps@ indicates that
    -- the implementation supports atomic operations on
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-tile-attachment tile attachment variables>.
    tileShadingAtomicOps :: Bool
  , -- | #features-tileShadingImageProcessing# @tileShadingImageProcessing@
    -- indicates that the implementation supports
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#textures-weightimage image processing operations>
    -- with tile attachments.
    tileShadingImageProcessing :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTileShadingFeaturesQCOM)
#endif
deriving instance Show PhysicalDeviceTileShadingFeaturesQCOM

instance ToCStruct PhysicalDeviceTileShadingFeaturesQCOM where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTileShadingFeaturesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (tileShading))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (tileShadingFragmentStage))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (tileShadingColorAttachments))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (tileShadingDepthAttachments))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (tileShadingStencilAttachments))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (tileShadingInputAttachments))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (tileShadingSampledAttachments))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (tileShadingPerTileDraw))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (tileShadingPerTileDispatch))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (tileShadingDispatchTile))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (tileShadingApron))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (tileShadingAnisotropicApron))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (tileShadingAtomicOps))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (tileShadingImageProcessing))
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_FEATURES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceTileShadingFeaturesQCOM where
  peekCStruct p = do
    tileShading <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    tileShadingFragmentStage <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    tileShadingColorAttachments <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    tileShadingDepthAttachments <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    tileShadingStencilAttachments <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    tileShadingInputAttachments <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    tileShadingSampledAttachments <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    tileShadingPerTileDraw <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    tileShadingPerTileDispatch <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    tileShadingDispatchTile <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    tileShadingApron <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    tileShadingAnisotropicApron <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    tileShadingAtomicOps <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    tileShadingImageProcessing <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    pure $ PhysicalDeviceTileShadingFeaturesQCOM
             (bool32ToBool tileShading)
             (bool32ToBool tileShadingFragmentStage)
             (bool32ToBool tileShadingColorAttachments)
             (bool32ToBool tileShadingDepthAttachments)
             (bool32ToBool tileShadingStencilAttachments)
             (bool32ToBool tileShadingInputAttachments)
             (bool32ToBool tileShadingSampledAttachments)
             (bool32ToBool tileShadingPerTileDraw)
             (bool32ToBool tileShadingPerTileDispatch)
             (bool32ToBool tileShadingDispatchTile)
             (bool32ToBool tileShadingApron)
             (bool32ToBool tileShadingAnisotropicApron)
             (bool32ToBool tileShadingAtomicOps)
             (bool32ToBool tileShadingImageProcessing)

instance Storable PhysicalDeviceTileShadingFeaturesQCOM where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTileShadingFeaturesQCOM where
  zero = PhysicalDeviceTileShadingFeaturesQCOM
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceTileShadingPropertiesQCOM - Structure describing
-- properties supported by VK_QCOM_tile_shading
--
-- = Description
--
-- If the 'PhysicalDeviceTileShadingPropertiesQCOM' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_shading VK_QCOM_tile_shading>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceTileShadingPropertiesQCOM = PhysicalDeviceTileShadingPropertiesQCOM
  { -- | #limits-maxApronSize# @maxApronSize@ is the maximum value supported
    -- which can be specified for
    -- 'RenderPassTileShadingCreateInfoQCOM'::@apronSize@ or @width@ and
    -- @height@.
    maxApronSize :: Word32
  , -- | #limits-preferNonCoherent# @preferNonCoherent@ indicates that the
    -- implementation prefers tile attachments declared in shaders with the
    -- @NonCoherentTileAttachmentReadQCOM@ decoration. Use of the decoration
    -- /may/ offer performance or power advantages.
    preferNonCoherent :: Bool
  , -- | #limits-tileGranularity# @tileGranularity@ provides a guarantee on the
    -- granularity of each tile. Each tile will have dimensions that are a
    -- multiple of this granularity in width and height.
    tileGranularity :: Extent2D
  , -- | #limits-maxTileShadingRate# @maxTileShadingRate@ is the maximum value of
    -- @TileShadingRateQCOM@ and /must/ be a power of 2.
    maxTileShadingRate :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceTileShadingPropertiesQCOM)
#endif
deriving instance Show PhysicalDeviceTileShadingPropertiesQCOM

instance ToCStruct PhysicalDeviceTileShadingPropertiesQCOM where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceTileShadingPropertiesQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxApronSize)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (preferNonCoherent))
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (tileGranularity)
    poke ((p `plusPtr` 32 :: Ptr Extent2D)) (maxTileShadingRate)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TILE_SHADING_PROPERTIES_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Extent2D)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Extent2D)) (zero)
    f

instance FromCStruct PhysicalDeviceTileShadingPropertiesQCOM where
  peekCStruct p = do
    maxApronSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    preferNonCoherent <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    tileGranularity <- peekCStruct @Extent2D ((p `plusPtr` 24 :: Ptr Extent2D))
    maxTileShadingRate <- peekCStruct @Extent2D ((p `plusPtr` 32 :: Ptr Extent2D))
    pure $ PhysicalDeviceTileShadingPropertiesQCOM
             maxApronSize
             (bool32ToBool preferNonCoherent)
             tileGranularity
             maxTileShadingRate

instance Storable PhysicalDeviceTileShadingPropertiesQCOM where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceTileShadingPropertiesQCOM where
  zero = PhysicalDeviceTileShadingPropertiesQCOM
           zero
           zero
           zero
           zero


-- | VkRenderPassTileShadingCreateInfoQCOM - Structure specifying, tile
-- shading information for a render pass object.
--
-- = Description
--
-- If this structure is not present, the render pass will have @flags@ set
-- to @0@ and @tileApronSize@ is set to @(0,0)@.
--
-- == Valid Usage
--
-- -   #VUID-VkRenderPassTileShadingCreateInfoQCOM-tileShading-10658# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tileShading tileShading>
--     feature is not enabled, 'TILE_SHADING_RENDER_PASS_ENABLE_BIT_QCOM'
--     /must/ not be included in @flags@
--
-- -   #VUID-VkRenderPassTileShadingCreateInfoQCOM-flags-10659# If
--     'TILE_SHADING_RENDER_PASS_ENABLE_BIT_QCOM' is not included in
--     @flags@ or the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tileShadingApron tileShadingApron>
--     feature is not enabled, @tileApronSize@ /must/ be @(0,0)@
--
-- -   #VUID-VkRenderPassTileShadingCreateInfoQCOM-flags-10660# If
--     'TILE_SHADING_RENDER_PASS_ENABLE_BIT_QCOM' is not included in
--     @flags@, or neither the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tileShadingPerTileDispatch tileShadingPerTileDispatch>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tileShadingPerTileDraw tileShadingPerTileDraw>
--     features are enabled, @flags@ /must/ not include
--     'TILE_SHADING_RENDER_PASS_PER_TILE_EXECUTION_BIT_QCOM'
--
-- -   #VUID-VkRenderPassTileShadingCreateInfoQCOM-tileShadingAnisotropicApron-10661#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tileShadingAnisotropicApron tileShadingAnisotropicApron>
--     feature is not enabled, @tileApronSize.x@ and /must/ be equal to
--     @tileApronSize.y@
--
-- -   #VUID-VkRenderPassTileShadingCreateInfoQCOM-tileApronSize-10662#
--     @tileApronSize.x@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxApronSize maxApronSize>
--
-- -   #VUID-VkRenderPassTileShadingCreateInfoQCOM-tileApronSize-10663#
--     @tileApronSize.y@ /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxApronSize maxApronSize>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRenderPassTileShadingCreateInfoQCOM-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDER_PASS_TILE_SHADING_CREATE_INFO_QCOM'
--
-- -   #VUID-VkRenderPassTileShadingCreateInfoQCOM-flags-parameter# @flags@
--     /must/ be a valid combination of 'TileShadingRenderPassFlagBitsQCOM'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_shading VK_QCOM_tile_shading>,
-- 'Vulkan.Core10.FundamentalTypes.Extent2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'TileShadingRenderPassFlagsQCOM'
data RenderPassTileShadingCreateInfoQCOM = RenderPassTileShadingCreateInfoQCOM
  { -- | @flags@ is a bitmask of 'TileShadingRenderPassFlagBitsQCOM'.
    flags :: TileShadingRenderPassFlagsQCOM
  , -- | @tileApronSize@ is a 'Vulkan.Core10.FundamentalTypes.Extent2D'
    -- describing the is size of the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-tile-shading-aprons tiling apron>
    -- in each dimension.
    tileApronSize :: Extent2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderPassTileShadingCreateInfoQCOM)
#endif
deriving instance Show RenderPassTileShadingCreateInfoQCOM

instance ToCStruct RenderPassTileShadingCreateInfoQCOM where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderPassTileShadingCreateInfoQCOM{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_TILE_SHADING_CREATE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr TileShadingRenderPassFlagsQCOM)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Extent2D)) (tileApronSize)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDER_PASS_TILE_SHADING_CREATE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct RenderPassTileShadingCreateInfoQCOM where
  peekCStruct p = do
    flags <- peek @TileShadingRenderPassFlagsQCOM ((p `plusPtr` 16 :: Ptr TileShadingRenderPassFlagsQCOM))
    tileApronSize <- peekCStruct @Extent2D ((p `plusPtr` 20 :: Ptr Extent2D))
    pure $ RenderPassTileShadingCreateInfoQCOM
             flags tileApronSize

instance Storable RenderPassTileShadingCreateInfoQCOM where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RenderPassTileShadingCreateInfoQCOM where
  zero = RenderPassTileShadingCreateInfoQCOM
           zero
           zero


-- | VkPerTileBeginInfoQCOM - Structure specifying per-tile begin information
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_shading VK_QCOM_tile_shading>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBeginPerTileExecutionQCOM'
data PerTileBeginInfoQCOM = PerTileBeginInfoQCOM
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerTileBeginInfoQCOM)
#endif
deriving instance Show PerTileBeginInfoQCOM

instance ToCStruct PerTileBeginInfoQCOM where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerTileBeginInfoQCOM f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PER_TILE_BEGIN_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PER_TILE_BEGIN_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PerTileBeginInfoQCOM where
  peekCStruct _ = pure $ PerTileBeginInfoQCOM
                           

instance Storable PerTileBeginInfoQCOM where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerTileBeginInfoQCOM where
  zero = PerTileBeginInfoQCOM
           


-- | VkPerTileEndInfoQCOM - Structure specifying per-tile end information
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_shading VK_QCOM_tile_shading>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdEndPerTileExecutionQCOM'
data PerTileEndInfoQCOM = PerTileEndInfoQCOM
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PerTileEndInfoQCOM)
#endif
deriving instance Show PerTileEndInfoQCOM

instance ToCStruct PerTileEndInfoQCOM where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PerTileEndInfoQCOM f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PER_TILE_END_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PER_TILE_END_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PerTileEndInfoQCOM where
  peekCStruct _ = pure $ PerTileEndInfoQCOM
                           

instance Storable PerTileEndInfoQCOM where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PerTileEndInfoQCOM where
  zero = PerTileEndInfoQCOM
           


-- | VkDispatchTileInfoQCOM - Structure specifying dispatch tile info
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_shading VK_QCOM_tile_shading>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'cmdDispatchTileQCOM'
data DispatchTileInfoQCOM = DispatchTileInfoQCOM
  {}
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DispatchTileInfoQCOM)
#endif
deriving instance Show DispatchTileInfoQCOM

instance ToCStruct DispatchTileInfoQCOM where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DispatchTileInfoQCOM f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPATCH_TILE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPATCH_TILE_INFO_QCOM)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct DispatchTileInfoQCOM where
  peekCStruct _ = pure $ DispatchTileInfoQCOM
                           

instance Storable DispatchTileInfoQCOM where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DispatchTileInfoQCOM where
  zero = DispatchTileInfoQCOM
           


type TileShadingRenderPassFlagsQCOM = TileShadingRenderPassFlagBitsQCOM

-- | VkTileShadingRenderPassFlagBitsQCOM - Bitmask specifying flags for tile
-- shading
--
-- = Description
--
-- -   'TILE_SHADING_RENDER_PASS_ENABLE_BIT_QCOM' specifies that the render
--     pass has tile shading enabled.
--
-- -   'TILE_SHADING_RENDER_PASS_PER_TILE_EXECUTION_BIT_QCOM' specifies
--     that the secondary command buffer will be executed within a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-per-tile-execution-model per-tile execution block>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QCOM_tile_shading VK_QCOM_tile_shading>,
-- 'TileShadingRenderPassFlagsQCOM'
newtype TileShadingRenderPassFlagBitsQCOM = TileShadingRenderPassFlagBitsQCOM Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkTileShadingRenderPassFlagBitsQCOM" "VK_TILE_SHADING_RENDER_PASS_ENABLE_BIT_QCOM"
pattern TILE_SHADING_RENDER_PASS_ENABLE_BIT_QCOM = TileShadingRenderPassFlagBitsQCOM 0x00000001

-- No documentation found for Nested "VkTileShadingRenderPassFlagBitsQCOM" "VK_TILE_SHADING_RENDER_PASS_PER_TILE_EXECUTION_BIT_QCOM"
pattern TILE_SHADING_RENDER_PASS_PER_TILE_EXECUTION_BIT_QCOM = TileShadingRenderPassFlagBitsQCOM 0x00000002

conNameTileShadingRenderPassFlagBitsQCOM :: String
conNameTileShadingRenderPassFlagBitsQCOM = "TileShadingRenderPassFlagBitsQCOM"

enumPrefixTileShadingRenderPassFlagBitsQCOM :: String
enumPrefixTileShadingRenderPassFlagBitsQCOM = "TILE_SHADING_RENDER_PASS_"

showTableTileShadingRenderPassFlagBitsQCOM :: [(TileShadingRenderPassFlagBitsQCOM, String)]
showTableTileShadingRenderPassFlagBitsQCOM =
  [
    ( TILE_SHADING_RENDER_PASS_ENABLE_BIT_QCOM
    , "ENABLE_BIT_QCOM"
    )
  ,
    ( TILE_SHADING_RENDER_PASS_PER_TILE_EXECUTION_BIT_QCOM
    , "PER_TILE_EXECUTION_BIT_QCOM"
    )
  ]

instance Show TileShadingRenderPassFlagBitsQCOM where
  showsPrec =
    enumShowsPrec
      enumPrefixTileShadingRenderPassFlagBitsQCOM
      showTableTileShadingRenderPassFlagBitsQCOM
      conNameTileShadingRenderPassFlagBitsQCOM
      (\(TileShadingRenderPassFlagBitsQCOM x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read TileShadingRenderPassFlagBitsQCOM where
  readPrec =
    enumReadPrec
      enumPrefixTileShadingRenderPassFlagBitsQCOM
      showTableTileShadingRenderPassFlagBitsQCOM
      conNameTileShadingRenderPassFlagBitsQCOM
      TileShadingRenderPassFlagBitsQCOM

type QCOM_TILE_SHADING_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_QCOM_TILE_SHADING_SPEC_VERSION"
pattern QCOM_TILE_SHADING_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_TILE_SHADING_SPEC_VERSION = 2


type QCOM_TILE_SHADING_EXTENSION_NAME = "VK_QCOM_tile_shading"

-- No documentation found for TopLevel "VK_QCOM_TILE_SHADING_EXTENSION_NAME"
pattern QCOM_TILE_SHADING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_TILE_SHADING_EXTENSION_NAME = "VK_QCOM_tile_shading"

