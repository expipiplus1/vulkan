{-# language CPP #-}
-- | = Name
--
-- VK_EXT_extended_dynamic_state3 - device extension
--
-- == VK_EXT_extended_dynamic_state3
--
-- [__Name String__]
--     @VK_EXT_extended_dynamic_state3@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     456
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_NV_clip_space_w_scaling
--
--     -   Interacts with VK_NV_coverage_reduction_mode
--
--     -   Interacts with VK_NV_fragment_coverage_to_color
--
--     -   Interacts with VK_NV_framebuffer_mixed_samples
--
--     -   Interacts with VK_NV_representative_fragment_test
--
--     -   Interacts with VK_NV_shading_rate_image
--
--     -   Interacts with VK_NV_viewport_swizzle
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_extended_dynamic_state3] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_extended_dynamic_state3 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_extended_dynamic_state3.adoc VK_EXT_extended_dynamic_state3>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-09-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Story, Nintendo
--
--     -   Jamie Madill, Google
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Faith Ekstrand, Collabora
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Ricardo Garcia, Igalia
--
--     -   Samuel Pitoiset, Valve
--
--     -   Shahbaz Youssefi, Google
--
--     -   Stu Smith, AMD
--
--     -   Tapani Pälli, Intel
--
-- == Description
--
-- This extension adds almost all of the remaining pipeline state as
-- dynamic state to help applications further reduce the number of
-- monolithic pipelines they need to create and bind.
--
-- == New Commands
--
-- -   'cmdSetAlphaToCoverageEnableEXT'
--
-- -   'cmdSetAlphaToOneEnableEXT'
--
-- -   'cmdSetColorBlendAdvancedEXT'
--
-- -   'cmdSetColorBlendEnableEXT'
--
-- -   'cmdSetColorBlendEquationEXT'
--
-- -   'cmdSetColorWriteMaskEXT'
--
-- -   'cmdSetConservativeRasterizationModeEXT'
--
-- -   'cmdSetDepthClampEnableEXT'
--
-- -   'cmdSetDepthClipEnableEXT'
--
-- -   'cmdSetDepthClipNegativeOneToOneEXT'
--
-- -   'cmdSetExtraPrimitiveOverestimationSizeEXT'
--
-- -   'cmdSetLineRasterizationModeEXT'
--
-- -   'cmdSetLineStippleEnableEXT'
--
-- -   'cmdSetLogicOpEnableEXT'
--
-- -   'cmdSetPolygonModeEXT'
--
-- -   'cmdSetProvokingVertexModeEXT'
--
-- -   'cmdSetRasterizationSamplesEXT'
--
-- -   'cmdSetRasterizationStreamEXT'
--
-- -   'cmdSetSampleLocationsEnableEXT'
--
-- -   'cmdSetSampleMaskEXT'
--
-- -   'cmdSetTessellationDomainOriginEXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_clip_space_w_scaling VK_NV_clip_space_w_scaling>
-- is supported:
--
-- -   'cmdSetViewportWScalingEnableNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode VK_NV_coverage_reduction_mode>
-- is supported:
--
-- -   'cmdSetCoverageReductionModeNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_fragment_coverage_to_color VK_NV_fragment_coverage_to_color>
-- is supported:
--
-- -   'cmdSetCoverageToColorEnableNV'
--
-- -   'cmdSetCoverageToColorLocationNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>
-- is supported:
--
-- -   'cmdSetCoverageModulationModeNV'
--
-- -   'cmdSetCoverageModulationTableEnableNV'
--
-- -   'cmdSetCoverageModulationTableNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_representative_fragment_test VK_NV_representative_fragment_test>
-- is supported:
--
-- -   'cmdSetRepresentativeFragmentTestEnableNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shading_rate_image VK_NV_shading_rate_image>
-- is supported:
--
-- -   'cmdSetShadingRateImageEnableNV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_viewport_swizzle VK_NV_viewport_swizzle>
-- is supported:
--
-- -   'cmdSetViewportSwizzleNV'
--
-- == New Structures
--
-- -   'ColorBlendAdvancedEXT'
--
-- -   'ColorBlendEquationEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExtendedDynamicState3FeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceExtendedDynamicState3PropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME'
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_3_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CONSERVATIVE_RASTERIZATION_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_NEGATIVE_ONE_TO_ONE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXTRA_PRIMITIVE_OVERESTIMATION_SIZE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_POLYGON_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PROVOKING_VERTEX_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_STREAM_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_TESSELLATION_DOMAIN_ORIGIN_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_PROPERTIES_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_clip_space_w_scaling VK_NV_clip_space_w_scaling>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_ENABLE_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode VK_NV_coverage_reduction_mode>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_REDUCTION_MODE_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_fragment_coverage_to_color VK_NV_fragment_coverage_to_color>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_LOCATION_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_MODE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_ENABLE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_representative_fragment_test VK_NV_representative_fragment_test>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_REPRESENTATIVE_FRAGMENT_TEST_ENABLE_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shading_rate_image VK_NV_shading_rate_image>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SHADING_RATE_IMAGE_ENABLE_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_viewport_swizzle VK_NV_viewport_swizzle>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV'
--
-- == Issues
--
-- 1) What about the VkPipelineMultisampleStateCreateInfo state
-- @sampleShadingEnable@ and @minSampleShading@?
--
-- [__UNRESOLVED__]
--
--     -   @sampleShadingEnable@ and @minSampleShading@ are required when
--         compiling the fragment shader, and it is not meaningful to set
--         them dynamically since they always need to match the fragment
--         shader state, so this hardware state may as well just come from
--         the pipeline with the fragment shader.
--
-- == Version History
--
-- -   Revision 2, 2022-07-18 (Piers Daniell)
--
--     -   Added rasterizationSamples
--
-- -   Revision 1, 2022-05-18 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'ColorBlendAdvancedEXT', 'ColorBlendEquationEXT',
-- 'PhysicalDeviceExtendedDynamicState3FeaturesEXT',
-- 'PhysicalDeviceExtendedDynamicState3PropertiesEXT',
-- 'cmdSetAlphaToCoverageEnableEXT', 'cmdSetAlphaToOneEnableEXT',
-- 'cmdSetColorBlendAdvancedEXT', 'cmdSetColorBlendEnableEXT',
-- 'cmdSetColorBlendEquationEXT', 'cmdSetColorWriteMaskEXT',
-- 'cmdSetConservativeRasterizationModeEXT', 'cmdSetDepthClampEnableEXT',
-- 'cmdSetDepthClipEnableEXT', 'cmdSetDepthClipNegativeOneToOneEXT',
-- 'cmdSetExtraPrimitiveOverestimationSizeEXT',
-- 'cmdSetLineRasterizationModeEXT', 'cmdSetLineStippleEnableEXT',
-- 'cmdSetLogicOpEnableEXT', 'cmdSetPolygonModeEXT',
-- 'cmdSetProvokingVertexModeEXT', 'cmdSetRasterizationSamplesEXT',
-- 'cmdSetRasterizationStreamEXT', 'cmdSetSampleLocationsEnableEXT',
-- 'cmdSetSampleMaskEXT', 'cmdSetTessellationDomainOriginEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_extended_dynamic_state3  ( cmdSetTessellationDomainOriginEXT
                                                         , cmdSetDepthClampEnableEXT
                                                         , cmdSetPolygonModeEXT
                                                         , cmdSetRasterizationSamplesEXT
                                                         , cmdSetSampleMaskEXT
                                                         , cmdSetAlphaToCoverageEnableEXT
                                                         , cmdSetAlphaToOneEnableEXT
                                                         , cmdSetLogicOpEnableEXT
                                                         , cmdSetColorBlendEnableEXT
                                                         , cmdSetColorBlendEquationEXT
                                                         , cmdSetColorWriteMaskEXT
                                                         , cmdSetRasterizationStreamEXT
                                                         , cmdSetConservativeRasterizationModeEXT
                                                         , cmdSetExtraPrimitiveOverestimationSizeEXT
                                                         , cmdSetDepthClipEnableEXT
                                                         , cmdSetSampleLocationsEnableEXT
                                                         , cmdSetColorBlendAdvancedEXT
                                                         , cmdSetProvokingVertexModeEXT
                                                         , cmdSetLineRasterizationModeEXT
                                                         , cmdSetLineStippleEnableEXT
                                                         , cmdSetDepthClipNegativeOneToOneEXT
                                                         , cmdSetViewportWScalingEnableNV
                                                         , cmdSetViewportSwizzleNV
                                                         , cmdSetCoverageToColorEnableNV
                                                         , cmdSetCoverageToColorLocationNV
                                                         , cmdSetCoverageModulationModeNV
                                                         , cmdSetCoverageModulationTableEnableNV
                                                         , cmdSetCoverageModulationTableNV
                                                         , cmdSetShadingRateImageEnableNV
                                                         , cmdSetCoverageReductionModeNV
                                                         , cmdSetRepresentativeFragmentTestEnableNV
                                                         , PhysicalDeviceExtendedDynamicState3FeaturesEXT(..)
                                                         , PhysicalDeviceExtendedDynamicState3PropertiesEXT(..)
                                                         , ColorBlendEquationEXT(..)
                                                         , ColorBlendAdvancedEXT(..)
                                                         , EXT_EXTENDED_DYNAMIC_STATE_3_SPEC_VERSION
                                                         , pattern EXT_EXTENDED_DYNAMIC_STATE_3_SPEC_VERSION
                                                         , EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME
                                                         , pattern EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME
                                                         , ViewportSwizzleNV(..)
                                                         , ViewportCoordinateSwizzleNV(..)
                                                         , BlendOverlapEXT(..)
                                                         , CoverageModulationModeNV(..)
                                                         , CoverageReductionModeNV(..)
                                                         , ConservativeRasterizationModeEXT(..)
                                                         , LineRasterizationModeEXT(..)
                                                         , ProvokingVertexModeEXT(..)
                                                         ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CFloat(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.Enums.BlendFactor (BlendFactor)
import Vulkan.Core10.Enums.BlendOp (BlendOp)
import Vulkan.Extensions.VK_EXT_blend_operation_advanced (BlendOverlapEXT)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Enums.ColorComponentFlagBits (ColorComponentFlagBits(..))
import Vulkan.Core10.Enums.ColorComponentFlagBits (ColorComponentFlags)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.VK_EXT_conservative_rasterization (ConservativeRasterizationModeEXT)
import Vulkan.Extensions.VK_EXT_conservative_rasterization (ConservativeRasterizationModeEXT(..))
import Vulkan.Extensions.VK_NV_framebuffer_mixed_samples (CoverageModulationModeNV)
import Vulkan.Extensions.VK_NV_framebuffer_mixed_samples (CoverageModulationModeNV(..))
import Vulkan.Extensions.VK_NV_coverage_reduction_mode (CoverageReductionModeNV)
import Vulkan.Extensions.VK_NV_coverage_reduction_mode (CoverageReductionModeNV(..))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetAlphaToCoverageEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetAlphaToOneEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetColorBlendAdvancedEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetColorBlendEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetColorBlendEquationEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetColorWriteMaskEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetConservativeRasterizationModeEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCoverageModulationModeNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCoverageModulationTableEnableNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCoverageModulationTableNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCoverageReductionModeNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCoverageToColorEnableNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetCoverageToColorLocationNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthClampEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthClipEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetDepthClipNegativeOneToOneEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetExtraPrimitiveOverestimationSizeEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetLineRasterizationModeEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetLineStippleEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetLogicOpEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetPolygonModeEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetProvokingVertexModeEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetRasterizationSamplesEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetRasterizationStreamEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetRepresentativeFragmentTestEnableNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetSampleLocationsEnableEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetSampleMaskEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetShadingRateImageEnableNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetTessellationDomainOriginEXT))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewportSwizzleNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetViewportWScalingEnableNV))
import Vulkan.Extensions.VK_EXT_line_rasterization (LineRasterizationModeEXT)
import Vulkan.Extensions.VK_EXT_line_rasterization (LineRasterizationModeEXT(..))
import Vulkan.Core10.Enums.PolygonMode (PolygonMode)
import Vulkan.Core10.Enums.PolygonMode (PolygonMode(..))
import Vulkan.Extensions.VK_EXT_provoking_vertex (ProvokingVertexModeEXT)
import Vulkan.Extensions.VK_EXT_provoking_vertex (ProvokingVertexModeEXT(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits(..))
import Vulkan.Core10.FundamentalTypes (SampleMask)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core11.Enums.TessellationDomainOrigin (TessellationDomainOrigin)
import Vulkan.Core11.Enums.TessellationDomainOrigin (TessellationDomainOrigin(..))
import Vulkan.Extensions.VK_NV_viewport_swizzle (ViewportSwizzleNV)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_PROPERTIES_EXT))
import Vulkan.Extensions.VK_EXT_blend_operation_advanced (BlendOverlapEXT(..))
import Vulkan.Extensions.VK_EXT_conservative_rasterization (ConservativeRasterizationModeEXT(..))
import Vulkan.Extensions.VK_NV_framebuffer_mixed_samples (CoverageModulationModeNV(..))
import Vulkan.Extensions.VK_NV_coverage_reduction_mode (CoverageReductionModeNV(..))
import Vulkan.Extensions.VK_EXT_line_rasterization (LineRasterizationModeEXT(..))
import Vulkan.Extensions.VK_EXT_provoking_vertex (ProvokingVertexModeEXT(..))
import Vulkan.Extensions.VK_NV_viewport_swizzle (ViewportCoordinateSwizzleNV(..))
import Vulkan.Extensions.VK_NV_viewport_swizzle (ViewportSwizzleNV(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetTessellationDomainOriginEXT
  :: FunPtr (Ptr CommandBuffer_T -> TessellationDomainOrigin -> IO ()) -> Ptr CommandBuffer_T -> TessellationDomainOrigin -> IO ()

-- | vkCmdSetTessellationDomainOriginEXT - Specify the origin of the
-- tessellation domain space dynamically for a command buffer
--
-- = Description
--
-- This command sets the origin of the tessellation domain space for
-- subsequent drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_TESSELLATION_DOMAIN_ORIGIN_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PipelineTessellationDomainOriginStateCreateInfo'::@domainOrigin@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetTessellationDomainOriginEXT-None-09423# At least one
--     of the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3TessellationDomainOrigin extendedDynamicState3TessellationDomainOrigin>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetTessellationDomainOriginEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetTessellationDomainOriginEXT-domainOrigin-parameter#
--     @domainOrigin@ /must/ be a valid
--     'Vulkan.Core11.Enums.TessellationDomainOrigin.TessellationDomainOrigin'
--     value
--
-- -   #VUID-vkCmdSetTessellationDomainOriginEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetTessellationDomainOriginEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetTessellationDomainOriginEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core11.Enums.TessellationDomainOrigin.TessellationDomainOrigin'
cmdSetTessellationDomainOriginEXT :: forall io
                                   . (MonadIO io)
                                  => -- | @commandBuffer@ is the command buffer into which the command will be
                                     -- recorded.
                                     CommandBuffer
                                  -> -- | @domainOrigin@ specifies the origin of the tessellation domain space.
                                     TessellationDomainOrigin
                                  -> io ()
cmdSetTessellationDomainOriginEXT commandBuffer domainOrigin = liftIO $ do
  let vkCmdSetTessellationDomainOriginEXTPtr = pVkCmdSetTessellationDomainOriginEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetTessellationDomainOriginEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetTessellationDomainOriginEXT is null" Nothing Nothing
  let vkCmdSetTessellationDomainOriginEXT' = mkVkCmdSetTessellationDomainOriginEXT vkCmdSetTessellationDomainOriginEXTPtr
  traceAroundEvent "vkCmdSetTessellationDomainOriginEXT" (vkCmdSetTessellationDomainOriginEXT'
                                                            (commandBufferHandle (commandBuffer))
                                                            (domainOrigin))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthClampEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthClampEnableEXT - Specify dynamically whether depth clamping
-- is enabled in the command buffer
--
-- = Description
--
-- This command sets whether depth clamping is enabled or disabled for
-- subsequent drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@depthClampEnable@
-- value used to create the currently active pipeline.
--
-- If the depth clamping state is changed dynamically, and the pipeline was
-- not created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
-- enabled, then depth clipping is enabled when depth clamping is disabled
-- and vice versa.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthClampEnableEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3DepthClampEnable extendedDynamicState3DepthClampEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- -   #VUID-vkCmdSetDepthClampEnableEXT-depthClamp-07449# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-depthClamp depthClamp>
--     feature is not enabled, @depthClampEnable@ must be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthClampEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthClampEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthClampEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDepthClampEnableEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthClampEnableEXT :: forall io
                           . (MonadIO io)
                          => -- | @commandBuffer@ is the command buffer into which the command will be
                             -- recorded.
                             CommandBuffer
                          -> -- | @depthClampEnable@ specifies whether depth clamping is enabled.
                             ("depthClampEnable" ::: Bool)
                          -> io ()
cmdSetDepthClampEnableEXT commandBuffer depthClampEnable = liftIO $ do
  let vkCmdSetDepthClampEnableEXTPtr = pVkCmdSetDepthClampEnableEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetDepthClampEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthClampEnableEXT is null" Nothing Nothing
  let vkCmdSetDepthClampEnableEXT' = mkVkCmdSetDepthClampEnableEXT vkCmdSetDepthClampEnableEXTPtr
  traceAroundEvent "vkCmdSetDepthClampEnableEXT" (vkCmdSetDepthClampEnableEXT'
                                                    (commandBufferHandle (commandBuffer))
                                                    (boolToBool32 (depthClampEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetPolygonModeEXT
  :: FunPtr (Ptr CommandBuffer_T -> PolygonMode -> IO ()) -> Ptr CommandBuffer_T -> PolygonMode -> IO ()

-- | vkCmdSetPolygonModeEXT - Specify polygon mode dynamically for a command
-- buffer
--
-- = Description
--
-- This command sets the polygon mode for subsequent drawing commands when
-- drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_POLYGON_MODE_EXT' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@polygonMode@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetPolygonModeEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3PolygonMode extendedDynamicState3PolygonMode>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- -   #VUID-vkCmdSetPolygonModeEXT-fillModeNonSolid-07424# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-fillModeNonSolid fillModeNonSolid>
--     feature is not enabled, @polygonMode@ /must/ be
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL' or
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- -   #VUID-vkCmdSetPolygonModeEXT-polygonMode-07425# If the
--     @VK_NV_fill_rectangle@ extension is not enabled, @polygonMode@
--     /must/ not be
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetPolygonModeEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetPolygonModeEXT-polygonMode-parameter# @polygonMode@
--     /must/ be a valid 'Vulkan.Core10.Enums.PolygonMode.PolygonMode'
--     value
--
-- -   #VUID-vkCmdSetPolygonModeEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetPolygonModeEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetPolygonModeEXT-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.PolygonMode.PolygonMode'
cmdSetPolygonModeEXT :: forall io
                      . (MonadIO io)
                     => -- | @commandBuffer@ is the command buffer into which the command will be
                        -- recorded.
                        CommandBuffer
                     -> -- | @polygonMode@ specifies polygon mode.
                        PolygonMode
                     -> io ()
cmdSetPolygonModeEXT commandBuffer polygonMode = liftIO $ do
  let vkCmdSetPolygonModeEXTPtr = pVkCmdSetPolygonModeEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetPolygonModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetPolygonModeEXT is null" Nothing Nothing
  let vkCmdSetPolygonModeEXT' = mkVkCmdSetPolygonModeEXT vkCmdSetPolygonModeEXTPtr
  traceAroundEvent "vkCmdSetPolygonModeEXT" (vkCmdSetPolygonModeEXT'
                                               (commandBufferHandle (commandBuffer))
                                               (polygonMode))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetRasterizationSamplesEXT
  :: FunPtr (Ptr CommandBuffer_T -> SampleCountFlagBits -> IO ()) -> Ptr CommandBuffer_T -> SampleCountFlagBits -> IO ()

-- | vkCmdSetRasterizationSamplesEXT - Specify the rasterization samples
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the @rasterizationSamples@ for subsequent drawing
-- commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@rasterizationSamples@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetRasterizationSamplesEXT-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3RasterizationSamples extendedDynamicState3RasterizationSamples>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetRasterizationSamplesEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetRasterizationSamplesEXT-rasterizationSamples-parameter#
--     @rasterizationSamples@ /must/ be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-vkCmdSetRasterizationSamplesEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetRasterizationSamplesEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetRasterizationSamplesEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
cmdSetRasterizationSamplesEXT :: forall io
                               . (MonadIO io)
                              => -- | @commandBuffer@ is the command buffer into which the command will be
                                 -- recorded.
                                 CommandBuffer
                              -> -- | @rasterizationSamples@ specifies @rasterizationSamples@.
                                 ("rasterizationSamples" ::: SampleCountFlagBits)
                              -> io ()
cmdSetRasterizationSamplesEXT commandBuffer rasterizationSamples = liftIO $ do
  let vkCmdSetRasterizationSamplesEXTPtr = pVkCmdSetRasterizationSamplesEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetRasterizationSamplesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetRasterizationSamplesEXT is null" Nothing Nothing
  let vkCmdSetRasterizationSamplesEXT' = mkVkCmdSetRasterizationSamplesEXT vkCmdSetRasterizationSamplesEXTPtr
  traceAroundEvent "vkCmdSetRasterizationSamplesEXT" (vkCmdSetRasterizationSamplesEXT'
                                                        (commandBufferHandle (commandBuffer))
                                                        (rasterizationSamples))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetSampleMaskEXT
  :: FunPtr (Ptr CommandBuffer_T -> SampleCountFlagBits -> Ptr SampleMask -> IO ()) -> Ptr CommandBuffer_T -> SampleCountFlagBits -> Ptr SampleMask -> IO ()

-- | vkCmdSetSampleMaskEXT - Specify the sample mask dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the sample mask for subsequent drawing commands when
-- drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT' set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@pSampleMask@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetSampleMaskEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3SampleMask extendedDynamicState3SampleMask>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetSampleMaskEXT-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetSampleMaskEXT-samples-parameter# @samples@ /must/ be a
--     valid 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
--     value
--
-- -   #VUID-vkCmdSetSampleMaskEXT-pSampleMask-parameter# @pSampleMask@
--     /must/ be a valid pointer to an array of
--     \(\lceil{\mathit{samples} \over 32}\rceil\)
--     'Vulkan.Core10.FundamentalTypes.SampleMask' values
--
-- -   #VUID-vkCmdSetSampleMaskEXT-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetSampleMaskEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetSampleMaskEXT-videocoding# This command /must/ only be
--     called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.FundamentalTypes.SampleMask'
cmdSetSampleMaskEXT :: forall io
                     . (MonadIO io)
                    => -- | @commandBuffer@ is the command buffer into which the command will be
                       -- recorded.
                       CommandBuffer
                    -> -- | @samples@ specifies the number of sample bits in the @pSampleMask@.
                       ("samples" ::: SampleCountFlagBits)
                    -> -- | @pSampleMask@ is a pointer to an array of
                       -- 'Vulkan.Core10.FundamentalTypes.SampleMask' values, where the array size
                       -- is based on the @samples@ parameter.
                       ("sampleMask" ::: Vector SampleMask)
                    -> io ()
cmdSetSampleMaskEXT commandBuffer samples sampleMask = liftIO . evalContT $ do
  let vkCmdSetSampleMaskEXTPtr = pVkCmdSetSampleMaskEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetSampleMaskEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetSampleMaskEXT is null" Nothing Nothing
  let vkCmdSetSampleMaskEXT' = mkVkCmdSetSampleMaskEXT vkCmdSetSampleMaskEXTPtr
  pPSampleMask <- ContT $ allocaBytes @SampleMask ((Data.Vector.length (sampleMask)) * 4)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPSampleMask `plusPtr` (4 * (i)) :: Ptr SampleMask) (e)) (sampleMask)
  lift $ traceAroundEvent "vkCmdSetSampleMaskEXT" (vkCmdSetSampleMaskEXT'
                                                     (commandBufferHandle (commandBuffer))
                                                     (samples)
                                                     (pPSampleMask))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetAlphaToCoverageEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetAlphaToCoverageEnableEXT - Specify the alpha to coverage enable
-- state dynamically for a command buffer
--
-- = Description
--
-- This command sets the @alphaToCoverageEnable@ state for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@alphaToCoverageEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetAlphaToCoverageEnableEXT-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3AlphaToCoverageEnable extendedDynamicState3AlphaToCoverageEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetAlphaToCoverageEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetAlphaToCoverageEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetAlphaToCoverageEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetAlphaToCoverageEnableEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetAlphaToCoverageEnableEXT :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @alphaToCoverageEnable@ specifies the @alphaToCoverageEnable@ state.
                                  ("alphaToCoverageEnable" ::: Bool)
                               -> io ()
cmdSetAlphaToCoverageEnableEXT commandBuffer alphaToCoverageEnable = liftIO $ do
  let vkCmdSetAlphaToCoverageEnableEXTPtr = pVkCmdSetAlphaToCoverageEnableEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetAlphaToCoverageEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetAlphaToCoverageEnableEXT is null" Nothing Nothing
  let vkCmdSetAlphaToCoverageEnableEXT' = mkVkCmdSetAlphaToCoverageEnableEXT vkCmdSetAlphaToCoverageEnableEXTPtr
  traceAroundEvent "vkCmdSetAlphaToCoverageEnableEXT" (vkCmdSetAlphaToCoverageEnableEXT'
                                                         (commandBufferHandle (commandBuffer))
                                                         (boolToBool32 (alphaToCoverageEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetAlphaToOneEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetAlphaToOneEnableEXT - Specify the alpha to one enable state
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the @alphaToOneEnable@ state for subsequent drawing
-- commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'::@alphaToOneEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetAlphaToOneEnableEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3AlphaToOneEnable extendedDynamicState3AlphaToOneEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- -   #VUID-vkCmdSetAlphaToOneEnableEXT-alphaToOne-07607# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-alphaToOne alphaToOne>
--     feature is not enabled, @alphaToOneEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetAlphaToOneEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetAlphaToOneEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetAlphaToOneEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetAlphaToOneEnableEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetAlphaToOneEnableEXT :: forall io
                           . (MonadIO io)
                          => -- | @commandBuffer@ is the command buffer into which the command will be
                             -- recorded.
                             CommandBuffer
                          -> -- | @alphaToOneEnable@ specifies the @alphaToOneEnable@ state.
                             ("alphaToOneEnable" ::: Bool)
                          -> io ()
cmdSetAlphaToOneEnableEXT commandBuffer alphaToOneEnable = liftIO $ do
  let vkCmdSetAlphaToOneEnableEXTPtr = pVkCmdSetAlphaToOneEnableEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetAlphaToOneEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetAlphaToOneEnableEXT is null" Nothing Nothing
  let vkCmdSetAlphaToOneEnableEXT' = mkVkCmdSetAlphaToOneEnableEXT vkCmdSetAlphaToOneEnableEXTPtr
  traceAroundEvent "vkCmdSetAlphaToOneEnableEXT" (vkCmdSetAlphaToOneEnableEXT'
                                                    (commandBufferHandle (commandBuffer))
                                                    (boolToBool32 (alphaToOneEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLogicOpEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetLogicOpEnableEXT - Specify dynamically whether logical
-- operations are enabled for a command buffer
--
-- = Description
--
-- This command sets whether logical operations are enabled for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_ENABLE_EXT' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'::@logicOpEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetLogicOpEnableEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3LogicOpEnable extendedDynamicState3LogicOpEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- -   #VUID-vkCmdSetLogicOpEnableEXT-logicOp-07366# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-logicOp logicOp>
--     feature is not enabled, @logicOpEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetLogicOpEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetLogicOpEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetLogicOpEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetLogicOpEnableEXT-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetLogicOpEnableEXT :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer into which the command will be
                          -- recorded.
                          CommandBuffer
                       -> -- | @logicOpEnable@ specifies whether logical operations are enabled.
                          ("logicOpEnable" ::: Bool)
                       -> io ()
cmdSetLogicOpEnableEXT commandBuffer logicOpEnable = liftIO $ do
  let vkCmdSetLogicOpEnableEXTPtr = pVkCmdSetLogicOpEnableEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetLogicOpEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetLogicOpEnableEXT is null" Nothing Nothing
  let vkCmdSetLogicOpEnableEXT' = mkVkCmdSetLogicOpEnableEXT vkCmdSetLogicOpEnableEXTPtr
  traceAroundEvent "vkCmdSetLogicOpEnableEXT" (vkCmdSetLogicOpEnableEXT'
                                                 (commandBufferHandle (commandBuffer))
                                                 (boolToBool32 (logicOpEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetColorBlendEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr Bool32 -> IO ()

-- | vkCmdSetColorBlendEnableEXT - Specify the @blendEnable@ for each
-- attachment dynamically for a command buffer
--
-- = Description
--
-- This command sets the color blending enable of the specified color
-- attachments for subsequent drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'::@blendEnable@
-- values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetColorBlendEnableEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3ColorBlendEnable extendedDynamicState3ColorBlendEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetColorBlendEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetColorBlendEnableEXT-pColorBlendEnables-parameter#
--     @pColorBlendEnables@ /must/ be a valid pointer to an array of
--     @attachmentCount@ 'Vulkan.Core10.FundamentalTypes.Bool32' values
--
-- -   #VUID-vkCmdSetColorBlendEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetColorBlendEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetColorBlendEnableEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetColorBlendEnableEXT-attachmentCount-arraylength#
--     @attachmentCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetColorBlendEnableEXT :: forall io
                           . (MonadIO io)
                          => -- | @commandBuffer@ is the command buffer into which the command will be
                             -- recorded.
                             CommandBuffer
                          -> -- | @firstAttachment@ the first color attachment the color blending enable
                             -- applies.
                             ("firstAttachment" ::: Word32)
                          -> -- | @pColorBlendEnables@ an array of booleans to indicate whether color
                             -- blending is enabled for the corresponding attachment.
                             ("colorBlendEnables" ::: Vector Bool)
                          -> io ()
cmdSetColorBlendEnableEXT commandBuffer
                            firstAttachment
                            colorBlendEnables = liftIO . evalContT $ do
  let vkCmdSetColorBlendEnableEXTPtr = pVkCmdSetColorBlendEnableEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetColorBlendEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetColorBlendEnableEXT is null" Nothing Nothing
  let vkCmdSetColorBlendEnableEXT' = mkVkCmdSetColorBlendEnableEXT vkCmdSetColorBlendEnableEXTPtr
  pPColorBlendEnables <- ContT $ allocaBytes @Bool32 ((Data.Vector.length (colorBlendEnables)) * 4)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPColorBlendEnables `plusPtr` (4 * (i)) :: Ptr Bool32) (boolToBool32 (e))) (colorBlendEnables)
  lift $ traceAroundEvent "vkCmdSetColorBlendEnableEXT" (vkCmdSetColorBlendEnableEXT'
                                                           (commandBufferHandle (commandBuffer))
                                                           (firstAttachment)
                                                           ((fromIntegral (Data.Vector.length $ (colorBlendEnables)) :: Word32))
                                                           (pPColorBlendEnables))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetColorBlendEquationEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ColorBlendEquationEXT -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ColorBlendEquationEXT -> IO ()

-- | vkCmdSetColorBlendEquationEXT - Specify the blend factors and operations
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the color blending factors and operations of the
-- specified attachments for subsequent drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'::@srcColorBlendFactor@,
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'::@dstColorBlendFactor@,
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'::@colorBlendOp@,
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'::@srcAlphaBlendFactor@,
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'::@dstAlphaBlendFactor@,
-- and
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'::@alphaBlendOp@
-- values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetColorBlendEquationEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3ColorBlendEquation extendedDynamicState3ColorBlendEquation>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetColorBlendEquationEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetColorBlendEquationEXT-pColorBlendEquations-parameter#
--     @pColorBlendEquations@ /must/ be a valid pointer to an array of
--     @attachmentCount@ valid 'ColorBlendEquationEXT' structures
--
-- -   #VUID-vkCmdSetColorBlendEquationEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetColorBlendEquationEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetColorBlendEquationEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetColorBlendEquationEXT-attachmentCount-arraylength#
--     @attachmentCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'ColorBlendEquationEXT', 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetColorBlendEquationEXT :: forall io
                             . (MonadIO io)
                            => -- | @commandBuffer@ is the command buffer into which the command will be
                               -- recorded.
                               CommandBuffer
                            -> -- | @firstAttachment@ the first color attachment the color blend factors and
                               -- operations apply to.
                               ("firstAttachment" ::: Word32)
                            -> -- | @pColorBlendEquations@ an array of 'ColorBlendEquationEXT' structs that
                               -- specify the color blend factors and operations for the corresponding
                               -- attachments.
                               ("colorBlendEquations" ::: Vector ColorBlendEquationEXT)
                            -> io ()
cmdSetColorBlendEquationEXT commandBuffer
                              firstAttachment
                              colorBlendEquations = liftIO . evalContT $ do
  let vkCmdSetColorBlendEquationEXTPtr = pVkCmdSetColorBlendEquationEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetColorBlendEquationEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetColorBlendEquationEXT is null" Nothing Nothing
  let vkCmdSetColorBlendEquationEXT' = mkVkCmdSetColorBlendEquationEXT vkCmdSetColorBlendEquationEXTPtr
  pPColorBlendEquations <- ContT $ allocaBytes @ColorBlendEquationEXT ((Data.Vector.length (colorBlendEquations)) * 24)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPColorBlendEquations `plusPtr` (24 * (i)) :: Ptr ColorBlendEquationEXT) (e)) (colorBlendEquations)
  lift $ traceAroundEvent "vkCmdSetColorBlendEquationEXT" (vkCmdSetColorBlendEquationEXT'
                                                             (commandBufferHandle (commandBuffer))
                                                             (firstAttachment)
                                                             ((fromIntegral (Data.Vector.length $ (colorBlendEquations)) :: Word32))
                                                             (pPColorBlendEquations))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetColorWriteMaskEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ColorComponentFlags -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ColorComponentFlags -> IO ()

-- | vkCmdSetColorWriteMaskEXT - Specify the color write masks for each
-- attachment dynamically for a command buffer
--
-- = Description
--
-- This command sets the color write masks of the specified attachments for
-- subsequent drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendAttachmentState'::@colorWriteMask@
-- values used to create the currently active pipeline.
--
-- Note
--
-- Formats with bits that are shared between components specified by
-- 'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits',
-- such as 'Vulkan.Core10.Enums.Format.FORMAT_E5B9G9R9_UFLOAT_PACK32',
-- cannot have their channels individually masked by this functionality;
-- either all components that share bits have to be enabled, or none of
-- them.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetColorWriteMaskEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3ColorWriteMask extendedDynamicState3ColorWriteMask>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetColorWriteMaskEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetColorWriteMaskEXT-pColorWriteMasks-parameter#
--     @pColorWriteMasks@ /must/ be a valid pointer to an array of
--     @attachmentCount@ valid combinations of
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits'
--     values
--
-- -   #VUID-vkCmdSetColorWriteMaskEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetColorWriteMaskEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetColorWriteMaskEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetColorWriteMaskEXT-attachmentCount-arraylength#
--     @attachmentCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlags',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetColorWriteMaskEXT :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @firstAttachment@ the first color attachment the color write masks apply
                           -- to.
                           ("firstAttachment" ::: Word32)
                        -> -- | @pColorWriteMasks@ an array of
                           -- 'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlags' values
                           -- that specify the color write masks of the corresponding attachments.
                           ("colorWriteMasks" ::: Vector ColorComponentFlags)
                        -> io ()
cmdSetColorWriteMaskEXT commandBuffer
                          firstAttachment
                          colorWriteMasks = liftIO . evalContT $ do
  let vkCmdSetColorWriteMaskEXTPtr = pVkCmdSetColorWriteMaskEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetColorWriteMaskEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetColorWriteMaskEXT is null" Nothing Nothing
  let vkCmdSetColorWriteMaskEXT' = mkVkCmdSetColorWriteMaskEXT vkCmdSetColorWriteMaskEXTPtr
  pPColorWriteMasks <- ContT $ allocaBytes @ColorComponentFlags ((Data.Vector.length (colorWriteMasks)) * 4)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPColorWriteMasks `plusPtr` (4 * (i)) :: Ptr ColorComponentFlags) (e)) (colorWriteMasks)
  lift $ traceAroundEvent "vkCmdSetColorWriteMaskEXT" (vkCmdSetColorWriteMaskEXT'
                                                         (commandBufferHandle (commandBuffer))
                                                         (firstAttachment)
                                                         ((fromIntegral (Data.Vector.length $ (colorWriteMasks)) :: Word32))
                                                         (pPColorWriteMasks))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetRasterizationStreamEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> IO ()

-- | vkCmdSetRasterizationStreamEXT - Specify the rasterization stream
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the @rasterizationStream@ state for subsequent drawing
-- commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_STREAM_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetRasterizationStreamEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3RasterizationStream extendedDynamicState3RasterizationStream>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- -   #VUID-vkCmdSetRasterizationStreamEXT-transformFeedback-07411# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-transformFeedback transformFeedback>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetRasterizationStreamEXT-rasterizationStream-07412#
--     @rasterizationStream@ /must/ be less than
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT'::@maxTransformFeedbackStreams@
--
-- -   #VUID-vkCmdSetRasterizationStreamEXT-rasterizationStream-07413#
--     @rasterizationStream@ /must/ be zero if
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT'::@transformFeedbackRasterizationStreamSelect@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetRasterizationStreamEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetRasterizationStreamEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetRasterizationStreamEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetRasterizationStreamEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetRasterizationStreamEXT :: forall io
                              . (MonadIO io)
                             => -- | @commandBuffer@ is the command buffer into which the command will be
                                -- recorded.
                                CommandBuffer
                             -> -- | @rasterizationStream@ specifies the @rasterizationStream@ state.
                                ("rasterizationStream" ::: Word32)
                             -> io ()
cmdSetRasterizationStreamEXT commandBuffer rasterizationStream = liftIO $ do
  let vkCmdSetRasterizationStreamEXTPtr = pVkCmdSetRasterizationStreamEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetRasterizationStreamEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetRasterizationStreamEXT is null" Nothing Nothing
  let vkCmdSetRasterizationStreamEXT' = mkVkCmdSetRasterizationStreamEXT vkCmdSetRasterizationStreamEXTPtr
  traceAroundEvent "vkCmdSetRasterizationStreamEXT" (vkCmdSetRasterizationStreamEXT'
                                                       (commandBufferHandle (commandBuffer))
                                                       (rasterizationStream))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetConservativeRasterizationModeEXT
  :: FunPtr (Ptr CommandBuffer_T -> ConservativeRasterizationModeEXT -> IO ()) -> Ptr CommandBuffer_T -> ConservativeRasterizationModeEXT -> IO ()

-- | vkCmdSetConservativeRasterizationModeEXT - Specify the conservative
-- rasterization mode dynamically for a command buffer
--
-- = Description
--
-- This command sets the @conservativeRasterizationMode@ state for
-- subsequent drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CONSERVATIVE_RASTERIZATION_MODE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_EXT_conservative_rasterization.PipelineRasterizationConservativeStateCreateInfoEXT'::@conservativeRasterizationMode@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetConservativeRasterizationModeEXT-None-09423# At least
--     one of the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3ConservativeRasterizationMode extendedDynamicState3ConservativeRasterizationMode>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetConservativeRasterizationModeEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetConservativeRasterizationModeEXT-conservativeRasterizationMode-parameter#
--     @conservativeRasterizationMode@ /must/ be a valid
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.ConservativeRasterizationModeEXT'
--     value
--
-- -   #VUID-vkCmdSetConservativeRasterizationModeEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetConservativeRasterizationModeEXT-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetConservativeRasterizationModeEXT-videocoding# This
--     command /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.VK_EXT_conservative_rasterization.ConservativeRasterizationModeEXT'
cmdSetConservativeRasterizationModeEXT :: forall io
                                        . (MonadIO io)
                                       => -- | @commandBuffer@ is the command buffer into which the command will be
                                          -- recorded.
                                          CommandBuffer
                                       -> -- | @conservativeRasterizationMode@ specifies the
                                          -- @conservativeRasterizationMode@ state.
                                          ConservativeRasterizationModeEXT
                                       -> io ()
cmdSetConservativeRasterizationModeEXT commandBuffer
                                         conservativeRasterizationMode = liftIO $ do
  let vkCmdSetConservativeRasterizationModeEXTPtr = pVkCmdSetConservativeRasterizationModeEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetConservativeRasterizationModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetConservativeRasterizationModeEXT is null" Nothing Nothing
  let vkCmdSetConservativeRasterizationModeEXT' = mkVkCmdSetConservativeRasterizationModeEXT vkCmdSetConservativeRasterizationModeEXTPtr
  traceAroundEvent "vkCmdSetConservativeRasterizationModeEXT" (vkCmdSetConservativeRasterizationModeEXT'
                                                                 (commandBufferHandle (commandBuffer))
                                                                 (conservativeRasterizationMode))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetExtraPrimitiveOverestimationSizeEXT
  :: FunPtr (Ptr CommandBuffer_T -> CFloat -> IO ()) -> Ptr CommandBuffer_T -> CFloat -> IO ()

-- | vkCmdSetExtraPrimitiveOverestimationSizeEXT - Specify the conservative
-- rasterization extra primitive overestimation size dynamically for a
-- command buffer
--
-- = Description
--
-- This command sets the @extraPrimitiveOverestimationSize@ for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXTRA_PRIMITIVE_OVERESTIMATION_SIZE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_EXT_conservative_rasterization.PipelineRasterizationConservativeStateCreateInfoEXT'::@extraPrimitiveOverestimationSize@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetExtraPrimitiveOverestimationSizeEXT-None-09423# At
--     least one of the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3ExtraPrimitiveOverestimationSize extendedDynamicState3ExtraPrimitiveOverestimationSize>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- -   #VUID-vkCmdSetExtraPrimitiveOverestimationSizeEXT-extraPrimitiveOverestimationSize-07428#
--     @extraPrimitiveOverestimationSize@ /must/ be in the range of @0.0@
--     to
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.PhysicalDeviceConservativeRasterizationPropertiesEXT'::@maxExtraPrimitiveOverestimationSize@
--     inclusive
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetExtraPrimitiveOverestimationSizeEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetExtraPrimitiveOverestimationSizeEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetExtraPrimitiveOverestimationSizeEXT-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetExtraPrimitiveOverestimationSizeEXT-videocoding# This
--     command /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetExtraPrimitiveOverestimationSizeEXT :: forall io
                                           . (MonadIO io)
                                          => -- | @commandBuffer@ is the command buffer into which the command will be
                                             -- recorded.
                                             CommandBuffer
                                          -> -- | @extraPrimitiveOverestimationSize@ specifies the
                                             -- @extraPrimitiveOverestimationSize@.
                                             ("extraPrimitiveOverestimationSize" ::: Float)
                                          -> io ()
cmdSetExtraPrimitiveOverestimationSizeEXT commandBuffer
                                            extraPrimitiveOverestimationSize = liftIO $ do
  let vkCmdSetExtraPrimitiveOverestimationSizeEXTPtr = pVkCmdSetExtraPrimitiveOverestimationSizeEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetExtraPrimitiveOverestimationSizeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetExtraPrimitiveOverestimationSizeEXT is null" Nothing Nothing
  let vkCmdSetExtraPrimitiveOverestimationSizeEXT' = mkVkCmdSetExtraPrimitiveOverestimationSizeEXT vkCmdSetExtraPrimitiveOverestimationSizeEXTPtr
  traceAroundEvent "vkCmdSetExtraPrimitiveOverestimationSizeEXT" (vkCmdSetExtraPrimitiveOverestimationSizeEXT'
                                                                    (commandBufferHandle (commandBuffer))
                                                                    (CFloat (extraPrimitiveOverestimationSize)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthClipEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthClipEnableEXT - Specify dynamically whether depth clipping
-- is enabled in the command buffer
--
-- = Description
--
-- This command sets whether depth clipping is enabled or disabled for
-- subsequent drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT'::@depthClipEnable@
-- value used to create the currently active pipeline, or is set to the
-- inverse of
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'::@depthClampEnable@
-- if
-- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT'
-- is not specified.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthClipEnableEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3DepthClipEnable extendedDynamicState3DepthClipEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- -   #VUID-vkCmdSetDepthClipEnableEXT-depthClipEnable-07451# The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-depthClipEnable depthClipEnable>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthClipEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthClipEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthClipEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDepthClipEnableEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthClipEnableEXT :: forall io
                          . (MonadIO io)
                         => -- | @commandBuffer@ is the command buffer into which the command will be
                            -- recorded.
                            CommandBuffer
                         -> -- | @depthClipEnable@ specifies whether depth clipping is enabled.
                            ("depthClipEnable" ::: Bool)
                         -> io ()
cmdSetDepthClipEnableEXT commandBuffer depthClipEnable = liftIO $ do
  let vkCmdSetDepthClipEnableEXTPtr = pVkCmdSetDepthClipEnableEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetDepthClipEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthClipEnableEXT is null" Nothing Nothing
  let vkCmdSetDepthClipEnableEXT' = mkVkCmdSetDepthClipEnableEXT vkCmdSetDepthClipEnableEXTPtr
  traceAroundEvent "vkCmdSetDepthClipEnableEXT" (vkCmdSetDepthClipEnableEXT'
                                                   (commandBufferHandle (commandBuffer))
                                                   (boolToBool32 (depthClipEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetSampleLocationsEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetSampleLocationsEnableEXT - Specify the samples locations enable
-- state dynamically for a command buffer
--
-- = Description
--
-- This command sets the @sampleLocationsEnable@ state for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetSampleLocationsEnableEXT-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3SampleLocationsEnable extendedDynamicState3SampleLocationsEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetSampleLocationsEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetSampleLocationsEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetSampleLocationsEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetSampleLocationsEnableEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetSampleLocationsEnableEXT :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @sampleLocationsEnable@ specifies the @sampleLocationsEnable@ state.
                                  ("sampleLocationsEnable" ::: Bool)
                               -> io ()
cmdSetSampleLocationsEnableEXT commandBuffer sampleLocationsEnable = liftIO $ do
  let vkCmdSetSampleLocationsEnableEXTPtr = pVkCmdSetSampleLocationsEnableEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetSampleLocationsEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetSampleLocationsEnableEXT is null" Nothing Nothing
  let vkCmdSetSampleLocationsEnableEXT' = mkVkCmdSetSampleLocationsEnableEXT vkCmdSetSampleLocationsEnableEXTPtr
  traceAroundEvent "vkCmdSetSampleLocationsEnableEXT" (vkCmdSetSampleLocationsEnableEXT'
                                                         (commandBufferHandle (commandBuffer))
                                                         (boolToBool32 (sampleLocationsEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetColorBlendAdvancedEXT
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ColorBlendAdvancedEXT -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ColorBlendAdvancedEXT -> IO ()

-- | vkCmdSetColorBlendAdvancedEXT - Specify the advanced color blend state
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the advanced blend operation parameters of the
-- specified attachments for subsequent drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PipelineColorBlendAdvancedStateCreateInfoEXT'::@srcPremultiplied@,
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PipelineColorBlendAdvancedStateCreateInfoEXT'::@dstPremultiplied@,
-- and
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PipelineColorBlendAdvancedStateCreateInfoEXT'::@blendOverlap@
-- values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetColorBlendAdvancedEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3ColorBlendAdvanced extendedDynamicState3ColorBlendAdvanced>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetColorBlendAdvancedEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetColorBlendAdvancedEXT-pColorBlendAdvanced-parameter#
--     @pColorBlendAdvanced@ /must/ be a valid pointer to an array of
--     @attachmentCount@ valid 'ColorBlendAdvancedEXT' structures
--
-- -   #VUID-vkCmdSetColorBlendAdvancedEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetColorBlendAdvancedEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetColorBlendAdvancedEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetColorBlendAdvancedEXT-attachmentCount-arraylength#
--     @attachmentCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'ColorBlendAdvancedEXT', 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetColorBlendAdvancedEXT :: forall io
                             . (MonadIO io)
                            => -- | @commandBuffer@ is the command buffer into which the command will be
                               -- recorded.
                               CommandBuffer
                            -> -- | @firstAttachment@ the first color attachment the advanced blend
                               -- parameters apply to.
                               ("firstAttachment" ::: Word32)
                            -> -- | @pColorBlendAdvanced@ an array of 'ColorBlendAdvancedEXT' structs that
                               -- specify the advanced color blend parameters for the corresponding
                               -- attachments.
                               ("colorBlendAdvanced" ::: Vector ColorBlendAdvancedEXT)
                            -> io ()
cmdSetColorBlendAdvancedEXT commandBuffer
                              firstAttachment
                              colorBlendAdvanced = liftIO . evalContT $ do
  let vkCmdSetColorBlendAdvancedEXTPtr = pVkCmdSetColorBlendAdvancedEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetColorBlendAdvancedEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetColorBlendAdvancedEXT is null" Nothing Nothing
  let vkCmdSetColorBlendAdvancedEXT' = mkVkCmdSetColorBlendAdvancedEXT vkCmdSetColorBlendAdvancedEXTPtr
  pPColorBlendAdvanced <- ContT $ allocaBytes @ColorBlendAdvancedEXT ((Data.Vector.length (colorBlendAdvanced)) * 20)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPColorBlendAdvanced `plusPtr` (20 * (i)) :: Ptr ColorBlendAdvancedEXT) (e)) (colorBlendAdvanced)
  lift $ traceAroundEvent "vkCmdSetColorBlendAdvancedEXT" (vkCmdSetColorBlendAdvancedEXT'
                                                             (commandBufferHandle (commandBuffer))
                                                             (firstAttachment)
                                                             ((fromIntegral (Data.Vector.length $ (colorBlendAdvanced)) :: Word32))
                                                             (pPColorBlendAdvanced))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetProvokingVertexModeEXT
  :: FunPtr (Ptr CommandBuffer_T -> ProvokingVertexModeEXT -> IO ()) -> Ptr CommandBuffer_T -> ProvokingVertexModeEXT -> IO ()

-- | vkCmdSetProvokingVertexModeEXT - Specify the provoking vertex mode
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the @provokingVertexMode@ state for subsequent drawing
-- commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PROVOKING_VERTEX_MODE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_EXT_provoking_vertex.PipelineRasterizationProvokingVertexStateCreateInfoEXT'::@provokingVertexMode@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetProvokingVertexModeEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3ProvokingVertexMode extendedDynamicState3ProvokingVertexMode>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- -   #VUID-vkCmdSetProvokingVertexModeEXT-provokingVertexMode-07447# If
--     @provokingVertexMode@ is
--     'Vulkan.Extensions.VK_EXT_provoking_vertex.PROVOKING_VERTEX_MODE_LAST_VERTEX_EXT',
--     then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-provokingVertexLast provokingVertexLast>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetProvokingVertexModeEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetProvokingVertexModeEXT-provokingVertexMode-parameter#
--     @provokingVertexMode@ /must/ be a valid
--     'Vulkan.Extensions.VK_EXT_provoking_vertex.ProvokingVertexModeEXT'
--     value
--
-- -   #VUID-vkCmdSetProvokingVertexModeEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetProvokingVertexModeEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetProvokingVertexModeEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.VK_EXT_provoking_vertex.ProvokingVertexModeEXT'
cmdSetProvokingVertexModeEXT :: forall io
                              . (MonadIO io)
                             => -- | @commandBuffer@ is the command buffer into which the command will be
                                -- recorded.
                                CommandBuffer
                             -> -- | @provokingVertexMode@ specifies the @provokingVertexMode@ state.
                                ProvokingVertexModeEXT
                             -> io ()
cmdSetProvokingVertexModeEXT commandBuffer provokingVertexMode = liftIO $ do
  let vkCmdSetProvokingVertexModeEXTPtr = pVkCmdSetProvokingVertexModeEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetProvokingVertexModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetProvokingVertexModeEXT is null" Nothing Nothing
  let vkCmdSetProvokingVertexModeEXT' = mkVkCmdSetProvokingVertexModeEXT vkCmdSetProvokingVertexModeEXTPtr
  traceAroundEvent "vkCmdSetProvokingVertexModeEXT" (vkCmdSetProvokingVertexModeEXT'
                                                       (commandBufferHandle (commandBuffer))
                                                       (provokingVertexMode))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLineRasterizationModeEXT
  :: FunPtr (Ptr CommandBuffer_T -> LineRasterizationModeEXT -> IO ()) -> Ptr CommandBuffer_T -> LineRasterizationModeEXT -> IO ()

-- | vkCmdSetLineRasterizationModeEXT - Specify the line rasterization mode
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the @lineRasterizationMode@ state for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'::@lineRasterizationMode@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetLineRasterizationModeEXT-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3LineRasterizationMode extendedDynamicState3LineRasterizationMode>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- -   #VUID-vkCmdSetLineRasterizationModeEXT-lineRasterizationMode-07418#
--     If @lineRasterizationMode@ is
--     'Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_RECTANGULAR_EXT',
--     then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-rectangularLines rectangularLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetLineRasterizationModeEXT-lineRasterizationMode-07419#
--     If @lineRasterizationMode@ is
--     'Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_BRESENHAM_EXT',
--     then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-bresenhamLines bresenhamLines>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdSetLineRasterizationModeEXT-lineRasterizationMode-07420#
--     If @lineRasterizationMode@ is
--     'Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT',
--     then the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-smoothLines smoothLines>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetLineRasterizationModeEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetLineRasterizationModeEXT-lineRasterizationMode-parameter#
--     @lineRasterizationMode@ /must/ be a valid
--     'Vulkan.Extensions.VK_EXT_line_rasterization.LineRasterizationModeEXT'
--     value
--
-- -   #VUID-vkCmdSetLineRasterizationModeEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetLineRasterizationModeEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetLineRasterizationModeEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.LineRasterizationModeEXT'
cmdSetLineRasterizationModeEXT :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @lineRasterizationMode@ specifies the @lineRasterizationMode@ state.
                                  LineRasterizationModeEXT
                               -> io ()
cmdSetLineRasterizationModeEXT commandBuffer lineRasterizationMode = liftIO $ do
  let vkCmdSetLineRasterizationModeEXTPtr = pVkCmdSetLineRasterizationModeEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetLineRasterizationModeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetLineRasterizationModeEXT is null" Nothing Nothing
  let vkCmdSetLineRasterizationModeEXT' = mkVkCmdSetLineRasterizationModeEXT vkCmdSetLineRasterizationModeEXTPtr
  traceAroundEvent "vkCmdSetLineRasterizationModeEXT" (vkCmdSetLineRasterizationModeEXT'
                                                         (commandBufferHandle (commandBuffer))
                                                         (lineRasterizationMode))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLineStippleEnableEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetLineStippleEnableEXT - Specify the line stipple enable
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the @stippledLineEnable@ state for subsequent drawing
-- commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'::@stippledLineEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetLineStippleEnableEXT-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3LineStippleEnable extendedDynamicState3LineStippleEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetLineStippleEnableEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetLineStippleEnableEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetLineStippleEnableEXT-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetLineStippleEnableEXT-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetLineStippleEnableEXT :: forall io
                            . (MonadIO io)
                           => -- | @commandBuffer@ is the command buffer into which the command will be
                              -- recorded.
                              CommandBuffer
                           -> -- | @stippledLineEnable@ specifies the @stippledLineEnable@ state.
                              ("stippledLineEnable" ::: Bool)
                           -> io ()
cmdSetLineStippleEnableEXT commandBuffer stippledLineEnable = liftIO $ do
  let vkCmdSetLineStippleEnableEXTPtr = pVkCmdSetLineStippleEnableEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetLineStippleEnableEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetLineStippleEnableEXT is null" Nothing Nothing
  let vkCmdSetLineStippleEnableEXT' = mkVkCmdSetLineStippleEnableEXT vkCmdSetLineStippleEnableEXTPtr
  traceAroundEvent "vkCmdSetLineStippleEnableEXT" (vkCmdSetLineStippleEnableEXT'
                                                     (commandBufferHandle (commandBuffer))
                                                     (boolToBool32 (stippledLineEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthClipNegativeOneToOneEXT
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetDepthClipNegativeOneToOneEXT - Specify the negative one to one
-- depth clip mode dynamically for a command buffer
--
-- = Description
--
-- This command sets the @negativeOneToOne@ state for subsequent drawing
-- commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_NEGATIVE_ONE_TO_ONE_EXT'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_EXT_depth_clip_control.PipelineViewportDepthClipControlCreateInfoEXT'::@negativeOneToOne@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetDepthClipNegativeOneToOneEXT-None-09423# At least one
--     of the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3DepthClipNegativeOneToOne extendedDynamicState3DepthClipNegativeOneToOne>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- -   #VUID-vkCmdSetDepthClipNegativeOneToOneEXT-depthClipControl-07453#
--     The
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-depthClipControl depthClipControl>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetDepthClipNegativeOneToOneEXT-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetDepthClipNegativeOneToOneEXT-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetDepthClipNegativeOneToOneEXT-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetDepthClipNegativeOneToOneEXT-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetDepthClipNegativeOneToOneEXT :: forall io
                                    . (MonadIO io)
                                   => -- | @commandBuffer@ is the command buffer into which the command will be
                                      -- recorded.
                                      CommandBuffer
                                   -> -- | @negativeOneToOne@ specifies the @negativeOneToOne@ state.
                                      ("negativeOneToOne" ::: Bool)
                                   -> io ()
cmdSetDepthClipNegativeOneToOneEXT commandBuffer negativeOneToOne = liftIO $ do
  let vkCmdSetDepthClipNegativeOneToOneEXTPtr = pVkCmdSetDepthClipNegativeOneToOneEXT (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetDepthClipNegativeOneToOneEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetDepthClipNegativeOneToOneEXT is null" Nothing Nothing
  let vkCmdSetDepthClipNegativeOneToOneEXT' = mkVkCmdSetDepthClipNegativeOneToOneEXT vkCmdSetDepthClipNegativeOneToOneEXTPtr
  traceAroundEvent "vkCmdSetDepthClipNegativeOneToOneEXT" (vkCmdSetDepthClipNegativeOneToOneEXT'
                                                             (commandBufferHandle (commandBuffer))
                                                             (boolToBool32 (negativeOneToOne)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportWScalingEnableNV
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetViewportWScalingEnableNV - Specify the viewport W scaling enable
-- state dynamically for a command buffer
--
-- = Description
--
-- This command sets the @viewportWScalingEnable@ state for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_ENABLE_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportWScalingEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetViewportWScalingEnableNV-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3ViewportWScalingEnable extendedDynamicState3ViewportWScalingEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetViewportWScalingEnableNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetViewportWScalingEnableNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetViewportWScalingEnableNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetViewportWScalingEnableNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_clip_space_w_scaling VK_NV_clip_space_w_scaling>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetViewportWScalingEnableNV :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @viewportWScalingEnable@ specifies the @viewportWScalingEnable@ state.
                                  ("viewportWScalingEnable" ::: Bool)
                               -> io ()
cmdSetViewportWScalingEnableNV commandBuffer
                                 viewportWScalingEnable = liftIO $ do
  let vkCmdSetViewportWScalingEnableNVPtr = pVkCmdSetViewportWScalingEnableNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetViewportWScalingEnableNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewportWScalingEnableNV is null" Nothing Nothing
  let vkCmdSetViewportWScalingEnableNV' = mkVkCmdSetViewportWScalingEnableNV vkCmdSetViewportWScalingEnableNVPtr
  traceAroundEvent "vkCmdSetViewportWScalingEnableNV" (vkCmdSetViewportWScalingEnableNV'
                                                         (commandBufferHandle (commandBuffer))
                                                         (boolToBool32 (viewportWScalingEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportSwizzleNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ViewportSwizzleNV -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Word32 -> Ptr ViewportSwizzleNV -> IO ()

-- | vkCmdSetViewportSwizzleNV - Specify the viewport swizzle state
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the viewport swizzle state for subsequent drawing
-- commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV' set
-- in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@viewportCount@,
-- and
-- 'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV'::@pViewportSwizzles@
-- values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetViewportSwizzleNV-None-09423# At least one of the
--     following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3ViewportSwizzle extendedDynamicState3ViewportSwizzle>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetViewportSwizzleNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetViewportSwizzleNV-pViewportSwizzles-parameter#
--     @pViewportSwizzles@ /must/ be a valid pointer to an array of
--     @viewportCount@ valid
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.ViewportSwizzleNV'
--     structures
--
-- -   #VUID-vkCmdSetViewportSwizzleNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetViewportSwizzleNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetViewportSwizzleNV-videocoding# This command /must/
--     only be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetViewportSwizzleNV-viewportCount-arraylength#
--     @viewportCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_viewport_swizzle VK_NV_viewport_swizzle>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.VK_NV_viewport_swizzle.ViewportSwizzleNV'
cmdSetViewportSwizzleNV :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @firstViewport@ is the index of the first viewport whose parameters are
                           -- updated by the command.
                           ("firstViewport" ::: Word32)
                        -> -- | @pViewportSwizzles@ is a pointer to an array of
                           -- 'Vulkan.Extensions.VK_NV_viewport_swizzle.ViewportSwizzleNV' structures
                           -- specifying viewport swizzles.
                           ("viewportSwizzles" ::: Vector ViewportSwizzleNV)
                        -> io ()
cmdSetViewportSwizzleNV commandBuffer
                          firstViewport
                          viewportSwizzles = liftIO . evalContT $ do
  let vkCmdSetViewportSwizzleNVPtr = pVkCmdSetViewportSwizzleNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetViewportSwizzleNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetViewportSwizzleNV is null" Nothing Nothing
  let vkCmdSetViewportSwizzleNV' = mkVkCmdSetViewportSwizzleNV vkCmdSetViewportSwizzleNVPtr
  pPViewportSwizzles <- ContT $ allocaBytes @ViewportSwizzleNV ((Data.Vector.length (viewportSwizzles)) * 16)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPViewportSwizzles `plusPtr` (16 * (i)) :: Ptr ViewportSwizzleNV) (e)) (viewportSwizzles)
  lift $ traceAroundEvent "vkCmdSetViewportSwizzleNV" (vkCmdSetViewportSwizzleNV'
                                                         (commandBufferHandle (commandBuffer))
                                                         (firstViewport)
                                                         ((fromIntegral (Data.Vector.length $ (viewportSwizzles)) :: Word32))
                                                         (pPViewportSwizzles))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCoverageToColorEnableNV
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetCoverageToColorEnableNV - Specify the coverage to color enable
-- state dynamically for a command buffer
--
-- = Description
--
-- This command sets the @coverageToColorEnable@ state for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_NV_fragment_coverage_to_color.PipelineCoverageToColorStateCreateInfoNV'::@coverageToColorEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetCoverageToColorEnableNV-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3CoverageToColorEnable extendedDynamicState3CoverageToColorEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetCoverageToColorEnableNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetCoverageToColorEnableNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetCoverageToColorEnableNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetCoverageToColorEnableNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_fragment_coverage_to_color VK_NV_fragment_coverage_to_color>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetCoverageToColorEnableNV :: forall io
                               . (MonadIO io)
                              => -- | @commandBuffer@ is the command buffer into which the command will be
                                 -- recorded.
                                 CommandBuffer
                              -> -- | @coverageToColorEnable@ specifies the @coverageToColorEnable@ state.
                                 ("coverageToColorEnable" ::: Bool)
                              -> io ()
cmdSetCoverageToColorEnableNV commandBuffer coverageToColorEnable = liftIO $ do
  let vkCmdSetCoverageToColorEnableNVPtr = pVkCmdSetCoverageToColorEnableNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetCoverageToColorEnableNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCoverageToColorEnableNV is null" Nothing Nothing
  let vkCmdSetCoverageToColorEnableNV' = mkVkCmdSetCoverageToColorEnableNV vkCmdSetCoverageToColorEnableNVPtr
  traceAroundEvent "vkCmdSetCoverageToColorEnableNV" (vkCmdSetCoverageToColorEnableNV'
                                                        (commandBufferHandle (commandBuffer))
                                                        (boolToBool32 (coverageToColorEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCoverageToColorLocationNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> IO ()

-- | vkCmdSetCoverageToColorLocationNV - Specify the coverage to color
-- location dynamically for a command buffer
--
-- = Description
--
-- This command sets the @coverageToColorLocation@ state for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_LOCATION_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_NV_fragment_coverage_to_color.PipelineCoverageToColorStateCreateInfoNV'::@coverageToColorLocation@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetCoverageToColorLocationNV-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3CoverageToColorLocation extendedDynamicState3CoverageToColorLocation>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetCoverageToColorLocationNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetCoverageToColorLocationNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetCoverageToColorLocationNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetCoverageToColorLocationNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_fragment_coverage_to_color VK_NV_fragment_coverage_to_color>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetCoverageToColorLocationNV :: forall io
                                 . (MonadIO io)
                                => -- | @commandBuffer@ is the command buffer into which the command will be
                                   -- recorded.
                                   CommandBuffer
                                -> -- | @coverageToColorLocation@ specifies the @coverageToColorLocation@ state.
                                   ("coverageToColorLocation" ::: Word32)
                                -> io ()
cmdSetCoverageToColorLocationNV commandBuffer
                                  coverageToColorLocation = liftIO $ do
  let vkCmdSetCoverageToColorLocationNVPtr = pVkCmdSetCoverageToColorLocationNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetCoverageToColorLocationNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCoverageToColorLocationNV is null" Nothing Nothing
  let vkCmdSetCoverageToColorLocationNV' = mkVkCmdSetCoverageToColorLocationNV vkCmdSetCoverageToColorLocationNVPtr
  traceAroundEvent "vkCmdSetCoverageToColorLocationNV" (vkCmdSetCoverageToColorLocationNV'
                                                          (commandBufferHandle (commandBuffer))
                                                          (coverageToColorLocation))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCoverageModulationModeNV
  :: FunPtr (Ptr CommandBuffer_T -> CoverageModulationModeNV -> IO ()) -> Ptr CommandBuffer_T -> CoverageModulationModeNV -> IO ()

-- | vkCmdSetCoverageModulationModeNV - Specify the coverage modulation mode
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the @coverageModulationMode@ state for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_MODE_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV'::@coverageModulationMode@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetCoverageModulationModeNV-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3CoverageModulationMode extendedDynamicState3CoverageModulationMode>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetCoverageModulationModeNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetCoverageModulationModeNV-coverageModulationMode-parameter#
--     @coverageModulationMode@ /must/ be a valid
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.CoverageModulationModeNV'
--     value
--
-- -   #VUID-vkCmdSetCoverageModulationModeNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetCoverageModulationModeNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetCoverageModulationModeNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.CoverageModulationModeNV'
cmdSetCoverageModulationModeNV :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @coverageModulationMode@ specifies the @coverageModulationMode@ state.
                                  CoverageModulationModeNV
                               -> io ()
cmdSetCoverageModulationModeNV commandBuffer
                                 coverageModulationMode = liftIO $ do
  let vkCmdSetCoverageModulationModeNVPtr = pVkCmdSetCoverageModulationModeNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetCoverageModulationModeNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCoverageModulationModeNV is null" Nothing Nothing
  let vkCmdSetCoverageModulationModeNV' = mkVkCmdSetCoverageModulationModeNV vkCmdSetCoverageModulationModeNVPtr
  traceAroundEvent "vkCmdSetCoverageModulationModeNV" (vkCmdSetCoverageModulationModeNV'
                                                         (commandBufferHandle (commandBuffer))
                                                         (coverageModulationMode))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCoverageModulationTableEnableNV
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetCoverageModulationTableEnableNV - Specify the coverage
-- modulation table enable state dynamically for a command buffer
--
-- = Description
--
-- This command sets the @coverageModulationTableEnable@ state for
-- subsequent drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_ENABLE_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV'::@coverageModulationTableEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetCoverageModulationTableEnableNV-None-09423# At least
--     one of the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3CoverageModulationTableEnable extendedDynamicState3CoverageModulationTableEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetCoverageModulationTableEnableNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetCoverageModulationTableEnableNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetCoverageModulationTableEnableNV-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetCoverageModulationTableEnableNV-videocoding# This
--     command /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetCoverageModulationTableEnableNV :: forall io
                                       . (MonadIO io)
                                      => -- | @commandBuffer@ is the command buffer into which the command will be
                                         -- recorded.
                                         CommandBuffer
                                      -> -- | @coverageModulationTableEnable@ specifies the
                                         -- @coverageModulationTableEnable@ state.
                                         ("coverageModulationTableEnable" ::: Bool)
                                      -> io ()
cmdSetCoverageModulationTableEnableNV commandBuffer
                                        coverageModulationTableEnable = liftIO $ do
  let vkCmdSetCoverageModulationTableEnableNVPtr = pVkCmdSetCoverageModulationTableEnableNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetCoverageModulationTableEnableNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCoverageModulationTableEnableNV is null" Nothing Nothing
  let vkCmdSetCoverageModulationTableEnableNV' = mkVkCmdSetCoverageModulationTableEnableNV vkCmdSetCoverageModulationTableEnableNVPtr
  traceAroundEvent "vkCmdSetCoverageModulationTableEnableNV" (vkCmdSetCoverageModulationTableEnableNV'
                                                                (commandBufferHandle (commandBuffer))
                                                                (boolToBool32 (coverageModulationTableEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCoverageModulationTableNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr CFloat -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr CFloat -> IO ()

-- | vkCmdSetCoverageModulationTableNV - Specify the coverage modulation
-- table dynamically for a command buffer
--
-- = Description
--
-- This command sets the table of modulation factors for subsequent drawing
-- commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV'::@coverageModulationTableCount@,
-- and
-- 'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV'::@pCoverageModulationTable@
-- values used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetCoverageModulationTableNV-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3CoverageModulationTable extendedDynamicState3CoverageModulationTable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetCoverageModulationTableNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetCoverageModulationTableNV-pCoverageModulationTable-parameter#
--     @pCoverageModulationTable@ /must/ be a valid pointer to an array of
--     @coverageModulationTableCount@ @float@ values
--
-- -   #VUID-vkCmdSetCoverageModulationTableNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetCoverageModulationTableNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetCoverageModulationTableNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- -   #VUID-vkCmdSetCoverageModulationTableNV-coverageModulationTableCount-arraylength#
--     @coverageModulationTableCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_framebuffer_mixed_samples VK_NV_framebuffer_mixed_samples>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetCoverageModulationTableNV :: forall io
                                 . (MonadIO io)
                                => -- | @commandBuffer@ is the command buffer into which the command will be
                                   -- recorded.
                                   CommandBuffer
                                -> -- | @pCoverageModulationTable@ specifies the table of modulation factors
                                   -- containing a value for each number of covered samples.
                                   ("coverageModulationTable" ::: Vector Float)
                                -> io ()
cmdSetCoverageModulationTableNV commandBuffer
                                  coverageModulationTable = liftIO . evalContT $ do
  let vkCmdSetCoverageModulationTableNVPtr = pVkCmdSetCoverageModulationTableNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdSetCoverageModulationTableNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCoverageModulationTableNV is null" Nothing Nothing
  let vkCmdSetCoverageModulationTableNV' = mkVkCmdSetCoverageModulationTableNV vkCmdSetCoverageModulationTableNVPtr
  pPCoverageModulationTable <- ContT $ allocaBytes @CFloat ((Data.Vector.length (coverageModulationTable)) * 4)
  lift $ Data.Vector.imapM_ (\i e -> poke (pPCoverageModulationTable `plusPtr` (4 * (i)) :: Ptr CFloat) (CFloat (e))) (coverageModulationTable)
  lift $ traceAroundEvent "vkCmdSetCoverageModulationTableNV" (vkCmdSetCoverageModulationTableNV'
                                                                 (commandBufferHandle (commandBuffer))
                                                                 ((fromIntegral (Data.Vector.length $ (coverageModulationTable)) :: Word32))
                                                                 (pPCoverageModulationTable))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetShadingRateImageEnableNV
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetShadingRateImageEnableNV - Specify the shading rate image enable
-- state dynamically for a command buffer
--
-- = Description
--
-- This command sets the @shadingRateImageEnable@ state for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SHADING_RATE_IMAGE_ENABLE_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'::@shadingRateImageEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetShadingRateImageEnableNV-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3ShadingRateImageEnable extendedDynamicState3ShadingRateImageEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetShadingRateImageEnableNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetShadingRateImageEnableNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetShadingRateImageEnableNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetShadingRateImageEnableNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_shading_rate_image VK_NV_shading_rate_image>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetShadingRateImageEnableNV :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @shadingRateImageEnable@ specifies the @shadingRateImageEnable@ state.
                                  ("shadingRateImageEnable" ::: Bool)
                               -> io ()
cmdSetShadingRateImageEnableNV commandBuffer
                                 shadingRateImageEnable = liftIO $ do
  let vkCmdSetShadingRateImageEnableNVPtr = pVkCmdSetShadingRateImageEnableNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetShadingRateImageEnableNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetShadingRateImageEnableNV is null" Nothing Nothing
  let vkCmdSetShadingRateImageEnableNV' = mkVkCmdSetShadingRateImageEnableNV vkCmdSetShadingRateImageEnableNVPtr
  traceAroundEvent "vkCmdSetShadingRateImageEnableNV" (vkCmdSetShadingRateImageEnableNV'
                                                         (commandBufferHandle (commandBuffer))
                                                         (boolToBool32 (shadingRateImageEnable)))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCoverageReductionModeNV
  :: FunPtr (Ptr CommandBuffer_T -> CoverageReductionModeNV -> IO ()) -> Ptr CommandBuffer_T -> CoverageReductionModeNV -> IO ()

-- | vkCmdSetCoverageReductionModeNV - Specify the coverage reduction mode
-- dynamically for a command buffer
--
-- = Description
--
-- This command sets the @coverageReductionMode@ state for subsequent
-- drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_REDUCTION_MODE_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateInfoNV'::@coverageReductionMode@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetCoverageReductionModeNV-None-09423# At least one of
--     the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3CoverageReductionMode extendedDynamicState3CoverageReductionMode>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetCoverageReductionModeNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetCoverageReductionModeNV-coverageReductionMode-parameter#
--     @coverageReductionMode@ /must/ be a valid
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.CoverageReductionModeNV'
--     value
--
-- -   #VUID-vkCmdSetCoverageReductionModeNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetCoverageReductionModeNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetCoverageReductionModeNV-videocoding# This command
--     /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode VK_NV_coverage_reduction_mode>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.CoverageReductionModeNV'
cmdSetCoverageReductionModeNV :: forall io
                               . (MonadIO io)
                              => -- | @commandBuffer@ is the command buffer into which the command will be
                                 -- recorded.
                                 CommandBuffer
                              -> -- | @coverageReductionMode@ specifies the @coverageReductionMode@ state.
                                 CoverageReductionModeNV
                              -> io ()
cmdSetCoverageReductionModeNV commandBuffer coverageReductionMode = liftIO $ do
  let vkCmdSetCoverageReductionModeNVPtr = pVkCmdSetCoverageReductionModeNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetCoverageReductionModeNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetCoverageReductionModeNV is null" Nothing Nothing
  let vkCmdSetCoverageReductionModeNV' = mkVkCmdSetCoverageReductionModeNV vkCmdSetCoverageReductionModeNVPtr
  traceAroundEvent "vkCmdSetCoverageReductionModeNV" (vkCmdSetCoverageReductionModeNV'
                                                        (commandBufferHandle (commandBuffer))
                                                        (coverageReductionMode))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetRepresentativeFragmentTestEnableNV
  :: FunPtr (Ptr CommandBuffer_T -> Bool32 -> IO ()) -> Ptr CommandBuffer_T -> Bool32 -> IO ()

-- | vkCmdSetRepresentativeFragmentTestEnableNV - Specify the representative
-- fragment test enable dynamically for a command buffer
--
-- = Description
--
-- This command sets the @representativeFragmentTestEnable@ state for
-- subsequent drawing commands when drawing using
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-objects shader objects>,
-- or when the graphics pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_REPRESENTATIVE_FRAGMENT_TEST_ENABLE_NV'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, this state is specified by the
-- 'Vulkan.Extensions.VK_NV_representative_fragment_test.PipelineRepresentativeFragmentTestStateCreateInfoNV'::@representativeFragmentTestEnable@
-- value used to create the currently active pipeline.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetRepresentativeFragmentTestEnableNV-None-09423# At
--     least one of the following /must/ be true:
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState3RepresentativeFragmentTestEnable extendedDynamicState3RepresentativeFragmentTestEnable>
--         feature is enabled
--
--     -   The
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shaderObject shaderObject>
--         feature is enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetRepresentativeFragmentTestEnableNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetRepresentativeFragmentTestEnableNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetRepresentativeFragmentTestEnableNV-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdSetRepresentativeFragmentTestEnableNV-videocoding# This
--     command /must/ only be called outside of a video coding scope
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
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
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_representative_fragment_test VK_NV_representative_fragment_test>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetRepresentativeFragmentTestEnableNV :: forall io
                                          . (MonadIO io)
                                         => -- | @commandBuffer@ is the command buffer into which the command will be
                                            -- recorded.
                                            CommandBuffer
                                         -> -- | @representativeFragmentTestEnable@ specifies the
                                            -- @representativeFragmentTestEnable@ state.
                                            ("representativeFragmentTestEnable" ::: Bool)
                                         -> io ()
cmdSetRepresentativeFragmentTestEnableNV commandBuffer
                                           representativeFragmentTestEnable = liftIO $ do
  let vkCmdSetRepresentativeFragmentTestEnableNVPtr = pVkCmdSetRepresentativeFragmentTestEnableNV (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetRepresentativeFragmentTestEnableNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetRepresentativeFragmentTestEnableNV is null" Nothing Nothing
  let vkCmdSetRepresentativeFragmentTestEnableNV' = mkVkCmdSetRepresentativeFragmentTestEnableNV vkCmdSetRepresentativeFragmentTestEnableNVPtr
  traceAroundEvent "vkCmdSetRepresentativeFragmentTestEnableNV" (vkCmdSetRepresentativeFragmentTestEnableNV'
                                                                   (commandBufferHandle (commandBuffer))
                                                                   (boolToBool32 (representativeFragmentTestEnable)))
  pure $ ()


-- | VkPhysicalDeviceExtendedDynamicState3FeaturesEXT - Structure describing
-- what extended dynamic state is supported by the implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceExtendedDynamicState3FeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceExtendedDynamicState3FeaturesEXT' /can/ also
-- be used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo'
-- to selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExtendedDynamicState3FeaturesEXT = PhysicalDeviceExtendedDynamicState3FeaturesEXT
  { -- | #features-extendedDynamicState3TessellationDomainOrigin#
    -- @extendedDynamicState3TessellationDomainOrigin@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_TESSELLATION_DOMAIN_ORIGIN_EXT'
    extendedDynamicState3TessellationDomainOrigin :: Bool
  , -- | #features-extendedDynamicState3DepthClampEnable#
    -- @extendedDynamicState3DepthClampEnable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_ENABLE_EXT'
    extendedDynamicState3DepthClampEnable :: Bool
  , -- | #features-extendedDynamicState3PolygonMode#
    -- @extendedDynamicState3PolygonMode@ indicates that the implementation
    -- supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_POLYGON_MODE_EXT'
    extendedDynamicState3PolygonMode :: Bool
  , -- | #features-extendedDynamicState3RasterizationSamples#
    -- @extendedDynamicState3RasterizationSamples@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
    extendedDynamicState3RasterizationSamples :: Bool
  , -- | #features-extendedDynamicState3SampleMask#
    -- @extendedDynamicState3SampleMask@ indicates that the implementation
    -- supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
    extendedDynamicState3SampleMask :: Bool
  , -- | #features-extendedDynamicState3AlphaToCoverageEnable#
    -- @extendedDynamicState3AlphaToCoverageEnable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
    extendedDynamicState3AlphaToCoverageEnable :: Bool
  , -- | #features-extendedDynamicState3AlphaToOneEnable#
    -- @extendedDynamicState3AlphaToOneEnable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
    extendedDynamicState3AlphaToOneEnable :: Bool
  , -- | #features-extendedDynamicState3LogicOpEnable#
    -- @extendedDynamicState3LogicOpEnable@ indicates that the implementation
    -- supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_ENABLE_EXT'
    extendedDynamicState3LogicOpEnable :: Bool
  , -- | #features-extendedDynamicState3ColorBlendEnable#
    -- @extendedDynamicState3ColorBlendEnable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
    extendedDynamicState3ColorBlendEnable :: Bool
  , -- | #features-extendedDynamicState3ColorBlendEquation#
    -- @extendedDynamicState3ColorBlendEquation@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
    extendedDynamicState3ColorBlendEquation :: Bool
  , -- | #features-extendedDynamicState3ColorWriteMask#
    -- @extendedDynamicState3ColorWriteMask@ indicates that the implementation
    -- supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
    extendedDynamicState3ColorWriteMask :: Bool
  , -- | #features-extendedDynamicState3RasterizationStream#
    -- @extendedDynamicState3RasterizationStream@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_STREAM_EXT'
    extendedDynamicState3RasterizationStream :: Bool
  , -- | #features-extendedDynamicState3ConservativeRasterizationMode#
    -- @extendedDynamicState3ConservativeRasterizationMode@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CONSERVATIVE_RASTERIZATION_MODE_EXT'
    extendedDynamicState3ConservativeRasterizationMode :: Bool
  , -- | #features-extendedDynamicState3ExtraPrimitiveOverestimationSize#
    -- @extendedDynamicState3ExtraPrimitiveOverestimationSize@ indicates that
    -- the implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXTRA_PRIMITIVE_OVERESTIMATION_SIZE_EXT'
    extendedDynamicState3ExtraPrimitiveOverestimationSize :: Bool
  , -- | #features-extendedDynamicState3DepthClipEnable#
    -- @extendedDynamicState3DepthClipEnable@ indicates that the implementation
    -- supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
    extendedDynamicState3DepthClipEnable :: Bool
  , -- | #features-extendedDynamicState3SampleLocationsEnable#
    -- @extendedDynamicState3SampleLocationsEnable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
    extendedDynamicState3SampleLocationsEnable :: Bool
  , -- | #features-extendedDynamicState3ColorBlendAdvanced#
    -- @extendedDynamicState3ColorBlendAdvanced@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
    extendedDynamicState3ColorBlendAdvanced :: Bool
  , -- | #features-extendedDynamicState3ProvokingVertexMode#
    -- @extendedDynamicState3ProvokingVertexMode@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PROVOKING_VERTEX_MODE_EXT'
    extendedDynamicState3ProvokingVertexMode :: Bool
  , -- | #features-extendedDynamicState3LineRasterizationMode#
    -- @extendedDynamicState3LineRasterizationMode@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
    extendedDynamicState3LineRasterizationMode :: Bool
  , -- | #features-extendedDynamicState3LineStippleEnable#
    -- @extendedDynamicState3LineStippleEnable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
    extendedDynamicState3LineStippleEnable :: Bool
  , -- | #features-extendedDynamicState3DepthClipNegativeOneToOne#
    -- @extendedDynamicState3DepthClipNegativeOneToOne@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_NEGATIVE_ONE_TO_ONE_EXT'
    extendedDynamicState3DepthClipNegativeOneToOne :: Bool
  , -- | #features-extendedDynamicState3ViewportWScalingEnable#
    -- @extendedDynamicState3ViewportWScalingEnable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_ENABLE_NV'
    extendedDynamicState3ViewportWScalingEnable :: Bool
  , -- | #features-extendedDynamicState3ViewportSwizzle#
    -- @extendedDynamicState3ViewportSwizzle@ indicates that the implementation
    -- supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV'
    extendedDynamicState3ViewportSwizzle :: Bool
  , -- | #features-extendedDynamicState3CoverageToColorEnable#
    -- @extendedDynamicState3CoverageToColorEnable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
    extendedDynamicState3CoverageToColorEnable :: Bool
  , -- | #features-extendedDynamicState3CoverageToColorLocation#
    -- @extendedDynamicState3CoverageToColorLocation@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_LOCATION_NV'
    extendedDynamicState3CoverageToColorLocation :: Bool
  , -- | #features-extendedDynamicState3CoverageModulationMode#
    -- @extendedDynamicState3CoverageModulationMode@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_MODE_NV'
    extendedDynamicState3CoverageModulationMode :: Bool
  , -- | #features-extendedDynamicState3CoverageModulationTableEnable#
    -- @extendedDynamicState3CoverageModulationTableEnable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_ENABLE_NV'
    extendedDynamicState3CoverageModulationTableEnable :: Bool
  , -- | #features-extendedDynamicState3CoverageModulationTable#
    -- @extendedDynamicState3CoverageModulationTable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_NV'
    extendedDynamicState3CoverageModulationTable :: Bool
  , -- | #features-extendedDynamicState3CoverageReductionMode#
    -- @extendedDynamicState3CoverageReductionMode@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_REDUCTION_MODE_NV'
    extendedDynamicState3CoverageReductionMode :: Bool
  , -- | #features-extendedDynamicState3RepresentativeFragmentTestEnable#
    -- @extendedDynamicState3RepresentativeFragmentTestEnable@ indicates that
    -- the implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_REPRESENTATIVE_FRAGMENT_TEST_ENABLE_NV'
    extendedDynamicState3RepresentativeFragmentTestEnable :: Bool
  , -- | #features-extendedDynamicState3ShadingRateImageEnable#
    -- @extendedDynamicState3ShadingRateImageEnable@ indicates that the
    -- implementation supports the following dynamic state:
    --
    -- -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SHADING_RATE_IMAGE_ENABLE_NV'
    extendedDynamicState3ShadingRateImageEnable :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExtendedDynamicState3FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceExtendedDynamicState3FeaturesEXT

instance ToCStruct PhysicalDeviceExtendedDynamicState3FeaturesEXT where
  withCStruct x f = allocaBytes 144 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExtendedDynamicState3FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3TessellationDomainOrigin))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3DepthClampEnable))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3PolygonMode))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3RasterizationSamples))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3SampleMask))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3AlphaToCoverageEnable))
    poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3AlphaToOneEnable))
    poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3LogicOpEnable))
    poke ((p `plusPtr` 48 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3ColorBlendEnable))
    poke ((p `plusPtr` 52 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3ColorBlendEquation))
    poke ((p `plusPtr` 56 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3ColorWriteMask))
    poke ((p `plusPtr` 60 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3RasterizationStream))
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3ConservativeRasterizationMode))
    poke ((p `plusPtr` 68 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3ExtraPrimitiveOverestimationSize))
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3DepthClipEnable))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3SampleLocationsEnable))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3ColorBlendAdvanced))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3ProvokingVertexMode))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3LineRasterizationMode))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3LineStippleEnable))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3DepthClipNegativeOneToOne))
    poke ((p `plusPtr` 100 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3ViewportWScalingEnable))
    poke ((p `plusPtr` 104 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3ViewportSwizzle))
    poke ((p `plusPtr` 108 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3CoverageToColorEnable))
    poke ((p `plusPtr` 112 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3CoverageToColorLocation))
    poke ((p `plusPtr` 116 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3CoverageModulationMode))
    poke ((p `plusPtr` 120 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3CoverageModulationTableEnable))
    poke ((p `plusPtr` 124 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3CoverageModulationTable))
    poke ((p `plusPtr` 128 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3CoverageReductionMode))
    poke ((p `plusPtr` 132 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3RepresentativeFragmentTestEnable))
    poke ((p `plusPtr` 136 :: Ptr Bool32)) (boolToBool32 (extendedDynamicState3ShadingRateImageEnable))
    f
  cStructSize = 144
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_FEATURES_EXT)
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
    poke ((p `plusPtr` 72 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 76 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 80 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 84 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 88 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 92 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 96 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 100 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 104 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 108 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 112 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 116 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 120 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 124 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 128 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 132 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 136 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExtendedDynamicState3FeaturesEXT where
  peekCStruct p = do
    extendedDynamicState3TessellationDomainOrigin <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    extendedDynamicState3DepthClampEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    extendedDynamicState3PolygonMode <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    extendedDynamicState3RasterizationSamples <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    extendedDynamicState3SampleMask <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    extendedDynamicState3AlphaToCoverageEnable <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    extendedDynamicState3AlphaToOneEnable <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    extendedDynamicState3LogicOpEnable <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    extendedDynamicState3ColorBlendEnable <- peek @Bool32 ((p `plusPtr` 48 :: Ptr Bool32))
    extendedDynamicState3ColorBlendEquation <- peek @Bool32 ((p `plusPtr` 52 :: Ptr Bool32))
    extendedDynamicState3ColorWriteMask <- peek @Bool32 ((p `plusPtr` 56 :: Ptr Bool32))
    extendedDynamicState3RasterizationStream <- peek @Bool32 ((p `plusPtr` 60 :: Ptr Bool32))
    extendedDynamicState3ConservativeRasterizationMode <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    extendedDynamicState3ExtraPrimitiveOverestimationSize <- peek @Bool32 ((p `plusPtr` 68 :: Ptr Bool32))
    extendedDynamicState3DepthClipEnable <- peek @Bool32 ((p `plusPtr` 72 :: Ptr Bool32))
    extendedDynamicState3SampleLocationsEnable <- peek @Bool32 ((p `plusPtr` 76 :: Ptr Bool32))
    extendedDynamicState3ColorBlendAdvanced <- peek @Bool32 ((p `plusPtr` 80 :: Ptr Bool32))
    extendedDynamicState3ProvokingVertexMode <- peek @Bool32 ((p `plusPtr` 84 :: Ptr Bool32))
    extendedDynamicState3LineRasterizationMode <- peek @Bool32 ((p `plusPtr` 88 :: Ptr Bool32))
    extendedDynamicState3LineStippleEnable <- peek @Bool32 ((p `plusPtr` 92 :: Ptr Bool32))
    extendedDynamicState3DepthClipNegativeOneToOne <- peek @Bool32 ((p `plusPtr` 96 :: Ptr Bool32))
    extendedDynamicState3ViewportWScalingEnable <- peek @Bool32 ((p `plusPtr` 100 :: Ptr Bool32))
    extendedDynamicState3ViewportSwizzle <- peek @Bool32 ((p `plusPtr` 104 :: Ptr Bool32))
    extendedDynamicState3CoverageToColorEnable <- peek @Bool32 ((p `plusPtr` 108 :: Ptr Bool32))
    extendedDynamicState3CoverageToColorLocation <- peek @Bool32 ((p `plusPtr` 112 :: Ptr Bool32))
    extendedDynamicState3CoverageModulationMode <- peek @Bool32 ((p `plusPtr` 116 :: Ptr Bool32))
    extendedDynamicState3CoverageModulationTableEnable <- peek @Bool32 ((p `plusPtr` 120 :: Ptr Bool32))
    extendedDynamicState3CoverageModulationTable <- peek @Bool32 ((p `plusPtr` 124 :: Ptr Bool32))
    extendedDynamicState3CoverageReductionMode <- peek @Bool32 ((p `plusPtr` 128 :: Ptr Bool32))
    extendedDynamicState3RepresentativeFragmentTestEnable <- peek @Bool32 ((p `plusPtr` 132 :: Ptr Bool32))
    extendedDynamicState3ShadingRateImageEnable <- peek @Bool32 ((p `plusPtr` 136 :: Ptr Bool32))
    pure $ PhysicalDeviceExtendedDynamicState3FeaturesEXT
             (bool32ToBool extendedDynamicState3TessellationDomainOrigin)
             (bool32ToBool extendedDynamicState3DepthClampEnable)
             (bool32ToBool extendedDynamicState3PolygonMode)
             (bool32ToBool extendedDynamicState3RasterizationSamples)
             (bool32ToBool extendedDynamicState3SampleMask)
             (bool32ToBool extendedDynamicState3AlphaToCoverageEnable)
             (bool32ToBool extendedDynamicState3AlphaToOneEnable)
             (bool32ToBool extendedDynamicState3LogicOpEnable)
             (bool32ToBool extendedDynamicState3ColorBlendEnable)
             (bool32ToBool extendedDynamicState3ColorBlendEquation)
             (bool32ToBool extendedDynamicState3ColorWriteMask)
             (bool32ToBool extendedDynamicState3RasterizationStream)
             (bool32ToBool extendedDynamicState3ConservativeRasterizationMode)
             (bool32ToBool extendedDynamicState3ExtraPrimitiveOverestimationSize)
             (bool32ToBool extendedDynamicState3DepthClipEnable)
             (bool32ToBool extendedDynamicState3SampleLocationsEnable)
             (bool32ToBool extendedDynamicState3ColorBlendAdvanced)
             (bool32ToBool extendedDynamicState3ProvokingVertexMode)
             (bool32ToBool extendedDynamicState3LineRasterizationMode)
             (bool32ToBool extendedDynamicState3LineStippleEnable)
             (bool32ToBool extendedDynamicState3DepthClipNegativeOneToOne)
             (bool32ToBool extendedDynamicState3ViewportWScalingEnable)
             (bool32ToBool extendedDynamicState3ViewportSwizzle)
             (bool32ToBool extendedDynamicState3CoverageToColorEnable)
             (bool32ToBool extendedDynamicState3CoverageToColorLocation)
             (bool32ToBool extendedDynamicState3CoverageModulationMode)
             (bool32ToBool extendedDynamicState3CoverageModulationTableEnable)
             (bool32ToBool extendedDynamicState3CoverageModulationTable)
             (bool32ToBool extendedDynamicState3CoverageReductionMode)
             (bool32ToBool extendedDynamicState3RepresentativeFragmentTestEnable)
             (bool32ToBool extendedDynamicState3ShadingRateImageEnable)

instance Storable PhysicalDeviceExtendedDynamicState3FeaturesEXT where
  sizeOf ~_ = 144
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExtendedDynamicState3FeaturesEXT where
  zero = PhysicalDeviceExtendedDynamicState3FeaturesEXT
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
           zero
           zero
           zero


-- | VkPhysicalDeviceExtendedDynamicState3PropertiesEXT - Structure
-- describing capabilities of extended dynamic state
--
-- = Description
--
-- If the 'PhysicalDeviceExtendedDynamicState3PropertiesEXT' structure is
-- included in the @pNext@ chain of the
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceExtendedDynamicState3PropertiesEXT = PhysicalDeviceExtendedDynamicState3PropertiesEXT
  { -- | #limits-dynamicPrimitiveTopologyUnrestricted#
    -- @dynamicPrimitiveTopologyUnrestricted@ indicates that the implementation
    -- allows
    -- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopology'
    -- to use a different
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#drawing-primitive-topology-class primitive topology class>
    -- to the one specified in the active graphics pipeline.
    dynamicPrimitiveTopologyUnrestricted :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceExtendedDynamicState3PropertiesEXT)
#endif
deriving instance Show PhysicalDeviceExtendedDynamicState3PropertiesEXT

instance ToCStruct PhysicalDeviceExtendedDynamicState3PropertiesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceExtendedDynamicState3PropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (dynamicPrimitiveTopologyUnrestricted))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceExtendedDynamicState3PropertiesEXT where
  peekCStruct p = do
    dynamicPrimitiveTopologyUnrestricted <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceExtendedDynamicState3PropertiesEXT
             (bool32ToBool dynamicPrimitiveTopologyUnrestricted)

instance Storable PhysicalDeviceExtendedDynamicState3PropertiesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceExtendedDynamicState3PropertiesEXT where
  zero = PhysicalDeviceExtendedDynamicState3PropertiesEXT
           zero


-- | VkColorBlendEquationEXT - Structure specifying the color blend factors
-- and operations for an attachment
--
-- == Valid Usage
--
-- -   #VUID-VkColorBlendEquationEXT-dualSrcBlend-07357# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dualSrcBlend dualSrcBlend>
--     feature is not enabled, @srcColorBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   #VUID-VkColorBlendEquationEXT-dualSrcBlend-07358# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dualSrcBlend dualSrcBlend>
--     feature is not enabled, @dstColorBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   #VUID-VkColorBlendEquationEXT-dualSrcBlend-07359# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dualSrcBlend dualSrcBlend>
--     feature is not enabled, @srcAlphaBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   #VUID-VkColorBlendEquationEXT-dualSrcBlend-07360# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-dualSrcBlend dualSrcBlend>
--     feature is not enabled, @dstAlphaBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   #VUID-VkColorBlendEquationEXT-colorBlendOp-07361# @colorBlendOp@ and
--     @alphaBlendOp@ /must/ not be
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_ZERO_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_OVER_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_OVER_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_IN_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_IN_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_OUT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_OUT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_ATOP_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_ATOP_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_XOR_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MULTIPLY_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SCREEN_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_OVERLAY_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DARKEN_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LIGHTEN_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_COLORDODGE_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_COLORBURN_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HARDLIGHT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_SOFTLIGHT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_DIFFERENCE_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_EXCLUSION_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_RGB_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARDODGE_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARBURN_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_VIVIDLIGHT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARLIGHT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PINLIGHT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HARDMIX_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_HUE_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_SATURATION_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_COLOR_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HSL_LUMINOSITY_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_CLAMPED_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_CLAMPED_ALPHA_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_DARKER_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MINUS_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_MINUS_CLAMPED_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_CONTRAST_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_OVG_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_RED_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_GREEN_EXT', or
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_BLUE_EXT'
--
-- -   #VUID-VkColorBlendEquationEXT-constantAlphaColorBlendFactors-07362#
--     If the @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@constantAlphaColorBlendFactors@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', @srcColorBlendFactor@
--     /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_CONSTANT_ALPHA' or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA'
--
-- -   #VUID-VkColorBlendEquationEXT-constantAlphaColorBlendFactors-07363#
--     If the @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@constantAlphaColorBlendFactors@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', @dstColorBlendFactor@
--     /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_CONSTANT_ALPHA' or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkColorBlendEquationEXT-srcColorBlendFactor-parameter#
--     @srcColorBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   #VUID-VkColorBlendEquationEXT-dstColorBlendFactor-parameter#
--     @dstColorBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   #VUID-VkColorBlendEquationEXT-colorBlendOp-parameter# @colorBlendOp@
--     /must/ be a valid 'Vulkan.Core10.Enums.BlendOp.BlendOp' value
--
-- -   #VUID-VkColorBlendEquationEXT-srcAlphaBlendFactor-parameter#
--     @srcAlphaBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   #VUID-VkColorBlendEquationEXT-dstAlphaBlendFactor-parameter#
--     @dstAlphaBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   #VUID-VkColorBlendEquationEXT-alphaBlendOp-parameter# @alphaBlendOp@
--     /must/ be a valid 'Vulkan.Core10.Enums.BlendOp.BlendOp' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Enums.BlendFactor.BlendFactor',
-- 'Vulkan.Core10.Enums.BlendOp.BlendOp', 'cmdSetColorBlendEquationEXT'
data ColorBlendEquationEXT = ColorBlendEquationEXT
  { -- | @srcColorBlendFactor@ selects which blend factor is used to determine
    -- the source factors (Sr,Sg,Sb).
    srcColorBlendFactor :: BlendFactor
  , -- | @dstColorBlendFactor@ selects which blend factor is used to determine
    -- the destination factors (Dr,Dg,Db).
    dstColorBlendFactor :: BlendFactor
  , -- | @colorBlendOp@ selects which blend operation is used to calculate the
    -- RGB values to write to the color attachment.
    colorBlendOp :: BlendOp
  , -- | @srcAlphaBlendFactor@ selects which blend factor is used to determine
    -- the source factor Sa.
    srcAlphaBlendFactor :: BlendFactor
  , -- | @dstAlphaBlendFactor@ selects which blend factor is used to determine
    -- the destination factor Da.
    dstAlphaBlendFactor :: BlendFactor
  , -- | @alphaBlendOp@ selects which blend operation is use to calculate the
    -- alpha values to write to the color attachment.
    alphaBlendOp :: BlendOp
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ColorBlendEquationEXT)
#endif
deriving instance Show ColorBlendEquationEXT

instance ToCStruct ColorBlendEquationEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ColorBlendEquationEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr BlendFactor)) (srcColorBlendFactor)
    poke ((p `plusPtr` 4 :: Ptr BlendFactor)) (dstColorBlendFactor)
    poke ((p `plusPtr` 8 :: Ptr BlendOp)) (colorBlendOp)
    poke ((p `plusPtr` 12 :: Ptr BlendFactor)) (srcAlphaBlendFactor)
    poke ((p `plusPtr` 16 :: Ptr BlendFactor)) (dstAlphaBlendFactor)
    poke ((p `plusPtr` 20 :: Ptr BlendOp)) (alphaBlendOp)
    f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr BlendFactor)) (zero)
    poke ((p `plusPtr` 4 :: Ptr BlendFactor)) (zero)
    poke ((p `plusPtr` 8 :: Ptr BlendOp)) (zero)
    poke ((p `plusPtr` 12 :: Ptr BlendFactor)) (zero)
    poke ((p `plusPtr` 16 :: Ptr BlendFactor)) (zero)
    poke ((p `plusPtr` 20 :: Ptr BlendOp)) (zero)
    f

instance FromCStruct ColorBlendEquationEXT where
  peekCStruct p = do
    srcColorBlendFactor <- peek @BlendFactor ((p `plusPtr` 0 :: Ptr BlendFactor))
    dstColorBlendFactor <- peek @BlendFactor ((p `plusPtr` 4 :: Ptr BlendFactor))
    colorBlendOp <- peek @BlendOp ((p `plusPtr` 8 :: Ptr BlendOp))
    srcAlphaBlendFactor <- peek @BlendFactor ((p `plusPtr` 12 :: Ptr BlendFactor))
    dstAlphaBlendFactor <- peek @BlendFactor ((p `plusPtr` 16 :: Ptr BlendFactor))
    alphaBlendOp <- peek @BlendOp ((p `plusPtr` 20 :: Ptr BlendOp))
    pure $ ColorBlendEquationEXT
             srcColorBlendFactor
             dstColorBlendFactor
             colorBlendOp
             srcAlphaBlendFactor
             dstAlphaBlendFactor
             alphaBlendOp

instance Storable ColorBlendEquationEXT where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ColorBlendEquationEXT where
  zero = ColorBlendEquationEXT
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkColorBlendAdvancedEXT - Structure specifying the advanced blend
-- operation parameters for an attachment
--
-- == Valid Usage
--
-- -   #VUID-VkColorBlendAdvancedEXT-srcPremultiplied-07505# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-advancedBlendNonPremultipliedSrcColor non-premultiplied source color>
--     property is not supported, @srcPremultiplied@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkColorBlendAdvancedEXT-dstPremultiplied-07506# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-advancedBlendNonPremultipliedDstColor non-premultiplied destination color>
--     property is not supported, @dstPremultiplied@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkColorBlendAdvancedEXT-blendOverlap-07507# If the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-advancedBlendCorrelatedOverlap correlated overlap>
--     property is not supported, @blendOverlap@ /must/ be
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.BLEND_OVERLAP_UNCORRELATED_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkColorBlendAdvancedEXT-advancedBlendOp-parameter#
--     @advancedBlendOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendOp.BlendOp' value
--
-- -   #VUID-VkColorBlendAdvancedEXT-blendOverlap-parameter# @blendOverlap@
--     /must/ be a valid
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.BlendOverlapEXT'
--     value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 VK_EXT_extended_dynamic_state3>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>,
-- 'Vulkan.Core10.Enums.BlendOp.BlendOp',
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.BlendOverlapEXT',
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'cmdSetColorBlendAdvancedEXT'
data ColorBlendAdvancedEXT = ColorBlendAdvancedEXT
  { -- | @advancedBlendOp@ selects which blend operation is used to calculate the
    -- RGB values to write to the color attachment.
    advancedBlendOp :: BlendOp
  , -- | @srcPremultiplied@ specifies whether the source color of the blend
    -- operation is treated as premultiplied.
    srcPremultiplied :: Bool
  , -- | @dstPremultiplied@ specifies whether the destination color of the blend
    -- operation is treated as premultiplied.
    dstPremultiplied :: Bool
  , -- | @blendOverlap@ is a
    -- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.BlendOverlapEXT'
    -- value specifying how the source and destination sample’s coverage is
    -- correlated.
    blendOverlap :: BlendOverlapEXT
  , -- | @clampResults@ specifies the results must be clamped to the [0,1] range
    -- before writing to the attachment, which is useful when the attachment
    -- format is not normalized fixed-point.
    clampResults :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ColorBlendAdvancedEXT)
#endif
deriving instance Show ColorBlendAdvancedEXT

instance ToCStruct ColorBlendAdvancedEXT where
  withCStruct x f = allocaBytes 20 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ColorBlendAdvancedEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr BlendOp)) (advancedBlendOp)
    poke ((p `plusPtr` 4 :: Ptr Bool32)) (boolToBool32 (srcPremultiplied))
    poke ((p `plusPtr` 8 :: Ptr Bool32)) (boolToBool32 (dstPremultiplied))
    poke ((p `plusPtr` 12 :: Ptr BlendOverlapEXT)) (blendOverlap)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (clampResults))
    f
  cStructSize = 20
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr BlendOp)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 8 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 12 :: Ptr BlendOverlapEXT)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct ColorBlendAdvancedEXT where
  peekCStruct p = do
    advancedBlendOp <- peek @BlendOp ((p `plusPtr` 0 :: Ptr BlendOp))
    srcPremultiplied <- peek @Bool32 ((p `plusPtr` 4 :: Ptr Bool32))
    dstPremultiplied <- peek @Bool32 ((p `plusPtr` 8 :: Ptr Bool32))
    blendOverlap <- peek @BlendOverlapEXT ((p `plusPtr` 12 :: Ptr BlendOverlapEXT))
    clampResults <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ ColorBlendAdvancedEXT
             advancedBlendOp
             (bool32ToBool srcPremultiplied)
             (bool32ToBool dstPremultiplied)
             blendOverlap
             (bool32ToBool clampResults)

instance Storable ColorBlendAdvancedEXT where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ColorBlendAdvancedEXT where
  zero = ColorBlendAdvancedEXT
           zero
           zero
           zero
           zero
           zero


type EXT_EXTENDED_DYNAMIC_STATE_3_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_EXTENDED_DYNAMIC_STATE_3_SPEC_VERSION"
pattern EXT_EXTENDED_DYNAMIC_STATE_3_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_EXTENDED_DYNAMIC_STATE_3_SPEC_VERSION = 2


type EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME = "VK_EXT_extended_dynamic_state3"

-- No documentation found for TopLevel "VK_EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME"
pattern EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME = "VK_EXT_extended_dynamic_state3"

