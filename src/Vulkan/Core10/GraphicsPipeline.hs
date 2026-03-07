{-# language CPP #-}
-- No documentation found for Chapter "GraphicsPipeline"
module Vulkan.Core10.GraphicsPipeline  ( createGraphicsPipelines
                                       , withGraphicsPipelines
                                       , Viewport(..)
                                       , VertexInputBindingDescription(..)
                                       , VertexInputAttributeDescription(..)
                                       , PipelineVertexInputStateCreateInfo(..)
                                       , PipelineInputAssemblyStateCreateInfo(..)
                                       , PipelineTessellationStateCreateInfo(..)
                                       , PipelineViewportStateCreateInfo(..)
                                       , PipelineRasterizationStateCreateInfo(..)
                                       , PipelineMultisampleStateCreateInfo(..)
                                       , PipelineColorBlendAttachmentState(..)
                                       , PipelineColorBlendStateCreateInfo(..)
                                       , PipelineDynamicStateCreateInfo(..)
                                       , StencilOpState(..)
                                       , PipelineDepthStencilStateCreateInfo(..)
                                       , GraphicsPipelineCreateInfo(..)
                                       , PipelineDynamicStateCreateFlags(..)
                                       , PipelineMultisampleStateCreateFlags(..)
                                       , PipelineRasterizationStateCreateFlags(..)
                                       , PipelineViewportStateCreateFlags(..)
                                       , PipelineTessellationStateCreateFlags(..)
                                       , PipelineInputAssemblyStateCreateFlags(..)
                                       , PipelineVertexInputStateCreateFlags(..)
                                       , PrimitiveTopology(..)
                                       , CompareOp(..)
                                       , PolygonMode(..)
                                       , FrontFace(..)
                                       , BlendFactor(..)
                                       , BlendOp(..)
                                       , StencilOp(..)
                                       , LogicOp(..)
                                       , VertexInputRate(..)
                                       , DynamicState(..)
                                       , CullModeFlagBits(..)
                                       , CullModeFlags
                                       , ColorComponentFlagBits(..)
                                       , ColorComponentFlags
                                       , PipelineColorBlendStateCreateFlagBits(..)
                                       , PipelineColorBlendStateCreateFlags
                                       , PipelineDepthStencilStateCreateFlagBits(..)
                                       , PipelineDepthStencilStateCreateFlags
                                       , SampleMask
                                       ) where

import Vulkan.CStruct.Utils (FixedArray)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import qualified Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.ComputePipeline (destroyPipeline)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_mixed_attachment_samples (AttachmentSampleCountInfoAMD)
import Vulkan.Core10.Enums.BlendFactor (BlendFactor)
import Vulkan.Core10.Enums.BlendOp (BlendOp)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Enums.ColorComponentFlagBits (ColorComponentFlags)
import Vulkan.Core10.Enums.CompareOp (CompareOp)
import Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_resolve (CustomResolveCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_bias_control (DepthBiasRepresentationInfoEXT)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateGraphicsPipelines))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.DynamicState (DynamicState)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ExternalFormatANDROID)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Enums.FrontFace (FrontFace)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_graphics_pipeline_library (GraphicsPipelineLibraryCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GraphicsPipelineShaderGroupsCreateInfoNV)
import Vulkan.Core10.Enums.LogicOp (LogicOp)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_multiview_per_view_attributes (MultiviewPerViewAttributesInfoNVX)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_binary (PipelineBinaryInfoKHR)
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PipelineColorBlendAdvancedStateCreateInfoEXT)
import Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits (PipelineColorBlendStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_color_write_enable (PipelineColorWriteCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_pipeline_compiler_control (PipelineCompilerControlCreateInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_framebuffer_mixed_samples (PipelineCoverageModulationStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (PipelineCoverageReductionStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_coverage_to_color (PipelineCoverageToColorStateCreateInfoNV)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap (PipelineCreateFlags2CreateInfo)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfo)
import Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits (PipelineDepthStencilStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_discard_rectangles (PipelineDiscardRectangleStateCreateInfoEXT)
import Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags (PipelineDynamicStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_VALVE_fragment_density_map_layered (PipelineFragmentDensityMapLayeredCreateInfoVALVE)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shading_rate_enums (PipelineFragmentShadingRateEnumStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PipelineFragmentShadingRateStateCreateInfoKHR)
import Vulkan.Core10.Enums.PipelineInputAssemblyStateCreateFlags (PipelineInputAssemblyStateCreateFlags)
import Vulkan.Core10.Handles (PipelineLayout)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR)
import Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags (PipelineMultisampleStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conservative_rasterization (PipelineRasterizationConservativeStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_enable (PipelineRasterizationDepthClipStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap (PipelineRasterizationLineStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_provoking_vertex (PipelineRasterizationProvokingVertexStateCreateInfoEXT)
import Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags (PipelineRasterizationStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_rasterization_order (PipelineRasterizationStateRasterizationOrderAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PipelineRasterizationStateStreamCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (PipelineRenderingCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_representative_fragment_test (PipelineRepresentativeFragmentTestStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality' (PipelineRobustnessCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (PipelineSampleLocationsStateCreateInfoEXT)
import Vulkan.Core10.ComputePipeline (PipelineShaderStageCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (PipelineTessellationDomainOriginStateCreateInfo)
import Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags (PipelineTessellationStateCreateFlags)
import {-# SOURCE #-} Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap (PipelineVertexInputDivisorStateCreateInfo)
import Vulkan.Core10.Enums.PipelineVertexInputStateCreateFlags (PipelineVertexInputStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PipelineViewportCoarseSampleOrderStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clamp_control (PipelineViewportDepthClampControlCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_control (PipelineViewportDepthClipControlCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_scissor_exclusive (PipelineViewportExclusiveScissorStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PipelineViewportShadingRateImageStateCreateInfoNV)
import Vulkan.Core10.Enums.PipelineViewportStateCreateFlags (PipelineViewportStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_viewport_swizzle (PipelineViewportSwizzleStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_clip_space_w_scaling (PipelineViewportWScalingStateCreateInfoNV)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.PolygonMode (PolygonMode)
import Vulkan.Core10.Enums.PrimitiveTopology (PrimitiveTopology)
import Vulkan.Core10.FundamentalTypes (Rect2D)
import Vulkan.Core10.Handles (RenderPass)
import {-# SOURCE #-} Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap (RenderingAttachmentLocationInfo)
import {-# SOURCE #-} Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap (RenderingInputAttachmentIndexInfo)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits(SampleCountFlagBits))
import Vulkan.Core10.FundamentalTypes (SampleMask)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StencilOp (StencilOp)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.VertexInputRate (VertexInputRate)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Enums.BlendFactor (BlendFactor(..))
import Vulkan.Core10.Enums.BlendOp (BlendOp(..))
import Vulkan.Core10.Enums.ColorComponentFlagBits (ColorComponentFlagBits(..))
import Vulkan.Core10.Enums.ColorComponentFlagBits (ColorComponentFlags)
import Vulkan.Core10.Enums.CompareOp (CompareOp(..))
import Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlagBits(..))
import Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlags)
import Vulkan.Core10.Enums.DynamicState (DynamicState(..))
import Vulkan.Core10.Enums.FrontFace (FrontFace(..))
import Vulkan.Core10.Enums.LogicOp (LogicOp(..))
import Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits (PipelineColorBlendStateCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits (PipelineColorBlendStateCreateFlags)
import Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits (PipelineDepthStencilStateCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits (PipelineDepthStencilStateCreateFlags)
import Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags (PipelineDynamicStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineInputAssemblyStateCreateFlags (PipelineInputAssemblyStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags (PipelineMultisampleStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags (PipelineRasterizationStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags (PipelineTessellationStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineVertexInputStateCreateFlags (PipelineVertexInputStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineViewportStateCreateFlags (PipelineViewportStateCreateFlags(..))
import Vulkan.Core10.Enums.PolygonMode (PolygonMode(..))
import Vulkan.Core10.Enums.PrimitiveTopology (PrimitiveTopology(..))
import Vulkan.Core10.FundamentalTypes (SampleMask)
import Vulkan.Core10.Enums.StencilOp (StencilOp(..))
import Vulkan.Core10.Enums.VertexInputRate (VertexInputRate(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateGraphicsPipelines
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct GraphicsPipelineCreateInfo) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct GraphicsPipelineCreateInfo) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateGraphicsPipelines - Create graphics pipelines
--
-- = Description
--
-- The 'GraphicsPipelineCreateInfo' structure includes an array of
-- 'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo' structures
-- for each of the desired active shader stages, as well as creation
-- information for all relevant fixed-function stages, and a pipeline
-- layout.
--
-- Pipelines are created and returned as described for
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-multiple Multiple Pipeline Creation>.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateGraphicsPipelines-device-09662# @device@ /must/
--     support at least one queue family with the
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' capability
--
-- -   #VUID-vkCreateGraphicsPipelines-flags-00720# If the @flags@ member
--     of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   #VUID-vkCreateGraphicsPipelines-flags-00721# If the @flags@ member
--     of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- -   #VUID-vkCreateGraphicsPipelines-pipelineCache-02876# If
--     @pipelineCache@ was created with
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT',
--     host access to @pipelineCache@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>
--
-- -   #VUID-vkCreateGraphicsPipelines-pNext-09616# If
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR'::@binaryCount@
--     is not @0@ for any element of @pCreateInfos@, @pipelineCache@ /must/
--     be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCreateGraphicsPipelines-pNext-09617# If a
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'
--     structure with the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'
--     flag set is included in the @pNext@ chain of any element of
--     @pCreateInfos@, @pipelineCache@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCreateGraphicsPipelines-binaryCount-09620# If
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR'::@binaryCount@
--     is not @0@ for any element of @pCreateInfos@,
--     'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT'
--     /must/ not be set in the @flags@ of that element
--
-- -   #VUID-vkCreateGraphicsPipelines-binaryCount-09621# If
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR'::@binaryCount@
--     is not @0@ for any element of @pCreateInfos@,
--     'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT'
--     /must/ not be set in the @flags@ of that element
--
-- -   #VUID-vkCreateGraphicsPipelines-binaryCount-09622# If
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR'::@binaryCount@
--     is not @0@ for any element of @pCreateInfos@,
--     'Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--     /must/ not be set in the @flags@ of that element
--
-- -   #VUID-vkCreateGraphicsPipelines-pCreateInfos-11414# If any element
--     of @pCreateInfos@ sets
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     and includes embedded sampler mappings, there /must/ be less than
--     (<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxSamplerAllocationCount maxSamplerAllocationCount>
--     -
--     (<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minSamplerHeapReservedRangeWithEmbedded minSamplerHeapReservedRangeWithEmbedded>
--     \/
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-samplerDescriptorSize samplerDescriptorSize>))
--     'Vulkan.Core10.Handles.Sampler' objects currently created on the
--     device
--
-- -   #VUID-vkCreateGraphicsPipelines-pCreateInfos-11429# If any element
--     of @pCreateInfos@ sets
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     and includes embedded sampler mappings, this command /must/ not
--     cause the total number of unique embedded samplers in pipelines and
--     shaders on this device to exceed
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxDescriptorHeapEmbeddedSamplers maxDescriptorHeapEmbeddedSamplers>
--
-- An implicit cache may be provided by the implementation or a layer. For
-- this reason, it is still valid to set
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
-- on @flags@ for any element of @pCreateInfos@ while passing
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE' for @pipelineCache@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateGraphicsPipelines-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateGraphicsPipelines-pipelineCache-parameter# If
--     @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   #VUID-vkCreateGraphicsPipelines-pCreateInfos-parameter#
--     @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'GraphicsPipelineCreateInfo' structures
--
-- -   #VUID-vkCreateGraphicsPipelines-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateGraphicsPipelines-pPipelines-parameter# @pPipelines@
--     /must/ be a valid pointer to an array of @createInfoCount@
--     'Vulkan.Core10.Handles.Pipeline' handles
--
-- -   #VUID-vkCreateGraphicsPipelines-createInfoCount-arraylength#
--     @createInfoCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCreateGraphicsPipelines-pipelineCache-parent# If
--     @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PIPELINE_COMPILE_REQUIRED_EXT'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Handles.Pipeline', 'Vulkan.Core10.Handles.PipelineCache'
createGraphicsPipelines :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the logical device that creates the graphics pipelines.
                           Device
                        -> -- | @pipelineCache@ is either 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                           -- indicating that pipeline caching is disabled, or to enable caching, the
                           -- handle of a valid 'Vulkan.Core10.Handles.PipelineCache' object. The
                           -- implementation /must/ not access this object outside of the duration of
                           -- this command.
                           PipelineCache
                        -> -- | @pCreateInfos@ is a pointer to an array of 'GraphicsPipelineCreateInfo'
                           -- structures.
                           ("createInfos" ::: Vector (SomeStruct GraphicsPipelineCreateInfo))
                        -> -- | @pAllocator@ controls host memory allocation as described in the
                           -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                           -- chapter.
                           ("allocator" ::: Maybe AllocationCallbacks)
                        -> io (Result, ("pipelines" ::: Vector Pipeline))
createGraphicsPipelines device
                          pipelineCache
                          createInfos
                          allocator = liftIO . evalContT $ do
  let vkCreateGraphicsPipelinesPtr = pVkCreateGraphicsPipelines (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateGraphicsPipelinesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateGraphicsPipelines is null" Nothing Nothing
  let vkCreateGraphicsPipelines' = mkVkCreateGraphicsPipelines vkCreateGraphicsPipelinesPtr
  pPCreateInfos <- ContT $ allocaBytes @(GraphicsPipelineCreateInfo _) ((Data.Vector.length (createInfos)) * 144)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (144 * (i)) :: Ptr (GraphicsPipelineCreateInfo _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ traceAroundEvent "vkCreateGraphicsPipelines" (vkCreateGraphicsPipelines'
                                                              (deviceHandle (device))
                                                              (pipelineCache)
                                                              ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))
                                                              (forgetExtensions (pPCreateInfos))
                                                              pAllocator
                                                              (pPPipelines))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (r, pPipelines)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createGraphicsPipelines' and 'destroyPipeline'
--
-- To ensure that 'destroyPipeline' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withGraphicsPipelines :: forall io r . MonadIO io => Device -> PipelineCache -> Vector (SomeStruct GraphicsPipelineCreateInfo) -> Maybe AllocationCallbacks -> (io (Result, Vector Pipeline) -> ((Result, Vector Pipeline) -> io ()) -> r) -> r
withGraphicsPipelines device pipelineCache pCreateInfos pAllocator b =
  b (createGraphicsPipelines device pipelineCache pCreateInfos pAllocator)
    (\(_, o1) -> traverse_ (\o1Elem -> destroyPipeline device
                                                         o1Elem
                                                         pAllocator) o1)


-- | VkViewport - Structure specifying a viewport
--
-- = Description
--
-- Despite their names, @minDepth@ /can/ be less than, equal to, or greater
-- than @maxDepth@.
--
-- The framebuffer depth coordinate @z@f /may/ be represented using either
-- a fixed-point or floating-point representation. However, a
-- floating-point representation /must/ be used if the depth\/stencil
-- attachment has a floating-point depth component. If an m-bit fixed-point
-- representation is used, we assume that it represents each value
-- \(\frac{k}{2^m - 1}\), where k ∈ { 0, 1, …​, 2m-1 }, as k (e.g. 1.0 is
-- represented in binary as a string of all ones).
--
-- The viewport parameters shown in the above equations are found from
-- these values as
--
-- -   ox = @x@ + @width@ \/ 2
--
-- -   oy = @y@ + @height@ \/ 2
--
-- -   oz = @minDepth@ (or (@maxDepth@ + @minDepth@) \/ 2 if
--     'Vulkan.Extensions.VK_EXT_depth_clip_control.PipelineViewportDepthClipControlCreateInfoEXT'::@negativeOneToOne@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE')
--
-- -   px = @width@
--
-- -   py = @height@
--
-- -   pz = @maxDepth@ - @minDepth@ (or (@maxDepth@ - @minDepth@) \/ 2 if
--     'Vulkan.Extensions.VK_EXT_depth_clip_control.PipelineViewportDepthClipControlCreateInfoEXT'::@negativeOneToOne@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE')
--
-- If a render pass transform is enabled, the values (px,py) and (ox, oy)
-- defining the viewport are transformed as described in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>
-- before participating in the viewport transform.
--
-- The application /can/ specify a negative term for @height@, which has
-- the effect of negating the y coordinate in clip space before performing
-- the transform. When using a negative @height@, the application /should/
-- also adjust the @y@ value to point to the lower left corner of the
-- viewport instead of the upper left corner. Using the negative @height@
-- allows the application to avoid having to negate the y component of the
-- @Position@ output from the last
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>.
--
-- The width and height of the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxViewportDimensions implementation-dependent maximum viewport dimensions>
-- /must/ be greater than or equal to the width and height of the largest
-- image which /can/ be created and attached to a framebuffer.
--
-- The floating-point viewport bounds are represented with an
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-viewportSubPixelBits implementation-dependent precision>.
--
-- == Valid Usage
--
-- -   #VUID-VkViewport-width-01770# @width@ /must/ be greater than @0.0@
--
-- -   #VUID-VkViewport-width-01771# @width@ /must/ be less than or equal
--     to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewportDimensions@[0]
--
-- -   #VUID-VkViewport-apiVersion-07917# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance1 VK_KHR_maintenance1>
--     extension is not enabled, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_AMD_negative_viewport_height VK_AMD_negative_viewport_height>
--     extension is not enabled, and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@apiVersion@
--     is less than Vulkan 1.1, @height@ /must/ be greater than @0.0@
--
-- -   #VUID-VkViewport-height-01773# The absolute value of @height@ /must/
--     be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewportDimensions@[1]
--
-- -   #VUID-VkViewport-x-01774# @x@ /must/ be greater than or equal to
--     @viewportBoundsRange@[0]
--
-- -   #VUID-VkViewport-x-01232# (@x@ + @width@) /must/ be less than or
--     equal to @viewportBoundsRange@[1]
--
-- -   #VUID-VkViewport-y-01775# @y@ /must/ be greater than or equal to
--     @viewportBoundsRange@[0]
--
-- -   #VUID-VkViewport-y-01776# @y@ /must/ be less than or equal to
--     @viewportBoundsRange@[1]
--
-- -   #VUID-VkViewport-y-01777# (@y@ + @height@) /must/ be greater than or
--     equal to @viewportBoundsRange@[0]
--
-- -   #VUID-VkViewport-y-01233# (@y@ + @height@) /must/ be less than or
--     equal to @viewportBoundsRange@[1]
--
-- -   #VUID-VkViewport-minDepth-01234# If the
--     @VK_EXT_depth_range_unrestricted@ extension is not enabled,
--     @minDepth@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- -   #VUID-VkViewport-maxDepth-01235# If the
--     @VK_EXT_depth_range_unrestricted@ extension is not enabled,
--     @maxDepth@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_NV_inherited_viewport_scissor.CommandBufferInheritanceViewportScissorInfoNV',
-- 'PipelineViewportStateCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount',
-- 'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdSetViewportWithCount'
data Viewport = Viewport
  { -- | @x@ and @y@ are the viewport’s upper left corner (x,y).
    x :: Float
  , -- No documentation found for Nested "VkViewport" "y"
    y :: Float
  , -- | @width@ and @height@ are the viewport’s width and height, respectively.
    width :: Float
  , -- No documentation found for Nested "VkViewport" "height"
    height :: Float
  , -- | @minDepth@ and @maxDepth@ are the depth range for the viewport.
    minDepth :: Float
  , -- No documentation found for Nested "VkViewport" "maxDepth"
    maxDepth :: Float
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (Viewport)
#endif
deriving instance Show Viewport

instance ToCStruct Viewport where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p Viewport{..} f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (x))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (y))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (width))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (height))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (minDepth))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (maxDepth))
    f
  cStructSize = 24
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 4 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 8 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 12 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 16 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 20 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct Viewport where
  peekCStruct p = do
    x <- peek @CFloat ((p `plusPtr` 0 :: Ptr CFloat))
    y <- peek @CFloat ((p `plusPtr` 4 :: Ptr CFloat))
    width <- peek @CFloat ((p `plusPtr` 8 :: Ptr CFloat))
    height <- peek @CFloat ((p `plusPtr` 12 :: Ptr CFloat))
    minDepth <- peek @CFloat ((p `plusPtr` 16 :: Ptr CFloat))
    maxDepth <- peek @CFloat ((p `plusPtr` 20 :: Ptr CFloat))
    pure $ Viewport
             (coerce @CFloat @Float x)
             (coerce @CFloat @Float y)
             (coerce @CFloat @Float width)
             (coerce @CFloat @Float height)
             (coerce @CFloat @Float minDepth)
             (coerce @CFloat @Float maxDepth)

instance Storable Viewport where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero Viewport where
  zero = Viewport
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkVertexInputBindingDescription - Structure specifying vertex input
-- binding description
--
-- == Valid Usage
--
-- -   #VUID-VkVertexInputBindingDescription-binding-00618# @binding@
--     /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-VkVertexInputBindingDescription-stride-00619# @stride@ /must/
--     be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindingStride@
--
-- -   #VUID-VkVertexInputBindingDescription-stride-04456# If the
--     @VK_KHR_portability_subset@ extension is enabled, @stride@ /must/ be
--     a multiple of, and at least as large as,
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetPropertiesKHR'::@minVertexInputBindingStrideAlignment@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkVertexInputBindingDescription-inputRate-parameter#
--     @inputRate@ /must/ be a valid
--     'Vulkan.Core10.Enums.VertexInputRate.VertexInputRate' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'PipelineVertexInputStateCreateInfo',
-- 'Vulkan.Core10.Enums.VertexInputRate.VertexInputRate'
data VertexInputBindingDescription = VertexInputBindingDescription
  { -- | @binding@ is the binding number that this structure describes.
    binding :: Word32
  , -- | @stride@ is the byte stride between consecutive elements within the
    -- buffer.
    stride :: Word32
  , -- | @inputRate@ is a 'Vulkan.Core10.Enums.VertexInputRate.VertexInputRate'
    -- value specifying whether vertex attribute addressing is a function of
    -- the vertex index or of the instance index.
    inputRate :: VertexInputRate
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VertexInputBindingDescription)
#endif
deriving instance Show VertexInputBindingDescription

instance ToCStruct VertexInputBindingDescription where
  withCStruct x f = allocaBytes 12 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VertexInputBindingDescription{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (binding)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (stride)
    poke ((p `plusPtr` 8 :: Ptr VertexInputRate)) (inputRate)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr VertexInputRate)) (zero)
    f

instance FromCStruct VertexInputBindingDescription where
  peekCStruct p = do
    binding <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    stride <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    inputRate <- peek @VertexInputRate ((p `plusPtr` 8 :: Ptr VertexInputRate))
    pure $ VertexInputBindingDescription
             binding stride inputRate

instance Storable VertexInputBindingDescription where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VertexInputBindingDescription where
  zero = VertexInputBindingDescription
           zero
           zero
           zero


-- | VkVertexInputAttributeDescription - Structure specifying vertex input
-- attribute description
--
-- == Valid Usage
--
-- -   #VUID-VkVertexInputAttributeDescription-location-00620# @location@
--     /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributes@
--
-- -   #VUID-VkVertexInputAttributeDescription-binding-00621# @binding@
--     /must/ be less than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-VkVertexInputAttributeDescription-offset-00622# @offset@
--     /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributeOffset@
--
-- -   #VUID-VkVertexInputAttributeDescription-format-00623# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-buffer-view-format-features format features>
--     of @format@ /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_VERTEX_BUFFER_BIT'
--
-- -   #VUID-VkVertexInputAttributeDescription-vertexAttributeAccessBeyondStride-04457#
--     If the @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@vertexAttributeAccessBeyondStride@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', the sum of @offset@ plus
--     the size of the vertex attribute data described by @format@ /must/
--     not be greater than @stride@ in the 'VertexInputBindingDescription'
--     referenced in @binding@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkVertexInputAttributeDescription-format-parameter# @format@
--     /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'PipelineVertexInputStateCreateInfo'
data VertexInputAttributeDescription = VertexInputAttributeDescription
  { -- | @location@ is the shader input location number for this attribute.
    location :: Word32
  , -- | @binding@ is the binding number which this attribute takes its data
    -- from.
    binding :: Word32
  , -- | @format@ is the size and type of the vertex attribute data.
    format :: Format
  , -- | @offset@ is a byte offset of this attribute relative to the start of an
    -- element in the vertex input binding.
    offset :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VertexInputAttributeDescription)
#endif
deriving instance Show VertexInputAttributeDescription

instance ToCStruct VertexInputAttributeDescription where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p VertexInputAttributeDescription{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (location)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (binding)
    poke ((p `plusPtr` 8 :: Ptr Format)) (format)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (offset)
    f
  cStructSize = 16
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 12 :: Ptr Word32)) (zero)
    f

instance FromCStruct VertexInputAttributeDescription where
  peekCStruct p = do
    location <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    binding <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    format <- peek @Format ((p `plusPtr` 8 :: Ptr Format))
    offset <- peek @Word32 ((p `plusPtr` 12 :: Ptr Word32))
    pure $ VertexInputAttributeDescription
             location binding format offset

instance Storable VertexInputAttributeDescription where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero VertexInputAttributeDescription where
  zero = VertexInputAttributeDescription
           zero
           zero
           zero
           zero


-- | VkPipelineVertexInputStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline vertex input state
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-vertexBindingDescriptionCount-00613#
--     @vertexBindingDescriptionCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-vertexAttributeDescriptionCount-00614#
--     @vertexAttributeDescriptionCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributes@
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-binding-00615# For every
--     @binding@ specified by each element of
--     @pVertexAttributeDescriptions@, a 'VertexInputBindingDescription'
--     /must/ exist in @pVertexBindingDescriptions@ with the same value of
--     @binding@
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-pVertexBindingDescriptions-00616#
--     All elements of @pVertexBindingDescriptions@ /must/ describe
--     distinct binding numbers
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-pVertexAttributeDescriptions-00617#
--     All elements of @pVertexAttributeDescriptions@ /must/ describe
--     distinct attribute locations
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO'
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-pNext-pNext# @pNext@
--     /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap.PipelineVertexInputDivisorStateCreateInfo'
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-pVertexBindingDescriptions-parameter#
--     If @vertexBindingDescriptionCount@ is not @0@,
--     @pVertexBindingDescriptions@ /must/ be a valid pointer to an array
--     of @vertexBindingDescriptionCount@ valid
--     'VertexInputBindingDescription' structures
--
-- -   #VUID-VkPipelineVertexInputStateCreateInfo-pVertexAttributeDescriptions-parameter#
--     If @vertexAttributeDescriptionCount@ is not @0@,
--     @pVertexAttributeDescriptions@ /must/ be a valid pointer to an array
--     of @vertexAttributeDescriptionCount@ valid
--     'VertexInputAttributeDescription' structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsShaderGroupCreateInfoNV',
-- 'Vulkan.Core10.Enums.PipelineVertexInputStateCreateFlags.PipelineVertexInputStateCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'VertexInputAttributeDescription', 'VertexInputBindingDescription'
data PipelineVertexInputStateCreateInfo (es :: [Type]) = PipelineVertexInputStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineVertexInputStateCreateFlags
  , -- | @pVertexBindingDescriptions@ is a pointer to an array of
    -- 'VertexInputBindingDescription' structures.
    vertexBindingDescriptions :: Vector VertexInputBindingDescription
  , -- | @pVertexAttributeDescriptions@ is a pointer to an array of
    -- 'VertexInputAttributeDescription' structures.
    vertexAttributeDescriptions :: Vector VertexInputAttributeDescription
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineVertexInputStateCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PipelineVertexInputStateCreateInfo es)

instance Extensible PipelineVertexInputStateCreateInfo where
  extensibleTypeName = "PipelineVertexInputStateCreateInfo"
  setNext PipelineVertexInputStateCreateInfo{..} next' = PipelineVertexInputStateCreateInfo{next = next', ..}
  getNext PipelineVertexInputStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineVertexInputStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineVertexInputDivisorStateCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss PipelineVertexInputStateCreateInfo es
         , PokeChain es ) => ToCStruct (PipelineVertexInputStateCreateInfo es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineVertexInputStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineVertexInputStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (vertexBindingDescriptions)) :: Word32))
    pPVertexBindingDescriptions' <- ContT $ allocaBytes @VertexInputBindingDescription ((Data.Vector.length (vertexBindingDescriptions)) * 12)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexBindingDescriptions' `plusPtr` (12 * (i)) :: Ptr VertexInputBindingDescription) (e)) (vertexBindingDescriptions)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDescription))) (pPVertexBindingDescriptions')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (vertexAttributeDescriptions)) :: Word32))
    pPVertexAttributeDescriptions' <- ContT $ allocaBytes @VertexInputAttributeDescription ((Data.Vector.length (vertexAttributeDescriptions)) * 16)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexAttributeDescriptions' `plusPtr` (16 * (i)) :: Ptr VertexInputAttributeDescription) (e)) (vertexAttributeDescriptions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr VertexInputAttributeDescription))) (pPVertexAttributeDescriptions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance ( Extendss PipelineVertexInputStateCreateInfo es
         , PeekChain es ) => FromCStruct (PipelineVertexInputStateCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineVertexInputStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineVertexInputStateCreateFlags))
    vertexBindingDescriptionCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pVertexBindingDescriptions <- peek @(Ptr VertexInputBindingDescription) ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDescription)))
    pVertexBindingDescriptions' <- generateM (fromIntegral vertexBindingDescriptionCount) (\i -> peekCStruct @VertexInputBindingDescription ((pVertexBindingDescriptions `advancePtrBytes` (12 * (i)) :: Ptr VertexInputBindingDescription)))
    vertexAttributeDescriptionCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pVertexAttributeDescriptions <- peek @(Ptr VertexInputAttributeDescription) ((p `plusPtr` 40 :: Ptr (Ptr VertexInputAttributeDescription)))
    pVertexAttributeDescriptions' <- generateM (fromIntegral vertexAttributeDescriptionCount) (\i -> peekCStruct @VertexInputAttributeDescription ((pVertexAttributeDescriptions `advancePtrBytes` (16 * (i)) :: Ptr VertexInputAttributeDescription)))
    pure $ PipelineVertexInputStateCreateInfo
             next
             flags
             pVertexBindingDescriptions'
             pVertexAttributeDescriptions'

instance es ~ '[] => Zero (PipelineVertexInputStateCreateInfo es) where
  zero = PipelineVertexInputStateCreateInfo
           ()
           zero
           mempty
           mempty


-- | VkPipelineInputAssemblyStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline input assembly state
--
-- = Description
--
-- Restarting the assembly of primitives discards the most recent index
-- values if those elements formed an incomplete primitive, and restarts
-- the primitive assembly using the subsequent indices, but only assembling
-- the immediately following element through the end of the originally
-- specified elements. The primitive restart index value comparison is
-- performed before adding the @vertexOffset@ value to the index value.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineInputAssemblyStateCreateInfo-topology-06252# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-primitiveTopologyListRestart primitiveTopologyListRestart>
--     feature is not enabled, and @topology@ is
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_POINT_LIST',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY',
--     or
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY',
--     @primitiveRestartEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkPipelineInputAssemblyStateCreateInfo-topology-06253# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-primitiveTopologyPatchListRestart primitiveTopologyPatchListRestart>
--     feature is not enabled, and @topology@ is
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST',
--     @primitiveRestartEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkPipelineInputAssemblyStateCreateInfo-topology-00429# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @topology@ /must/ not be any of
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY'
--     or
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY'
--
-- -   #VUID-VkPipelineInputAssemblyStateCreateInfo-topology-00430# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @topology@ /must/ not be
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST'
--
-- -   #VUID-VkPipelineInputAssemblyStateCreateInfo-triangleFans-04452# If
--     the @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@triangleFans@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', @topology@ /must/ not be
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_FAN'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineInputAssemblyStateCreateInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO'
--
-- -   #VUID-VkPipelineInputAssemblyStateCreateInfo-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkPipelineInputAssemblyStateCreateInfo-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- -   #VUID-VkPipelineInputAssemblyStateCreateInfo-topology-parameter#
--     @topology@ /must/ be a valid
--     'Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Enums.PipelineInputAssemblyStateCreateFlags.PipelineInputAssemblyStateCreateFlags',
-- 'Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineInputAssemblyStateCreateInfo = PipelineInputAssemblyStateCreateInfo
  { -- | @flags@ is reserved for future use.
    flags :: PipelineInputAssemblyStateCreateFlags
  , -- | @topology@ is a
    -- 'Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology' defining the
    -- primitive topology, as described below.
    topology :: PrimitiveTopology
  , -- | @primitiveRestartEnable@ controls whether a special vertex index value
    -- is treated as restarting the assembly of primitives. This enable only
    -- applies to indexed draws
    -- ('Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexed',
    -- 'Vulkan.Extensions.VK_EXT_multi_draw.cmdDrawMultiIndexedEXT', and
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect'), and the
    -- special index value is either 0xFFFFFFFF when the @indexType@ parameter
    -- of Vulkan 1.4 or
    -- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.cmdBindIndexBuffer2'
    -- or 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer' is equal to
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32'; 0xFF when @indexType@
    -- is equal to 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8'; or 0xFFFF
    -- when @indexType@ is equal to
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16'. Primitive restart is
    -- not allowed for “list” topologies, unless one of the features
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-primitiveTopologyPatchListRestart primitiveTopologyPatchListRestart>
    -- (for
    -- 'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST')
    -- or
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-primitiveTopologyListRestart primitiveTopologyListRestart>
    -- (for all other list topologies) is enabled.
    primitiveRestartEnable :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineInputAssemblyStateCreateInfo)
#endif
deriving instance Show PipelineInputAssemblyStateCreateInfo

instance ToCStruct PipelineInputAssemblyStateCreateInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineInputAssemblyStateCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineInputAssemblyStateCreateFlags)) (flags)
    poke ((p `plusPtr` 20 :: Ptr PrimitiveTopology)) (topology)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (primitiveRestartEnable))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr PrimitiveTopology)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PipelineInputAssemblyStateCreateInfo where
  peekCStruct p = do
    flags <- peek @PipelineInputAssemblyStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineInputAssemblyStateCreateFlags))
    topology <- peek @PrimitiveTopology ((p `plusPtr` 20 :: Ptr PrimitiveTopology))
    primitiveRestartEnable <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PipelineInputAssemblyStateCreateInfo
             flags topology (bool32ToBool primitiveRestartEnable)

instance Storable PipelineInputAssemblyStateCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineInputAssemblyStateCreateInfo where
  zero = PipelineInputAssemblyStateCreateInfo
           zero
           zero
           zero


-- | VkPipelineTessellationStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline tessellation state
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineTessellationStateCreateInfo-patchControlPoints-01214#
--     @patchControlPoints@ /must/ be greater than zero and less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTessellationPatchSize@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineTessellationStateCreateInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO'
--
-- -   #VUID-VkPipelineTessellationStateCreateInfo-pNext-pNext# @pNext@
--     /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PipelineTessellationDomainOriginStateCreateInfo'
--
-- -   #VUID-VkPipelineTessellationStateCreateInfo-sType-unique# The
--     @sType@ value of each structure in the @pNext@ chain /must/ be
--     unique
--
-- -   #VUID-VkPipelineTessellationStateCreateInfo-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsShaderGroupCreateInfoNV',
-- 'Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags.PipelineTessellationStateCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineTessellationStateCreateInfo (es :: [Type]) = PipelineTessellationStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineTessellationStateCreateFlags
  , -- | @patchControlPoints@ is the number of control points per patch.
    patchControlPoints :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineTessellationStateCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PipelineTessellationStateCreateInfo es)

instance Extensible PipelineTessellationStateCreateInfo where
  extensibleTypeName = "PipelineTessellationStateCreateInfo"
  setNext PipelineTessellationStateCreateInfo{..} next' = PipelineTessellationStateCreateInfo{next = next', ..}
  getNext PipelineTessellationStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineTessellationStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineTessellationDomainOriginStateCreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss PipelineTessellationStateCreateInfo es
         , PokeChain es ) => ToCStruct (PipelineTessellationStateCreateInfo es) where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineTessellationStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineTessellationStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (patchControlPoints)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ f

instance ( Extendss PipelineTessellationStateCreateInfo es
         , PeekChain es ) => FromCStruct (PipelineTessellationStateCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineTessellationStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineTessellationStateCreateFlags))
    patchControlPoints <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ PipelineTessellationStateCreateInfo
             next flags patchControlPoints

instance es ~ '[] => Zero (PipelineTessellationStateCreateInfo es) where
  zero = PipelineTessellationStateCreateInfo
           ()
           zero
           zero


-- | VkPipelineViewportStateCreateInfo - Structure specifying parameters of a
-- newly created pipeline viewport state
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-viewportCount-01216# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-multiViewport multiViewport>
--     feature is not enabled, @viewportCount@ /must/ not be greater than
--     @1@
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-scissorCount-01217# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-multiViewport multiViewport>
--     feature is not enabled, @scissorCount@ /must/ not be greater than
--     @1@
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-viewportCount-01218#
--     @viewportCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-scissorCount-01219#
--     @scissorCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-x-02821# The @x@ and @y@
--     members of @offset@ member of any element of @pScissors@ /must/ be
--     greater than or equal to @0@
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-offset-02822# Evaluation of
--     (@offset.x@ + @extent.width@) /must/ not cause a signed integer
--     addition overflow for any element of @pScissors@
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-offset-02823# Evaluation of
--     (@offset.y@ + @extent.height@) /must/ not cause a signed integer
--     addition overflow for any element of @pScissors@
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-scissorCount-04134# If
--     @scissorCount@ and @viewportCount@ are both not dynamic, then
--     @scissorCount@ and @viewportCount@ /must/ be identical
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-viewportCount-04135# If the
--     graphics pipeline is being created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     set then @viewportCount@ /must/ be @0@, otherwise @viewportCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-scissorCount-04136# If the
--     graphics pipeline is being created with
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     set then @scissorCount@ /must/ be @0@, otherwise @scissorCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-viewportWScalingEnable-01726#
--     If the @viewportWScalingEnable@ member of a
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--     structure included in the @pNext@ chain is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the @viewportCount@ member of
--     the
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--     structure /must/ be greater than or equal to
--     'PipelineViewportStateCreateInfo'::@viewportCount@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO'
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-pNext-pNext# Each @pNext@
--     member of any structure (including this one) in the @pNext@ chain
--     /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportCoarseSampleOrderStateCreateInfoNV',
--     'Vulkan.Extensions.VK_EXT_depth_clamp_control.PipelineViewportDepthClampControlCreateInfoEXT',
--     'Vulkan.Extensions.VK_EXT_depth_clip_control.PipelineViewportDepthClipControlCreateInfoEXT',
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV',
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV',
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV',
--     or
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPipelineViewportStateCreateInfo-flags-zerobitmask# @flags@
--     /must/ be @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Enums.PipelineViewportStateCreateFlags.PipelineViewportStateCreateFlags',
-- 'Vulkan.Core10.FundamentalTypes.Rect2D',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'Viewport'
data PipelineViewportStateCreateInfo (es :: [Type]) = PipelineViewportStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineViewportStateCreateFlags
  , -- | @viewportCount@ is the number of viewports used by the pipeline.
    viewportCount :: Word32
  , -- | @pViewports@ is a pointer to an array of 'Viewport' structures, defining
    -- the viewport transforms. If the viewport state is dynamic, this member
    -- is ignored.
    viewports :: Vector Viewport
  , -- | @scissorCount@ is the number of
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-scissor scissors>
    -- and /must/ match the number of viewports.
    scissorCount :: Word32
  , -- | @pScissors@ is a pointer to an array of
    -- 'Vulkan.Core10.FundamentalTypes.Rect2D' structures defining the
    -- rectangular bounds of the scissor for the corresponding viewport. If the
    -- scissor state is dynamic, this member is ignored.
    scissors :: Vector Rect2D
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineViewportStateCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PipelineViewportStateCreateInfo es)

instance Extensible PipelineViewportStateCreateInfo where
  extensibleTypeName = "PipelineViewportStateCreateInfo"
  setNext PipelineViewportStateCreateInfo{..} next' = PipelineViewportStateCreateInfo{next = next', ..}
  getNext PipelineViewportStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineViewportStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineViewportDepthClampControlCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineViewportDepthClipControlCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineViewportCoarseSampleOrderStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineViewportShadingRateImageStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineViewportExclusiveScissorStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineViewportSwizzleStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineViewportWScalingStateCreateInfoNV = Just f
    | otherwise = Nothing

instance ( Extendss PipelineViewportStateCreateInfo es
         , PokeChain es ) => ToCStruct (PipelineViewportStateCreateInfo es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineViewportStateCreateFlags)) (flags)
    let pViewportsLength = Data.Vector.length $ (viewports)
    viewportCount'' <- lift $ if (viewportCount) == 0
      then pure $ fromIntegral pViewportsLength
      else do
        unless (fromIntegral pViewportsLength == (viewportCount) || pViewportsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pViewports must be empty or have 'viewportCount' elements" Nothing Nothing
        pure (viewportCount)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (viewportCount'')
    pViewports'' <- if Data.Vector.null (viewports)
      then pure nullPtr
      else do
        pPViewports <- ContT $ allocaBytes @Viewport (((Data.Vector.length (viewports))) * 24)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPViewports `plusPtr` (24 * (i)) :: Ptr Viewport) (e)) ((viewports))
        pure $ pPViewports
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Viewport))) pViewports''
    let pScissorsLength = Data.Vector.length $ (scissors)
    scissorCount'' <- lift $ if (scissorCount) == 0
      then pure $ fromIntegral pScissorsLength
      else do
        unless (fromIntegral pScissorsLength == (scissorCount) || pScissorsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pScissors must be empty or have 'scissorCount' elements" Nothing Nothing
        pure (scissorCount)
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) (scissorCount'')
    pScissors'' <- if Data.Vector.null (scissors)
      then pure nullPtr
      else do
        pPScissors <- ContT $ allocaBytes @Rect2D (((Data.Vector.length (scissors))) * 16)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e)) ((scissors))
        pure $ pPScissors
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Rect2D))) pScissors''
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance ( Extendss PipelineViewportStateCreateInfo es
         , PeekChain es ) => FromCStruct (PipelineViewportStateCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineViewportStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineViewportStateCreateFlags))
    viewportCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pViewports <- peek @(Ptr Viewport) ((p `plusPtr` 24 :: Ptr (Ptr Viewport)))
    let pViewportsLength = if pViewports == nullPtr then 0 else (fromIntegral viewportCount)
    pViewports' <- generateM pViewportsLength (\i -> peekCStruct @Viewport ((pViewports `advancePtrBytes` (24 * (i)) :: Ptr Viewport)))
    scissorCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pScissors <- peek @(Ptr Rect2D) ((p `plusPtr` 40 :: Ptr (Ptr Rect2D)))
    let pScissorsLength = if pScissors == nullPtr then 0 else (fromIntegral scissorCount)
    pScissors' <- generateM pScissorsLength (\i -> peekCStruct @Rect2D ((pScissors `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))
    pure $ PipelineViewportStateCreateInfo
             next flags viewportCount pViewports' scissorCount pScissors'

instance es ~ '[] => Zero (PipelineViewportStateCreateInfo es) where
  zero = PipelineViewportStateCreateInfo
           ()
           zero
           zero
           mempty
           zero
           mempty


-- | VkPipelineRasterizationStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline rasterization state
--
-- = Description
--
-- The application /can/ also add a
-- 'Vulkan.Extensions.VK_AMD_rasterization_order.PipelineRasterizationStateRasterizationOrderAMD'
-- structure to the @pNext@ chain of a
-- 'PipelineRasterizationStateCreateInfo' structure. This structure enables
-- selecting the rasterization order to use when rendering with the
-- corresponding graphics pipeline as described in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-order Rasterization Order>.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-depthClampEnable-00782#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-depthClamp depthClamp>
--     feature is not enabled, @depthClampEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-polygonMode-01507# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-fillModeNonSolid fillModeNonSolid>
--     feature is not enabled, @polygonMode@ /must/ be
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL' or
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-polygonMode-01414# If
--     the @VK_NV_fill_rectangle@ extension is not enabled, @polygonMode@
--     /must/ not be
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-pointPolygons-04458# If
--     the @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@pointPolygons@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and
--     @rasterizerDiscardEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE',
--     @polygonMode@ /must/ not be
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_POINT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO'
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-pNext-pNext# Each
--     @pNext@ member of any structure (including this one) in the @pNext@
--     chain /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_depth_bias_control.DepthBiasRepresentationInfoEXT',
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.PipelineRasterizationConservativeStateCreateInfoEXT',
--     'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT',
--     'Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap.PipelineRasterizationLineStateCreateInfo',
--     'Vulkan.Extensions.VK_EXT_provoking_vertex.PipelineRasterizationProvokingVertexStateCreateInfoEXT',
--     'Vulkan.Extensions.VK_AMD_rasterization_order.PipelineRasterizationStateRasterizationOrderAMD',
--     or
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-sType-unique# The
--     @sType@ value of each structure in the @pNext@ chain /must/ be
--     unique
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-polygonMode-parameter#
--     @polygonMode@ /must/ be a valid
--     'Vulkan.Core10.Enums.PolygonMode.PolygonMode' value
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-cullMode-parameter#
--     @cullMode@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlagBits' values
--
-- -   #VUID-VkPipelineRasterizationStateCreateInfo-frontFace-parameter#
--     @frontFace@ /must/ be a valid
--     'Vulkan.Core10.Enums.FrontFace.FrontFace' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlags',
-- 'Vulkan.Core10.Enums.FrontFace.FrontFace', 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags.PipelineRasterizationStateCreateFlags',
-- 'Vulkan.Core10.Enums.PolygonMode.PolygonMode',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationStateCreateInfo (es :: [Type]) = PipelineRasterizationStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineRasterizationStateCreateFlags
  , -- | @depthClampEnable@ controls whether to clamp the fragment’s depth values
    -- as described in
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-depth Depth Test>.
    -- If the pipeline is not created with
    -- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT'
    -- present then enabling depth clamp will also disable clipping primitives
    -- to the z planes of the frustum as described in
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>.
    -- Otherwise depth clipping is controlled by the state set in
    -- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT'.
    depthClampEnable :: Bool
  , -- | @rasterizerDiscardEnable@ controls whether primitives are discarded
    -- immediately before the rasterization stage.
    rasterizerDiscardEnable :: Bool
  , -- | @polygonMode@ is the triangle rendering mode. See
    -- 'Vulkan.Core10.Enums.PolygonMode.PolygonMode'.
    polygonMode :: PolygonMode
  , -- | @cullMode@ is the triangle facing direction used for primitive culling.
    -- See 'Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlagBits'.
    cullMode :: CullModeFlags
  , -- | @frontFace@ is a 'Vulkan.Core10.Enums.FrontFace.FrontFace' value
    -- specifying the front-facing triangle orientation to be used for culling.
    frontFace :: FrontFace
  , -- | @depthBiasEnable@ controls whether to bias fragment depth values.
    depthBiasEnable :: Bool
  , -- | @depthBiasConstantFactor@ is a scalar factor controlling the constant
    -- depth value added to each fragment.
    depthBiasConstantFactor :: Float
  , -- | @depthBiasClamp@ is the maximum (or minimum) depth bias of a fragment.
    depthBiasClamp :: Float
  , -- | @depthBiasSlopeFactor@ is a scalar factor applied to a fragment’s slope
    -- in depth bias calculations.
    depthBiasSlopeFactor :: Float
  , -- | @lineWidth@ is the width of rasterized line segments.
    lineWidth :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineRasterizationStateCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PipelineRasterizationStateCreateInfo es)

instance Extensible PipelineRasterizationStateCreateInfo where
  extensibleTypeName = "PipelineRasterizationStateCreateInfo"
  setNext PipelineRasterizationStateCreateInfo{..} next' = PipelineRasterizationStateCreateInfo{next = next', ..}
  getNext PipelineRasterizationStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineRasterizationStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DepthBiasRepresentationInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineRasterizationProvokingVertexStateCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineRasterizationLineStateCreateInfo = Just f
    | Just Refl <- eqT @e @PipelineRasterizationDepthClipStateCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineRasterizationStateStreamCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineRasterizationConservativeStateCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineRasterizationStateRasterizationOrderAMD = Just f
    | otherwise = Nothing

instance ( Extendss PipelineRasterizationStateCreateInfo es
         , PokeChain es ) => ToCStruct (PipelineRasterizationStateCreateInfo es) where
  withCStruct x f = allocaBytes 64 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineRasterizationStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineRasterizationStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (depthClampEnable))
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (rasterizerDiscardEnable))
    lift $ poke ((p `plusPtr` 28 :: Ptr PolygonMode)) (polygonMode)
    lift $ poke ((p `plusPtr` 32 :: Ptr CullModeFlags)) (cullMode)
    lift $ poke ((p `plusPtr` 36 :: Ptr FrontFace)) (frontFace)
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (depthBiasEnable))
    lift $ poke ((p `plusPtr` 44 :: Ptr CFloat)) (CFloat (depthBiasConstantFactor))
    lift $ poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (depthBiasClamp))
    lift $ poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (depthBiasSlopeFactor))
    lift $ poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (lineWidth))
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 28 :: Ptr PolygonMode)) (zero)
    lift $ poke ((p `plusPtr` 36 :: Ptr FrontFace)) (zero)
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 44 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 48 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 52 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 56 :: Ptr CFloat)) (CFloat (zero))
    lift $ f

instance ( Extendss PipelineRasterizationStateCreateInfo es
         , PeekChain es ) => FromCStruct (PipelineRasterizationStateCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineRasterizationStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineRasterizationStateCreateFlags))
    depthClampEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    rasterizerDiscardEnable <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    polygonMode <- peek @PolygonMode ((p `plusPtr` 28 :: Ptr PolygonMode))
    cullMode <- peek @CullModeFlags ((p `plusPtr` 32 :: Ptr CullModeFlags))
    frontFace <- peek @FrontFace ((p `plusPtr` 36 :: Ptr FrontFace))
    depthBiasEnable <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    depthBiasConstantFactor <- peek @CFloat ((p `plusPtr` 44 :: Ptr CFloat))
    depthBiasClamp <- peek @CFloat ((p `plusPtr` 48 :: Ptr CFloat))
    depthBiasSlopeFactor <- peek @CFloat ((p `plusPtr` 52 :: Ptr CFloat))
    lineWidth <- peek @CFloat ((p `plusPtr` 56 :: Ptr CFloat))
    pure $ PipelineRasterizationStateCreateInfo
             next
             flags
             (bool32ToBool depthClampEnable)
             (bool32ToBool rasterizerDiscardEnable)
             polygonMode
             cullMode
             frontFace
             (bool32ToBool depthBiasEnable)
             (coerce @CFloat @Float depthBiasConstantFactor)
             (coerce @CFloat @Float depthBiasClamp)
             (coerce @CFloat @Float depthBiasSlopeFactor)
             (coerce @CFloat @Float lineWidth)

instance es ~ '[] => Zero (PipelineRasterizationStateCreateInfo es) where
  zero = PipelineRasterizationStateCreateInfo
           ()
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


-- | VkPipelineMultisampleStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline multisample state
--
-- = Description
--
-- Each bit in the sample mask is associated with a unique
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-multisampling-coverage-mask sample index>
-- as defined for the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-multisampling-coverage-mask coverage mask>.
-- Each bit b for mask word w in the sample mask corresponds to sample
-- index i, where i = 32 × w + b. @pSampleMask@ has a length equal to ⌈
-- @rasterizationSamples@ \/ 32 ⌉ words.
--
-- If @pSampleMask@ is @NULL@, it is treated as if the mask has all bits
-- set to @1@.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineMultisampleStateCreateInfo-sampleShadingEnable-00784#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-sampleRateShading sampleRateShading>
--     feature is not enabled, @sampleShadingEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkPipelineMultisampleStateCreateInfo-alphaToOneEnable-00785#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-alphaToOne alphaToOne>
--     feature is not enabled, @alphaToOneEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkPipelineMultisampleStateCreateInfo-minSampleShading-00786#
--     @minSampleShading@ /must/ be in the range [0,1]
--
-- -   #VUID-VkPipelineMultisampleStateCreateInfo-rasterizationSamples-01415#
--     If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-coverageReductionMode coverageReductionMode>
--     feature is not enabled, or the @pNext@ chain does not contain
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateInfoNV',
--     or
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateInfoNV'::@coverageReductionMode@
--     is not set to
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.COVERAGE_REDUCTION_MODE_TRUNCATE_NV',
--     and the subpass has any color attachments, and
--     @rasterizationSamples@ is greater than the number of color samples,
--     then
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-sampleshading sample shading>
--     /must/ not be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineMultisampleStateCreateInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO'
--
-- -   #VUID-VkPipelineMultisampleStateCreateInfo-pNext-pNext# Each @pNext@
--     member of any structure (including this one) in the @pNext@ chain
--     /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV',
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateInfoNV',
--     'Vulkan.Extensions.VK_NV_fragment_coverage_to_color.PipelineCoverageToColorStateCreateInfoNV',
--     or
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--
-- -   #VUID-VkPipelineMultisampleStateCreateInfo-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPipelineMultisampleStateCreateInfo-flags-zerobitmask#
--     @flags@ /must/ be @0@
--
-- -   #VUID-VkPipelineMultisampleStateCreateInfo-rasterizationSamples-parameter#
--     @rasterizationSamples@ /must/ be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags.PipelineMultisampleStateCreateFlags',
-- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Vulkan.Core10.FundamentalTypes.SampleMask',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineMultisampleStateCreateInfo (es :: [Type]) = PipelineMultisampleStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineMultisampleStateCreateFlags
  , -- | @rasterizationSamples@ is a
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
    -- specifying the number of samples used in rasterization. This value is
    -- ignored for the purposes of setting the number of samples used in
    -- rasterization if the pipeline is created with the
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
    -- dynamic state set, but if
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT' dynamic
    -- state is not set, it is still used to define the size of the
    -- @pSampleMask@ array as described below.
    rasterizationSamples :: SampleCountFlagBits
  , -- | @sampleShadingEnable@ /can/ be used to enable
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-sampleshading Sample Shading>.
    sampleShadingEnable :: Bool
  , -- | @minSampleShading@ specifies a minimum fraction of sample shading if
    -- @sampleShadingEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE'.
    minSampleShading :: Float
  , -- | @pSampleMask@ is a pointer to an array of
    -- 'Vulkan.Core10.FundamentalTypes.SampleMask' values used in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-samplemask sample mask test>.
    sampleMask :: Vector SampleMask
  , -- | @alphaToCoverageEnable@ controls whether a temporary coverage value is
    -- generated based on the alpha component of the fragment’s first color
    -- output as specified in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-covg Multisample Coverage>
    -- section.
    alphaToCoverageEnable :: Bool
  , -- | @alphaToOneEnable@ controls whether the alpha component of the
    -- fragment’s first color output is replaced with one as described in
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-covg Multisample Coverage>.
    alphaToOneEnable :: Bool
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineMultisampleStateCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PipelineMultisampleStateCreateInfo es)

instance Extensible PipelineMultisampleStateCreateInfo where
  extensibleTypeName = "PipelineMultisampleStateCreateInfo"
  setNext PipelineMultisampleStateCreateInfo{..} next' = PipelineMultisampleStateCreateInfo{next = next', ..}
  getNext PipelineMultisampleStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineMultisampleStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineCoverageReductionStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineCoverageModulationStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineSampleLocationsStateCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineCoverageToColorStateCreateInfoNV = Just f
    | otherwise = Nothing

instance ( Extendss PipelineMultisampleStateCreateInfo es
         , PokeChain es ) => ToCStruct (PipelineMultisampleStateCreateInfo es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineMultisampleStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineMultisampleStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr SampleCountFlagBits)) (rasterizationSamples)
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (sampleShadingEnable))
    lift $ poke ((p `plusPtr` 28 :: Ptr CFloat)) (CFloat (minSampleShading))
    pSampleMask'' <- case Data.Vector.length (sampleMask) of
      0      -> pure nullPtr
      vecLen -> do
        let requiredLen = case (rasterizationSamples) of
              SampleCountFlagBits n -> (n + 31) `quot` 32
        lift $ unless (requiredLen == fromIntegral vecLen) $
          throwIO $ IOError Nothing InvalidArgument "" "sampleMask must be either empty or contain enough bits to cover all the sample specified by 'rasterizationSamples'" Nothing Nothing
        do
          pPSampleMask' <- ContT $ allocaBytes @SampleMask ((Data.Vector.length ((sampleMask))) * 4)
          lift $ Data.Vector.imapM_ (\i e -> poke (pPSampleMask' `plusPtr` (4 * (i)) :: Ptr SampleMask) (e)) ((sampleMask))
          pure $ pPSampleMask'
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr SampleMask))) pSampleMask''
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (alphaToCoverageEnable))
    lift $ poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (alphaToOneEnable))
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr SampleCountFlagBits)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 28 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 40 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 44 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ f

instance ( Extendss PipelineMultisampleStateCreateInfo es
         , PeekChain es ) => FromCStruct (PipelineMultisampleStateCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineMultisampleStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineMultisampleStateCreateFlags))
    rasterizationSamples <- peek @SampleCountFlagBits ((p `plusPtr` 20 :: Ptr SampleCountFlagBits))
    sampleShadingEnable <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    minSampleShading <- peek @CFloat ((p `plusPtr` 28 :: Ptr CFloat))
    pSampleMask <- peek @(Ptr SampleMask) ((p `plusPtr` 32 :: Ptr (Ptr SampleMask)))
    pSampleMask' <- if pSampleMask == nullPtr
      then pure mempty
      else generateM (case rasterizationSamples of
        SampleCountFlagBits n -> (fromIntegral n + 31) `quot` 32) (\i -> peek @SampleMask (((pSampleMask) `advancePtrBytes` (4 * (i)) :: Ptr SampleMask)))
    alphaToCoverageEnable <- peek @Bool32 ((p `plusPtr` 40 :: Ptr Bool32))
    alphaToOneEnable <- peek @Bool32 ((p `plusPtr` 44 :: Ptr Bool32))
    pure $ PipelineMultisampleStateCreateInfo
             next
             flags
             rasterizationSamples
             (bool32ToBool sampleShadingEnable)
             (coerce @CFloat @Float minSampleShading)
             pSampleMask'
             (bool32ToBool alphaToCoverageEnable)
             (bool32ToBool alphaToOneEnable)

instance es ~ '[] => Zero (PipelineMultisampleStateCreateInfo es) where
  zero = PipelineMultisampleStateCreateInfo
           ()
           zero
           zero
           zero
           zero
           mempty
           zero
           zero


-- | VkPipelineColorBlendAttachmentState - Structure specifying a pipeline
-- color blend attachment state
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-srcColorBlendFactor-00608#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dualSrcBlend dualSrcBlend>
--     feature is not enabled, @srcColorBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-dstColorBlendFactor-00609#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dualSrcBlend dualSrcBlend>
--     feature is not enabled, @dstColorBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-srcAlphaBlendFactor-00610#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dualSrcBlend dualSrcBlend>
--     feature is not enabled, @srcAlphaBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-dstAlphaBlendFactor-00611#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dualSrcBlend dualSrcBlend>
--     feature is not enabled, @dstAlphaBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-colorBlendOp-01406# If
--     either of @colorBlendOp@ or @alphaBlendOp@ is an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @colorBlendOp@ /must/ equal @alphaBlendOp@
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-advancedBlendIndependentBlend-01407#
--     If
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendIndependentBlend@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE' and @colorBlendOp@ is an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @colorBlendOp@ /must/ be the same for all attachments
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-advancedBlendIndependentBlend-01408#
--     If
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendIndependentBlend@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE' and @alphaBlendOp@ is an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @alphaBlendOp@ /must/ be the same for all attachments
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-advancedBlendAllOperations-01409#
--     If
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendAllOperations@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then @colorBlendOp@
--     /must/ not be 'Vulkan.Core10.Enums.BlendOp.BLEND_OP_ZERO_EXT',
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
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_RGB_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARDODGE_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARBURN_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_VIVIDLIGHT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARLIGHT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_PINLIGHT_EXT',
--     'Vulkan.Core10.Enums.BlendOp.BLEND_OP_HARDMIX_EXT',
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
-- -   #VUID-VkPipelineColorBlendAttachmentState-colorBlendOp-01410# If
--     @colorBlendOp@ or @alphaBlendOp@ is an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @colorAttachmentCount@ of the subpass this pipeline is compiled
--     against /must/ be less than or equal to
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendMaxColorAttachments@
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-constantAlphaColorBlendFactors-04454#
--     If the @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@constantAlphaColorBlendFactors@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', @srcColorBlendFactor@
--     /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_CONSTANT_ALPHA' or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA'
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-constantAlphaColorBlendFactors-04455#
--     If the @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@constantAlphaColorBlendFactors@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', @dstColorBlendFactor@
--     /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_CONSTANT_ALPHA' or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_CONSTANT_ALPHA'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-srcColorBlendFactor-parameter#
--     @srcColorBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-dstColorBlendFactor-parameter#
--     @dstColorBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-colorBlendOp-parameter#
--     @colorBlendOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendOp.BlendOp' value
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-srcAlphaBlendFactor-parameter#
--     @srcAlphaBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-dstAlphaBlendFactor-parameter#
--     @dstAlphaBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-alphaBlendOp-parameter#
--     @alphaBlendOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendOp.BlendOp' value
--
-- -   #VUID-VkPipelineColorBlendAttachmentState-colorWriteMask-parameter#
--     @colorWriteMask@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.BlendFactor.BlendFactor',
-- 'Vulkan.Core10.Enums.BlendOp.BlendOp',
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlags',
-- 'PipelineColorBlendStateCreateInfo'
data PipelineColorBlendAttachmentState = PipelineColorBlendAttachmentState
  { -- | @blendEnable@ controls whether blending is enabled for the corresponding
    -- color attachment. If blending is not enabled, the source fragment’s
    -- color for that attachment is passed through unmodified.
    blendEnable :: Bool
  , -- | @srcColorBlendFactor@ selects which blend factor is used to determine
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
  , -- | @alphaBlendOp@ selects which blend operation is used to calculate the
    -- alpha values to write to the color attachment.
    alphaBlendOp :: BlendOp
  , -- | @colorWriteMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits'
    -- specifying which of the R, G, B, and\/or A components are enabled for
    -- writing, as described for the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#framebuffer-color-write-mask Color Write Mask>.
    colorWriteMask :: ColorComponentFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineColorBlendAttachmentState)
#endif
deriving instance Show PipelineColorBlendAttachmentState

instance ToCStruct PipelineColorBlendAttachmentState where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineColorBlendAttachmentState{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Bool32)) (boolToBool32 (blendEnable))
    poke ((p `plusPtr` 4 :: Ptr BlendFactor)) (srcColorBlendFactor)
    poke ((p `plusPtr` 8 :: Ptr BlendFactor)) (dstColorBlendFactor)
    poke ((p `plusPtr` 12 :: Ptr BlendOp)) (colorBlendOp)
    poke ((p `plusPtr` 16 :: Ptr BlendFactor)) (srcAlphaBlendFactor)
    poke ((p `plusPtr` 20 :: Ptr BlendFactor)) (dstAlphaBlendFactor)
    poke ((p `plusPtr` 24 :: Ptr BlendOp)) (alphaBlendOp)
    poke ((p `plusPtr` 28 :: Ptr ColorComponentFlags)) (colorWriteMask)
    f
  cStructSize = 32
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 4 :: Ptr BlendFactor)) (zero)
    poke ((p `plusPtr` 8 :: Ptr BlendFactor)) (zero)
    poke ((p `plusPtr` 12 :: Ptr BlendOp)) (zero)
    poke ((p `plusPtr` 16 :: Ptr BlendFactor)) (zero)
    poke ((p `plusPtr` 20 :: Ptr BlendFactor)) (zero)
    poke ((p `plusPtr` 24 :: Ptr BlendOp)) (zero)
    f

instance FromCStruct PipelineColorBlendAttachmentState where
  peekCStruct p = do
    blendEnable <- peek @Bool32 ((p `plusPtr` 0 :: Ptr Bool32))
    srcColorBlendFactor <- peek @BlendFactor ((p `plusPtr` 4 :: Ptr BlendFactor))
    dstColorBlendFactor <- peek @BlendFactor ((p `plusPtr` 8 :: Ptr BlendFactor))
    colorBlendOp <- peek @BlendOp ((p `plusPtr` 12 :: Ptr BlendOp))
    srcAlphaBlendFactor <- peek @BlendFactor ((p `plusPtr` 16 :: Ptr BlendFactor))
    dstAlphaBlendFactor <- peek @BlendFactor ((p `plusPtr` 20 :: Ptr BlendFactor))
    alphaBlendOp <- peek @BlendOp ((p `plusPtr` 24 :: Ptr BlendOp))
    colorWriteMask <- peek @ColorComponentFlags ((p `plusPtr` 28 :: Ptr ColorComponentFlags))
    pure $ PipelineColorBlendAttachmentState
             (bool32ToBool blendEnable)
             srcColorBlendFactor
             dstColorBlendFactor
             colorBlendOp
             srcAlphaBlendFactor
             dstAlphaBlendFactor
             alphaBlendOp
             colorWriteMask

instance Storable PipelineColorBlendAttachmentState where
  sizeOf ~_ = 32
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineColorBlendAttachmentState where
  zero = PipelineColorBlendAttachmentState
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPipelineColorBlendStateCreateInfo - Structure specifying parameters of
-- a newly created pipeline color blend state
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineColorBlendStateCreateInfo-pAttachments-00605# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-independentBlend independentBlend>
--     feature is not enabled, all elements of @pAttachments@ /must/ be
--     identical
--
-- -   #VUID-VkPipelineColorBlendStateCreateInfo-logicOpEnable-00606# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-logicOp logicOp>
--     feature is not enabled, @logicOpEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkPipelineColorBlendStateCreateInfo-logicOpEnable-00607# If
--     @logicOpEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE', @logicOp@
--     /must/ be a valid 'Vulkan.Core10.Enums.LogicOp.LogicOp' value
--
-- -   #VUID-VkPipelineColorBlendStateCreateInfo-rasterizationOrderColorAttachmentAccess-06465#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-rasterizationOrderColorAttachmentAccess rasterizationOrderColorAttachmentAccess>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT'
--
-- -   #VUID-VkPipelineColorBlendStateCreateInfo-pAttachments-07353# If
--     @attachmentCount@ is not @0@ , and any of
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT',
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
--     are not set, @pAttachments@ /must/ be a valid pointer to an array of
--     @attachmentCount@ valid 'PipelineColorBlendAttachmentState'
--     structures
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineColorBlendStateCreateInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO'
--
-- -   #VUID-VkPipelineColorBlendStateCreateInfo-pNext-pNext# Each @pNext@
--     member of any structure (including this one) in the @pNext@ chain
--     /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PipelineColorBlendAdvancedStateCreateInfoEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_color_write_enable.PipelineColorWriteCreateInfoEXT'
--
-- -   #VUID-VkPipelineColorBlendStateCreateInfo-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPipelineColorBlendStateCreateInfo-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlagBits'
--     values
--
-- -   #VUID-VkPipelineColorBlendStateCreateInfo-pAttachments-parameter# If
--     @attachmentCount@ is not @0@, and @pAttachments@ is not @NULL@,
--     @pAttachments@ /must/ be a valid pointer to an array of
--     @attachmentCount@ valid 'PipelineColorBlendAttachmentState'
--     structures
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Enums.LogicOp.LogicOp',
-- 'PipelineColorBlendAttachmentState',
-- 'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineColorBlendStateCreateInfo (es :: [Type]) = PipelineColorBlendStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PipelineColorBlendStateCreateFlagBits'
    -- specifying additional color blending information.
    flags :: PipelineColorBlendStateCreateFlags
  , -- | @logicOpEnable@ controls whether to apply
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#framebuffer-logicop Logical Operations>.
    logicOpEnable :: Bool
  , -- | @logicOp@ selects which logical operation to apply.
    logicOp :: LogicOp
  , -- | @attachmentCount@ is the number of 'PipelineColorBlendAttachmentState'
    -- elements in @pAttachments@. It is ignored if the pipeline is created
    -- with
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT',
    -- and
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
    -- dynamic states set, and either
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
    -- set or the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-advancedBlendCoherentOperations advancedBlendCoherentOperations>
    -- feature is not enabled.
    attachmentCount :: Word32
  , -- | @pAttachments@ is a pointer to an array of
    -- 'PipelineColorBlendAttachmentState' structures defining blend state for
    -- each color attachment. It is ignored if the pipeline is created with
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT',
    -- and
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
    -- dynamic states set, and either
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
    -- set or the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-advancedBlendCoherentOperations advancedBlendCoherentOperations>
    -- feature is not enabled.
    attachments :: Vector PipelineColorBlendAttachmentState
  , -- | @blendConstants@ is a pointer to an array of four values used as the R,
    -- G, B, and A components of the blend constant that are used in blending,
    -- depending on the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#framebuffer-blendfactors blend factor>.
    blendConstants :: (Float, Float, Float, Float)
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineColorBlendStateCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PipelineColorBlendStateCreateInfo es)

instance Extensible PipelineColorBlendStateCreateInfo where
  extensibleTypeName = "PipelineColorBlendStateCreateInfo"
  setNext PipelineColorBlendStateCreateInfo{..} next' = PipelineColorBlendStateCreateInfo{next = next', ..}
  getNext PipelineColorBlendStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineColorBlendStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineColorWriteCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineColorBlendAdvancedStateCreateInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss PipelineColorBlendStateCreateInfo es
         , PokeChain es ) => ToCStruct (PipelineColorBlendStateCreateInfo es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineColorBlendStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineColorBlendStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (logicOpEnable))
    lift $ poke ((p `plusPtr` 24 :: Ptr LogicOp)) (logicOp)
    let pAttachmentsLength = Data.Vector.length $ (attachments)
    attachmentCount'' <- lift $ if (attachmentCount) == 0
      then pure $ fromIntegral pAttachmentsLength
      else do
        unless (fromIntegral pAttachmentsLength == (attachmentCount) || pAttachmentsLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pAttachments must be empty or have 'attachmentCount' elements" Nothing Nothing
        pure (attachmentCount)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) (attachmentCount'')
    pAttachments'' <- if Data.Vector.null (attachments)
      then pure nullPtr
      else do
        pPAttachments <- ContT $ allocaBytes @PipelineColorBlendAttachmentState (((Data.Vector.length (attachments))) * 32)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPAttachments `plusPtr` (32 * (i)) :: Ptr PipelineColorBlendAttachmentState) (e)) ((attachments))
        pure $ pPAttachments
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineColorBlendAttachmentState))) pAttachments''
    let pBlendConstants' = lowerArrayPtr ((p `plusPtr` 40 :: Ptr (FixedArray 4 CFloat)))
    lift $ case (blendConstants) of
      (e0, e1, e2, e3) -> do
        poke (pBlendConstants' :: Ptr CFloat) (CFloat (e0))
        poke (pBlendConstants' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pBlendConstants' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pBlendConstants' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 24 :: Ptr LogicOp)) (zero)
    let pBlendConstants' = lowerArrayPtr ((p `plusPtr` 40 :: Ptr (FixedArray 4 CFloat)))
    lift $ case ((zero, zero, zero, zero)) of
      (e0, e1, e2, e3) -> do
        poke (pBlendConstants' :: Ptr CFloat) (CFloat (e0))
        poke (pBlendConstants' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pBlendConstants' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pBlendConstants' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    lift $ f

instance ( Extendss PipelineColorBlendStateCreateInfo es
         , PeekChain es ) => FromCStruct (PipelineColorBlendStateCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineColorBlendStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineColorBlendStateCreateFlags))
    logicOpEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    logicOp <- peek @LogicOp ((p `plusPtr` 24 :: Ptr LogicOp))
    attachmentCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pAttachments <- peek @(Ptr PipelineColorBlendAttachmentState) ((p `plusPtr` 32 :: Ptr (Ptr PipelineColorBlendAttachmentState)))
    let pAttachmentsLength = if pAttachments == nullPtr then 0 else (fromIntegral attachmentCount)
    pAttachments' <- generateM pAttachmentsLength (\i -> peekCStruct @PipelineColorBlendAttachmentState ((pAttachments `advancePtrBytes` (32 * (i)) :: Ptr PipelineColorBlendAttachmentState)))
    let pblendConstants = lowerArrayPtr @CFloat ((p `plusPtr` 40 :: Ptr (FixedArray 4 CFloat)))
    blendConstants0 <- peek @CFloat ((pblendConstants `advancePtrBytes` 0 :: Ptr CFloat))
    blendConstants1 <- peek @CFloat ((pblendConstants `advancePtrBytes` 4 :: Ptr CFloat))
    blendConstants2 <- peek @CFloat ((pblendConstants `advancePtrBytes` 8 :: Ptr CFloat))
    blendConstants3 <- peek @CFloat ((pblendConstants `advancePtrBytes` 12 :: Ptr CFloat))
    pure $ PipelineColorBlendStateCreateInfo
             next
             flags
             (bool32ToBool logicOpEnable)
             logicOp
             attachmentCount
             pAttachments'
             (( (coerce @CFloat @Float blendConstants0)
              , (coerce @CFloat @Float blendConstants1)
              , (coerce @CFloat @Float blendConstants2)
              , (coerce @CFloat @Float blendConstants3) ))

instance es ~ '[] => Zero (PipelineColorBlendStateCreateInfo es) where
  zero = PipelineColorBlendStateCreateInfo
           ()
           zero
           zero
           zero
           zero
           mempty
           (zero, zero, zero, zero)


-- | VkPipelineDynamicStateCreateInfo - Structure specifying parameters of a
-- newly created pipeline dynamic state
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineDynamicStateCreateInfo-pDynamicStates-01442# Each
--     element of @pDynamicStates@ /must/ be unique
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineDynamicStateCreateInfo-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO'
--
-- -   #VUID-VkPipelineDynamicStateCreateInfo-pNext-pNext# @pNext@ /must/
--     be @NULL@
--
-- -   #VUID-VkPipelineDynamicStateCreateInfo-flags-zerobitmask# @flags@
--     /must/ be @0@
--
-- -   #VUID-VkPipelineDynamicStateCreateInfo-pDynamicStates-parameter# If
--     @dynamicStateCount@ is not @0@, @pDynamicStates@ /must/ be a valid
--     pointer to an array of @dynamicStateCount@ valid
--     'Vulkan.Core10.Enums.DynamicState.DynamicState' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.DynamicState.DynamicState',
-- 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags.PipelineDynamicStateCreateFlags',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineDynamicStateCreateInfo = PipelineDynamicStateCreateInfo
  { -- | @flags@ is reserved for future use.
    flags :: PipelineDynamicStateCreateFlags
  , -- | @pDynamicStates@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.DynamicState.DynamicState' values specifying which
    -- pieces of pipeline state will use the values from dynamic state commands
    -- rather than from pipeline state creation information.
    dynamicStates :: Vector DynamicState
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineDynamicStateCreateInfo)
#endif
deriving instance Show PipelineDynamicStateCreateInfo

instance ToCStruct PipelineDynamicStateCreateInfo where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineDynamicStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineDynamicStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (dynamicStates)) :: Word32))
    pPDynamicStates' <- ContT $ allocaBytes @DynamicState ((Data.Vector.length (dynamicStates)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDynamicStates' `plusPtr` (4 * (i)) :: Ptr DynamicState) (e)) (dynamicStates)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DynamicState))) (pPDynamicStates')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PipelineDynamicStateCreateInfo where
  peekCStruct p = do
    flags <- peek @PipelineDynamicStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineDynamicStateCreateFlags))
    dynamicStateCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pDynamicStates <- peek @(Ptr DynamicState) ((p `plusPtr` 24 :: Ptr (Ptr DynamicState)))
    pDynamicStates' <- generateM (fromIntegral dynamicStateCount) (\i -> peek @DynamicState ((pDynamicStates `advancePtrBytes` (4 * (i)) :: Ptr DynamicState)))
    pure $ PipelineDynamicStateCreateInfo
             flags pDynamicStates'

instance Zero PipelineDynamicStateCreateInfo where
  zero = PipelineDynamicStateCreateInfo
           zero
           mempty


-- | VkStencilOpState - Structure specifying stencil operation state
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.CompareOp.CompareOp',
-- 'PipelineDepthStencilStateCreateInfo',
-- 'Vulkan.Core10.Enums.StencilOp.StencilOp'
data StencilOpState = StencilOpState
  { -- | @failOp@ is a 'Vulkan.Core10.Enums.StencilOp.StencilOp' value specifying
    -- the action performed on samples that fail the stencil test.
    --
    -- #VUID-VkStencilOpState-failOp-parameter# @failOp@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
    failOp :: StencilOp
  , -- | @passOp@ is a 'Vulkan.Core10.Enums.StencilOp.StencilOp' value specifying
    -- the action performed on samples that pass both the depth and stencil
    -- tests.
    --
    -- #VUID-VkStencilOpState-passOp-parameter# @passOp@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
    passOp :: StencilOp
  , -- | @depthFailOp@ is a 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
    -- specifying the action performed on samples that pass the stencil test
    -- and fail the depth test.
    --
    -- #VUID-VkStencilOpState-depthFailOp-parameter# @depthFailOp@ /must/ be a
    -- valid 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
    depthFailOp :: StencilOp
  , -- | @compareOp@ is a 'Vulkan.Core10.Enums.CompareOp.CompareOp' value
    -- specifying the comparison operator used in the stencil test.
    --
    -- #VUID-VkStencilOpState-compareOp-parameter# @compareOp@ /must/ be a
    -- valid 'Vulkan.Core10.Enums.CompareOp.CompareOp' value
    compareOp :: CompareOp
  , -- | @compareMask@ selects the bits of the unsigned integer stencil values
    -- participating in the stencil test.
    compareMask :: Word32
  , -- | @writeMask@ selects the bits of the unsigned integer stencil values
    -- updated by the stencil test in the stencil framebuffer attachment.
    writeMask :: Word32
  , -- | @reference@ is an integer stencil reference value that is used in the
    -- unsigned stencil comparison.
    reference :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (StencilOpState)
#endif
deriving instance Show StencilOpState

instance ToCStruct StencilOpState where
  withCStruct x f = allocaBytes 28 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p StencilOpState{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StencilOp)) (failOp)
    poke ((p `plusPtr` 4 :: Ptr StencilOp)) (passOp)
    poke ((p `plusPtr` 8 :: Ptr StencilOp)) (depthFailOp)
    poke ((p `plusPtr` 12 :: Ptr CompareOp)) (compareOp)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (compareMask)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (writeMask)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (reference)
    f
  cStructSize = 28
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StencilOp)) (zero)
    poke ((p `plusPtr` 4 :: Ptr StencilOp)) (zero)
    poke ((p `plusPtr` 8 :: Ptr StencilOp)) (zero)
    poke ((p `plusPtr` 12 :: Ptr CompareOp)) (zero)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct StencilOpState where
  peekCStruct p = do
    failOp <- peek @StencilOp ((p `plusPtr` 0 :: Ptr StencilOp))
    passOp <- peek @StencilOp ((p `plusPtr` 4 :: Ptr StencilOp))
    depthFailOp <- peek @StencilOp ((p `plusPtr` 8 :: Ptr StencilOp))
    compareOp <- peek @CompareOp ((p `plusPtr` 12 :: Ptr CompareOp))
    compareMask <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    writeMask <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    reference <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ StencilOpState
             failOp passOp depthFailOp compareOp compareMask writeMask reference

instance Storable StencilOpState where
  sizeOf ~_ = 28
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero StencilOpState where
  zero = StencilOpState
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPipelineDepthStencilStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline depth stencil state
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineDepthStencilStateCreateInfo-depthBoundsTestEnable-00598#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-depthBounds depthBounds>
--     feature is not enabled, @depthBoundsTestEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkPipelineDepthStencilStateCreateInfo-separateStencilMaskRef-04453#
--     If the @VK_KHR_portability_subset@ extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@separateStencilMaskRef@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and the value of
--     'PipelineDepthStencilStateCreateInfo'::@stencilTestEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', and the value of
--     'PipelineRasterizationStateCreateInfo'::@cullMode@ is
--     'Vulkan.Core10.Enums.CullModeFlagBits.CULL_MODE_NONE', the value of
--     @reference@ in each of the 'StencilOpState' structs in @front@ and
--     @back@ /must/ be the same
--
-- -   #VUID-VkPipelineDepthStencilStateCreateInfo-rasterizationOrderDepthAttachmentAccess-06463#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-rasterizationOrderDepthAttachmentAccess rasterizationOrderDepthAttachmentAccess>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT'
--
-- -   #VUID-VkPipelineDepthStencilStateCreateInfo-rasterizationOrderStencilAttachmentAccess-06464#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-rasterizationOrderStencilAttachmentAccess rasterizationOrderStencilAttachmentAccess>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineDepthStencilStateCreateInfo-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO'
--
-- -   #VUID-VkPipelineDepthStencilStateCreateInfo-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkPipelineDepthStencilStateCreateInfo-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlagBits'
--     values
--
-- -   #VUID-VkPipelineDepthStencilStateCreateInfo-depthCompareOp-parameter#
--     @depthCompareOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   #VUID-VkPipelineDepthStencilStateCreateInfo-front-parameter# @front@
--     /must/ be a valid 'StencilOpState' structure
--
-- -   #VUID-VkPipelineDepthStencilStateCreateInfo-back-parameter# @back@
--     /must/ be a valid 'StencilOpState' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.CompareOp.CompareOp', 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlags',
-- 'StencilOpState', 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineDepthStencilStateCreateInfo = PipelineDepthStencilStateCreateInfo
  { -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PipelineDepthStencilStateCreateFlagBits'
    -- specifying additional depth\/stencil state information.
    flags :: PipelineDepthStencilStateCreateFlags
  , -- | @depthTestEnable@ controls whether
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-depth depth testing>
    -- is enabled.
    depthTestEnable :: Bool
  , -- | @depthWriteEnable@ controls whether
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-depth-write depth writes>
    -- are enabled when @depthTestEnable@ is
    -- 'Vulkan.Core10.FundamentalTypes.TRUE'. Depth writes are always disabled
    -- when @depthTestEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE'.
    depthWriteEnable :: Bool
  , -- | @depthCompareOp@ is a 'Vulkan.Core10.Enums.CompareOp.CompareOp' value
    -- specifying the comparison operator to use in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-depth-comparison Depth Comparison>
    -- step of the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-depth depth test>.
    depthCompareOp :: CompareOp
  , -- | @depthBoundsTestEnable@ controls whether
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-dbt depth bounds testing>
    -- is enabled.
    depthBoundsTestEnable :: Bool
  , -- | @stencilTestEnable@ controls whether
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-stencil stencil testing>
    -- is enabled.
    stencilTestEnable :: Bool
  , -- | @front@ and @back@ are 'StencilOpState' values controlling the
    -- corresponding parameters of the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-stencil stencil test>.
    front :: StencilOpState
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "back"
    back :: StencilOpState
  , -- | @minDepthBounds@ is the minimum depth bound used in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-dbt depth bounds test>.
    minDepthBounds :: Float
  , -- | @maxDepthBounds@ is the maximum depth bound used in the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragops-dbt depth bounds test>.
    maxDepthBounds :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineDepthStencilStateCreateInfo)
#endif
deriving instance Show PipelineDepthStencilStateCreateInfo

instance ToCStruct PipelineDepthStencilStateCreateInfo where
  withCStruct x f = allocaBytes 104 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineDepthStencilStateCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineDepthStencilStateCreateFlags)) (flags)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (depthTestEnable))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (depthWriteEnable))
    poke ((p `plusPtr` 28 :: Ptr CompareOp)) (depthCompareOp)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (depthBoundsTestEnable))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (stencilTestEnable))
    poke ((p `plusPtr` 40 :: Ptr StencilOpState)) (front)
    poke ((p `plusPtr` 68 :: Ptr StencilOpState)) (back)
    poke ((p `plusPtr` 96 :: Ptr CFloat)) (CFloat (minDepthBounds))
    poke ((p `plusPtr` 100 :: Ptr CFloat)) (CFloat (maxDepthBounds))
    f
  cStructSize = 104
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr CompareOp)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 40 :: Ptr StencilOpState)) (zero)
    poke ((p `plusPtr` 68 :: Ptr StencilOpState)) (zero)
    poke ((p `plusPtr` 96 :: Ptr CFloat)) (CFloat (zero))
    poke ((p `plusPtr` 100 :: Ptr CFloat)) (CFloat (zero))
    f

instance FromCStruct PipelineDepthStencilStateCreateInfo where
  peekCStruct p = do
    flags <- peek @PipelineDepthStencilStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineDepthStencilStateCreateFlags))
    depthTestEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    depthWriteEnable <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    depthCompareOp <- peek @CompareOp ((p `plusPtr` 28 :: Ptr CompareOp))
    depthBoundsTestEnable <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    stencilTestEnable <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    front <- peekCStruct @StencilOpState ((p `plusPtr` 40 :: Ptr StencilOpState))
    back <- peekCStruct @StencilOpState ((p `plusPtr` 68 :: Ptr StencilOpState))
    minDepthBounds <- peek @CFloat ((p `plusPtr` 96 :: Ptr CFloat))
    maxDepthBounds <- peek @CFloat ((p `plusPtr` 100 :: Ptr CFloat))
    pure $ PipelineDepthStencilStateCreateInfo
             flags
             (bool32ToBool depthTestEnable)
             (bool32ToBool depthWriteEnable)
             depthCompareOp
             (bool32ToBool depthBoundsTestEnable)
             (bool32ToBool stencilTestEnable)
             front
             back
             (coerce @CFloat @Float minDepthBounds)
             (coerce @CFloat @Float maxDepthBounds)

instance Storable PipelineDepthStencilStateCreateInfo where
  sizeOf ~_ = 104
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineDepthStencilStateCreateInfo where
  zero = PipelineDepthStencilStateCreateInfo
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


-- | VkGraphicsPipelineCreateInfo - Structure specifying parameters of a
-- newly created graphics pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- If any shader stage fails to compile, the compile log will be reported
-- back to the application, and
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV' will be generated.
--
-- With @VK_EXT_extended_dynamic_state3@, it is possible that many of the
-- 'GraphicsPipelineCreateInfo' members above /can/ be @NULL@ because all
-- their state is dynamic and therefore ignored. This is optional so the
-- application /can/ still use a valid pointer if it needs to set the
-- @pNext@ or @flags@ fields to specify state for other extensions.
--
-- The state required for a graphics pipeline is divided into
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>,
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
-- and
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output state>.
--
-- __Vertex Input State__
--
-- Vertex input state is defined by:
--
-- -   'PipelineVertexInputStateCreateInfo'
--
-- -   'PipelineInputAssemblyStateCreateInfo'
--
-- If this pipeline specifies
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization state>
-- either directly or by including it as a pipeline library and its
-- @pStages@ includes a vertex shader, this state /must/ be specified to
-- create a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete graphics pipeline>.
--
-- If a pipeline includes
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_VERTEX_INPUT_INTERFACE_BIT_EXT'
-- in
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
-- either explicitly or as a default, and either the conditions requiring
-- this state for a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete graphics pipeline>
-- are met or this pipeline does not specify
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization state>
-- in any way, that pipeline /must/ specify this state directly.
--
-- __Pre-Rasterization Shader State__
--
-- Pre-rasterization shader state is defined by:
--
-- -   'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo'
--     entries for:
--
--     -   Vertex shaders
--
--     -   Tessellation control shaders
--
--     -   Tessellation evaluation shaders
--
--     -   Geometry shaders
--
--     -   Task shaders
--
--     -   Mesh shaders
--
-- -   Within the 'Vulkan.Core10.Handles.PipelineLayout', all descriptor
--     sets with pre-rasterization shader bindings if
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--     was specified.
--
--     -   If
--         'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--         was not specified, the full pipeline layout /must/ be specified.
--
-- -   'PipelineViewportStateCreateInfo'
--
-- -   'PipelineRasterizationStateCreateInfo'
--
-- -   'PipelineTessellationStateCreateInfo'
--
-- -   'Vulkan.Core10.Handles.RenderPass' and @subpass@ parameter
--
-- -   The @viewMask@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'
--     (formats are ignored)
--
-- -   'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'
--
-- -   'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'
--
-- -   Inclusion\/omission of the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE'
--     flag
--
-- This state /must/ be specified to create a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete graphics pipeline>.
--
-- If either the @pNext@ chain includes a
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'
-- structure with
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
-- included in @flags@, or it is not specified and would default to include
-- that value, this state /must/ be specified in the pipeline.
--
-- __Fragment Shader State__
--
-- Fragment shader state is defined by:
--
-- -   A 'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo'
--     entry for the fragment shader
--
-- -   Within the 'Vulkan.Core10.Handles.PipelineLayout', all descriptor
--     sets with fragment shader bindings if
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--     was specified.
--
--     -   If
--         'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--         was not specified, the full pipeline layout /must/ be specified.
--
-- -   'PipelineMultisampleStateCreateInfo' if
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-sampleshading sample shading>
--     is enabled or @renderpass@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   'PipelineDepthStencilStateCreateInfo'
--
-- -   'Vulkan.Core10.Handles.RenderPass' and @subpass@ parameter
--
-- -   The @viewMask@ parameter of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'
--     (formats are ignored)
--
-- -   'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'
--
-- -   'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'
--
-- -   'Vulkan.Extensions.VK_NV_representative_fragment_test.PipelineRepresentativeFragmentTestStateCreateInfoNV'
--
-- -   Inclusion\/omission of the
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--     flag
--
-- -   Inclusion\/omission of the
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.PIPELINE_RASTERIZATION_STATE_CREATE_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--     flag
--
-- -   'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'
--
-- -   Inclusion\/omission of the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_PER_LAYER_FRAGMENT_DENSITY_BIT_VALVE'
--     flag
--
-- -   The @customResolve@ parameter of
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'.
--     Formats are ignored, and not including the structure behaves
--     identically to setting @customResolve@ to
--     'Vulkan.Core10.FundamentalTypes.FALSE', unlike in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>.
--
-- If a pipeline specifies
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization state>
-- either directly or by including it as a pipeline library and
-- @rasterizerDiscardEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE' or
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
-- is used, this state /must/ be specified to create a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete graphics pipeline>.
--
-- If a pipeline includes
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT'
-- in
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
-- either explicitly or as a default, and either the conditions requiring
-- this state for a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete graphics pipeline>
-- are met or this pipeline does not specify
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization state>
-- in any way, that pipeline /must/ specify this state directly.
--
-- __Fragment Output State__
--
-- Fragment output state is defined by:
--
-- -   'PipelineColorBlendStateCreateInfo'
--
-- -   'Vulkan.Core10.Handles.RenderPass' and @subpass@ parameter
--
-- -   'PipelineMultisampleStateCreateInfo'
--
-- -   'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'
--
-- -   'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--
-- -   'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--
-- -   'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'
--
-- -   'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--
-- -   Inclusion\/omission of the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--     and
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--     flags
--
-- -   Inclusion\/omission of the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_ENABLE_LEGACY_DITHERING_BIT_EXT'
--     flag
--
-- -   'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingAttachmentLocationInfo'
--
-- If a pipeline specifies
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization state>
-- either directly or by including it as a pipeline library and
-- @rasterizerDiscardEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE' or
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
-- is used, this state /must/ be specified to create a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete graphics pipeline>.
--
-- If a pipeline includes
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
-- in
-- 'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
-- either explicitly or as a default, and either the conditions requiring
-- this state for a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete graphics pipeline>
-- are met or this pipeline does not specify
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization state>
-- in any way, that pipeline /must/ specify this state directly.
--
-- __Dynamic State__
--
-- Dynamic state values set via @pDynamicState@ /must/ be ignored if the
-- state they correspond to is not otherwise statically set by one of the
-- state subsets used to create the pipeline. Additionally, setting dynamic
-- state values /must/ not modify whether state in a linked library is
-- static or dynamic; this is set and unchangeable when the library is
-- created. For example, if a pipeline only included
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
-- then any dynamic state value corresponding to depth or stencil testing
-- has no effect. Any linked library that has dynamic state enabled that
-- same dynamic state /must/ also be enabled in all the other linked
-- libraries to which that dynamic state applies.
--
-- __Complete Graphics Pipelines__
--
-- A complete graphics pipeline always includes
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
-- with other subsets included depending on that state as specified in the
-- above sections.
--
-- __Graphics Pipeline Library Layouts__
--
-- If different subsets are linked together with pipeline layouts created
-- with
-- 'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
-- the final effective pipeline layout is effectively the union of the
-- linked pipeline layouts. When binding descriptor sets for this pipeline,
-- the pipeline layout used /must/ be compatible with this union. This
-- pipeline layout /can/ be overridden when linking with
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT'
-- by providing a 'Vulkan.Core10.Handles.PipelineLayout' that is
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorsets-compatibility compatible>
-- with this union other than
-- 'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
-- or when linking without
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT'
-- by providing a 'Vulkan.Core10.Handles.PipelineLayout' that is fully
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorsets-compatibility compatible>
-- with this union.
--
-- If the @pNext@ chain includes a
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.PipelineCreateFlags2CreateInfo'
-- structure,
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.PipelineCreateFlags2CreateInfo'::@flags@
-- from that structure is used instead of @flags@ from this structure.
--
-- == Valid Usage
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-None-09497# If the @pNext@ chain
--     does not include a
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.PipelineCreateFlags2CreateInfo'
--     structure, @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-07984# If @flags@ contains
--     the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be
--     a valid graphics 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-07985# If @flags@ contains
--     the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be a valid index into the calling command’s @pCreateInfos@ parameter
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-07986# If @flags@ contains
--     the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, @basePipelineIndex@ /must/ be -1 or @basePipelineHandle@
--     /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-layout-07987# If a push constant
--     block is declared in a shader and @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', a push constant range in
--     @layout@ /must/ match the shader stage
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-layout-10069# If a push constant
--     block is declared in a shader and @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the block must be
--     contained inside the push constant range in @layout@ that matches
--     the stage
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-layout-07988# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variable>
--     is declared in a shader and @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the corresponding
--     descriptor set in @layout@ /must/ match the shader stage
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-layout-07990# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variable>
--     is declared in a shader, @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the descriptor type is
--     not
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT',
--     the corresponding descriptor set in @layout@ /must/ match the
--     descriptor type
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-layout-07991# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variable>
--     is declared in a shader as an array and @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the corresponding
--     descriptor binding used to create @layout@ /must/ have a
--     @descriptorCount@ that is greater than or equal to the length of the
--     array
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-None-10391# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variables>
--     is declared in a shader as an array of descriptors, then the
--     descriptor type of that variable /must/ not be
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-11798# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shader64BitIndexing shader64BitIndexing>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_64_BIT_INDEXING_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pipelineCreationCacheControl-02878#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
--     nor
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pipelineProtectedAccess-07368# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineProtectedAccess pipelineProtectedAccess>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT'
--     nor
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-07369# @flags@ /must/ not
--     include both
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT'
--     and
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-11311# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     includes
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     @layout@ /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-11312# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     includes
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     all shader variables in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources shader resource interface>
--     with a 'Vulkan.Core10.Handles.DescriptorSet' and @Binding@
--     decoration /must/ have a mapping declared in
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.ShaderDescriptorSetAndBindingMappingInfoEXT'::pMappings
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-stage-02096# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     the @stage@ member of one element of @pStages@ /must/ be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT' or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-02095# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     the geometric shader stages provided in @pStages@ /must/ be either
--     from the mesh shading pipeline (@stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT')
--     or from the primitive shading pipeline (@stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT')
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-09631# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @pStages@ contains both
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     and
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT',
--     then the mesh shader’s entry point /must/ not declare a variable
--     with a @DrawIndex@ @BuiltIn@ decoration
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-TaskNV-07063# The shader stages
--     for
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--     /must/ use either the @TaskNV@ and @MeshNV@ @Execution@ @Model@ or
--     the @TaskEXT@ and @MeshEXT@ @Execution@ @Model@, but /must/ not use
--     both
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-00729# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @pStages@ includes a tessellation control shader stage, it
--     /must/ include a tessellation evaluation shader stage
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-00730# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @pStages@ includes a tessellation evaluation shader stage, it
--     /must/ include a tessellation control shader stage
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-09022# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @pStages@ includes a tessellation control shader stage, and the
--     @VK_EXT_extended_dynamic_state3@ extension is not enabled or the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--     dynamic state is not set, @pTessellationState@ /must/ be a valid
--     pointer to a valid 'PipelineTessellationStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pTessellationState-09023# If
--     @pTessellationState@ is not @NULL@ it /must/ be a pointer to a valid
--     'PipelineTessellationStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-00732# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @pStages@ includes tessellation shader stages, the shader code
--     of at least one stage /must/ contain an @OpExecutionMode@
--     instruction specifying the type of subdivision in the pipeline
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-00733# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @pStages@ includes tessellation shader stages, and the shader
--     code of both stages contain an @OpExecutionMode@ instruction
--     specifying the type of subdivision in the pipeline, they /must/ both
--     specify the same subdivision mode
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-00734# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @pStages@ includes tessellation shader stages, the shader code
--     of at least one stage /must/ contain an @OpExecutionMode@
--     instruction specifying the output patch size in the pipeline
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-00735# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @pStages@ includes tessellation shader stages, and the shader
--     code of both contain an @OpExecutionMode@ instruction specifying the
--     out patch size in the pipeline, they /must/ both specify the same
--     patch size
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-08888# If the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>
--     and @pStages@ includes tessellation shader stages, and either
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY'
--     dynamic state is not enabled or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-dynamicPrimitiveTopologyUnrestricted dynamicPrimitiveTopologyUnrestricted>
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', the @topology@ member of
--     @pInputAssembly@ /must/ be
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-topology-08889# If the pipeline
--     is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>
--     and the @topology@ member of @pInputAssembly@ is
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST',
--     and either
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY'
--     dynamic state is not enabled or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-dynamicPrimitiveTopologyUnrestricted dynamicPrimitiveTopologyUnrestricted>
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then @pStages@ /must/
--     include tessellation shader stages
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-TessellationEvaluation-07723# If
--     the pipeline is being created with a @TessellationEvaluation@
--     @Execution@ @Model@, no @Geometry@ @Execution@ @Model@, uses the
--     @PointMode@ @Execution@ @Mode@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-shaderTessellationAndGeometryPointSize shaderTessellationAndGeometryPointSize>
--     feature is enabled, a @PointSize@ decorated variable /must/ be
--     written to if the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance5 maintenance5>
--     feature is not enabled
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-topology-08773# If the pipeline
--     is being created with a @Vertex@ @Execution@ @Model@ and no
--     @TessellationEvaluation@ or @Geometry@ @Execution@ @Model@, and the
--     @topology@ member of @pInputAssembly@ is
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_POINT_LIST',
--     and either
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY'
--     dynamic state is not enabled or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-dynamicPrimitiveTopologyUnrestricted dynamicPrimitiveTopologyUnrestricted>
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', a @PointSize@ decorated
--     variable /must/ be written to if the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance5 maintenance5>
--     feature is not enabled
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-TessellationEvaluation-07724# If
--     the pipeline is being created with a @TessellationEvaluation@
--     @Execution@ @Model@, no @Geometry@ @Execution@ @Model@, uses the
--     @PointMode@ @Execution@ @Mode@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-shaderTessellationAndGeometryPointSize shaderTessellationAndGeometryPointSize>
--     feature is not enabled, a @PointSize@ decorated variable /must/ not
--     be written to
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-shaderTessellationAndGeometryPointSize-08776#
--     If the pipeline is being created with a @Geometry@ @Execution@
--     @Model@, uses the @OutputPoints@ @Execution@ @Mode@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-shaderTessellationAndGeometryPointSize shaderTessellationAndGeometryPointSize>
--     feature is enabled, a @PointSize@ decorated variable /must/ be
--     written to for every vertex emitted if the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance5 maintenance5>
--     feature is not enabled
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-Geometry-07726# If the pipeline
--     is being created with a @Geometry@ @Execution@ @Model@, uses the
--     @OutputPoints@ @Execution@ @Mode@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-shaderTessellationAndGeometryPointSize shaderTessellationAndGeometryPointSize>
--     feature is not enabled, a @PointSize@ decorated variable /must/ not
--     be written to
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-00738# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @pStages@ includes a geometry shader stage, and does not include
--     any tessellation shader stages, its shader code /must/ contain an
--     @OpExecutionMode@ instruction specifying an input primitive type
--     that is
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-geometry-execution compatible>
--     with the primitive topology specified in @pInputAssembly@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-00739# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @pStages@ includes a geometry shader stage, and also includes
--     tessellation shader stages, its shader code /must/ contain an
--     @OpExecutionMode@ instruction specifying an input primitive type
--     that is
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-geometry-execution compatible>
--     with the primitive topology that is output by the tessellation
--     stages
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-00740# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     it includes both a fragment shader and a geometry shader, and the
--     fragment shader code reads from an input variable that is decorated
--     with @PrimitiveId@, then the geometry shader code /must/ write to a
--     matching output variable, decorated with @PrimitiveId@, in all
--     execution paths
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-PrimitiveId-06264# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     it includes a mesh shader and the fragment shader code reads from an
--     input variable that is decorated with @PrimitiveId@, then the mesh
--     shader code /must/ write to a matching output variable, decorated
--     with @PrimitiveId@, in all execution paths
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06038# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     the fragment shader /must/ not read from any input attachment that
--     is defined as 'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' in
--     @subpass@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-00742# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and multiple pre-rasterization shader stages are included in
--     @pStages@, the shader code for the entry points identified by those
--     @pStages@ and the rest of the state identified by this structure
--     /must/ adhere to the pipeline linking rules described in the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-None-04889# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     the fragment shader and last
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>
--     and any relevant state /must/ adhere to the pipeline linking rules
--     described in the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06041# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     then for each color attachment in the subpass, if the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#potential-format-features potential format features>
--     of the format of the corresponding attachment description do not
--     contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the @blendEnable@ member of the corresponding element of the
--     @pAttachments@ member of @pColorBlendState@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-07609# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     the @pColorBlendState@ pointer is not @NULL@, the @attachmentCount@
--     member of @pColorBlendState@ is not ignored, and the subpass uses
--     color attachments, the @attachmentCount@ member of
--     @pColorBlendState@ /must/ be equal to the @colorAttachmentCount@
--     used to create @subpass@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04130# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and @pViewportState->pViewports@ is not dynamic, then
--     @pViewportState->pViewports@ /must/ be a valid pointer to an array
--     of @pViewportState->viewportCount@ valid 'Viewport' structures
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04131# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and @pViewportState->pScissors@ is not dynamic, then
--     @pViewportState->pScissors@ /must/ be a valid pointer to an array of
--     @pViewportState->scissorCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-00749# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-wideLines wideLines>
--     feature is not enabled, and no element of the @pDynamicStates@
--     member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_WIDTH', the
--     @lineWidth@ member of @pRasterizationState@ /must/ be @1.0@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-rasterizerDiscardEnable-09024# If
--     the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     dynamic state is enabled or the @rasterizerDiscardEnable@ member of
--     @pRasterizationState@ is 'Vulkan.Core10.FundamentalTypes.FALSE', and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-pViewportState-null related dynamic state is not set>,
--     @pViewportState@ /must/ be a valid pointer to a valid
--     'PipelineViewportStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pViewportState-09025# If
--     @pViewportState@ is not @NULL@ it /must/ be a valid pointer to a
--     valid 'PipelineViewportStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pMultisampleState-09026# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and the @VK_EXT_extended_dynamic_state3@ extension is not enabled or
--     any of the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT', or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
--     dynamic states is not set, or the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-alphaToOne alphaToOne>
--     feature is enabled and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
--     is not set, @pMultisampleState@ /must/ be a valid pointer to a valid
--     'PipelineMultisampleStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pMultisampleState-09027# If
--     @pMultisampleState@ is not @NULL@ it /must/ be a valid pointer to a
--     valid 'PipelineMultisampleStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-alphaToCoverageEnable-08891# If
--     the pipeline is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     the 'PipelineMultisampleStateCreateInfo'::@alphaToCoverageEnable@ is
--     not ignored and is 'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-fragmentoutput Fragment Output Interface>
--     /must/ contain a variable for the alpha @Component@ word in
--     @Location@ 0 at @Index@ 0
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09028# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and @subpass@ uses a depth\/stencil attachment, and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-pDepthStencilState-null related dynamic state is not set>,
--     @pDepthStencilState@ /must/ be a valid pointer to a valid
--     'PipelineDepthStencilStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDepthStencilState-09029# If
--     @pDepthStencilState@ is not @NULL@ it /must/ be a valid pointer to a
--     valid 'PipelineDepthStencilStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09030# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and @subpass@ uses color attachments, and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-pColorBlendState-null related dynamic state is not set>,
--     @pColorBlendState@ /must/ be a valid pointer to a valid
--     'PipelineColorBlendStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-00754# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-depthBiasClamp depthBiasClamp>
--     feature is not enabled, no element of the @pDynamicStates@ member of
--     @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS', and the
--     @depthBiasEnable@ member of @pRasterizationState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the @depthBiasClamp@ member
--     of @pRasterizationState@ /must/ be @0.0@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-02510# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     the @VK_EXT_depth_range_unrestricted@ extension is not enabled and
--     no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS', and
--     the @depthBoundsTestEnable@ member of @pDepthStencilState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the @minDepthBounds@ and
--     @maxDepthBounds@ members of @pDepthStencilState@ /must/ be between
--     @0.0@ and @1.0@, inclusive
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-10913# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS', and
--     @pDynamicStates@ includes
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE'
--     or the @depthBoundsTestEnable@ member of @pDepthStencilState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', @minDepthBounds@ /must/ be
--     less than or equal to @maxDepthBounds@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07610# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and @rasterizationSamples@ and @sampleLocationsInfo@ are not
--     dynamic, and
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     included in the @pNext@ chain of @pMultisampleState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     @sampleLocationsInfo.sampleLocationGridSize.width@ /must/ evenly
--     divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@maxSampleLocationGridSize.width@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07611# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and @rasterizationSamples@ and @sampleLocationsInfo@ are not
--     dynamic, and
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     the included in the @pNext@ chain of @pMultisampleState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE' or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     is used, @sampleLocationsInfo.sampleLocationGridSize.height@ /must/
--     evenly divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@maxSampleLocationGridSize.height@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07612# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and @rasterizationSamples@ and @sampleLocationsInfo@ are not
--     dynamic, and
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     included in the @pNext@ chain of @pMultisampleState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE' or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--     is used, @sampleLocationsInfo.sampleLocationsPerPixel@ /must/ equal
--     @rasterizationSamples@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-sampleLocationsEnable-01524# If
--     the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and the @sampleLocationsEnable@ member of a
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pMultisampleState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the fragment shader code
--     /must/ not statically use the extended instruction
--     @InterpolateAtSample@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-multisampledRenderToSingleSampled-06853#
--     If the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and none of the @VK_AMD_mixed_attachment_samples@ extension, the
--     @VK_NV_framebuffer_mixed_samples@ extension, or the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-multisampledRenderToSingleSampled multisampledRenderToSingleSampled>
--     feature are enabled, @rasterizationSamples@ is not dynamic, and if
--     @subpass@ uses color and\/or depth\/stencil attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be the
--     same as the sample count for those subpass attachments
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-subpass-01505# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and the @VK_AMD_mixed_attachment_samples@ extension is enabled,
--     @rasterizationSamples@ is not dynamic, and if @subpass@ uses color
--     and\/or depth\/stencil attachments, then the @rasterizationSamples@
--     member of @pMultisampleState@ /must/ equal the maximum of the sample
--     counts of those subpass attachments
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06854# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     @VK_EXT_multisampled_render_to_single_sampled@ extension is enabled,
--     @rasterizationSamples@ is not dynamic, and @subpass@ has a
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'
--     structure included in the
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2'::@pNext@
--     chain with @multisampledRenderToSingleSampledEnable@ equal to
--     'Vulkan.Core10.FundamentalTypes.TRUE', then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be equal
--     to
--     'Vulkan.Extensions.VK_EXT_multisampled_render_to_single_sampled.MultisampledRenderToSingleSampledInfoEXT'::@rasterizationSamples@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-subpass-01411# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     the @VK_NV_framebuffer_mixed_samples@ extension is enabled,
--     @rasterizationSamples@ is not dynamic, and if @subpass@ has a
--     depth\/stencil attachment and depth test, stencil test, or depth
--     bounds test are enabled, then the @rasterizationSamples@ member of
--     @pMultisampleState@ /must/ be the same as the sample count of the
--     depth\/stencil attachment
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-subpass-01412# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     the @VK_NV_framebuffer_mixed_samples@ extension is enabled,
--     @rasterizationSamples@ is not dynamic, and if @subpass@ has any
--     color attachments, then the @rasterizationSamples@ member of
--     @pMultisampleState@ /must/ be greater than or equal to the sample
--     count for those subpass attachments
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-coverageReductionMode-02722# If
--     the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-coverageReductionMode coverageReductionMode>
--     feature is enabled, and @rasterizationSamples@ is not dynamic, the
--     coverage reduction mode specified by
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateInfoNV'::@coverageReductionMode@,
--     the @rasterizationSamples@ member of @pMultisampleState@ and the
--     sample counts for the color and depth\/stencil attachments (if the
--     subpass has them) /must/ be a valid combination returned by
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-subpass-00758# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @rasterizationSamples@ is not dynamic, and @subpass@ does not use
--     any color and\/or depth\/stencil attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ follow
--     the rules for a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-noattachments zero-attachment subpass>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06046# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @subpass@ /must/ be
--     a valid subpass within @renderPass@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06047# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     @subpass@ viewMask is not @0@, and @multiviewTessellationShader@ is
--     not enabled, then @pStages@ /must/ not include tessellation shaders
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06048# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     @subpass@ viewMask is not @0@, and @multiviewGeometryShader@ is not
--     enabled, then @pStages@ /must/ not include a geometry shader
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06050# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and @subpass@ viewMask is not @0@, then all of the shaders in the
--     pipeline /must/ not include variables decorated with the @Layer@
--     built-in decoration in their interfaces
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-07064# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     @subpass@ viewMask is not @0@, and @multiviewMeshShader@ is not
--     enabled, then @pStages@ /must/ not include a mesh shader
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-12325# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     @pStages@ include a mesh shader, and @subpass@ viewMask is not @0@,
--     then the index of the most significant bit in @viewMask@ /must/ be
--     less than
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxMeshMultiviewViewCount maxMeshMultiviewViewCount>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-00764# @flags@ /must/ not
--     contain the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DISPATCH_BASE_BIT'
--     flag
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-01565# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and an input attachment was referenced by an @aspectMask@ at
--     @renderPass@ creation time, the fragment shader /must/ only read
--     from the aspects that were specified for that input attachment
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-layout-01688# The number of
--     resources in @layout@ accessible to each shader stage that is used
--     by the pipeline /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-01715# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV',
--     and the @viewportWScalingEnable@ member of a
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--     structure, included in the @pNext@ chain of @pViewportState@, is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the @pViewportWScalings@
--     member of the
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--     /must/ be a pointer to an array of
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     valid
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.ViewportWScalingNV'
--     structures
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04056# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV',
--     and if @pViewportState->pNext@ chain includes a
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     structure, and if its @exclusiveScissorCount@ member is not @0@,
--     then its @pExclusiveScissors@ member /must/ be a valid pointer to an
--     array of @exclusiveScissorCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07854# If
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_ENABLE_NV'
--     is included in the @pDynamicStates@ array then the implementation
--     /must/ support at least @specVersion@ @2@ of the
--     @VK_NV_scissor_exclusive@ extension
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04057# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV',
--     and if @pViewportState->pNext@ chain includes a
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'
--     structure, then its @pShadingRatePalettes@ member /must/ be a valid
--     pointer to an array of @viewportCount@ valid
--     'Vulkan.Extensions.VK_NV_shading_rate_image.ShadingRatePaletteNV'
--     structures
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04058# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT',
--     and if @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'
--     structure, and if its @discardRectangleCount@ member is not @0@,
--     then its @pDiscardRectangles@ member /must/ be a valid pointer to an
--     array of @discardRectangleCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07855# If
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_ENABLE_EXT'
--     is included in the @pDynamicStates@ array then the implementation
--     /must/ support at least @specVersion@ @2@ of the
--     @VK_EXT_discard_rectangles@ extension
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07856# If
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_MODE_EXT'
--     is included in the @pDynamicStates@ array then the implementation
--     /must/ support at least @specVersion@ @2@ of the
--     @VK_EXT_discard_rectangles@ extension
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-02097# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>,
--     and @pVertexInputState@ is not dynamic, then @pVertexInputState@
--     /must/ be a valid pointer to a valid
--     'PipelineVertexInputStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-Input-07904# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-vertexAttributeRobustness vertexAttributeRobustness>
--     feature is not enabled, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance9 maintenance9>
--     feature is not enabled, and the pipeline is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>
--     and @pVertexInputState@ is not dynamic, then all variables with the
--     @Input@ storage class decorated with @Location@ in the @Vertex@
--     @Execution@ @Model@ @OpEntryPoint@ /must/ contain a location in
--     'VertexInputAttributeDescription'::@location@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-Input-08733# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>
--     and @pVertexInputState@ is not dynamic, then the numeric type
--     associated with all @Input@ variables of the corresponding
--     @Location@ in the @Vertex@ @Execution@ @Model@ @OpEntryPoint@ /must/
--     be the same as 'VertexInputAttributeDescription'::@format@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pVertexInputState-08929# If the
--     pipeline is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>
--     and @pVertexInputState@ is not dynamic, and
--     'VertexInputAttributeDescription'::@format@ has a 64-bit component,
--     then the scalar width associated with all @Input@ variables of the
--     corresponding @Location@ in the @Vertex@ @Execution@ @Model@
--     @OpEntryPoint@ /must/ be 64-bit
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pVertexInputState-08930# If the
--     pipeline is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>
--     and @pVertexInputState@ is not dynamic, and the scalar width
--     associated with a @Location@ decorated @Input@ variable in the
--     @Vertex@ @Execution@ @Model@ @OpEntryPoint@ is 64-bit, then the
--     corresponding 'VertexInputAttributeDescription'::@format@ /must/
--     have a 64-bit component
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pVertexInputState-09198# If the
--     pipeline is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>
--     and @pVertexInputState@ is not dynamic, and
--     'VertexInputAttributeDescription'::@format@ has a 64-bit component,
--     then all @Input@ variables at the corresponding @Location@ in the
--     @Vertex@ @Execution@ @Model@ @OpEntryPoint@ /must/ not use
--     components that are not present in the format
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-dynamicPrimitiveTopologyUnrestricted-09031#
--     If the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>,
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-pInputAssemblyState-null related dynamic state is not set>,
--     @pInputAssemblyState@ /must/ be a valid pointer to a valid
--     'PipelineInputAssemblyStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pInputAssemblyState-09032# If
--     @pInputAssemblyState@ is not @NULL@ it /must/ be a valid pointer to
--     a valid 'PipelineInputAssemblyStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-02317# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     the @Xfb@ execution mode /can/ be specified by no more than one
--     shader stage in @pStages@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-02318# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and any shader stage in @pStages@ specifies @Xfb@ execution mode it
--     /must/ be the last
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-rasterizationStream-02319# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and a
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--     value other than zero is specified, all variables in the output
--     interface of the entry point being compiled decorated with
--     @Position@, @PointSize@, @ClipDistance@, or @CullDistance@ /must/ be
--     decorated with identical @Stream@ values that match the
--     @rasterizationStream@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-rasterizationStream-02320# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--     is zero, or not specified, all variables in the output interface of
--     the entry point being compiled decorated with @Position@,
--     @PointSize@, @ClipDistance@, or @CullDistance@ /must/ be decorated
--     with a @Stream@ value of zero, or /must/ not specify the @Stream@
--     decoration
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-geometryStreams-02321# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and the last
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>
--     is a geometry shader, and that geometry shader uses the
--     @GeometryStreams@ capability, then
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackFeaturesEXT'::@geometryStreams@
--     feature /must/ be enabled
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-None-02322# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and there are any mesh shader stages in the pipeline there /must/
--     not be any shader stage in the pipeline with a @Xfb@ execution mode
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-lineRasterizationMode-02766# If
--     the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and at least one of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and @pMultisampleState@ is not @NULL@, the @lineRasterizationMode@
--     member of a
--     'Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap.PipelineRasterizationLineStateCreateInfo'
--     structure included in the @pNext@ chain of @pRasterizationState@ is
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_BRESENHAM'
--     or
--     'Vulkan.Core14.Enums.LineRasterizationMode.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH',
--     then the @alphaToCoverageEnable@, @alphaToOneEnable@, and
--     @sampleShadingEnable@ members of @pMultisampleState@ /must/ all be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-stippledLineEnable-02767# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     the @stippledLineEnable@ member of
--     'Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap.PipelineRasterizationLineStateCreateInfo'
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', and no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE', then
--     the @lineStippleFactor@ member of
--     'Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap.PipelineRasterizationLineStateCreateInfo'
--     /must/ be in the range [1,256]
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-shaderMeshEnqueue-10187# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-shaderMeshEnqueue shaderMeshEnqueue>
--     feature is not enabled, shaders specified by @pStages@ /must/ not
--     declare the @ShaderEnqueueAMDX@ capability
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-10188# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR',
--     shaders specified by @pStages@ /must/ not declare the
--     @ShaderEnqueueAMDX@ capability
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-10189# If any shader
--     stages in @pStages@ declare the @ShaderEnqueueAMDX@ capability,
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_EXECUTION_GRAPH_BIT_AMDX'
--     and
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_LIBRARY_BIT_KHR'
--     /must/ be included in @flags@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-10190# If
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_EXECUTION_GRAPH_BIT_AMDX'
--     is included in @flags@, and the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     there /must/ not be a task or vertex shader specified in @pStages@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-10191# If
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_EXECUTION_GRAPH_BIT_AMDX'
--     is included in @flags@, all elements of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     /must/ have been created with
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_EXECUTION_GRAPH_BIT_AMDX'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-03372# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-03373# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-03374# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-03375# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-03376# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-03377# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-03577# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-04947# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-03378# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature is not enabled, and the minimum value of
--     'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@
--     used to create the 'Vulkan.Core10.Handles.Instance' and @apiVersion@
--     supported by the physical device is less than Version 1.3 there
--     /must/ be no element of the @pDynamicStates@ member of
--     @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CULL_MODE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRONT_FACE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE',
--     or 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-03379# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     is included in the @pDynamicStates@ array then @viewportCount@
--     /must/ be zero
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-03380# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     is included in the @pDynamicStates@ array then @scissorCount@ /must/
--     be zero
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04132# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     is included in the @pDynamicStates@ array then
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT' /must/ not
--     be present
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04133# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
--     is included in the @pDynamicStates@ array then
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR' /must/ not
--     be present
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07065# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and includes a mesh shader, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY',
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04868# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState2 extendedDynamicState2>
--     feature is not enabled, and the minimum value of
--     'Vulkan.Core10.DeviceInitialization.ApplicationInfo'::@apiVersion@
--     used to create the 'Vulkan.Core10.Handles.Instance' and @apiVersion@
--     supported by the physical device is less than Version 1.3 there
--     /must/ be no element of the @pDynamicStates@ member of
--     @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE',
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04869# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState2LogicOp extendedDynamicState2LogicOp>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04870# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState2PatchControlPoints extendedDynamicState2PatchControlPoints>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07066# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and includes a mesh shader, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE',
--     or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-02877# If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCommandsNV ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-02966# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV',
--     then all stages /must/ not specify @Xfb@ execution mode
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-libraryCount-06648# If the
--     pipeline is not created with a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete set of state>,
--     or
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@libraryCount@
--     is not @0@,
--     'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsPipelineShaderGroupsCreateInfoNV'::@groupCount@
--     and
--     'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsPipelineShaderGroupsCreateInfoNV'::@pipelineCount@
--     /must/ be @0@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-libraryCount-06649# If the
--     pipeline is created with a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete set of state>,
--     and
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@libraryCount@
--     is @0@, and the @pNext@ chain includes an instance of
--     'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsPipelineShaderGroupsCreateInfoNV',
--     'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsPipelineShaderGroupsCreateInfoNV'::@groupCount@
--     /must/ be greater than @0@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-11000# If @flags@ includes
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT',
--     then the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-11001# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and @flags@ includes
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT',
--     then all stages /must/ not specify @Xfb@ execution mode
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04494# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.width@
--     /must/ be greater than or equal to @1@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04495# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.height@
--     /must/ be greater than or equal to @1@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04496# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.width@
--     /must/ be a power-of-two value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04497# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.height@
--     /must/ be a power-of-two value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04498# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.width@
--     /must/ be less than or equal to @4@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04499# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.height@
--     /must/ be less than or equal to @4@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04500# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     feature is not enabled,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.width@
--     and
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.height@
--     /must/ both be equal to @1@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-06567# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@combinerOps@[0]
--     /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
--     value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-06568# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@combinerOps@[1]
--     /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
--     value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04501# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>
--     feature is not enabled,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@combinerOps@[0]
--     /must/ be
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04502# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     feature is not enabled,
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@combinerOps@[1]
--     /must/ be
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-primitiveFragmentShadingRateWithMultipleViewports-04503#
--     If the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported,
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT'
--     is not included in @pDynamicState->pDynamicStates@, and
--     'PipelineViewportStateCreateInfo'::@viewportCount@ is greater than
--     @1@, entry points specified in @pStages@ /must/ not write to the
--     @PrimitiveShadingRateKHR@ built-in
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-primitiveFragmentShadingRateWithMultipleViewports-04504#
--     If the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, and entry points specified in @pStages@
--     write to the @ViewportIndex@ built-in, they /must/ not also write to
--     the @PrimitiveShadingRateKHR@ built-in
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-primitiveFragmentShadingRateWithMultipleViewports-04505#
--     If the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-primitiveFragmentShadingRateWithMultipleViewports primitiveFragmentShadingRateWithMultipleViewports>
--     limit is not supported, and entry points specified in @pStages@
--     write to the @ViewportMaskNV@ built-in, they /must/ not also write
--     to the @PrimitiveShadingRateKHR@ built-in
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-fragmentShadingRateNonTrivialCombinerOps-04506#
--     If the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-fragmentShadingRateNonTrivialCombinerOps fragmentShadingRateNonTrivialCombinerOps>
--     limit is not supported, and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@, elements of
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@combinerOps@
--     /must/ be
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-None-06569# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@shadingRateType@
--     /must/ be a valid
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.FragmentShadingRateTypeNV'
--     value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-06570# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@shadingRate@
--     /must/ be a valid
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.FragmentShadingRateNV'
--     value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-06571# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@combinerOps@[0]
--     /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
--     value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-06572# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@,
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@combinerOps@[1]
--     /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FragmentShadingRateCombinerOpKHR'
--     value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04569# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-fragmentShadingRateEnums fragmentShadingRateEnums>
--     feature is not enabled,
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@shadingRateType@
--     /must/ be equal to
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.FRAGMENT_SHADING_RATE_TYPE_FRAGMENT_SIZE_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04570# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     feature is not enabled,
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@shadingRate@
--     /must/ be equal to
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.FRAGMENT_SHADING_RATE_1_INVOCATION_PER_PIXEL_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04571# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>
--     feature is not enabled,
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@combinerOps@[0]
--     /must/ be
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-04572# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     feature is not enabled,
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@combinerOps@[1]
--     /must/ be
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-fragmentShadingRateNonTrivialCombinerOps-04573#
--     If the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-fragmentShadingRateNonTrivialCombinerOps fragmentShadingRateNonTrivialCombinerOps>
--     limit is not supported and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--     is not included in @pDynamicState->pDynamicStates@, elements of
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@combinerOps@
--     /must/ be
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_KEEP_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.FRAGMENT_SHADING_RATE_COMBINER_OP_REPLACE_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-None-04574# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-supersampleFragmentShadingRates supersampleFragmentShadingRates>
--     feature is not enabled,
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@shadingRate@
--     /must/ not be equal to
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.FRAGMENT_SHADING_RATE_2_INVOCATIONS_PER_PIXEL_NV',
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.FRAGMENT_SHADING_RATE_4_INVOCATIONS_PER_PIXEL_NV',
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.FRAGMENT_SHADING_RATE_8_INVOCATIONS_PER_PIXEL_NV',
--     or
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.FRAGMENT_SHADING_RATE_16_INVOCATIONS_PER_PIXEL_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-None-04575# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-noInvocationFragmentShadingRates noInvocationFragmentShadingRates>
--     feature is not enabled,
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV'::@shadingRate@
--     /must/ not be equal to
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.FRAGMENT_SHADING_RATE_NO_INVOCATIONS_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-03578# All
--     elements of the @pDynamicStates@ member of @pDynamicState@ /must/
--     not be
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04807# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-vertexInputDynamicState vertexInputDynamicState>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07067# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and includes a mesh shader, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-04800# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-colorWriteEnable colorWriteEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_ENABLE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-rasterizationSamples-04899# If
--     the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and the @VK_QCOM_render_pass_shader_resolve@ extension or the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-customResolve customResolve>
--     feature is enabled, @rasterizationSamples@ is not dynamic, and if
--     subpass has any input attachments, and if the subpass description
--     contains
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_EXT',
--     then the sample count of the input attachments /must/ equal
--     @rasterizationSamples@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-sampleShadingEnable-04900# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and the @VK_QCOM_render_pass_shader_resolve@ extension or the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-customResolve customResolve>
--     feature is enabled, and if the subpass description contains
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_FRAGMENT_REGION_BIT_EXT',
--     then @sampleShadingEnable@ /must/ be false
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-dynamicRendering-06576# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRendering dynamicRendering>
--     feature is not enabled and the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-multiview-06577# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-multiview multiview>
--     feature is not enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     /must/ be @0@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06578# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the
--     index of the most significant bit in
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     /must/ be less than
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxMultiviewViewCount maxMultiviewViewCount>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06579# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     is not 0,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     /must/ be a valid pointer to an array of @colorAttachmentCount@
--     valid 'Vulkan.Core10.Enums.Format.Format' values
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06580# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', each
--     element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06582# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and any
--     element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     is not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', that format
--     /must/ be a format with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     or
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06583# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06584# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06585# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     is not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a
--     format with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06586# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     is not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a
--     format with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#potential-format-features potential format features>
--     that include
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06587# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     is not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a
--     format that includes a depth component
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06588# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     is not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', it /must/ be a
--     format that includes a stencil component
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06589# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     is not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     is not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED',
--     @depthAttachmentFormat@ /must/ equal @stencilAttachmentFormat@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09033# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is being
--     created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and either of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@depthAttachmentFormat@
--     or
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@stencilAttachmentFormat@
--     are not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', and the
--     @VK_EXT_extended_dynamic_state3@ extension is not enabled or any of
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP', or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS'
--     dynamic states are not set, @pDepthStencilState@ /must/ be a valid
--     pointer to a valid 'PipelineDepthStencilStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDepthStencilState-09034# If
--     @pDepthStencilState@ is not @NULL@ it /must/ be a valid pointer to a
--     valid 'PipelineDepthStencilStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09035# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE' and the pipeline is
--     being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     but not
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and the @VK_EXT_extended_dynamic_state3@ extension is not enabled,
--     or any of the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP', or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS'
--     dynamic states are not set, @pDepthStencilState@ /must/ be a valid
--     pointer to a valid 'PipelineDepthStencilStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDepthStencilState-09036# If
--     @pDepthStencilState@ is not @NULL@ it /must/ be a valid pointer to a
--     valid 'PipelineDepthStencilStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09037# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is being
--     created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and any element of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@pColorAttachmentFormats@
--     is not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED', and the
--     @VK_EXT_extended_dynamic_state3@ extension is not enabled, or any of
--     the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_ENABLE_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT',
--     or 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_BLEND_CONSTANTS'
--     dynamic states are not set, @pColorBlendState@ /must/ be a valid
--     pointer to a valid 'PipelineColorBlendStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pColorBlendState-09038# If
--     @pColorBlendState@ is not @NULL@ it /must/ be a valid pointer to a
--     valid 'PipelineColorBlendStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06055# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', @pColorBlendState@ is
--     not dynamic, and the pipeline is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @pColorBlendState->attachmentCount@ /must/ be equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-11504# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', a
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--     is in the pNext chain, and the pipeline is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     /must/ be equal to
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@colorAttachmentCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06057# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is being
--     created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     is not @0@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-multiview-tess multiviewTessellationShader>
--     feature is not enabled, then @pStages@ /must/ not include
--     tessellation shaders
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06058# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is being
--     created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     is not @0@, and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-multiview-gs multiviewGeometryShader>
--     feature is not enabled, then @pStages@ /must/ not include a geometry
--     shader
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06059# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is being
--     created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     is not @0@, all of the shaders in the pipeline /must/ not include
--     variables decorated with the @Layer@ built-in decoration in their
--     interfaces
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-07720# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is being
--     created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     is not @0@, and @multiviewMeshShader@ is not enabled, then @pStages@
--     /must/ not include a mesh shader
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-12326# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is being
--     created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     @pStages@ include a mesh shader, and
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     is not @0@, then the index of the most significant bit in @viewMask@
--     /must/ be less than
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxMeshMultiviewViewCount maxMeshMultiviewViewCount>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06061# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     fragment shaders in @pStages@ /must/ not include the
--     @InputAttachment@ capability
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-08710# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     fragment shaders in @pStages@ /must/ not include any of the
--     @TileImageColorReadAccessEXT@, @TileImageDepthReadAccessEXT@, or
--     @TileImageStencilReadAccessEXT@ capabilities
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06062# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>
--     and @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', for
--     each color attachment format defined by the
--     @pColorAttachmentFormats@ member of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo',
--     if its
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#potential-format-features potential format features>
--     do not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT',
--     then the @blendEnable@ member of the corresponding element of the
--     @pAttachments@ member of @pColorBlendState@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06063# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>
--     and @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', if the
--     @pNext@ chain includes
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV',
--     the @colorAttachmentCount@ member of that structure /must/ be equal
--     to the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06591# If @pStages@
--     includes a fragment shader stage, and the fragment shader declares
--     the @EarlyFragmentTests@ execution mode, the @flags@ member of
--     'PipelineDepthStencilStateCreateInfo' /must/ not include
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06482# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and the @flags@ member of 'PipelineColorBlendStateCreateInfo'
--     includes
--     'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT',
--     @renderPass@ /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-None-09526# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-dynamicRenderingLocalRead dynamicRenderingLocalRead>
--     feature is not enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     and the @flags@ member of 'PipelineDepthStencilStateCreateInfo'
--     includes
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT',
--     @renderPass@ /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pColorAttachmentSamples-06592# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     elements of the @pColorAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     /must/ be valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' values
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-depthStencilAttachmentSamples-06593#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>
--     and the @depthStencilAttachmentSamples@ member of
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD'
--     or
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.AttachmentSampleCountInfoNV'
--     is not 0, it /must/ be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09527# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     the @flags@ member of 'PipelineColorBlendStateCreateInfo' includes
--     'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlagBits.PIPELINE_COLOR_BLEND_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_ACCESS_BIT_EXT'
--     @subpass@ /must/ have been created with
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_COLOR_ACCESS_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09528# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     the @flags@ member of 'PipelineDepthStencilStateCreateInfo' includes
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT',
--     @subpass@ /must/ have been created with
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_DEPTH_ACCESS_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09529# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     the @flags@ member of 'PipelineDepthStencilStateCreateInfo' includes
--     'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlagBits.PIPELINE_DEPTH_STENCIL_STATE_CREATE_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT',
--     @subpass@ /must/ have been created with
--     'Vulkan.Core10.Enums.SubpassDescriptionFlagBits.SUBPASS_DESCRIPTION_RASTERIZATION_ORDER_ATTACHMENT_STENCIL_ACCESS_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pipelineStageCreationFeedbackCount-06594#
--     If
--     'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfo'::@pipelineStageCreationFeedbackCount@
--     is not @0@, it /must/ be equal to @stageCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06595# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline is being
--     created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and
--     'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.MultiviewPerViewAttributesInfoNVX'::@perViewAttributesPositionXOnly@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE' then
--     'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.MultiviewPerViewAttributesInfoNVX'::@perViewAttributes@
--     /must/ also be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06596# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other flag, the value of
--     'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.MultiviewPerViewAttributesInfoNVX'::@perViewAttributes@
--     specified in both this pipeline and the library /must/ be equal
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06597# If one element
--     of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and another element includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     the value of
--     'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.MultiviewPerViewAttributesInfoNVX'::@perViewAttributes@
--     specified in both libraries /must/ be equal
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06598# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other flag, the value of
--     'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.MultiviewPerViewAttributesInfoNVX'::@perViewAttributesPositionXOnly@
--     specified in both this pipeline and the library /must/ be equal
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06599# If one element
--     of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and another element includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     the value of
--     'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.MultiviewPerViewAttributesInfoNVX'::@perViewAttributesPositionXOnly@
--     specified in both libraries /must/ be equal
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-06600# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     @pStages@ /must/ be a valid pointer to an array of @stageCount@
--     valid 'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo'
--     structures
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-stageCount-09587# If the pipeline
--     does not require
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     @stageCount@ /must/ be zero
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pRasterizationState-06601# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-pRasterizationState-null related dynamic state is not set>,
--     @pRasterizationState@ /must/ be a valid pointer to a valid
--     'PipelineRasterizationStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pRasterizationState-09039# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-pMultisampleState-null related dynamic state is not set>,
--     then @pMultisampleState@ /must/ be a valid pointer to a valid
--     'PipelineMultisampleStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pRasterizationState-09040# If
--     @pRasterizationState@ is not @NULL@ it /must/ be a valid pointer to
--     a valid 'PipelineRasterizationStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-layout-06602# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     @layout@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout'
--     handle
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-06603# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output state>,
--     and @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @renderPass@ /must/ be a valid 'Vulkan.Core10.Handles.RenderPass'
--     handle
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-stageCount-09530# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     @stageCount@ /must/ be greater than @0@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-graphicsPipelineLibrary-06606# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-graphicsPipelineLibrary graphicsPipelineLibrary>
--     feature is not enabled, and if the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-shaderMeshEnqueue shaderMeshEnqueue>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06608# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-shaderMeshEnqueue shaderMeshEnqueue>
--     feature is not enabled, and the pipeline is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete all possible state subsets>,
--     @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06609# If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT',
--     pipeline libraries included via
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-09245# If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT',
--     @flags@ /must/ also include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06610# If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT',
--     pipeline libraries included via
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     /must/ have been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06611# Any pipeline
--     libraries included via
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     /must/ not include any
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets state subset>
--     already defined by this structure or defined by any other pipeline
--     library in
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06612# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other flag, and @layout@ was not created with
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
--     then the @layout@ used by this pipeline and the library /must/ be
--     /identically defined/
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06613# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     one element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and another element includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and the @layout@ specified by either library was not created with
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
--     then the @layout@ used by each library /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06614# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other subset, and @layout@ was created with
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
--     then the @layout@ used by the library /must/ also have been created
--     with
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06615# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     one element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and another element includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and the @layout@ specified by either library was created with
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
--     then the @layout@ used by both libraries /must/ have been created
--     with
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06616# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other subset, and @layout@ was created with
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
--     elements of the @pSetLayouts@ array which @layout@ was created with
--     that are not 'Vulkan.Core10.APIConstants.NULL_HANDLE' /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--     to the element at the same index of @pSetLayouts@ used to create the
--     library’s @layout@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06617# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     one element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and another element includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and the @layout@ specified by either library was created with
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
--     elements of the @pSetLayouts@ array which either @layout@ was
--     created with that are not 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--     to the element at the same index of @pSetLayouts@ used to create the
--     other library’s @layout@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06618# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other flag, any descriptor set layout /N/ specified by
--     @layout@ in both this pipeline and the library which include
--     bindings accessed by shader stages in each /must/ be /identically
--     defined/
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06619# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     one element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and another element includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     any descriptor set layout /N/ specified by @layout@ in both
--     libraries which include bindings accessed by shader stages in each
--     /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06620# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other flag, push constants specified in @layout@ in
--     both this pipeline and the library which are available to shader
--     stages in each /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06621# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     and one element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and another element includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     push constants specified in @layout@ in both this pipeline and the
--     library which are available to shader stages in each /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06679# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     and
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other subset, any element of the @pSetLayouts@ array
--     when @layout@ was created and the corresponding element of the
--     @pSetLayouts@ array used to create the library’s @layout@ /must/ not
--     both be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06681# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     one element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and another element includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and any element of the @pSetLayouts@ array used to create each
--     library’s @layout@ was 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     then the corresponding element of the @pSetLayouts@ array used to
--     create the other library’s @layout@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06756# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other subset, and any element of the @pSetLayouts@
--     array which @layout@ was created with was
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', then the corresponding
--     element of the @pSetLayouts@ array used to create the library’s
--     @layout@ /must/ not have shader bindings for shaders in the other
--     subset
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06757# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other subset, and any element of the @pSetLayouts@
--     array used to create the library’s @layout@ was
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', then the corresponding
--     element of the @pSetLayouts@ array used to create this pipeline’s
--     @layout@ /must/ not have shader bindings for shaders in the other
--     subset
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06758# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     one element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and another element includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and any element of the @pSetLayouts@ array used to create each
--     library’s @layout@ was 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     then the corresponding element of the @pSetLayouts@ array used to
--     create the other library’s @layout@ /must/ not have shader bindings
--     for shaders in the other subset
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06682# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     and
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes both
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     @layout@ /must/ have been created with no elements of the
--     @pSetLayouts@ array set to 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06683# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     and @pRasterizationState->rasterizerDiscardEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', @layout@ /must/ have been
--     created with no elements of the @pSetLayouts@ array set to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06684# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes at least one of and no more than two of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes one of the other flags, the value of @subpass@ /must/ be
--     equal to that used to create the library
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06623# If one element
--     of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes at least one of and no more than two of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     and another element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes one of the other flags, the value of @subpass@ used to
--     create each library /must/ be identical
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderpass-06624# If @renderpass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes at least one of and no more than two of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes one of the other flags, @renderPass@ /must/ be compatible
--     with that used to create the library
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderpass-06625# If @renderpass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes at least one of and no more than two of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes one of the other flags, the value of @renderPass@ used to
--     create that library /must/ also be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06626# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes at least one of and no more than two of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes one of the other flags, and @renderPass@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     used by this pipeline and that specified by the library /must/ be
--     identical
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06627# If one element
--     of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes at least one of and no more than two of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     another element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes one of the other flags, and @renderPass@ was
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' for both libraries, the
--     value of
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     set by each library /must/ be identical
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06628# If one element
--     of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes at least one of and no more than two of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     and another element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes one of the other flags, the @renderPass@ objects used to
--     create each library /must/ be compatible or all equal to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderpass-06631# If @renderPass@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     and the @VK_EXT_extended_dynamic_state3@ extension is not enabled or
--     any of the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT', or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
--     dynamic states is not set, or the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-alphaToOne alphaToOne>
--     feature is enabled and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
--     is not set, then @pMultisampleState@ /must/ be a valid pointer to a
--     valid 'PipelineMultisampleStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-Input-06632# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     with a fragment shader that either enables
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-sampleshading sample shading>
--     or decorates any variable in the @Input@ storage class with
--     @Sample@, and the @VK_EXT_extended_dynamic_state3@ extension is not
--     enabled or any of the
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT', or
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
--     dynamic states is not set, or the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-alphaToOne alphaToOne>
--     feature is enabled and
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
--     is not set, then @pMultisampleState@ /must/ be a valid pointer to a
--     valid 'PipelineMultisampleStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-11856# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes either
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--     is included and
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     also includes either
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     the library /must/ also include
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--     and the
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     specified by the library /must/ be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-11857# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes either
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--     is not included or
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     also includes either
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     either the library /must/ not include
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--     or the
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     specified by the library /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-customResolve-11858# If one
--     element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     includes either
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--     is included and
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', and another element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     also includes either
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     the other library /must/ also include
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--     and the
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     specified by the library /must/ be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-customResolve-11859# If one
--     element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     includes either
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--     is not included or
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', and another element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     also includes either
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     either the other library /must/ not include
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'
--     or the
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT'::@customResolve@
--     specified by the library /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06633# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT'
--     with a @pMultisampleState@ that was not @NULL@, and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     was created with
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     @pMultisampleState@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--     to that used to create the library
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06634# If an element
--     of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     was created with
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT'
--     with a @pMultisampleState@ that was not @NULL@, and if
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     @pMultisampleState@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--     to that used to create the library
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06635# If one element
--     of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     was created with
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT'
--     with a @pMultisampleState@ that was not @NULL@, and if a different
--     element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     was created with
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     the @pMultisampleState@ used to create each library /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06636# If one element
--     of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     was created with
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
--     and a value of @pMultisampleState->sampleShadingEnable@ equal
--     'Vulkan.Core10.FundamentalTypes.TRUE', and if a different element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     was created with
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     the @pMultisampleState@ used to create each library /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06637# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     @pMultisampleState->sampleShadingEnable@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     was created with
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     @pMultisampleState@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--     to that used to create the library
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-09567# If one element
--     of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     was created with
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT'
--     and a value of @pMultisampleState->sampleShadingEnable@ equal
--     'Vulkan.Core10.FundamentalTypes.TRUE', and if
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     @pMultisampleState@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#glossary-identically-defined identically defined>
--     to that used to create the library
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06638# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes only one of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and an element of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes the other flag, values specified in
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'
--     for both this pipeline and that library /must/ be identical
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06639# If one element
--     of
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     and another element includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     values specified in
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'
--     for both this pipeline and that library /must/ be identical
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06640# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     @pStages@ /must/ be a valid pointer to an array of @stageCount@
--     valid 'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo'
--     structures
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06642# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     @layout@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout'
--     handle
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06643# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     and @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @renderPass@ /must/ be a valid 'Vulkan.Core10.Handles.RenderPass'
--     handle
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06644# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     @stageCount@ /must/ be greater than @0@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06645# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     is non-zero, if @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR',
--     any libraries /must/ have also been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06646# If
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes more than one library, and any library was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR',
--     all libraries /must/ have also been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pLibraries-06647# If
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@
--     includes at least one library,
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     is non-zero, and any library was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR',
--     @flags@ /must/ include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-None-07826# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     the pipeline includes a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete set of state>,
--     and there are no libraries included in
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'::@pLibraries@,
--     then 'Vulkan.Core10.Handles.PipelineLayout' /must/ be a valid
--     pipeline layout
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-layout-07827# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     the pipeline includes a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete set of state>
--     specified entirely by libraries, and each library was created with a
--     'Vulkan.Core10.Handles.PipelineLayout' created without
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
--     then @layout@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorsets-compatibility compatible>
--     with the layouts in those libraries
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06729# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT',
--     the pipeline includes a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete set of state>
--     specified entirely by libraries, and each library was created with a
--     'Vulkan.Core10.Handles.PipelineLayout' created with
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
--     then @layout@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorsets-compatibility compatible>
--     with the union of the libraries\' pipeline layouts other than the
--     inclusion\/exclusion of
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-06730# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     @flags@ does not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LINK_TIME_OPTIMIZATION_BIT_EXT',
--     the pipeline includes a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-complete complete set of state>
--     specified entirely by libraries, and each library was created with a
--     'Vulkan.Core10.Handles.PipelineLayout' created with
--     'Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits.PIPELINE_LAYOUT_CREATE_INDEPENDENT_SETS_BIT_EXT',
--     then @layout@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#descriptorsets-compatibility compatible>
--     with the union of the libraries\' pipeline layouts
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-conservativePointAndLineRasterization-08892#
--     If
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-conservativePointAndLineRasterization conservativePointAndLineRasterization>
--     is not supported and the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#drawing-rasterization-input-topology effective rasterization input topology>
--     is in line or point topology class, then
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.PipelineRasterizationConservativeStateCreateInfoEXT'::@conservativeRasterizationMode@
--     /must/ be
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.CONSERVATIVE_RASTERIZATION_MODE_DISABLED_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-06894# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     but not
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     elements of @pStages@ /must/ not have @stage@ set to
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-06895# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     but not
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     elements of @pStages@ /must/ not have @stage@ set to a shader stage
--     which participates in pre-rasterization
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-06896# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     all elements of @pStages@ /must/ have a @stage@ set to a shader
--     stage which participates in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-stage-06897# If the pipeline
--     requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and\/or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     any value of @stage@ /must/ not be set in more than one element of
--     @pStages@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3TessellationDomainOrigin-07370#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3TessellationDomainOrigin extendedDynamicState3TessellationDomainOrigin>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_TESSELLATION_DOMAIN_ORIGIN_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3DepthClampEnable-07371#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3DepthClampEnable extendedDynamicState3DepthClampEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_ENABLE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3PolygonMode-07372#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3PolygonMode extendedDynamicState3PolygonMode>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_POLYGON_MODE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3RasterizationSamples-07373#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3RasterizationSamples extendedDynamicState3RasterizationSamples>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3SampleMask-07374#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3SampleMask extendedDynamicState3SampleMask>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3AlphaToCoverageEnable-07375#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3AlphaToCoverageEnable extendedDynamicState3AlphaToCoverageEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3AlphaToOneEnable-07376#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3AlphaToOneEnable extendedDynamicState3AlphaToOneEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3LogicOpEnable-07377#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3LogicOpEnable extendedDynamicState3LogicOpEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_ENABLE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3ColorBlendEnable-07378#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3ColorBlendEnable extendedDynamicState3ColorBlendEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3ColorBlendEquation-07379#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3ColorBlendEquation extendedDynamicState3ColorBlendEquation>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3ColorWriteMask-07380#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3ColorWriteMask extendedDynamicState3ColorWriteMask>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3RasterizationStream-07381#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3RasterizationStream extendedDynamicState3RasterizationStream>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_STREAM_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3ConservativeRasterizationMode-07382#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3ConservativeRasterizationMode extendedDynamicState3ConservativeRasterizationMode>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CONSERVATIVE_RASTERIZATION_MODE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3ExtraPrimitiveOverestimationSize-07383#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3ExtraPrimitiveOverestimationSize extendedDynamicState3ExtraPrimitiveOverestimationSize>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXTRA_PRIMITIVE_OVERESTIMATION_SIZE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-09639# If the
--     pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     @pDynamicState@ includes
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CONSERVATIVE_RASTERIZATION_MODE_EXT',
--     and @pDynamicState@ does not include
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXTRA_PRIMITIVE_OVERESTIMATION_SIZE_EXT',
--     @pRasterizationState@ /must/ include a
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.PipelineRasterizationConservativeStateCreateInfoEXT'
--     in its @pNext@ chain
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3DepthClipEnable-07384#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3DepthClipEnable extendedDynamicState3DepthClipEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3SampleLocationsEnable-07385#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3SampleLocationsEnable extendedDynamicState3SampleLocationsEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3ColorBlendAdvanced-07386#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3ColorBlendAdvanced extendedDynamicState3ColorBlendAdvanced>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3ProvokingVertexMode-07387#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3ProvokingVertexMode extendedDynamicState3ProvokingVertexMode>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PROVOKING_VERTEX_MODE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3LineRasterizationMode-07388#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3LineRasterizationMode extendedDynamicState3LineRasterizationMode>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3LineStippleEnable-07389#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3LineStippleEnable extendedDynamicState3LineStippleEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3DepthClipNegativeOneToOne-07390#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3DepthClipNegativeOneToOne extendedDynamicState3DepthClipNegativeOneToOne>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_NEGATIVE_ONE_TO_ONE_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3ViewportWScalingEnable-07391#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3ViewportWScalingEnable extendedDynamicState3ViewportWScalingEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_ENABLE_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3ViewportSwizzle-07392#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3ViewportSwizzle extendedDynamicState3ViewportSwizzle>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3CoverageToColorEnable-07393#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3CoverageToColorEnable extendedDynamicState3CoverageToColorEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3CoverageToColorLocation-07394#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3CoverageToColorLocation extendedDynamicState3CoverageToColorLocation>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_LOCATION_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3CoverageModulationMode-07395#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3CoverageModulationMode extendedDynamicState3CoverageModulationMode>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_MODE_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3CoverageModulationTableEnable-07396#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3CoverageModulationTableEnable extendedDynamicState3CoverageModulationTableEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_ENABLE_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3CoverageModulationTable-07397#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3CoverageModulationTable extendedDynamicState3CoverageModulationTable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3CoverageReductionMode-07398#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3CoverageReductionMode extendedDynamicState3CoverageReductionMode>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_REDUCTION_MODE_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3RepresentativeFragmentTestEnable-07399#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3RepresentativeFragmentTestEnable extendedDynamicState3RepresentativeFragmentTestEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_REPRESENTATIVE_FRAGMENT_TEST_ENABLE_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-extendedDynamicState3ShadingRateImageEnable-07400#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-extendedDynamicState3ShadingRateImageEnable extendedDynamicState3ShadingRateImageEnable>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SHADING_RATE_IMAGE_ENABLE_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-07401# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-07997# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07730# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-multiviewPerViewViewports multiviewPerViewViewports>
--     feature is enabled, @renderpass@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT',
--     then the index of the most significant bit in each element of
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@pViewMasks@
--     /must/ be less than @pViewportState->viewportCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicStates-07731# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-multiviewPerViewViewports multiviewPerViewViewports>
--     feature is enabled, @renderpass@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT',
--     then the index of the most significant bit in each element of
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo'::@pViewMasks@
--     /must/ be less than @pViewportState->scissorCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-multiviewPerViewViewports-12249#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-multiviewPerViewViewports multiviewPerViewViewports>
--     feature is enabled, @renderpass@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT',
--     then the index of the most significant bit in
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     /must/ be less than @pViewportState->viewportCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-multiviewPerViewViewports-12250#
--     If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-multiviewPerViewViewports multiviewPerViewViewports>
--     feature is enabled, @renderpass@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>,
--     and no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT',
--     then the index of the most significant bit in
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     /must/ be less than @pViewportState->scissorCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-08711# If @pStages@
--     includes a fragment shader stage,
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE'
--     is not set in 'PipelineDynamicStateCreateInfo'::@pDynamicStates@,
--     and the fragment shader declares the @EarlyFragmentTests@ execution
--     mode and uses @OpDepthAttachmentReadEXT@, the @depthWriteEnable@
--     member of 'PipelineDepthStencilStateCreateInfo' /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pStages-08712# If @pStages@
--     includes a fragment shader stage,
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_WRITE_MASK'
--     is not set in 'PipelineDynamicStateCreateInfo'::@pDynamicStates@,
--     and the fragment shader declares the @EarlyFragmentTests@ execution
--     mode and uses @OpStencilAttachmentReadEXT@, the value of
--     'StencilOpState'::@writeMask@ for both @front@ and @back@ in
--     'PipelineDepthStencilStateCreateInfo' /must/ be @0@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-08744# If @renderPass@
--     is 'Vulkan.Core10.APIConstants.NULL_HANDLE', the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output state>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>,
--     the pipeline enables
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-sampleshading sample shading>,
--     @rasterizationSamples@ is not dynamic, and the @pNext@ chain
--     includes a
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'
--     structure, @rasterizationSamples@ /must/ be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--     that is set in @imageCreateSampleCounts@ (as defined in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-image-creation-limits Image Creation Limits>)
--     for every element of @depthAttachmentFormat@,
--     @stencilAttachmentFormat@ and the @pColorAttachmentFormats@ array
--     which is not 'Vulkan.Core10.Enums.Format.FORMAT_UNDEFINED'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-08897# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_VERTEX_INPUT_INTERFACE_BIT_EXT',
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     is specified either in a library or by the inclusion of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     and that state includes a vertex shader stage in @pStages@, the
--     pipeline /must/ define
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-08898# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_VERTEX_INPUT_INTERFACE_BIT_EXT',
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     is not specified, the pipeline /must/ define
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-08899# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR',
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     is specified either in a library or by the inclusion of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     and that state includes a vertex shader stage in @pStages@, the
--     pipeline /must/ either define
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-vertex-input vertex input state>
--     or include that state in a linked pipeline library
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-08900# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT'
--     the pipeline /must/ define
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-08901# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR',
--     the pipeline /must/ either define
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     or include that state in a linked pipeline library
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-08903# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     is specified either in a library or by the inclusion of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     and that state either includes
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     or has @pRasterizationState->rasterizerDiscardEnable@ set to
--     'Vulkan.Core10.FundamentalTypes.FALSE', the pipeline /must/ define
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-08904# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     is not specified, the pipeline /must/ define
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-08906# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     is specified either in a library or by the inclusion of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_OUTPUT_INTERFACE_BIT_EXT',
--     and that state either includes
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     or has @pRasterizationState->rasterizerDiscardEnable@ set to
--     'Vulkan.Core10.FundamentalTypes.FALSE', the pipeline /must/ define
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-08907# If
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT'::@flags@
--     includes
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_FRAGMENT_SHADER_BIT_EXT',
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     is not specified, the pipeline /must/ define
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-08909# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR',
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader state>
--     is specified either in a library or by the inclusion of
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GRAPHICS_PIPELINE_LIBRARY_PRE_RASTERIZATION_SHADERS_BIT_EXT',
--     and that state either includes
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE'
--     or has @pRasterizationState->rasterizerDiscardEnable@ set to
--     'Vulkan.Core10.FundamentalTypes.FALSE', the pipeline /must/ define
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     or include those states in linked pipeline libraries
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-None-09043# If
--     @pDynamicState->pDynamicStates@ does not include
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT',
--     and the format of any color attachment is
--     'Vulkan.Core10.Enums.Format.FORMAT_E5B9G9R9_UFLOAT_PACK32', the
--     @colorWriteMask@ member of the corresponding element of
--     @pColorBlendState->pAttachments@ /must/ either include all of
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.COLOR_COMPONENT_R_BIT',
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.COLOR_COMPONENT_G_BIT',
--     and
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.COLOR_COMPONENT_B_BIT',
--     or none of them
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09301# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     is not @0@,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@viewMask@
--     /must/ be @0@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09304# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     is not @0@, and @rasterizationSamples@ is not dynamic,
--     'PipelineMultisampleStateCreateInfo'::@rasterizationSamples@ /must/
--     be @1@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09305# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     is not @0@, and @blendEnable@ is not dynamic, the @blendEnable@
--     member of each element of @pColorBlendState->pAttachments@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09306# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     is not @0@, and @pDynamicState->pDynamicStates@ does not include
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR',
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.width@
--     /must/ be @1@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09307# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     is not @0@, and @pDynamicState->pDynamicStates@ does not include
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR',
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.height@
--     /must/ be @1@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09308# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output pre-rasterization shader state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     is not @0@, the last
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>
--     /must/ not statically use a variable with the
--     @PrimitiveShadingRateKHR@ built-in
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09309# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     is not @0@,
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--     /must/ be @1@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09310# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID'::@externalFormat@
--     is not @0@, the fragment shader /must/ not declare the
--     @DepthReplacing@ or @StencilRefReplacingEXT@ execution modes
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09313# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @subpass@ includes an external format resolve attachment, and
--     @rasterizationSamples@ is not dynamic,
--     'PipelineMultisampleStateCreateInfo'::@rasterizationSamples@ /must/
--     be 'Vulkan.Core10.Enums.SampleCountFlagBits.SAMPLE_COUNT_1_BIT'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09314# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @subpass@ includes an external format resolve attachment, and
--     @blendEnable@ is not dynamic, the @blendEnable@ member of each
--     element of @pColorBlendState->pAttachments@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09315# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @subpass@ includes an external format resolve attachment, and
--     @pDynamicState->pDynamicStates@ does not include
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR',
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.width@
--     /must/ be @1@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09316# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @subpass@ includes an external format resolve attachment, and
--     @pDynamicState->pDynamicStates@ does not include
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR',
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'::@fragmentSize.height@
--     /must/ be @1@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-externalFormatResolve-09317# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-externalFormatResolve externalFormatResolve>
--     feature is enabled, the pipeline requires
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output pre-rasterization shader state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output interface state>,
--     @renderPass@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     @subpass@ includes an external format resolve attachment, the last
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stage>
--     /must/ not statically use a variable with the
--     @PrimitiveShadingRateKHR@ built-in
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09531# If the pipeline
--     is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output state>,
--     the value of @renderPass@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'
--     is included,
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'::@colorAttachmentCount@
--     /must/ be equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09652# If the pipeline
--     is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-shader fragment shader state>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output state>,
--     the value of @renderPass@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'
--     is not included, the fragment shader /must/ not contain any input
--     attachments with a @InputAttachmentIndex@ greater than or equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-renderPass-09532# If the pipeline
--     is being created with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-fragment-output fragment output state>,
--     and the value of @renderPass@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingAttachmentLocationInfo'::@colorAttachmentCount@
--     /must/ be equal to
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo'::@colorAttachmentCount@
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-11273# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     includes
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     all libraries linked to this pipeline /must/ also have that flag set
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-11274# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     all libraries linked to this pipeline /must/ also not have that flag
--     set
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-12355# If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     all libraries linked to this pipeline /must/ also have that flag set
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-12356# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DESCRIPTOR_BUFFER_BIT_EXT',
--     all libraries linked to this pipeline /must/ also not have that flag
--     set
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-12357# If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT',
--     all libraries linked to this pipeline /must/ also have that flag set
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-12358# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT',
--     all libraries linked to this pipeline /must/ also not have that flag
--     set
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-12359# If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT',
--     all libraries linked to this pipeline /must/ also have that flag set
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-flags-12360# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT',
--     all libraries linked to this pipeline /must/ also not have that flag
--     set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pNext-pNext# Each @pNext@ member
--     of any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_AMD_mixed_attachment_samples.AttachmentSampleCountInfoAMD',
--     'Vulkan.Extensions.VK_EXT_custom_resolve.CustomResolveCreateInfoEXT',
--     'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID',
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkExternalFormatOHOS VkExternalFormatOHOS>,
--     'Vulkan.Extensions.VK_EXT_graphics_pipeline_library.GraphicsPipelineLibraryCreateInfoEXT',
--     'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsPipelineShaderGroupsCreateInfoNV',
--     'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.MultiviewPerViewAttributesInfoNVX',
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR',
--     'Vulkan.Extensions.VK_AMD_pipeline_compiler_control.PipelineCompilerControlCreateInfoAMD',
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.PipelineCreateFlags2CreateInfo',
--     'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfo',
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT',
--     'Vulkan.Extensions.VK_VALVE_fragment_density_map_layered.PipelineFragmentDensityMapLayeredCreateInfoVALVE',
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PipelineFragmentShadingRateEnumStateCreateInfoNV',
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR',
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR',
--     'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.PipelineRenderingCreateInfo',
--     'Vulkan.Extensions.VK_NV_representative_fragment_test.PipelineRepresentativeFragmentTestStateCreateInfoNV',
--     'Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'.PipelineRobustnessCreateInfo',
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingAttachmentLocationInfo',
--     or
--     'Vulkan.Core14.Promoted_From_VK_KHR_dynamic_rendering_local_readRoadmap.RenderingInputAttachmentIndexInfo'
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-sType-unique# The @sType@ value
--     of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-pDynamicState-parameter# If
--     @pDynamicState@ is not @NULL@, @pDynamicState@ /must/ be a valid
--     pointer to a valid 'PipelineDynamicStateCreateInfo' structure
--
-- -   #VUID-VkGraphicsPipelineCreateInfo-commonparent# Each of
--     @basePipelineHandle@, @layout@, and @renderPass@ that are valid
--     handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.Pipeline', 'PipelineColorBlendStateCreateInfo',
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'PipelineDepthStencilStateCreateInfo', 'PipelineDynamicStateCreateInfo',
-- 'PipelineInputAssemblyStateCreateInfo',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'PipelineMultisampleStateCreateInfo',
-- 'PipelineRasterizationStateCreateInfo',
-- 'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo',
-- 'PipelineTessellationStateCreateInfo',
-- 'PipelineVertexInputStateCreateInfo', 'PipelineViewportStateCreateInfo',
-- 'Vulkan.Core10.Handles.RenderPass',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createGraphicsPipelines'
data GraphicsPipelineCreateInfo (es :: [Type]) = GraphicsPipelineCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
    -- specifying how the pipeline will be generated.
    flags :: PipelineCreateFlags
  , -- | @stageCount@ is the number of entries in the @pStages@ array.
    stageCount :: Word32
  , -- | @pStages@ is a pointer to an array of @stageCount@
    -- 'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo' structures
    -- describing the set of the shader stages to be included in the graphics
    -- pipeline.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pVertexInputState@ is a pointer to a
    -- 'PipelineVertexInputStateCreateInfo' structure. It is ignored if the
    -- pipeline includes a mesh shader stage. It /can/ be @NULL@ if the
    -- pipeline is created with the
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
    -- dynamic state set.
    vertexInputState :: Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
  , -- | @pInputAssemblyState@ is a pointer to a
    -- 'PipelineInputAssemblyStateCreateInfo' structure which determines input
    -- assembly behavior for vertex shading, as described in
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#drawing Drawing Commands>.
    -- #pipelines-pInputAssemblyState-null# If the
    -- @VK_EXT_extended_dynamic_state3@ extension is enabled, it /can/ be
    -- @NULL@ if the pipeline is created with both
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_RESTART_ENABLE',
    -- and 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY'
    -- dynamic states set and
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-dynamicPrimitiveTopologyUnrestricted dynamicPrimitiveTopologyUnrestricted>
    -- is 'Vulkan.Core10.FundamentalTypes.TRUE'. It is ignored if the pipeline
    -- includes a mesh shader stage.
    inputAssemblyState :: Maybe PipelineInputAssemblyStateCreateInfo
  , -- | @pTessellationState@ is a pointer to a
    -- 'PipelineTessellationStateCreateInfo' structure defining tessellation
    -- state used by tessellation shaders. It /can/ be @NULL@ if the pipeline
    -- is created with the
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PATCH_CONTROL_POINTS_EXT'
    -- dynamic state set.
    tessellationState :: Maybe (SomeStruct PipelineTessellationStateCreateInfo)
  , -- | @pViewportState@ is a pointer to a 'PipelineViewportStateCreateInfo'
    -- structure defining viewport state used when rasterization is enabled.
    -- #pipelines-pViewportState-null# If the @VK_EXT_extended_dynamic_state3@
    -- extension is enabled, it /can/ be @NULL@ if the pipeline is created with
    -- both
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT',
    -- and 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT'
    -- dynamic states set.
    viewportState :: Maybe (SomeStruct PipelineViewportStateCreateInfo)
  , -- | @pRasterizationState@ is a pointer to a
    -- 'PipelineRasterizationStateCreateInfo' structure defining rasterization
    -- state. #pipelines-pRasterizationState-null# If the
    -- @VK_EXT_extended_dynamic_state3@ extension is enabled, it /can/ be
    -- @NULL@ if the pipeline is created with all of
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_ENABLE_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZER_DISCARD_ENABLE',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_POLYGON_MODE_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CULL_MODE',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRONT_FACE',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS_ENABLE',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS', and
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_WIDTH' dynamic
    -- states set.
    rasterizationState :: Maybe (SomeStruct PipelineRasterizationStateCreateInfo)
  , -- | @pMultisampleState@ is a pointer to a
    -- 'PipelineMultisampleStateCreateInfo' structure defining multisample
    -- state used when rasterization is enabled.
    -- #pipelines-pMultisampleState-null# If the
    -- @VK_EXT_extended_dynamic_state3@ extension is enabled, it /can/ be
    -- @NULL@ if the pipeline is created with all of
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT', and
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
    -- dynamic states set, and either the
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-alphaToOne alphaToOne>
    -- feature is not enabled or
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
    -- is set, in which case
    -- 'PipelineMultisampleStateCreateInfo'::@sampleShadingEnable@ is assumed
    -- to be 'Vulkan.Core10.FundamentalTypes.FALSE'.
    multisampleState :: Maybe (SomeStruct PipelineMultisampleStateCreateInfo)
  , -- | @pDepthStencilState@ is a pointer to a
    -- 'PipelineDepthStencilStateCreateInfo' structure defining depth\/stencil
    -- state used when rasterization is enabled for depth or stencil
    -- attachments accessed during rendering.
    -- #pipelines-pDepthStencilState-null# If the
    -- @VK_EXT_extended_dynamic_state3@ extension is enabled, it /can/ be
    -- @NULL@ if the pipeline is created with all of
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP', and
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS' dynamic
    -- states set.
    depthStencilState :: Maybe PipelineDepthStencilStateCreateInfo
  , -- | @pColorBlendState@ is a pointer to a 'PipelineColorBlendStateCreateInfo'
    -- structure defining color blend state used when rasterization is enabled
    -- for any color attachments accessed during rendering.
    -- #pipelines-pColorBlendState-null# If the
    -- @VK_EXT_extended_dynamic_state3@ extension is enabled, it /can/ be
    -- @NULL@ if the pipeline is created with all of
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_ENABLE_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT',
    -- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT',
    -- and 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_BLEND_CONSTANTS'
    -- dynamic states set.
    colorBlendState :: Maybe (SomeStruct PipelineColorBlendStateCreateInfo)
  , -- | @pDynamicState@ is a pointer to a 'PipelineDynamicStateCreateInfo'
    -- structure defining which properties of the pipeline state object are
    -- dynamic and /can/ be changed independently of the pipeline state. This
    -- /can/ be @NULL@, which means no state in the pipeline is considered
    -- dynamic.
    dynamicState :: Maybe PipelineDynamicStateCreateInfo
  , -- | @layout@ is the description of binding locations used by both the
    -- pipeline and descriptor sets used with the pipeline. If
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@apiVersion@
    -- is greater than or equal to Vulkan 1.3 or
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>
    -- is enabled @layout@ /must/ not be accessed by the implementation outside
    -- of the duration of the command this structure is passed to.
    layout :: PipelineLayout
  , -- | @renderPass@ is a handle to a render pass object describing the
    -- environment in which the pipeline will be used. The pipeline /must/ only
    -- be used with a render pass instance compatible with the one provided.
    -- See
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-compatibility Render Pass Compatibility>
    -- for more information. The implementation /must/ not access this object
    -- outside of the duration of the command this structure is passed to.
    renderPass :: RenderPass
  , -- | @subpass@ is the index of the subpass in the render pass where this
    -- pipeline will be used.
    subpass :: Word32
  , -- | @basePipelineHandle@ is a pipeline to derive from.
    basePipelineHandle :: Pipeline
  , -- | @basePipelineIndex@ is an index into the @pCreateInfos@ parameter to use
    -- as a pipeline to derive from.
    basePipelineIndex :: Int32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsPipelineCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (GraphicsPipelineCreateInfo es)

instance Extensible GraphicsPipelineCreateInfo where
  extensibleTypeName = "GraphicsPipelineCreateInfo"
  setNext GraphicsPipelineCreateInfo{..} next' = GraphicsPipelineCreateInfo{next = next', ..}
  getNext GraphicsPipelineCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends GraphicsPipelineCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineFragmentDensityMapLayeredCreateInfoVALVE = Just f
    | Just Refl <- eqT @e @RenderingInputAttachmentIndexInfo = Just f
    | Just Refl <- eqT @e @RenderingAttachmentLocationInfo = Just f
    | Just Refl <- eqT @e @PipelineRobustnessCreateInfo = Just f
    | Just Refl <- eqT @e @GraphicsPipelineLibraryCreateInfoEXT = Just f
    | Just Refl <- eqT @e @MultiviewPerViewAttributesInfoNVX = Just f
    | Just Refl <- eqT @e @AttachmentSampleCountInfoAMD = Just f
    | Just Refl <- eqT @e @PipelineRenderingCreateInfo = Just f
    | Just Refl <- eqT @e @CustomResolveCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineFragmentShadingRateEnumStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineFragmentShadingRateStateCreateInfoKHR = Just f
    | Just Refl <- eqT @e @PipelineLibraryCreateInfoKHR = Just f
    | Just Refl <- eqT @e @PipelineCompilerControlCreateInfoAMD = Just f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfo = Just f
    | Just Refl <- eqT @e @PipelineRepresentativeFragmentTestStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @ExternalFormatANDROID = Just f
    | Just Refl <- eqT @e @PipelineDiscardRectangleStateCreateInfoEXT = Just f
    | Just Refl <- eqT @e @GraphicsPipelineShaderGroupsCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineBinaryInfoKHR = Just f
    | Just Refl <- eqT @e @PipelineCreateFlags2CreateInfo = Just f
    | otherwise = Nothing

instance ( Extendss GraphicsPipelineCreateInfo es
         , PokeChain es ) => ToCStruct (GraphicsPipelineCreateInfo es) where
  withCStruct x f = allocaBytes 144 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsPipelineCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags)) (flags)
    let pStagesLength = Data.Vector.length $ (stages)
    stageCount'' <- lift $ if (stageCount) == 0
      then pure $ fromIntegral pStagesLength
      else do
        unless (fromIntegral pStagesLength == (stageCount) || pStagesLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pStages must be empty or have 'stageCount' elements" Nothing Nothing
        pure (stageCount)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (stageCount'')
    pStages'' <- if Data.Vector.null (stages)
      then pure nullPtr
      else do
        pPStages <- ContT $ allocaBytes @(PipelineShaderStageCreateInfo _) (((Data.Vector.length (stages))) * 48)
        Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) ((stages))
        pure $ pPStages
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) pStages''
    pVertexInputState'' <- case (vertexInputState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineVertexInputStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineVertexInputStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr (PipelineVertexInputStateCreateInfo _)))) pVertexInputState''
    pInputAssemblyState'' <- case (inputAssemblyState) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr PipelineInputAssemblyStateCreateInfo))) pInputAssemblyState''
    pTessellationState'' <- case (tessellationState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineTessellationStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineTessellationStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr (PipelineTessellationStateCreateInfo _)))) pTessellationState''
    pViewportState'' <- case (viewportState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineViewportStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineViewportStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (PipelineViewportStateCreateInfo _)))) pViewportState''
    pRasterizationState'' <- case (rasterizationState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineRasterizationStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineRasterizationStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (PipelineRasterizationStateCreateInfo _)))) pRasterizationState''
    pMultisampleState'' <- case (multisampleState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineMultisampleStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineMultisampleStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr (PipelineMultisampleStateCreateInfo _)))) pMultisampleState''
    pDepthStencilState'' <- case (depthStencilState) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 80 :: Ptr (Ptr PipelineDepthStencilStateCreateInfo))) pDepthStencilState''
    pColorBlendState'' <- case (colorBlendState) of
      Nothing -> pure nullPtr
      Just j -> ContT @_ @_ @(Ptr (PipelineColorBlendStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineColorBlendStateCreateInfo (j) (cont . castPtr)
    lift $ poke ((p `plusPtr` 88 :: Ptr (Ptr (PipelineColorBlendStateCreateInfo _)))) pColorBlendState''
    pDynamicState'' <- case (dynamicState) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 96 :: Ptr (Ptr PipelineDynamicStateCreateInfo))) pDynamicState''
    lift $ poke ((p `plusPtr` 104 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 112 :: Ptr RenderPass)) (renderPass)
    lift $ poke ((p `plusPtr` 120 :: Ptr Word32)) (subpass)
    lift $ poke ((p `plusPtr` 128 :: Ptr Pipeline)) (basePipelineHandle)
    lift $ poke ((p `plusPtr` 136 :: Ptr Int32)) (basePipelineIndex)
    lift $ f
  cStructSize = 144
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 120 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 136 :: Ptr Int32)) (zero)
    lift $ f

instance ( Extendss GraphicsPipelineCreateInfo es
         , PeekChain es ) => FromCStruct (GraphicsPipelineCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCreateFlags))
    stageCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _))))
    let pStagesLength = if pStages == nullPtr then 0 else (fromIntegral stageCount)
    pStages' <- generateM pStagesLength (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    pVertexInputState <- peek @(Ptr (PipelineVertexInputStateCreateInfo _)) ((p `plusPtr` 32 :: Ptr (Ptr (PipelineVertexInputStateCreateInfo _))))
    pVertexInputState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pVertexInputState
    pInputAssemblyState <- peek @(Ptr PipelineInputAssemblyStateCreateInfo) ((p `plusPtr` 40 :: Ptr (Ptr PipelineInputAssemblyStateCreateInfo)))
    pInputAssemblyState' <- maybePeek (\j -> peekCStruct @PipelineInputAssemblyStateCreateInfo (j)) pInputAssemblyState
    pTessellationState <- peek @(Ptr (PipelineTessellationStateCreateInfo _)) ((p `plusPtr` 48 :: Ptr (Ptr (PipelineTessellationStateCreateInfo _))))
    pTessellationState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pTessellationState
    pViewportState <- peek @(Ptr (PipelineViewportStateCreateInfo _)) ((p `plusPtr` 56 :: Ptr (Ptr (PipelineViewportStateCreateInfo _))))
    pViewportState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pViewportState
    pRasterizationState <- peek @(Ptr (PipelineRasterizationStateCreateInfo _)) ((p `plusPtr` 64 :: Ptr (Ptr (PipelineRasterizationStateCreateInfo _))))
    pRasterizationState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pRasterizationState
    pMultisampleState <- peek @(Ptr (PipelineMultisampleStateCreateInfo _)) ((p `plusPtr` 72 :: Ptr (Ptr (PipelineMultisampleStateCreateInfo _))))
    pMultisampleState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pMultisampleState
    pDepthStencilState <- peek @(Ptr PipelineDepthStencilStateCreateInfo) ((p `plusPtr` 80 :: Ptr (Ptr PipelineDepthStencilStateCreateInfo)))
    pDepthStencilState' <- maybePeek (\j -> peekCStruct @PipelineDepthStencilStateCreateInfo (j)) pDepthStencilState
    pColorBlendState <- peek @(Ptr (PipelineColorBlendStateCreateInfo _)) ((p `plusPtr` 88 :: Ptr (Ptr (PipelineColorBlendStateCreateInfo _))))
    pColorBlendState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pColorBlendState
    pDynamicState <- peek @(Ptr PipelineDynamicStateCreateInfo) ((p `plusPtr` 96 :: Ptr (Ptr PipelineDynamicStateCreateInfo)))
    pDynamicState' <- maybePeek (\j -> peekCStruct @PipelineDynamicStateCreateInfo (j)) pDynamicState
    layout <- peek @PipelineLayout ((p `plusPtr` 104 :: Ptr PipelineLayout))
    renderPass <- peek @RenderPass ((p `plusPtr` 112 :: Ptr RenderPass))
    subpass <- peek @Word32 ((p `plusPtr` 120 :: Ptr Word32))
    basePipelineHandle <- peek @Pipeline ((p `plusPtr` 128 :: Ptr Pipeline))
    basePipelineIndex <- peek @Int32 ((p `plusPtr` 136 :: Ptr Int32))
    pure $ GraphicsPipelineCreateInfo
             next
             flags
             stageCount
             pStages'
             pVertexInputState'
             pInputAssemblyState'
             pTessellationState'
             pViewportState'
             pRasterizationState'
             pMultisampleState'
             pDepthStencilState'
             pColorBlendState'
             pDynamicState'
             layout
             renderPass
             subpass
             basePipelineHandle
             basePipelineIndex

instance es ~ '[] => Zero (GraphicsPipelineCreateInfo es) where
  zero = GraphicsPipelineCreateInfo
           ()
           zero
           zero
           mempty
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing
           Nothing
           zero
           zero
           zero
           zero
           zero

