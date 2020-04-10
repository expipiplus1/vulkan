{-# language CPP #-}
module Graphics.Vulkan.Core10.Pipeline  ( createGraphicsPipelines
                                        , withGraphicsPipelines
                                        , createComputePipelines
                                        , withComputePipelines
                                        , destroyPipeline
                                        , SpecializationMapEntry(..)
                                        , SpecializationInfo(..)
                                        , PipelineShaderStageCreateInfo(..)
                                        , ComputePipelineCreateInfo(..)
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
                                        ) where

import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import qualified Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Data.Either (Either)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(CFloat))
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import qualified Data.Vector.Storable.Sized (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.CStruct.Extends (forgetExtensions)
import Graphics.Vulkan.CStruct.Utils (lowerArrayPtr)
import Graphics.Vulkan.CStruct.Extends (peekSomeCStruct)
import Graphics.Vulkan.CStruct.Extends (pokeSomeCStruct)
import Graphics.Vulkan.CStruct.Extends (withSomeCStruct)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.Enums.BlendFactor (BlendFactor)
import Graphics.Vulkan.Core10.Enums.BlendOp (BlendOp)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.Enums.ColorComponentFlagBits (ColorComponentFlags)
import Graphics.Vulkan.Core10.Enums.CompareOp (CompareOp)
import Graphics.Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlags)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateComputePipelines))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkCreateGraphicsPipelines))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyPipeline))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.Core10.Enums.DynamicState (DynamicState)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.Core10.Enums.Format (Format)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.FrontFace (FrontFace)
import Graphics.Vulkan.Core10.Enums.LogicOp (LogicOp)
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.Core10.Handles (Pipeline)
import Graphics.Vulkan.Core10.Handles (Pipeline(..))
import Graphics.Vulkan.Core10.Handles (PipelineCache)
import Graphics.Vulkan.Core10.Handles (PipelineCache(..))
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced (PipelineColorBlendAdvancedStateCreateInfoEXT)
import Graphics.Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlags (PipelineColorBlendStateCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_AMD_pipeline_compiler_control (PipelineCompilerControlCreateInfoAMD)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples (PipelineCoverageModulationStateCreateInfoNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_coverage_reduction_mode (PipelineCoverageReductionStateCreateInfoNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color (PipelineCoverageToColorStateCreateInfoNV)
import Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfoEXT)
import Graphics.Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlags (PipelineDepthStencilStateCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles (PipelineDiscardRectangleStateCreateInfoEXT)
import Graphics.Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags (PipelineDynamicStateCreateFlags)
import Graphics.Vulkan.Core10.Enums.PipelineInputAssemblyStateCreateFlags (PipelineInputAssemblyStateCreateFlags)
import Graphics.Vulkan.Core10.Handles (PipelineLayout)
import Graphics.Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags (PipelineMultisampleStateCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization (PipelineRasterizationConservativeStateCreateInfoEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable (PipelineRasterizationDepthClipStateCreateInfoEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_line_rasterization (PipelineRasterizationLineStateCreateInfoEXT)
import Graphics.Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags (PipelineRasterizationStateCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_AMD_rasterization_order (PipelineRasterizationStateRasterizationOrderAMD)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_transform_feedback (PipelineRasterizationStateStreamCreateInfoEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test (PipelineRepresentativeFragmentTestStateCreateInfoNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_sample_locations (PipelineSampleLocationsStateCreateInfoEXT)
import Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control (PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (PipelineTessellationDomainOriginStateCreateInfo)
import Graphics.Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags (PipelineTessellationStateCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PipelineVertexInputDivisorStateCreateInfoEXT)
import Graphics.Vulkan.Core10.Enums.PipelineVertexInputStateCreateFlags (PipelineVertexInputStateCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_shading_rate_image (PipelineViewportCoarseSampleOrderStateCreateInfoNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive (PipelineViewportExclusiveScissorStateCreateInfoNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_shading_rate_image (PipelineViewportShadingRateImageStateCreateInfoNV)
import Graphics.Vulkan.Core10.Enums.PipelineViewportStateCreateFlags (PipelineViewportStateCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle (PipelineViewportSwizzleStateCreateInfoNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling (PipelineViewportWScalingStateCreateInfoNV)
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.PolygonMode (PolygonMode)
import Graphics.Vulkan.Core10.Enums.PrimitiveTopology (PrimitiveTopology)
import Graphics.Vulkan.Core10.CommandBufferBuilding (Rect2D)
import Graphics.Vulkan.Core10.Handles (RenderPass)
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Graphics.Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits(SampleCountFlagBits))
import Graphics.Vulkan.Core10.BaseType (SampleMask)
import Graphics.Vulkan.Core10.Handles (ShaderModule)
import Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits)
import Graphics.Vulkan.CStruct.Extends (SomeStruct)
import Graphics.Vulkan.CStruct.Extends (SomeStruct(..))
import Graphics.Vulkan.Core10.Enums.StencilOp (StencilOp)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Core10.Enums.VertexInputRate (VertexInputRate)
import Graphics.Vulkan.Core10.CommandBufferBuilding (Viewport)
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateGraphicsPipelines
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr (GraphicsPipelineCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr (GraphicsPipelineCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateGraphicsPipelines - Create graphics pipelines
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the graphics pipelines.
--
-- -   @pipelineCache@ is either
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', indicating that
--     pipeline caching is disabled; or the handle of a valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-cache pipeline cache>
--     object, in which case use of that cache is enabled for the duration
--     of the command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is a pointer to an array of
--     'GraphicsPipelineCreateInfo' structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelines@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.Handles.Pipeline' handles in which the
--     resulting graphics pipeline objects are returned.
--
-- = Description
--
-- The 'GraphicsPipelineCreateInfo' structure includes an array of shader
-- create info structures containing all the desired active shader stages,
-- as well as creation info to define all relevant fixed-function stages,
-- and a pipeline layout.
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   If @pipelineCache@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @pipelineCache@
--     /must/ be a valid 'Graphics.Vulkan.Core10.Handles.PipelineCache'
--     handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'GraphicsPipelineCreateInfo' structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Graphics.Vulkan.Core10.Handles.Pipeline' handles
--
-- -   @createInfoCount@ /must/ be greater than @0@
--
-- -   If @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device', 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Handles.Pipeline',
-- 'Graphics.Vulkan.Core10.Handles.PipelineCache'
createGraphicsPipelines :: PokeChain a => Device -> PipelineCache -> ("createInfos" ::: Vector (GraphicsPipelineCreateInfo a)) -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (("pipelines" ::: Vector Pipeline))
createGraphicsPipelines device pipelineCache createInfos allocator = evalContT $ do
  let vkCreateGraphicsPipelines' = mkVkCreateGraphicsPipelines (pVkCreateGraphicsPipelines (deviceCmds (device :: Device)))
  pPCreateInfos <- ContT $ allocaBytesAligned @(GraphicsPipelineCreateInfo _) ((Data.Vector.length (createInfos)) * 144) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCreateInfos `plusPtr` (144 * (i)) :: Ptr (GraphicsPipelineCreateInfo _)) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ vkCreateGraphicsPipelines' (deviceHandle (device)) (pipelineCache) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (pPCreateInfos) pAllocator (pPPipelines)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (pPipelines)

-- | A safe wrapper for 'createGraphicsPipelines' and 'destroyPipeline' using
-- 'bracket'
--
-- The allocated value must not be returned from the provided computation
withGraphicsPipelines :: PokeChain a => Device -> PipelineCache -> Vector (GraphicsPipelineCreateInfo a) -> Maybe AllocationCallbacks -> ((Vector Pipeline) -> IO r) -> IO r
withGraphicsPipelines device pipelineCache pCreateInfos pAllocator =
  bracket
    (createGraphicsPipelines device pipelineCache pCreateInfos pAllocator)
    (\(o0) -> traverse_ (\o0Elem -> destroyPipeline device o0Elem pAllocator) o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateComputePipelines
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr (ComputePipelineCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr (ComputePipelineCreateInfo a) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateComputePipelines - Creates a new compute pipeline object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the compute pipelines.
--
-- -   @pipelineCache@ is either
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', indicating that
--     pipeline caching is disabled; or the handle of a valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-cache pipeline cache>
--     object, in which case use of that cache is enabled for the duration
--     of the command.
--
-- -   @createInfoCount@ is the length of the @pCreateInfos@ and
--     @pPipelines@ arrays.
--
-- -   @pCreateInfos@ is a pointer to an array of
--     'ComputePipelineCreateInfo' structures.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pPipelines@ is a pointer to an array of
--     'Graphics.Vulkan.Core10.Handles.Pipeline' handles in which the
--     resulting compute pipeline objects are returned.
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   If @pipelineCache@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @pipelineCache@
--     /must/ be a valid 'Graphics.Vulkan.Core10.Handles.PipelineCache'
--     handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'ComputePipelineCreateInfo' structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Graphics.Vulkan.Core10.Handles.Pipeline' handles
--
-- -   @createInfoCount@ /must/ be greater than @0@
--
-- -   If @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'ComputePipelineCreateInfo', 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Pipeline',
-- 'Graphics.Vulkan.Core10.Handles.PipelineCache'
createComputePipelines :: PokeChain a => Device -> PipelineCache -> ("createInfos" ::: Vector (ComputePipelineCreateInfo a)) -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (("pipelines" ::: Vector Pipeline))
createComputePipelines device pipelineCache createInfos allocator = evalContT $ do
  let vkCreateComputePipelines' = mkVkCreateComputePipelines (pVkCreateComputePipelines (deviceCmds (device :: Device)))
  pPCreateInfos <- ContT $ allocaBytesAligned @(ComputePipelineCreateInfo _) ((Data.Vector.length (createInfos)) * 96) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPCreateInfos `plusPtr` (96 * (i)) :: Ptr (ComputePipelineCreateInfo _)) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ vkCreateComputePipelines' (deviceHandle (device)) (pipelineCache) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (pPCreateInfos) pAllocator (pPPipelines)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (pPipelines)

-- | A safe wrapper for 'createComputePipelines' and 'destroyPipeline' using
-- 'bracket'
--
-- The allocated value must not be returned from the provided computation
withComputePipelines :: PokeChain a => Device -> PipelineCache -> Vector (ComputePipelineCreateInfo a) -> Maybe AllocationCallbacks -> ((Vector Pipeline) -> IO r) -> IO r
withComputePipelines device pipelineCache pCreateInfos pAllocator =
  bracket
    (createComputePipelines device pipelineCache pCreateInfos pAllocator)
    (\(o0) -> traverse_ (\o0Elem -> destroyPipeline device o0Elem pAllocator) o0)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipeline
  :: FunPtr (Ptr Device_T -> Pipeline -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Pipeline -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyPipeline - Destroy a pipeline object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the pipeline.
--
-- -   @pipeline@ is the handle of the pipeline to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @pipeline@ /must/ have
--     completed execution
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @pipeline@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @pipeline@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Graphics.Vulkan.Core10.Handles.Device'
--     handle
--
-- -   If @pipeline@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE', @pipeline@ /must/
--     be a valid 'Graphics.Vulkan.Core10.Handles.Pipeline' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   If @pipeline@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @pipeline@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Core10.Handles.Pipeline'
destroyPipeline :: Device -> Pipeline -> ("allocator" ::: Maybe AllocationCallbacks) -> IO ()
destroyPipeline device pipeline allocator = evalContT $ do
  let vkDestroyPipeline' = mkVkDestroyPipeline (pVkDestroyPipeline (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyPipeline' (deviceHandle (device)) (pipeline) pAllocator
  pure $ ()


-- | VkSpecializationMapEntry - Structure specifying a specialization map
-- entry
--
-- = Description
--
-- If a @constantID@ value is not a specialization constant ID used in the
-- shader, that map entry does not affect the behavior of the pipeline.
--
-- == Valid Usage
--
-- -   For a @constantID@ specialization constant declared in a shader,
--     @size@ /must/ match the byte size of the @constantID@. If the
--     specialization constant is of type @boolean@, @size@ /must/ be the
--     byte size of 'Graphics.Vulkan.Core10.BaseType.Bool32'
--
-- = See Also
--
-- 'SpecializationInfo'
data SpecializationMapEntry = SpecializationMapEntry
  { -- | @constantID@ is the ID of the specialization constant in SPIR-V.
    constantID :: Word32
  , -- | @offset@ is the byte offset of the specialization constant value within
    -- the supplied data buffer.
    offset :: Word32
  , -- | @size@ is the byte size of the specialization constant value within the
    -- supplied data buffer.
    size :: Word64
  }
  deriving (Typeable)
deriving instance Show SpecializationMapEntry

instance ToCStruct SpecializationMapEntry where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SpecializationMapEntry{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (constantID)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (offset)
    poke ((p `plusPtr` 8 :: Ptr CSize)) (CSize (size))
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr CSize)) (CSize (zero))
    f

instance FromCStruct SpecializationMapEntry where
  peekCStruct p = do
    constantID <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    offset <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    size <- peek @CSize ((p `plusPtr` 8 :: Ptr CSize))
    pure $ SpecializationMapEntry
             constantID offset ((\(CSize a) -> a) size)

instance Storable SpecializationMapEntry where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SpecializationMapEntry where
  zero = SpecializationMapEntry
           zero
           zero
           zero


-- | VkSpecializationInfo - Structure specifying specialization info
--
-- = Description
--
-- @pMapEntries@ is a pointer to a 'SpecializationMapEntry' structure.
--
-- == Valid Usage
--
-- -   The @offset@ member of each element of @pMapEntries@ /must/ be less
--     than @dataSize@
--
-- -   The @size@ member of each element of @pMapEntries@ /must/ be less
--     than or equal to @dataSize@ minus @offset@
--
-- == Valid Usage (Implicit)
--
-- -   If @mapEntryCount@ is not @0@, @pMapEntries@ /must/ be a valid
--     pointer to an array of @mapEntryCount@ valid
--     'SpecializationMapEntry' structures
--
-- -   If @dataSize@ is not @0@, @pData@ /must/ be a valid pointer to an
--     array of @dataSize@ bytes
--
-- = See Also
--
-- 'PipelineShaderStageCreateInfo', 'SpecializationMapEntry'
data SpecializationInfo = SpecializationInfo
  { -- | @pMapEntries@ is a pointer to an array of 'SpecializationMapEntry'
    -- structures which map constant IDs to offsets in @pData@.
    mapEntries :: Vector SpecializationMapEntry
  , -- | @dataSize@ is the byte size of the @pData@ buffer.
    dataSize :: Word64
  , -- | @pData@ contains the actual constant values to specialize with.
    data' :: Ptr ()
  }
  deriving (Typeable)
deriving instance Show SpecializationInfo

instance ToCStruct SpecializationInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SpecializationInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (mapEntries)) :: Word32))
    pPMapEntries' <- ContT $ allocaBytesAligned @SpecializationMapEntry ((Data.Vector.length (mapEntries)) * 16) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPMapEntries' `plusPtr` (16 * (i)) :: Ptr SpecializationMapEntry) (e) . ($ ())) (mapEntries)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr SpecializationMapEntry))) (pPMapEntries')
    lift $ poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (dataSize))
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (data')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    pPMapEntries' <- ContT $ allocaBytesAligned @SpecializationMapEntry ((Data.Vector.length (mempty)) * 16) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPMapEntries' `plusPtr` (16 * (i)) :: Ptr SpecializationMapEntry) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr SpecializationMapEntry))) (pPMapEntries')
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    lift $ f

instance FromCStruct SpecializationInfo where
  peekCStruct p = do
    mapEntryCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pMapEntries <- peek @(Ptr SpecializationMapEntry) ((p `plusPtr` 8 :: Ptr (Ptr SpecializationMapEntry)))
    pMapEntries' <- generateM (fromIntegral mapEntryCount) (\i -> peekCStruct @SpecializationMapEntry ((pMapEntries `advancePtrBytes` (16 * (i)) :: Ptr SpecializationMapEntry)))
    dataSize <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    pData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ SpecializationInfo
             pMapEntries' ((\(CSize a) -> a) dataSize) pData

instance Zero SpecializationInfo where
  zero = SpecializationInfo
           mempty
           zero
           zero


-- | VkPipelineShaderStageCreateInfo - Structure specifying parameters of a
-- newly created pipeline shader stage
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stage@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stage@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shader>
--     feature is not enabled, @stage@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shader>
--     feature is not enabled, @stage@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_NV'
--
-- -   @stage@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL_GRAPHICS',
--     or
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL'
--
-- -   @pName@ /must/ be the name of an @OpEntryPoint@ in @module@ with an
--     execution model that matches @stage@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the @ClipDistance@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxClipDistances@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the @CullDistance@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxCullDistances@
--
-- -   If the identified entry point includes any variables in its
--     interface that are declared with the @ClipDistance@ or
--     @CullDistance@ @BuiltIn@ decoration, those variables /must/ not have
--     array sizes which sum to more than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxCombinedClipAndCullDistances@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the
--     'Graphics.Vulkan.Core10.BaseType.SampleMask' @BuiltIn@ decoration,
--     that variable /must/ not have an array size greater than
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxSampleMaskWords@
--
-- -   If @stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     the identified entry point /must/ not include any input variable in
--     its interface that is decorated with @CullDistance@
--
-- -   If @stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     and the identified entry point has an @OpExecutionMode@ instruction
--     that specifies a patch size with @OutputVertices@, the patch size
--     /must/ be greater than @0@ and less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTessellationPatchSize@
--
-- -   If @stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies a maximum output vertex count that is
--     greater than @0@ and less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxGeometryOutputVertices@
--
-- -   If @stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies an invocation count that is greater than
--     @0@ and less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxGeometryShaderInvocations@
--
-- -   If @stage@ is a vertex processing stage, and the identified entry
--     point writes to @Layer@ for any primitive, it /must/ write the same
--     value to @Layer@ for all vertices of a given primitive
--
-- -   If @stage@ is a vertex processing stage, and the identified entry
--     point writes to @ViewportIndex@ for any primitive, it /must/ write
--     the same value to @ViewportIndex@ for all vertices of a given
--     primitive
--
-- -   If @stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     the identified entry point /must/ not include any output variables
--     in its interface decorated with @CullDistance@
--
-- -   If @stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and the identified entry point writes to @FragDepth@ in any
--     execution path, it /must/ write to @FragDepth@ in all execution
--     paths
--
-- -   If @stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and the identified entry point writes to @FragStencilRefEXT@ in any
--     execution path, it /must/ write to @FragStencilRefEXT@ in all
--     execution paths
--
-- -   If @stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies a maximum output vertex count,
--     @OutputVertices@, that is greater than @0@ and less than or equal to
--     'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderPropertiesNV'::@maxMeshOutputVertices@.
--
-- -   If @stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies a maximum output primitive count,
--     @OutputPrimitivesNV@, that is greater than @0@ and less than or
--     equal to
--     'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderPropertiesNV'::@maxMeshOutputPrimitives@.
--
-- -   If @flags@ has the
--     'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--     flag set, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subgroupSizeControl subgroupSizeControl>
--     feature /must/ be enabled.
--
-- -   If @flags@ has the
--     'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--     flag set, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-computeFullSubgroups computeFullSubgroups>
--     feature /must/ be enabled.
--
-- -   If a
--     'Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--     structure is included in the @pNext@ chain, @flags@ /must/ not have
--     the
--     'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--     flag set.
--
-- -   If a
--     'Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--     structure is included in the @pNext@ chain, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subgroupSizeControl subgroupSizeControl>
--     feature /must/ be enabled, and @stage@ /must/ be a valid bit
--     specified in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-required-subgroup-size-stages requiredSubgroupSizeStages>.
--
-- -   If a
--     'Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--     structure is included in the @pNext@ chain and @stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT',
--     the local workgroup size of the shader /must/ be less than or equal
--     to the product of
--     'Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'::@requiredSubgroupSize@
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-max-subgroups-per-workgroup maxComputeWorkgroupSubgroups>.
--
-- -   If a
--     'Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--     structure is included in the @pNext@ chain, and @flags@ has the
--     'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--     flag set, the local workgroup size in the X dimension of the
--     pipeline /must/ be a multiple of
--     'Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'::@requiredSubgroupSize@.
--
-- -   If @flags@ has both the
--     'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--     and
--     'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--     flags set, the local workgroup size in the X dimension of the
--     pipeline /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-max-subgroup-size maxSubgroupSize>.
--
-- -   If @flags@ has the
--     'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--     flag set and @flags@ does not have the
--     'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--     flag set and no
--     'Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--     structure is included in the @pNext@ chain, the local workgroup size
--     in the X dimension of the pipeline /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PipelineShaderStageCreateFlagBits'
--     values
--
-- -   @stage@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
--     value
--
-- -   @module@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.ShaderModule' handle
--
-- -   @pName@ /must/ be a null-terminated UTF-8 string
--
-- -   If @pSpecializationInfo@ is not @NULL@, @pSpecializationInfo@ /must/
--     be a valid pointer to a valid 'SpecializationInfo' structure
--
-- = See Also
--
-- 'ComputePipelineCreateInfo', 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PipelineShaderStageCreateFlags',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
-- 'Graphics.Vulkan.Core10.Handles.ShaderModule',
-- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits',
-- 'SpecializationInfo',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineShaderStageCreateInfo (es :: [Type]) = PipelineShaderStageCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PipelineShaderStageCreateFlagBits'
    -- specifying how the pipeline shader stage will be generated.
    flags :: PipelineShaderStageCreateFlags
  , -- | @stage@ is a
    -- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits'
    -- value specifying a single pipeline stage.
    stage :: ShaderStageFlagBits
  , -- | @module@ is a 'Graphics.Vulkan.Core10.Handles.ShaderModule' object
    -- containing the shader for this stage.
    module' :: ShaderModule
  , -- | @pName@ is a pointer to a null-terminated UTF-8 string specifying the
    -- entry point name of the shader for this stage.
    name :: ByteString
  , -- | @pSpecializationInfo@ is a pointer to a 'SpecializationInfo' structure,
    -- as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-specialization-constants Specialization Constants>,
    -- or @NULL@.
    specializationInfo :: Maybe SpecializationInfo
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (PipelineShaderStageCreateInfo es)

instance Extensible PipelineShaderStageCreateInfo where
  extensibleType = STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineShaderStageCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineShaderStageCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (PipelineShaderStageCreateInfo es) where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineShaderStageCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineShaderStageCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr ShaderStageFlagBits)) (stage)
    lift $ poke ((p `plusPtr` 24 :: Ptr ShaderModule)) (module')
    pName'' <- ContT $ useAsCString (name)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) pName''
    pSpecializationInfo'' <- case (specializationInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr SpecializationInfo))) pSpecializationInfo''
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr ShaderStageFlagBits)) (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr ShaderModule)) (zero)
    pName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) pName''
    lift $ f

instance PeekChain es => FromCStruct (PipelineShaderStageCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineShaderStageCreateFlags ((p `plusPtr` 16 :: Ptr PipelineShaderStageCreateFlags))
    stage <- peek @ShaderStageFlagBits ((p `plusPtr` 20 :: Ptr ShaderStageFlagBits))
    module' <- peek @ShaderModule ((p `plusPtr` 24 :: Ptr ShaderModule))
    pName <- packCString =<< peek ((p `plusPtr` 32 :: Ptr (Ptr CChar)))
    pSpecializationInfo <- peek @(Ptr SpecializationInfo) ((p `plusPtr` 40 :: Ptr (Ptr SpecializationInfo)))
    pSpecializationInfo' <- maybePeek (\j -> peekCStruct @SpecializationInfo (j)) pSpecializationInfo
    pure $ PipelineShaderStageCreateInfo
             next flags stage module' pName pSpecializationInfo'

instance es ~ '[] => Zero (PipelineShaderStageCreateInfo es) where
  zero = PipelineShaderStageCreateInfo
           ()
           zero
           zero
           zero
           mempty
           Nothing


-- | VkComputePipelineCreateInfo - Structure specifying parameters of a newly
-- created compute pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- == Valid Usage
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be
--     a valid handle to a compute
--     'Graphics.Vulkan.Core10.Handles.Pipeline'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @basePipelineIndex@ /must/ be a valid index into the calling
--     command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not -1, @basePipelineHandle@ /must/
--     be 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @basePipelineIndex@ /must/ be -1
--
-- -   The @stage@ member of @stage@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   The shader code for the entry point identified by @stage@ and the
--     rest of the state identified by this structure /must/ adhere to the
--     pipeline linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   @layout@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with the layout of the compute shader specified in @stage@
--
-- -   The number of resources in @layout@ accessible to the compute shader
--     stage /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_AMD_pipeline_compiler_control.PipelineCompilerControlCreateInfoAMD'
--     or
--     'Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   @stage@ /must/ be a valid 'PipelineShaderStageCreateInfo' structure
--
-- -   @layout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   Both of @basePipelineHandle@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Pipeline',
-- 'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'Graphics.Vulkan.Core10.Handles.PipelineLayout',
-- 'PipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createComputePipelines'
data ComputePipelineCreateInfo (es :: [Type]) = ComputePipelineCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
    -- specifying how the pipeline will be generated.
    flags :: PipelineCreateFlags
  , -- | @stage@ is a 'PipelineShaderStageCreateInfo' structure describing the
    -- compute shader.
    stage :: SomeStruct PipelineShaderStageCreateInfo
  , -- | @layout@ is the description of binding locations used by both the
    -- pipeline and descriptor sets used with the pipeline.
    layout :: PipelineLayout
  , -- | @basePipelineHandle@ is a pipeline to derive from
    basePipelineHandle :: Pipeline
  , -- | @basePipelineIndex@ is an index into the @pCreateInfos@ parameter to use
    -- as a pipeline to derive from
    basePipelineIndex :: Int32
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (ComputePipelineCreateInfo es)

instance Extensible ComputePipelineCreateInfo where
  extensibleType = STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
  setNext x next = x{next = next}
  getNext ComputePipelineCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ComputePipelineCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineCompilerControlCreateInfoAMD = Just f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfoEXT = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (ComputePipelineCreateInfo es) where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ComputePipelineCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags)) (flags)
    ContT $ pokeSomeCStruct (forgetExtensions ((p `plusPtr` 24 :: Ptr (PipelineShaderStageCreateInfo _)))) (stage) . ($ ())
    lift $ poke ((p `plusPtr` 72 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 80 :: Ptr Pipeline)) (basePipelineHandle)
    lift $ poke ((p `plusPtr` 88 :: Ptr Int32)) (basePipelineIndex)
    lift $ f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    ContT $ pokeSomeCStruct (forgetExtensions ((p `plusPtr` 24 :: Ptr (PipelineShaderStageCreateInfo _)))) ((SomeStruct zero)) . ($ ())
    lift $ poke ((p `plusPtr` 72 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 88 :: Ptr Int32)) (zero)
    lift $ f

instance PeekChain es => FromCStruct (ComputePipelineCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCreateFlags))
    stage <- peekSomeCStruct (forgetExtensions ((p `plusPtr` 24 :: Ptr (PipelineShaderStageCreateInfo a))))
    layout <- peek @PipelineLayout ((p `plusPtr` 72 :: Ptr PipelineLayout))
    basePipelineHandle <- peek @Pipeline ((p `plusPtr` 80 :: Ptr Pipeline))
    basePipelineIndex <- peek @Int32 ((p `plusPtr` 88 :: Ptr Int32))
    pure $ ComputePipelineCreateInfo
             next flags stage layout basePipelineHandle basePipelineIndex

instance es ~ '[] => Zero (ComputePipelineCreateInfo es) where
  zero = ComputePipelineCreateInfo
           ()
           zero
           (SomeStruct zero)
           zero
           zero
           zero


-- | VkVertexInputBindingDescription - Structure specifying vertex input
-- binding description
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'PipelineVertexInputStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.VertexInputRate.VertexInputRate'
data VertexInputBindingDescription = VertexInputBindingDescription
  { -- | @binding@ /must/ be less than
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
    binding :: Word32
  , -- | @stride@ /must/ be less than or equal to
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindingStride@
    stride :: Word32
  , -- | @inputRate@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.VertexInputRate.VertexInputRate' value
    inputRate :: VertexInputRate
  }
  deriving (Typeable)
deriving instance Show VertexInputBindingDescription

instance ToCStruct VertexInputBindingDescription where
  withCStruct x f = allocaBytesAligned 12 4 $ \p -> pokeCStruct p x (f p)
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.Format.Format',
-- 'PipelineVertexInputStateCreateInfo'
data VertexInputAttributeDescription = VertexInputAttributeDescription
  { -- | @location@ /must/ be less than
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributes@
    location :: Word32
  , -- | @binding@ /must/ be less than
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
    binding :: Word32
  , -- | @format@ /must/ be a valid 'Graphics.Vulkan.Core10.Enums.Format.Format'
    -- value
    format :: Format
  , -- | @offset@ /must/ be less than or equal to
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributeOffset@
    offset :: Word32
  }
  deriving (Typeable)
deriving instance Show VertexInputAttributeDescription

instance ToCStruct VertexInputAttributeDescription where
  withCStruct x f = allocaBytesAligned 16 4 $ \p -> pokeCStruct p x (f p)
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
-- -   @vertexBindingDescriptionCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   @vertexAttributeDescriptionCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributes@
--
-- -   For every @binding@ specified by each element of
--     @pVertexAttributeDescriptions@, a 'VertexInputBindingDescription'
--     /must/ exist in @pVertexBindingDescriptions@ with the same value of
--     @binding@
--
-- -   All elements of @pVertexBindingDescriptions@ /must/ describe
--     distinct binding numbers
--
-- -   All elements of @pVertexAttributeDescriptions@ /must/ describe
--     distinct attribute locations
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PipelineVertexInputDivisorStateCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- -   If @vertexBindingDescriptionCount@ is not @0@,
--     @pVertexBindingDescriptions@ /must/ be a valid pointer to an array
--     of @vertexBindingDescriptionCount@ valid
--     'VertexInputBindingDescription' structures
--
-- -   If @vertexAttributeDescriptionCount@ is not @0@,
--     @pVertexAttributeDescriptions@ /must/ be a valid pointer to an array
--     of @vertexAttributeDescriptionCount@ valid
--     'VertexInputAttributeDescription' structures
--
-- = See Also
--
-- 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.PipelineVertexInputStateCreateFlags.PipelineVertexInputStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'VertexInputAttributeDescription', 'VertexInputBindingDescription'
data PipelineVertexInputStateCreateInfo (es :: [Type]) = PipelineVertexInputStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
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
deriving instance Show (Chain es) => Show (PipelineVertexInputStateCreateInfo es)

instance Extensible PipelineVertexInputStateCreateInfo where
  extensibleType = STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineVertexInputStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineVertexInputStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineVertexInputDivisorStateCreateInfoEXT = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (PipelineVertexInputStateCreateInfo es) where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineVertexInputStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineVertexInputStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (vertexBindingDescriptions)) :: Word32))
    pPVertexBindingDescriptions' <- ContT $ allocaBytesAligned @VertexInputBindingDescription ((Data.Vector.length (vertexBindingDescriptions)) * 12) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPVertexBindingDescriptions' `plusPtr` (12 * (i)) :: Ptr VertexInputBindingDescription) (e) . ($ ())) (vertexBindingDescriptions)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDescription))) (pPVertexBindingDescriptions')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (vertexAttributeDescriptions)) :: Word32))
    pPVertexAttributeDescriptions' <- ContT $ allocaBytesAligned @VertexInputAttributeDescription ((Data.Vector.length (vertexAttributeDescriptions)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPVertexAttributeDescriptions' `plusPtr` (16 * (i)) :: Ptr VertexInputAttributeDescription) (e) . ($ ())) (vertexAttributeDescriptions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr VertexInputAttributeDescription))) (pPVertexAttributeDescriptions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPVertexBindingDescriptions' <- ContT $ allocaBytesAligned @VertexInputBindingDescription ((Data.Vector.length (mempty)) * 12) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPVertexBindingDescriptions' `plusPtr` (12 * (i)) :: Ptr VertexInputBindingDescription) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDescription))) (pPVertexBindingDescriptions')
    pPVertexAttributeDescriptions' <- ContT $ allocaBytesAligned @VertexInputAttributeDescription ((Data.Vector.length (mempty)) * 16) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPVertexAttributeDescriptions' `plusPtr` (16 * (i)) :: Ptr VertexInputAttributeDescription) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr VertexInputAttributeDescription))) (pPVertexAttributeDescriptions')
    lift $ f

instance PeekChain es => FromCStruct (PipelineVertexInputStateCreateInfo es) where
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
             next flags pVertexBindingDescriptions' pVertexAttributeDescriptions'

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
-- -   If @topology@ is
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_POINT_LIST',
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST',
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST',
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY',
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY'
--     or
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST',
--     @primitiveRestartEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @topology@ /must/ not be any of
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY',
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY',
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY'
--     or
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @topology@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @topology@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology'
--     value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32', 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.PipelineInputAssemblyStateCreateFlags.PipelineInputAssemblyStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineInputAssemblyStateCreateInfo = PipelineInputAssemblyStateCreateInfo
  { -- | @flags@ is reserved for future use.
    flags :: PipelineInputAssemblyStateCreateFlags
  , -- | @topology@ is a
    -- 'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology'
    -- defining the primitive topology, as described below.
    topology :: PrimitiveTopology
  , -- | @primitiveRestartEnable@ controls whether a special vertex index value
    -- is treated as restarting the assembly of primitives. This enable only
    -- applies to indexed draws
    -- ('Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexed' and
    -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect'),
    -- and the special index value is either 0xFFFFFFFF when the @indexType@
    -- parameter of
    -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer' is
    -- equal to 'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32',
    -- 0xFF when @indexType@ is equal to
    -- 'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8_EXT', or 0xFFFF
    -- when @indexType@ is equal to
    -- 'Graphics.Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16'. Primitive
    -- restart is not allowed for “list” topologies.
    primitiveRestartEnable :: Bool
  }
  deriving (Typeable)
deriving instance Show PipelineInputAssemblyStateCreateInfo

instance ToCStruct PipelineInputAssemblyStateCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
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
-- -   @patchControlPoints@ /must/ be greater than zero and less than or
--     equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTessellationPatchSize@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PipelineTessellationDomainOriginStateCreateInfo'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags.PipelineTessellationStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineTessellationStateCreateInfo (es :: [Type]) = PipelineTessellationStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineTessellationStateCreateFlags
  , -- | @patchControlPoints@ number of control points per patch.
    patchControlPoints :: Word32
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (PipelineTessellationStateCreateInfo es)

instance Extensible PipelineTessellationStateCreateInfo where
  extensibleType = STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineTessellationStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineTessellationStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineTessellationDomainOriginStateCreateInfo = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (PipelineTessellationStateCreateInfo es) where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
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

instance PeekChain es => FromCStruct (PipelineTessellationStateCreateInfo es) where
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
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @scissorCount@ /must/ be @1@
--
-- -   @viewportCount@ /must/ be between @1@ and
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   @scissorCount@ /must/ be between @1@ and
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   @scissorCount@ and @viewportCount@ /must/ be identical
--
-- -   The @x@ and @y@ members of @offset@ member of any element of
--     @pScissors@ /must/ be greater than or equal to @0@
--
-- -   Evaluation of (@offset.x@ + @extent.width@) /must/ not cause a
--     signed integer addition overflow for any element of @pScissors@
--
-- -   Evaluation of (@offset.y@ + @extent.height@) /must/ not cause a
--     signed integer addition overflow for any element of @pScissors@
--
-- -   If the @viewportWScalingEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--     structure included in the @pNext@ chain is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', the @viewportCount@ member
--     of the
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--     structure /must/ be equal to @viewportCount@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportCoarseSampleOrderStateCreateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV',
--     or
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- -   @scissorCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.PipelineViewportStateCreateFlags.PipelineViewportStateCreateFlags',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Viewport'
data PipelineViewportStateCreateInfo (es :: [Type]) = PipelineViewportStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineViewportStateCreateFlags
  , -- | @pViewports@ is a pointer to an array of
    -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Viewport' structures,
    -- defining the viewport transforms. If the viewport state is dynamic, this
    -- member is ignored.
    viewports :: Either Word32 (Vector Viewport)
  , -- | @pScissors@ is a pointer to an array of
    -- 'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D' structures
    -- defining the rectangular bounds of the scissor for the corresponding
    -- viewport. If the scissor state is dynamic, this member is ignored.
    scissors :: Either Word32 (Vector Rect2D)
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (PipelineViewportStateCreateInfo es)

instance Extensible PipelineViewportStateCreateInfo where
  extensibleType = STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineViewportStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineViewportStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineViewportCoarseSampleOrderStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineViewportShadingRateImageStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineViewportExclusiveScissorStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineViewportSwizzleStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineViewportWScalingStateCreateInfoNV = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (PipelineViewportStateCreateInfo es) where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineViewportStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineViewportStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (either id (fromIntegral . Data.Vector.length) (viewports)) :: Word32))
    pViewports'' <- case (viewports) of
      Left _ -> pure nullPtr
      Right v -> do
        pPViewports' <- ContT $ allocaBytesAligned @Viewport ((Data.Vector.length (v)) * 24) 4
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPViewports' `plusPtr` (24 * (i)) :: Ptr Viewport) (e) . ($ ())) (v)
        pure $ pPViewports'
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Viewport))) pViewports''
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (either id (fromIntegral . Data.Vector.length) (scissors)) :: Word32))
    pScissors'' <- case (scissors) of
      Left _ -> pure nullPtr
      Right v -> do
        pPScissors' <- ContT $ allocaBytesAligned @Rect2D ((Data.Vector.length (v)) * 16) 4
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPScissors' `plusPtr` (16 * (i)) :: Ptr Rect2D) (e) . ($ ())) (v)
        pure $ pPScissors'
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr Rect2D))) pScissors''
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance PeekChain es => FromCStruct (PipelineViewportStateCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineViewportStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineViewportStateCreateFlags))
    viewportCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pViewports <- peek @(Ptr Viewport) ((p `plusPtr` 24 :: Ptr (Ptr Viewport)))
    pViewports' <- maybePeek (\j -> generateM (fromIntegral viewportCount) (\i -> peekCStruct @Viewport (((j) `advancePtrBytes` (24 * (i)) :: Ptr Viewport)))) pViewports
    let pViewports'' = maybe (Left viewportCount) Right pViewports'
    scissorCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pScissors <- peek @(Ptr Rect2D) ((p `plusPtr` 40 :: Ptr (Ptr Rect2D)))
    pScissors' <- maybePeek (\j -> generateM (fromIntegral scissorCount) (\i -> peekCStruct @Rect2D (((j) `advancePtrBytes` (16 * (i)) :: Ptr Rect2D)))) pScissors
    let pScissors'' = maybe (Left scissorCount) Right pScissors'
    pure $ PipelineViewportStateCreateInfo
             next flags pViewports'' pScissors''

instance es ~ '[] => Zero (PipelineViewportStateCreateInfo es) where
  zero = PipelineViewportStateCreateInfo
           ()
           zero
           (Left 0)
           (Left 0)


-- | VkPipelineRasterizationStateCreateInfo - Structure specifying parameters
-- of a newly created pipeline rasterization state
--
-- = Description
--
-- The application /can/ also add a
-- 'Graphics.Vulkan.Extensions.VK_AMD_rasterization_order.PipelineRasterizationStateRasterizationOrderAMD'
-- structure to the @pNext@ chain of a
-- 'PipelineRasterizationStateCreateInfo' structure. This structure enables
-- selecting the rasterization order to use when rendering with the
-- corresponding graphics pipeline as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primrast-order Rasterization Order>.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthClamp depth clamping>
--     feature is not enabled, @depthClampEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fillModeNonSolid non-solid fill modes>
--     feature is not enabled, @polygonMode@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL' or
--     'Graphics.Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- -   If the
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_NV_fill_rectangle@
--     extension is not enabled, @polygonMode@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization.PipelineRasterizationConservativeStateCreateInfoEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT',
--     'Graphics.Vulkan.Extensions.VK_AMD_rasterization_order.PipelineRasterizationStateRasterizationOrderAMD',
--     or
--     'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @polygonMode@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.PolygonMode.PolygonMode' value
--
-- -   @cullMode@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlagBits'
--     values
--
-- -   @frontFace@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.FrontFace.FrontFace' value
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlags',
-- 'Graphics.Vulkan.Core10.Enums.FrontFace.FrontFace',
-- 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags.PipelineRasterizationStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.PolygonMode.PolygonMode',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineRasterizationStateCreateInfo (es :: [Type]) = PipelineRasterizationStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineRasterizationStateCreateFlags
  , -- | @depthClampEnable@ controls whether to clamp the fragment’s depth values
    -- as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth Depth Test>.
    -- If the pipeline is not created with
    -- 'Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT'
    -- present then enabling depth clamp will also disable clipping primitives
    -- to the z planes of the frustrum as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>.
    -- Otherwise depth clipping is controlled by the state set in
    -- 'Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT'.
    depthClampEnable :: Bool
  , -- | @rasterizerDiscardEnable@ controls whether primitives are discarded
    -- immediately before the rasterization stage.
    rasterizerDiscardEnable :: Bool
  , -- | @polygonMode@ is the triangle rendering mode. See
    -- 'Graphics.Vulkan.Core10.Enums.PolygonMode.PolygonMode'.
    polygonMode :: PolygonMode
  , -- | @cullMode@ is the triangle facing direction used for primitive culling.
    -- See 'Graphics.Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlagBits'.
    cullMode :: CullModeFlags
  , -- | @frontFace@ is a 'Graphics.Vulkan.Core10.Enums.FrontFace.FrontFace'
    -- value specifying the front-facing triangle orientation to be used for
    -- culling.
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
deriving instance Show (Chain es) => Show (PipelineRasterizationStateCreateInfo es)

instance Extensible PipelineRasterizationStateCreateInfo where
  extensibleType = STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineRasterizationStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineRasterizationStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineRasterizationLineStateCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineRasterizationDepthClipStateCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineRasterizationStateStreamCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineRasterizationConservativeStateCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineRasterizationStateRasterizationOrderAMD = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (PipelineRasterizationStateCreateInfo es) where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
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

instance PeekChain es => FromCStruct (PipelineRasterizationStateCreateInfo es) where
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
             next flags (bool32ToBool depthClampEnable) (bool32ToBool rasterizerDiscardEnable) polygonMode cullMode frontFace (bool32ToBool depthBiasEnable) ((\(CFloat a) -> a) depthBiasConstantFactor) ((\(CFloat a) -> a) depthBiasClamp) ((\(CFloat a) -> a) depthBiasSlopeFactor) ((\(CFloat a) -> a) lineWidth)

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
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sampleRateShading sample rate shading>
--     feature is not enabled, @sampleShadingEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-alphaToOne alpha to one>
--     feature is not enabled, @alphaToOneEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   @minSampleShading@ /must/ be in the range [0,1]
--
-- -   If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and
--     if the subpass has any color attachments and @rasterizationSamples@
--     is greater than the number of color samples, then
--     @sampleShadingEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateInfoNV',
--     'Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color.PipelineCoverageToColorStateCreateInfoNV',
--     or
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @rasterizationSamples@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
--     value
--
-- -   If @pSampleMask@ is not @NULL@, @pSampleMask@ /must/ be a valid
--     pointer to an array of
--     \(\lceil{\mathit{rasterizationSamples} \over 32}\rceil\)
--     'Graphics.Vulkan.Core10.BaseType.SampleMask' values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32', 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags.PipelineMultisampleStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits',
-- 'Graphics.Vulkan.Core10.BaseType.SampleMask',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineMultisampleStateCreateInfo (es :: [Type]) = PipelineMultisampleStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineMultisampleStateCreateFlags
  , -- | @rasterizationSamples@ is a
    -- 'Graphics.Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits'
    -- specifying the number of samples used in rasterization.
    rasterizationSamples :: SampleCountFlagBits
  , -- | @sampleShadingEnable@ /can/ be used to enable
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-sampleshading Sample Shading>.
    sampleShadingEnable :: Bool
  , -- | @minSampleShading@ specifies a minimum fraction of sample shading if
    -- @sampleShadingEnable@ is set to 'Graphics.Vulkan.Core10.BaseType.TRUE'.
    minSampleShading :: Float
  , -- | @pSampleMask@ is a bitmask of static coverage information that is ANDed
    -- with the coverage information generated during rasterization, as
    -- described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-samplemask Sample Mask>.
    sampleMask :: Vector SampleMask
  , -- | @alphaToCoverageEnable@ controls whether a temporary coverage value is
    -- generated based on the alpha component of the fragment’s first color
    -- output as specified in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-covg Multisample Coverage>
    -- section.
    alphaToCoverageEnable :: Bool
  , -- | @alphaToOneEnable@ controls whether the alpha component of the
    -- fragment’s first color output is replaced with one as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-covg Multisample Coverage>.
    alphaToOneEnable :: Bool
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (PipelineMultisampleStateCreateInfo es)

instance Extensible PipelineMultisampleStateCreateInfo where
  extensibleType = STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineMultisampleStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineMultisampleStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineCoverageReductionStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineCoverageModulationStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineSampleLocationsStateCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineCoverageToColorStateCreateInfoNV = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (PipelineMultisampleStateCreateInfo es) where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
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
          pPSampleMask' <- ContT $ allocaBytesAligned @SampleMask ((Data.Vector.length ((sampleMask))) * 4) 4
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

instance PeekChain es => FromCStruct (PipelineMultisampleStateCreateInfo es) where
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
             next flags rasterizationSamples (bool32ToBool sampleShadingEnable) ((\(CFloat a) -> a) minSampleShading) pSampleMask' (bool32ToBool alphaToCoverageEnable) (bool32ToBool alphaToOneEnable)

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
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @srcColorBlendFactor@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA',
--     or
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @dstColorBlendFactor@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA',
--     or
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @srcAlphaBlendFactor@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA',
--     or
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @dstAlphaBlendFactor@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA',
--     or
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If either of @colorBlendOp@ or @alphaBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @colorBlendOp@ /must/ equal @alphaBlendOp@
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendIndependentBlend@
--     is 'Graphics.Vulkan.Core10.BaseType.FALSE' and @colorBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @colorBlendOp@ /must/ be the same for all attachments.
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendIndependentBlend@
--     is 'Graphics.Vulkan.Core10.BaseType.FALSE' and @alphaBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @alphaBlendOp@ /must/ be the same for all attachments.
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendAllOperations@
--     is 'Graphics.Vulkan.Core10.BaseType.FALSE', then @colorBlendOp@
--     /must/ not be
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_ZERO_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_OVER_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_OVER_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_IN_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_IN_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_OUT_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_OUT_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_SRC_ATOP_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_DST_ATOP_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_XOR_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_RGB_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARDODGE_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARBURN_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_VIVIDLIGHT_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_LINEARLIGHT_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_PINLIGHT_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_HARDMIX_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_CLAMPED_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_CLAMPED_ALPHA_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_PLUS_DARKER_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_MINUS_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_MINUS_CLAMPED_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_CONTRAST_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_INVERT_OVG_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_RED_EXT',
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_GREEN_EXT', or
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BLEND_OP_BLUE_EXT'
--
-- -   If @colorBlendOp@ or @alphaBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then
--     'Graphics.Vulkan.Core10.Pass.SubpassDescription'::@colorAttachmentCount@
--     of the subpass this pipeline is compiled against /must/ be less than
--     or equal to
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::advancedBlendMaxColorAttachments
--
-- == Valid Usage (Implicit)
--
-- -   @srcColorBlendFactor@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   @dstColorBlendFactor@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   @colorBlendOp@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BlendOp' value
--
-- -   @srcAlphaBlendFactor@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   @dstAlphaBlendFactor@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   @alphaBlendOp@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.BlendOp.BlendOp' value
--
-- -   @colorWriteMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.BlendFactor.BlendFactor',
-- 'Graphics.Vulkan.Core10.Enums.BlendOp.BlendOp',
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlags',
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
  , -- | @alphaBlendOp@ selects which blend operation is use to calculate the
    -- alpha values to write to the color attachment.
    alphaBlendOp :: BlendOp
  , -- | @colorWriteMask@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits'
    -- specifying which of the R, G, B, and\/or A components are enabled for
    -- writing, as described for the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-color-write-mask Color Write Mask>.
    colorWriteMask :: ColorComponentFlags
  }
  deriving (Typeable)
deriving instance Show PipelineColorBlendAttachmentState

instance ToCStruct PipelineColorBlendAttachmentState where
  withCStruct x f = allocaBytesAligned 32 4 $ \p -> pokeCStruct p x (f p)
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
             (bool32ToBool blendEnable) srcColorBlendFactor dstColorBlendFactor colorBlendOp srcAlphaBlendFactor dstAlphaBlendFactor alphaBlendOp colorWriteMask

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
-- = Description
--
-- Each element of the @pAttachments@ array is a
-- 'PipelineColorBlendAttachmentState' structure specifying per-target
-- blending state for each individual color attachment. If the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-independentBlend independent blending>
-- feature is not enabled on the device, all
-- 'PipelineColorBlendAttachmentState' elements in the @pAttachments@ array
-- /must/ be identical.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-independentBlend independent blending>
--     feature is not enabled, all elements of @pAttachments@ /must/ be
--     identical
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-logicOp logic operations>
--     feature is not enabled, @logicOpEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If @logicOpEnable@ is 'Graphics.Vulkan.Core10.BaseType.TRUE',
--     @logicOp@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.LogicOp.LogicOp' value
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.PipelineColorBlendAdvancedStateCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- -   If @attachmentCount@ is not @0@, @pAttachments@ /must/ be a valid
--     pointer to an array of @attachmentCount@ valid
--     'PipelineColorBlendAttachmentState' structures
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32', 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.LogicOp.LogicOp',
-- 'PipelineColorBlendAttachmentState',
-- 'Graphics.Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlags.PipelineColorBlendStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineColorBlendStateCreateInfo (es :: [Type]) = PipelineColorBlendStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineColorBlendStateCreateFlags
  , -- | @logicOpEnable@ controls whether to apply
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-logicop Logical Operations>.
    logicOpEnable :: Bool
  , -- | @logicOp@ selects which logical operation to apply.
    logicOp :: LogicOp
  , -- | @pAttachments@: is a pointer to an array of per target attachment
    -- states.
    attachments :: Vector PipelineColorBlendAttachmentState
  , -- | @blendConstants@ is a pointer to an array of four values used as the R,
    -- G, B, and A components of the blend constant that are used in blending,
    -- depending on the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blendfactors blend factor>.
    blendConstants :: (Float, Float, Float, Float)
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (PipelineColorBlendStateCreateInfo es)

instance Extensible PipelineColorBlendStateCreateInfo where
  extensibleType = STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineColorBlendStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineColorBlendStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineColorBlendAdvancedStateCreateInfoEXT = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (PipelineColorBlendStateCreateInfo es) where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineColorBlendStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineColorBlendStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (logicOpEnable))
    lift $ poke ((p `plusPtr` 24 :: Ptr LogicOp)) (logicOp)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (attachments)) :: Word32))
    pPAttachments' <- ContT $ allocaBytesAligned @PipelineColorBlendAttachmentState ((Data.Vector.length (attachments)) * 32) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachments' `plusPtr` (32 * (i)) :: Ptr PipelineColorBlendAttachmentState) (e) . ($ ())) (attachments)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineColorBlendAttachmentState))) (pPAttachments')
    let pBlendConstants' = lowerArrayPtr ((p `plusPtr` 40 :: Ptr (Data.Vector.Storable.Sized.Vector 4 CFloat)))
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
    pPAttachments' <- ContT $ allocaBytesAligned @PipelineColorBlendAttachmentState ((Data.Vector.length (mempty)) * 32) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachments' `plusPtr` (32 * (i)) :: Ptr PipelineColorBlendAttachmentState) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineColorBlendAttachmentState))) (pPAttachments')
    let pBlendConstants' = lowerArrayPtr ((p `plusPtr` 40 :: Ptr (Data.Vector.Storable.Sized.Vector 4 CFloat)))
    lift $ case ((zero, zero, zero, zero)) of
      (e0, e1, e2, e3) -> do
        poke (pBlendConstants' :: Ptr CFloat) (CFloat (e0))
        poke (pBlendConstants' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pBlendConstants' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pBlendConstants' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    lift $ f

instance PeekChain es => FromCStruct (PipelineColorBlendStateCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineColorBlendStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineColorBlendStateCreateFlags))
    logicOpEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    logicOp <- peek @LogicOp ((p `plusPtr` 24 :: Ptr LogicOp))
    attachmentCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pAttachments <- peek @(Ptr PipelineColorBlendAttachmentState) ((p `plusPtr` 32 :: Ptr (Ptr PipelineColorBlendAttachmentState)))
    pAttachments' <- generateM (fromIntegral attachmentCount) (\i -> peekCStruct @PipelineColorBlendAttachmentState ((pAttachments `advancePtrBytes` (32 * (i)) :: Ptr PipelineColorBlendAttachmentState)))
    let pblendConstants = lowerArrayPtr @CFloat ((p `plusPtr` 40 :: Ptr (Data.Vector.Storable.Sized.Vector 4 CFloat)))
    blendConstants0 <- peek @CFloat ((pblendConstants `advancePtrBytes` 0 :: Ptr CFloat))
    blendConstants1 <- peek @CFloat ((pblendConstants `advancePtrBytes` 4 :: Ptr CFloat))
    blendConstants2 <- peek @CFloat ((pblendConstants `advancePtrBytes` 8 :: Ptr CFloat))
    blendConstants3 <- peek @CFloat ((pblendConstants `advancePtrBytes` 12 :: Ptr CFloat))
    pure $ PipelineColorBlendStateCreateInfo
             next flags (bool32ToBool logicOpEnable) logicOp pAttachments' ((((\(CFloat a) -> a) blendConstants0), ((\(CFloat a) -> a) blendConstants1), ((\(CFloat a) -> a) blendConstants2), ((\(CFloat a) -> a) blendConstants3)))

instance es ~ '[] => Zero (PipelineColorBlendStateCreateInfo es) where
  zero = PipelineColorBlendStateCreateInfo
           ()
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
-- -   Each element of @pDynamicStates@ /must/ be unique
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   If @dynamicStateCount@ is not @0@, @pDynamicStates@ /must/ be a
--     valid pointer to an array of @dynamicStateCount@ valid
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DynamicState' values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.DynamicState.DynamicState',
-- 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags.PipelineDynamicStateCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineDynamicStateCreateInfo = PipelineDynamicStateCreateInfo
  { -- | @flags@ is reserved for future use.
    flags :: PipelineDynamicStateCreateFlags
  , -- | @pDynamicStates@ is a pointer to an array of
    -- 'Graphics.Vulkan.Core10.Enums.DynamicState.DynamicState' values
    -- specifying which pieces of pipeline state will use the values from
    -- dynamic state commands rather than from pipeline state creation info.
    dynamicStates :: Vector DynamicState
  }
  deriving (Typeable)
deriving instance Show PipelineDynamicStateCreateInfo

instance ToCStruct PipelineDynamicStateCreateInfo where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineDynamicStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineDynamicStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (dynamicStates)) :: Word32))
    pPDynamicStates' <- ContT $ allocaBytesAligned @DynamicState ((Data.Vector.length (dynamicStates)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDynamicStates' `plusPtr` (4 * (i)) :: Ptr DynamicState) (e)) (dynamicStates)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DynamicState))) (pPDynamicStates')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pPDynamicStates' <- ContT $ allocaBytesAligned @DynamicState ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDynamicStates' `plusPtr` (4 * (i)) :: Ptr DynamicState) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr DynamicState))) (pPDynamicStates')
    lift $ f

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
-- 'Graphics.Vulkan.Core10.Enums.CompareOp.CompareOp',
-- 'PipelineDepthStencilStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.StencilOp.StencilOp'
data StencilOpState = StencilOpState
  { -- | @failOp@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.StencilOp.StencilOp' value
    failOp :: StencilOp
  , -- | @passOp@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.StencilOp.StencilOp' value
    passOp :: StencilOp
  , -- | @depthFailOp@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.StencilOp.StencilOp' value
    depthFailOp :: StencilOp
  , -- | @compareOp@ /must/ be a valid
    -- 'Graphics.Vulkan.Core10.Enums.CompareOp.CompareOp' value
    compareOp :: CompareOp
  , -- | @compareMask@ selects the bits of the unsigned integer stencil values
    -- participating in the stencil test.
    compareMask :: Word32
  , -- | @writeMask@ selects the bits of the unsigned integer stencil values
    -- updated by the stencil test in the stencil framebuffer attachment.
    writeMask :: Word32
  , -- | @reference@ is an integer reference value that is used in the unsigned
    -- stencil comparison.
    reference :: Word32
  }
  deriving (Typeable)
deriving instance Show StencilOpState

instance ToCStruct StencilOpState where
  withCStruct x f = allocaBytesAligned 28 4 $ \p -> pokeCStruct p x (f p)
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
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthBounds depth bounds testing>
--     feature is not enabled, @depthBoundsTestEnable@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @depthCompareOp@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   @front@ /must/ be a valid 'StencilOpState' structure
--
-- -   @back@ /must/ be a valid 'StencilOpState' structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.CompareOp.CompareOp',
-- 'GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlags.PipelineDepthStencilStateCreateFlags',
-- 'StencilOpState',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineDepthStencilStateCreateInfo = PipelineDepthStencilStateCreateInfo
  { -- | @flags@ is reserved for future use.
    flags :: PipelineDepthStencilStateCreateFlags
  , -- | @depthTestEnable@ controls whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth depth testing>
    -- is enabled.
    depthTestEnable :: Bool
  , -- | @depthWriteEnable@ controls whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth-write depth writes>
    -- are enabled when @depthTestEnable@ is
    -- 'Graphics.Vulkan.Core10.BaseType.TRUE'. Depth writes are always disabled
    -- when @depthTestEnable@ is 'Graphics.Vulkan.Core10.BaseType.FALSE'.
    depthWriteEnable :: Bool
  , -- | @depthCompareOp@ is the comparison operator used in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth depth test>.
    depthCompareOp :: CompareOp
  , -- | @depthBoundsTestEnable@ controls whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-dbt depth bounds testing>
    -- is enabled.
    depthBoundsTestEnable :: Bool
  , -- | @stencilTestEnable@ controls whether
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil stencil testing>
    -- is enabled.
    stencilTestEnable :: Bool
  , -- | @front@ and @back@ control the parameters of the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-stencil stencil test>.
    front :: StencilOpState
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "back"
    back :: StencilOpState
  , -- | @minDepthBounds@ and @maxDepthBounds@ define the range of values used in
    -- the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-dbt depth bounds test>.
    minDepthBounds :: Float
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "maxDepthBounds"
    maxDepthBounds :: Float
  }
  deriving (Typeable)
deriving instance Show PipelineDepthStencilStateCreateInfo

instance ToCStruct PipelineDepthStencilStateCreateInfo where
  withCStruct x f = allocaBytesAligned 104 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineDepthStencilStateCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineDepthStencilStateCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (depthTestEnable))
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (depthWriteEnable))
    lift $ poke ((p `plusPtr` 28 :: Ptr CompareOp)) (depthCompareOp)
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (depthBoundsTestEnable))
    lift $ poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (stencilTestEnable))
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr StencilOpState)) (front) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 68 :: Ptr StencilOpState)) (back) . ($ ())
    lift $ poke ((p `plusPtr` 96 :: Ptr CFloat)) (CFloat (minDepthBounds))
    lift $ poke ((p `plusPtr` 100 :: Ptr CFloat)) (CFloat (maxDepthBounds))
    lift $ f
  cStructSize = 104
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 28 :: Ptr CompareOp)) (zero)
    lift $ poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    lift $ poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    ContT $ pokeCStruct ((p `plusPtr` 40 :: Ptr StencilOpState)) (zero) . ($ ())
    ContT $ pokeCStruct ((p `plusPtr` 68 :: Ptr StencilOpState)) (zero) . ($ ())
    lift $ poke ((p `plusPtr` 96 :: Ptr CFloat)) (CFloat (zero))
    lift $ poke ((p `plusPtr` 100 :: Ptr CFloat)) (CFloat (zero))
    lift $ f

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
             flags (bool32ToBool depthTestEnable) (bool32ToBool depthWriteEnable) depthCompareOp (bool32ToBool depthBoundsTestEnable) (bool32ToBool stencilTestEnable) front back ((\(CFloat a) -> a) minDepthBounds) ((\(CFloat a) -> a) maxDepthBounds)

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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- If any shader stage fails to compile, the compile log will be reported
-- back to the application, and
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV' will be
-- generated.
--
-- == Valid Usage
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be
--     a valid handle to a graphics
--     'Graphics.Vulkan.Core10.Handles.Pipeline'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @basePipelineIndex@ /must/ be a valid index into the calling
--     command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not -1, @basePipelineHandle@ /must/
--     be 'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @flags@ contains the
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Graphics.Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @basePipelineIndex@ /must/ be -1
--
-- -   The @stage@ member of each element of @pStages@ /must/ be unique
--
-- -   The geometric shader stages provided in @pStages@ /must/ be either
--     from the mesh shading pipeline (@stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_NV'
--     or
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV')
--     or from the primitive shading pipeline (@stage@ is
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     or
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT').
--
-- -   The @stage@ member of one element of @pStages@ /must/ be either
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT'
--     or
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV'.
--
-- -   The @stage@ member of each element of @pStages@ /must/ not be
--     'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   If @pStages@ includes a tessellation control shader stage, it /must/
--     include a tessellation evaluation shader stage
--
-- -   If @pStages@ includes a tessellation evaluation shader stage, it
--     /must/ include a tessellation control shader stage
--
-- -   If @pStages@ includes a tessellation control shader stage and a
--     tessellation evaluation shader stage, @pTessellationState@ /must/ be
--     a valid pointer to a valid 'PipelineTessellationStateCreateInfo'
--     structure
--
-- -   If @pStages@ includes tessellation shader stages, the shader code of
--     at least one stage /must/ contain an @OpExecutionMode@ instruction
--     that specifies the type of subdivision in the pipeline
--
-- -   If @pStages@ includes tessellation shader stages, and the shader
--     code of both stages contain an @OpExecutionMode@ instruction that
--     specifies the type of subdivision in the pipeline, they /must/ both
--     specify the same subdivision mode
--
-- -   If @pStages@ includes tessellation shader stages, the shader code of
--     at least one stage /must/ contain an @OpExecutionMode@ instruction
--     that specifies the output patch size in the pipeline
--
-- -   If @pStages@ includes tessellation shader stages, and the shader
--     code of both contain an @OpExecutionMode@ instruction that specifies
--     the out patch size in the pipeline, they /must/ both specify the
--     same patch size
--
-- -   If @pStages@ includes tessellation shader stages, the @topology@
--     member of @pInputAssembly@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST'
--
-- -   If the @topology@ member of @pInputAssembly@ is
--     'Graphics.Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST',
--     @pStages@ /must/ include tessellation shader stages
--
-- -   If @pStages@ includes a geometry shader stage, and does not include
--     any tessellation shader stages, its shader code /must/ contain an
--     @OpExecutionMode@ instruction that specifies an input primitive type
--     that is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-geometry-execution compatible>
--     with the primitive topology specified in @pInputAssembly@
--
-- -   If @pStages@ includes a geometry shader stage, and also includes
--     tessellation shader stages, its shader code /must/ contain an
--     @OpExecutionMode@ instruction that specifies an input primitive type
--     that is
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-geometry-execution compatible>
--     with the primitive topology that is output by the tessellation
--     stages
--
-- -   If @pStages@ includes a fragment shader stage and a geometry shader
--     stage, and the fragment shader code reads from an input variable
--     that is decorated with @PrimitiveID@, then the geometry shader code
--     /must/ write to a matching output variable, decorated with
--     @PrimitiveID@, in all execution paths
--
-- -   If @pStages@ includes a fragment shader stage, its shader code
--     /must/ not read from any input attachment that is defined as
--     'Graphics.Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' in @subpass@
--
-- -   The shader code for the entry points identified by @pStages@, and
--     the rest of the state identified by this structure /must/ adhere to
--     the pipeline linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   If rasterization is not disabled and @subpass@ uses a depth\/stencil
--     attachment in @renderPass@ that has a layout of
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     in the 'Graphics.Vulkan.Core10.Pass.AttachmentReference' defined by
--     @subpass@, the @depthWriteEnable@ member of @pDepthStencilState@
--     /must/ be 'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If rasterization is not disabled and @subpass@ uses a depth\/stencil
--     attachment in @renderPass@ that has a layout of
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     or
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     in the 'Graphics.Vulkan.Core10.Pass.AttachmentReference' defined by
--     @subpass@, the @failOp@, @passOp@ and @depthFailOp@ members of each
--     of the @front@ and @back@ members of @pDepthStencilState@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StencilOp.STENCIL_OP_KEEP'
--
-- -   If rasterization is not disabled and the subpass uses color
--     attachments, then for each color attachment in the subpass the
--     @blendEnable@ member of the corresponding element of the
--     @pAttachment@ member of @pColorBlendState@ /must/ be
--     'Graphics.Vulkan.Core10.BaseType.FALSE' if the attached image’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     does not contain
--     'Graphics.Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT'.
--
-- -   If rasterization is not disabled and the subpass uses color
--     attachments, the @attachmentCount@ member of @pColorBlendState@
--     /must/ be equal to the @colorAttachmentCount@ used to create
--     @subpass@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT',
--     the @pViewports@ member of @pViewportState@ /must/ be a valid
--     pointer to an array of @pViewportState->viewportCount@ valid
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.Viewport' structures
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR',
--     the @pScissors@ member of @pViewportState@ /must/ be a valid pointer
--     to an array of @pViewportState->scissorCount@
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.Rect2D' structures
--
-- -   If the wide lines feature is not enabled, and no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_WIDTH',
--     the @lineWidth@ member of @pRasterizationState@ /must/ be @1.0@
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Graphics.Vulkan.Core10.BaseType.FALSE', @pViewportState@ /must/ be
--     a valid pointer to a valid 'PipelineViewportStateCreateInfo'
--     structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Graphics.Vulkan.Core10.BaseType.FALSE', @pMultisampleState@ /must/
--     be a valid pointer to a valid 'PipelineMultisampleStateCreateInfo'
--     structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Graphics.Vulkan.Core10.BaseType.FALSE', and @subpass@ uses a
--     depth\/stencil attachment, @pDepthStencilState@ /must/ be a valid
--     pointer to a valid 'PipelineDepthStencilStateCreateInfo' structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Graphics.Vulkan.Core10.BaseType.FALSE', and @subpass@ uses color
--     attachments, @pColorBlendState@ /must/ be a valid pointer to a valid
--     'PipelineColorBlendStateCreateInfo' structure
--
-- -   If the depth bias clamping feature is not enabled, no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS',
--     and the @depthBiasEnable@ member of @pRasterizationState@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', the @depthBiasClamp@ member
--     of @pRasterizationState@ /must/ be @0.0@
--
-- -   If the
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_EXT_depth_range_unrestricted@
--     extension is not enabled and no element of the @pDynamicStates@
--     member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS',
--     and the @depthBoundsTestEnable@ member of @pDepthStencilState@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', the @minDepthBounds@ and
--     @maxDepthBounds@ members of @pDepthStencilState@ /must/ be between
--     @0.0@ and @1.0@, inclusive
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT',
--     and the @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pMultisampleState@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE',
--     @sampleLocationsInfo.sampleLocationGridSize.width@ /must/ evenly
--     divide
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.width@
--     as returned by
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT',
--     and the @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pMultisampleState@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE',
--     @sampleLocationsInfo.sampleLocationGridSize.height@ /must/ evenly
--     divide
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.height@
--     as returned by
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT',
--     and the @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pMultisampleState@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE',
--     @sampleLocationsInfo.sampleLocationsPerPixel@ /must/ equal
--     @rasterizationSamples@
--
-- -   If the @sampleLocationsEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pMultisampleState@ is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', the fragment shader code
--     /must/ not statically use the extended instruction
--     @InterpolateAtSample@
--
-- -   @layout@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with all shaders specified in @pStages@
--
-- -   If neither the @VK_AMD_mixed_attachment_samples@ nor the
--     @VK_NV_framebuffer_mixed_samples@ extensions are enabled, and if
--     @subpass@ uses color and\/or depth\/stencil attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be the
--     same as the sample count for those subpass attachments
--
-- -   If the @VK_AMD_mixed_attachment_samples@ extension is enabled, and
--     if @subpass@ uses color and\/or depth\/stencil attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ equal
--     the maximum of the sample counts of those subpass attachments
--
-- -   If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and
--     if @subpass@ has a depth\/stencil attachment and depth test, stencil
--     test, or depth bounds test are enabled, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be the
--     same as the sample count of the depth\/stencil attachment
--
-- -   If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and
--     if @subpass@ has any color attachments, then the
--     @rasterizationSamples@ member of @pMultisampleState@ /must/ be
--     greater than or equal to the sample count for those subpass
--     attachments
--
-- -   If the @VK_NV_coverage_reduction_mode@ extension is enabled, the
--     coverage reduction mode specified by
--     'Graphics.Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateInfoNV'::@coverageReductionMode@,
--     the @rasterizationSamples@ member of @pMultisampleState@ and the
--     sample counts for the color and depth\/stencil attachments (if the
--     subpass has them) /must/ be a valid combination returned by
--     'Graphics.Vulkan.Extensions.VK_NV_coverage_reduction_mode.getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
--
-- -   If @subpass@ does not use any color and\/or depth\/stencil
--     attachments, then the @rasterizationSamples@ member of
--     @pMultisampleState@ /must/ follow the rules for a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-noattachments zero-attachment subpass>
--
-- -   @subpass@ /must/ be a valid subpass within @renderPass@
--
-- -   If the @renderPass@ has multiview enabled and @subpass@ has more
--     than one bit set in the view mask and @multiviewTessellationShader@
--     is not enabled, then @pStages@ /must/ not include tessellation
--     shaders.
--
-- -   If the @renderPass@ has multiview enabled and @subpass@ has more
--     than one bit set in the view mask and @multiviewGeometryShader@ is
--     not enabled, then @pStages@ /must/ not include a geometry shader.
--
-- -   If the @renderPass@ has multiview enabled and @subpass@ has more
--     than one bit set in the view mask, shaders in the pipeline /must/
--     not write to the @Layer@ built-in output
--
-- -   If the @renderPass@ has multiview enabled, then all shaders /must/
--     not include variables decorated with the @Layer@ built-in decoration
--     in their interfaces.
--
-- -   @flags@ /must/ not contain the
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.PIPELINE_CREATE_DISPATCH_BASE'
--     flag.
--
-- -   If @pStages@ includes a fragment shader stage and an input
--     attachment was referenced by the
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.RenderPassInputAttachmentAspectCreateInfo'
--     at @renderPass@ create time, its shader code /must/ not read from
--     any aspect that was not specified in the @aspectMask@ of the
--     corresponding
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.InputAttachmentAspectReference'
--     structure.
--
-- -   The number of resources in @layout@ accessible to each shader stage
--     that is used by the pipeline /must/ be less than or equal to
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_NV',
--     and the @viewportWScalingEnable@ member of a
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--     structure, included in the @pNext@ chain of @pViewportState@, is
--     'Graphics.Vulkan.Core10.BaseType.TRUE', the @pViewportWScalings@
--     member of the
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--     /must/ be a pointer to an array of
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'::@viewportCount@
--     valid
--     'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.ViewportWScalingNV'
--     structures
--
-- -   If @pStages@ includes a vertex shader stage, @pVertexInputState@
--     /must/ be a valid pointer to a valid
--     'PipelineVertexInputStateCreateInfo' structure
--
-- -   If @pStages@ includes a vertex shader stage, @pInputAssemblyState@
--     /must/ be a valid pointer to a valid
--     'PipelineInputAssemblyStateCreateInfo' structure
--
-- -   The @Xfb@ execution mode /can/ be specified by only one shader stage
--     in @pStages@
--
-- -   If any shader stage in @pStages@ specifies @Xfb@ execution mode it
--     /must/ be the last vertex processing stage
--
-- -   If a
--     'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--     value other than zero is specified, all variables in the output
--     interface of the entry point being compiled decorated with
--     @Position@, @PointSize@, @ClipDistance@, or @CullDistance@ /must/
--     all be decorated with identical @Stream@ values that match the
--     @rasterizationStream@
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--     is zero, or not specified, all variables in the output interface of
--     the entry point being compiled decorated with @Position@,
--     @PointSize@, @ClipDistance@, or @CullDistance@ /must/ all be
--     decorated with a @Stream@ value of zero, or /must/ not specify the
--     @Stream@ decoration
--
-- -   If the last vertex processing stage is a geometry shader, and that
--     geometry shader uses the @GeometryStreams@ capability, then
--     'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackFeaturesEXT'::@geometryStreams@
--     feature /must/ be enabled
--
-- -   If there are any mesh shader stages in the pipeline there /must/ not
--     be any shader stage in the pipeline with a @Xfb@ execution mode.
--
-- -   If the @lineRasterizationMode@ member of a
--     'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pRasterizationState@ is
--     'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_BRESENHAM_EXT'
--     or
--     'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT'
--     and if rasterization is enabled, then the @alphaToCoverageEnable@,
--     @alphaToOneEnable@, and @sampleShadingEnable@ members of
--     @pMultisampleState@ /must/ all be
--     'Graphics.Vulkan.Core10.BaseType.FALSE'
--
-- -   If the @stippledLineEnable@ member of
--     'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'
--     is 'Graphics.Vulkan.Core10.BaseType.TRUE' and no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     'Graphics.Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_EXT',
--     then the @lineStippleFactor@ member of
--     'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'
--     /must/ be in the range [1,256]
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Extensions.VK_AMD_pipeline_compiler_control.PipelineCompilerControlCreateInfoAMD',
--     'Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfoEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT',
--     or
--     'Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test.PipelineRepresentativeFragmentTestStateCreateInfoNV'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   @pStages@ /must/ be a valid pointer to an array of @stageCount@
--     valid 'PipelineShaderStageCreateInfo' structures
--
-- -   @pRasterizationState@ /must/ be a valid pointer to a valid
--     'PipelineRasterizationStateCreateInfo' structure
--
-- -   If @pDynamicState@ is not @NULL@, @pDynamicState@ /must/ be a valid
--     pointer to a valid 'PipelineDynamicStateCreateInfo' structure
--
-- -   @layout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   @renderPass@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.RenderPass' handle
--
-- -   @stageCount@ /must/ be greater than @0@
--
-- -   Each of @basePipelineHandle@, @layout@, and @renderPass@ that are
--     valid handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Pipeline',
-- 'PipelineColorBlendStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'PipelineDepthStencilStateCreateInfo', 'PipelineDynamicStateCreateInfo',
-- 'PipelineInputAssemblyStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Handles.PipelineLayout',
-- 'PipelineMultisampleStateCreateInfo',
-- 'PipelineRasterizationStateCreateInfo', 'PipelineShaderStageCreateInfo',
-- 'PipelineTessellationStateCreateInfo',
-- 'PipelineVertexInputStateCreateInfo', 'PipelineViewportStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Handles.RenderPass',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createGraphicsPipelines'
data GraphicsPipelineCreateInfo (es :: [Type]) = GraphicsPipelineCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Graphics.Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
    -- specifying how the pipeline will be generated.
    flags :: PipelineCreateFlags
  , -- | @pStages@ is a pointer to an array of @stageCount@
    -- 'PipelineShaderStageCreateInfo' structures describing the set of the
    -- shader stages to be included in the graphics pipeline.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pVertexInputState@ is a pointer to a
    -- 'PipelineVertexInputStateCreateInfo' structure. It is ignored if the
    -- pipeline includes a mesh shader stage.
    vertexInputState :: Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
  , -- | @pInputAssemblyState@ is a pointer to a
    -- 'PipelineInputAssemblyStateCreateInfo' structure which determines input
    -- assembly behavior, as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing Drawing Commands>.
    -- It is ignored if the pipeline includes a mesh shader stage.
    inputAssemblyState :: Maybe PipelineInputAssemblyStateCreateInfo
  , -- | @pTessellationState@ is a pointer to a
    -- 'PipelineTessellationStateCreateInfo' structure, and is ignored if the
    -- pipeline does not include a tessellation control shader stage and
    -- tessellation evaluation shader stage.
    tessellationState :: Maybe (SomeStruct PipelineTessellationStateCreateInfo)
  , -- | @pViewportState@ is a pointer to a 'PipelineViewportStateCreateInfo'
    -- structure, and is ignored if the pipeline has rasterization disabled.
    viewportState :: Maybe (SomeStruct PipelineViewportStateCreateInfo)
  , -- | @pRasterizationState@ is a pointer to a
    -- 'PipelineRasterizationStateCreateInfo' structure.
    rasterizationState :: SomeStruct PipelineRasterizationStateCreateInfo
  , -- | @pMultisampleState@ is a pointer to a
    -- 'PipelineMultisampleStateCreateInfo' structure, and is ignored if the
    -- pipeline has rasterization disabled.
    multisampleState :: Maybe (SomeStruct PipelineMultisampleStateCreateInfo)
  , -- | @pDepthStencilState@ is a pointer to a
    -- 'PipelineDepthStencilStateCreateInfo' structure, and is ignored if the
    -- pipeline has rasterization disabled or if the subpass of the render pass
    -- the pipeline is created against does not use a depth\/stencil
    -- attachment.
    depthStencilState :: Maybe PipelineDepthStencilStateCreateInfo
  , -- | @pColorBlendState@ is a pointer to a 'PipelineColorBlendStateCreateInfo'
    -- structure, and is ignored if the pipeline has rasterization disabled or
    -- if the subpass of the render pass the pipeline is created against does
    -- not use any color attachments.
    colorBlendState :: Maybe (SomeStruct PipelineColorBlendStateCreateInfo)
  , -- | @pDynamicState@ is a pointer to a 'PipelineDynamicStateCreateInfo'
    -- structure, and is used to indicate which properties of the pipeline
    -- state object are dynamic and /can/ be changed independently of the
    -- pipeline state. This /can/ be @NULL@, which means no state in the
    -- pipeline is considered dynamic.
    dynamicState :: Maybe PipelineDynamicStateCreateInfo
  , -- | @layout@ is the description of binding locations used by both the
    -- pipeline and descriptor sets used with the pipeline.
    layout :: PipelineLayout
  , -- | @renderPass@ is a handle to a render pass object describing the
    -- environment in which the pipeline will be used; the pipeline /must/ only
    -- be used with an instance of any render pass compatible with the one
    -- provided. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-compatibility Render Pass Compatibility>
    -- for more information.
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
deriving instance Show (Chain es) => Show (GraphicsPipelineCreateInfo es)

instance Extensible GraphicsPipelineCreateInfo where
  extensibleType = STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
  setNext x next = x{next = next}
  getNext GraphicsPipelineCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends GraphicsPipelineCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineCompilerControlCreateInfoAMD = Just f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineRepresentativeFragmentTestStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineDiscardRectangleStateCreateInfoEXT = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (GraphicsPipelineCreateInfo es) where
  withCStruct x f = allocaBytesAligned 144 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GraphicsPipelineCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stages)) :: Word32))
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (stages)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (stages)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
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
    pRasterizationState'' <- ContT @_ @_ @(Ptr (PipelineRasterizationStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineRasterizationStateCreateInfo (rasterizationState) (cont . castPtr)
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
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (mempty)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    pRasterizationState'' <- ContT @_ @_ @(Ptr (PipelineRasterizationStateCreateInfo '[])) $ \cont -> withSomeCStruct @PipelineRasterizationStateCreateInfo ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr (PipelineRasterizationStateCreateInfo _)))) pRasterizationState''
    lift $ poke ((p `plusPtr` 104 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 112 :: Ptr RenderPass)) (zero)
    lift $ poke ((p `plusPtr` 120 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 136 :: Ptr Int32)) (zero)
    lift $ f

instance PeekChain es => FromCStruct (GraphicsPipelineCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCreateFlags))
    stageCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo a))))
    pStages' <- generateM (fromIntegral stageCount) (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    pVertexInputState <- peek @(Ptr (PipelineVertexInputStateCreateInfo _)) ((p `plusPtr` 32 :: Ptr (Ptr (PipelineVertexInputStateCreateInfo a))))
    pVertexInputState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pVertexInputState
    pInputAssemblyState <- peek @(Ptr PipelineInputAssemblyStateCreateInfo) ((p `plusPtr` 40 :: Ptr (Ptr PipelineInputAssemblyStateCreateInfo)))
    pInputAssemblyState' <- maybePeek (\j -> peekCStruct @PipelineInputAssemblyStateCreateInfo (j)) pInputAssemblyState
    pTessellationState <- peek @(Ptr (PipelineTessellationStateCreateInfo _)) ((p `plusPtr` 48 :: Ptr (Ptr (PipelineTessellationStateCreateInfo a))))
    pTessellationState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pTessellationState
    pViewportState <- peek @(Ptr (PipelineViewportStateCreateInfo _)) ((p `plusPtr` 56 :: Ptr (Ptr (PipelineViewportStateCreateInfo a))))
    pViewportState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pViewportState
    pRasterizationState <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 64 :: Ptr (Ptr (PipelineRasterizationStateCreateInfo a))))
    pMultisampleState <- peek @(Ptr (PipelineMultisampleStateCreateInfo _)) ((p `plusPtr` 72 :: Ptr (Ptr (PipelineMultisampleStateCreateInfo a))))
    pMultisampleState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pMultisampleState
    pDepthStencilState <- peek @(Ptr PipelineDepthStencilStateCreateInfo) ((p `plusPtr` 80 :: Ptr (Ptr PipelineDepthStencilStateCreateInfo)))
    pDepthStencilState' <- maybePeek (\j -> peekCStruct @PipelineDepthStencilStateCreateInfo (j)) pDepthStencilState
    pColorBlendState <- peek @(Ptr (PipelineColorBlendStateCreateInfo _)) ((p `plusPtr` 88 :: Ptr (Ptr (PipelineColorBlendStateCreateInfo a))))
    pColorBlendState' <- maybePeek (\j -> peekSomeCStruct (forgetExtensions (j))) pColorBlendState
    pDynamicState <- peek @(Ptr PipelineDynamicStateCreateInfo) ((p `plusPtr` 96 :: Ptr (Ptr PipelineDynamicStateCreateInfo)))
    pDynamicState' <- maybePeek (\j -> peekCStruct @PipelineDynamicStateCreateInfo (j)) pDynamicState
    layout <- peek @PipelineLayout ((p `plusPtr` 104 :: Ptr PipelineLayout))
    renderPass <- peek @RenderPass ((p `plusPtr` 112 :: Ptr RenderPass))
    subpass <- peek @Word32 ((p `plusPtr` 120 :: Ptr Word32))
    basePipelineHandle <- peek @Pipeline ((p `plusPtr` 128 :: Ptr Pipeline))
    basePipelineIndex <- peek @Int32 ((p `plusPtr` 136 :: Ptr Int32))
    pure $ GraphicsPipelineCreateInfo
             next flags pStages' pVertexInputState' pInputAssemblyState' pTessellationState' pViewportState' pRasterizationState pMultisampleState' pDepthStencilState' pColorBlendState' pDynamicState' layout renderPass subpass basePipelineHandle basePipelineIndex

instance es ~ '[] => Zero (GraphicsPipelineCreateInfo es) where
  zero = GraphicsPipelineCreateInfo
           ()
           zero
           mempty
           Nothing
           Nothing
           Nothing
           Nothing
           (SomeStruct zero)
           Nothing
           Nothing
           Nothing
           Nothing
           zero
           zero
           zero
           zero
           zero

