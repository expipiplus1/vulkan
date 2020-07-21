{-# language CPP #-}
module Vulkan.Core10.Pipeline  ( createGraphicsPipelines
                               , withGraphicsPipelines
                               , createComputePipelines
                               , withComputePipelines
                               , destroyPipeline
                               , Viewport(..)
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
                               , Pipeline(..)
                               , PipelineLayoutCreateFlags(..)
                               , PipelineDepthStencilStateCreateFlags(..)
                               , PipelineDynamicStateCreateFlags(..)
                               , PipelineColorBlendStateCreateFlags(..)
                               , PipelineMultisampleStateCreateFlags(..)
                               , PipelineRasterizationStateCreateFlags(..)
                               , PipelineViewportStateCreateFlags(..)
                               , PipelineTessellationStateCreateFlags(..)
                               , PipelineInputAssemblyStateCreateFlags(..)
                               , PipelineVertexInputStateCreateFlags(..)
                               , PrimitiveTopology(..)
                               , CompareOp(..)
                               , PolygonMode(..)
                               , CullModeFlagBits(..)
                               , CullModeFlags
                               , FrontFace(..)
                               , BlendFactor(..)
                               , BlendOp(..)
                               , StencilOp(..)
                               , LogicOp(..)
                               , VertexInputRate(..)
                               , DynamicState(..)
                               , ShaderStageFlagBits(..)
                               , ShaderStageFlags
                               , PipelineCreateFlagBits(..)
                               , PipelineCreateFlags
                               , PipelineShaderStageCreateFlagBits(..)
                               , PipelineShaderStageCreateFlags
                               , ColorComponentFlagBits(..)
                               , ColorComponentFlags
                               , SampleMask
                               ) where

import Vulkan.CStruct.Utils (FixedArray)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
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
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
import Control.Monad.IO.Class (MonadIO)
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
import GHC.Generics (Generic)
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
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Utils (lowerArrayPtr)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Enums.BlendFactor (BlendFactor)
import Vulkan.Core10.Enums.BlendOp (BlendOp)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Enums.ColorComponentFlagBits (ColorComponentFlags)
import Vulkan.Core10.Enums.CompareOp (CompareOp)
import Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlags)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateComputePipelines))
import Vulkan.Dynamic (DeviceCmds(pVkCreateGraphicsPipelines))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyPipeline))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.DynamicState (DynamicState)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.FrontFace (FrontFace)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GraphicsPipelineShaderGroupsCreateInfoNV)
import Vulkan.Core10.Enums.LogicOp (LogicOp)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PipelineColorBlendAdvancedStateCreateInfoEXT)
import Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlags (PipelineColorBlendStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_pipeline_compiler_control (PipelineCompilerControlCreateInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_framebuffer_mixed_samples (PipelineCoverageModulationStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (PipelineCoverageReductionStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_coverage_to_color (PipelineCoverageToColorStateCreateInfoNV)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfoEXT)
import Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlags (PipelineDepthStencilStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_discard_rectangles (PipelineDiscardRectangleStateCreateInfoEXT)
import Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags (PipelineDynamicStateCreateFlags)
import Vulkan.Core10.Enums.PipelineInputAssemblyStateCreateFlags (PipelineInputAssemblyStateCreateFlags)
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags (PipelineMultisampleStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conservative_rasterization (PipelineRasterizationConservativeStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_enable (PipelineRasterizationDepthClipStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PipelineRasterizationLineStateCreateInfoEXT)
import Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags (PipelineRasterizationStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_rasterization_order (PipelineRasterizationStateRasterizationOrderAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PipelineRasterizationStateStreamCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_representative_fragment_test (PipelineRepresentativeFragmentTestStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (PipelineSampleLocationsStateCreateInfoEXT)
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subgroup_size_control (PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (PipelineTessellationDomainOriginStateCreateInfo)
import Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags (PipelineTessellationStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PipelineVertexInputDivisorStateCreateInfoEXT)
import Vulkan.Core10.Enums.PipelineVertexInputStateCreateFlags (PipelineVertexInputStateCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PipelineViewportCoarseSampleOrderStateCreateInfoNV)
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
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits(SampleCountFlagBits))
import Vulkan.Core10.FundamentalTypes (SampleMask)
import Vulkan.Core10.Handles (ShaderModule)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10.Enums.StencilOp (StencilOp)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Core10.Enums.VertexInputRate (VertexInputRate)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO))
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
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlags (PipelineColorBlendStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlags (PipelineDepthStencilStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags (PipelineDynamicStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineInputAssemblyStateCreateFlags (PipelineInputAssemblyStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineLayoutCreateFlags (PipelineLayoutCreateFlags(..))
import Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags (PipelineMultisampleStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags (PipelineRasterizationStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlags)
import Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags (PipelineTessellationStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineVertexInputStateCreateFlags (PipelineVertexInputStateCreateFlags(..))
import Vulkan.Core10.Enums.PipelineViewportStateCreateFlags (PipelineViewportStateCreateFlags(..))
import Vulkan.Core10.Enums.PolygonMode (PolygonMode(..))
import Vulkan.Core10.Enums.PrimitiveTopology (PrimitiveTopology(..))
import Vulkan.Core10.FundamentalTypes (SampleMask)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
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
-- The 'GraphicsPipelineCreateInfo' structure includes an array of shader
-- create info structures containing all the desired active shader stages,
-- as well as creation info to define all relevant fixed-function stages,
-- and a pipeline layout.
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- -   If @pipelineCache@ was created with
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT',
--     host access to @pipelineCache@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>
--
-- Note
--
-- An implicit cache may be provided by the implementation or a layer. For
-- this reason, it is still valid to set
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
-- on @flags@ for any element of @pCreateInfos@ while passing
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE' for @pipelineCache@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'GraphicsPipelineCreateInfo' structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Vulkan.Core10.Handles.Pipeline' handles
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
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED_EXT'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Handles.Pipeline', 'Vulkan.Core10.Handles.PipelineCache'
createGraphicsPipelines :: forall io
                         . (MonadIO io)
                        => -- | @device@ is the logical device that creates the graphics pipelines.
                           Device
                        -> -- | @pipelineCache@ is either 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                           -- indicating that pipeline caching is disabled; or the handle of a valid
                           -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-cache pipeline cache>
                           -- object, in which case use of that cache is enabled for the duration of
                           -- the command.
                           PipelineCache
                        -> -- | @pCreateInfos@ is a pointer to an array of 'GraphicsPipelineCreateInfo'
                           -- structures.
                           ("createInfos" ::: Vector (SomeStruct GraphicsPipelineCreateInfo))
                        -> -- | @pAllocator@ controls host memory allocation as described in the
                           -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                           -- chapter.
                           ("allocator" ::: Maybe AllocationCallbacks)
                        -> io (Result, ("pipelines" ::: Vector Pipeline))
createGraphicsPipelines device pipelineCache createInfos allocator = liftIO . evalContT $ do
  let vkCreateGraphicsPipelinesPtr = pVkCreateGraphicsPipelines (deviceCmds (device :: Device))
  lift $ unless (vkCreateGraphicsPipelinesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateGraphicsPipelines is null" Nothing Nothing
  let vkCreateGraphicsPipelines' = mkVkCreateGraphicsPipelines vkCreateGraphicsPipelinesPtr
  pPCreateInfos <- ContT $ allocaBytesAligned @(GraphicsPipelineCreateInfo _) ((Data.Vector.length (createInfos)) * 144) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (144 * (i)) :: Ptr (GraphicsPipelineCreateInfo _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ vkCreateGraphicsPipelines' (deviceHandle (device)) (pipelineCache) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (forgetExtensions (pPCreateInfos)) pAllocator (pPPipelines)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (r, pPipelines)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createGraphicsPipelines' and 'destroyPipeline'
--
-- To ensure that 'destroyPipeline' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withGraphicsPipelines :: forall io r . MonadIO io => Device -> PipelineCache -> Vector (SomeStruct GraphicsPipelineCreateInfo) -> Maybe AllocationCallbacks -> (io (Result, Vector Pipeline) -> ((Result, Vector Pipeline) -> io ()) -> r) -> r
withGraphicsPipelines device pipelineCache pCreateInfos pAllocator b =
  b (createGraphicsPipelines device pipelineCache pCreateInfos pAllocator)
    (\(_, o1) -> traverse_ (\o1Elem -> destroyPipeline device o1Elem pAllocator) o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateComputePipelines
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct ComputePipelineCreateInfo) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct ComputePipelineCreateInfo) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateComputePipelines - Creates a new compute pipeline object
--
-- == Valid Usage
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   If the @flags@ member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- -   If @pipelineCache@ was created with
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT',
--     host access to @pipelineCache@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'ComputePipelineCreateInfo' structures
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Vulkan.Core10.Handles.Pipeline' handles
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
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED_EXT'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'ComputePipelineCreateInfo', 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Handles.Pipeline', 'Vulkan.Core10.Handles.PipelineCache'
createComputePipelines :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the logical device that creates the compute pipelines.
                          Device
                       -> -- | @pipelineCache@ is either 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                          -- indicating that pipeline caching is disabled; or the handle of a valid
                          -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-cache pipeline cache>
                          -- object, in which case use of that cache is enabled for the duration of
                          -- the command.
                          PipelineCache
                       -> -- | @pCreateInfos@ is a pointer to an array of 'ComputePipelineCreateInfo'
                          -- structures.
                          ("createInfos" ::: Vector (SomeStruct ComputePipelineCreateInfo))
                       -> -- | @pAllocator@ controls host memory allocation as described in the
                          -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                          -- chapter.
                          ("allocator" ::: Maybe AllocationCallbacks)
                       -> io (Result, ("pipelines" ::: Vector Pipeline))
createComputePipelines device pipelineCache createInfos allocator = liftIO . evalContT $ do
  let vkCreateComputePipelinesPtr = pVkCreateComputePipelines (deviceCmds (device :: Device))
  lift $ unless (vkCreateComputePipelinesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateComputePipelines is null" Nothing Nothing
  let vkCreateComputePipelines' = mkVkCreateComputePipelines vkCreateComputePipelinesPtr
  pPCreateInfos <- ContT $ allocaBytesAligned @(ComputePipelineCreateInfo _) ((Data.Vector.length (createInfos)) * 96) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (96 * (i)) :: Ptr (ComputePipelineCreateInfo _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ vkCreateComputePipelines' (deviceHandle (device)) (pipelineCache) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (forgetExtensions (pPCreateInfos)) pAllocator (pPPipelines)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (r, pPipelines)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createComputePipelines' and 'destroyPipeline'
--
-- To ensure that 'destroyPipeline' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withComputePipelines :: forall io r . MonadIO io => Device -> PipelineCache -> Vector (SomeStruct ComputePipelineCreateInfo) -> Maybe AllocationCallbacks -> (io (Result, Vector Pipeline) -> ((Result, Vector Pipeline) -> io ()) -> r) -> r
withComputePipelines device pipelineCache pCreateInfos pAllocator b =
  b (createComputePipelines device pipelineCache pCreateInfos pAllocator)
    (\(_, o1) -> traverse_ (\o1Elem -> destroyPipeline device o1Elem pAllocator) o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipeline
  :: FunPtr (Ptr Device_T -> Pipeline -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Pipeline -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyPipeline - Destroy a pipeline object
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @pipeline@ /must/ have
--     completed execution
--
-- -   If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @pipeline@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @pipeline@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   If @pipeline@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
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
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline'
destroyPipeline :: forall io
                 . (MonadIO io)
                => -- | @device@ is the logical device that destroys the pipeline.
                   Device
                -> -- | @pipeline@ is the handle of the pipeline to destroy.
                   Pipeline
                -> -- | @pAllocator@ controls host memory allocation as described in the
                   -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                   -- chapter.
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io ()
destroyPipeline device pipeline allocator = liftIO . evalContT $ do
  let vkDestroyPipelinePtr = pVkDestroyPipeline (deviceCmds (device :: Device))
  lift $ unless (vkDestroyPipelinePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyPipeline is null" Nothing Nothing
  let vkDestroyPipeline' = mkVkDestroyPipeline vkDestroyPipelinePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyPipeline' (deviceHandle (device)) (pipeline) pAllocator
  pure $ ()


-- | VkViewport - Structure specifying a viewport
--
-- = Description
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
-- -   oz = @minDepth@
--
-- -   px = @width@
--
-- -   py = @height@
--
-- -   pz = @maxDepth@ - @minDepth@.
--
-- If a render pass transform is enabled, the values (px,py) and (ox, oy)
-- defining the viewport are transformed as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-renderpass-transform render pass transform>
-- before participating in the viewport transform.
--
-- The application /can/ specify a negative term for @height@, which has
-- the effect of negating the y coordinate in clip space before performing
-- the transform. When using a negative @height@, the application /should/
-- also adjust the @y@ value to point to the lower left corner of the
-- viewport instead of the upper left corner. Using the negative @height@
-- allows the application to avoid having to negate the y component of the
-- @Position@ output from the last vertex processing stage in shaders that
-- also target other graphics APIs.
--
-- The width and height of the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxViewportDimensions implementation-dependent maximum viewport dimensions>
-- /must/ be greater than or equal to the width and height of the largest
-- image which /can/ be created and attached to a framebuffer.
--
-- The floating-point viewport bounds are represented with an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-viewportSubPixelBits implementation-dependent precision>.
--
-- == Valid Usage
--
-- -   @width@ /must/ be greater than @0.0@
--
-- -   @width@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewportDimensions@[0]
--
-- -   The absolute value of @height@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewportDimensions@[1]
--
-- -   @x@ /must/ be greater than or equal to @viewportBoundsRange@[0]
--
-- -   (@x@ + @width@) /must/ be less than or equal to
--     @viewportBoundsRange@[1]
--
-- -   @y@ /must/ be greater than or equal to @viewportBoundsRange@[0]
--
-- -   @y@ /must/ be less than or equal to @viewportBoundsRange@[1]
--
-- -   (@y@ + @height@) /must/ be greater than or equal to
--     @viewportBoundsRange@[0]
--
-- -   (@y@ + @height@) /must/ be less than or equal to
--     @viewportBoundsRange@[1]
--
-- -   Unless @VK_EXT_depth_range_unrestricted@ extension is enabled
--     @minDepth@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- -   Unless @VK_EXT_depth_range_unrestricted@ extension is enabled
--     @maxDepth@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- = See Also
--
-- 'PipelineViewportStateCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT'
data Viewport = Viewport
  { -- | @x@ and @y@ are the viewport’s upper left corner (x,y).
    x :: Float
  , -- No documentation found for Nested "VkViewport" "y"
    y :: Float
  , -- | @width@ and @height@ are the viewport’s width and height, respectively.
    width :: Float
  , -- No documentation found for Nested "VkViewport" "height"
    height :: Float
  , -- | @minDepth@ and @maxDepth@ are the depth range for the viewport. It is
    -- valid for @minDepth@ to be greater than or equal to @maxDepth@.
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
  withCStruct x f = allocaBytesAligned 24 4 $ \p -> pokeCStruct p x (f p)
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
             ((\(CFloat a) -> a) x) ((\(CFloat a) -> a) y) ((\(CFloat a) -> a) width) ((\(CFloat a) -> a) height) ((\(CFloat a) -> a) minDepth) ((\(CFloat a) -> a) maxDepth)

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
--     byte size of 'Vulkan.Core10.FundamentalTypes.Bool32'
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
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SpecializationMapEntry)
#endif
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SpecializationInfo)
#endif
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
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-meshShader mesh shader>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-taskShader task shader>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_NV'
--
-- -   @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL_GRAPHICS',
--     or 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL'
--
-- -   @pName@ /must/ be the name of an @OpEntryPoint@ in @module@ with an
--     execution model that matches @stage@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the @ClipDistance@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxClipDistances@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the @CullDistance@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxCullDistances@
--
-- -   If the identified entry point includes any variables in its
--     interface that are declared with the @ClipDistance@ or
--     @CullDistance@ @BuiltIn@ decoration, those variables /must/ not have
--     array sizes which sum to more than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxCombinedClipAndCullDistances@
--
-- -   If the identified entry point includes any variable in its interface
--     that is declared with the
--     'Vulkan.Core10.FundamentalTypes.SampleMask' @BuiltIn@ decoration,
--     that variable /must/ not have an array size greater than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxSampleMaskWords@
--
-- -   If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     the identified entry point /must/ not include any input variable in
--     its interface that is decorated with @CullDistance@
--
-- -   If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     and the identified entry point has an @OpExecutionMode@ instruction
--     that specifies a patch size with @OutputVertices@, the patch size
--     /must/ be greater than @0@ and less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTessellationPatchSize@
--
-- -   If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies a maximum output vertex count that is
--     greater than @0@ and less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxGeometryOutputVertices@
--
-- -   If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies an invocation count that is greater than
--     @0@ and less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxGeometryShaderInvocations@
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
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     the identified entry point /must/ not include any output variables
--     in its interface decorated with @CullDistance@
--
-- -   If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and the identified entry point writes to @FragDepth@ in any
--     execution path, it /must/ write to @FragDepth@ in all execution
--     paths
--
-- -   If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and the identified entry point writes to @FragStencilRefEXT@ in any
--     execution path, it /must/ write to @FragStencilRefEXT@ in all
--     execution paths
--
-- -   If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies a maximum output vertex count,
--     @OutputVertices@, that is greater than @0@ and less than or equal to
--     'Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderPropertiesNV'::@maxMeshOutputVertices@
--
-- -   If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction that specifies a maximum output primitive count,
--     @OutputPrimitivesNV@, that is greater than @0@ and less than or
--     equal to
--     'Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderPropertiesNV'::@maxMeshOutputPrimitives@
--
-- -   If @flags@ has the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--     flag set, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subgroupSizeControl subgroupSizeControl>
--     feature /must/ be enabled
--
-- -   If @flags@ has the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--     flag set, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-computeFullSubgroups computeFullSubgroups>
--     feature /must/ be enabled
--
-- -   If a
--     'Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--     structure is included in the @pNext@ chain, @flags@ /must/ not have
--     the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--     flag set
--
-- -   If a
--     'Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--     structure is included in the @pNext@ chain, the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subgroupSizeControl subgroupSizeControl>
--     feature /must/ be enabled, and @stage@ /must/ be a valid bit
--     specified in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-required-subgroup-size-stages requiredSubgroupSizeStages>
--
-- -   If a
--     'Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--     structure is included in the @pNext@ chain and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT',
--     the local workgroup size of the shader /must/ be less than or equal
--     to the product of
--     'Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'::@requiredSubgroupSize@
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-max-subgroups-per-workgroup maxComputeWorkgroupSubgroups>
--
-- -   If a
--     'Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--     structure is included in the @pNext@ chain, and @flags@ has the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--     flag set, the local workgroup size in the X dimension of the
--     pipeline /must/ be a multiple of
--     'Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'::@requiredSubgroupSize@
--
-- -   If @flags@ has both the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--     and
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--     flags set, the local workgroup size in the X dimension of the
--     pipeline /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-max-subgroup-size maxSubgroupSize>
--
-- -   If @flags@ has the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--     flag set and @flags@ does not have the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--     flag set and no
--     'Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--     structure is included in the @pNext@ chain, the local workgroup size
--     in the X dimension of the pipeline /must/ be a multiple of
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PipelineShaderStageCreateFlagBits'
--     values
--
-- -   @stage@ /must/ be a valid
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' value
--
-- -   @module@ /must/ be a valid 'Vulkan.Core10.Handles.ShaderModule'
--     handle
--
-- -   @pName@ /must/ be a null-terminated UTF-8 string
--
-- -   If @pSpecializationInfo@ is not @NULL@, @pSpecializationInfo@ /must/
--     be a valid pointer to a valid 'SpecializationInfo' structure
--
-- = See Also
--
-- 'ComputePipelineCreateInfo', 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsShaderGroupCreateInfoNV',
-- 'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PipelineShaderStageCreateFlags',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
-- 'Vulkan.Core10.Handles.ShaderModule',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits',
-- 'SpecializationInfo', 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineShaderStageCreateInfo (es :: [Type]) = PipelineShaderStageCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PipelineShaderStageCreateFlagBits'
    -- specifying how the pipeline shader stage will be generated.
    flags :: PipelineShaderStageCreateFlags
  , -- | @stage@ is a
    -- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' value
    -- specifying a single pipeline stage.
    stage :: ShaderStageFlagBits
  , -- | @module@ is a 'Vulkan.Core10.Handles.ShaderModule' object containing the
    -- shader for this stage.
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineShaderStageCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PipelineShaderStageCreateInfo es)

instance Extensible PipelineShaderStageCreateInfo where
  extensibleType = STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineShaderStageCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineShaderStageCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss PipelineShaderStageCreateInfo es, PokeChain es) => ToCStruct (PipelineShaderStageCreateInfo es) where
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

instance (Extendss PipelineShaderStageCreateInfo es, PeekChain es) => FromCStruct (PipelineShaderStageCreateInfo es) where
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
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be
--     a valid handle to a compute 'Vulkan.Core10.Handles.Pipeline'
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be a valid index into the calling command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not -1, @basePipelineHandle@ /must/
--     be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be -1
--
-- -   The @stage@ member of @stage@ /must/ be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
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
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Vulkan.Extensions.VK_AMD_pipeline_compiler_control.PipelineCompilerControlCreateInfoAMD'
--     or
--     'Vulkan.Extensions.VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   @stage@ /must/ be a valid 'PipelineShaderStageCreateInfo' structure
--
-- -   @layout@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout'
--     handle
--
-- -   Both of @basePipelineHandle@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'Vulkan.Core10.Handles.PipelineLayout', 'PipelineShaderStageCreateInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createComputePipelines'
data ComputePipelineCreateInfo (es :: [Type]) = ComputePipelineCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ComputePipelineCreateInfo (es :: [Type]))
#endif
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

instance (Extendss ComputePipelineCreateInfo es, PokeChain es) => ToCStruct (ComputePipelineCreateInfo es) where
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

instance (Extendss ComputePipelineCreateInfo es, PeekChain es) => FromCStruct (ComputePipelineCreateInfo es) where
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
-- 'Vulkan.Core10.Enums.VertexInputRate.VertexInputRate'
data VertexInputBindingDescription = VertexInputBindingDescription
  { -- | @binding@ is the binding number that this structure describes.
    --
    -- @binding@ /must/ be less than
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
    binding :: Word32
  , -- | @stride@ is the distance in bytes between two consecutive elements
    -- within the buffer.
    --
    -- @stride@ /must/ be less than or equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindingStride@
    stride :: Word32
  , -- | @inputRate@ is a 'Vulkan.Core10.Enums.VertexInputRate.VertexInputRate'
    -- value specifying whether vertex attribute addressing is a function of
    -- the vertex index or of the instance index.
    --
    -- @inputRate@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.VertexInputRate.VertexInputRate' value
    inputRate :: VertexInputRate
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VertexInputBindingDescription)
#endif
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
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'PipelineVertexInputStateCreateInfo'
data VertexInputAttributeDescription = VertexInputAttributeDescription
  { -- | @location@ is the shader binding location number for this attribute.
    --
    -- @location@ /must/ be less than
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributes@
    location :: Word32
  , -- | @binding@ is the binding number which this attribute takes its data
    -- from.
    --
    -- @binding@ /must/ be less than
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
    binding :: Word32
  , -- | @format@ is the size and type of the vertex attribute data.
    --
    -- @format@ /must/ be allowed as a vertex buffer format, as specified by
    -- the
    -- 'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_VERTEX_BUFFER_BIT'
    -- flag in
    -- 'Vulkan.Core10.DeviceInitialization.FormatProperties'::@bufferFeatures@
    -- returned by
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties'
    --
    -- @format@ /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
    format :: Format
  , -- | @offset@ is a byte offset of this attribute relative to the start of an
    -- element in the vertex input binding.
    --
    -- @offset@ /must/ be less than or equal to
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributeOffset@
    offset :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (VertexInputAttributeDescription)
#endif
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
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   @vertexAttributeDescriptionCount@ /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxVertexInputAttributes@
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
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PipelineVertexInputDivisorStateCreateInfoEXT'
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
  extensibleType = STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineVertexInputStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineVertexInputStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineVertexInputDivisorStateCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss PipelineVertexInputStateCreateInfo es, PokeChain es) => ToCStruct (PipelineVertexInputStateCreateInfo es) where
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

instance (Extendss PipelineVertexInputStateCreateInfo es, PeekChain es) => FromCStruct (PipelineVertexInputStateCreateInfo es) where
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
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_POINT_LIST',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY'
--     or
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST',
--     @primitiveRestartEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @topology@ /must/ not be any of
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY',
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY'
--     or
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @topology@ /must/ not be
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @topology@ /must/ be a valid
--     'Vulkan.Core10.Enums.PrimitiveTopology.PrimitiveTopology' value
--
-- = See Also
--
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
    -- ('Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexed' and
    -- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect'), and the
    -- special index value is either 0xFFFFFFFF when the @indexType@ parameter
    -- of 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer' is equal to
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32', 0xFF when @indexType@
    -- is equal to 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8_EXT', or
    -- 0xFFFF when @indexType@ is equal to
    -- 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16'. Primitive restart is
    -- not allowed for “list” topologies.
    primitiveRestartEnable :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineInputAssemblyStateCreateInfo)
#endif
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
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTessellationPatchSize@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PipelineTessellationDomainOriginStateCreateInfo'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsShaderGroupCreateInfoNV',
-- 'Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags.PipelineTessellationStateCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineTessellationStateCreateInfo (es :: [Type]) = PipelineTessellationStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: PipelineTessellationStateCreateFlags
  , -- | @patchControlPoints@ number of control points per patch.
    patchControlPoints :: Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineTessellationStateCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PipelineTessellationStateCreateInfo es)

instance Extensible PipelineTessellationStateCreateInfo where
  extensibleType = STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineTessellationStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineTessellationStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineTessellationDomainOriginStateCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss PipelineTessellationStateCreateInfo es, PokeChain es) => ToCStruct (PipelineTessellationStateCreateInfo es) where
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

instance (Extendss PipelineTessellationStateCreateInfo es, PeekChain es) => FromCStruct (PipelineTessellationStateCreateInfo es) where
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
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   @scissorCount@ /must/ be between @1@ and
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxViewports@,
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
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--     structure included in the @pNext@ chain is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the @viewportCount@ member of
--     the
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
--     structure /must/ be equal to @viewportCount@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportCoarseSampleOrderStateCreateInfoNV',
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV',
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV',
--     'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV',
--     or
--     'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV'
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
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-scissor scissors>
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

instance (Extendss PipelineViewportStateCreateInfo es, PokeChain es) => ToCStruct (PipelineViewportStateCreateInfo es) where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
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
        pPViewports <- ContT $ allocaBytesAligned @Viewport (((Data.Vector.length (viewports))) * 24) 4
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPViewports `plusPtr` (24 * (i)) :: Ptr Viewport) (e) . ($ ())) ((viewports))
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
        pPScissors <- ContT $ allocaBytesAligned @Rect2D (((Data.Vector.length (scissors))) * 16) 4
        Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPScissors `plusPtr` (16 * (i)) :: Ptr Rect2D) (e) . ($ ())) ((scissors))
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

instance (Extendss PipelineViewportStateCreateInfo es, PeekChain es) => FromCStruct (PipelineViewportStateCreateInfo es) where
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primrast-order Rasterization Order>.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-depthClamp depth clamping>
--     feature is not enabled, @depthClampEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fillModeNonSolid non-solid fill modes>
--     feature is not enabled, @polygonMode@ /must/ be
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL' or
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- -   If the @VK_NV_fill_rectangle@ extension is not enabled,
--     @polygonMode@ /must/ not be
--     'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Vulkan.Extensions.VK_EXT_conservative_rasterization.PipelineRasterizationConservativeStateCreateInfoEXT',
--     'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT',
--     'Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT',
--     'Vulkan.Extensions.VK_AMD_rasterization_order.PipelineRasterizationStateRasterizationOrderAMD',
--     or
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @polygonMode@ /must/ be a valid
--     'Vulkan.Core10.Enums.PolygonMode.PolygonMode' value
--
-- -   @cullMode@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.CullModeFlagBits.CullModeFlagBits' values
--
-- -   @frontFace@ /must/ be a valid
--     'Vulkan.Core10.Enums.FrontFace.FrontFace' value
--
-- = See Also
--
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
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-depth Depth Test>.
    -- If the pipeline is not created with
    -- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT'
    -- present then enabling depth clamp will also disable clipping primitives
    -- to the z planes of the frustrum as described in
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>.
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

instance (Extendss PipelineRasterizationStateCreateInfo es, PokeChain es) => ToCStruct (PipelineRasterizationStateCreateInfo es) where
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

instance (Extendss PipelineRasterizationStateCreateInfo es, PeekChain es) => FromCStruct (PipelineRasterizationStateCreateInfo es) where
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
-- = Description
--
-- Each bit in the sample mask is associated with a unique
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-multisampling-coverage-mask sample index>
-- as defined for the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-multisampling-coverage-mask coverage mask>.
-- Each bit b for mask word w in the sample mask corresponds to sample
-- index i, where i = 32 × w + b. @pSampleMask@ has a length equal to ⌈
-- @rasterizationSamples@ \/ 32 ⌉ words.
--
-- If @pSampleMask@ is @NULL@, it is treated as if the mask has all bits
-- set to @1@.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sampleRateShading sample rate shading>
--     feature is not enabled, @sampleShadingEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-alphaToOne alpha to one>
--     feature is not enabled, @alphaToOneEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   @minSampleShading@ /must/ be in the range [0,1]
--
-- -   If the @VK_NV_framebuffer_mixed_samples@ extension is enabled, and
--     if the subpass has any color attachments and @rasterizationSamples@
--     is greater than the number of color samples, then
--     @sampleShadingEnable@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV',
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateInfoNV',
--     'Vulkan.Extensions.VK_NV_fragment_coverage_to_color.PipelineCoverageToColorStateCreateInfoNV',
--     or
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @rasterizationSamples@ /must/ be a valid
--     'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' value
--
-- -   If @pSampleMask@ is not @NULL@, @pSampleMask@ /must/ be a valid
--     pointer to an array of
--     \(\lceil{\mathit{rasterizationSamples} \over 32}\rceil\)
--     'Vulkan.Core10.FundamentalTypes.SampleMask' values
--
-- = See Also
--
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
    -- 'Vulkan.Core10.Enums.SampleCountFlagBits.SampleCountFlagBits' specifying
    -- the number of samples used in rasterization.
    rasterizationSamples :: SampleCountFlagBits
  , -- | @sampleShadingEnable@ /can/ be used to enable
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-sampleshading Sample Shading>.
    sampleShadingEnable :: Bool
  , -- | @minSampleShading@ specifies a minimum fraction of sample shading if
    -- @sampleShadingEnable@ is set to 'Vulkan.Core10.FundamentalTypes.TRUE'.
    minSampleShading :: Float
  , -- | @pSampleMask@ is an array of 'Vulkan.Core10.FundamentalTypes.SampleMask'
    -- values used in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-samplemask sample mask test>.
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineMultisampleStateCreateInfo (es :: [Type]))
#endif
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

instance (Extendss PipelineMultisampleStateCreateInfo es, PokeChain es) => ToCStruct (PipelineMultisampleStateCreateInfo es) where
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

instance (Extendss PipelineMultisampleStateCreateInfo es, PeekChain es) => FromCStruct (PipelineMultisampleStateCreateInfo es) where
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
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @dstColorBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @srcAlphaBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-dualSrcBlend dual source blending>
--     feature is not enabled, @dstAlphaBlendFactor@ /must/ not be
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_SRC1_ALPHA', or
--     'Vulkan.Core10.Enums.BlendFactor.BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
--
-- -   If either of @colorBlendOp@ or @alphaBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @colorBlendOp@ /must/ equal @alphaBlendOp@
--
-- -   If
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendIndependentBlend@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE' and @colorBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @colorBlendOp@ /must/ be the same for all attachments
--
-- -   If
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::@advancedBlendIndependentBlend@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE' and @alphaBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then @alphaBlendOp@ /must/ be the same for all attachments
--
-- -   If
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
-- -   If @colorBlendOp@ or @alphaBlendOp@ is an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operation>,
--     then 'Vulkan.Core10.Pass.SubpassDescription'::@colorAttachmentCount@
--     of the subpass this pipeline is compiled against /must/ be less than
--     or equal to
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT'::advancedBlendMaxColorAttachments
--
-- == Valid Usage (Implicit)
--
-- -   @srcColorBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   @dstColorBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   @colorBlendOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendOp.BlendOp' value
--
-- -   @srcAlphaBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   @dstAlphaBlendFactor@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendFactor.BlendFactor' value
--
-- -   @alphaBlendOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.BlendOp.BlendOp' value
--
-- -   @colorWriteMask@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits'
--     values
--
-- = See Also
--
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
  , -- | @alphaBlendOp@ selects which blend operation is use to calculate the
    -- alpha values to write to the color attachment.
    alphaBlendOp :: BlendOp
  , -- | @colorWriteMask@ is a bitmask of
    -- 'Vulkan.Core10.Enums.ColorComponentFlagBits.ColorComponentFlagBits'
    -- specifying which of the R, G, B, and\/or A components are enabled for
    -- writing, as described for the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-color-write-mask Color Write Mask>.
    colorWriteMask :: ColorComponentFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineColorBlendAttachmentState)
#endif
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
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   If @logicOpEnable@ is 'Vulkan.Core10.FundamentalTypes.TRUE',
--     @logicOp@ /must/ be a valid 'Vulkan.Core10.Enums.LogicOp.LogicOp'
--     value
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PipelineColorBlendAdvancedStateCreateInfoEXT'
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
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Enums.LogicOp.LogicOp',
-- 'PipelineColorBlendAttachmentState',
-- 'Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlags.PipelineColorBlendStateCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineColorBlendStateCreateInfo (es :: [Type]) = PipelineColorBlendStateCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineColorBlendStateCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PipelineColorBlendStateCreateInfo es)

instance Extensible PipelineColorBlendStateCreateInfo where
  extensibleType = STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  setNext x next = x{next = next}
  getNext PipelineColorBlendStateCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineColorBlendStateCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineColorBlendAdvancedStateCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss PipelineColorBlendStateCreateInfo es, PokeChain es) => ToCStruct (PipelineColorBlendStateCreateInfo es) where
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
    pPAttachments' <- ContT $ allocaBytesAligned @PipelineColorBlendAttachmentState ((Data.Vector.length (mempty)) * 32) 4
    Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPAttachments' `plusPtr` (32 * (i)) :: Ptr PipelineColorBlendAttachmentState) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr PipelineColorBlendAttachmentState))) (pPAttachments')
    let pBlendConstants' = lowerArrayPtr ((p `plusPtr` 40 :: Ptr (FixedArray 4 CFloat)))
    lift $ case ((zero, zero, zero, zero)) of
      (e0, e1, e2, e3) -> do
        poke (pBlendConstants' :: Ptr CFloat) (CFloat (e0))
        poke (pBlendConstants' `plusPtr` 4 :: Ptr CFloat) (CFloat (e1))
        poke (pBlendConstants' `plusPtr` 8 :: Ptr CFloat) (CFloat (e2))
        poke (pBlendConstants' `plusPtr` 12 :: Ptr CFloat) (CFloat (e3))
    lift $ f

instance (Extendss PipelineColorBlendStateCreateInfo es, PeekChain es) => FromCStruct (PipelineColorBlendStateCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineColorBlendStateCreateFlags ((p `plusPtr` 16 :: Ptr PipelineColorBlendStateCreateFlags))
    logicOpEnable <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    logicOp <- peek @LogicOp ((p `plusPtr` 24 :: Ptr LogicOp))
    attachmentCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pAttachments <- peek @(Ptr PipelineColorBlendAttachmentState) ((p `plusPtr` 32 :: Ptr (Ptr PipelineColorBlendAttachmentState)))
    pAttachments' <- generateM (fromIntegral attachmentCount) (\i -> peekCStruct @PipelineColorBlendAttachmentState ((pAttachments `advancePtrBytes` (32 * (i)) :: Ptr PipelineColorBlendAttachmentState)))
    let pblendConstants = lowerArrayPtr @CFloat ((p `plusPtr` 40 :: Ptr (FixedArray 4 CFloat)))
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
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   If @dynamicStateCount@ is not @0@, @pDynamicStates@ /must/ be a
--     valid pointer to an array of @dynamicStateCount@ valid
--     'Vulkan.Core10.Enums.DynamicState.DynamicState' values
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.DynamicState.DynamicState',
-- 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags.PipelineDynamicStateCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineDynamicStateCreateInfo = PipelineDynamicStateCreateInfo
  { -- | @flags@ is reserved for future use.
    flags :: PipelineDynamicStateCreateFlags
  , -- | @pDynamicStates@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.DynamicState.DynamicState' values specifying which
    -- pieces of pipeline state will use the values from dynamic state commands
    -- rather than from pipeline state creation info.
    dynamicStates :: Vector DynamicState
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineDynamicStateCreateInfo)
#endif
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
-- 'Vulkan.Core10.Enums.CompareOp.CompareOp',
-- 'PipelineDepthStencilStateCreateInfo',
-- 'Vulkan.Core10.Enums.StencilOp.StencilOp'
data StencilOpState = StencilOpState
  { -- | @failOp@ is a 'Vulkan.Core10.Enums.StencilOp.StencilOp' value specifying
    -- the action performed on samples that fail the stencil test.
    --
    -- @failOp@ /must/ be a valid 'Vulkan.Core10.Enums.StencilOp.StencilOp'
    -- value
    failOp :: StencilOp
  , -- | @passOp@ is a 'Vulkan.Core10.Enums.StencilOp.StencilOp' value specifying
    -- the action performed on samples that pass both the depth and stencil
    -- tests.
    --
    -- @passOp@ /must/ be a valid 'Vulkan.Core10.Enums.StencilOp.StencilOp'
    -- value
    passOp :: StencilOp
  , -- | @depthFailOp@ is a 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
    -- specifying the action performed on samples that pass the stencil test
    -- and fail the depth test.
    --
    -- @depthFailOp@ /must/ be a valid
    -- 'Vulkan.Core10.Enums.StencilOp.StencilOp' value
    depthFailOp :: StencilOp
  , -- | @compareOp@ is a 'Vulkan.Core10.Enums.CompareOp.CompareOp' value
    -- specifying the comparison operator used in the stencil test.
    --
    -- @compareOp@ /must/ be a valid 'Vulkan.Core10.Enums.CompareOp.CompareOp'
    -- value
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
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (StencilOpState)
#endif
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
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @depthCompareOp@ /must/ be a valid
--     'Vulkan.Core10.Enums.CompareOp.CompareOp' value
--
-- -   @front@ /must/ be a valid 'StencilOpState' structure
--
-- -   @back@ /must/ be a valid 'StencilOpState' structure
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.CompareOp.CompareOp', 'GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlags.PipelineDepthStencilStateCreateFlags',
-- 'StencilOpState', 'Vulkan.Core10.Enums.StructureType.StructureType'
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
    -- 'Vulkan.Core10.FundamentalTypes.TRUE'. Depth writes are always disabled
    -- when @depthTestEnable@ is 'Vulkan.Core10.FundamentalTypes.FALSE'.
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
  , -- | @minDepthBounds@ is the minimum depth bound used in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-dbt depth bounds test>.
    minDepthBounds :: Float
  , -- | @maxDepthBounds@ is the maximum depth bound used in the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-dbt depth bounds test>.
    maxDepthBounds :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineDepthStencilStateCreateInfo)
#endif
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
-- 'Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV' will be generated.
--
-- == Valid Usage
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be
--     a valid handle to a graphics 'Vulkan.Core10.Handles.Pipeline'
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be a valid index into the calling command’s @pCreateInfos@ parameter
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not -1, @basePipelineHandle@ /must/
--     be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   If @flags@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be -1
--
-- -   The @stage@ member of each element of @pStages@ /must/ be unique
--
-- -   The geometric shader stages provided in @pStages@ /must/ be either
--     from the mesh shading pipeline (@stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_NV'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV')
--     or from the primitive shading pipeline (@stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT')
--
-- -   The @stage@ member of one element of @pStages@ /must/ be either
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT' or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV'
--
-- -   The @stage@ member of each element of @pStages@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
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
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST'
--
-- -   If the @topology@ member of @pInputAssembly@ is
--     'Vulkan.Core10.Enums.PrimitiveTopology.PRIMITIVE_TOPOLOGY_PATCH_LIST',
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
--     'Vulkan.Core10.APIConstants.ATTACHMENT_UNUSED' in @subpass@
--
-- -   The shader code for the entry points identified by @pStages@, and
--     the rest of the state identified by this structure /must/ adhere to
--     the pipeline linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   If rasterization is not disabled and @subpass@ uses a depth\/stencil
--     attachment in @renderPass@ that has a layout of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL'
--     in the 'Vulkan.Core10.Pass.AttachmentReference' defined by
--     @subpass@, the @depthWriteEnable@ member of @pDepthStencilState@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   If rasterization is not disabled and @subpass@ uses a depth\/stencil
--     attachment in @renderPass@ that has a layout of
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     or
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL'
--     in the 'Vulkan.Core10.Pass.AttachmentReference' defined by
--     @subpass@, the @failOp@, @passOp@ and @depthFailOp@ members of each
--     of the @front@ and @back@ members of @pDepthStencilState@ /must/ be
--     'Vulkan.Core10.Enums.StencilOp.STENCIL_OP_KEEP'
--
-- -   If rasterization is not disabled and the subpass uses color
--     attachments, then for each color attachment in the subpass the
--     @blendEnable@ member of the corresponding element of the
--     @pAttachment@ member of @pColorBlendState@ /must/ be
--     'Vulkan.Core10.FundamentalTypes.FALSE' if the attached image’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-format-features format features>
--     does not contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT'
--
-- -   If rasterization is not disabled and the subpass uses color
--     attachments, the @attachmentCount@ member of @pColorBlendState@
--     /must/ be equal to the @colorAttachmentCount@ used to create
--     @subpass@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT', the
--     @pViewports@ member of @pViewportState@ /must/ be a valid pointer to
--     an array of @pViewportState->viewportCount@ valid 'Viewport'
--     structures
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR', the
--     @pScissors@ member of @pViewportState@ /must/ be a valid pointer to
--     an array of @pViewportState->scissorCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   If the wide lines feature is not enabled, and no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_WIDTH', the
--     @lineWidth@ member of @pRasterizationState@ /must/ be @1.0@
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', @pViewportState@ /must/ be a
--     valid pointer to a valid 'PipelineViewportStateCreateInfo' structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', @pMultisampleState@ /must/
--     be a valid pointer to a valid 'PipelineMultisampleStateCreateInfo'
--     structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and @subpass@ uses a
--     depth\/stencil attachment, @pDepthStencilState@ /must/ be a valid
--     pointer to a valid 'PipelineDepthStencilStateCreateInfo' structure
--
-- -   If the @rasterizerDiscardEnable@ member of @pRasterizationState@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', and @subpass@ uses color
--     attachments, @pColorBlendState@ /must/ be a valid pointer to a valid
--     'PipelineColorBlendStateCreateInfo' structure
--
-- -   If the depth bias clamping feature is not enabled, no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BIAS', and the
--     @depthBiasEnable@ member of @pRasterizationState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the @depthBiasClamp@ member
--     of @pRasterizationState@ /must/ be @0.0@
--
-- -   If the @VK_EXT_depth_range_unrestricted@ extension is not enabled
--     and no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS', and
--     the @depthBoundsTestEnable@ member of @pDepthStencilState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the @minDepthBounds@ and
--     @maxDepthBounds@ members of @pDepthStencilState@ /must/ be between
--     @0.0@ and @1.0@, inclusive
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT',
--     and the @sampleLocationsEnable@ member of a
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pMultisampleState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     @sampleLocationsInfo.sampleLocationGridSize.width@ /must/ evenly
--     divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.width@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT',
--     and the @sampleLocationsEnable@ member of a
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pMultisampleState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     @sampleLocationsInfo.sampleLocationGridSize.height@ /must/ evenly
--     divide
--     'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT'::@sampleLocationGridSize.height@
--     as returned by
--     'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT'
--     with a @samples@ parameter equaling @rasterizationSamples@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT',
--     and the @sampleLocationsEnable@ member of a
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pMultisampleState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     @sampleLocationsInfo.sampleLocationsPerPixel@ /must/ equal
--     @rasterizationSamples@
--
-- -   If the @sampleLocationsEnable@ member of a
--     'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pMultisampleState@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the fragment shader code
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
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateInfoNV'::@coverageReductionMode@,
--     the @rasterizationSamples@ member of @pMultisampleState@ and the
--     sample counts for the color and depth\/stencil attachments (if the
--     subpass has them) /must/ be a valid combination returned by
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
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
--     shaders
--
-- -   If the @renderPass@ has multiview enabled and @subpass@ has more
--     than one bit set in the view mask and @multiviewGeometryShader@ is
--     not enabled, then @pStages@ /must/ not include a geometry shader
--
-- -   If the @renderPass@ has multiview enabled and @subpass@ has more
--     than one bit set in the view mask, shaders in the pipeline /must/
--     not write to the @Layer@ built-in output
--
-- -   If the @renderPass@ has multiview enabled, then all shaders /must/
--     not include variables decorated with the @Layer@ built-in decoration
--     in their interfaces
--
-- -   @flags@ /must/ not contain the
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.PIPELINE_CREATE_DISPATCH_BASE'
--     flag
--
-- -   If @pStages@ includes a fragment shader stage and an input
--     attachment was referenced by an @aspectMask@ at @renderPass@
--     creation time, its shader code /must/ only read from the aspects
--     that were specified for that input attachment
--
-- -   The number of resources in @layout@ accessible to each shader stage
--     that is used by the pipeline /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
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
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXCLUSIVE_SCISSOR_NV',
--     and if @pViewportState->pNext@ chain includes a
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV'
--     structure, and if its @exclusiveScissorCount@ member is not @0@,
--     then its @pExclusiveScissors@ member /must/ be a valid pointer to an
--     array of @exclusiveScissorCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SHADING_RATE_PALETTE_NV',
--     and if @pViewportState->pNext@ chain includes a
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV'
--     structure, then its @pShadingRatePalettes@ member /must/ be a valid
--     pointer to an array of @viewportCount@ valid
--     'Vulkan.Extensions.VK_NV_shading_rate_image.ShadingRatePaletteNV'
--     structures
--
-- -   If no element of the @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DISCARD_RECTANGLE_EXT',
--     and if @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT'
--     structure, and if its @discardRectangleCount@ member is not @0@,
--     then its @pDiscardRectangles@ member /must/ be a valid pointer to an
--     array of @discardRectangleCount@
--     'Vulkan.Core10.FundamentalTypes.Rect2D' structures
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
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--     value other than zero is specified, all variables in the output
--     interface of the entry point being compiled decorated with
--     @Position@, @PointSize@, @ClipDistance@, or @CullDistance@ /must/
--     all be decorated with identical @Stream@ values that match the
--     @rasterizationStream@
--
-- -   If
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT'::@rasterizationStream@
--     is zero, or not specified, all variables in the output interface of
--     the entry point being compiled decorated with @Position@,
--     @PointSize@, @ClipDistance@, or @CullDistance@ /must/ all be
--     decorated with a @Stream@ value of zero, or /must/ not specify the
--     @Stream@ decoration
--
-- -   If the last vertex processing stage is a geometry shader, and that
--     geometry shader uses the @GeometryStreams@ capability, then
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackFeaturesEXT'::@geometryStreams@
--     feature /must/ be enabled
--
-- -   If there are any mesh shader stages in the pipeline there /must/ not
--     be any shader stage in the pipeline with a @Xfb@ execution mode
--
-- -   If the @lineRasterizationMode@ member of a
--     'Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'
--     structure included in the @pNext@ chain of @pRasterizationState@ is
--     'Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_BRESENHAM_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_line_rasterization.LINE_RASTERIZATION_MODE_RECTANGULAR_SMOOTH_EXT'
--     and if rasterization is enabled, then the @alphaToCoverageEnable@,
--     @alphaToOneEnable@, and @sampleShadingEnable@ members of
--     @pMultisampleState@ /must/ all be
--     'Vulkan.Core10.FundamentalTypes.FALSE'
--
-- -   If the @stippledLineEnable@ member of
--     'Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'
--     is 'Vulkan.Core10.FundamentalTypes.TRUE' and no element of the
--     @pDynamicStates@ member of @pDynamicState@ is
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_EXT',
--     then the @lineStippleFactor@ member of
--     'Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT'
--     /must/ be in the range [1,256]
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- -   @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-extendedDynamicState extendedDynamicState>
--     feature is not enabled, there /must/ be no element of the
--     @pDynamicStates@ member of @pDynamicState@ set to
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CULL_MODE_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRONT_FACE_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PRIMITIVE_TOPOLOGY_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_BINDING_STRIDE_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_TEST_ENABLE_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_WRITE_ENABLE_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_COMPARE_OP_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_BOUNDS_TEST_ENABLE_EXT',
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_TEST_ENABLE_EXT',
--     or 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_STENCIL_OP_EXT'
--
-- -   If
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_WITH_COUNT_EXT'
--     is included in the @pDynamicStates@ array then @viewportCount@
--     /must/ be zero
--
-- -   If
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SCISSOR_WITH_COUNT_EXT'
--     is included in the @pDynamicStates@ array then @scissorCount@ /must/
--     be zero
--
-- -   If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- -   If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV',
--     then all stages /must/ not specify @Xfb@ execution mode
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsPipelineShaderGroupsCreateInfoNV',
--     'Vulkan.Extensions.VK_AMD_pipeline_compiler_control.PipelineCompilerControlCreateInfoAMD',
--     'Vulkan.Extensions.VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfoEXT',
--     'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT',
--     or
--     'Vulkan.Extensions.VK_NV_representative_fragment_test.PipelineRepresentativeFragmentTestStateCreateInfoNV'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
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
-- -   @layout@ /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout'
--     handle
--
-- -   @renderPass@ /must/ be a valid 'Vulkan.Core10.Handles.RenderPass'
--     handle
--
-- -   @stageCount@ /must/ be greater than @0@
--
-- -   Each of @basePipelineHandle@, @layout@, and @renderPass@ that are
--     valid handles of non-ignored parameters /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Pipeline', 'PipelineColorBlendStateCreateInfo',
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'PipelineDepthStencilStateCreateInfo', 'PipelineDynamicStateCreateInfo',
-- 'PipelineInputAssemblyStateCreateInfo',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'PipelineMultisampleStateCreateInfo',
-- 'PipelineRasterizationStateCreateInfo', 'PipelineShaderStageCreateInfo',
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
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GraphicsPipelineCreateInfo (es :: [Type]))
#endif
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
    | Just Refl <- eqT @e @GraphicsPipelineShaderGroupsCreateInfoNV = Just f
    | otherwise = Nothing

instance (Extendss GraphicsPipelineCreateInfo es, PokeChain es) => ToCStruct (GraphicsPipelineCreateInfo es) where
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

instance (Extendss GraphicsPipelineCreateInfo es, PeekChain es) => FromCStruct (GraphicsPipelineCreateInfo es) where
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

