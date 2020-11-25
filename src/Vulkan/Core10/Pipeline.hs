{-# language CPP #-}
-- No documentation found for Chapter "Pipeline"
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
                               , FrontFace(..)
                               , BlendFactor(..)
                               , BlendOp(..)
                               , StencilOp(..)
                               , LogicOp(..)
                               , VertexInputRate(..)
                               , DynamicState(..)
                               , CullModeFlagBits(..)
                               , CullModeFlags
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
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shading_rate_enums (PipelineFragmentShadingRateEnumStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PipelineFragmentShadingRateStateCreateInfoKHR)
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

-- No documentation found for TopLevel "vkCreateGraphicsPipelines"
createGraphicsPipelines :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkCreateGraphicsPipelines" "device"
                           Device
                        -> -- No documentation found for Nested "vkCreateGraphicsPipelines" "pipelineCache"
                           PipelineCache
                        -> -- No documentation found for Nested "vkCreateGraphicsPipelines" "pCreateInfos"
                           ("createInfos" ::: Vector (SomeStruct GraphicsPipelineCreateInfo))
                        -> -- No documentation found for Nested "vkCreateGraphicsPipelines" "pAllocator"
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
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
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

-- No documentation found for TopLevel "vkCreateComputePipelines"
createComputePipelines :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkCreateComputePipelines" "device"
                          Device
                       -> -- No documentation found for Nested "vkCreateComputePipelines" "pipelineCache"
                          PipelineCache
                       -> -- No documentation found for Nested "vkCreateComputePipelines" "pCreateInfos"
                          ("createInfos" ::: Vector (SomeStruct ComputePipelineCreateInfo))
                       -> -- No documentation found for Nested "vkCreateComputePipelines" "pAllocator"
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
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
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

-- No documentation found for TopLevel "vkDestroyPipeline"
destroyPipeline :: forall io
                 . (MonadIO io)
                => -- No documentation found for Nested "vkDestroyPipeline" "device"
                   Device
                -> -- No documentation found for Nested "vkDestroyPipeline" "pipeline"
                   Pipeline
                -> -- No documentation found for Nested "vkDestroyPipeline" "pAllocator"
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



-- No documentation found for TopLevel "VkViewport"
data Viewport = Viewport
  { -- No documentation found for Nested "VkViewport" "x"
    x :: Float
  , -- No documentation found for Nested "VkViewport" "y"
    y :: Float
  , -- No documentation found for Nested "VkViewport" "width"
    width :: Float
  , -- No documentation found for Nested "VkViewport" "height"
    height :: Float
  , -- No documentation found for Nested "VkViewport" "minDepth"
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



-- No documentation found for TopLevel "VkSpecializationMapEntry"
data SpecializationMapEntry = SpecializationMapEntry
  { -- No documentation found for Nested "VkSpecializationMapEntry" "constantID"
    constantID :: Word32
  , -- No documentation found for Nested "VkSpecializationMapEntry" "offset"
    offset :: Word32
  , -- No documentation found for Nested "VkSpecializationMapEntry" "size"
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



-- No documentation found for TopLevel "VkSpecializationInfo"
data SpecializationInfo = SpecializationInfo
  { -- No documentation found for Nested "VkSpecializationInfo" "pMapEntries"
    mapEntries :: Vector SpecializationMapEntry
  , -- No documentation found for Nested "VkSpecializationInfo" "dataSize"
    dataSize :: Word64
  , -- No documentation found for Nested "VkSpecializationInfo" "pData"
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
    lift $ Data.Vector.imapM_ (\i e -> poke (pPMapEntries' `plusPtr` (16 * (i)) :: Ptr SpecializationMapEntry) (e)) (mapEntries)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr SpecializationMapEntry))) (pPMapEntries')
    lift $ poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (dataSize))
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (data')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    pPMapEntries' <- ContT $ allocaBytesAligned @SpecializationMapEntry ((Data.Vector.length (mempty)) * 16) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPMapEntries' `plusPtr` (16 * (i)) :: Ptr SpecializationMapEntry) (e)) (mempty)
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



-- No documentation found for TopLevel "VkPipelineShaderStageCreateInfo"
data PipelineShaderStageCreateInfo (es :: [Type]) = PipelineShaderStageCreateInfo
  { -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "flags"
    flags :: PipelineShaderStageCreateFlags
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "stage"
    stage :: ShaderStageFlagBits
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "module"
    module' :: ShaderModule
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "pName"
    name :: ByteString
  , -- No documentation found for Nested "VkPipelineShaderStageCreateInfo" "pSpecializationInfo"
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



-- No documentation found for TopLevel "VkComputePipelineCreateInfo"
data ComputePipelineCreateInfo (es :: [Type]) = ComputePipelineCreateInfo
  { -- No documentation found for Nested "VkComputePipelineCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "flags"
    flags :: PipelineCreateFlags
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "stage"
    stage :: SomeStruct PipelineShaderStageCreateInfo
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "layout"
    layout :: PipelineLayout
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "basePipelineHandle"
    basePipelineHandle :: Pipeline
  , -- No documentation found for Nested "VkComputePipelineCreateInfo" "basePipelineIndex"
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



-- No documentation found for TopLevel "VkVertexInputBindingDescription"
data VertexInputBindingDescription = VertexInputBindingDescription
  { -- No documentation found for Nested "VkVertexInputBindingDescription" "binding"
    binding :: Word32
  , -- No documentation found for Nested "VkVertexInputBindingDescription" "stride"
    stride :: Word32
  , -- No documentation found for Nested "VkVertexInputBindingDescription" "inputRate"
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



-- No documentation found for TopLevel "VkVertexInputAttributeDescription"
data VertexInputAttributeDescription = VertexInputAttributeDescription
  { -- No documentation found for Nested "VkVertexInputAttributeDescription" "location"
    location :: Word32
  , -- No documentation found for Nested "VkVertexInputAttributeDescription" "binding"
    binding :: Word32
  , -- No documentation found for Nested "VkVertexInputAttributeDescription" "format"
    format :: Format
  , -- No documentation found for Nested "VkVertexInputAttributeDescription" "offset"
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



-- No documentation found for TopLevel "VkPipelineVertexInputStateCreateInfo"
data PipelineVertexInputStateCreateInfo (es :: [Type]) = PipelineVertexInputStateCreateInfo
  { -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "flags"
    flags :: PipelineVertexInputStateCreateFlags
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "pVertexBindingDescriptions"
    vertexBindingDescriptions :: Vector VertexInputBindingDescription
  , -- No documentation found for Nested "VkPipelineVertexInputStateCreateInfo" "pVertexAttributeDescriptions"
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
    lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexBindingDescriptions' `plusPtr` (12 * (i)) :: Ptr VertexInputBindingDescription) (e)) (vertexBindingDescriptions)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDescription))) (pPVertexBindingDescriptions')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (vertexAttributeDescriptions)) :: Word32))
    pPVertexAttributeDescriptions' <- ContT $ allocaBytesAligned @VertexInputAttributeDescription ((Data.Vector.length (vertexAttributeDescriptions)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexAttributeDescriptions' `plusPtr` (16 * (i)) :: Ptr VertexInputAttributeDescription) (e)) (vertexAttributeDescriptions)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr VertexInputAttributeDescription))) (pPVertexAttributeDescriptions')
    lift $ f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    pPVertexBindingDescriptions' <- ContT $ allocaBytesAligned @VertexInputBindingDescription ((Data.Vector.length (mempty)) * 12) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexBindingDescriptions' `plusPtr` (12 * (i)) :: Ptr VertexInputBindingDescription) (e)) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr VertexInputBindingDescription))) (pPVertexBindingDescriptions')
    pPVertexAttributeDescriptions' <- ContT $ allocaBytesAligned @VertexInputAttributeDescription ((Data.Vector.length (mempty)) * 16) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPVertexAttributeDescriptions' `plusPtr` (16 * (i)) :: Ptr VertexInputAttributeDescription) (e)) (mempty)
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



-- No documentation found for TopLevel "VkPipelineInputAssemblyStateCreateInfo"
data PipelineInputAssemblyStateCreateInfo = PipelineInputAssemblyStateCreateInfo
  { -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "flags"
    flags :: PipelineInputAssemblyStateCreateFlags
  , -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "topology"
    topology :: PrimitiveTopology
  , -- No documentation found for Nested "VkPipelineInputAssemblyStateCreateInfo" "primitiveRestartEnable"
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



-- No documentation found for TopLevel "VkPipelineTessellationStateCreateInfo"
data PipelineTessellationStateCreateInfo (es :: [Type]) = PipelineTessellationStateCreateInfo
  { -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "flags"
    flags :: PipelineTessellationStateCreateFlags
  , -- No documentation found for Nested "VkPipelineTessellationStateCreateInfo" "patchControlPoints"
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



-- No documentation found for TopLevel "VkPipelineViewportStateCreateInfo"
data PipelineViewportStateCreateInfo (es :: [Type]) = PipelineViewportStateCreateInfo
  { -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "flags"
    flags :: PipelineViewportStateCreateFlags
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "viewportCount"
    viewportCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "pViewports"
    viewports :: Vector Viewport
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "scissorCount"
    scissorCount :: Word32
  , -- No documentation found for Nested "VkPipelineViewportStateCreateInfo" "pScissors"
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
        pPScissors <- ContT $ allocaBytesAligned @Rect2D (((Data.Vector.length (scissors))) * 16) 4
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



-- No documentation found for TopLevel "VkPipelineRasterizationStateCreateInfo"
data PipelineRasterizationStateCreateInfo (es :: [Type]) = PipelineRasterizationStateCreateInfo
  { -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "flags"
    flags :: PipelineRasterizationStateCreateFlags
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "depthClampEnable"
    depthClampEnable :: Bool
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "rasterizerDiscardEnable"
    rasterizerDiscardEnable :: Bool
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "polygonMode"
    polygonMode :: PolygonMode
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "cullMode"
    cullMode :: CullModeFlags
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "frontFace"
    frontFace :: FrontFace
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "depthBiasEnable"
    depthBiasEnable :: Bool
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "depthBiasConstantFactor"
    depthBiasConstantFactor :: Float
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "depthBiasClamp"
    depthBiasClamp :: Float
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "depthBiasSlopeFactor"
    depthBiasSlopeFactor :: Float
  , -- No documentation found for Nested "VkPipelineRasterizationStateCreateInfo" "lineWidth"
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



-- No documentation found for TopLevel "VkPipelineMultisampleStateCreateInfo"
data PipelineMultisampleStateCreateInfo (es :: [Type]) = PipelineMultisampleStateCreateInfo
  { -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "flags"
    flags :: PipelineMultisampleStateCreateFlags
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "rasterizationSamples"
    rasterizationSamples :: SampleCountFlagBits
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "sampleShadingEnable"
    sampleShadingEnable :: Bool
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "minSampleShading"
    minSampleShading :: Float
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "pSampleMask"
    sampleMask :: Vector SampleMask
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "alphaToCoverageEnable"
    alphaToCoverageEnable :: Bool
  , -- No documentation found for Nested "VkPipelineMultisampleStateCreateInfo" "alphaToOneEnable"
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



-- No documentation found for TopLevel "VkPipelineColorBlendAttachmentState"
data PipelineColorBlendAttachmentState = PipelineColorBlendAttachmentState
  { -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "blendEnable"
    blendEnable :: Bool
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "srcColorBlendFactor"
    srcColorBlendFactor :: BlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "dstColorBlendFactor"
    dstColorBlendFactor :: BlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "colorBlendOp"
    colorBlendOp :: BlendOp
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "srcAlphaBlendFactor"
    srcAlphaBlendFactor :: BlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "dstAlphaBlendFactor"
    dstAlphaBlendFactor :: BlendFactor
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "alphaBlendOp"
    alphaBlendOp :: BlendOp
  , -- No documentation found for Nested "VkPipelineColorBlendAttachmentState" "colorWriteMask"
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



-- No documentation found for TopLevel "VkPipelineColorBlendStateCreateInfo"
data PipelineColorBlendStateCreateInfo (es :: [Type]) = PipelineColorBlendStateCreateInfo
  { -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "flags"
    flags :: PipelineColorBlendStateCreateFlags
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "logicOpEnable"
    logicOpEnable :: Bool
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "logicOp"
    logicOp :: LogicOp
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "pAttachments"
    attachments :: Vector PipelineColorBlendAttachmentState
  , -- No documentation found for Nested "VkPipelineColorBlendStateCreateInfo" "blendConstants"
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
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAttachments' `plusPtr` (32 * (i)) :: Ptr PipelineColorBlendAttachmentState) (e)) (attachments)
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
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAttachments' `plusPtr` (32 * (i)) :: Ptr PipelineColorBlendAttachmentState) (e)) (mempty)
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



-- No documentation found for TopLevel "VkPipelineDynamicStateCreateInfo"
data PipelineDynamicStateCreateInfo = PipelineDynamicStateCreateInfo
  { -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "flags"
    flags :: PipelineDynamicStateCreateFlags
  , -- No documentation found for Nested "VkPipelineDynamicStateCreateInfo" "pDynamicStates"
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



-- No documentation found for TopLevel "VkStencilOpState"
data StencilOpState = StencilOpState
  { -- No documentation found for Nested "VkStencilOpState" "failOp"
    failOp :: StencilOp
  , -- No documentation found for Nested "VkStencilOpState" "passOp"
    passOp :: StencilOp
  , -- No documentation found for Nested "VkStencilOpState" "depthFailOp"
    depthFailOp :: StencilOp
  , -- No documentation found for Nested "VkStencilOpState" "compareOp"
    compareOp :: CompareOp
  , -- No documentation found for Nested "VkStencilOpState" "compareMask"
    compareMask :: Word32
  , -- No documentation found for Nested "VkStencilOpState" "writeMask"
    writeMask :: Word32
  , -- No documentation found for Nested "VkStencilOpState" "reference"
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



-- No documentation found for TopLevel "VkPipelineDepthStencilStateCreateInfo"
data PipelineDepthStencilStateCreateInfo = PipelineDepthStencilStateCreateInfo
  { -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "flags"
    flags :: PipelineDepthStencilStateCreateFlags
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "depthTestEnable"
    depthTestEnable :: Bool
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "depthWriteEnable"
    depthWriteEnable :: Bool
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "depthCompareOp"
    depthCompareOp :: CompareOp
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "depthBoundsTestEnable"
    depthBoundsTestEnable :: Bool
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "stencilTestEnable"
    stencilTestEnable :: Bool
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "front"
    front :: StencilOpState
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "back"
    back :: StencilOpState
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "minDepthBounds"
    minDepthBounds :: Float
  , -- No documentation found for Nested "VkPipelineDepthStencilStateCreateInfo" "maxDepthBounds"
    maxDepthBounds :: Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineDepthStencilStateCreateInfo)
#endif
deriving instance Show PipelineDepthStencilStateCreateInfo

instance ToCStruct PipelineDepthStencilStateCreateInfo where
  withCStruct x f = allocaBytesAligned 104 8 $ \p -> pokeCStruct p x (f p)
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
             flags (bool32ToBool depthTestEnable) (bool32ToBool depthWriteEnable) depthCompareOp (bool32ToBool depthBoundsTestEnable) (bool32ToBool stencilTestEnable) front back ((\(CFloat a) -> a) minDepthBounds) ((\(CFloat a) -> a) maxDepthBounds)


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



-- No documentation found for TopLevel "VkGraphicsPipelineCreateInfo"
data GraphicsPipelineCreateInfo (es :: [Type]) = GraphicsPipelineCreateInfo
  { -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "flags"
    flags :: PipelineCreateFlags
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pStages"
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pVertexInputState"
    vertexInputState :: Maybe (SomeStruct PipelineVertexInputStateCreateInfo)
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pInputAssemblyState"
    inputAssemblyState :: Maybe PipelineInputAssemblyStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pTessellationState"
    tessellationState :: Maybe (SomeStruct PipelineTessellationStateCreateInfo)
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pViewportState"
    viewportState :: Maybe (SomeStruct PipelineViewportStateCreateInfo)
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pRasterizationState"
    rasterizationState :: SomeStruct PipelineRasterizationStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pMultisampleState"
    multisampleState :: Maybe (SomeStruct PipelineMultisampleStateCreateInfo)
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pDepthStencilState"
    depthStencilState :: Maybe PipelineDepthStencilStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pColorBlendState"
    colorBlendState :: Maybe (SomeStruct PipelineColorBlendStateCreateInfo)
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "pDynamicState"
    dynamicState :: Maybe PipelineDynamicStateCreateInfo
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "layout"
    layout :: PipelineLayout
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "renderPass"
    renderPass :: RenderPass
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "subpass"
    subpass :: Word32
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "basePipelineHandle"
    basePipelineHandle :: Pipeline
  , -- No documentation found for Nested "VkGraphicsPipelineCreateInfo" "basePipelineIndex"
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
    | Just Refl <- eqT @e @PipelineFragmentShadingRateEnumStateCreateInfoNV = Just f
    | Just Refl <- eqT @e @PipelineFragmentShadingRateStateCreateInfoKHR = Just f
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

