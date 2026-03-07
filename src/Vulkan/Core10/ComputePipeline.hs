{-# language CPP #-}
-- No documentation found for Chapter "ComputePipeline"
module Vulkan.Core10.ComputePipeline  ( createComputePipelines
                                      , withComputePipelines
                                      , destroyPipeline
                                      , SpecializationMapEntry(..)
                                      , SpecializationInfo(..)
                                      , PipelineShaderStageCreateInfo(..)
                                      , ComputePipelineCreateInfo(..)
                                      , Pipeline(..)
                                      , ShaderStageFlagBits(..)
                                      , ShaderStageFlags
                                      , PipelineCreateFlagBits(..)
                                      , PipelineCreateFlags
                                      , PipelineShaderStageCreateFlagBits(..)
                                      , PipelineShaderStageCreateFlags
                                      , PipelineLayoutCreateFlagBits(..)
                                      , PipelineLayoutCreateFlags
                                      ) where

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
import Foreign.Ptr (plusPtr)
import Data.ByteString (packCString)
import Data.ByteString (useAsCString)
import Data.Coerce (coerce)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(..))
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
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands_compute (ComputePipelineIndirectBufferInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsObjectNameInfoEXT)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateComputePipelines))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyPipeline))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_binary (PipelineBinaryInfoKHR)
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_pipeline_compiler_control (PipelineCompilerControlCreateInfoAMD)
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap (PipelineCreateFlags2CreateInfo)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfo)
import Vulkan.Core10.Handles (PipelineLayout)
import {-# SOURCE #-} Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality' (PipelineRobustnessCreateInfo)
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_module_identifier (PipelineShaderStageModuleIdentifierCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMDX_shader_enqueue (PipelineShaderStageNodeCreateInfoAMDX)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control (PipelineShaderStageRequiredSubgroupSizeCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_heap (ShaderDescriptorSetAndBindingMappingInfoEXT)
import Vulkan.Core10.Handles (ShaderModule)
import {-# SOURCE #-} Vulkan.Core10.Shader (ShaderModuleCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_cache (ShaderModuleValidationCacheCreateInfoEXT)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits)
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Extensions.VK_HUAWEI_subpass_shading (SubpassShadingPipelineCreateInfoHUAWEI)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits (PipelineLayoutCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineLayoutCreateFlagBits (PipelineLayoutCreateFlags)
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(..))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateComputePipelines
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct ComputePipelineCreateInfo) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct ComputePipelineCreateInfo) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateComputePipelines - Creates a new compute pipeline object
--
-- = Description
--
-- Pipelines are created and returned as described for
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-multiple Multiple Pipeline Creation>.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateComputePipelines-device-09661# @device@ /must/ support
--     at least one queue family with the
--     'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT' capability
--
-- -   #VUID-vkCreateComputePipelines-flags-00695# If the @flags@ member of
--     any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   #VUID-vkCreateComputePipelines-flags-00696# If the @flags@ member of
--     any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- -   #VUID-vkCreateComputePipelines-pipelineCache-02873# If
--     @pipelineCache@ was created with
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT',
--     host access to @pipelineCache@ /must/ be
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>
--
-- -   #VUID-vkCreateComputePipelines-pNext-09616# If
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR'::@binaryCount@
--     is not @0@ for any element of @pCreateInfos@, @pipelineCache@ /must/
--     be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCreateComputePipelines-pNext-09617# If a
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'
--     structure with the
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'
--     flag set is included in the @pNext@ chain of any element of
--     @pCreateInfos@, @pipelineCache@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCreateComputePipelines-binaryCount-09620# If
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR'::@binaryCount@
--     is not @0@ for any element of @pCreateInfos@,
--     'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PIPELINE_CREATION_FEEDBACK_APPLICATION_PIPELINE_CACHE_HIT_BIT'
--     /must/ not be set in the @flags@ of that element
--
-- -   #VUID-vkCreateComputePipelines-binaryCount-09621# If
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR'::@binaryCount@
--     is not @0@ for any element of @pCreateInfos@,
--     'Vulkan.Core13.Enums.PipelineCreationFeedbackFlagBits.PIPELINE_CREATION_FEEDBACK_BASE_PIPELINE_ACCELERATION_BIT'
--     /must/ not be set in the @flags@ of that element
--
-- -   #VUID-vkCreateComputePipelines-binaryCount-09622# If
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR'::@binaryCount@
--     is not @0@ for any element of @pCreateInfos@,
--     'Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--     /must/ not be set in the @flags@ of that element
--
-- -   #VUID-vkCreateComputePipelines-pCreateInfos-11414# If any element of
--     @pCreateInfos@ sets
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
-- -   #VUID-vkCreateComputePipelines-pCreateInfos-11429# If any element of
--     @pCreateInfos@ sets
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT'
--     and includes embedded sampler mappings, this command /must/ not
--     cause the total number of unique embedded samplers in pipelines and
--     shaders on this device to exceed
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxDescriptorHeapEmbeddedSamplers maxDescriptorHeapEmbeddedSamplers>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateComputePipelines-device-parameter# @device@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateComputePipelines-pipelineCache-parameter# If
--     @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   #VUID-vkCreateComputePipelines-pCreateInfos-parameter#
--     @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'ComputePipelineCreateInfo' structures
--
-- -   #VUID-vkCreateComputePipelines-pAllocator-parameter# If @pAllocator@
--     is not @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateComputePipelines-pPipelines-parameter# @pPipelines@
--     /must/ be a valid pointer to an array of @createInfoCount@
--     'Vulkan.Core10.Handles.Pipeline' handles
--
-- -   #VUID-vkCreateComputePipelines-createInfoCount-arraylength#
--     @createInfoCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCreateComputePipelines-pipelineCache-parent# If
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
-- 'ComputePipelineCreateInfo', 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.Handles.Pipeline', 'Vulkan.Core10.Handles.PipelineCache'
createComputePipelines :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the logical device that creates the compute pipelines.
                          Device
                       -> -- | @pipelineCache@ is either 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                          -- indicating that pipeline caching is disabled, or to enable caching, the
                          -- handle of a valid 'Vulkan.Core10.Handles.PipelineCache' object. The
                          -- implementation /must/ not access this object outside of the duration of
                          -- this command.
                          PipelineCache
                       -> -- | @pCreateInfos@ is a pointer to an array of 'ComputePipelineCreateInfo'
                          -- structures.
                          ("createInfos" ::: Vector (SomeStruct ComputePipelineCreateInfo))
                       -> -- | @pAllocator@ controls host memory allocation as described in the
                          -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                          -- chapter.
                          ("allocator" ::: Maybe AllocationCallbacks)
                       -> io (Result, ("pipelines" ::: Vector Pipeline))
createComputePipelines device
                         pipelineCache
                         createInfos
                         allocator = liftIO . evalContT $ do
  let vkCreateComputePipelinesPtr = pVkCreateComputePipelines (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateComputePipelinesPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateComputePipelines is null" Nothing Nothing
  let vkCreateComputePipelines' = mkVkCreateComputePipelines vkCreateComputePipelinesPtr
  pPCreateInfos <- ContT $ allocaBytes @(ComputePipelineCreateInfo _) ((Data.Vector.length (createInfos)) * 96)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (96 * (i)) :: Ptr (ComputePipelineCreateInfo _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ traceAroundEvent "vkCreateComputePipelines" (vkCreateComputePipelines'
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
    (\(_, o1) -> traverse_ (\o1Elem -> destroyPipeline device
                                                         o1Elem
                                                         pAllocator) o1)


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
-- -   #VUID-vkDestroyPipeline-pipeline-00765# All submitted commands that
--     refer to @pipeline@ /must/ have completed execution
--
-- -   #VUID-vkDestroyPipeline-pipeline-00766# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @pipeline@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyPipeline-pipeline-00767# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @pipeline@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyPipeline-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyPipeline-pipeline-parameter# If @pipeline@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @pipeline@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-vkDestroyPipeline-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyPipeline-pipeline-parent# If @pipeline@ is a valid
--     handle, it /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @pipeline@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline'
destroyPipeline :: forall io
                 . (MonadIO io)
                => -- | @device@ is the logical device that destroys the pipeline.
                   Device
                -> -- | @pipeline@ is the handle of the pipeline to destroy.
                   Pipeline
                -> -- | @pAllocator@ controls host memory allocation as described in the
                   -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory-allocation Memory Allocation>
                   -- chapter.
                   ("allocator" ::: Maybe AllocationCallbacks)
                -> io ()
destroyPipeline device pipeline allocator = liftIO . evalContT $ do
  let vkDestroyPipelinePtr = pVkDestroyPipeline (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyPipelinePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyPipeline is null" Nothing Nothing
  let vkDestroyPipeline' = mkVkDestroyPipeline vkDestroyPipelinePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyPipeline" (vkDestroyPipeline'
                                                 (deviceHandle (device))
                                                 (pipeline)
                                                 pAllocator)
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
-- -   #VUID-VkSpecializationMapEntry-constantID-00776# For a @constantID@
--     specialization constant declared in a shader, @size@ /must/ match
--     the byte size of the @constantID@. If the specialization constant is
--     of type @boolean@, @size@ /must/ be the byte size of
--     'Vulkan.Core10.FundamentalTypes.Bool32'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
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
             constantID offset (coerce @CSize @Word64 size)

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


-- | VkSpecializationInfo - Structure specifying specialization information
--
-- == Valid Usage
--
-- -   #VUID-VkSpecializationInfo-offset-00773# The @offset@ member of each
--     element of @pMapEntries@ /must/ be less than @dataSize@
--
-- -   #VUID-VkSpecializationInfo-pMapEntries-00774# The @size@ member of
--     each element of @pMapEntries@ /must/ be less than or equal to
--     @dataSize@ minus @offset@
--
-- -   #VUID-VkSpecializationInfo-constantID-04911# The @constantID@ value
--     of each element of @pMapEntries@ /must/ be unique within
--     @pMapEntries@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSpecializationInfo-pMapEntries-parameter# If @mapEntryCount@
--     is not @0@, @pMapEntries@ /must/ be a valid pointer to an array of
--     @mapEntryCount@ valid 'SpecializationMapEntry' structures
--
-- -   #VUID-VkSpecializationInfo-pData-parameter# If @dataSize@ is not
--     @0@, @pData@ /must/ be a valid pointer to an array of @dataSize@
--     bytes
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineShaderModuleCreateInfoARM',
-- 'PipelineShaderStageCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateInfoEXT',
-- 'SpecializationMapEntry'
data SpecializationInfo = SpecializationInfo
  { -- | @pMapEntries@ is a pointer to an array of 'SpecializationMapEntry'
    -- structures, which map constant IDs to offsets in @pData@.
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
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SpecializationInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (mapEntries)) :: Word32))
    pPMapEntries' <- ContT $ allocaBytes @SpecializationMapEntry ((Data.Vector.length (mapEntries)) * 16)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPMapEntries' `plusPtr` (16 * (i)) :: Ptr SpecializationMapEntry) (e)) (mapEntries)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr SpecializationMapEntry))) (pPMapEntries')
    lift $ poke ((p `plusPtr` 16 :: Ptr CSize)) (CSize (dataSize))
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (data')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct SpecializationInfo where
  peekCStruct p = do
    mapEntryCount <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    pMapEntries <- peek @(Ptr SpecializationMapEntry) ((p `plusPtr` 8 :: Ptr (Ptr SpecializationMapEntry)))
    pMapEntries' <- generateM (fromIntegral mapEntryCount) (\i -> peekCStruct @SpecializationMapEntry ((pMapEntries `advancePtrBytes` (16 * (i)) :: Ptr SpecializationMapEntry)))
    dataSize <- peek @CSize ((p `plusPtr` 16 :: Ptr CSize))
    pData <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ SpecializationInfo
             pMapEntries' (coerce @CSize @Word64 dataSize) pData

instance Zero SpecializationInfo where
  zero = SpecializationInfo
           mempty
           zero
           zero


-- | VkPipelineShaderStageCreateInfo - Structure specifying parameters of a
-- newly created pipeline shader stage
--
-- = Description
--
-- If @module@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', the shader
-- code used by the pipeline is defined by @module@. If @module@ is
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', the shader code is defined by
-- the chained 'Vulkan.Core10.Shader.ShaderModuleCreateInfo' if present.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-shaderModuleIdentifier shaderModuleIdentifier>
-- feature is enabled, applications /can/ omit shader code for @stage@ and
-- instead provide a module identifier. This is done by including a
-- 'Vulkan.Extensions.VK_EXT_shader_module_identifier.PipelineShaderStageModuleIdentifierCreateInfoEXT'
-- structure with @identifierSize@ not equal to 0 in the @pNext@ chain. A
-- shader stage created in this way is equivalent to one created using a
-- shader module with the same identifier. The identifier allows an
-- implementation to look up a pipeline without consuming a valid SPIR-V
-- module. If a pipeline is not found, pipeline compilation is not possible
-- and the implementation /must/ fail as specified by
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'.
--
-- When an identifier is used in lieu of a shader module, implementations
-- /may/ fail pipeline compilation with
-- 'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED' for any reason.
--
-- The rationale for the relaxed requirement on implementations to return a
-- pipeline with
-- 'Vulkan.Extensions.VK_EXT_shader_module_identifier.PipelineShaderStageModuleIdentifierCreateInfoEXT'
-- is that layers or tools may intercept pipeline creation calls and
-- require the full SPIR-V context to operate correctly. ICDs are not
-- expected to fail pipeline compilation if the pipeline exists in a cache
-- somewhere.
--
-- Applications /can/ use identifiers when creating pipelines with
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'.
-- When creating such pipelines, 'Vulkan.Core10.Enums.Result.SUCCESS' /may/
-- be returned, but subsequently fail when referencing the pipeline in a
-- 'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
-- struct. Applications /must/ allow pipeline compilation to fail during
-- link steps with
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
-- as it /may/ not be possible to determine if a pipeline /can/ be created
-- from identifiers until the link step.
--
-- == Valid Usage
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-descriptorHeap-11314# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-descriptorHeap descriptorHeap>
--     feature is not enabled,
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.ShaderDescriptorSetAndBindingMappingInfoEXT'::@mappingCount@
--     /must/ be 0
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-11315# If the @pNext@
--     chain specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_RESOURCE_HEAP_DATA_EXT',
--     the mapped resource in the shader /must/ be a variable with a
--     structure type decorated with @Block@ in the @Uniform@ @Storage@
--     @Class@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-11316# If the @pNext@
--     chain specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_DATA_EXT',
--     the mapped structure /must/ not be larger than the sum of
--     @pushDataOffset@ used in the mapping and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxPushDataSize maxPushDataSize>
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-11317# If the @pNext@
--     chain specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_DATA_EXT',
--     the sum of mapped structure size and @shaderRecordDataOffset@ used
--     in the mapping /must/ not be larger than
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxShaderGroupStride maxShaderGroupStride>
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-11318# If the @pNext@
--     chain specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT'
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     the mapped resource in the shader /must/ be one of:
--
--     -   A variable with a structure type decorated with @Block@ in the
--         @Uniform@ @Storage@ @Class@
--
--     -   A variable with a structure type decorated with @BufferBlock@ in
--         the @Uniform@ @Storage@ @Class@
--
--     -   A variable with a structure type decorated with @Block@ in the
--         @StorageBuffer@ @Storage@ @Class@
--
--     -   A @OpTypeAccelerationStructureKHR@ variable
--
--     -   A @OpTypeAccelerationStructureNV@ variable
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-11378# If the @pNext@
--     chain specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_PUSH_ADDRESS_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_SHADER_RECORD_ADDRESS_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_INDIRECT_ADDRESS_EXT',
--     the @OpArrayLength@ or @OpUntypedArrayLengthKHR@ instruction /must/
--     not be used on that resource
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-11399# If the @pNext@
--     chain specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorheaps-bindings descriptor mapping>
--     using
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_CONSTANT_OFFSET_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_PUSH_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_SHADER_RECORD_INDEX_EXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_EXT',
--     or
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DESCRIPTOR_MAPPING_SOURCE_HEAP_WITH_INDIRECT_INDEX_ARRAY_EXT',
--     and the mapped resource declaration is an array, the
--     @pEmbeddedSampler@ member of the corresponding mapping structure
--     /must/ be @NULL@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-00704# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-geometryShader geometryShader>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT'
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-00705# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-tessellationShader tessellationShader>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT'
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-02091# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-meshShader meshShaders>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-02092# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-taskShader taskShaders>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-clustercullingShader-07813# If
--     the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-clustercullingShader clustercullingShader>
--     feature is not enabled, @stage@ /must/ not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CLUSTER_CULLING_BIT_HUAWEI'
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-00706# @stage@ /must/
--     not be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL_GRAPHICS',
--     or 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL'
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pName-00707# @pName@ /must/ be
--     the name of an @OpEntryPoint@ in @module@ with an execution model
--     that matches @stage@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-maxClipDistances-00708# If the
--     identified entry point includes any variable in its interface that
--     is declared with the @ClipDistance@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxClipDistances@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-maxCullDistances-00709# If the
--     identified entry point includes any variable in its interface that
--     is declared with the @CullDistance@ @BuiltIn@ decoration, that
--     variable /must/ not have an array size greater than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxCullDistances@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-maxCombinedClipAndCullDistances-00710#
--     If the identified entry point includes variables in its interface
--     that are declared with the @ClipDistance@ @BuiltIn@ decoration and
--     variables in its interface that are declared with the @CullDistance@
--     @BuiltIn@ decoration, those variables /must/ not have array sizes
--     which sum to more than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxCombinedClipAndCullDistances@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-maxSampleMaskWords-00711# If
--     the identified entry point includes any variable in its interface
--     that is declared with the
--     'Vulkan.Core10.FundamentalTypes.SampleMask' @BuiltIn@ decoration,
--     that variable /must/ not have an array size greater than
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxSampleMaskWords@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-00713# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT'
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     and the identified entry point has an @OpExecutionMode@ instruction
--     specifying a patch size with @OutputVertices@, the patch size /must/
--     be greater than @0@ and less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxTessellationPatchSize@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-00714# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction specifying a maximum output vertex count that is greater
--     than @0@ and less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxGeometryOutputVertices@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-00715# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     the identified entry point /must/ have an @OpExecutionMode@
--     instruction specifying an invocation count that is greater than @0@
--     and less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxGeometryShaderInvocations@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-02596# If @stage@ is
--     either
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     and the identified entry point writes to @Layer@ for any primitive,
--     it /must/ write the same value to @Layer@ for all vertices of a
--     given primitive
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-02597# If @stage@ is
--     either
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_VERTEX_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_CONTROL_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_GEOMETRY_BIT',
--     and the identified entry point writes to @ViewportIndex@ for any
--     primitive, it /must/ write the same value to @ViewportIndex@ for all
--     vertices of a given primitive
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-06685# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and the identified entry point writes to @FragDepth@ in any
--     execution path, all execution paths that are not exclusive to helper
--     invocations /must/ either discard the fragment, or write or
--     initialize the value of @FragDepth@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-06686# If @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_FRAGMENT_BIT',
--     and the identified entry point writes to @FragStencilRefEXT@ in any
--     execution path, all execution paths that are not exclusive to helper
--     invocations /must/ either discard the fragment, or write or
--     initialize the value of @FragStencilRefEXT@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-flags-02784# If @flags@ has
--     the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT'
--     flag set, the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-subgroupSizeControl subgroupSizeControl>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-flags-02785# If @flags@ has
--     the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT'
--     flag set, the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-computeFullSubgroups computeFullSubgroups>
--     feature /must/ be enabled
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-flags-08988# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT',
--     @stage@ /must/ be one of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-02754# If a
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfo'
--     structure is included in the @pNext@ chain, @flags@ /must/ not have
--     the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT'
--     flag set
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-02755# If a
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfo'
--     structure is included in the @pNext@ chain, the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-subgroupSizeControl subgroupSizeControl>
--     feature /must/ be enabled, and @stage@ /must/ be a valid bit
--     specified in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-requiredSubgroupSizeStages requiredSubgroupSizeStages>
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-02756# If a
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfo'
--     structure is included in the @pNext@ chain and @stage@ is
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT',
--     the local workgroup size of the shader /must/ be less than or equal
--     to the product of
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfo'::@requiredSubgroupSize@
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxComputeWorkgroupSubgroups maxComputeWorkgroupSubgroups>
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-02757# If a
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfo'
--     structure is included in the @pNext@ chain, and @flags@ has the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT'
--     flag set, the local workgroup size in the X dimension of the
--     pipeline /must/ be a multiple of
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfo'::@requiredSubgroupSize@
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-flags-02758# If @flags@ has
--     both the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT'
--     and
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT'
--     flags set, the local workgroup size in the X dimension of the
--     pipeline /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-maxSubgroupSize maxSubgroupSize>
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-flags-02759# If @flags@ has
--     the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT'
--     flag set and @flags@ does not have the
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT'
--     flag set and no
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfo'
--     structure is included in the @pNext@ chain, the local workgroup size
--     in the X dimension of the pipeline /must/ be a multiple of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-subgroupSize subgroupSize>
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-module-08987# If @module@ uses
--     the @OpTypeCooperativeMatrixKHR@ instruction with a @Scope@ equal to
--     @Subgroup@, then the local workgroup size in the X dimension of the
--     pipeline /must/ be a multiple of the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-sgs effective subgroup size>
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-module-10169# If @module@ uses
--     the @OpTypeCooperativeMatrixKHR@ instruction with a @Scope@ equal to
--     @Workgroup@, then the local workgroup size in the X dimension of the
--     pipeline /must/ be a multiple of the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-sgs effective subgroup size>
--     and the total local workgroup size /must/ be a power of two multiple
--     of the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces-builtin-variables-sgs effective subgroup size>
--     and /must/ be less than or equal to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-cooperativeMatrixWorkgroupScopeMaxWorkgroupSize cooperativeMatrixWorkgroupScopeMaxWorkgroupSize>
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-08771# If a shader
--     module identifier is not specified for this @stage@, @module@ /must/
--     be a valid 'Vulkan.Core10.Handles.ShaderModule' , or the @pNext@
--     chain of the parent @Vk*CreateInfo@ structure /must/ set
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR'::@binaryCount@
--     to a value greater than @0@, if none of the following features are
--     enabled:
--
--     -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-graphicsPipelineLibrary graphicsPipelineLibrary>
--
--     -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance5 maintenance5>
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-06845# If a shader
--     module identifier is not specified for this @stage@, @module@ /must/
--     be a valid 'Vulkan.Core10.Handles.ShaderModule', or there /must/ be
--     a valid 'Vulkan.Core10.Shader.ShaderModuleCreateInfo' structure in
--     the @pNext@ chain , or the @pNext@ chain of the parent
--     @Vk*CreateInfo@ structure /must/ set
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR'::@binaryCount@
--     to a value greater than @0@,
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-06844# If a shader
--     module identifier is specified for this @stage@, the @pNext@ chain
--     /must/ not include a 'Vulkan.Core10.Shader.ShaderModuleCreateInfo'
--     structure
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-06848# If a shader
--     module identifier is specified for this @stage@, @module@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pSpecializationInfo-06849# If
--     a shader module identifier is not specified, the shader code used by
--     the pipeline /must/ be valid as described by the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirv-spec Khronos SPIR-V Specification>
--     after applying the specializations provided in
--     @pSpecializationInfo@, if any, and then converting all
--     specialization constants into fixed constants
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO'
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pNext-pNext# Each @pNext@
--     member of any structure (including this one) in the @pNext@ chain
--     /must/ be either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT',
--     'Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'.PipelineRobustnessCreateInfo',
--     'Vulkan.Extensions.VK_EXT_shader_module_identifier.PipelineShaderStageModuleIdentifierCreateInfoEXT',
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.PipelineShaderStageNodeCreateInfoAMDX',
--     'Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfo',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.ShaderDescriptorSetAndBindingMappingInfoEXT',
--     'Vulkan.Core10.Shader.ShaderModuleCreateInfo', or
--     'Vulkan.Extensions.VK_EXT_validation_cache.ShaderModuleValidationCacheCreateInfoEXT'
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-sType-unique# The @sType@
--     value of each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PipelineShaderStageCreateFlagBits'
--     values
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-stage-parameter# @stage@
--     /must/ be a valid
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits' value
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-module-parameter# If @module@
--     is not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @module@ /must/ be
--     a valid 'Vulkan.Core10.Handles.ShaderModule' handle
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pName-parameter# @pName@
--     /must/ be a null-terminated UTF-8 string
--
-- -   #VUID-VkPipelineShaderStageCreateInfo-pSpecializationInfo-parameter#
--     If @pSpecializationInfo@ is not @NULL@, @pSpecializationInfo@ /must/
--     be a valid pointer to a valid 'SpecializationInfo' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'ComputePipelineCreateInfo',
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.ExecutionGraphPipelineCreateInfoAMDX',
-- 'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsShaderGroupCreateInfoNV',
-- 'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PipelineShaderStageCreateFlags',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
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
  , -- | @module@ is optionally a 'Vulkan.Core10.Handles.ShaderModule' object
    -- containing the shader code for this stage. The implementation /must/ not
    -- access this object outside of the duration of the command this structure
    -- is passed to.
    module' :: ShaderModule
  , -- | @pName@ is a pointer to a null-terminated UTF-8 string specifying the
    -- entry point name of the shader for this stage.
    name :: ByteString
  , -- | @pSpecializationInfo@ is a pointer to a 'SpecializationInfo' structure,
    -- as described in
    -- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-specialization-constants Specialization Constants>,
    -- or @NULL@.
    specializationInfo :: Maybe SpecializationInfo
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineShaderStageCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (PipelineShaderStageCreateInfo es)

instance Extensible PipelineShaderStageCreateInfo where
  extensibleTypeName = "PipelineShaderStageCreateInfo"
  setNext PipelineShaderStageCreateInfo{..} next' = PipelineShaderStageCreateInfo{next = next', ..}
  getNext PipelineShaderStageCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends PipelineShaderStageCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ShaderDescriptorSetAndBindingMappingInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineShaderStageNodeCreateInfoAMDX = Just f
    | Just Refl <- eqT @e @PipelineRobustnessCreateInfo = Just f
    | Just Refl <- eqT @e @PipelineShaderStageModuleIdentifierCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PipelineShaderStageRequiredSubgroupSizeCreateInfo = Just f
    | Just Refl <- eqT @e @DebugUtilsObjectNameInfoEXT = Just f
    | Just Refl <- eqT @e @ShaderModuleValidationCacheCreateInfoEXT = Just f
    | Just Refl <- eqT @e @(ShaderModuleCreateInfo '[]) = Just f
    | otherwise = Nothing

instance ( Extendss PipelineShaderStageCreateInfo es
         , PokeChain es ) => ToCStruct (PipelineShaderStageCreateInfo es) where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
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
    pName'' <- ContT $ useAsCString (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CChar))) pName''
    lift $ f

instance ( Extendss PipelineShaderStageCreateInfo es
         , PeekChain es ) => FromCStruct (PipelineShaderStageCreateInfo es) where
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- If the @pNext@ chain includes a
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.PipelineCreateFlags2CreateInfo'
-- structure,
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.PipelineCreateFlags2CreateInfo'::@flags@
-- from that structure is used instead of @flags@ from this structure.
--
-- == Valid Usage
--
-- -   #VUID-VkComputePipelineCreateInfo-None-09497# If the @pNext@ chain
--     does not include a
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.PipelineCreateFlags2CreateInfo'
--     structure, @flags@ /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-07984# If @flags@ contains
--     the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is -1, @basePipelineHandle@ /must/ be
--     a valid compute 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-07985# If @flags@ contains
--     the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be a valid index into the calling command’s @pCreateInfos@ parameter
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-07986# If @flags@ contains
--     the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, @basePipelineIndex@ /must/ be -1 or @basePipelineHandle@
--     /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkComputePipelineCreateInfo-layout-07987# If a push constant
--     block is declared in a shader and @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', a push constant range in
--     @layout@ /must/ match the shader stage
--
-- -   #VUID-VkComputePipelineCreateInfo-layout-10069# If a push constant
--     block is declared in a shader and @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the block must be
--     contained inside the push constant range in @layout@ that matches
--     the stage
--
-- -   #VUID-VkComputePipelineCreateInfo-layout-07988# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variable>
--     is declared in a shader and @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the corresponding
--     descriptor set in @layout@ /must/ match the shader stage
--
-- -   #VUID-VkComputePipelineCreateInfo-layout-07990# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variable>
--     is declared in a shader, @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', and the descriptor type is
--     not
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_MUTABLE_EXT',
--     the corresponding descriptor set in @layout@ /must/ match the
--     descriptor type
--
-- -   #VUID-VkComputePipelineCreateInfo-layout-07991# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variable>
--     is declared in a shader as an array and @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the corresponding
--     descriptor binding used to create @layout@ /must/ have a
--     @descriptorCount@ that is greater than or equal to the length of the
--     array
--
-- -   #VUID-VkComputePipelineCreateInfo-None-10391# If a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources resource variables>
--     is declared in a shader as an array of descriptors, then the
--     descriptor type of that variable /must/ not be
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-11798# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shader64BitIndexing shader64BitIndexing>
--     feature is not enabled, @flags@ /must/ not contain
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_64_BIT_INDEXING_BIT_EXT'
--
-- -   #VUID-VkComputePipelineCreateInfo-pipelineCreationCacheControl-02878#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
--     nor
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT'
--
-- -   #VUID-VkComputePipelineCreateInfo-pipelineProtectedAccess-07368# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineProtectedAccess pipelineProtectedAccess>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT'
--     nor
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-07369# @flags@ /must/ not
--     include both
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_NO_PROTECTED_ACCESS_BIT'
--     and
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_PROTECTED_ACCESS_ONLY_BIT'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-11311# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     includes
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     @layout@ /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-11312# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     includes
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     all shader variables in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-resources shader resource interface>
--     with a 'Vulkan.Core10.Handles.DescriptorSet' and @Binding@
--     decoration /must/ have a mapping declared in
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.ShaderDescriptorSetAndBindingMappingInfoEXT'::pMappings
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-03365# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-03366# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-03367# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-03368# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-03369# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-03370# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-03576# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-04945# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-09007# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedComputePipelines ::deviceGeneratedComputePipelines>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-09008# If @flags@ includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV',
--     then the @pNext@ chain /must/ include a pointer to a valid instance
--     of
--     'Vulkan.Extensions.VK_NV_device_generated_commands_compute.ComputePipelineIndirectBufferInfoNV'
--     specifying the address where the pipeline’s metadata will be saved
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-11007# If @flags@ includes
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_EXT',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-deviceGeneratedCommands ::deviceGeneratedCommands>
--     feature /must/ be enabled
--
-- -   #VUID-VkComputePipelineCreateInfo-stage-00701# The @stage@ member of
--     @stage@ /must/ be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_COMPUTE_BIT'
--
-- -   #VUID-VkComputePipelineCreateInfo-stage-00702# The shader code for
--     the entry point identified by @stage@ and the rest of the state
--     identified by this structure /must/ adhere to the pipeline linking
--     rules described in the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   #VUID-VkComputePipelineCreateInfo-layout-01687# If @layout@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', the number of resources in
--     @layout@ accessible to the compute shader stage /must/ be less than
--     or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   #VUID-VkComputePipelineCreateInfo-shaderEnqueue-09177# If the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-shaderEnqueue shaderEnqueue>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-09178# If @flags@ does not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR',
--     the shader specified by @stage@ /must/ not declare the
--     @ShaderEnqueueAMDX@ capability
--
-- -   #VUID-VkComputePipelineCreateInfo-pipelineStageCreationFeedbackCount-06566#
--     If
--     'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfo'::@pipelineStageCreationFeedbackCount@
--     is not @0@, it /must/ be @1@
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-07367# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT'
--
-- -   #VUID-VkComputePipelineCreateInfo-flags-07996# @flags@ /must/ not
--     include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV'
--
-- -   #VUID-VkComputePipelineCreateInfo-None-11367# If
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlags2CreateInfoKHR'::@flags@
--     does not include
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DESCRIPTOR_HEAP_BIT_EXT',
--     @layout@ /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkComputePipelineCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO'
--
-- -   #VUID-VkComputePipelineCreateInfo-pNext-pNext# Each @pNext@ member
--     of any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_NV_device_generated_commands_compute.ComputePipelineIndirectBufferInfoNV',
--     'Vulkan.Extensions.VK_KHR_pipeline_binary.PipelineBinaryInfoKHR',
--     'Vulkan.Extensions.VK_AMD_pipeline_compiler_control.PipelineCompilerControlCreateInfoAMD',
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance5Roadmap.PipelineCreateFlags2CreateInfo',
--     'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfo',
--     'Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'.PipelineRobustnessCreateInfo',
--     or
--     'Vulkan.Extensions.VK_HUAWEI_subpass_shading.SubpassShadingPipelineCreateInfoHUAWEI'
--
-- -   #VUID-VkComputePipelineCreateInfo-sType-unique# The @sType@ value of
--     each structure in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkComputePipelineCreateInfo-stage-parameter# @stage@ /must/ be
--     a valid 'PipelineShaderStageCreateInfo' structure
--
-- -   #VUID-VkComputePipelineCreateInfo-layout-parameter# If @layout@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @layout@ /must/ be a
--     valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkComputePipelineCreateInfo-commonparent# Both of
--     @basePipelineHandle@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'Vulkan.Core10.Handles.PipelineLayout', 'PipelineShaderStageCreateInfo',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createComputePipelines',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands_compute.getPipelineIndirectMemoryRequirementsNV'
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
    -- pipeline and descriptor sets used with the pipeline. If
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceProperties'::@apiVersion@
    -- is greater than or equal to Vulkan 1.3 or
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance4 VK_KHR_maintenance4>
    -- is enabled @layout@ /must/ not be accessed by the implementation outside
    -- of the duration of the command this structure is passed to.
    layout :: PipelineLayout
  , -- | @basePipelineHandle@ is a pipeline to derive from.
    basePipelineHandle :: Pipeline
  , -- | @basePipelineIndex@ is an index into the @pCreateInfos@ parameter to use
    -- as a pipeline to derive from.
    basePipelineIndex :: Int32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ComputePipelineCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (ComputePipelineCreateInfo es)

instance Extensible ComputePipelineCreateInfo where
  extensibleTypeName = "ComputePipelineCreateInfo"
  setNext ComputePipelineCreateInfo{..} next' = ComputePipelineCreateInfo{next = next', ..}
  getNext ComputePipelineCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends ComputePipelineCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineRobustnessCreateInfo = Just f
    | Just Refl <- eqT @e @PipelineCompilerControlCreateInfoAMD = Just f
    | Just Refl <- eqT @e @SubpassShadingPipelineCreateInfoHUAWEI = Just f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfo = Just f
    | Just Refl <- eqT @e @PipelineBinaryInfoKHR = Just f
    | Just Refl <- eqT @e @PipelineCreateFlags2CreateInfo = Just f
    | Just Refl <- eqT @e @ComputePipelineIndirectBufferInfoNV = Just f
    | otherwise = Nothing

instance ( Extendss ComputePipelineCreateInfo es
         , PokeChain es ) => ToCStruct (ComputePipelineCreateInfo es) where
  withCStruct x f = allocaBytes 96 $ \p -> pokeCStruct p x (f p)
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
    lift $ poke ((p `plusPtr` 88 :: Ptr Int32)) (zero)
    lift $ f

instance ( Extendss ComputePipelineCreateInfo es
         , PeekChain es ) => FromCStruct (ComputePipelineCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCreateFlags))
    stage <- peekSomeCStruct (forgetExtensions ((p `plusPtr` 24 :: Ptr (PipelineShaderStageCreateInfo _))))
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

