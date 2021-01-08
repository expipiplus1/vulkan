{-# language CPP #-}
-- No documentation found for Chapter "Device"
module Vulkan.Core10.Device  ( createDevice
                             , withDevice
                             , destroyDevice
                             , DeviceQueueCreateInfo(..)
                             , DeviceCreateInfo(..)
                             , Device(..)
                             , DeviceCreateFlags(..)
                             , DeviceQueueCreateFlagBits(..)
                             , DeviceQueueCreateFlags
                             ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
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
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CFloat(..))
import Foreign.C.Types (CFloat(CFloat))
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Dynamic (initDeviceCmds)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyDevice))
import Vulkan.Core10.Enums.DeviceCreateFlags (DeviceCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (DeviceDeviceMemoryReportCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostics_config (DeviceDiagnosticsConfigCreateInfoNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (DeviceGroupDeviceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_memory_overallocation_behavior (DeviceMemoryOverallocationCreateInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_private_data (DevicePrivateDataCreateInfoEXT)
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_global_priority (DeviceQueueGlobalPriorityCreateInfoEXT)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Dynamic (InstanceCmds(pVkCreateDevice))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (PhysicalDevice)
import Vulkan.Core10.Handles (PhysicalDevice(..))
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage (PhysicalDevice16BitStorageFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_4444_formats (PhysicalDevice4444FormatsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage (PhysicalDevice8BitStorageFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_astc_decode_mode (PhysicalDeviceASTCDecodeFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (PhysicalDeviceAccelerationStructureFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_device_coherent_memory (PhysicalDeviceCoherentMemoryFeaturesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (PhysicalDeviceConditionalRenderingFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_corner_sampled_image (PhysicalDeviceCornerSampledImageFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (PhysicalDeviceCoverageReductionModeFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (PhysicalDeviceCustomBorderColorFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing (PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_enable (PhysicalDeviceDepthClipEnableFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (PhysicalDeviceDeviceGeneratedCommandsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (PhysicalDeviceDeviceMemoryReportFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostics_config (PhysicalDeviceDiagnosticsConfigFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_scissor_exclusive (PhysicalDeviceExclusiveScissorFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state (PhysicalDeviceExtendedDynamicStateFeaturesEXT)
import Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceFeatures2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map2 (PhysicalDeviceFragmentDensityMap2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_shader_interlock (PhysicalDeviceFragmentShaderInterlockFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shading_rate_enums (PhysicalDeviceFragmentShadingRateEnumsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRateFeaturesKHR)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset (PhysicalDeviceHostQueryResetFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_robustness (PhysicalDeviceImageRobustnessFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (PhysicalDeviceImagelessFramebufferFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_index_type_uint8 (PhysicalDeviceIndexTypeUint8FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PhysicalDeviceLineRasterizationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_priority (PhysicalDeviceMemoryPriorityFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_mesh_shader (PhysicalDeviceMeshShaderFeaturesNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_VALVE_mutable_descriptor_type (PhysicalDeviceMutableDescriptorTypeFeaturesVALVE)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PhysicalDevicePerformanceQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control (PhysicalDevicePipelineCreationCacheControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PhysicalDevicePipelineExecutablePropertiesFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_portability_subset (PhysicalDevicePortabilitySubsetFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_private_data (PhysicalDevicePrivateDataFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (PhysicalDeviceProtectedMemoryFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_query (PhysicalDeviceRayQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (PhysicalDeviceRayTracingPipelineFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_representative_fragment_test (PhysicalDeviceRepresentativeFragmentTestFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_robustness2 (PhysicalDeviceRobustness2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (PhysicalDeviceSamplerYcbcrConversionFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout (PhysicalDeviceScalarBlockLayoutFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (PhysicalDeviceSeparateDepthStencilLayoutsFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_atomic_float (PhysicalDeviceShaderAtomicFloatFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64 (PhysicalDeviceShaderAtomicInt64Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_clock (PhysicalDeviceShaderClockFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation (PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters (PhysicalDeviceShaderDrawParametersFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8 (PhysicalDeviceShaderFloat16Int8Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_image_atomic_int64 (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_image_footprint (PhysicalDeviceShaderImageFootprintFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_shader_integer_functions2 (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsFeaturesNV)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types (PhysicalDeviceShaderSubgroupExtendedTypesFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_terminate_invocation (PhysicalDeviceShaderTerminateInvocationFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImageFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_texel_buffer_alignment (PhysicalDeviceTexelBufferAlignmentFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr (PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout (PhysicalDeviceUniformBufferStandardLayoutFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers (PhysicalDeviceVariablePointersFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan11Features)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan12Features)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model (PhysicalDeviceVulkanMemoryModelFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_ycbcr_image_arrays (PhysicalDeviceYcbcrImageArraysFeaturesEXT)
import Vulkan.Core10.Handles (PhysicalDevice_T)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Enums.DeviceCreateFlags (DeviceCreateFlags(..))
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlagBits(..))
import Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDevice
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (SomeStruct DeviceCreateInfo) -> Ptr AllocationCallbacks -> Ptr (Ptr Device_T) -> IO Result) -> Ptr PhysicalDevice_T -> Ptr (SomeStruct DeviceCreateInfo) -> Ptr AllocationCallbacks -> Ptr (Ptr Device_T) -> IO Result

-- | vkCreateDevice - Create a new device instance
--
-- = Description
--
-- 'createDevice' verifies that extensions and features requested in the
-- @ppEnabledExtensionNames@ and @pEnabledFeatures@ members of
-- @pCreateInfo@, respectively, are supported by the implementation. If any
-- requested extension is not supported, 'createDevice' /must/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'. If any
-- requested feature is not supported, 'createDevice' /must/ return
-- 'Vulkan.Core10.Enums.Result.ERROR_FEATURE_NOT_PRESENT'. Support for
-- extensions /can/ be checked before creating a device by querying
-- 'Vulkan.Core10.ExtensionDiscovery.enumerateDeviceExtensionProperties'.
-- Support for features /can/ similarly be checked by querying
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFeatures'.
--
-- After verifying and enabling the extensions the
-- 'Vulkan.Core10.Handles.Device' object is created and returned to the
-- application.
--
-- Multiple logical devices /can/ be created from the same physical device.
-- Logical device creation /may/ fail due to lack of device-specific
-- resources (in addition to the other errors). If that occurs,
-- 'createDevice' will return
-- 'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateDevice-ppEnabledExtensionNames-01387# All
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-extensions-extensiondependencies required extensions>
--     for each extension in the
--     'DeviceCreateInfo'::@ppEnabledExtensionNames@ list /must/ also be
--     present in that list
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateDevice-physicalDevice-parameter# @physicalDevice@
--     /must/ be a valid 'Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   #VUID-vkCreateDevice-pCreateInfo-parameter# @pCreateInfo@ /must/ be
--     a valid pointer to a valid 'DeviceCreateInfo' structure
--
-- -   #VUID-vkCreateDevice-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateDevice-pDevice-parameter# @pDevice@ /must/ be a valid
--     pointer to a 'Vulkan.Core10.Handles.Device' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FEATURE_NOT_PRESENT'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'DeviceCreateInfo',
-- 'Vulkan.Core10.Handles.PhysicalDevice'
createDevice :: forall a io
              . (Extendss DeviceCreateInfo a, PokeChain a, MonadIO io)
             => -- | @physicalDevice@ /must/ be one of the device handles returned from a
                -- call to 'Vulkan.Core10.DeviceInitialization.enumeratePhysicalDevices'
                -- (see
                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-physical-device-enumeration Physical Device Enumeration>).
                PhysicalDevice
             -> -- | @pCreateInfo@ is a pointer to a 'DeviceCreateInfo' structure containing
                -- information about how to create the device.
                (DeviceCreateInfo a)
             -> -- | @pAllocator@ controls host memory allocation as described in the
                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                -- chapter.
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io (Device)
createDevice physicalDevice createInfo allocator = liftIO . evalContT $ do
  let cmds = instanceCmds (physicalDevice :: PhysicalDevice)
  let vkCreateDevicePtr = pVkCreateDevice cmds
  lift $ unless (vkCreateDevicePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateDevice is null" Nothing Nothing
  let vkCreateDevice' = mkVkCreateDevice vkCreateDevicePtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDevice <- ContT $ bracket (callocBytes @(Ptr Device_T) 8) free
  r <- lift $ traceAroundEvent "vkCreateDevice" (vkCreateDevice' (physicalDeviceHandle (physicalDevice)) (forgetExtensions pCreateInfo) pAllocator (pPDevice))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDevice <- lift $ peek @(Ptr Device_T) pPDevice
  pDevice' <- lift $ (\h -> Device h <$> initDeviceCmds cmds h) pDevice
  pure $ (pDevice')

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createDevice' and 'destroyDevice'
--
-- To ensure that 'destroyDevice' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withDevice :: forall a io r . (Extendss DeviceCreateInfo a, PokeChain a, MonadIO io) => PhysicalDevice -> DeviceCreateInfo a -> Maybe AllocationCallbacks -> (io Device -> (Device -> io ()) -> r) -> r
withDevice physicalDevice pCreateInfo pAllocator b =
  b (createDevice physicalDevice pCreateInfo pAllocator)
    (\(o0) -> destroyDevice o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDevice
  :: FunPtr (Ptr Device_T -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyDevice - Destroy a logical device
--
-- = Description
--
-- To ensure that no work is active on the device,
-- 'Vulkan.Core10.Queue.deviceWaitIdle' /can/ be used to gate the
-- destruction of the device. Prior to destroying a device, an application
-- is responsible for destroying\/freeing any Vulkan objects that were
-- created using that device as the first parameter of the corresponding
-- @vkCreate*@ or @vkAllocate*@ command.
--
-- Note
--
-- The lifetime of each of these objects is bound by the lifetime of the
-- 'Vulkan.Core10.Handles.Device' object. Therefore, to avoid resource
-- leaks, it is critical that an application explicitly free all of these
-- resources prior to calling 'destroyDevice'.
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyDevice-device-00378# All child objects created on
--     @device@ /must/ have been destroyed prior to destroying @device@
--
-- -   #VUID-vkDestroyDevice-device-00379# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @device@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyDevice-device-00380# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @device@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyDevice-device-parameter# If @device@ is not @NULL@,
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyDevice-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- == Host Synchronization
--
-- -   Host access to @device@ /must/ be externally synchronized
--
-- -   Host access to all 'Vulkan.Core10.Handles.Queue' objects received
--     from @device@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device'
destroyDevice :: forall io
               . (MonadIO io)
              => -- | @device@ is the logical device to destroy.
                 Device
              -> -- | @pAllocator@ controls host memory allocation as described in the
                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                 -- chapter.
                 ("allocator" ::: Maybe AllocationCallbacks)
              -> io ()
destroyDevice device allocator = liftIO . evalContT $ do
  let vkDestroyDevicePtr = pVkDestroyDevice (deviceCmds (device :: Device))
  lift $ unless (vkDestroyDevicePtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyDevice is null" Nothing Nothing
  let vkDestroyDevice' = mkVkDestroyDevice vkDestroyDevicePtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyDevice" (vkDestroyDevice' (deviceHandle (device)) pAllocator)
  pure $ ()


-- | VkDeviceQueueCreateInfo - Structure specifying parameters of a newly
-- created device queue
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceQueueCreateInfo-queueFamilyIndex-00381#
--     @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--
-- -   #VUID-VkDeviceQueueCreateInfo-queueCount-00382# @queueCount@ /must/
--     be less than or equal to the @queueCount@ member of the
--     'Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--     structure, as returned by
--     'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     in the @pQueueFamilyProperties@[queueFamilyIndex]
--
-- -   #VUID-VkDeviceQueueCreateInfo-pQueuePriorities-00383# Each element
--     of @pQueuePriorities@ /must/ be between @0.0@ and @1.0@ inclusive
--
-- -   #VUID-VkDeviceQueueCreateInfo-flags-02861# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-protectedMemory protected memory>
--     feature is not enabled, the
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_PROTECTED_BIT'
--     bit of @flags@ /must/ not be set
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceQueueCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO'
--
-- -   #VUID-VkDeviceQueueCreateInfo-pNext-pNext# @pNext@ /must/ be @NULL@
--     or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_global_priority.DeviceQueueGlobalPriorityCreateInfoEXT'
--
-- -   #VUID-VkDeviceQueueCreateInfo-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkDeviceQueueCreateInfo-flags-parameter# @flags@ /must/ be a
--     valid combination of
--     'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlagBits'
--     values
--
-- -   #VUID-VkDeviceQueueCreateInfo-pQueuePriorities-parameter#
--     @pQueuePriorities@ /must/ be a valid pointer to an array of
--     @queueCount@ @float@ values
--
-- -   #VUID-VkDeviceQueueCreateInfo-queueCount-arraylength# @queueCount@
--     /must/ be greater than @0@
--
-- = See Also
--
-- 'DeviceCreateInfo',
-- 'Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceQueueCreateInfo (es :: [Type]) = DeviceQueueCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask indicating behavior of the queue.
    flags :: DeviceQueueCreateFlags
  , -- | @queueFamilyIndex@ is an unsigned integer indicating the index of the
    -- queue family to create on this device. This index corresponds to the
    -- index of an element of the @pQueueFamilyProperties@ array that was
    -- returned by
    -- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'.
    queueFamilyIndex :: Word32
  , -- | @pQueuePriorities@ is a pointer to an array of @queueCount@ normalized
    -- floating point values, specifying priorities of work that will be
    -- submitted to each created queue. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-priority Queue Priority>
    -- for more information.
    queuePriorities :: Vector Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceQueueCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DeviceQueueCreateInfo es)

instance Extensible DeviceQueueCreateInfo where
  extensibleTypeName = "DeviceQueueCreateInfo"
  setNext x next = x{next = next}
  getNext DeviceQueueCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DeviceQueueCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeviceQueueGlobalPriorityCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss DeviceQueueCreateInfo es, PokeChain es) => ToCStruct (DeviceQueueCreateInfo es) where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceQueueCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceQueueCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (queueFamilyIndex)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queuePriorities)) :: Word32))
    pPQueuePriorities' <- ContT $ allocaBytesAligned @CFloat ((Data.Vector.length (queuePriorities)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueuePriorities' `plusPtr` (4 * (i)) :: Ptr CFloat) (CFloat (e))) (queuePriorities)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CFloat))) (pPQueuePriorities')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    lift $ f

instance (Extendss DeviceQueueCreateInfo es, PeekChain es) => FromCStruct (DeviceQueueCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @DeviceQueueCreateFlags ((p `plusPtr` 16 :: Ptr DeviceQueueCreateFlags))
    queueFamilyIndex <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    queueCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pQueuePriorities <- peek @(Ptr CFloat) ((p `plusPtr` 32 :: Ptr (Ptr CFloat)))
    pQueuePriorities' <- generateM (fromIntegral queueCount) (\i -> do
      pQueuePrioritiesElem <- peek @CFloat ((pQueuePriorities `advancePtrBytes` (4 * (i)) :: Ptr CFloat))
      pure $ coerce @CFloat @Float pQueuePrioritiesElem)
    pure $ DeviceQueueCreateInfo
             next flags queueFamilyIndex pQueuePriorities'

instance es ~ '[] => Zero (DeviceQueueCreateInfo es) where
  zero = DeviceQueueCreateInfo
           ()
           zero
           zero
           mempty


-- | VkDeviceCreateInfo - Structure specifying parameters of a newly created
-- device
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceCreateInfo-queueFamilyIndex-02802# The
--     @queueFamilyIndex@ member of each element of @pQueueCreateInfos@
--     /must/ be unique within @pQueueCreateInfos@, except that two members
--     can share the same @queueFamilyIndex@ if one is a protected-capable
--     queue and one is not a protected-capable queue
--
-- -   #VUID-VkDeviceCreateInfo-pNext-00373# If the @pNext@ chain includes
--     a
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--     structure, then @pEnabledFeatures@ /must/ be @NULL@
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-01840#
--     @ppEnabledExtensionNames@ /must/ not contain
--     @VK_AMD_negative_viewport_height@
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-03328#
--     @ppEnabledExtensionNames@ /must/ not contain both
--     @VK_KHR_buffer_device_address@ and @VK_EXT_buffer_device_address@
--
-- -   #VUID-VkDeviceCreateInfo-pNext-02829# If the @pNext@ chain includes
--     a 'Vulkan.Core12.PhysicalDeviceVulkan11Features' structure, then it
--     /must/ not include a
--     'Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage.PhysicalDevice16BitStorageFeatures',
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures',
--     'Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers.PhysicalDeviceVariablePointersFeatures',
--     'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryFeatures',
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.PhysicalDeviceSamplerYcbcrConversionFeatures',
--     or
--     'Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters.PhysicalDeviceShaderDrawParametersFeatures'
--     structure
--
-- -   #VUID-VkDeviceCreateInfo-pNext-02830# If the @pNext@ chain includes
--     a 'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure, then it
--     /must/ not include a
--     'Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage.PhysicalDevice8BitStorageFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64.PhysicalDeviceShaderAtomicInt64Features',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8Features',
--     'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures',
--     'Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout.PhysicalDeviceScalarBlockLayoutFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.PhysicalDeviceImagelessFramebufferFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout.PhysicalDeviceUniformBufferStandardLayoutFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types.PhysicalDeviceShaderSubgroupExtendedTypesFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.PhysicalDeviceSeparateDepthStencilLayoutsFeatures',
--     'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.PhysicalDeviceHostQueryResetFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures',
--     or
--     'Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeatures'
--     structure
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensions-04476# If
--     @ppEnabledExtensions@ contains @\"VK_KHR_shader_draw_parameters\"@
--     and the @pNext@ chain includes a
--     'Vulkan.Core12.PhysicalDeviceVulkan11Features' structure, then
--     'Vulkan.Core12.PhysicalDeviceVulkan11Features'::@shaderDrawParameters@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensions-02831# If
--     @ppEnabledExtensions@ contains @\"VK_KHR_draw_indirect_count\"@ and
--     the @pNext@ chain includes a
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure, then
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@drawIndirectCount@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensions-02832# If
--     @ppEnabledExtensions@ contains
--     @\"VK_KHR_sampler_mirror_clamp_to_edge\"@ and the @pNext@ chain
--     includes a 'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure,
--     then
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@samplerMirrorClampToEdge@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensions-02833# If
--     @ppEnabledExtensions@ contains @\"VK_EXT_descriptor_indexing\"@ and
--     the @pNext@ chain includes a
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure, then
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@descriptorIndexing@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensions-02834# If
--     @ppEnabledExtensions@ contains @\"VK_EXT_sampler_filter_minmax\"@
--     and the @pNext@ chain includes a
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure, then
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@samplerFilterMinmax@
--     /must/ be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensions-02835# If
--     @ppEnabledExtensions@ contains
--     @\"VK_EXT_shader_viewport_index_layer\"@ and the @pNext@ chain
--     includes a 'Vulkan.Core12.PhysicalDeviceVulkan12Features' structure,
--     then
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@shaderOutputViewportIndex@
--     and
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features'::@shaderOutputLayer@
--     /must/ both be 'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkDeviceCreateInfo-pProperties-04451# If the
--     @VK_KHR_portability_subset@ extension is included in @pProperties@
--     of
--     'Vulkan.Core10.ExtensionDiscovery.enumerateDeviceExtensionProperties',
--     @ppEnabledExtensions@ /must/ include \"VK_KHR_portability_subset\".
--
-- -   #VUID-VkDeviceCreateInfo-shadingRateImage-04478# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-shadingRateImage-04479# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>
--     /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-shadingRateImage-04480# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-shadingRateImage shadingRateImage>
--     is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-fragmentDensityMap-04481# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineFragmentShadingRate pipelineFragmentShadingRate>
--     /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-fragmentDensityMap-04482# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-primitiveFragmentShadingRate primitiveFragmentShadingRate>
--     /must/ not be enabled
--
-- -   #VUID-VkDeviceCreateInfo-fragmentDensityMap-04483# If
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-fragmentDensityMap fragmentDensityMap>
--     is enabled,
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-attachmentFragmentShadingRate attachmentFragmentShadingRate>
--     /must/ not be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_CREATE_INFO'
--
-- -   #VUID-VkDeviceCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_device_memory_report.DeviceDeviceMemoryReportCreateInfoEXT',
--     'Vulkan.Extensions.VK_NV_device_diagnostics_config.DeviceDiagnosticsConfigCreateInfoNV',
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo',
--     'Vulkan.Extensions.VK_AMD_memory_overallocation_behavior.DeviceMemoryOverallocationCreateInfoAMD',
--     'Vulkan.Extensions.VK_EXT_private_data.DevicePrivateDataCreateInfoEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage.PhysicalDevice16BitStorageFeatures',
--     'Vulkan.Extensions.VK_EXT_4444_formats.PhysicalDevice4444FormatsFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage.PhysicalDevice8BitStorageFeatures',
--     'Vulkan.Extensions.VK_EXT_astc_decode_mode.PhysicalDeviceASTCDecodeFeaturesEXT',
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.PhysicalDeviceAccelerationStructureFeaturesKHR',
--     'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures',
--     'Vulkan.Extensions.VK_EXT_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeaturesEXT',
--     'Vulkan.Extensions.VK_AMD_device_coherent_memory.PhysicalDeviceCoherentMemoryFeaturesAMD',
--     'Vulkan.Extensions.VK_NV_compute_shader_derivatives.PhysicalDeviceComputeShaderDerivativesFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_conditional_rendering.PhysicalDeviceConditionalRenderingFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_cooperative_matrix.PhysicalDeviceCooperativeMatrixFeaturesNV',
--     'Vulkan.Extensions.VK_NV_corner_sampled_image.PhysicalDeviceCornerSampledImageFeaturesNV',
--     'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PhysicalDeviceCoverageReductionModeFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_custom_border_color.PhysicalDeviceCustomBorderColorFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing.PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_depth_clip_enable.PhysicalDeviceDepthClipEnableFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures',
--     'Vulkan.Extensions.VK_NV_device_generated_commands.PhysicalDeviceDeviceGeneratedCommandsFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_device_memory_report.PhysicalDeviceDeviceMemoryReportFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_device_diagnostics_config.PhysicalDeviceDiagnosticsConfigFeaturesNV',
--     'Vulkan.Extensions.VK_NV_scissor_exclusive.PhysicalDeviceExclusiveScissorFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_extended_dynamic_state.PhysicalDeviceExtendedDynamicStateFeaturesEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Extensions.VK_EXT_fragment_density_map2.PhysicalDeviceFragmentDensityMap2FeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_fragment_shader_barycentric.PhysicalDeviceFragmentShaderBarycentricFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_fragment_shader_interlock.PhysicalDeviceFragmentShaderInterlockFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.PhysicalDeviceFragmentShadingRateEnumsFeaturesNV',
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PhysicalDeviceFragmentShadingRateFeaturesKHR',
--     'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.PhysicalDeviceHostQueryResetFeatures',
--     'Vulkan.Extensions.VK_EXT_image_robustness.PhysicalDeviceImageRobustnessFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.PhysicalDeviceImagelessFramebufferFeatures',
--     'Vulkan.Extensions.VK_EXT_index_type_uint8.PhysicalDeviceIndexTypeUint8FeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_line_rasterization.PhysicalDeviceLineRasterizationFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_memory_priority.PhysicalDeviceMemoryPriorityFeaturesEXT',
--     'Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderFeaturesNV',
--     'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures',
--     'Vulkan.Extensions.VK_VALVE_mutable_descriptor_type.PhysicalDeviceMutableDescriptorTypeFeaturesVALVE',
--     'Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryFeaturesKHR',
--     'Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PhysicalDevicePipelineCreationCacheControlFeaturesEXT',
--     'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PhysicalDevicePipelineExecutablePropertiesFeaturesKHR',
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR',
--     'Vulkan.Extensions.VK_EXT_private_data.PhysicalDevicePrivateDataFeaturesEXT',
--     'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryFeatures',
--     'Vulkan.Extensions.VK_KHR_ray_query.PhysicalDeviceRayQueryFeaturesKHR',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelineFeaturesKHR',
--     'Vulkan.Extensions.VK_NV_representative_fragment_test.PhysicalDeviceRepresentativeFragmentTestFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_robustness2.PhysicalDeviceRobustness2FeaturesEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.PhysicalDeviceSamplerYcbcrConversionFeatures',
--     'Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout.PhysicalDeviceScalarBlockLayoutFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.PhysicalDeviceSeparateDepthStencilLayoutsFeatures',
--     'Vulkan.Extensions.VK_EXT_shader_atomic_float.PhysicalDeviceShaderAtomicFloatFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64.PhysicalDeviceShaderAtomicInt64Features',
--     'Vulkan.Extensions.VK_KHR_shader_clock.PhysicalDeviceShaderClockFeaturesKHR',
--     'Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation.PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT',
--     'Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters.PhysicalDeviceShaderDrawParametersFeatures',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8Features',
--     'Vulkan.Extensions.VK_EXT_shader_image_atomic_int64.PhysicalDeviceShaderImageAtomicInt64FeaturesEXT',
--     'Vulkan.Extensions.VK_NV_shader_image_footprint.PhysicalDeviceShaderImageFootprintFeaturesNV',
--     'Vulkan.Extensions.VK_INTEL_shader_integer_functions2.PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL',
--     'Vulkan.Extensions.VK_NV_shader_sm_builtins.PhysicalDeviceShaderSMBuiltinsFeaturesNV',
--     'Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types.PhysicalDeviceShaderSubgroupExtendedTypesFeatures',
--     'Vulkan.Extensions.VK_KHR_shader_terminate_invocation.PhysicalDeviceShaderTerminateInvocationFeaturesKHR',
--     'Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImageFeaturesNV',
--     'Vulkan.Extensions.VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
--     'Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr.PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreFeatures',
--     'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackFeaturesEXT',
--     'Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout.PhysicalDeviceUniformBufferStandardLayoutFeatures',
--     'Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers.PhysicalDeviceVariablePointersFeatures',
--     'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PhysicalDeviceVertexAttributeDivisorFeaturesEXT',
--     'Vulkan.Core12.PhysicalDeviceVulkan11Features',
--     'Vulkan.Core12.PhysicalDeviceVulkan12Features',
--     'Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeatures',
--     or
--     'Vulkan.Extensions.VK_EXT_ycbcr_image_arrays.PhysicalDeviceYcbcrImageArraysFeaturesEXT'
--
-- -   #VUID-VkDeviceCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique, with the exception of
--     structures of type
--     'Vulkan.Extensions.VK_EXT_device_memory_report.DeviceDeviceMemoryReportCreateInfoEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_private_data.DevicePrivateDataCreateInfoEXT'
--
-- -   #VUID-VkDeviceCreateInfo-flags-zerobitmask# @flags@ /must/ be @0@
--
-- -   #VUID-VkDeviceCreateInfo-pQueueCreateInfos-parameter#
--     @pQueueCreateInfos@ /must/ be a valid pointer to an array of
--     @queueCreateInfoCount@ valid 'DeviceQueueCreateInfo' structures
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledLayerNames-parameter# If
--     @enabledLayerCount@ is not @0@, @ppEnabledLayerNames@ /must/ be a
--     valid pointer to an array of @enabledLayerCount@ null-terminated
--     UTF-8 strings
--
-- -   #VUID-VkDeviceCreateInfo-ppEnabledExtensionNames-parameter# If
--     @enabledExtensionCount@ is not @0@, @ppEnabledExtensionNames@ /must/
--     be a valid pointer to an array of @enabledExtensionCount@
--     null-terminated UTF-8 strings
--
-- -   #VUID-VkDeviceCreateInfo-pEnabledFeatures-parameter# If
--     @pEnabledFeatures@ is not @NULL@, @pEnabledFeatures@ /must/ be a
--     valid pointer to a valid
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures'
--     structure
--
-- -   #VUID-VkDeviceCreateInfo-queueCreateInfoCount-arraylength#
--     @queueCreateInfoCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.DeviceCreateFlags.DeviceCreateFlags',
-- 'DeviceQueueCreateInfo',
-- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createDevice'
data DeviceCreateInfo (es :: [Type]) = DeviceCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is reserved for future use.
    flags :: DeviceCreateFlags
  , -- | @pQueueCreateInfos@ is a pointer to an array of 'DeviceQueueCreateInfo'
    -- structures describing the queues that are requested to be created along
    -- with the logical device. Refer to the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-queue-creation Queue Creation>
    -- section below for further details.
    queueCreateInfos :: Vector (SomeStruct DeviceQueueCreateInfo)
  , -- | @ppEnabledLayerNames@ is deprecated and ignored. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-layers-devicelayerdeprecation>.
    enabledLayerNames :: Vector ByteString
  , -- | @ppEnabledExtensionNames@ is a pointer to an array of
    -- @enabledExtensionCount@ null-terminated UTF-8 strings containing the
    -- names of extensions to enable for the created device. See the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-extensions>
    -- section for further details.
    enabledExtensionNames :: Vector ByteString
  , -- | @pEnabledFeatures@ is @NULL@ or a pointer to a
    -- 'Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures' structure
    -- containing boolean indicators of all the features to be enabled. Refer
    -- to the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features Features>
    -- section for further details.
    enabledFeatures :: Maybe PhysicalDeviceFeatures
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DeviceCreateInfo es)

instance Extensible DeviceCreateInfo where
  extensibleTypeName = "DeviceCreateInfo"
  setNext x next = x{next = next}
  getNext DeviceCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DeviceCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PhysicalDeviceMutableDescriptorTypeFeaturesVALVE = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShadingRateEnumsFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderTerminateInvocationFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShadingRateFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderImageAtomicInt64FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDevice4444FormatsFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDevicePortabilitySubsetFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImageRobustnessFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRobustness2FeaturesEXT = Just f
    | Just Refl <- eqT @e @DeviceDiagnosticsConfigCreateInfoNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDiagnosticsConfigFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExtendedDynamicStateFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCustomBorderColorFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCoherentMemoryFeaturesAMD = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan12Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan11Features = Just f
    | Just Refl <- eqT @e @PhysicalDevicePipelineCreationCacheControlFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceLineRasterizationFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSubgroupSizeControlFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTexelBufferAlignmentFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDevicePipelineExecutablePropertiesFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSeparateDepthStencilLayoutsFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShaderInterlockFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderSMBuiltinsFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceIndexTypeUint8FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderClockFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCoverageReductionModeFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDevicePerformanceQueryFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceYcbcrImageArraysFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCooperativeMatrixFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceImagelessFramebufferFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBufferDeviceAddressFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBufferDeviceAddressFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMemoryPriorityFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDepthClipEnableFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceUniformBufferStandardLayoutFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceScalarBlockLayoutFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentDensityMap2FeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentDensityMapFeaturesEXT = Just f
    | Just Refl <- eqT @e @DeviceMemoryOverallocationCreateInfoAMD = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayQueryFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRayTracingPipelineFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceAccelerationStructureFeaturesKHR = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMeshShaderFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShadingRateImageFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderImageFootprintFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceFragmentShaderBarycentricFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceComputeShaderDerivativesFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceCornerSampledImageFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceExclusiveScissorFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceRepresentativeFragmentTestFeaturesNV = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTransformFeedbackFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceASTCDecodeFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVertexAttributeDivisorFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderAtomicFloatFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderAtomicInt64Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkanMemoryModelFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceConditionalRenderingFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDevice8BitStorageFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTimelineSemaphoreFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDescriptorIndexingFeatures = Just f
    | Just Refl <- eqT @e @DeviceDeviceMemoryReportCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDeviceMemoryReportFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceHostQueryResetFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderFloat16Int8Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderDrawParametersFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceInlineUniformBlockFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceBlendOperationAdvancedFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceProtectedMemoryFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceSamplerYcbcrConversionFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceShaderSubgroupExtendedTypesFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDevice16BitStorageFeatures = Just f
    | Just Refl <- eqT @e @DeviceGroupDeviceCreateInfo = Just f
    | Just Refl <- eqT @e @PhysicalDeviceMultiviewFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVariablePointersFeatures = Just f
    | Just Refl <- eqT @e @(PhysicalDeviceFeatures2 '[]) = Just f
    | Just Refl <- eqT @e @PhysicalDevicePrivateDataFeaturesEXT = Just f
    | Just Refl <- eqT @e @DevicePrivateDataCreateInfoEXT = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = Just f
    | otherwise = Nothing

instance (Extendss DeviceCreateInfo es, PokeChain es) => ToCStruct (DeviceCreateInfo es) where
  withCStruct x f = allocaBytesAligned 72 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (queueCreateInfos)) :: Word32))
    pPQueueCreateInfos' <- ContT $ allocaBytesAligned @(DeviceQueueCreateInfo _) ((Data.Vector.length (queueCreateInfos)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPQueueCreateInfos' `plusPtr` (40 * (i)) :: Ptr (DeviceQueueCreateInfo _))) (e) . ($ ())) (queueCreateInfos)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (DeviceQueueCreateInfo _)))) (pPQueueCreateInfos')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (enabledLayerNames)) :: Word32))
    pPpEnabledLayerNames' <- ContT $ allocaBytesAligned @(Ptr CChar) ((Data.Vector.length (enabledLayerNames)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      ppEnabledLayerNames'' <- ContT $ useAsCString (e)
      lift $ poke (pPpEnabledLayerNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) ppEnabledLayerNames'') (enabledLayerNames)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (Ptr CChar)))) (pPpEnabledLayerNames')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (enabledExtensionNames)) :: Word32))
    pPpEnabledExtensionNames' <- ContT $ allocaBytesAligned @(Ptr CChar) ((Data.Vector.length (enabledExtensionNames)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      ppEnabledExtensionNames'' <- ContT $ useAsCString (e)
      lift $ poke (pPpEnabledExtensionNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) ppEnabledExtensionNames'') (enabledExtensionNames)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (Ptr CChar)))) (pPpEnabledExtensionNames')
    pEnabledFeatures'' <- case (enabledFeatures) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr PhysicalDeviceFeatures))) pEnabledFeatures''
    lift $ f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance (Extendss DeviceCreateInfo es, PeekChain es) => FromCStruct (DeviceCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @DeviceCreateFlags ((p `plusPtr` 16 :: Ptr DeviceCreateFlags))
    queueCreateInfoCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pQueueCreateInfos <- peek @(Ptr (DeviceQueueCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (DeviceQueueCreateInfo _))))
    pQueueCreateInfos' <- generateM (fromIntegral queueCreateInfoCount) (\i -> peekSomeCStruct (forgetExtensions ((pQueueCreateInfos `advancePtrBytes` (40 * (i)) :: Ptr (DeviceQueueCreateInfo _)))))
    enabledLayerCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    ppEnabledLayerNames <- peek @(Ptr (Ptr CChar)) ((p `plusPtr` 40 :: Ptr (Ptr (Ptr CChar))))
    ppEnabledLayerNames' <- generateM (fromIntegral enabledLayerCount) (\i -> packCString =<< peek ((ppEnabledLayerNames `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CChar))))
    enabledExtensionCount <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    ppEnabledExtensionNames <- peek @(Ptr (Ptr CChar)) ((p `plusPtr` 56 :: Ptr (Ptr (Ptr CChar))))
    ppEnabledExtensionNames' <- generateM (fromIntegral enabledExtensionCount) (\i -> packCString =<< peek ((ppEnabledExtensionNames `advancePtrBytes` (8 * (i)) :: Ptr (Ptr CChar))))
    pEnabledFeatures <- peek @(Ptr PhysicalDeviceFeatures) ((p `plusPtr` 64 :: Ptr (Ptr PhysicalDeviceFeatures)))
    pEnabledFeatures' <- maybePeek (\j -> peekCStruct @PhysicalDeviceFeatures (j)) pEnabledFeatures
    pure $ DeviceCreateInfo
             next flags pQueueCreateInfos' ppEnabledLayerNames' ppEnabledExtensionNames' pEnabledFeatures'

instance es ~ '[] => Zero (DeviceCreateInfo es) where
  zero = DeviceCreateInfo
           ()
           zero
           mempty
           mempty
           mempty
           Nothing

