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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Control.Monad.IO.Class (MonadIO)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
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
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
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

-- No documentation found for TopLevel "vkCreateDevice"
createDevice :: forall a io
              . (Extendss DeviceCreateInfo a, PokeChain a, MonadIO io)
             => -- No documentation found for Nested "vkCreateDevice" "physicalDevice"
                PhysicalDevice
             -> -- No documentation found for Nested "vkCreateDevice" "pCreateInfo"
                (DeviceCreateInfo a)
             -> -- No documentation found for Nested "vkCreateDevice" "pAllocator"
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
  r <- lift $ vkCreateDevice' (physicalDeviceHandle (physicalDevice)) (forgetExtensions pCreateInfo) pAllocator (pPDevice)
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

-- No documentation found for TopLevel "vkDestroyDevice"
destroyDevice :: forall io
               . (MonadIO io)
              => -- No documentation found for Nested "vkDestroyDevice" "device"
                 Device
              -> -- No documentation found for Nested "vkDestroyDevice" "pAllocator"
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
  lift $ vkDestroyDevice' (deviceHandle (device)) pAllocator
  pure $ ()



-- No documentation found for TopLevel "VkDeviceQueueCreateInfo"
data DeviceQueueCreateInfo (es :: [Type]) = DeviceQueueCreateInfo
  { -- No documentation found for Nested "VkDeviceQueueCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "flags"
    flags :: DeviceQueueCreateFlags
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "queueFamilyIndex"
    queueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkDeviceQueueCreateInfo" "pQueuePriorities"
    queuePriorities :: Vector Float
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceQueueCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DeviceQueueCreateInfo es)

instance Extensible DeviceQueueCreateInfo where
  extensibleType = STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
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
    pPQueuePriorities' <- ContT $ allocaBytesAligned @CFloat ((Data.Vector.length (mempty)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPQueuePriorities' `plusPtr` (4 * (i)) :: Ptr CFloat) (CFloat (e))) (mempty)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr CFloat))) (pPQueuePriorities')
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
      pure $ (\(CFloat a) -> a) pQueuePrioritiesElem)
    pure $ DeviceQueueCreateInfo
             next flags queueFamilyIndex pQueuePriorities'

instance es ~ '[] => Zero (DeviceQueueCreateInfo es) where
  zero = DeviceQueueCreateInfo
           ()
           zero
           zero
           mempty



-- No documentation found for TopLevel "VkDeviceCreateInfo"
data DeviceCreateInfo (es :: [Type]) = DeviceCreateInfo
  { -- No documentation found for Nested "VkDeviceCreateInfo" "pNext"
    next :: Chain es
  , -- No documentation found for Nested "VkDeviceCreateInfo" "flags"
    flags :: DeviceCreateFlags
  , -- No documentation found for Nested "VkDeviceCreateInfo" "pQueueCreateInfos"
    queueCreateInfos :: Vector (SomeStruct DeviceQueueCreateInfo)
  , -- No documentation found for Nested "VkDeviceCreateInfo" "ppEnabledLayerNames"
    enabledLayerNames :: Vector ByteString
  , -- No documentation found for Nested "VkDeviceCreateInfo" "ppEnabledExtensionNames"
    enabledExtensionNames :: Vector ByteString
  , -- No documentation found for Nested "VkDeviceCreateInfo" "pEnabledFeatures"
    enabledFeatures :: Maybe PhysicalDeviceFeatures
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (DeviceCreateInfo es)

instance Extensible DeviceCreateInfo where
  extensibleType = STRUCTURE_TYPE_DEVICE_CREATE_INFO
  setNext x next = x{next = next}
  getNext DeviceCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DeviceCreateInfo e => b) -> Maybe b
  extends _ f
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
    pPQueueCreateInfos' <- ContT $ allocaBytesAligned @(DeviceQueueCreateInfo _) ((Data.Vector.length (mempty)) * 40) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPQueueCreateInfos' `plusPtr` (40 * (i)) :: Ptr (DeviceQueueCreateInfo _))) (e) . ($ ())) (mempty)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (DeviceQueueCreateInfo _)))) (pPQueueCreateInfos')
    pPpEnabledLayerNames' <- ContT $ allocaBytesAligned @(Ptr CChar) ((Data.Vector.length (mempty)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      ppEnabledLayerNames'' <- ContT $ useAsCString (e)
      lift $ poke (pPpEnabledLayerNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) ppEnabledLayerNames'') (mempty)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr (Ptr CChar)))) (pPpEnabledLayerNames')
    pPpEnabledExtensionNames' <- ContT $ allocaBytesAligned @(Ptr CChar) ((Data.Vector.length (mempty)) * 8) 8
    Data.Vector.imapM_ (\i e -> do
      ppEnabledExtensionNames'' <- ContT $ useAsCString (e)
      lift $ poke (pPpEnabledExtensionNames' `plusPtr` (8 * (i)) :: Ptr (Ptr CChar)) ppEnabledExtensionNames'') (mempty)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr (Ptr CChar)))) (pPpEnabledExtensionNames')
    lift $ f

instance (Extendss DeviceCreateInfo es, PeekChain es) => FromCStruct (DeviceCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @DeviceCreateFlags ((p `plusPtr` 16 :: Ptr DeviceCreateFlags))
    queueCreateInfoCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pQueueCreateInfos <- peek @(Ptr (DeviceQueueCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (DeviceQueueCreateInfo a))))
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

