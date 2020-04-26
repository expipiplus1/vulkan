{-# language CPP #-}
module Graphics.Vulkan.Core10.Device  ( createDevice
                                      , withDevice
                                      , destroyDevice
                                      , DeviceQueueCreateInfo(..)
                                      , DeviceCreateInfo(..)
                                      ) where

import Control.Exception.Base (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.ByteString (ByteString)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Graphics.Vulkan.CStruct.Utils (advancePtrBytes)
import Graphics.Vulkan.CStruct.Extends (forgetExtensions)
import Graphics.Vulkan.Dynamic (initDeviceCmds)
import Graphics.Vulkan.CStruct.Extends (peekSomeCStruct)
import Graphics.Vulkan.CStruct.Extends (pokeSomeCStruct)
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Core10.Handles (Device(Device))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDestroyDevice))
import Graphics.Vulkan.Core10.Enums.DeviceCreateFlags (DeviceCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (DeviceGroupDeviceCreateInfo)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_AMD_memory_overallocation_behavior (DeviceMemoryOverallocationCreateInfoAMD)
import Graphics.Vulkan.Core10.Enums.DeviceQueueCreateFlagBits (DeviceQueueCreateFlags)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_global_priority (DeviceQueueGlobalPriorityCreateInfoEXT)
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.CStruct.Extends (Extends)
import Graphics.Vulkan.CStruct.Extends (Extensible(..))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Dynamic (InstanceCmds(pVkCreateDevice))
import Graphics.Vulkan.CStruct.Extends (PeekChain)
import Graphics.Vulkan.CStruct.Extends (PeekChain(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage (PhysicalDevice16BitStorageFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage (PhysicalDevice8BitStorageFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode (PhysicalDeviceASTCDecodeFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced (PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_AMD_device_coherent_memory (PhysicalDeviceCoherentMemoryFeaturesAMD)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering (PhysicalDeviceConditionalRenderingFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_corner_sampled_image (PhysicalDeviceCornerSampledImageFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_coverage_reduction_mode (PhysicalDeviceCoverageReductionModeFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing (PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable (PhysicalDeviceDepthClipEnableFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive (PhysicalDeviceExclusiveScissorFeaturesNV)
import Graphics.Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceFeatures2)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_fragment_shader_interlock (PhysicalDeviceFragmentShaderInterlockFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset (PhysicalDeviceHostQueryResetFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (PhysicalDeviceImagelessFramebufferFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_index_type_uint8 (PhysicalDeviceIndexTypeUint8FeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_line_rasterization (PhysicalDeviceLineRasterizationFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_memory_priority (PhysicalDeviceMemoryPriorityFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_mesh_shader (PhysicalDeviceMeshShaderFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_KHR_performance_query (PhysicalDevicePerformanceQueryFeaturesKHR)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PhysicalDevicePipelineExecutablePropertiesFeaturesKHR)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (PhysicalDeviceProtectedMemoryFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test (PhysicalDeviceRepresentativeFragmentTestFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (PhysicalDeviceSamplerYcbcrConversionFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout (PhysicalDeviceScalarBlockLayoutFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (PhysicalDeviceSeparateDepthStencilLayoutsFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64 (PhysicalDeviceShaderAtomicInt64Features)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_KHR_shader_clock (PhysicalDeviceShaderClockFeaturesKHR)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation (PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters (PhysicalDeviceShaderDrawParametersFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8 (PhysicalDeviceShaderFloat16Int8Features)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_shader_image_footprint (PhysicalDeviceShaderImageFootprintFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_INTEL_shader_integer_functions2 (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types (PhysicalDeviceShaderSubgroupExtendedTypesFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImageFeaturesNV)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_texel_buffer_alignment (PhysicalDeviceTexelBufferAlignmentFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr (PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout (PhysicalDeviceUniformBufferStandardLayoutFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers (PhysicalDeviceVariablePointersFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorFeaturesEXT)
import {-# SOURCE #-} Graphics.Vulkan.Core12 (PhysicalDeviceVulkan11Features)
import {-# SOURCE #-} Graphics.Vulkan.Core12 (PhysicalDeviceVulkan12Features)
import {-# SOURCE #-} Graphics.Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model (PhysicalDeviceVulkanMemoryModelFeatures)
import {-# SOURCE #-} Graphics.Vulkan.Extensions.VK_EXT_ycbcr_image_arrays (PhysicalDeviceYcbcrImageArraysFeaturesEXT)
import Graphics.Vulkan.Core10.Handles (PhysicalDevice_T)
import Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct.Extends (PokeChain(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.CStruct.Extends (SomeStruct)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDevice
  :: FunPtr (Ptr PhysicalDevice_T -> Ptr (DeviceCreateInfo a) -> Ptr AllocationCallbacks -> Ptr (Ptr Device_T) -> IO Result) -> Ptr PhysicalDevice_T -> Ptr (DeviceCreateInfo a) -> Ptr AllocationCallbacks -> Ptr (Ptr Device_T) -> IO Result

-- | vkCreateDevice - Create a new device instance
--
-- = Parameters
--
-- -   @physicalDevice@ /must/ be one of the device handles returned from a
--     call to
--     'Graphics.Vulkan.Core10.DeviceInitialization.enumeratePhysicalDevices'
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-physical-device-enumeration Physical Device Enumeration>).
--
-- -   @pCreateInfo@ is a pointer to a 'DeviceCreateInfo' structure
--     containing information about how to create the device.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pDevice@ is a pointer to a handle in which the created
--     'Graphics.Vulkan.Core10.Handles.Device' is returned.
--
-- = Description
--
-- 'createDevice' verifies that extensions and features requested in the
-- @ppEnabledExtensionNames@ and @pEnabledFeatures@ members of
-- @pCreateInfo@, respectively, are supported by the implementation. If any
-- requested extension is not supported, 'createDevice' /must/ return
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'. If
-- any requested feature is not supported, 'createDevice' /must/ return
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_FEATURE_NOT_PRESENT'. Support
-- for extensions /can/ be checked before creating a device by querying
-- 'Graphics.Vulkan.Core10.ExtensionDiscovery.enumerateDeviceExtensionProperties'.
-- Support for features /can/ similarly be checked by querying
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFeatures'.
--
-- After verifying and enabling the extensions the
-- 'Graphics.Vulkan.Core10.Handles.Device' object is created and returned
-- to the application. If a requested extension is only supported by a
-- layer, both the layer and the extension need to be specified at
-- 'Graphics.Vulkan.Core10.DeviceInitialization.createInstance' time for
-- the creation to succeed.
--
-- Multiple logical devices /can/ be created from the same physical device.
-- Logical device creation /may/ fail due to lack of device-specific
-- resources (in addition to the other errors). If that occurs,
-- 'createDevice' will return
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'.
--
-- == Valid Usage
--
-- -   All
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-extensions-extensiondependencies required extensions>
--     for each extension in the
--     'DeviceCreateInfo'::@ppEnabledExtensionNames@ list /must/ also be
--     present in that list.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'DeviceCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pDevice@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.Device' handle
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
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_EXTENSION_NOT_PRESENT'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_FEATURE_NOT_PRESENT'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_TOO_MANY_OBJECTS'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device', 'DeviceCreateInfo',
-- 'Graphics.Vulkan.Core10.Handles.PhysicalDevice'
createDevice :: forall a io . (PokeChain a, MonadIO io) => PhysicalDevice -> DeviceCreateInfo a -> ("allocator" ::: Maybe AllocationCallbacks) -> io (Device)
createDevice physicalDevice createInfo allocator = liftIO . evalContT $ do
  let cmds = instanceCmds (physicalDevice :: PhysicalDevice)
  let vkCreateDevice' = mkVkCreateDevice (pVkCreateDevice cmds)
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPDevice <- ContT $ bracket (callocBytes @(Ptr Device_T) 8) free
  r <- lift $ vkCreateDevice' (physicalDeviceHandle (physicalDevice)) pCreateInfo pAllocator (pPDevice)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pDevice <- lift $ peek @(Ptr Device_T) pPDevice
  pDevice' <- lift $ (\h -> Device h <$> initDeviceCmds cmds h) pDevice
  pure $ (pDevice')

-- | A convenience wrapper to make a compatible pair of 'createDevice' and
-- 'destroyDevice'
--
-- To ensure that 'destroyDevice' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the first argument.
-- To just extract the pair pass '(,)' as the first argument.
--
withDevice :: forall a io r . (PokeChain a, MonadIO io) => (io (Device) -> ((Device) -> io ()) -> r) -> PhysicalDevice -> DeviceCreateInfo a -> Maybe AllocationCallbacks -> r
withDevice b physicalDevice pCreateInfo pAllocator =
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
-- = Parameters
--
-- -   @device@ is the logical device to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- = Description
--
-- To ensure that no work is active on the device,
-- 'Graphics.Vulkan.Core10.Queue.deviceWaitIdle' /can/ be used to gate the
-- destruction of the device. Prior to destroying a device, an application
-- is responsible for destroying\/freeing any Vulkan objects that were
-- created using that device as the first parameter of the corresponding
-- @vkCreate*@ or @vkAllocate*@ command.
--
-- Note
--
-- The lifetime of each of these objects is bound by the lifetime of the
-- 'Graphics.Vulkan.Core10.Handles.Device' object. Therefore, to avoid
-- resource leaks, it is critical that an application explicitly free all
-- of these resources prior to calling 'destroyDevice'.
--
-- == Valid Usage
--
-- -   All child objects created on @device@ /must/ have been destroyed
--     prior to destroying @device@
--
-- -   If 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @device@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     were provided when @device@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   If @device@ is not @NULL@, @device@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- == Host Synchronization
--
-- -   Host access to @device@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device'
destroyDevice :: forall io . MonadIO io => Device -> ("allocator" ::: Maybe AllocationCallbacks) -> io ()
destroyDevice device allocator = liftIO . evalContT $ do
  let vkDestroyDevice' = mkVkDestroyDevice (pVkDestroyDevice (deviceCmds (device :: Device)))
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ vkDestroyDevice' (deviceHandle (device)) pAllocator
  pure $ ()


-- | VkDeviceQueueCreateInfo - Structure specifying parameters of a newly
-- created device queue
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be less than @pQueueFamilyPropertyCount@
--     returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--
-- -   @queueCount@ /must/ be less than or equal to the @queueCount@ member
--     of the
--     'Graphics.Vulkan.Core10.DeviceInitialization.QueueFamilyProperties'
--     structure, as returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'
--     in the @pQueueFamilyProperties@[queueFamilyIndex]
--
-- -   Each element of @pQueuePriorities@ /must/ be between @0.0@ and @1.0@
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-protectedMemory protected memory>
--     feature is not enabled, the
--     'Graphics.Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DEVICE_QUEUE_CREATE_PROTECTED_BIT'
--     bit of @flags@ /must/ not be set.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_global_priority.DeviceQueueGlobalPriorityCreateInfoEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlagBits'
--     values
--
-- -   @pQueuePriorities@ /must/ be a valid pointer to an array of
--     @queueCount@ @float@ values
--
-- -   @queueCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'DeviceCreateInfo',
-- 'Graphics.Vulkan.Core10.Enums.DeviceQueueCreateFlagBits.DeviceQueueCreateFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data DeviceQueueCreateInfo (es :: [Type]) = DeviceQueueCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
    next :: Chain es
  , -- | @flags@ is a bitmask indicating behavior of the queue.
    flags :: DeviceQueueCreateFlags
  , -- | @queueFamilyIndex@ is an unsigned integer indicating the index of the
    -- queue family to create on this device. This index corresponds to the
    -- index of an element of the @pQueueFamilyProperties@ array that was
    -- returned by
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties'.
    queueFamilyIndex :: Word32
  , -- | @pQueuePriorities@ is a pointer to an array of @queueCount@ normalized
    -- floating point values, specifying priorities of work that will be
    -- submitted to each created queue. See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#devsandqueues-priority Queue Priority>
    -- for more information.
    queuePriorities :: Vector Float
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (DeviceQueueCreateInfo es)

instance Extensible DeviceQueueCreateInfo where
  extensibleType = STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
  setNext x next = x{next = next}
  getNext DeviceQueueCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DeviceQueueCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @DeviceQueueGlobalPriorityCreateInfoEXT = Just f
    | otherwise = Nothing

instance PokeChain es => ToCStruct (DeviceQueueCreateInfo es) where
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

instance PeekChain es => FromCStruct (DeviceQueueCreateInfo es) where
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


-- | VkDeviceCreateInfo - Structure specifying parameters of a newly created
-- device
--
-- == Valid Usage
--
-- -   The @queueFamilyIndex@ member of each element of @pQueueCreateInfos@
--     /must/ be unique within @pQueueCreateInfos@, except that two members
--     can share the same @queueFamilyIndex@ if one is a protected-capable
--     queue and one is not a protected-capable queue
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
--     structure, then @pEnabledFeatures@ /must/ be @NULL@
--
-- -   @ppEnabledExtensionNames@ /must/ not contain
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_AMD_negative_viewport_height@
--
-- -   @ppEnabledExtensionNames@ /must/ not contain both
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_KHR_buffer_device_address@
--     and
--     @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.2-extensions\/html\/vkspec.html#VK_EXT_buffer_device_address@
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan11Features' structure,
--     then it /must/ not include a
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage.PhysicalDevice16BitStorageFeatures',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers.PhysicalDeviceVariablePointersFeatures',
--     'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryFeatures',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.PhysicalDeviceSamplerYcbcrConversionFeatures',
--     or
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters.PhysicalDeviceShaderDrawParametersFeatures'
--     structure
--
-- -   If the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features' structure,
--     then it /must/ not include a
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage.PhysicalDevice8BitStorageFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64.PhysicalDeviceShaderAtomicInt64Features',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8Features',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout.PhysicalDeviceScalarBlockLayoutFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.PhysicalDeviceImagelessFramebufferFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout.PhysicalDeviceUniformBufferStandardLayoutFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types.PhysicalDeviceShaderSubgroupExtendedTypesFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.PhysicalDeviceSeparateDepthStencilLayoutsFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.PhysicalDeviceHostQueryResetFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures',
--     or
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeatures'
--     structure
--
-- -   If @ppEnabledExtensions@ contains @\"VK_KHR_draw_indirect_count\"@
--     and the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features' structure,
--     then
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features'::@drawIndirectCount@
--     /must/ be 'Graphics.Vulkan.Core10.BaseType.TRUE'
--
-- -   If @ppEnabledExtensions@ contains
--     @\"VK_KHR_sampler_mirror_clamp_to_edge\"@ and the @pNext@ chain
--     includes a 'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features'
--     structure, then
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features'::@samplerMirrorClampToEdge@
--     /must/ be 'Graphics.Vulkan.Core10.BaseType.TRUE'
--
-- -   If @ppEnabledExtensions@ contains @\"VK_EXT_descriptor_indexing\"@
--     and the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features' structure,
--     then
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features'::@descriptorIndexing@
--     /must/ be 'Graphics.Vulkan.Core10.BaseType.TRUE'
--
-- -   If @ppEnabledExtensions@ contains @\"VK_EXT_sampler_filter_minmax\"@
--     and the @pNext@ chain includes a
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features' structure,
--     then
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features'::@samplerFilterMinmax@
--     /must/ be 'Graphics.Vulkan.Core10.BaseType.TRUE'
--
-- -   If @ppEnabledExtensions@ contains
--     @\"VK_EXT_shader_viewport_index_layer\"@ and the @pNext@ chain
--     includes a 'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features'
--     structure, then
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features'::@shaderOutputViewportIndex@
--     and
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features'::@shaderOutputLayer@
--     /must/ both be 'Graphics.Vulkan.Core10.BaseType.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo',
--     'Graphics.Vulkan.Extensions.VK_AMD_memory_overallocation_behavior.DeviceMemoryOverallocationCreateInfoAMD',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage.PhysicalDevice16BitStorageFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage.PhysicalDevice8BitStorageFeatures',
--     'Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode.PhysicalDeviceASTCDecodeFeaturesEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedFeaturesEXT',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures',
--     'Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeaturesEXT',
--     'Graphics.Vulkan.Extensions.VK_AMD_device_coherent_memory.PhysicalDeviceCoherentMemoryFeaturesAMD',
--     'Graphics.Vulkan.Extensions.VK_NV_compute_shader_derivatives.PhysicalDeviceComputeShaderDerivativesFeaturesNV',
--     'Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering.PhysicalDeviceConditionalRenderingFeaturesEXT',
--     'Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix.PhysicalDeviceCooperativeMatrixFeaturesNV',
--     'Graphics.Vulkan.Extensions.VK_NV_corner_sampled_image.PhysicalDeviceCornerSampledImageFeaturesNV',
--     'Graphics.Vulkan.Extensions.VK_NV_coverage_reduction_mode.PhysicalDeviceCoverageReductionModeFeaturesNV',
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing.PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV',
--     'Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable.PhysicalDeviceDepthClipEnableFeaturesEXT',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures',
--     'Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive.PhysicalDeviceExclusiveScissorFeaturesNV',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapFeaturesEXT',
--     'Graphics.Vulkan.Extensions.VK_NV_fragment_shader_barycentric.PhysicalDeviceFragmentShaderBarycentricFeaturesNV',
--     'Graphics.Vulkan.Extensions.VK_EXT_fragment_shader_interlock.PhysicalDeviceFragmentShaderInterlockFeaturesEXT',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.PhysicalDeviceHostQueryResetFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.PhysicalDeviceImagelessFramebufferFeatures',
--     'Graphics.Vulkan.Extensions.VK_EXT_index_type_uint8.PhysicalDeviceIndexTypeUint8FeaturesEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockFeaturesEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.PhysicalDeviceLineRasterizationFeaturesEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_memory_priority.PhysicalDeviceMemoryPriorityFeaturesEXT',
--     'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderFeaturesNV',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures',
--     'Graphics.Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryFeaturesKHR',
--     'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PhysicalDevicePipelineExecutablePropertiesFeaturesKHR',
--     'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryFeatures',
--     'Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test.PhysicalDeviceRepresentativeFragmentTestFeaturesNV',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.PhysicalDeviceSamplerYcbcrConversionFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout.PhysicalDeviceScalarBlockLayoutFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.PhysicalDeviceSeparateDepthStencilLayoutsFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64.PhysicalDeviceShaderAtomicInt64Features',
--     'Graphics.Vulkan.Extensions.VK_KHR_shader_clock.PhysicalDeviceShaderClockFeaturesKHR',
--     'Graphics.Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation.PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters.PhysicalDeviceShaderDrawParametersFeatures',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8Features',
--     'Graphics.Vulkan.Extensions.VK_NV_shader_image_footprint.PhysicalDeviceShaderImageFootprintFeaturesNV',
--     'Graphics.Vulkan.Extensions.VK_INTEL_shader_integer_functions2.PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL',
--     'Graphics.Vulkan.Extensions.VK_NV_shader_sm_builtins.PhysicalDeviceShaderSMBuiltinsFeaturesNV',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types.PhysicalDeviceShaderSubgroupExtendedTypesFeatures',
--     'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImageFeaturesNV',
--     'Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeaturesEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
--     'Graphics.Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr.PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreFeatures',
--     'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackFeaturesEXT',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout.PhysicalDeviceUniformBufferStandardLayoutFeatures',
--     'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers.PhysicalDeviceVariablePointersFeatures',
--     'Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PhysicalDeviceVertexAttributeDivisorFeaturesEXT',
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan11Features',
--     'Graphics.Vulkan.Core12.PhysicalDeviceVulkan12Features',
--     'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeatures',
--     or
--     'Graphics.Vulkan.Extensions.VK_EXT_ycbcr_image_arrays.PhysicalDeviceYcbcrImageArraysFeaturesEXT'
--
-- -   The @sType@ value of each struct in the @pNext@ chain /must/ be
--     unique
--
-- -   @flags@ /must/ be @0@
--
-- -   @pQueueCreateInfos@ /must/ be a valid pointer to an array of
--     @queueCreateInfoCount@ valid 'DeviceQueueCreateInfo' structures
--
-- -   If @enabledLayerCount@ is not @0@, @ppEnabledLayerNames@ /must/ be a
--     valid pointer to an array of @enabledLayerCount@ null-terminated
--     UTF-8 strings
--
-- -   If @enabledExtensionCount@ is not @0@, @ppEnabledExtensionNames@
--     /must/ be a valid pointer to an array of @enabledExtensionCount@
--     null-terminated UTF-8 strings
--
-- -   If @pEnabledFeatures@ is not @NULL@, @pEnabledFeatures@ /must/ be a
--     valid pointer to a valid
--     'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures'
--     structure
--
-- -   @queueCreateInfoCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.DeviceCreateFlags.DeviceCreateFlags',
-- 'DeviceQueueCreateInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createDevice'
data DeviceCreateInfo (es :: [Type]) = DeviceCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
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
    -- 'Graphics.Vulkan.Core10.DeviceInitialization.PhysicalDeviceFeatures'
    -- structure containing boolean indicators of all the features to be
    -- enabled. Refer to the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features Features>
    -- section for further details.
    enabledFeatures :: Maybe PhysicalDeviceFeatures
  }
  deriving (Typeable)
deriving instance Show (Chain es) => Show (DeviceCreateInfo es)

instance Extensible DeviceCreateInfo where
  extensibleType = STRUCTURE_TYPE_DEVICE_CREATE_INFO
  setNext x next = x{next = next}
  getNext DeviceCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends DeviceCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PhysicalDeviceCoherentMemoryFeaturesAMD = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan12Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkan11Features = Just f
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
    | Just Refl <- eqT @e @PhysicalDeviceFragmentDensityMapFeaturesEXT = Just f
    | Just Refl <- eqT @e @DeviceMemoryOverallocationCreateInfoAMD = Just f
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
    | Just Refl <- eqT @e @PhysicalDeviceShaderAtomicInt64Features = Just f
    | Just Refl <- eqT @e @PhysicalDeviceVulkanMemoryModelFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceConditionalRenderingFeaturesEXT = Just f
    | Just Refl <- eqT @e @PhysicalDevice8BitStorageFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceTimelineSemaphoreFeatures = Just f
    | Just Refl <- eqT @e @PhysicalDeviceDescriptorIndexingFeatures = Just f
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
    | otherwise = Nothing

instance PokeChain es => ToCStruct (DeviceCreateInfo es) where
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

instance PeekChain es => FromCStruct (DeviceCreateInfo es) where
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

