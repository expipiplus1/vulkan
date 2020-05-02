{-# language CPP #-}
module Graphics.Vulkan.Core10.AllocationCallbacks  (AllocationCallbacks(..)) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.FuncPointers (PFN_vkAllocationFunction)
import Graphics.Vulkan.Core10.FuncPointers (PFN_vkFreeFunction)
import Graphics.Vulkan.Core10.FuncPointers (PFN_vkInternalAllocationNotification)
import Graphics.Vulkan.Core10.FuncPointers (PFN_vkInternalFreeNotification)
import Graphics.Vulkan.Core10.FuncPointers (PFN_vkReallocationFunction)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
-- | VkAllocationCallbacks - Structure containing callback function pointers
-- for memory allocation
--
-- == Valid Usage
--
-- -   @pfnAllocation@ /must/ be a valid pointer to a valid user-defined
--     'Graphics.Vulkan.Core10.FuncPointers.PFN_vkAllocationFunction'
--
-- -   @pfnReallocation@ /must/ be a valid pointer to a valid user-defined
--     'Graphics.Vulkan.Core10.FuncPointers.PFN_vkReallocationFunction'
--
-- -   @pfnFree@ /must/ be a valid pointer to a valid user-defined
--     'Graphics.Vulkan.Core10.FuncPointers.PFN_vkFreeFunction'
--
-- -   If either of @pfnInternalAllocation@ or @pfnInternalFree@ is not
--     @NULL@, both /must/ be valid callbacks
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.FuncPointers.PFN_vkAllocationFunction',
-- 'Graphics.Vulkan.Core10.FuncPointers.PFN_vkFreeFunction',
-- 'Graphics.Vulkan.Core10.FuncPointers.PFN_vkInternalAllocationNotification',
-- 'Graphics.Vulkan.Core10.FuncPointers.PFN_vkInternalFreeNotification',
-- 'Graphics.Vulkan.Core10.FuncPointers.PFN_vkReallocationFunction',
-- 'Graphics.Vulkan.Core10.Memory.allocateMemory',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.createAccelerationStructureKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.createAccelerationStructureNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_android_surface.createAndroidSurfaceKHR',
-- 'Graphics.Vulkan.Core10.Buffer.createBuffer',
-- 'Graphics.Vulkan.Core10.BufferView.createBufferView',
-- 'Graphics.Vulkan.Core10.CommandPool.createCommandPool',
-- 'Graphics.Vulkan.Core10.Pipeline.createComputePipelines',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.createDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.createDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_deferred_host_operations.createDeferredOperationKHR',
-- 'Graphics.Vulkan.Core10.DescriptorSet.createDescriptorPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.createDescriptorSetLayout',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.createDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.createDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.Core10.Device.createDevice',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.createDisplayModeKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.createDisplayPlaneSurfaceKHR',
-- 'Graphics.Vulkan.Core10.Event.createEvent',
-- 'Graphics.Vulkan.Core10.Fence.createFence',
-- 'Graphics.Vulkan.Core10.Pass.createFramebuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.createGraphicsPipelines',
-- 'Graphics.Vulkan.Extensions.VK_EXT_headless_surface.createHeadlessSurfaceEXT',
-- 'Graphics.Vulkan.Extensions.VK_MVK_ios_surface.createIOSSurfaceMVK',
-- 'Graphics.Vulkan.Core10.Image.createImage',
-- 'Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface.createImagePipeSurfaceFUCHSIA',
-- 'Graphics.Vulkan.Core10.ImageView.createImageView',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.createIndirectCommandsLayoutNV',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.createInstance',
-- 'Graphics.Vulkan.Extensions.VK_MVK_macos_surface.createMacOSSurfaceMVK',
-- 'Graphics.Vulkan.Extensions.VK_EXT_metal_surface.createMetalSurfaceEXT',
-- 'Graphics.Vulkan.Core10.PipelineCache.createPipelineCache',
-- 'Graphics.Vulkan.Core10.PipelineLayout.createPipelineLayout',
-- 'Graphics.Vulkan.Core10.Query.createQueryPool',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.createRayTracingPipelinesKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV',
-- 'Graphics.Vulkan.Core10.Pass.createRenderPass',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.createRenderPass2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2.createRenderPass2KHR',
-- 'Graphics.Vulkan.Core10.Sampler.createSampler',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.createSemaphore',
-- 'Graphics.Vulkan.Core10.Shader.createShaderModule',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display_swapchain.createSharedSwapchainsKHR',
-- 'Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface.createStreamDescriptorSurfaceGGP',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.createValidationCacheEXT',
-- 'Graphics.Vulkan.Extensions.VK_NN_vi_surface.createViSurfaceNN',
-- 'Graphics.Vulkan.Extensions.VK_KHR_wayland_surface.createWaylandSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_surface.createWin32SurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xcb_surface.createXcbSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xlib_surface.createXlibSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_ray_tracing.destroyAccelerationStructureKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.destroyAccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.Buffer.destroyBuffer',
-- 'Graphics.Vulkan.Core10.BufferView.destroyBufferView',
-- 'Graphics.Vulkan.Core10.CommandPool.destroyCommandPool',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.destroyDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.destroyDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_deferred_host_operations.destroyDeferredOperationKHR',
-- 'Graphics.Vulkan.Core10.DescriptorSet.destroyDescriptorPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.destroyDescriptorSetLayout',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.Core10.Device.destroyDevice',
-- 'Graphics.Vulkan.Core10.Event.destroyEvent',
-- 'Graphics.Vulkan.Core10.Fence.destroyFence',
-- 'Graphics.Vulkan.Core10.Pass.destroyFramebuffer',
-- 'Graphics.Vulkan.Core10.Image.destroyImage',
-- 'Graphics.Vulkan.Core10.ImageView.destroyImageView',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_generated_commands.destroyIndirectCommandsLayoutNV',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.destroyInstance',
-- 'Graphics.Vulkan.Core10.Pipeline.destroyPipeline',
-- 'Graphics.Vulkan.Core10.PipelineCache.destroyPipelineCache',
-- 'Graphics.Vulkan.Core10.PipelineLayout.destroyPipelineLayout',
-- 'Graphics.Vulkan.Core10.Query.destroyQueryPool',
-- 'Graphics.Vulkan.Core10.Pass.destroyRenderPass',
-- 'Graphics.Vulkan.Core10.Sampler.destroySampler',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.destroySemaphore',
-- 'Graphics.Vulkan.Core10.Shader.destroyShaderModule',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.destroySurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.destroySwapchainKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.destroyValidationCacheEXT',
-- 'Graphics.Vulkan.Core10.Memory.freeMemory',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.registerDeviceEventEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.registerDisplayEventEXT'
data AllocationCallbacks = AllocationCallbacks
  { -- | @pUserData@ is a value to be interpreted by the implementation of the
    -- callbacks. When any of the callbacks in 'AllocationCallbacks' are
    -- called, the Vulkan implementation will pass this value as the first
    -- parameter to the callback. This value /can/ vary each time an allocator
    -- is passed into a command, even when the same object takes an allocator
    -- in multiple commands.
    userData :: Ptr ()
  , -- | @pfnAllocation@ is a
    -- 'Graphics.Vulkan.Core10.FuncPointers.PFN_vkAllocationFunction' pointer
    -- to an application-defined memory allocation function.
    pfnAllocation :: PFN_vkAllocationFunction
  , -- | @pfnReallocation@ is a
    -- 'Graphics.Vulkan.Core10.FuncPointers.PFN_vkReallocationFunction' pointer
    -- to an application-defined memory reallocation function.
    pfnReallocation :: PFN_vkReallocationFunction
  , -- | @pfnFree@ is a 'Graphics.Vulkan.Core10.FuncPointers.PFN_vkFreeFunction'
    -- pointer to an application-defined memory free function.
    pfnFree :: PFN_vkFreeFunction
  , -- | @pfnInternalAllocation@ is a
    -- 'Graphics.Vulkan.Core10.FuncPointers.PFN_vkInternalAllocationNotification'
    -- pointer to an application-defined function that is called by the
    -- implementation when the implementation makes internal allocations.
    pfnInternalAllocation :: PFN_vkInternalAllocationNotification
  , -- | @pfnInternalFree@ is a
    -- 'Graphics.Vulkan.Core10.FuncPointers.PFN_vkInternalFreeNotification'
    -- pointer to an application-defined function that is called by the
    -- implementation when the implementation frees internal allocations.
    pfnInternalFree :: PFN_vkInternalFreeNotification
  }
  deriving (Typeable)
deriving instance Show AllocationCallbacks

instance ToCStruct AllocationCallbacks where
  withCStruct x f = allocaBytesAligned 48 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AllocationCallbacks{..} f = do
    poke ((p `plusPtr` 0 :: Ptr (Ptr ()))) (userData)
    poke ((p `plusPtr` 8 :: Ptr PFN_vkAllocationFunction)) (pfnAllocation)
    poke ((p `plusPtr` 16 :: Ptr PFN_vkReallocationFunction)) (pfnReallocation)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkFreeFunction)) (pfnFree)
    poke ((p `plusPtr` 32 :: Ptr PFN_vkInternalAllocationNotification)) (pfnInternalAllocation)
    poke ((p `plusPtr` 40 :: Ptr PFN_vkInternalFreeNotification)) (pfnInternalFree)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 8 :: Ptr PFN_vkAllocationFunction)) (zero)
    poke ((p `plusPtr` 16 :: Ptr PFN_vkReallocationFunction)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PFN_vkFreeFunction)) (zero)
    f

instance FromCStruct AllocationCallbacks where
  peekCStruct p = do
    pUserData <- peek @(Ptr ()) ((p `plusPtr` 0 :: Ptr (Ptr ())))
    pfnAllocation <- peek @PFN_vkAllocationFunction ((p `plusPtr` 8 :: Ptr PFN_vkAllocationFunction))
    pfnReallocation <- peek @PFN_vkReallocationFunction ((p `plusPtr` 16 :: Ptr PFN_vkReallocationFunction))
    pfnFree <- peek @PFN_vkFreeFunction ((p `plusPtr` 24 :: Ptr PFN_vkFreeFunction))
    pfnInternalAllocation <- peek @PFN_vkInternalAllocationNotification ((p `plusPtr` 32 :: Ptr PFN_vkInternalAllocationNotification))
    pfnInternalFree <- peek @PFN_vkInternalFreeNotification ((p `plusPtr` 40 :: Ptr PFN_vkInternalFreeNotification))
    pure $ AllocationCallbacks
             pUserData pfnAllocation pfnReallocation pfnFree pfnInternalAllocation pfnInternalFree

instance Storable AllocationCallbacks where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AllocationCallbacks where
  zero = AllocationCallbacks
           zero
           zero
           zero
           zero
           zero
           zero

