{-# language CPP #-}
module Vulkan.Core10.AllocationCallbacks  (AllocationCallbacks(..)) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (plusPtr)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.FuncPointers (PFN_vkAllocationFunction)
import Vulkan.Core10.FuncPointers (PFN_vkFreeFunction)
import Vulkan.Core10.FuncPointers (PFN_vkInternalAllocationNotification)
import Vulkan.Core10.FuncPointers (PFN_vkInternalFreeNotification)
import Vulkan.Core10.FuncPointers (PFN_vkReallocationFunction)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
-- | VkAllocationCallbacks - Structure containing callback function pointers
-- for memory allocation
--
-- == Valid Usage
--
-- -   @pfnAllocation@ /must/ be a valid pointer to a valid user-defined
--     'Vulkan.Core10.FuncPointers.PFN_vkAllocationFunction'
--
-- -   @pfnReallocation@ /must/ be a valid pointer to a valid user-defined
--     'Vulkan.Core10.FuncPointers.PFN_vkReallocationFunction'
--
-- -   @pfnFree@ /must/ be a valid pointer to a valid user-defined
--     'Vulkan.Core10.FuncPointers.PFN_vkFreeFunction'
--
-- -   If either of @pfnInternalAllocation@ or @pfnInternalFree@ is not
--     @NULL@, both /must/ be valid callbacks
--
-- = See Also
--
-- 'Vulkan.Core10.FuncPointers.PFN_vkAllocationFunction',
-- 'Vulkan.Core10.FuncPointers.PFN_vkFreeFunction',
-- 'Vulkan.Core10.FuncPointers.PFN_vkInternalAllocationNotification',
-- 'Vulkan.Core10.FuncPointers.PFN_vkInternalFreeNotification',
-- 'Vulkan.Core10.FuncPointers.PFN_vkReallocationFunction',
-- 'Vulkan.Core10.Memory.allocateMemory',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.createAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.createAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_KHR_android_surface.createAndroidSurfaceKHR',
-- 'Vulkan.Core10.Buffer.createBuffer',
-- 'Vulkan.Core10.BufferView.createBufferView',
-- 'Vulkan.Core10.CommandPool.createCommandPool',
-- 'Vulkan.Core10.Pipeline.createComputePipelines',
-- 'Vulkan.Extensions.VK_EXT_debug_report.createDebugReportCallbackEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.createDebugUtilsMessengerEXT',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.createDeferredOperationKHR',
-- 'Vulkan.Core10.DescriptorSet.createDescriptorPool',
-- 'Vulkan.Core10.DescriptorSet.createDescriptorSetLayout',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.createDescriptorUpdateTemplate',
-- 'Vulkan.Extensions.VK_KHR_descriptor_update_template.createDescriptorUpdateTemplateKHR',
-- 'Vulkan.Core10.Device.createDevice',
-- 'Vulkan.Extensions.VK_KHR_display.createDisplayModeKHR',
-- 'Vulkan.Extensions.VK_KHR_display.createDisplayPlaneSurfaceKHR',
-- 'Vulkan.Core10.Event.createEvent', 'Vulkan.Core10.Fence.createFence',
-- 'Vulkan.Core10.Pass.createFramebuffer',
-- 'Vulkan.Core10.Pipeline.createGraphicsPipelines',
-- 'Vulkan.Extensions.VK_EXT_headless_surface.createHeadlessSurfaceEXT',
-- 'Vulkan.Extensions.VK_MVK_ios_surface.createIOSSurfaceMVK',
-- 'Vulkan.Core10.Image.createImage',
-- 'Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface.createImagePipeSurfaceFUCHSIA',
-- 'Vulkan.Core10.ImageView.createImageView',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.createIndirectCommandsLayoutNV',
-- 'Vulkan.Core10.DeviceInitialization.createInstance',
-- 'Vulkan.Extensions.VK_MVK_macos_surface.createMacOSSurfaceMVK',
-- 'Vulkan.Extensions.VK_EXT_metal_surface.createMetalSurfaceEXT',
-- 'Vulkan.Core10.PipelineCache.createPipelineCache',
-- 'Vulkan.Core10.PipelineLayout.createPipelineLayout',
-- 'Vulkan.Core10.Query.createQueryPool',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.createRayTracingPipelinesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV',
-- 'Vulkan.Core10.Pass.createRenderPass',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.createRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.createRenderPass2KHR',
-- 'Vulkan.Core10.Sampler.createSampler',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversion',
-- 'Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversionKHR',
-- 'Vulkan.Core10.QueueSemaphore.createSemaphore',
-- 'Vulkan.Core10.Shader.createShaderModule',
-- 'Vulkan.Extensions.VK_KHR_display_swapchain.createSharedSwapchainsKHR',
-- 'Vulkan.Extensions.VK_GGP_stream_descriptor_surface.createStreamDescriptorSurfaceGGP',
-- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.createValidationCacheEXT',
-- 'Vulkan.Extensions.VK_NN_vi_surface.createViSurfaceNN',
-- 'Vulkan.Extensions.VK_KHR_wayland_surface.createWaylandSurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_win32_surface.createWin32SurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_xcb_surface.createXcbSurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_xlib_surface.createXlibSurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.destroyAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.destroyAccelerationStructureNV',
-- 'Vulkan.Core10.Buffer.destroyBuffer',
-- 'Vulkan.Core10.BufferView.destroyBufferView',
-- 'Vulkan.Core10.CommandPool.destroyCommandPool',
-- 'Vulkan.Extensions.VK_EXT_debug_report.destroyDebugReportCallbackEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.destroyDebugUtilsMessengerEXT',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.destroyDeferredOperationKHR',
-- 'Vulkan.Core10.DescriptorSet.destroyDescriptorPool',
-- 'Vulkan.Core10.DescriptorSet.destroyDescriptorSetLayout',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplate',
-- 'Vulkan.Extensions.VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplateKHR',
-- 'Vulkan.Core10.Device.destroyDevice',
-- 'Vulkan.Core10.Event.destroyEvent', 'Vulkan.Core10.Fence.destroyFence',
-- 'Vulkan.Core10.Pass.destroyFramebuffer',
-- 'Vulkan.Core10.Image.destroyImage',
-- 'Vulkan.Core10.ImageView.destroyImageView',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.destroyIndirectCommandsLayoutNV',
-- 'Vulkan.Core10.DeviceInitialization.destroyInstance',
-- 'Vulkan.Core10.Pipeline.destroyPipeline',
-- 'Vulkan.Core10.PipelineCache.destroyPipelineCache',
-- 'Vulkan.Core10.PipelineLayout.destroyPipelineLayout',
-- 'Vulkan.Core10.Query.destroyQueryPool',
-- 'Vulkan.Core10.Pass.destroyRenderPass',
-- 'Vulkan.Core10.Sampler.destroySampler',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversion',
-- 'Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversionKHR',
-- 'Vulkan.Core10.QueueSemaphore.destroySemaphore',
-- 'Vulkan.Core10.Shader.destroyShaderModule',
-- 'Vulkan.Extensions.VK_KHR_surface.destroySurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.destroySwapchainKHR',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.destroyValidationCacheEXT',
-- 'Vulkan.Core10.Memory.freeMemory',
-- 'Vulkan.Extensions.VK_EXT_display_control.registerDeviceEventEXT',
-- 'Vulkan.Extensions.VK_EXT_display_control.registerDisplayEventEXT'
data AllocationCallbacks = AllocationCallbacks
  { -- | @pUserData@ is a value to be interpreted by the implementation of the
    -- callbacks. When any of the callbacks in 'AllocationCallbacks' are
    -- called, the Vulkan implementation will pass this value as the first
    -- parameter to the callback. This value /can/ vary each time an allocator
    -- is passed into a command, even when the same object takes an allocator
    -- in multiple commands.
    userData :: Ptr ()
  , -- | @pfnAllocation@ is a
    -- 'Vulkan.Core10.FuncPointers.PFN_vkAllocationFunction' pointer to an
    -- application-defined memory allocation function.
    pfnAllocation :: PFN_vkAllocationFunction
  , -- | @pfnReallocation@ is a
    -- 'Vulkan.Core10.FuncPointers.PFN_vkReallocationFunction' pointer to an
    -- application-defined memory reallocation function.
    pfnReallocation :: PFN_vkReallocationFunction
  , -- | @pfnFree@ is a 'Vulkan.Core10.FuncPointers.PFN_vkFreeFunction' pointer
    -- to an application-defined memory free function.
    pfnFree :: PFN_vkFreeFunction
  , -- | @pfnInternalAllocation@ is a
    -- 'Vulkan.Core10.FuncPointers.PFN_vkInternalAllocationNotification'
    -- pointer to an application-defined function that is called by the
    -- implementation when the implementation makes internal allocations.
    pfnInternalAllocation :: PFN_vkInternalAllocationNotification
  , -- | @pfnInternalFree@ is a
    -- 'Vulkan.Core10.FuncPointers.PFN_vkInternalFreeNotification' pointer to
    -- an application-defined function that is called by the implementation
    -- when the implementation frees internal allocations.
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

