{-# language CPP #-}
module Graphics.Vulkan.Extensions.Handles  ( ObjectTableNVX(..)
                                           , IndirectCommandsLayoutNVX(..)
                                           , ValidationCacheEXT(..)
                                           , AccelerationStructureNV(..)
                                           , PerformanceConfigurationINTEL(..)
                                           , DisplayKHR(..)
                                           , DisplayModeKHR(..)
                                           , SurfaceKHR(..)
                                           , SwapchainKHR(..)
                                           , DebugReportCallbackEXT(..)
                                           , DebugUtilsMessengerEXT(..)
                                           , Instance(..)
                                           , PhysicalDevice(..)
                                           , Device(..)
                                           , Queue(..)
                                           , CommandBuffer(..)
                                           , DeviceMemory(..)
                                           , CommandPool(..)
                                           , Buffer(..)
                                           , BufferView(..)
                                           , Image(..)
                                           , ImageView(..)
                                           , ShaderModule(..)
                                           , Pipeline(..)
                                           , PipelineLayout(..)
                                           , Sampler(..)
                                           , DescriptorSet(..)
                                           , DescriptorSetLayout(..)
                                           , Fence(..)
                                           , Semaphore(..)
                                           , QueryPool(..)
                                           , Framebuffer(..)
                                           , RenderPass(..)
                                           , PipelineCache(..)
                                           , DescriptorUpdateTemplate(..)
                                           , SamplerYcbcrConversion(..)
                                           ) where

import GHC.Show (showParen)
import Numeric (showHex)
import Foreign.Storable (Storable)
import Data.Word (Word64)
import Graphics.Vulkan.Core10.APIConstants (IsHandle)
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Core10.Handles (Buffer(..))
import Graphics.Vulkan.Core10.Handles (BufferView(..))
import Graphics.Vulkan.Core10.Handles (CommandBuffer(..))
import Graphics.Vulkan.Core10.Handles (CommandPool(..))
import Graphics.Vulkan.Core10.Handles (DescriptorSet(..))
import Graphics.Vulkan.Core10.Handles (DescriptorSetLayout(..))
import Graphics.Vulkan.Core11.Handles (DescriptorUpdateTemplate(..))
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Core10.Handles (DeviceMemory(..))
import Graphics.Vulkan.Core10.Handles (Fence(..))
import Graphics.Vulkan.Core10.Handles (Framebuffer(..))
import Graphics.Vulkan.Core10.Handles (Image(..))
import Graphics.Vulkan.Core10.Handles (ImageView(..))
import Graphics.Vulkan.Core10.Handles (Instance(..))
import Graphics.Vulkan.Core10.Handles (PhysicalDevice(..))
import Graphics.Vulkan.Core10.Handles (Pipeline(..))
import Graphics.Vulkan.Core10.Handles (PipelineCache(..))
import Graphics.Vulkan.Core10.Handles (PipelineLayout(..))
import Graphics.Vulkan.Core10.Handles (QueryPool(..))
import Graphics.Vulkan.Core10.Handles (Queue(..))
import Graphics.Vulkan.Core10.Handles (RenderPass(..))
import Graphics.Vulkan.Core10.Handles (Sampler(..))
import Graphics.Vulkan.Core11.Handles (SamplerYcbcrConversion(..))
import Graphics.Vulkan.Core10.Handles (Semaphore(..))
import Graphics.Vulkan.Core10.Handles (ShaderModule(..))
-- | VkObjectTableNVX - Opaque handle to an object table
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.CmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.CmdReserveSpaceForCommandsInfoNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.createObjectTableNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.destroyObjectTableNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.registerObjectsNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.unregisterObjectsNVX'
newtype ObjectTableNVX = ObjectTableNVX Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show ObjectTableNVX where
  showsPrec p (ObjectTableNVX x) = showParen (p >= 11) (showString "ObjectTableNVX 0x" . showHex x)


-- | VkIndirectCommandsLayoutNVX - Opaque handle to an indirect commands
-- layout object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.CmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.CmdReserveSpaceForCommandsInfoNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.createIndirectCommandsLayoutNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.destroyIndirectCommandsLayoutNVX'
newtype IndirectCommandsLayoutNVX = IndirectCommandsLayoutNVX Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show IndirectCommandsLayoutNVX where
  showsPrec p (IndirectCommandsLayoutNVX x) = showParen (p >= 11) (showString "IndirectCommandsLayoutNVX 0x" . showHex x)


-- | VkValidationCacheEXT - Opaque handle to a validation cache object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.ShaderModuleValidationCacheCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.createValidationCacheEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.destroyValidationCacheEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.getValidationCacheDataEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.mergeValidationCachesEXT'
newtype ValidationCacheEXT = ValidationCacheEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show ValidationCacheEXT where
  showsPrec p (ValidationCacheEXT x) = showParen (p >= 11) (showString "ValidationCacheEXT 0x" . showHex x)


-- | VkAccelerationStructureNV - Opaque handle to an acceleration structure
-- object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureMemoryRequirementsInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.BindAccelerationStructureMemoryInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.WriteDescriptorSetAccelerationStructureNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdCopyAccelerationStructureNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.createAccelerationStructureNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.destroyAccelerationStructureNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
newtype AccelerationStructureNV = AccelerationStructureNV Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show AccelerationStructureNV where
  showsPrec p (AccelerationStructureNV x) = showParen (p >= 11) (showString "AccelerationStructureNV 0x" . showHex x)


-- | VkPerformanceConfigurationINTEL - Device configuration for performance
-- queries
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.acquirePerformanceConfigurationINTEL',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.queueSetPerformanceConfigurationINTEL',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.releasePerformanceConfigurationINTEL'
newtype PerformanceConfigurationINTEL = PerformanceConfigurationINTEL Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show PerformanceConfigurationINTEL where
  showsPrec p (PerformanceConfigurationINTEL x) = showParen (p >= 11) (showString "PerformanceConfigurationINTEL 0x" . showHex x)


-- | VkDisplayKHR - Opaque handle to a display object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.DisplayPlanePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display.acquireXlibDisplayEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.createDisplayModeKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.displayPowerControlEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2.getDisplayModeProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.getDisplayModePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.getDisplayPlaneSupportedDisplaysKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display.getRandROutputDisplayEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.registerDisplayEventEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display.releaseDisplayEXT'
newtype DisplayKHR = DisplayKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DisplayKHR where
  showsPrec p (DisplayKHR x) = showParen (p >= 11) (showString "DisplayKHR 0x" . showHex x)


-- | VkDisplayModeKHR - Opaque handle to a display mode object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.DisplayModePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayPlaneInfo2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.createDisplayModeKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.getDisplayPlaneCapabilitiesKHR'
newtype DisplayModeKHR = DisplayModeKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DisplayModeKHR where
  showsPrec p (DisplayModeKHR x) = showParen (p >= 11) (showString "DisplayModeKHR 0x" . showHex x)


-- | VkSurfaceKHR - Opaque handle to a surface object
--
-- = Description
--
-- The @VK_KHR_surface@ extension declares the 'SurfaceKHR' object, and
-- provides a function for destroying 'SurfaceKHR' objects. Separate
-- platform-specific extensions each provide a function for creating a
-- 'SurfaceKHR' object for the respective platform. From the application’s
-- perspective this is an opaque handle, just like the handles of other
-- Vulkan objects.
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_android_surface.createAndroidSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.createDisplayPlaneSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_headless_surface.createHeadlessSurfaceEXT',
-- 'Graphics.Vulkan.Extensions.VK_MVK_ios_surface.createIOSSurfaceMVK',
-- 'Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface.createImagePipeSurfaceFUCHSIA',
-- 'Graphics.Vulkan.Extensions.VK_MVK_macos_surface.createMacOSSurfaceMVK',
-- 'Graphics.Vulkan.Extensions.VK_EXT_metal_surface.createMetalSurfaceEXT',
-- 'Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface.createStreamDescriptorSurfaceGGP',
-- 'Graphics.Vulkan.Extensions.VK_NN_vi_surface.createViSurfaceNN',
-- 'Graphics.Vulkan.Extensions.VK_KHR_wayland_surface.createWaylandSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_surface.createWin32SurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xcb_surface.createXcbSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xlib_surface.createXlibSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.destroySurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupSurfacePresentModesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.getPhysicalDevicePresentRectanglesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.getPhysicalDeviceSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
newtype SurfaceKHR = SurfaceKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show SurfaceKHR where
  showsPrec p (SurfaceKHR x) = showParen (p >= 11) (showString "SurfaceKHR 0x" . showHex x)


-- | VkSwapchainKHR - Opaque handle to a swapchain object
--
-- = Description
--
-- A swapchain is an abstraction for an array of presentable images that
-- are associated with a surface. The presentable images are represented by
-- 'Graphics.Vulkan.Core10.Handles.Image' objects created by the platform.
-- One image (which /can/ be an array image for multiview\/stereoscopic-3D
-- surfaces) is displayed at a time, but multiple images /can/ be queued
-- for presentation. An application renders to the image, and then queues
-- the image for presentation to the surface.
--
-- A native window /cannot/ be associated with more than one non-retired
-- swapchain at a time. Further, swapchains /cannot/ be created for native
-- windows that have a non-Vulkan graphics API surface associated with
-- them.
--
-- Note
--
-- The presentation engine is an abstraction for the platform’s compositor
-- or display engine.
--
-- The presentation engine /may/ be synchronous or asynchronous with
-- respect to the application and\/or logical device.
--
-- Some implementations /may/ use the device’s graphics queue or dedicated
-- presentation hardware to perform presentation.
--
-- The presentable images of a swapchain are owned by the presentation
-- engine. An application /can/ acquire use of a presentable image from the
-- presentation engine. Use of a presentable image /must/ occur only after
-- the image is returned by
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR', and
-- before it is presented by
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR'. This
-- includes transitioning the image layout and rendering commands.
--
-- An application /can/ acquire use of a presentable image with
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR'. After
-- acquiring a presentable image and before modifying it, the application
-- /must/ use a synchronization primitive to ensure that the presentation
-- engine has finished reading from the image. The application /can/ then
-- transition the image’s layout, queue rendering commands to it, etc.
-- Finally, the application presents the image with
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', which
-- releases the acquisition of the image.
--
-- The presentation engine controls the order in which presentable images
-- are acquired for use by the application.
--
-- Note
--
-- This allows the platform to handle situations which require out-of-order
-- return of images after presentation. At the same time, it allows the
-- application to generate command buffers referencing all of the images in
-- the swapchain at initialization time, rather than in its main loop.
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.AcquireNextImageInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.BindImageMemorySwapchainInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive.acquireFullScreenExclusiveModeEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display_swapchain.createSharedSwapchainsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.destroySwapchainKHR',
-- 'Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing.getPastPresentationTimingGOOGLE',
-- 'Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing.getRefreshCycleDurationGOOGLE',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.getSwapchainCounterEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.getSwapchainImagesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.getSwapchainStatusKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive.releaseFullScreenExclusiveModeEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata.setHdrMetadataEXT',
-- 'Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr.setLocalDimmingAMD'
newtype SwapchainKHR = SwapchainKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show SwapchainKHR where
  showsPrec p (SwapchainKHR x) = showParen (p >= 11) (showString "SwapchainKHR 0x" . showHex x)


-- | VkDebugReportCallbackEXT - Opaque handle to a debug report callback
-- object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.createDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.destroyDebugReportCallbackEXT'
newtype DebugReportCallbackEXT = DebugReportCallbackEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DebugReportCallbackEXT where
  showsPrec p (DebugReportCallbackEXT x) = showParen (p >= 11) (showString "DebugReportCallbackEXT 0x" . showHex x)


-- | VkDebugUtilsMessengerEXT - Opaque handle to a debug messenger object
--
-- = Description
--
-- The debug messenger will provide detailed feedback on the application’s
-- use of Vulkan when events of interest occur. When an event of interest
-- does occur, the debug messenger will submit a debug message to the debug
-- callback that was provided during its creation. Additionally, the debug
-- messenger is responsible with filtering out debug messages that the
-- callback is not interested in and will only provide desired debug
-- messages.
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.createDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.destroyDebugUtilsMessengerEXT'
newtype DebugUtilsMessengerEXT = DebugUtilsMessengerEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DebugUtilsMessengerEXT where
  showsPrec p (DebugUtilsMessengerEXT x) = showParen (p >= 11) (showString "DebugUtilsMessengerEXT 0x" . showHex x)

