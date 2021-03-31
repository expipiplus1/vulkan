{-# language CPP #-}
-- No documentation found for Chapter "Handles"
module Vulkan.Extensions.Handles  ( IndirectCommandsLayoutNV(..)
                                  , ValidationCacheEXT(..)
                                  , AccelerationStructureKHR(..)
                                  , AccelerationStructureNV(..)
                                  , PerformanceConfigurationINTEL(..)
                                  , DeferredOperationKHR(..)
                                  , PrivateDataSlotEXT(..)
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
                                  , Event(..)
                                  , QueryPool(..)
                                  , Framebuffer(..)
                                  , RenderPass(..)
                                  , PipelineCache(..)
                                  , DescriptorUpdateTemplate(..)
                                  , SamplerYcbcrConversion(..)
                                  ) where

import GHC.Show (showParen)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Word (Word64)
import Vulkan.Core10.APIConstants (HasObjectType(..))
import Vulkan.Core10.APIConstants (IsHandle)
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_ACCELERATION_STRUCTURE_NV))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEFERRED_OPERATION_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DISPLAY_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DISPLAY_MODE_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SURFACE_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SWAPCHAIN_KHR))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_VALIDATION_CACHE_EXT))
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Core10.Handles (BufferView(..))
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandPool(..))
import Vulkan.Core10.Handles (DescriptorSet(..))
import Vulkan.Core10.Handles (DescriptorSetLayout(..))
import Vulkan.Core11.Handles (DescriptorUpdateTemplate(..))
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (DeviceMemory(..))
import Vulkan.Core10.Handles (Event(..))
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.Core10.Handles (Framebuffer(..))
import Vulkan.Core10.Handles (Image(..))
import Vulkan.Core10.Handles (ImageView(..))
import Vulkan.Core10.Handles (Instance(..))
import Vulkan.Core10.Handles (PhysicalDevice(..))
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Core10.Handles (PipelineLayout(..))
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (RenderPass(..))
import Vulkan.Core10.Handles (Sampler(..))
import Vulkan.Core11.Handles (SamplerYcbcrConversion(..))
import Vulkan.Core10.Handles (Semaphore(..))
import Vulkan.Core10.Handles (ShaderModule(..))
-- | VkIndirectCommandsLayoutNV - Opaque handle to an indirect commands
-- layout object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsMemoryRequirementsInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.createIndirectCommandsLayoutNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.destroyIndirectCommandsLayoutNV'
newtype IndirectCommandsLayoutNV = IndirectCommandsLayoutNV Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType IndirectCommandsLayoutNV where
  objectTypeAndHandle (IndirectCommandsLayoutNV h) = (OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NV, h)
instance Show IndirectCommandsLayoutNV where
  showsPrec p (IndirectCommandsLayoutNV x) = showParen (p >= 11) (showString "IndirectCommandsLayoutNV 0x" . showHex x)


-- | VkValidationCacheEXT - Opaque handle to a validation cache object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_EXT_validation_cache.ShaderModuleValidationCacheCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.createValidationCacheEXT',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.destroyValidationCacheEXT',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.getValidationCacheDataEXT',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.mergeValidationCachesEXT'
newtype ValidationCacheEXT = ValidationCacheEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType ValidationCacheEXT where
  objectTypeAndHandle (ValidationCacheEXT h) = (OBJECT_TYPE_VALIDATION_CACHE_EXT, h)
instance Show ValidationCacheEXT where
  showsPrec p (ValidationCacheEXT x) = showParen (p >= 11) (showString "ValidationCacheEXT 0x" . showHex x)


-- | VkAccelerationStructureKHR - Opaque handle to an acceleration structure
-- object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureBuildGeometryInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureDeviceAddressInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.CopyAccelerationStructureInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.CopyAccelerationStructureToMemoryInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.CopyMemoryToAccelerationStructureInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.WriteDescriptorSetAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdWriteAccelerationStructuresPropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.createAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.destroyAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.writeAccelerationStructuresPropertiesKHR'
newtype AccelerationStructureKHR = AccelerationStructureKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType AccelerationStructureKHR where
  objectTypeAndHandle (AccelerationStructureKHR h) = (OBJECT_TYPE_ACCELERATION_STRUCTURE_KHR, h)
instance Show AccelerationStructureKHR where
  showsPrec p (AccelerationStructureKHR x) = showParen (p >= 11) (showString "AccelerationStructureKHR 0x" . showHex x)


-- | VkAccelerationStructureNV - Opaque handle to an acceleration structure
-- object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureMemoryRequirementsInfoNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.BindAccelerationStructureMemoryInfoNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.WriteDescriptorSetAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdCopyAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.createAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.destroyAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV'
newtype AccelerationStructureNV = AccelerationStructureNV Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType AccelerationStructureNV where
  objectTypeAndHandle (AccelerationStructureNV h) = (OBJECT_TYPE_ACCELERATION_STRUCTURE_NV, h)
instance Show AccelerationStructureNV where
  showsPrec p (AccelerationStructureNV x) = showParen (p >= 11) (showString "AccelerationStructureNV 0x" . showHex x)


-- | VkPerformanceConfigurationINTEL - Device configuration for performance
-- queries
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_INTEL_performance_query.acquirePerformanceConfigurationINTEL',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.queueSetPerformanceConfigurationINTEL',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.releasePerformanceConfigurationINTEL'
newtype PerformanceConfigurationINTEL = PerformanceConfigurationINTEL Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType PerformanceConfigurationINTEL where
  objectTypeAndHandle (PerformanceConfigurationINTEL h) = (OBJECT_TYPE_PERFORMANCE_CONFIGURATION_INTEL, h)
instance Show PerformanceConfigurationINTEL where
  showsPrec p (PerformanceConfigurationINTEL x) = showParen (p >= 11) (showString "PerformanceConfigurationINTEL 0x" . showHex x)


-- | VkDeferredOperationKHR - A deferred operation
--
-- = Description
--
-- This handle refers to a tracking structure which manages the execution
-- state for a deferred command.
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.buildAccelerationStructuresKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.copyAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.copyAccelerationStructureToMemoryKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.copyMemoryToAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.createDeferredOperationKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.createRayTracingPipelinesKHR',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.deferredOperationJoinKHR',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.destroyDeferredOperationKHR',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.getDeferredOperationMaxConcurrencyKHR',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.getDeferredOperationResultKHR'
newtype DeferredOperationKHR = DeferredOperationKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DeferredOperationKHR where
  objectTypeAndHandle (DeferredOperationKHR h) = (OBJECT_TYPE_DEFERRED_OPERATION_KHR, h)
instance Show DeferredOperationKHR where
  showsPrec p (DeferredOperationKHR x) = showParen (p >= 11) (showString "DeferredOperationKHR 0x" . showHex x)


-- | VkPrivateDataSlotEXT - Opaque handle to a private data slot object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_EXT_private_data.createPrivateDataSlotEXT',
-- 'Vulkan.Extensions.VK_EXT_private_data.destroyPrivateDataSlotEXT',
-- 'Vulkan.Extensions.VK_EXT_private_data.getPrivateDataEXT',
-- 'Vulkan.Extensions.VK_EXT_private_data.setPrivateDataEXT'
newtype PrivateDataSlotEXT = PrivateDataSlotEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType PrivateDataSlotEXT where
  objectTypeAndHandle (PrivateDataSlotEXT h) = (OBJECT_TYPE_PRIVATE_DATA_SLOT_EXT, h)
instance Show PrivateDataSlotEXT where
  showsPrec p (PrivateDataSlotEXT x) = showParen (p >= 11) (showString "PrivateDataSlotEXT 0x" . showHex x)


-- | VkDisplayKHR - Opaque handle to a display object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPlanePropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplayPropertiesKHR',
-- 'Vulkan.Extensions.VK_NV_acquire_winrt_display.acquireWinrtDisplayNV',
-- 'Vulkan.Extensions.VK_EXT_acquire_xlib_display.acquireXlibDisplayEXT',
-- 'Vulkan.Extensions.VK_KHR_display.createDisplayModeKHR',
-- 'Vulkan.Extensions.VK_EXT_display_control.displayPowerControlEXT',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.getDisplayModeProperties2KHR',
-- 'Vulkan.Extensions.VK_KHR_display.getDisplayModePropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_display.getDisplayPlaneSupportedDisplaysKHR',
-- 'Vulkan.Extensions.VK_EXT_acquire_xlib_display.getRandROutputDisplayEXT',
-- 'Vulkan.Extensions.VK_NV_acquire_winrt_display.getWinrtDisplayNV',
-- 'Vulkan.Extensions.VK_EXT_display_control.registerDisplayEventEXT',
-- 'Vulkan.Extensions.VK_EXT_direct_mode_display.releaseDisplayEXT'
newtype DisplayKHR = DisplayKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DisplayKHR where
  objectTypeAndHandle (DisplayKHR h) = (OBJECT_TYPE_DISPLAY_KHR, h)
instance Show DisplayKHR where
  showsPrec p (DisplayKHR x) = showParen (p >= 11) (showString "DisplayKHR 0x" . showHex x)


-- | VkDisplayModeKHR - Opaque handle to a display mode object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_display.DisplayModePropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayPlaneInfo2KHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_display.createDisplayModeKHR',
-- 'Vulkan.Extensions.VK_KHR_display.getDisplayPlaneCapabilitiesKHR'
newtype DisplayModeKHR = DisplayModeKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DisplayModeKHR where
  objectTypeAndHandle (DisplayModeKHR h) = (OBJECT_TYPE_DISPLAY_MODE_KHR, h)
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
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_android_surface.createAndroidSurfaceKHR',
-- 'Vulkan.Extensions.VK_EXT_directfb_surface.createDirectFBSurfaceEXT',
-- 'Vulkan.Extensions.VK_KHR_display.createDisplayPlaneSurfaceKHR',
-- 'Vulkan.Extensions.VK_EXT_headless_surface.createHeadlessSurfaceEXT',
-- 'Vulkan.Extensions.VK_MVK_ios_surface.createIOSSurfaceMVK',
-- 'Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface.createImagePipeSurfaceFUCHSIA',
-- 'Vulkan.Extensions.VK_MVK_macos_surface.createMacOSSurfaceMVK',
-- 'Vulkan.Extensions.VK_EXT_metal_surface.createMetalSurfaceEXT',
-- 'Vulkan.Extensions.VK_QNX_screen_surface.createScreenSurfaceQNX',
-- 'Vulkan.Extensions.VK_GGP_stream_descriptor_surface.createStreamDescriptorSurfaceGGP',
-- 'Vulkan.Extensions.VK_NN_vi_surface.createViSurfaceNN',
-- 'Vulkan.Extensions.VK_KHR_wayland_surface.createWaylandSurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_win32_surface.createWin32SurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_xcb_surface.createXcbSurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_xlib_surface.createXlibSurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.destroySurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupSurfacePresentModesKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.getPhysicalDevicePresentRectanglesKHR',
-- 'Vulkan.Extensions.VK_EXT_display_surface_counter.getPhysicalDeviceSurfaceCapabilities2EXT',
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR'
newtype SurfaceKHR = SurfaceKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType SurfaceKHR where
  objectTypeAndHandle (SurfaceKHR h) = (OBJECT_TYPE_SURFACE_KHR, h)
instance Show SurfaceKHR where
  showsPrec p (SurfaceKHR x) = showParen (p >= 11) (showString "SurfaceKHR 0x" . showHex x)


-- | VkSwapchainKHR - Opaque handle to a swapchain object
--
-- = Description
--
-- A swapchain is an abstraction for an array of presentable images that
-- are associated with a surface. The presentable images are represented by
-- 'Vulkan.Core10.Handles.Image' objects created by the platform. One image
-- (which /can/ be an array image for multiview\/stereoscopic-3D surfaces)
-- is displayed at a time, but multiple images /can/ be queued for
-- presentation. An application renders to the image, and then queues the
-- image for presentation to the surface.
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
-- 'Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR', and before it
-- is released by 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR'.
-- This includes transitioning the image layout and rendering commands.
--
-- An application /can/ acquire use of a presentable image with
-- 'Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR'. After
-- acquiring a presentable image and before modifying it, the application
-- /must/ use a synchronization primitive to ensure that the presentation
-- engine has finished reading from the image. The application /can/ then
-- transition the image’s layout, queue rendering commands to it, etc.
-- Finally, the application presents the image with
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', which releases the
-- acquisition of the image.
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
-- 'Vulkan.Extensions.VK_KHR_swapchain.AcquireNextImageInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.BindImageMemorySwapchainInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.acquireFullScreenExclusiveModeEXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR',
-- 'Vulkan.Extensions.VK_KHR_display_swapchain.createSharedSwapchainsKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.destroySwapchainKHR',
-- 'Vulkan.Extensions.VK_GOOGLE_display_timing.getPastPresentationTimingGOOGLE',
-- 'Vulkan.Extensions.VK_GOOGLE_display_timing.getRefreshCycleDurationGOOGLE',
-- 'Vulkan.Extensions.VK_EXT_display_control.getSwapchainCounterEXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.getSwapchainImagesKHR',
-- 'Vulkan.Extensions.VK_KHR_shared_presentable_image.getSwapchainStatusKHR',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.releaseFullScreenExclusiveModeEXT',
-- 'Vulkan.Extensions.VK_EXT_hdr_metadata.setHdrMetadataEXT',
-- 'Vulkan.Extensions.VK_AMD_display_native_hdr.setLocalDimmingAMD'
newtype SwapchainKHR = SwapchainKHR Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType SwapchainKHR where
  objectTypeAndHandle (SwapchainKHR h) = (OBJECT_TYPE_SWAPCHAIN_KHR, h)
instance Show SwapchainKHR where
  showsPrec p (SwapchainKHR x) = showParen (p >= 11) (showString "SwapchainKHR 0x" . showHex x)


-- | VkDebugReportCallbackEXT - Opaque handle to a debug report callback
-- object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_EXT_debug_report.createDebugReportCallbackEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_report.destroyDebugReportCallbackEXT'
newtype DebugReportCallbackEXT = DebugReportCallbackEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DebugReportCallbackEXT where
  objectTypeAndHandle (DebugReportCallbackEXT h) = (OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT, h)
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
-- 'Vulkan.Extensions.VK_EXT_debug_utils.createDebugUtilsMessengerEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.destroyDebugUtilsMessengerEXT'
newtype DebugUtilsMessengerEXT = DebugUtilsMessengerEXT Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DebugUtilsMessengerEXT where
  objectTypeAndHandle (DebugUtilsMessengerEXT h) = (OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT, h)
instance Show DebugUtilsMessengerEXT where
  showsPrec p (DebugUtilsMessengerEXT x) = showParen (p >= 11) (showString "DebugUtilsMessengerEXT 0x" . showHex x)

