{-# language CPP #-}
module Vulkan.CStruct.Extends  ( BaseOutStructure(..)
                               , BaseInStructure(..)
                               , Extends
                               , PeekChain(..)
                               , PokeChain(..)
                               , Chain
                               , Extendss
                               , SomeStruct(..)
                               , extendSomeStruct
                               , withSomeStruct
                               , withSomeCStruct
                               , peekSomeCStruct
                               , pokeSomeCStruct
                               , forgetExtensions
                               , Extensible(..)
                               , pattern (::&)
                               , pattern (:&)
                               ) where

import Data.Maybe (fromMaybe)
import Type.Reflection (typeRep)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import GHC.Base (join)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (Ptr)
import GHC.TypeLits (ErrorMessage(..))
import GHC.TypeLits (TypeError)
import Data.Kind (Constraint)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AabbPositionsKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureBuildGeometryInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureBuildOffsetInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureCreateGeometryTypeInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (AccelerationStructureCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureDeviceAddressInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureGeometryAabbsDataKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureGeometryInstancesDataKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureGeometryKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureGeometryTrianglesDataKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (AccelerationStructureInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureInstanceKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureMemoryRequirementsInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (AccelerationStructureMemoryRequirementsInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (AccelerationStructureVersionKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (AcquireNextImageInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (AcquireProfilingLockInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (AndroidHardwareBufferFormatPropertiesANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (AndroidHardwareBufferPropertiesANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (AndroidHardwareBufferUsageANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_android_surface (AndroidSurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (ApplicationInfo)
import {-# SOURCE #-} Vulkan.Core10.Pass (AttachmentDescription)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (AttachmentDescription2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (AttachmentDescriptionStencilLayout)
import {-# SOURCE #-} Vulkan.Core10.Pass (AttachmentReference)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (AttachmentReference2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (AttachmentReferenceStencilLayout)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (AttachmentSampleLocationsEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (BindAccelerationStructureMemoryInfoKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindBufferMemoryDeviceGroupInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (BindBufferMemoryInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2 (BindImageMemoryDeviceGroupInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (BindImageMemoryInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (BindImageMemorySwapchainInfoKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (BindImagePlaneMemoryInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (BindIndexBufferIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (BindShaderGroupIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (BindSparseInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (BindVertexBufferIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (BlitImageInfo2KHR)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (BufferCopy)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (BufferCopy2KHR)
import {-# SOURCE #-} Vulkan.Core10.Buffer (BufferCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_buffer_device_address (BufferDeviceAddressCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferDeviceAddressInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (BufferImageCopy)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (BufferImageCopy2KHR)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (BufferMemoryBarrier)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (BufferMemoryRequirementsInfo2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferOpaqueCaptureAddressCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.BufferView (BufferViewCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_calibrated_timestamps (CalibratedTimestampInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints (CheckpointDataNV)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ClearAttachment)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ClearDepthStencilValue)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ClearRect)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (CoarseSampleLocationNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (CoarseSampleOrderCustomNV)
import {-# SOURCE #-} Vulkan.Core10.CommandBuffer (CommandBufferAllocateInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandBuffer (CommandBufferBeginInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (CommandBufferInheritanceConditionalRenderingInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.CommandBuffer (CommandBufferInheritanceInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_render_pass_transform (CommandBufferInheritanceRenderPassTransformInfoQCOM)
import {-# SOURCE #-} Vulkan.Core10.CommandPool (CommandPoolCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.ImageView (ComponentMapping)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (ComputePipelineCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (ConditionalRenderingBeginInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_driver_properties (ConformanceVersion)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (CooperativeMatrixPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (CopyAccelerationStructureInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (CopyAccelerationStructureToMemoryInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (CopyBufferInfo2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (CopyBufferToImageInfo2KHR)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (CopyDescriptorSet)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (CopyImageInfo2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (CopyImageToBufferInfo2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (CopyMemoryToAccelerationStructureInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (D3D12FenceSubmitInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_marker (DebugMarkerMarkerInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_marker (DebugMarkerObjectNameInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_marker (DebugMarkerObjectTagInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_report (DebugReportCallbackCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsLabelEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsMessengerCallbackDataEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsMessengerCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsObjectNameInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsObjectTagInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationBufferCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationImageCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation (DedicatedAllocationMemoryAllocateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_deferred_host_operations (DeferredOperationInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorBufferInfo)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorImageInfo)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorPoolCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (DescriptorPoolInlineUniformBlockCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorPoolSize)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorSetAllocateInfo)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorSetLayoutBinding)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetLayoutBindingFlagsCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorSetLayoutCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance3 (DescriptorSetLayoutSupport)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetVariableDescriptorCountAllocateInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (DescriptorSetVariableDescriptorCountLayoutSupport)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (DescriptorUpdateTemplateCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (DescriptorUpdateTemplateEntry)
import {-# SOURCE #-} Vulkan.Core10.Device (DeviceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (DeviceDeviceMemoryReportCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostics_config (DeviceDiagnosticsConfigCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (DeviceEventInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupBindSparseInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupCommandBufferBeginInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (DeviceGroupDeviceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentInfoKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupRenderPassBeginInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (DeviceGroupSubmitInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupSwapchainCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (DeviceMemoryOpaqueCaptureAddressInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_memory_overallocation_behavior (DeviceMemoryOverallocationCreateInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (DeviceMemoryReportCallbackDataEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_private_data (DevicePrivateDataCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Device (DeviceQueueCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_global_priority (DeviceQueueGlobalPriorityCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (DeviceQueueInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_directfb_surface (DirectFBSurfaceCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (DispatchIndirectCommand)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (DisplayEventInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayModeCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayModeParametersKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_display_properties2 (DisplayModeProperties2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayModePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_display_native_hdr (DisplayNativeHdrSurfaceCapabilitiesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_display_properties2 (DisplayPlaneCapabilities2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayPlaneCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_display_properties2 (DisplayPlaneInfo2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_display_properties2 (DisplayPlaneProperties2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayPlanePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (DisplayPowerInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display_swapchain (DisplayPresentInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_display_properties2 (DisplayProperties2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplaySurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (DrawIndexedIndirectCommand)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (DrawIndirectCommand)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_mesh_shader (DrawMeshTasksIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (DrmFormatModifierPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (DrmFormatModifierPropertiesListEXT)
import {-# SOURCE #-} Vulkan.Core10.Event (EventCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_fence (ExportFenceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_win32 (ExportFenceWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExportMemoryAllocateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory (ExportMemoryAllocateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (ExportMemoryWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_win32 (ExportMemoryWin32HandleInfoNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore (ExportSemaphoreCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (ExportSemaphoreWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.ExtensionDiscovery (ExtensionProperties)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Extent2D)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Extent3D)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalBufferProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities (ExternalFenceProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ExternalFormatANDROID)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalImageFormatProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalImageFormatPropertiesNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryBufferCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory (ExternalMemoryImageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory (ExternalMemoryImageCreateInfoNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalMemoryProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities (ExternalSemaphoreProperties)
import {-# SOURCE #-} Vulkan.Core10.Fence (FenceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_fd (FenceGetFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_win32 (FenceGetWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_filter_cubic (FilterCubicImageViewImageFormatPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (FormatProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (FormatProperties2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (FramebufferAttachmentImageInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (FramebufferAttachmentsCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.Pass (FramebufferCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (FramebufferMixedSamplesCombinationNV)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GeneratedCommandsInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GeneratedCommandsMemoryRequirementsInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (GeometryAABBNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (GeometryDataNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (GeometryNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (GeometryTrianglesNV)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (GraphicsPipelineCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GraphicsPipelineShaderGroupsCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GraphicsShaderGroupCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_hdr_metadata (HdrMetadataEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_headless_surface (HeadlessSurfaceCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_MVK_ios_surface (IOSSurfaceCreateInfoMVK)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageBlit)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (ImageBlit2KHR)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageCopy)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (ImageCopy2KHR)
import {-# SOURCE #-} Vulkan.Core10.Image (ImageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierExplicitCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierListCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_image_format_list (ImageFormatListCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (ImageFormatProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (ImageFormatProperties2)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (ImageMemoryBarrier)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (ImageMemoryRequirementsInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface (ImagePipeSurfaceCreateInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (ImagePlaneMemoryRequirementsInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageResolve)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (ImageResolve2KHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (ImageSparseMemoryRequirementsInfo2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage (ImageStencilUsageCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (ImageSubresource)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageSubresourceLayers)
import {-# SOURCE #-} Vulkan.Core10.ImageView (ImageSubresourceRange)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (ImageSwapchainCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_astc_decode_mode (ImageViewASTCDecodeModeEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_image_view_handle (ImageViewAddressPropertiesNVX)
import {-# SOURCE #-} Vulkan.Core10.ImageView (ImageViewCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_image_view_handle (ImageViewHandleInfoNVX)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (ImageViewUsageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (ImportAndroidHardwareBufferInfoANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_fd (ImportFenceFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_win32 (ImportFenceWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_fd (ImportMemoryFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_host (ImportMemoryHostPointerInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (ImportMemoryWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_win32 (ImportMemoryWin32HandleInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_fd (ImportSemaphoreFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (ImportSemaphoreWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (IndirectCommandsLayoutCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (IndirectCommandsLayoutTokenNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (IndirectCommandsStreamNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (InitializePerformanceApiInfoINTEL)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (InputAttachmentAspectReference)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (InstanceCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.LayerDiscovery (LayerProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_MVK_macos_surface (MacOSSurfaceCreateInfoMVK)
import {-# SOURCE #-} Vulkan.Core10.Memory (MappedMemoryRange)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group (MemoryAllocateFlagsInfo)
import {-# SOURCE #-} Vulkan.Core10.Memory (MemoryAllocateInfo)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (MemoryBarrier)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedAllocateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation (MemoryDedicatedRequirements)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_fd (MemoryFdPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (MemoryGetAndroidHardwareBufferInfoANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_fd (MemoryGetFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (MemoryGetWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (MemoryHeap)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_host (MemoryHostPointerPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (MemoryOpaqueCaptureAddressAllocateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_priority (MemoryPriorityAllocateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.MemoryManagement (MemoryRequirements)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (MemoryType)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (MemoryWin32HandlePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_surface (MetalSurfaceCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (MultisamplePropertiesEXT)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Offset2D)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Offset3D)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (PastPresentationTimingGOOGLE)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceConfigurationAcquireInfoINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PerformanceCounterDescriptionKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PerformanceCounterKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceMarkerInfoINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceOverrideInfoINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PerformanceQuerySubmitInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceStreamMarkerInfoINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceValueINTEL)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage (PhysicalDevice16BitStorageFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_4444_formats (PhysicalDevice4444FormatsFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage (PhysicalDevice8BitStorageFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_astc_decode_mode (PhysicalDeviceASTCDecodeFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PhysicalDeviceBlendOperationAdvancedPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_device_coherent_memory (PhysicalDeviceCoherentMemoryFeaturesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_compute_shader_derivatives (PhysicalDeviceComputeShaderDerivativesFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (PhysicalDeviceConditionalRenderingFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conservative_rasterization (PhysicalDeviceConservativeRasterizationPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (PhysicalDeviceCooperativeMatrixPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_corner_sampled_image (PhysicalDeviceCornerSampledImageFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (PhysicalDeviceCoverageReductionModeFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (PhysicalDeviceCustomBorderColorFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (PhysicalDeviceCustomBorderColorPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing (PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_enable (PhysicalDeviceDepthClipEnableFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (PhysicalDeviceDepthStencilResolveProperties)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing (PhysicalDeviceDescriptorIndexingProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (PhysicalDeviceDeviceGeneratedCommandsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (PhysicalDeviceDeviceGeneratedCommandsPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_memory_report (PhysicalDeviceDeviceMemoryReportFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostics_config (PhysicalDeviceDiagnosticsConfigFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_discard_rectangles (PhysicalDeviceDiscardRectanglePropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_driver_properties (PhysicalDeviceDriverProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_scissor_exclusive (PhysicalDeviceExclusiveScissorFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state (PhysicalDeviceExtendedDynamicStateFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceExternalBufferInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities (PhysicalDeviceExternalFenceInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceExternalImageFormatInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_host (PhysicalDeviceExternalMemoryHostPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities (PhysicalDeviceExternalSemaphoreInfo)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceFeatures2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls (PhysicalDeviceFloatControlsProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map2 (PhysicalDeviceFragmentDensityMap2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map2 (PhysicalDeviceFragmentDensityMap2PropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (PhysicalDeviceFragmentDensityMapPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shader_barycentric (PhysicalDeviceFragmentShaderBarycentricFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_shader_interlock (PhysicalDeviceFragmentShaderInterlockFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (PhysicalDeviceGroupProperties)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset (PhysicalDeviceHostQueryResetFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceIDProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (PhysicalDeviceImageDrmFormatModifierInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceImageFormatInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_robustness (PhysicalDeviceImageRobustnessFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_filter_cubic (PhysicalDeviceImageViewImageFormatInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (PhysicalDeviceImagelessFramebufferFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_index_type_uint8 (PhysicalDeviceIndexTypeUint8FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (PhysicalDeviceInlineUniformBlockPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceLimits)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PhysicalDeviceLineRasterizationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PhysicalDeviceLineRasterizationPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance3 (PhysicalDeviceMaintenance3Properties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_budget (PhysicalDeviceMemoryBudgetPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_memory_priority (PhysicalDeviceMemoryPriorityFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceMemoryProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceMemoryProperties2)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_mesh_shader (PhysicalDeviceMeshShaderFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_mesh_shader (PhysicalDeviceMeshShaderPropertiesNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_multiview_per_view_attributes (PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (PhysicalDeviceMultiviewProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pci_bus_info (PhysicalDevicePCIBusInfoPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PhysicalDevicePerformanceQueryFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PhysicalDevicePerformanceQueryPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control (PhysicalDevicePipelineCreationCacheControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PhysicalDevicePipelineExecutablePropertiesFeaturesKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (PhysicalDevicePointClippingProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_portability_subset (PhysicalDevicePortabilitySubsetFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_portability_subset (PhysicalDevicePortabilitySubsetPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_private_data (PhysicalDevicePrivateDataFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceProperties2)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (PhysicalDeviceProtectedMemoryFeatures)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (PhysicalDeviceProtectedMemoryProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_push_descriptor (PhysicalDevicePushDescriptorPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (PhysicalDeviceRayTracingFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (PhysicalDeviceRayTracingPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (PhysicalDeviceRayTracingPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_representative_fragment_test (PhysicalDeviceRepresentativeFragmentTestFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_robustness2 (PhysicalDeviceRobustness2FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_robustness2 (PhysicalDeviceRobustness2PropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (PhysicalDeviceSampleLocationsPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax (PhysicalDeviceSamplerFilterMinmaxProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (PhysicalDeviceSamplerYcbcrConversionFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout (PhysicalDeviceScalarBlockLayoutFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts (PhysicalDeviceSeparateDepthStencilLayoutsFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_atomic_float (PhysicalDeviceShaderAtomicFloatFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64 (PhysicalDeviceShaderAtomicInt64Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shader_clock (PhysicalDeviceShaderClockFeaturesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_core_properties2 (PhysicalDeviceShaderCoreProperties2AMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_core_properties (PhysicalDeviceShaderCorePropertiesAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation (PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters (PhysicalDeviceShaderDrawParametersFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8 (PhysicalDeviceShaderFloat16Int8Features)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_image_atomic_int64 (PhysicalDeviceShaderImageAtomicInt64FeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_image_footprint (PhysicalDeviceShaderImageFootprintFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_shader_integer_functions2 (PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shader_sm_builtins (PhysicalDeviceShaderSMBuiltinsPropertiesNV)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types (PhysicalDeviceShaderSubgroupExtendedTypesFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImageFeaturesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PhysicalDeviceShadingRateImagePropertiesNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceSparseImageFormatInfo2)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceSparseProperties)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup (PhysicalDeviceSubgroupProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (PhysicalDeviceSurfaceInfo2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_texel_buffer_alignment (PhysicalDeviceTexelBufferAlignmentFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_texel_buffer_alignment (PhysicalDeviceTexelBufferAlignmentPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr (PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreFeatures)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (PhysicalDeviceTimelineSemaphoreProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_tooling_info (PhysicalDeviceToolPropertiesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PhysicalDeviceTransformFeedbackPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout (PhysicalDeviceUniformBufferStandardLayoutFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers (PhysicalDeviceVariablePointersFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PhysicalDeviceVertexAttributeDivisorPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan11Features)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan11Properties)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan12Features)
import {-# SOURCE #-} Vulkan.Core12 (PhysicalDeviceVulkan12Properties)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model (PhysicalDeviceVulkanMemoryModelFeatures)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_ycbcr_image_arrays (PhysicalDeviceYcbcrImageArraysFeaturesEXT)
import {-# SOURCE #-} Vulkan.Core10.PipelineCache (PipelineCacheCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_blend_operation_advanced (PipelineColorBlendAdvancedStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineColorBlendAttachmentState)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineColorBlendStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_pipeline_compiler_control (PipelineCompilerControlCreateInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_framebuffer_mixed_samples (PipelineCoverageModulationStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (PipelineCoverageReductionStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_coverage_to_color (PipelineCoverageToColorStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineDepthStencilStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_discard_rectangles (PipelineDiscardRectangleStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineDynamicStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutableInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutableInternalRepresentationKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutablePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutableStatisticKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineInputAssemblyStateCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.PipelineLayout (PipelineLayoutCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineMultisampleStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conservative_rasterization (PipelineRasterizationConservativeStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_depth_clip_enable (PipelineRasterizationDepthClipStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (PipelineRasterizationLineStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineRasterizationStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_rasterization_order (PipelineRasterizationStateRasterizationOrderAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_transform_feedback (PipelineRasterizationStateStreamCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_representative_fragment_test (PipelineRepresentativeFragmentTestStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (PipelineSampleLocationsStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_subgroup_size_control (PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (PipelineTessellationDomainOriginStateCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineTessellationStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (PipelineVertexInputDivisorStateCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineVertexInputStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PipelineViewportCoarseSampleOrderStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_scissor_exclusive (PipelineViewportExclusiveScissorStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (PipelineViewportShadingRateImageStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (PipelineViewportStateCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_viewport_swizzle (PipelineViewportSwizzleStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_clip_space_w_scaling (PipelineViewportWScalingStateCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_GGP_frame_token (PresentFrameTokenGGP)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (PresentInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_incremental_present (PresentRegionKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_incremental_present (PresentRegionsKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (PresentTimeGOOGLE)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (PresentTimesInfoGOOGLE)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_private_data (PrivateDataSlotCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (ProtectedSubmitInfo)
import {-# SOURCE #-} Vulkan.Core10.PipelineLayout (PushConstantRange)
import {-# SOURCE #-} Vulkan.Core10.Query (QueryPoolCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (QueryPoolPerformanceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (QueryPoolPerformanceQueryCreateInfoINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints (QueueFamilyCheckpointPropertiesNV)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (QueueFamilyProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (QueueFamilyProperties2)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (RayTracingPipelineCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (RayTracingPipelineCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (RayTracingPipelineInterfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (RayTracingShaderGroupCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (RayTracingShaderGroupCreateInfoNV)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Rect2D)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_incremental_present (RectLayerKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (RefreshCycleDurationGOOGLE)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer (RenderPassAttachmentBeginInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (RenderPassBeginInfo)
import {-# SOURCE #-} Vulkan.Core10.Pass (RenderPassCreateInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (RenderPassCreateInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_fragment_density_map (RenderPassFragmentDensityMapCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance2 (RenderPassInputAttachmentAspectCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_multiview (RenderPassMultiviewCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (RenderPassSampleLocationsBeginInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_render_pass_transform (RenderPassTransformBeginInfoQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_copy_commands2 (ResolveImageInfo2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SampleLocationEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SampleLocationsInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Sampler (SamplerCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_custom_border_color (SamplerCustomBorderColorCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax (SamplerReductionModeCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionImageFormatProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionInfo)
import {-# SOURCE #-} Vulkan.Core10.QueueSemaphore (SemaphoreCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_fd (SemaphoreGetFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (SemaphoreGetWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreSignalInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreTypeCreateInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreWaitInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (SetStateFlagsIndirectCommandNV)
import {-# SOURCE #-} Vulkan.Core10.Shader (ShaderModuleCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_cache (ShaderModuleValidationCacheCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_info (ShaderResourceUsageAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_info (ShaderStatisticsInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (ShadingRatePaletteNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_shared_presentable_image (SharedPresentSurfaceCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (SparseBufferMemoryBindInfo)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (SparseImageFormatProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (SparseImageFormatProperties2)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (SparseImageMemoryBind)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (SparseImageMemoryBindInfo)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (SparseImageMemoryRequirements)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (SparseImageMemoryRequirements2)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (SparseImageOpaqueMemoryBindInfo)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (SparseMemoryBind)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (SpecializationInfo)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (SpecializationMapEntry)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (StencilOpState)
import {-# SOURCE #-} Vulkan.Extensions.VK_GGP_stream_descriptor_surface (StreamDescriptorSurfaceCreateInfoGGP)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (StridedBufferRegionKHR)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
import {-# SOURCE #-} Vulkan.Core10.Queue (SubmitInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassBeginInfo)
import {-# SOURCE #-} Vulkan.Core10.Pass (SubpassDependency)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassDependency2)
import {-# SOURCE #-} Vulkan.Core10.Pass (SubpassDescription)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassDescription2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve (SubpassDescriptionDepthStencilResolve)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassEndInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SubpassSampleLocationsEXT)
import {-# SOURCE #-} Vulkan.Core10.Image (SubresourceLayout)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCapabilities2EXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (SurfaceCapabilities2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceCapabilitiesFullScreenExclusiveEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface (SurfaceCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (SurfaceFormat2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface (SurfaceFormatKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_full_screen_exclusive (SurfaceFullScreenExclusiveWin32InfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface_protected_capabilities (SurfaceProtectedCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (SwapchainCounterCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_display_native_hdr (SwapchainDisplayNativeHdrCreateInfoAMD)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_texture_gather_bias_lod (TextureLODGatherFormatPropertiesAMD)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (TimelineSemaphoreSubmitInfo)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (TraceRaysIndirectCommandKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (TransformMatrixKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_cache (ValidationCacheCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_features (ValidationFeaturesEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_flags (ValidationFlagsEXT)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (VertexInputAttributeDescription)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (VertexInputBindingDescription)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_attribute_divisor (VertexInputBindingDivisorDescriptionEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NN_vi_surface (ViSurfaceCreateInfoNN)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (Viewport)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_viewport_swizzle (ViewportSwizzleNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_clip_space_w_scaling (ViewportWScalingNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_wayland_surface (WaylandSurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_win32_keyed_mutex (Win32KeyedMutexAcquireReleaseInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_win32_keyed_mutex (Win32KeyedMutexAcquireReleaseInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_win32_surface (Win32SurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (WriteDescriptorSet)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing (WriteDescriptorSetAccelerationStructureKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_inline_uniform_block (WriteDescriptorSetInlineUniformBlockEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_hdr_metadata (XYColorEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_xcb_surface (XcbSurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_xlib_surface (XlibSurfaceCreateInfoKHR)
import Vulkan.Zero (Zero(..))
-- | VkBaseOutStructure - Base structure for a read-only pointer chain
--
-- = Description
--
-- 'BaseOutStructure' can be used to facilitate iterating through a
-- structure pointer chain that returns data back to the application.
--
-- = See Also
--
-- 'BaseOutStructure', 'Vulkan.Core10.Enums.StructureType.StructureType'
data BaseOutStructure = BaseOutStructure
  { -- | @sType@ is the structure type of the structure being iterated through.
    sType :: StructureType
  , -- | @pNext@ is @NULL@ or a pointer to the next structure in a structure
    -- chain.
    next :: Ptr BaseOutStructure
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BaseOutStructure)
#endif
deriving instance Show BaseOutStructure

instance ToCStruct BaseOutStructure where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BaseOutStructure{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (sType)
    poke ((p `plusPtr` 8 :: Ptr (Ptr BaseOutStructure))) (next)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr BaseOutStructure))) (zero)
    f

instance FromCStruct BaseOutStructure where
  peekCStruct p = do
    sType <- peek @StructureType ((p `plusPtr` 0 :: Ptr StructureType))
    pNext <- peek @(Ptr BaseOutStructure) ((p `plusPtr` 8 :: Ptr (Ptr BaseOutStructure)))
    pure $ BaseOutStructure
             sType pNext

instance Storable BaseOutStructure where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BaseOutStructure where
  zero = BaseOutStructure
           zero
           zero


-- | VkBaseInStructure - Base structure for a read-only pointer chain
--
-- = Description
--
-- 'BaseInStructure' can be used to facilitate iterating through a
-- read-only structure pointer chain.
--
-- = See Also
--
-- 'BaseInStructure', 'Vulkan.Core10.Enums.StructureType.StructureType'
data BaseInStructure = BaseInStructure
  { -- | @sType@ is the structure type of the structure being iterated through.
    sType :: StructureType
  , -- | @pNext@ is @NULL@ or a pointer to the next structure in a structure
    -- chain.
    next :: Ptr BaseInStructure
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BaseInStructure)
#endif
deriving instance Show BaseInStructure

instance ToCStruct BaseInStructure where
  withCStruct x f = allocaBytesAligned 16 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BaseInStructure{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (sType)
    poke ((p `plusPtr` 8 :: Ptr (Ptr BaseInStructure))) (next)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (zero)
    poke ((p `plusPtr` 8 :: Ptr (Ptr BaseInStructure))) (zero)
    f

instance FromCStruct BaseInStructure where
  peekCStruct p = do
    sType <- peek @StructureType ((p `plusPtr` 0 :: Ptr StructureType))
    pNext <- peek @(Ptr BaseInStructure) ((p `plusPtr` 8 :: Ptr (Ptr BaseInStructure)))
    pure $ BaseInStructure
             sType pNext

instance Storable BaseInStructure where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BaseInStructure where
  zero = BaseInStructure
           zero
           zero


type family Extends (a :: [Type] -> Type) (b :: Type) :: Constraint where
  Extends AccelerationStructureBuildGeometryInfoKHR DeferredOperationInfoKHR = ()
  Extends AndroidHardwareBufferPropertiesANDROID AndroidHardwareBufferFormatPropertiesANDROID = ()
  Extends AttachmentDescription2 AttachmentDescriptionStencilLayout = ()
  Extends AttachmentReference2 AttachmentReferenceStencilLayout = ()
  Extends BindBufferMemoryInfo BindBufferMemoryDeviceGroupInfo = ()
  Extends BindImageMemoryInfo BindImageMemoryDeviceGroupInfo = ()
  Extends BindImageMemoryInfo BindImageMemorySwapchainInfoKHR = ()
  Extends BindImageMemoryInfo BindImagePlaneMemoryInfo = ()
  Extends BindSparseInfo DeviceGroupBindSparseInfo = ()
  Extends BindSparseInfo TimelineSemaphoreSubmitInfo = ()
  Extends BufferCreateInfo DedicatedAllocationBufferCreateInfoNV = ()
  Extends BufferCreateInfo ExternalMemoryBufferCreateInfo = ()
  Extends BufferCreateInfo BufferOpaqueCaptureAddressCreateInfo = ()
  Extends BufferCreateInfo BufferDeviceAddressCreateInfoEXT = ()
  Extends CommandBufferBeginInfo DeviceGroupCommandBufferBeginInfo = ()
  Extends CommandBufferInheritanceInfo CommandBufferInheritanceConditionalRenderingInfoEXT = ()
  Extends CommandBufferInheritanceInfo CommandBufferInheritanceRenderPassTransformInfoQCOM = ()
  Extends ComputePipelineCreateInfo PipelineCreationFeedbackCreateInfoEXT = ()
  Extends ComputePipelineCreateInfo PipelineCompilerControlCreateInfoAMD = ()
  Extends CopyAccelerationStructureInfoKHR DeferredOperationInfoKHR = ()
  Extends CopyAccelerationStructureToMemoryInfoKHR DeferredOperationInfoKHR = ()
  Extends CopyMemoryToAccelerationStructureInfoKHR DeferredOperationInfoKHR = ()
  Extends DescriptorPoolCreateInfo DescriptorPoolInlineUniformBlockCreateInfoEXT = ()
  Extends DescriptorSetAllocateInfo DescriptorSetVariableDescriptorCountAllocateInfo = ()
  Extends DescriptorSetLayoutCreateInfo DescriptorSetLayoutBindingFlagsCreateInfo = ()
  Extends DescriptorSetLayoutSupport DescriptorSetVariableDescriptorCountLayoutSupport = ()
  Extends DeviceCreateInfo PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = ()
  Extends DeviceCreateInfo DevicePrivateDataCreateInfoEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePrivateDataFeaturesEXT = ()
  Extends DeviceCreateInfo (PhysicalDeviceFeatures2 '[]) = ()
  Extends DeviceCreateInfo PhysicalDeviceVariablePointersFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceMultiviewFeatures = ()
  Extends DeviceCreateInfo DeviceGroupDeviceCreateInfo = ()
  Extends DeviceCreateInfo PhysicalDevice16BitStorageFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderSubgroupExtendedTypesFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceSamplerYcbcrConversionFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceProtectedMemoryFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceBlendOperationAdvancedFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceInlineUniformBlockFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderDrawParametersFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderFloat16Int8Features = ()
  Extends DeviceCreateInfo PhysicalDeviceHostQueryResetFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceDeviceMemoryReportFeaturesEXT = ()
  Extends DeviceCreateInfo DeviceDeviceMemoryReportCreateInfoEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDescriptorIndexingFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceTimelineSemaphoreFeatures = ()
  Extends DeviceCreateInfo PhysicalDevice8BitStorageFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceConditionalRenderingFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceVulkanMemoryModelFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderAtomicInt64Features = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderAtomicFloatFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceVertexAttributeDivisorFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceASTCDecodeFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceTransformFeedbackFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceRepresentativeFragmentTestFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceExclusiveScissorFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceCornerSampledImageFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceComputeShaderDerivativesFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentShaderBarycentricFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderImageFootprintFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceShadingRateImageFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceMeshShaderFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceRayTracingFeaturesKHR = ()
  Extends DeviceCreateInfo DeviceMemoryOverallocationCreateInfoAMD = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentDensityMapFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentDensityMap2FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceScalarBlockLayoutFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceUniformBufferStandardLayoutFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceDepthClipEnableFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceMemoryPriorityFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceBufferDeviceAddressFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceBufferDeviceAddressFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceImagelessFramebufferFeatures = ()
  Extends DeviceCreateInfo PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceCooperativeMatrixFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceYcbcrImageArraysFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePerformanceQueryFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceCoverageReductionModeFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderClockFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceIndexTypeUint8FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderSMBuiltinsFeaturesNV = ()
  Extends DeviceCreateInfo PhysicalDeviceFragmentShaderInterlockFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceSeparateDepthStencilLayoutsFeatures = ()
  Extends DeviceCreateInfo PhysicalDevicePipelineExecutablePropertiesFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceTexelBufferAlignmentFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceSubgroupSizeControlFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceLineRasterizationFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePipelineCreationCacheControlFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceVulkan11Features = ()
  Extends DeviceCreateInfo PhysicalDeviceVulkan12Features = ()
  Extends DeviceCreateInfo PhysicalDeviceCoherentMemoryFeaturesAMD = ()
  Extends DeviceCreateInfo PhysicalDeviceCustomBorderColorFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceExtendedDynamicStateFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceDiagnosticsConfigFeaturesNV = ()
  Extends DeviceCreateInfo DeviceDiagnosticsConfigCreateInfoNV = ()
  Extends DeviceCreateInfo PhysicalDeviceRobustness2FeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceImageRobustnessFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDevicePortabilitySubsetFeaturesKHR = ()
  Extends DeviceCreateInfo PhysicalDevice4444FormatsFeaturesEXT = ()
  Extends DeviceCreateInfo PhysicalDeviceShaderImageAtomicInt64FeaturesEXT = ()
  Extends DeviceQueueCreateInfo DeviceQueueGlobalPriorityCreateInfoEXT = ()
  Extends FenceCreateInfo ExportFenceCreateInfo = ()
  Extends FenceCreateInfo ExportFenceWin32HandleInfoKHR = ()
  Extends FormatProperties2 DrmFormatModifierPropertiesListEXT = ()
  Extends FramebufferCreateInfo FramebufferAttachmentsCreateInfo = ()
  Extends GraphicsPipelineCreateInfo GraphicsPipelineShaderGroupsCreateInfoNV = ()
  Extends GraphicsPipelineCreateInfo PipelineDiscardRectangleStateCreateInfoEXT = ()
  Extends GraphicsPipelineCreateInfo PipelineRepresentativeFragmentTestStateCreateInfoNV = ()
  Extends GraphicsPipelineCreateInfo PipelineCreationFeedbackCreateInfoEXT = ()
  Extends GraphicsPipelineCreateInfo PipelineCompilerControlCreateInfoAMD = ()
  Extends ImageCreateInfo DedicatedAllocationImageCreateInfoNV = ()
  Extends ImageCreateInfo ExternalMemoryImageCreateInfoNV = ()
  Extends ImageCreateInfo ExternalMemoryImageCreateInfo = ()
  Extends ImageCreateInfo ImageSwapchainCreateInfoKHR = ()
  Extends ImageCreateInfo ImageFormatListCreateInfo = ()
  Extends ImageCreateInfo ExternalFormatANDROID = ()
  Extends ImageCreateInfo ImageDrmFormatModifierListCreateInfoEXT = ()
  Extends ImageCreateInfo ImageDrmFormatModifierExplicitCreateInfoEXT = ()
  Extends ImageCreateInfo ImageStencilUsageCreateInfo = ()
  Extends ImageFormatProperties2 ExternalImageFormatProperties = ()
  Extends ImageFormatProperties2 SamplerYcbcrConversionImageFormatProperties = ()
  Extends ImageFormatProperties2 TextureLODGatherFormatPropertiesAMD = ()
  Extends ImageFormatProperties2 AndroidHardwareBufferUsageANDROID = ()
  Extends ImageFormatProperties2 FilterCubicImageViewImageFormatPropertiesEXT = ()
  Extends ImageMemoryBarrier SampleLocationsInfoEXT = ()
  Extends ImageMemoryRequirementsInfo2 ImagePlaneMemoryRequirementsInfo = ()
  Extends ImageViewCreateInfo ImageViewUsageCreateInfo = ()
  Extends ImageViewCreateInfo SamplerYcbcrConversionInfo = ()
  Extends ImageViewCreateInfo ImageViewASTCDecodeModeEXT = ()
  Extends InstanceCreateInfo DebugReportCallbackCreateInfoEXT = ()
  Extends InstanceCreateInfo ValidationFlagsEXT = ()
  Extends InstanceCreateInfo ValidationFeaturesEXT = ()
  Extends InstanceCreateInfo DebugUtilsMessengerCreateInfoEXT = ()
  Extends MemoryAllocateInfo DedicatedAllocationMemoryAllocateInfoNV = ()
  Extends MemoryAllocateInfo ExportMemoryAllocateInfoNV = ()
  Extends MemoryAllocateInfo ImportMemoryWin32HandleInfoNV = ()
  Extends MemoryAllocateInfo ExportMemoryWin32HandleInfoNV = ()
  Extends MemoryAllocateInfo ExportMemoryAllocateInfo = ()
  Extends MemoryAllocateInfo ImportMemoryWin32HandleInfoKHR = ()
  Extends MemoryAllocateInfo ExportMemoryWin32HandleInfoKHR = ()
  Extends MemoryAllocateInfo ImportMemoryFdInfoKHR = ()
  Extends MemoryAllocateInfo MemoryAllocateFlagsInfo = ()
  Extends MemoryAllocateInfo MemoryDedicatedAllocateInfo = ()
  Extends MemoryAllocateInfo ImportMemoryHostPointerInfoEXT = ()
  Extends MemoryAllocateInfo ImportAndroidHardwareBufferInfoANDROID = ()
  Extends MemoryAllocateInfo MemoryPriorityAllocateInfoEXT = ()
  Extends MemoryAllocateInfo MemoryOpaqueCaptureAddressAllocateInfo = ()
  Extends MemoryRequirements2 MemoryDedicatedRequirements = ()
  Extends PhysicalDeviceExternalSemaphoreInfo SemaphoreTypeCreateInfo = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDeviceGeneratedCommandsFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePrivateDataFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVariablePointersFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMultiviewFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevice16BitStorageFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderSubgroupExtendedTypesFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSamplerYcbcrConversionFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceProtectedMemoryFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceBlendOperationAdvancedFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceInlineUniformBlockFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderDrawParametersFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderFloat16Int8Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceHostQueryResetFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDeviceMemoryReportFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDescriptorIndexingFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceTimelineSemaphoreFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevice8BitStorageFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceConditionalRenderingFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVulkanMemoryModelFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderAtomicInt64Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderAtomicFloatFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVertexAttributeDivisorFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceASTCDecodeFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceTransformFeedbackFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRepresentativeFragmentTestFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceExclusiveScissorFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCornerSampledImageFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceComputeShaderDerivativesFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentShaderBarycentricFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderImageFootprintFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShadingRateImageFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMeshShaderFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRayTracingFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentDensityMapFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentDensityMap2FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceScalarBlockLayoutFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceUniformBufferStandardLayoutFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDepthClipEnableFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceMemoryPriorityFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceBufferDeviceAddressFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceBufferDeviceAddressFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImagelessFramebufferFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCooperativeMatrixFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceYcbcrImageArraysFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePerformanceQueryFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCoverageReductionModeFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderClockFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceIndexTypeUint8FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderSMBuiltinsFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceFragmentShaderInterlockFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSeparateDepthStencilLayoutsFeatures = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePipelineExecutablePropertiesFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceTexelBufferAlignmentFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceSubgroupSizeControlFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceLineRasterizationFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePipelineCreationCacheControlFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVulkan11Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceVulkan12Features = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCoherentMemoryFeaturesAMD = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceCustomBorderColorFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceExtendedDynamicStateFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceDiagnosticsConfigFeaturesNV = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceRobustness2FeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceImageRobustnessFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevicePortabilitySubsetFeaturesKHR = ()
  Extends PhysicalDeviceFeatures2 PhysicalDevice4444FormatsFeaturesEXT = ()
  Extends PhysicalDeviceFeatures2 PhysicalDeviceShaderImageAtomicInt64FeaturesEXT = ()
  Extends PhysicalDeviceImageFormatInfo2 PhysicalDeviceExternalImageFormatInfo = ()
  Extends PhysicalDeviceImageFormatInfo2 ImageFormatListCreateInfo = ()
  Extends PhysicalDeviceImageFormatInfo2 PhysicalDeviceImageDrmFormatModifierInfoEXT = ()
  Extends PhysicalDeviceImageFormatInfo2 ImageStencilUsageCreateInfo = ()
  Extends PhysicalDeviceImageFormatInfo2 PhysicalDeviceImageViewImageFormatInfoEXT = ()
  Extends PhysicalDeviceMemoryProperties2 PhysicalDeviceMemoryBudgetPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceDeviceGeneratedCommandsPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDevicePushDescriptorPropertiesKHR = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceDriverProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceIDProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMultiviewProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceDiscardRectanglePropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceSubgroupProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDevicePointClippingProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceProtectedMemoryProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceSamplerFilterMinmaxProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceSampleLocationsPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceBlendOperationAdvancedPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceInlineUniformBlockPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMaintenance3Properties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceFloatControlsProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceExternalMemoryHostPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceConservativeRasterizationPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderCorePropertiesAMD = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderCoreProperties2AMD = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceDescriptorIndexingProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceTimelineSemaphoreProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceVertexAttributeDivisorPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDevicePCIBusInfoPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceDepthStencilResolveProperties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceTransformFeedbackPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShadingRateImagePropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceMeshShaderPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceRayTracingPropertiesKHR = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceRayTracingPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceFragmentDensityMapPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceFragmentDensityMap2PropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceCooperativeMatrixPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDevicePerformanceQueryPropertiesKHR = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceShaderSMBuiltinsPropertiesNV = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceTexelBufferAlignmentPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceSubgroupSizeControlPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceLineRasterizationPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceVulkan11Properties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceVulkan12Properties = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceCustomBorderColorPropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDeviceRobustness2PropertiesEXT = ()
  Extends PhysicalDeviceProperties2 PhysicalDevicePortabilitySubsetPropertiesKHR = ()
  Extends PhysicalDeviceSurfaceInfo2KHR SurfaceFullScreenExclusiveInfoEXT = ()
  Extends PhysicalDeviceSurfaceInfo2KHR SurfaceFullScreenExclusiveWin32InfoEXT = ()
  Extends PipelineColorBlendStateCreateInfo PipelineColorBlendAdvancedStateCreateInfoEXT = ()
  Extends PipelineMultisampleStateCreateInfo PipelineCoverageToColorStateCreateInfoNV = ()
  Extends PipelineMultisampleStateCreateInfo PipelineSampleLocationsStateCreateInfoEXT = ()
  Extends PipelineMultisampleStateCreateInfo PipelineCoverageModulationStateCreateInfoNV = ()
  Extends PipelineMultisampleStateCreateInfo PipelineCoverageReductionStateCreateInfoNV = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationStateRasterizationOrderAMD = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationConservativeStateCreateInfoEXT = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationStateStreamCreateInfoEXT = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationDepthClipStateCreateInfoEXT = ()
  Extends PipelineRasterizationStateCreateInfo PipelineRasterizationLineStateCreateInfoEXT = ()
  Extends PipelineShaderStageCreateInfo PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT = ()
  Extends PipelineTessellationStateCreateInfo PipelineTessellationDomainOriginStateCreateInfo = ()
  Extends PipelineVertexInputStateCreateInfo PipelineVertexInputDivisorStateCreateInfoEXT = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportWScalingStateCreateInfoNV = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportSwizzleStateCreateInfoNV = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportExclusiveScissorStateCreateInfoNV = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportShadingRateImageStateCreateInfoNV = ()
  Extends PipelineViewportStateCreateInfo PipelineViewportCoarseSampleOrderStateCreateInfoNV = ()
  Extends PresentInfoKHR DisplayPresentInfoKHR = ()
  Extends PresentInfoKHR PresentRegionsKHR = ()
  Extends PresentInfoKHR DeviceGroupPresentInfoKHR = ()
  Extends PresentInfoKHR PresentTimesInfoGOOGLE = ()
  Extends PresentInfoKHR PresentFrameTokenGGP = ()
  Extends QueryPoolCreateInfo QueryPoolPerformanceCreateInfoKHR = ()
  Extends QueryPoolCreateInfo QueryPoolPerformanceQueryCreateInfoINTEL = ()
  Extends QueueFamilyProperties2 QueueFamilyCheckpointPropertiesNV = ()
  Extends RayTracingPipelineCreateInfoKHR PipelineCreationFeedbackCreateInfoEXT = ()
  Extends RayTracingPipelineCreateInfoKHR DeferredOperationInfoKHR = ()
  Extends RayTracingPipelineCreateInfoNV PipelineCreationFeedbackCreateInfoEXT = ()
  Extends RenderPassBeginInfo DeviceGroupRenderPassBeginInfo = ()
  Extends RenderPassBeginInfo RenderPassSampleLocationsBeginInfoEXT = ()
  Extends RenderPassBeginInfo RenderPassAttachmentBeginInfo = ()
  Extends RenderPassBeginInfo RenderPassTransformBeginInfoQCOM = ()
  Extends RenderPassCreateInfo RenderPassMultiviewCreateInfo = ()
  Extends RenderPassCreateInfo RenderPassInputAttachmentAspectCreateInfo = ()
  Extends RenderPassCreateInfo RenderPassFragmentDensityMapCreateInfoEXT = ()
  Extends RenderPassCreateInfo2 RenderPassFragmentDensityMapCreateInfoEXT = ()
  Extends SamplerCreateInfo SamplerYcbcrConversionInfo = ()
  Extends SamplerCreateInfo SamplerReductionModeCreateInfo = ()
  Extends SamplerCreateInfo SamplerCustomBorderColorCreateInfoEXT = ()
  Extends SamplerYcbcrConversionCreateInfo ExternalFormatANDROID = ()
  Extends SemaphoreCreateInfo ExportSemaphoreCreateInfo = ()
  Extends SemaphoreCreateInfo ExportSemaphoreWin32HandleInfoKHR = ()
  Extends SemaphoreCreateInfo SemaphoreTypeCreateInfo = ()
  Extends ShaderModuleCreateInfo ShaderModuleValidationCacheCreateInfoEXT = ()
  Extends SubmitInfo Win32KeyedMutexAcquireReleaseInfoNV = ()
  Extends SubmitInfo Win32KeyedMutexAcquireReleaseInfoKHR = ()
  Extends SubmitInfo D3D12FenceSubmitInfoKHR = ()
  Extends SubmitInfo DeviceGroupSubmitInfo = ()
  Extends SubmitInfo ProtectedSubmitInfo = ()
  Extends SubmitInfo TimelineSemaphoreSubmitInfo = ()
  Extends SubmitInfo PerformanceQuerySubmitInfoKHR = ()
  Extends SubpassDescription2 SubpassDescriptionDepthStencilResolve = ()
  Extends SurfaceCapabilities2KHR DisplayNativeHdrSurfaceCapabilitiesAMD = ()
  Extends SurfaceCapabilities2KHR SharedPresentSurfaceCapabilitiesKHR = ()
  Extends SurfaceCapabilities2KHR SurfaceProtectedCapabilitiesKHR = ()
  Extends SurfaceCapabilities2KHR SurfaceCapabilitiesFullScreenExclusiveEXT = ()
  Extends SwapchainCreateInfoKHR SwapchainCounterCreateInfoEXT = ()
  Extends SwapchainCreateInfoKHR DeviceGroupSwapchainCreateInfoKHR = ()
  Extends SwapchainCreateInfoKHR SwapchainDisplayNativeHdrCreateInfoAMD = ()
  Extends SwapchainCreateInfoKHR ImageFormatListCreateInfo = ()
  Extends SwapchainCreateInfoKHR SurfaceFullScreenExclusiveInfoEXT = ()
  Extends SwapchainCreateInfoKHR SurfaceFullScreenExclusiveWin32InfoEXT = ()
  Extends WriteDescriptorSet WriteDescriptorSetInlineUniformBlockEXT = ()
  Extends WriteDescriptorSet WriteDescriptorSetAccelerationStructureKHR = ()
  Extends a b = TypeError (ShowType a :<>: Text " is not extended by " :<>: ShowType b)

data SomeStruct (a :: [Type] -> Type) where
  SomeStruct
    :: forall a es
     . (Extendss a es, PokeChain es, Show (Chain es))
    => a es
    -> SomeStruct a

deriving instance (forall es. Show (Chain es) => Show (a es)) => Show (SomeStruct a)

-- | The constraint is so on this instance to encourage type inference
instance Zero (a '[]) => Zero (SomeStruct a) where
  zero = SomeStruct (zero :: a '[])

-- | Forget which extensions a pointed-to struct has by casting the pointer
forgetExtensions :: Ptr (a es) -> Ptr (SomeStruct a)
forgetExtensions = castPtr

-- | Add an extension to the beginning of the struct chain
--
-- This can be used to optionally extend structs based on some condition (for
-- example, an extension or layer being available)
extendSomeStruct
  :: (Extensible a, Extends a e, ToCStruct e, Show e)
  => e
  -> SomeStruct a
  -> SomeStruct a
extendSomeStruct e (SomeStruct a) = SomeStruct (setNext a (e, getNext a))

-- | Consume a 'SomeStruct' value
withSomeStruct
  :: forall a b
   . SomeStruct a
  -> (forall es . (Extendss a es, PokeChain es, Show (Chain es)) => a es -> b)
  -> b
withSomeStruct (SomeStruct s) f = f s

-- | Write the C representation of some extended @a@ and use the pointer,
-- the pointer must not be returned from the continuation.
withSomeCStruct
  :: forall a b
   . (forall es . (Extendss a es, PokeChain es) => ToCStruct (a es))
  => SomeStruct a
  -> (forall es . (Extendss a es, PokeChain es) => Ptr (a es) -> IO b)
  -> IO b
withSomeCStruct s f = withSomeStruct s (`withCStruct` f)

-- | Given some memory for the head of the chain, allocate and poke the
-- tail and run an action.
pokeSomeCStruct
  :: (forall es . (Extendss a es, PokeChain es) => ToCStruct (a es))
  => Ptr (SomeStruct a)
  -- ^ Pointer to some memory at least the size of the head of the struct
  -- chain.
  -> SomeStruct a
  -- ^ The struct to poke
  -> IO b
  -- ^ Computation to run while the poked tail is valid
  -> IO b
pokeSomeCStruct p (SomeStruct s) = pokeCStruct (castPtr p) s

-- | Given a pointer to a struct with an unknown chain, peek the struct and
-- its chain.
peekSomeCStruct
  :: forall a
   . (Extensible a, forall es . (Extendss a es, PeekChain es) => FromCStruct (a es))
  => Ptr (SomeStruct a)
  -> IO (SomeStruct a)
peekSomeCStruct p = do
  head'  <- peekCStruct (castPtr @_ @(a '[]) p)
  pNext <- peek @(Ptr BaseOutStructure) (p `plusPtr` 8)
  peekSomeChain @a pNext $ \tail' -> SomeStruct (setNext head' tail')

peekSomeChain
  :: forall a b
   . (Extensible a)
  => Ptr BaseOutStructure
  -> (  forall es
      . (Extendss a es, PokeChain es, Show (Chain es))
     => Chain es
     -> b
     )
  -> IO b
peekSomeChain p c = if p == nullPtr
  then pure (c ())
  else do
    baseOut <- peek p
    join
      $ peekChainHead @a (sType (baseOut :: BaseOutStructure))
                         (castPtr @BaseOutStructure @() p)
      $ \head' -> peekSomeChain @a (next (baseOut :: BaseOutStructure))
                                  (\tail' -> c (head', tail'))

peekChainHead
  :: forall a b
   . Extensible a
  => StructureType
  -> Ptr ()
  -> (forall e . (Extends a e, ToCStruct e, Show e) => e -> b)
  -> IO b
peekChainHead ty p c = case ty of
  STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR -> go @DisplayPresentInfoKHR
  STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT -> go @DebugReportCallbackCreateInfoEXT
  STRUCTURE_TYPE_VALIDATION_FLAGS_EXT -> go @ValidationFlagsEXT
  STRUCTURE_TYPE_VALIDATION_FEATURES_EXT -> go @ValidationFeaturesEXT
  STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD -> go @PipelineRasterizationStateRasterizationOrderAMD
  STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV -> go @DedicatedAllocationImageCreateInfoNV
  STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV -> go @DedicatedAllocationBufferCreateInfoNV
  STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV -> go @DedicatedAllocationMemoryAllocateInfoNV
  STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV -> go @ExternalMemoryImageCreateInfoNV
  STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV -> go @ExportMemoryAllocateInfoNV
  STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV -> go @ImportMemoryWin32HandleInfoNV
  STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV -> go @ExportMemoryWin32HandleInfoNV
  STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV -> go @Win32KeyedMutexAcquireReleaseInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV -> go @PhysicalDeviceDeviceGeneratedCommandsFeaturesNV
  STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT -> go @DevicePrivateDataCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT -> go @PhysicalDevicePrivateDataFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV -> go @PhysicalDeviceDeviceGeneratedCommandsPropertiesNV
  STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV -> go @GraphicsPipelineShaderGroupsCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 -> go @(PhysicalDeviceFeatures2 '[])
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR -> go @PhysicalDevicePushDescriptorPropertiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES -> go @PhysicalDeviceDriverProperties
  STRUCTURE_TYPE_PRESENT_REGIONS_KHR -> go @PresentRegionsKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES -> go @PhysicalDeviceVariablePointersFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO -> go @PhysicalDeviceExternalImageFormatInfo
  STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES -> go @ExternalImageFormatProperties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES -> go @PhysicalDeviceIDProperties
  STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO -> go @ExternalMemoryImageCreateInfo
  STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO -> go @ExternalMemoryBufferCreateInfo
  STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO -> go @ExportMemoryAllocateInfo
  STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR -> go @ImportMemoryWin32HandleInfoKHR
  STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR -> go @ExportMemoryWin32HandleInfoKHR
  STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR -> go @ImportMemoryFdInfoKHR
  STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR -> go @Win32KeyedMutexAcquireReleaseInfoKHR
  STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO -> go @ExportSemaphoreCreateInfo
  STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR -> go @ExportSemaphoreWin32HandleInfoKHR
  STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR -> go @D3D12FenceSubmitInfoKHR
  STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO -> go @ExportFenceCreateInfo
  STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR -> go @ExportFenceWin32HandleInfoKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES -> go @PhysicalDeviceMultiviewFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES -> go @PhysicalDeviceMultiviewProperties
  STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO -> go @RenderPassMultiviewCreateInfo
  STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT -> go @SwapchainCounterCreateInfoEXT
  STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO -> go @MemoryAllocateFlagsInfo
  STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO -> go @BindBufferMemoryDeviceGroupInfo
  STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO -> go @BindImageMemoryDeviceGroupInfo
  STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO -> go @DeviceGroupRenderPassBeginInfo
  STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO -> go @DeviceGroupCommandBufferBeginInfo
  STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO -> go @DeviceGroupSubmitInfo
  STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO -> go @DeviceGroupBindSparseInfo
  STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR -> go @ImageSwapchainCreateInfoKHR
  STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR -> go @BindImageMemorySwapchainInfoKHR
  STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR -> go @DeviceGroupPresentInfoKHR
  STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO -> go @DeviceGroupDeviceCreateInfo
  STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR -> go @DeviceGroupSwapchainCreateInfoKHR
  STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD -> go @DisplayNativeHdrSurfaceCapabilitiesAMD
  STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD -> go @SwapchainDisplayNativeHdrCreateInfoAMD
  STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE -> go @PresentTimesInfoGOOGLE
  STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV -> go @PipelineViewportWScalingStateCreateInfoNV
  STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV -> go @PipelineViewportSwizzleStateCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT -> go @PhysicalDeviceDiscardRectanglePropertiesEXT
  STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT -> go @PipelineDiscardRectangleStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX -> go @PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO -> go @RenderPassInputAttachmentAspectCreateInfo
  STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR -> go @SharedPresentSurfaceCapabilitiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES -> go @PhysicalDevice16BitStorageFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES -> go @PhysicalDeviceSubgroupProperties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES -> go @PhysicalDeviceShaderSubgroupExtendedTypesFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES -> go @PhysicalDevicePointClippingProperties
  STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS -> go @MemoryDedicatedRequirements
  STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO -> go @MemoryDedicatedAllocateInfo
  STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO -> go @ImageViewUsageCreateInfo
  STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO -> go @PipelineTessellationDomainOriginStateCreateInfo
  STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO -> go @SamplerYcbcrConversionInfo
  STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO -> go @BindImagePlaneMemoryInfo
  STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO -> go @ImagePlaneMemoryRequirementsInfo
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES -> go @PhysicalDeviceSamplerYcbcrConversionFeatures
  STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES -> go @SamplerYcbcrConversionImageFormatProperties
  STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD -> go @TextureLODGatherFormatPropertiesAMD
  STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO -> go @ProtectedSubmitInfo
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES -> go @PhysicalDeviceProtectedMemoryFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES -> go @PhysicalDeviceProtectedMemoryProperties
  STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV -> go @PipelineCoverageToColorStateCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES -> go @PhysicalDeviceSamplerFilterMinmaxProperties
  STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT -> go @SampleLocationsInfoEXT
  STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT -> go @RenderPassSampleLocationsBeginInfoEXT
  STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT -> go @PipelineSampleLocationsStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT -> go @PhysicalDeviceSampleLocationsPropertiesEXT
  STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO -> go @SamplerReductionModeCreateInfo
  STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT -> go @PhysicalDeviceBlendOperationAdvancedFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT -> go @PhysicalDeviceBlendOperationAdvancedPropertiesEXT
  STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT -> go @PipelineColorBlendAdvancedStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT -> go @PhysicalDeviceInlineUniformBlockFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT -> go @PhysicalDeviceInlineUniformBlockPropertiesEXT
  STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT -> go @WriteDescriptorSetInlineUniformBlockEXT
  STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT -> go @DescriptorPoolInlineUniformBlockCreateInfoEXT
  STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV -> go @PipelineCoverageModulationStateCreateInfoNV
  STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO -> go @ImageFormatListCreateInfo
  STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT -> go @ShaderModuleValidationCacheCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES -> go @PhysicalDeviceMaintenance3Properties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES -> go @PhysicalDeviceShaderDrawParametersFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES -> go @PhysicalDeviceShaderFloat16Int8Features
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES -> go @PhysicalDeviceFloatControlsProperties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES -> go @PhysicalDeviceHostQueryResetFeatures
  STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT -> go @DeviceQueueGlobalPriorityCreateInfoEXT
  STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT -> go @DebugUtilsMessengerCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_MEMORY_REPORT_FEATURES_EXT -> go @PhysicalDeviceDeviceMemoryReportFeaturesEXT
  STRUCTURE_TYPE_DEVICE_DEVICE_MEMORY_REPORT_CREATE_INFO_EXT -> go @DeviceDeviceMemoryReportCreateInfoEXT
  STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT -> go @ImportMemoryHostPointerInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT -> go @PhysicalDeviceExternalMemoryHostPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT -> go @PhysicalDeviceConservativeRasterizationPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD -> go @PhysicalDeviceShaderCorePropertiesAMD
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD -> go @PhysicalDeviceShaderCoreProperties2AMD
  STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT -> go @PipelineRasterizationConservativeStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES -> go @PhysicalDeviceDescriptorIndexingFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES -> go @PhysicalDeviceDescriptorIndexingProperties
  STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO -> go @DescriptorSetLayoutBindingFlagsCreateInfo
  STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO -> go @DescriptorSetVariableDescriptorCountAllocateInfo
  STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT -> go @DescriptorSetVariableDescriptorCountLayoutSupport
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES -> go @PhysicalDeviceTimelineSemaphoreFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES -> go @PhysicalDeviceTimelineSemaphoreProperties
  STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO -> go @SemaphoreTypeCreateInfo
  STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO -> go @TimelineSemaphoreSubmitInfo
  STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT -> go @PipelineVertexInputDivisorStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT -> go @PhysicalDeviceVertexAttributeDivisorPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT -> go @PhysicalDevicePCIBusInfoPropertiesEXT
  STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID -> go @ImportAndroidHardwareBufferInfoANDROID
  STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID -> go @AndroidHardwareBufferUsageANDROID
  STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID -> go @AndroidHardwareBufferFormatPropertiesANDROID
  STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT -> go @CommandBufferInheritanceConditionalRenderingInfoEXT
  STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID -> go @ExternalFormatANDROID
  STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES -> go @PhysicalDevice8BitStorageFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT -> go @PhysicalDeviceConditionalRenderingFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES -> go @PhysicalDeviceVulkanMemoryModelFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES -> go @PhysicalDeviceShaderAtomicInt64Features
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT_FEATURES_EXT -> go @PhysicalDeviceShaderAtomicFloatFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT -> go @PhysicalDeviceVertexAttributeDivisorFeaturesEXT
  STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV -> go @QueueFamilyCheckpointPropertiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES -> go @PhysicalDeviceDepthStencilResolveProperties
  STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE -> go @SubpassDescriptionDepthStencilResolve
  STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT -> go @ImageViewASTCDecodeModeEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT -> go @PhysicalDeviceASTCDecodeFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT -> go @PhysicalDeviceTransformFeedbackFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT -> go @PhysicalDeviceTransformFeedbackPropertiesEXT
  STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT -> go @PipelineRasterizationStateStreamCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV -> go @PhysicalDeviceRepresentativeFragmentTestFeaturesNV
  STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV -> go @PipelineRepresentativeFragmentTestStateCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV -> go @PhysicalDeviceExclusiveScissorFeaturesNV
  STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV -> go @PipelineViewportExclusiveScissorStateCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV -> go @PhysicalDeviceCornerSampledImageFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV -> go @PhysicalDeviceComputeShaderDerivativesFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV -> go @PhysicalDeviceFragmentShaderBarycentricFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV -> go @PhysicalDeviceShaderImageFootprintFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV -> go @PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV -> go @PipelineViewportShadingRateImageStateCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV -> go @PhysicalDeviceShadingRateImageFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV -> go @PhysicalDeviceShadingRateImagePropertiesNV
  STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV -> go @PipelineViewportCoarseSampleOrderStateCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV -> go @PhysicalDeviceMeshShaderFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV -> go @PhysicalDeviceMeshShaderPropertiesNV
  STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR -> go @WriteDescriptorSetAccelerationStructureKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR -> go @PhysicalDeviceRayTracingFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR -> go @PhysicalDeviceRayTracingPropertiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV -> go @PhysicalDeviceRayTracingPropertiesNV
  STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT -> go @DrmFormatModifierPropertiesListEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT -> go @PhysicalDeviceImageDrmFormatModifierInfoEXT
  STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT -> go @ImageDrmFormatModifierListCreateInfoEXT
  STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT -> go @ImageDrmFormatModifierExplicitCreateInfoEXT
  STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO -> go @ImageStencilUsageCreateInfo
  STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD -> go @DeviceMemoryOverallocationCreateInfoAMD
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT -> go @PhysicalDeviceFragmentDensityMapFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT -> go @PhysicalDeviceFragmentDensityMap2FeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT -> go @PhysicalDeviceFragmentDensityMapPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT -> go @PhysicalDeviceFragmentDensityMap2PropertiesEXT
  STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT -> go @RenderPassFragmentDensityMapCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES -> go @PhysicalDeviceScalarBlockLayoutFeatures
  STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR -> go @SurfaceProtectedCapabilitiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES -> go @PhysicalDeviceUniformBufferStandardLayoutFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT -> go @PhysicalDeviceDepthClipEnableFeaturesEXT
  STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT -> go @PipelineRasterizationDepthClipStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT -> go @PhysicalDeviceMemoryBudgetPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT -> go @PhysicalDeviceMemoryPriorityFeaturesEXT
  STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT -> go @MemoryPriorityAllocateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES -> go @PhysicalDeviceBufferDeviceAddressFeatures
  STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT -> go @PhysicalDeviceBufferDeviceAddressFeaturesEXT
  STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO -> go @BufferOpaqueCaptureAddressCreateInfo
  STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT -> go @BufferDeviceAddressCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT -> go @PhysicalDeviceImageViewImageFormatInfoEXT
  STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT -> go @FilterCubicImageViewImageFormatPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES -> go @PhysicalDeviceImagelessFramebufferFeatures
  STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO -> go @FramebufferAttachmentsCreateInfo
  STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO -> go @RenderPassAttachmentBeginInfo
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT -> go @PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV -> go @PhysicalDeviceCooperativeMatrixFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV -> go @PhysicalDeviceCooperativeMatrixPropertiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT -> go @PhysicalDeviceYcbcrImageArraysFeaturesEXT
  STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP -> go @PresentFrameTokenGGP
  STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT -> go @PipelineCreationFeedbackCreateInfoEXT
  STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT -> go @SurfaceFullScreenExclusiveInfoEXT
  STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT -> go @SurfaceFullScreenExclusiveWin32InfoEXT
  STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT -> go @SurfaceCapabilitiesFullScreenExclusiveEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR -> go @PhysicalDevicePerformanceQueryFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR -> go @PhysicalDevicePerformanceQueryPropertiesKHR
  STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR -> go @QueryPoolPerformanceCreateInfoKHR
  STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR -> go @PerformanceQuerySubmitInfoKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV -> go @PhysicalDeviceCoverageReductionModeFeaturesNV
  STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV -> go @PipelineCoverageReductionStateCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL -> go @PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL
  STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL -> go @QueryPoolPerformanceQueryCreateInfoINTEL
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR -> go @PhysicalDeviceShaderClockFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT -> go @PhysicalDeviceIndexTypeUint8FeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV -> go @PhysicalDeviceShaderSMBuiltinsPropertiesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV -> go @PhysicalDeviceShaderSMBuiltinsFeaturesNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT -> go @PhysicalDeviceFragmentShaderInterlockFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES -> go @PhysicalDeviceSeparateDepthStencilLayoutsFeatures
  STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT -> go @AttachmentReferenceStencilLayout
  STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT -> go @AttachmentDescriptionStencilLayout
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR -> go @PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT -> go @PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT -> go @PhysicalDeviceTexelBufferAlignmentFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT -> go @PhysicalDeviceTexelBufferAlignmentPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT -> go @PhysicalDeviceSubgroupSizeControlFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT -> go @PhysicalDeviceSubgroupSizeControlPropertiesEXT
  STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT -> go @PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
  STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO -> go @MemoryOpaqueCaptureAddressAllocateInfo
  STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT -> go @PhysicalDeviceLineRasterizationFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT -> go @PhysicalDeviceLineRasterizationPropertiesEXT
  STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT -> go @PipelineRasterizationLineStateCreateInfoEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT -> go @PhysicalDevicePipelineCreationCacheControlFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES -> go @PhysicalDeviceVulkan11Features
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES -> go @PhysicalDeviceVulkan11Properties
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES -> go @PhysicalDeviceVulkan12Features
  STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES -> go @PhysicalDeviceVulkan12Properties
  STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD -> go @PipelineCompilerControlCreateInfoAMD
  STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD -> go @PhysicalDeviceCoherentMemoryFeaturesAMD
  STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT -> throwIO $ IOError Nothing InvalidArgument "peekChainHead" ("struct type STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT contains an undiscriminated union (ClearColorValue) and can't be safely peeked") Nothing Nothing
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT -> go @PhysicalDeviceCustomBorderColorPropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT -> go @PhysicalDeviceCustomBorderColorFeaturesEXT
  STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR -> go @DeferredOperationInfoKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT -> go @PhysicalDeviceExtendedDynamicStateFeaturesEXT
  STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM -> go @RenderPassTransformBeginInfoQCOM
  STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM -> go @CommandBufferInheritanceRenderPassTransformInfoQCOM
  STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV -> go @PhysicalDeviceDiagnosticsConfigFeaturesNV
  STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV -> go @DeviceDiagnosticsConfigCreateInfoNV
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT -> go @PhysicalDeviceRobustness2FeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT -> go @PhysicalDeviceRobustness2PropertiesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_ROBUSTNESS_FEATURES_EXT -> go @PhysicalDeviceImageRobustnessFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_FEATURES_KHR -> go @PhysicalDevicePortabilitySubsetFeaturesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_PORTABILITY_SUBSET_PROPERTIES_KHR -> go @PhysicalDevicePortabilitySubsetPropertiesKHR
  STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT -> go @PhysicalDevice4444FormatsFeaturesEXT
  STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_ATOMIC_INT64_FEATURES_EXT -> go @PhysicalDeviceShaderImageAtomicInt64FeaturesEXT
  t -> throwIO $ IOError Nothing InvalidArgument "peekChainHead" ("Unrecognized struct type: " <> show t) Nothing Nothing
 where
  go :: forall e . (Typeable e, FromCStruct e, ToCStruct e, Show e) => IO b
  go =
    let r = extends @a @e Proxy $ do
          head' <- peekCStruct @e (castPtr p)
          pure $ c head'
    in  fromMaybe
          (throwIO $ IOError
            Nothing
            InvalidArgument
            "peekChainHead"
            (  "Illegal struct extension of "
            <> show (extensibleType @a)
            <> " with "
            <> show ty
            )
            Nothing
            Nothing
          )
          r

class Extensible (a :: [Type] -> Type) where
  extensibleType :: StructureType
  getNext :: a es -> Chain es
  setNext :: a ds -> Chain es -> a es
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends a e => b) -> Maybe b

type family Chain (xs :: [a]) = (r :: a) | r -> xs where
  Chain '[]    = ()
  Chain (x:xs) = (x, Chain xs)

-- | A pattern synonym to separate the head of a struct chain from the
-- tail, use in conjunction with ':&' to extract several members.
--
-- @
-- Head{..} ::& () <- returningNoTail a b c
-- -- Equivalent to
-- Head{..} <- returningNoTail @'[] a b c
-- @
--
-- @
-- Head{..} ::& Foo{..} :& Bar{..} :& () <- returningWithTail a b c
-- @
--
-- @
-- myFun (Head{..} :&& Foo{..} :& ())
-- @
pattern (::&) :: Extensible a => a es' -> Chain es -> a es
pattern a ::& es <- (\a -> (a, getNext a) -> (a, es))
  where a ::& es = setNext a es
infix 6 ::&
{-# complete (::&) :: DisplayPresentInfoKHR #-}
{-# complete (::&) :: DebugReportCallbackCreateInfoEXT #-}
{-# complete (::&) :: ValidationFlagsEXT #-}
{-# complete (::&) :: ValidationFeaturesEXT #-}
{-# complete (::&) :: PipelineRasterizationStateRasterizationOrderAMD #-}
{-# complete (::&) :: DedicatedAllocationImageCreateInfoNV #-}
{-# complete (::&) :: DedicatedAllocationBufferCreateInfoNV #-}
{-# complete (::&) :: DedicatedAllocationMemoryAllocateInfoNV #-}
{-# complete (::&) :: ExternalMemoryImageCreateInfoNV #-}
{-# complete (::&) :: ExportMemoryAllocateInfoNV #-}
{-# complete (::&) :: ImportMemoryWin32HandleInfoNV #-}
{-# complete (::&) :: ExportMemoryWin32HandleInfoNV #-}
{-# complete (::&) :: Win32KeyedMutexAcquireReleaseInfoNV #-}
{-# complete (::&) :: PhysicalDeviceDeviceGeneratedCommandsFeaturesNV #-}
{-# complete (::&) :: DevicePrivateDataCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDevicePrivateDataFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceDeviceGeneratedCommandsPropertiesNV #-}
{-# complete (::&) :: GraphicsPipelineShaderGroupsCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceFeatures2 #-}
{-# complete (::&) :: PhysicalDevicePushDescriptorPropertiesKHR #-}
{-# complete (::&) :: PhysicalDeviceDriverProperties #-}
{-# complete (::&) :: PresentRegionsKHR #-}
{-# complete (::&) :: PhysicalDeviceVariablePointersFeatures #-}
{-# complete (::&) :: PhysicalDeviceExternalImageFormatInfo #-}
{-# complete (::&) :: ExternalImageFormatProperties #-}
{-# complete (::&) :: PhysicalDeviceIDProperties #-}
{-# complete (::&) :: ExternalMemoryImageCreateInfo #-}
{-# complete (::&) :: ExternalMemoryBufferCreateInfo #-}
{-# complete (::&) :: ExportMemoryAllocateInfo #-}
{-# complete (::&) :: ImportMemoryWin32HandleInfoKHR #-}
{-# complete (::&) :: ExportMemoryWin32HandleInfoKHR #-}
{-# complete (::&) :: ImportMemoryFdInfoKHR #-}
{-# complete (::&) :: Win32KeyedMutexAcquireReleaseInfoKHR #-}
{-# complete (::&) :: ExportSemaphoreCreateInfo #-}
{-# complete (::&) :: ExportSemaphoreWin32HandleInfoKHR #-}
{-# complete (::&) :: D3D12FenceSubmitInfoKHR #-}
{-# complete (::&) :: ExportFenceCreateInfo #-}
{-# complete (::&) :: ExportFenceWin32HandleInfoKHR #-}
{-# complete (::&) :: PhysicalDeviceMultiviewFeatures #-}
{-# complete (::&) :: PhysicalDeviceMultiviewProperties #-}
{-# complete (::&) :: RenderPassMultiviewCreateInfo #-}
{-# complete (::&) :: SwapchainCounterCreateInfoEXT #-}
{-# complete (::&) :: MemoryAllocateFlagsInfo #-}
{-# complete (::&) :: BindBufferMemoryDeviceGroupInfo #-}
{-# complete (::&) :: BindImageMemoryDeviceGroupInfo #-}
{-# complete (::&) :: DeviceGroupRenderPassBeginInfo #-}
{-# complete (::&) :: DeviceGroupCommandBufferBeginInfo #-}
{-# complete (::&) :: DeviceGroupSubmitInfo #-}
{-# complete (::&) :: DeviceGroupBindSparseInfo #-}
{-# complete (::&) :: ImageSwapchainCreateInfoKHR #-}
{-# complete (::&) :: BindImageMemorySwapchainInfoKHR #-}
{-# complete (::&) :: DeviceGroupPresentInfoKHR #-}
{-# complete (::&) :: DeviceGroupDeviceCreateInfo #-}
{-# complete (::&) :: DeviceGroupSwapchainCreateInfoKHR #-}
{-# complete (::&) :: DisplayNativeHdrSurfaceCapabilitiesAMD #-}
{-# complete (::&) :: SwapchainDisplayNativeHdrCreateInfoAMD #-}
{-# complete (::&) :: PresentTimesInfoGOOGLE #-}
{-# complete (::&) :: PipelineViewportWScalingStateCreateInfoNV #-}
{-# complete (::&) :: PipelineViewportSwizzleStateCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceDiscardRectanglePropertiesEXT #-}
{-# complete (::&) :: PipelineDiscardRectangleStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX #-}
{-# complete (::&) :: RenderPassInputAttachmentAspectCreateInfo #-}
{-# complete (::&) :: SharedPresentSurfaceCapabilitiesKHR #-}
{-# complete (::&) :: PhysicalDevice16BitStorageFeatures #-}
{-# complete (::&) :: PhysicalDeviceSubgroupProperties #-}
{-# complete (::&) :: PhysicalDeviceShaderSubgroupExtendedTypesFeatures #-}
{-# complete (::&) :: PhysicalDevicePointClippingProperties #-}
{-# complete (::&) :: MemoryDedicatedRequirements #-}
{-# complete (::&) :: MemoryDedicatedAllocateInfo #-}
{-# complete (::&) :: ImageViewUsageCreateInfo #-}
{-# complete (::&) :: PipelineTessellationDomainOriginStateCreateInfo #-}
{-# complete (::&) :: SamplerYcbcrConversionInfo #-}
{-# complete (::&) :: BindImagePlaneMemoryInfo #-}
{-# complete (::&) :: ImagePlaneMemoryRequirementsInfo #-}
{-# complete (::&) :: PhysicalDeviceSamplerYcbcrConversionFeatures #-}
{-# complete (::&) :: SamplerYcbcrConversionImageFormatProperties #-}
{-# complete (::&) :: TextureLODGatherFormatPropertiesAMD #-}
{-# complete (::&) :: ProtectedSubmitInfo #-}
{-# complete (::&) :: PhysicalDeviceProtectedMemoryFeatures #-}
{-# complete (::&) :: PhysicalDeviceProtectedMemoryProperties #-}
{-# complete (::&) :: PipelineCoverageToColorStateCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceSamplerFilterMinmaxProperties #-}
{-# complete (::&) :: SampleLocationsInfoEXT #-}
{-# complete (::&) :: RenderPassSampleLocationsBeginInfoEXT #-}
{-# complete (::&) :: PipelineSampleLocationsStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceSampleLocationsPropertiesEXT #-}
{-# complete (::&) :: SamplerReductionModeCreateInfo #-}
{-# complete (::&) :: PhysicalDeviceBlendOperationAdvancedFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT #-}
{-# complete (::&) :: PipelineColorBlendAdvancedStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceInlineUniformBlockFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceInlineUniformBlockPropertiesEXT #-}
{-# complete (::&) :: WriteDescriptorSetInlineUniformBlockEXT #-}
{-# complete (::&) :: DescriptorPoolInlineUniformBlockCreateInfoEXT #-}
{-# complete (::&) :: PipelineCoverageModulationStateCreateInfoNV #-}
{-# complete (::&) :: ImageFormatListCreateInfo #-}
{-# complete (::&) :: ShaderModuleValidationCacheCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceMaintenance3Properties #-}
{-# complete (::&) :: PhysicalDeviceShaderDrawParametersFeatures #-}
{-# complete (::&) :: PhysicalDeviceShaderFloat16Int8Features #-}
{-# complete (::&) :: PhysicalDeviceFloatControlsProperties #-}
{-# complete (::&) :: PhysicalDeviceHostQueryResetFeatures #-}
{-# complete (::&) :: DeviceQueueGlobalPriorityCreateInfoEXT #-}
{-# complete (::&) :: DebugUtilsMessengerCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceDeviceMemoryReportFeaturesEXT #-}
{-# complete (::&) :: DeviceDeviceMemoryReportCreateInfoEXT #-}
{-# complete (::&) :: ImportMemoryHostPointerInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceExternalMemoryHostPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceConservativeRasterizationPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderCorePropertiesAMD #-}
{-# complete (::&) :: PhysicalDeviceShaderCoreProperties2AMD #-}
{-# complete (::&) :: PipelineRasterizationConservativeStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceDescriptorIndexingFeatures #-}
{-# complete (::&) :: PhysicalDeviceDescriptorIndexingProperties #-}
{-# complete (::&) :: DescriptorSetLayoutBindingFlagsCreateInfo #-}
{-# complete (::&) :: DescriptorSetVariableDescriptorCountAllocateInfo #-}
{-# complete (::&) :: DescriptorSetVariableDescriptorCountLayoutSupport #-}
{-# complete (::&) :: PhysicalDeviceTimelineSemaphoreFeatures #-}
{-# complete (::&) :: PhysicalDeviceTimelineSemaphoreProperties #-}
{-# complete (::&) :: SemaphoreTypeCreateInfo #-}
{-# complete (::&) :: TimelineSemaphoreSubmitInfo #-}
{-# complete (::&) :: PipelineVertexInputDivisorStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceVertexAttributeDivisorPropertiesEXT #-}
{-# complete (::&) :: PhysicalDevicePCIBusInfoPropertiesEXT #-}
{-# complete (::&) :: ImportAndroidHardwareBufferInfoANDROID #-}
{-# complete (::&) :: AndroidHardwareBufferUsageANDROID #-}
{-# complete (::&) :: AndroidHardwareBufferFormatPropertiesANDROID #-}
{-# complete (::&) :: CommandBufferInheritanceConditionalRenderingInfoEXT #-}
{-# complete (::&) :: ExternalFormatANDROID #-}
{-# complete (::&) :: PhysicalDevice8BitStorageFeatures #-}
{-# complete (::&) :: PhysicalDeviceConditionalRenderingFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceVulkanMemoryModelFeatures #-}
{-# complete (::&) :: PhysicalDeviceShaderAtomicInt64Features #-}
{-# complete (::&) :: PhysicalDeviceShaderAtomicFloatFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT #-}
{-# complete (::&) :: QueueFamilyCheckpointPropertiesNV #-}
{-# complete (::&) :: PhysicalDeviceDepthStencilResolveProperties #-}
{-# complete (::&) :: SubpassDescriptionDepthStencilResolve #-}
{-# complete (::&) :: ImageViewASTCDecodeModeEXT #-}
{-# complete (::&) :: PhysicalDeviceASTCDecodeFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceTransformFeedbackFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceTransformFeedbackPropertiesEXT #-}
{-# complete (::&) :: PipelineRasterizationStateStreamCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceRepresentativeFragmentTestFeaturesNV #-}
{-# complete (::&) :: PipelineRepresentativeFragmentTestStateCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceExclusiveScissorFeaturesNV #-}
{-# complete (::&) :: PipelineViewportExclusiveScissorStateCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceCornerSampledImageFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceComputeShaderDerivativesFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceShaderImageFootprintFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV #-}
{-# complete (::&) :: PipelineViewportShadingRateImageStateCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceShadingRateImageFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceShadingRateImagePropertiesNV #-}
{-# complete (::&) :: PipelineViewportCoarseSampleOrderStateCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceMeshShaderFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceMeshShaderPropertiesNV #-}
{-# complete (::&) :: WriteDescriptorSetAccelerationStructureKHR #-}
{-# complete (::&) :: PhysicalDeviceRayTracingFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceRayTracingPropertiesKHR #-}
{-# complete (::&) :: PhysicalDeviceRayTracingPropertiesNV #-}
{-# complete (::&) :: DrmFormatModifierPropertiesListEXT #-}
{-# complete (::&) :: PhysicalDeviceImageDrmFormatModifierInfoEXT #-}
{-# complete (::&) :: ImageDrmFormatModifierListCreateInfoEXT #-}
{-# complete (::&) :: ImageDrmFormatModifierExplicitCreateInfoEXT #-}
{-# complete (::&) :: ImageStencilUsageCreateInfo #-}
{-# complete (::&) :: DeviceMemoryOverallocationCreateInfoAMD #-}
{-# complete (::&) :: PhysicalDeviceFragmentDensityMapFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceFragmentDensityMap2FeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceFragmentDensityMapPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceFragmentDensityMap2PropertiesEXT #-}
{-# complete (::&) :: RenderPassFragmentDensityMapCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceScalarBlockLayoutFeatures #-}
{-# complete (::&) :: SurfaceProtectedCapabilitiesKHR #-}
{-# complete (::&) :: PhysicalDeviceUniformBufferStandardLayoutFeatures #-}
{-# complete (::&) :: PhysicalDeviceDepthClipEnableFeaturesEXT #-}
{-# complete (::&) :: PipelineRasterizationDepthClipStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceMemoryBudgetPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceMemoryPriorityFeaturesEXT #-}
{-# complete (::&) :: MemoryPriorityAllocateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceBufferDeviceAddressFeatures #-}
{-# complete (::&) :: PhysicalDeviceBufferDeviceAddressFeaturesEXT #-}
{-# complete (::&) :: BufferOpaqueCaptureAddressCreateInfo #-}
{-# complete (::&) :: BufferDeviceAddressCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceImageViewImageFormatInfoEXT #-}
{-# complete (::&) :: FilterCubicImageViewImageFormatPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceImagelessFramebufferFeatures #-}
{-# complete (::&) :: FramebufferAttachmentsCreateInfo #-}
{-# complete (::&) :: RenderPassAttachmentBeginInfo #-}
{-# complete (::&) :: PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceCooperativeMatrixFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceCooperativeMatrixPropertiesNV #-}
{-# complete (::&) :: PhysicalDeviceYcbcrImageArraysFeaturesEXT #-}
{-# complete (::&) :: PresentFrameTokenGGP #-}
{-# complete (::&) :: PipelineCreationFeedbackCreateInfoEXT #-}
{-# complete (::&) :: SurfaceFullScreenExclusiveInfoEXT #-}
{-# complete (::&) :: SurfaceFullScreenExclusiveWin32InfoEXT #-}
{-# complete (::&) :: SurfaceCapabilitiesFullScreenExclusiveEXT #-}
{-# complete (::&) :: PhysicalDevicePerformanceQueryFeaturesKHR #-}
{-# complete (::&) :: PhysicalDevicePerformanceQueryPropertiesKHR #-}
{-# complete (::&) :: QueryPoolPerformanceCreateInfoKHR #-}
{-# complete (::&) :: PerformanceQuerySubmitInfoKHR #-}
{-# complete (::&) :: PhysicalDeviceCoverageReductionModeFeaturesNV #-}
{-# complete (::&) :: PipelineCoverageReductionStateCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL #-}
{-# complete (::&) :: QueryPoolPerformanceQueryCreateInfoINTEL #-}
{-# complete (::&) :: PhysicalDeviceShaderClockFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceIndexTypeUint8FeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderSMBuiltinsPropertiesNV #-}
{-# complete (::&) :: PhysicalDeviceShaderSMBuiltinsFeaturesNV #-}
{-# complete (::&) :: PhysicalDeviceFragmentShaderInterlockFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceSeparateDepthStencilLayoutsFeatures #-}
{-# complete (::&) :: AttachmentReferenceStencilLayout #-}
{-# complete (::&) :: AttachmentDescriptionStencilLayout #-}
{-# complete (::&) :: PhysicalDevicePipelineExecutablePropertiesFeaturesKHR #-}
{-# complete (::&) :: PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceTexelBufferAlignmentFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceTexelBufferAlignmentPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceSubgroupSizeControlFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceSubgroupSizeControlPropertiesEXT #-}
{-# complete (::&) :: PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT #-}
{-# complete (::&) :: MemoryOpaqueCaptureAddressAllocateInfo #-}
{-# complete (::&) :: PhysicalDeviceLineRasterizationFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceLineRasterizationPropertiesEXT #-}
{-# complete (::&) :: PipelineRasterizationLineStateCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDevicePipelineCreationCacheControlFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceVulkan11Features #-}
{-# complete (::&) :: PhysicalDeviceVulkan11Properties #-}
{-# complete (::&) :: PhysicalDeviceVulkan12Features #-}
{-# complete (::&) :: PhysicalDeviceVulkan12Properties #-}
{-# complete (::&) :: PipelineCompilerControlCreateInfoAMD #-}
{-# complete (::&) :: PhysicalDeviceCoherentMemoryFeaturesAMD #-}
{-# complete (::&) :: SamplerCustomBorderColorCreateInfoEXT #-}
{-# complete (::&) :: PhysicalDeviceCustomBorderColorPropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceCustomBorderColorFeaturesEXT #-}
{-# complete (::&) :: DeferredOperationInfoKHR #-}
{-# complete (::&) :: PhysicalDeviceExtendedDynamicStateFeaturesEXT #-}
{-# complete (::&) :: RenderPassTransformBeginInfoQCOM #-}
{-# complete (::&) :: CommandBufferInheritanceRenderPassTransformInfoQCOM #-}
{-# complete (::&) :: PhysicalDeviceDiagnosticsConfigFeaturesNV #-}
{-# complete (::&) :: DeviceDiagnosticsConfigCreateInfoNV #-}
{-# complete (::&) :: PhysicalDeviceRobustness2FeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceRobustness2PropertiesEXT #-}
{-# complete (::&) :: PhysicalDeviceImageRobustnessFeaturesEXT #-}
{-# complete (::&) :: PhysicalDevicePortabilitySubsetFeaturesKHR #-}
{-# complete (::&) :: PhysicalDevicePortabilitySubsetPropertiesKHR #-}
{-# complete (::&) :: PhysicalDevice4444FormatsFeaturesEXT #-}
{-# complete (::&) :: PhysicalDeviceShaderImageAtomicInt64FeaturesEXT #-}

-- | View the head and tail of a 'Chain', see '::&'
--
-- Equivalent to @(,)@
pattern (:&) :: e -> Chain es -> Chain (e:es)
pattern e :& es = (e, es)
infixr 7 :&
{-# complete (:&) #-}

type family Extendss (p :: [Type] -> Type) (xs :: [Type]) :: Constraint where
  Extendss p '[]      = ()
  Extendss p (x : xs) = (Extends p x, Extendss p xs)

class PokeChain es where
  withChain :: Chain es -> (Ptr (Chain es) -> IO a) -> IO a
  withZeroChain :: (Ptr (Chain es) -> IO a) -> IO a

instance PokeChain '[] where
  withChain () f = f nullPtr
  withZeroChain f = f nullPtr

instance (ToCStruct e, PokeChain es) => PokeChain (e:es) where
  withChain (e, es) f = evalContT $ do
    t <- ContT $ withChain es
    h <- ContT $ withCStruct e
    lift $ linkChain h t
    lift $ f (castPtr h)
  withZeroChain f = evalContT $ do
    t <- ContT $ withZeroChain @es
    h <- ContT $ withZeroCStruct @e
    lift $ linkChain h t
    lift $ f (castPtr h)

class PeekChain es where
  peekChain :: Ptr (Chain es) -> IO (Chain es)

instance PeekChain '[] where
  peekChain _ = pure ()

instance (FromCStruct e, PeekChain es) => PeekChain (e:es) where
  peekChain p = do
    h <- peekCStruct @e (castPtr p)
    tPtr <- peek (p `plusPtr` 8)
    t <- peekChain tPtr
    pure (h, t)

linkChain :: Ptr a -> Ptr b -> IO ()
linkChain head' tail' = poke (head' `plusPtr` 8) tail'

