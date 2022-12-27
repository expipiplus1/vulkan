{-# language CPP #-}
{-# language NoDuplicateRecordFields #-}
-- No documentation found for Chapter "Dynamic"
module Vulkan.Dynamic  ( InstanceCmds(..)
                       , getInstanceProcAddr'
                       , initInstanceCmds
                       , DeviceCmds(..)
                       , initDeviceCmds
                       ) where

import Vulkan.CStruct.Utils (FixedArray)
import Foreign.Ptr (castFunPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Vulkan.Zero (Zero(..))
import Foreign.C.Types (CChar)
import Foreign.C.Types (CFloat)
import Foreign.C.Types (CInt)
import Foreign.C.Types (CSize)
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Ptr (Ptr(Ptr))
import Data.Word (Word16)
import Data.Word (Word32)
import Data.Word (Word64)
import Vulkan.NamedType ((:::))
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (AHardwareBuffer)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureBuildGeometryInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureBuildRangeInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureBuildSizesInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureBuildTypeKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (AccelerationStructureCaptureDescriptorDataInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureCompatibilityKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (AccelerationStructureCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureDeviceAddressInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (AccelerationStructureInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.Handles (AccelerationStructureKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (AccelerationStructureMemoryRequirementsInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.Handles (AccelerationStructureNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureVersionInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (AcquireNextImageInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (AcquireProfilingLockInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (AndroidHardwareBufferPropertiesANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_android_surface (AndroidSurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.CStruct.Extends (BaseOutStructure)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (BindAccelerationStructureMemoryInfoNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (BindBufferMemoryInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2 (BindImageMemoryInfo)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (BindSparseInfo)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (BlitImageInfo2)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Bool32)
import {-# SOURCE #-} Vulkan.Core10.Handles (Buffer)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (BufferCaptureDescriptorDataInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferCollectionCreateInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.Handles (BufferCollectionFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferCollectionPropertiesFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (BufferConstraintsInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (BufferCopy)
import {-# SOURCE #-} Vulkan.Core10.Buffer (BufferCreateInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferDeviceAddressInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (BufferImageCopy)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (BufferMemoryBarrier)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (BufferMemoryRequirementsInfo2)
import {-# SOURCE #-} Vulkan.Core10.Handles (BufferView)
import {-# SOURCE #-} Vulkan.Core10.BufferView (BufferViewCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_calibrated_timestamps (CalibratedTimestampInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_synchronization2 (CheckpointData2NV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints (CheckpointDataNV)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ClearAttachment)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ClearColorValue)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ClearDepthStencilValue)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ClearRect)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (CoarseSampleOrderCustomNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (CoarseSampleOrderTypeNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (ColorBlendAdvancedEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_extended_dynamic_state3 (ColorBlendEquationEXT)
import {-# SOURCE #-} Vulkan.Core10.Enums.ColorComponentFlagBits (ColorComponentFlags)
import {-# SOURCE #-} Vulkan.Core10.CommandBuffer (CommandBufferAllocateInfo)
import {-# SOURCE #-} Vulkan.Core10.CommandBuffer (CommandBufferBeginInfo)
import {-# SOURCE #-} Vulkan.Core10.Enums.CommandBufferResetFlagBits (CommandBufferResetFlags)
import {-# SOURCE #-} Vulkan.Core10.Handles (CommandBuffer_T)
import {-# SOURCE #-} Vulkan.Core10.Handles (CommandPool)
import {-# SOURCE #-} Vulkan.Core10.CommandPool (CommandPoolCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.Enums.CommandPoolResetFlagBits (CommandPoolResetFlags)
import {-# SOURCE #-} Vulkan.Core11.Enums.CommandPoolTrimFlags (CommandPoolTrimFlags)
import {-# SOURCE #-} Vulkan.Core10.Enums.CompareOp (CompareOp)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (ComputePipelineCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conditional_rendering (ConditionalRenderingBeginInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_conservative_rasterization (ConservativeRasterizationModeEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_cooperative_matrix (CooperativeMatrixPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureToMemoryInfoKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyBufferInfo2)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyBufferToImageInfo2)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (CopyDescriptorSet)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyImageInfo2)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (CopyImageToBufferInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_acceleration_structure (CopyMemoryToAccelerationStructureInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (CopyMemoryToMicromapInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (CopyMicromapInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (CopyMicromapToMemoryInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_framebuffer_mixed_samples (CoverageModulationModeNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (CoverageReductionModeNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_binary_import (CuFunctionCreateInfoNVX)
import {-# SOURCE #-} Vulkan.Extensions.Handles (CuFunctionNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_binary_import (CuLaunchInfoNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_binary_import (CuModuleCreateInfoNVX)
import {-# SOURCE #-} Vulkan.Extensions.Handles (CuModuleNVX)
import {-# SOURCE #-} Vulkan.Core10.Enums.CullModeFlagBits (CullModeFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_marker (DebugMarkerMarkerInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_marker (DebugMarkerObjectNameInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_marker (DebugMarkerObjectTagInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_report (DebugReportCallbackCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.Handles (DebugReportCallbackEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_report (DebugReportFlagsEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsLabelEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsMessageSeverityFlagBitsEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsMessageTypeFlagsEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsMessengerCallbackDataEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsMessengerCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.Handles (DebugUtilsMessengerEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsObjectNameInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_debug_utils (DebugUtilsObjectTagInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_memory_decompression (DecompressMemoryRegionNV)
import {-# SOURCE #-} Vulkan.Extensions.Handles (DeferredOperationKHR)
import {-# SOURCE #-} Vulkan.Core10.Enums.DependencyFlagBits (DependencyFlags)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (DependencyInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (DescriptorBufferBindingInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (DescriptorGetInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Handles (DescriptorPool)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorPoolCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.Enums.DescriptorPoolResetFlags (DescriptorPoolResetFlags)
import {-# SOURCE #-} Vulkan.Core10.Handles (DescriptorSet)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorSetAllocateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_VALVE_descriptor_set_host_mapping (DescriptorSetBindingReferenceVALVE)
import {-# SOURCE #-} Vulkan.Core10.Handles (DescriptorSetLayout)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (DescriptorSetLayoutCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_VALVE_descriptor_set_host_mapping (DescriptorSetLayoutHostMappingInfoVALVE)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_maintenance3 (DescriptorSetLayoutSupport)
import {-# SOURCE #-} Vulkan.Core11.Handles (DescriptorUpdateTemplate)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template (DescriptorUpdateTemplateCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (DeviceAddress)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (DeviceBufferMemoryRequirements)
import {-# SOURCE #-} Vulkan.Core10.Device (DeviceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (DeviceEventInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_fault (DeviceFaultCountsEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_device_fault (DeviceFaultInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (DeviceGroupPresentModeFlagsKHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_maintenance4 (DeviceImageMemoryRequirements)
import {-# SOURCE #-} Vulkan.Core10.Handles (DeviceMemory)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (DeviceMemoryOpaqueCaptureAddressInfo)
import {-# SOURCE #-} Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory (DeviceQueueInfo2)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (DeviceSize)
import {-# SOURCE #-} Vulkan.Core10.Handles (Device_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_directfb_surface (DirectFBSurfaceCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_xlib_surface (Display)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (DisplayEventInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.Handles (DisplayKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayModeCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.Handles (DisplayModeKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_display_properties2 (DisplayModeProperties2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayModePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_display_properties2 (DisplayPlaneCapabilities2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayPlaneCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_display_properties2 (DisplayPlaneInfo2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_display_properties2 (DisplayPlaneProperties2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayPlanePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_control (DisplayPowerInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_display_properties2 (DisplayProperties2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplayPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_display (DisplaySurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.Handles (Event)
import {-# SOURCE #-} Vulkan.Core10.Event (EventCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalObjectsInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.ExtensionDiscovery (ExtensionProperties)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Extent2D)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (ExternalBufferProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities (ExternalFenceProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalImageFormatPropertiesNV)
import {-# SOURCE #-} Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits (ExternalMemoryHandleTypeFlagBits)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_capabilities (ExternalMemoryHandleTypeFlagsNV)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities (ExternalSemaphoreProperties)
import {-# SOURCE #-} Vulkan.Core10.Handles (Fence)
import {-# SOURCE #-} Vulkan.Core10.Fence (FenceCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_fd (FenceGetFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_win32 (FenceGetWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.Enums.Filter (Filter)
import {-# SOURCE #-} Vulkan.Core10.Enums.Format (Format)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (FormatProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (FormatProperties2)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (FragmentShadingRateCombinerOpKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_fragment_shading_rate_enums (FragmentShadingRateNV)
import {-# SOURCE #-} Vulkan.Core10.Handles (Framebuffer)
import {-# SOURCE #-} Vulkan.Core10.Pass (FramebufferCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_coverage_reduction_mode (FramebufferMixedSamplesCombinationNV)
import {-# SOURCE #-} Vulkan.Core10.Enums.FrontFace (FrontFace)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GeneratedCommandsInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (GeneratedCommandsMemoryRequirementsInfoNV)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (GraphicsPipelineCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_win32 (HANDLE)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_hdr_metadata (HdrMetadataEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_headless_surface (HeadlessSurfaceCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_directfb_surface (IDirectFB)
import {-# SOURCE #-} Vulkan.Extensions.VK_MVK_ios_surface (IOSSurfaceCreateInfoMVK)
import {-# SOURCE #-} Vulkan.Core10.Handles (Image)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageBlit)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (ImageCaptureDescriptorDataInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_buffer_collection (ImageConstraintsInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageCopy)
import {-# SOURCE #-} Vulkan.Core10.Enums.ImageCreateFlagBits (ImageCreateFlags)
import {-# SOURCE #-} Vulkan.Core10.Image (ImageCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_drm_format_modifier (ImageDrmFormatModifierPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (ImageFormatProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (ImageFormatProperties2)
import {-# SOURCE #-} Vulkan.Core10.Enums.ImageLayout (ImageLayout)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (ImageMemoryBarrier)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (ImageMemoryRequirementsInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface (ImagePipeSurfaceCreateInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageResolve)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (ImageSparseMemoryRequirementsInfo2)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (ImageSubresource)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control (ImageSubresource2EXT)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (ImageSubresourceLayers)
import {-# SOURCE #-} Vulkan.Core10.ImageView (ImageSubresourceRange)
import {-# SOURCE #-} Vulkan.Core10.Enums.ImageTiling (ImageTiling)
import {-# SOURCE #-} Vulkan.Core10.Enums.ImageType (ImageType)
import {-# SOURCE #-} Vulkan.Core10.Enums.ImageUsageFlagBits (ImageUsageFlags)
import {-# SOURCE #-} Vulkan.Core10.Handles (ImageView)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_image_view_handle (ImageViewAddressPropertiesNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (ImageViewCaptureDescriptorDataInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.ImageView (ImageViewCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_NVX_image_view_handle (ImageViewHandleInfoNVX)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_fd (ImportFenceFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_fence_win32 (ImportFenceWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_fd (ImportSemaphoreFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (ImportSemaphoreWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_external_semaphore (ImportSemaphoreZirconHandleInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core10.Enums.IndexType (IndexType)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_device_generated_commands (IndirectCommandsLayoutCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.Handles (IndirectCommandsLayoutNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (InitializePerformanceApiInfoINTEL)
import {-# SOURCE #-} Vulkan.Core10.Handles (Instance_T)
import {-# SOURCE #-} Vulkan.Core10.LayerDiscovery (LayerProperties)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_line_rasterization (LineRasterizationModeEXT)
import {-# SOURCE #-} Vulkan.Core10.Enums.LogicOp (LogicOp)
import {-# SOURCE #-} Vulkan.Extensions.VK_MVK_macos_surface (MacOSSurfaceCreateInfoMVK)
import {-# SOURCE #-} Vulkan.Core10.Memory (MappedMemoryRange)
import {-# SOURCE #-} Vulkan.Core10.Memory (MemoryAllocateInfo)
import {-# SOURCE #-} Vulkan.Core10.OtherTypes (MemoryBarrier)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_fd (MemoryFdPropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer (MemoryGetAndroidHardwareBufferInfoANDROID)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_fd (MemoryGetFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_rdma (MemoryGetRemoteAddressInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (MemoryGetWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_external_memory (MemoryGetZirconHandleInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_external_memory_host (MemoryHostPointerPropertiesEXT)
import {-# SOURCE #-} Vulkan.Core10.Enums.MemoryMapFlags (MemoryMapFlags)
import {-# SOURCE #-} Vulkan.Core10.MemoryManagement (MemoryRequirements)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (MemoryRequirements2)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_memory_requirements2 (MemoryRequirements2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_memory_win32 (MemoryWin32HandlePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_external_memory (MemoryZirconHandlePropertiesFUCHSIA)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_surface (MetalSurfaceCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapBuildInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapBuildSizesInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.Handles (MicromapEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_opacity_micromap (MicromapVersionInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multi_draw (MultiDrawIndexedInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_multi_draw (MultiDrawInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (MultisamplePropertiesEXT)
import {-# SOURCE #-} Vulkan.Core10.Enums.ObjectType (ObjectType)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowExecuteInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowImageFormatInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowImageFormatPropertiesNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowSessionBindingPointNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_optical_flow (OpticalFlowSessionCreateInfoNV)
import {-# SOURCE #-} Vulkan.Extensions.Handles (OpticalFlowSessionNV)
import {-# SOURCE #-} Vulkan.Core10.FuncPointers (PFN_vkVoidFunction)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (PastPresentationTimingGOOGLE)
import {-# SOURCE #-} Vulkan.Core11.Enums.PeerMemoryFeatureFlagBits (PeerMemoryFeatureFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceConfigurationAcquireInfoINTEL)
import {-# SOURCE #-} Vulkan.Extensions.Handles (PerformanceConfigurationINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PerformanceCounterDescriptionKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (PerformanceCounterKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceMarkerInfoINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceOverrideInfoINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceParameterTypeINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceStreamMarkerInfoINTEL)
import {-# SOURCE #-} Vulkan.Extensions.VK_INTEL_performance_query (PerformanceValueINTEL)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities (PhysicalDeviceExternalBufferInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities (PhysicalDeviceExternalFenceInfo)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities (PhysicalDeviceExternalSemaphoreInfo)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceFeatures)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceFeatures2)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_fragment_shading_rate (PhysicalDeviceFragmentShadingRateKHR)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (PhysicalDeviceGroupProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceImageFormatInfo2)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceMemoryProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceMemoryProperties2)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (PhysicalDeviceProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceProperties2)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceSparseImageFormatInfo2)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (PhysicalDeviceSurfaceInfo2KHR)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_tooling_info (PhysicalDeviceToolProperties)
import {-# SOURCE #-} Vulkan.Core10.Handles (PhysicalDevice_T)
import {-# SOURCE #-} Vulkan.Core10.Handles (Pipeline)
import {-# SOURCE #-} Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint)
import {-# SOURCE #-} Vulkan.Core10.Handles (PipelineCache)
import {-# SOURCE #-} Vulkan.Core10.PipelineCache (PipelineCacheCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutableInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutableInternalRepresentationKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutablePropertiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineExecutableStatisticKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_properties (PipelineInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_pipeline_executable_properties (PipelineInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.Handles (PipelineLayout)
import {-# SOURCE #-} Vulkan.Core10.PipelineLayout (PipelineLayoutCreateInfo)
import {-# SOURCE #-} Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits)
import {-# SOURCE #-} Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import {-# SOURCE #-} Vulkan.Core13.Enums.PipelineStageFlags2 (PipelineStageFlags2)
import {-# SOURCE #-} Vulkan.Core10.Enums.PolygonMode (PolygonMode)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (PresentInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface (PresentModeKHR)
import {-# SOURCE #-} Vulkan.Core10.Enums.PrimitiveTopology (PrimitiveTopology)
import {-# SOURCE #-} Vulkan.Core13.Handles (PrivateDataSlot)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_private_data (PrivateDataSlotCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_provoking_vertex (ProvokingVertexModeEXT)
import {-# SOURCE #-} Vulkan.Core10.Enums.QueryControlFlagBits (QueryControlFlags)
import {-# SOURCE #-} Vulkan.Core10.Handles (QueryPool)
import {-# SOURCE #-} Vulkan.Core10.Query (QueryPoolCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_performance_query (QueryPoolPerformanceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Core10.Enums.QueryResultFlagBits (QueryResultFlags)
import {-# SOURCE #-} Vulkan.Core10.Enums.QueryType (QueryType)
import {-# SOURCE #-} Vulkan.Core10.DeviceInitialization (QueueFamilyProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (QueueFamilyProperties2)
import {-# SOURCE #-} Vulkan.Core10.Handles (Queue_T)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_acquire_xlib_display (RROutput)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingPipelineCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_ray_tracing (RayTracingPipelineCreateInfoNV)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (Rect2D)
import {-# SOURCE #-} Vulkan.Extensions.VK_GOOGLE_display_timing (RefreshCycleDurationGOOGLE)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_external_memory_rdma (RemoteAddressNV)
import {-# SOURCE #-} Vulkan.Core10.Handles (RenderPass)
import {-# SOURCE #-} Vulkan.Core10.CommandBufferBuilding (RenderPassBeginInfo)
import {-# SOURCE #-} Vulkan.Core10.Pass (RenderPassCreateInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (RenderPassCreateInfo2)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering (RenderingInfo)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2 (ResolveImageInfo2)
import {-# SOURCE #-} Vulkan.Core10.Enums.Result (Result)
import {-# SOURCE #-} Vulkan.Core10.Enums.SampleCountFlagBits (SampleCountFlagBits)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_sample_locations (SampleLocationsInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.FundamentalTypes (SampleMask)
import {-# SOURCE #-} Vulkan.Core10.Handles (Sampler)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_descriptor_buffer (SamplerCaptureDescriptorDataInfoEXT)
import {-# SOURCE #-} Vulkan.Core10.Sampler (SamplerCreateInfo)
import {-# SOURCE #-} Vulkan.Core11.Handles (SamplerYcbcrConversion)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion (SamplerYcbcrConversionCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_QNX_screen_surface (ScreenSurfaceCreateInfoQNX)
import {-# SOURCE #-} Vulkan.Extensions.VK_QNX_screen_surface (Screen_window)
import {-# SOURCE #-} Vulkan.Core10.Handles (Semaphore)
import {-# SOURCE #-} Vulkan.Core10.QueueSemaphore (SemaphoreCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_fd (SemaphoreGetFdInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_external_semaphore_win32 (SemaphoreGetWin32HandleInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_external_semaphore (SemaphoreGetZirconHandleInfoFUCHSIA)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreSignalInfo)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore (SemaphoreWaitInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (ShaderGroupShaderKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_AMD_shader_info (ShaderInfoTypeAMD)
import {-# SOURCE #-} Vulkan.Core10.Handles (ShaderModule)
import {-# SOURCE #-} Vulkan.Core10.Shader (ShaderModuleCreateInfo)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_shader_module_identifier (ShaderModuleIdentifierEXT)
import {-# SOURCE #-} Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits)
import {-# SOURCE #-} Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_shading_rate_image (ShadingRatePaletteNV)
import Vulkan.CStruct.Extends (SomeStruct)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (SparseImageFormatProperties)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (SparseImageFormatProperties2)
import {-# SOURCE #-} Vulkan.Core10.SparseResourceMemoryManagement (SparseImageMemoryRequirements)
import {-# SOURCE #-} Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2 (SparseImageMemoryRequirements2)
import {-# SOURCE #-} Vulkan.Core10.Enums.StencilFaceFlagBits (StencilFaceFlags)
import {-# SOURCE #-} Vulkan.Core10.Enums.StencilOp (StencilOp)
import {-# SOURCE #-} Vulkan.Extensions.VK_GGP_stream_descriptor_surface (StreamDescriptorSurfaceCreateInfoGGP)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (StridedDeviceAddressRegionKHR)
import {-# SOURCE #-} Vulkan.Core10.Queue (SubmitInfo)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_KHR_synchronization2 (SubmitInfo2)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassBeginInfo)
import {-# SOURCE #-} Vulkan.Core10.Enums.SubpassContents (SubpassContents)
import {-# SOURCE #-} Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2 (SubpassEndInfo)
import {-# SOURCE #-} Vulkan.Core10.Image (SubresourceLayout)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control (SubresourceLayout2EXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCapabilities2EXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (SurfaceCapabilities2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface (SurfaceCapabilitiesKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagBitsEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_get_surface_capabilities2 (SurfaceFormat2KHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_surface (SurfaceFormatKHR)
import {-# SOURCE #-} Vulkan.Extensions.Handles (SurfaceKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.Handles (SwapchainKHR)
import {-# SOURCE #-} Vulkan.Core11.Enums.TessellationDomainOrigin (TessellationDomainOrigin)
import {-# SOURCE #-} Vulkan.Extensions.VK_QCOM_tile_properties (TilePropertiesQCOM)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_calibrated_timestamps (TimeDomainEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_validation_cache (ValidationCacheCreateInfoEXT)
import {-# SOURCE #-} Vulkan.Extensions.Handles (ValidationCacheEXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state (VertexInputAttributeDescription2EXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state (VertexInputBindingDescription2EXT)
import {-# SOURCE #-} Vulkan.Extensions.VK_NN_vi_surface (ViSurfaceCreateInfoNN)
import {-# SOURCE #-} Vulkan.Core10.Pipeline (Viewport)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_viewport_swizzle (ViewportSwizzleNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_NV_clip_space_w_scaling (ViewportWScalingNV)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_xlib_surface (VisualID)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_wayland_surface (WaylandSurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_win32_surface (Win32SurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_wayland_surface (Wl_display)
import {-# SOURCE #-} Vulkan.Core10.DescriptorSet (WriteDescriptorSet)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_xcb_surface (XcbSurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_xcb_surface (Xcb_connection_t)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_xcb_surface (Xcb_visualid_t)
import {-# SOURCE #-} Vulkan.Extensions.VK_KHR_xlib_surface (XlibSurfaceCreateInfoKHR)
import {-# SOURCE #-} Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface (Zx_handle_t)
data InstanceCmds = InstanceCmds
  { instanceCmdsHandle :: Ptr Instance_T
  , pVkDestroyInstance :: FunPtr (Ptr Instance_T -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkEnumeratePhysicalDevices :: FunPtr (Ptr Instance_T -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr (Ptr PhysicalDevice_T)) -> IO Result)
  , pVkGetInstanceProcAddr :: FunPtr (Ptr Instance_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
  , pVkGetPhysicalDeviceProperties :: FunPtr (Ptr PhysicalDevice_T -> ("pProperties" ::: Ptr PhysicalDeviceProperties) -> IO ())
  , pVkGetPhysicalDeviceQueueFamilyProperties :: FunPtr (Ptr PhysicalDevice_T -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr QueueFamilyProperties) -> IO ())
  , pVkGetPhysicalDeviceMemoryProperties :: FunPtr (Ptr PhysicalDevice_T -> ("pMemoryProperties" ::: Ptr PhysicalDeviceMemoryProperties) -> IO ())
  , pVkGetPhysicalDeviceFeatures :: FunPtr (Ptr PhysicalDevice_T -> ("pFeatures" ::: Ptr PhysicalDeviceFeatures) -> IO ())
  , pVkGetPhysicalDeviceFormatProperties :: FunPtr (Ptr PhysicalDevice_T -> Format -> ("pFormatProperties" ::: Ptr FormatProperties) -> IO ())
  , pVkGetPhysicalDeviceImageFormatProperties :: FunPtr (Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ("pImageFormatProperties" ::: Ptr ImageFormatProperties) -> IO Result)
  , pVkCreateDevice :: FunPtr (Ptr PhysicalDevice_T -> ("pCreateInfo" ::: Ptr (SomeStruct DeviceCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pDevice" ::: Ptr (Ptr Device_T)) -> IO Result)
  , pVkEnumerateDeviceLayerProperties :: FunPtr (Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr LayerProperties) -> IO Result)
  , pVkEnumerateDeviceExtensionProperties :: FunPtr (Ptr PhysicalDevice_T -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr ExtensionProperties) -> IO Result)
  , pVkGetPhysicalDeviceSparseImageFormatProperties :: FunPtr (Ptr PhysicalDevice_T -> Format -> ImageType -> ("samples" ::: SampleCountFlagBits) -> ImageUsageFlags -> ImageTiling -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr SparseImageFormatProperties) -> IO ())
  , pVkCreateAndroidSurfaceKHR :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr AndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkGetPhysicalDeviceDisplayPropertiesKHR :: FunPtr (Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayPropertiesKHR) -> IO Result)
  , pVkGetPhysicalDeviceDisplayPlanePropertiesKHR :: FunPtr (Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayPlanePropertiesKHR) -> IO Result)
  , pVkGetDisplayPlaneSupportedDisplaysKHR :: FunPtr (Ptr PhysicalDevice_T -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr DisplayKHR) -> IO Result)
  , pVkGetDisplayModePropertiesKHR :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayModePropertiesKHR) -> IO Result)
  , pVkCreateDisplayModeKHR :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> ("pCreateInfo" ::: Ptr DisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pMode" ::: Ptr DisplayModeKHR) -> IO Result)
  , pVkGetDisplayPlaneCapabilitiesKHR :: FunPtr (Ptr PhysicalDevice_T -> DisplayModeKHR -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr DisplayPlaneCapabilitiesKHR) -> IO Result)
  , pVkCreateDisplayPlaneSurfaceKHR :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr DisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkDestroySurfaceKHR :: FunPtr (Ptr Instance_T -> SurfaceKHR -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetPhysicalDeviceSurfaceSupportKHR :: FunPtr (Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> SurfaceKHR -> ("pSupported" ::: Ptr Bool32) -> IO Result)
  , pVkGetPhysicalDeviceSurfaceCapabilitiesKHR :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> ("pSurfaceCapabilities" ::: Ptr SurfaceCapabilitiesKHR) -> IO Result)
  , pVkGetPhysicalDeviceSurfaceFormatsKHR :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr SurfaceFormatKHR) -> IO Result)
  , pVkGetPhysicalDeviceSurfacePresentModesKHR :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr PresentModeKHR) -> IO Result)
  , pVkCreateViSurfaceNN :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr ViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkCreateWaylandSurfaceKHR :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr WaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkGetPhysicalDeviceWaylandPresentationSupportKHR :: FunPtr (Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> Ptr Wl_display -> IO Bool32)
  , pVkCreateWin32SurfaceKHR :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr Win32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkGetPhysicalDeviceWin32PresentationSupportKHR :: FunPtr (Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> IO Bool32)
  , pVkCreateXlibSurfaceKHR :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr XlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkGetPhysicalDeviceXlibPresentationSupportKHR :: FunPtr (Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> VisualID -> IO Bool32)
  , pVkCreateXcbSurfaceKHR :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr XcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkGetPhysicalDeviceXcbPresentationSupportKHR :: FunPtr (Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> Ptr Xcb_connection_t -> ("visual_id" ::: Xcb_visualid_t) -> IO Bool32)
  , pVkCreateDirectFBSurfaceEXT :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr DirectFBSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkGetPhysicalDeviceDirectFBPresentationSupportEXT :: FunPtr (Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> ("dfb" ::: Ptr IDirectFB) -> IO Bool32)
  , pVkCreateImagePipeSurfaceFUCHSIA :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr ImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkCreateStreamDescriptorSurfaceGGP :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr StreamDescriptorSurfaceCreateInfoGGP) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkCreateScreenSurfaceQNX :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr ScreenSurfaceCreateInfoQNX) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkGetPhysicalDeviceScreenPresentationSupportQNX :: FunPtr (Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> Ptr Screen_window -> IO Bool32)
  , pVkCreateDebugReportCallbackEXT :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr DebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pCallback" ::: Ptr DebugReportCallbackEXT) -> IO Result)
  , pVkDestroyDebugReportCallbackEXT :: FunPtr (Ptr Instance_T -> DebugReportCallbackEXT -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkDebugReportMessageEXT :: FunPtr (Ptr Instance_T -> DebugReportFlagsEXT -> DebugReportObjectTypeEXT -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ())
  , pVkGetPhysicalDeviceExternalImageFormatPropertiesNV :: FunPtr (Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ("externalHandleType" ::: ExternalMemoryHandleTypeFlagsNV) -> ("pExternalImageFormatProperties" ::: Ptr ExternalImageFormatPropertiesNV) -> IO Result)
  , pVkGetPhysicalDeviceFeatures2 :: FunPtr (Ptr PhysicalDevice_T -> ("pFeatures" ::: Ptr (SomeStruct PhysicalDeviceFeatures2)) -> IO ())
  , pVkGetPhysicalDeviceProperties2 :: FunPtr (Ptr PhysicalDevice_T -> ("pProperties" ::: Ptr (SomeStruct PhysicalDeviceProperties2)) -> IO ())
  , pVkGetPhysicalDeviceFormatProperties2 :: FunPtr (Ptr PhysicalDevice_T -> Format -> ("pFormatProperties" ::: Ptr (SomeStruct FormatProperties2)) -> IO ())
  , pVkGetPhysicalDeviceImageFormatProperties2 :: FunPtr (Ptr PhysicalDevice_T -> ("pImageFormatInfo" ::: Ptr (SomeStruct PhysicalDeviceImageFormatInfo2)) -> ("pImageFormatProperties" ::: Ptr (SomeStruct ImageFormatProperties2)) -> IO Result)
  , pVkGetPhysicalDeviceQueueFamilyProperties2 :: FunPtr (Ptr PhysicalDevice_T -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr (SomeStruct QueueFamilyProperties2)) -> IO ())
  , pVkGetPhysicalDeviceMemoryProperties2 :: FunPtr (Ptr PhysicalDevice_T -> ("pMemoryProperties" ::: Ptr (SomeStruct PhysicalDeviceMemoryProperties2)) -> IO ())
  , pVkGetPhysicalDeviceSparseImageFormatProperties2 :: FunPtr (Ptr PhysicalDevice_T -> ("pFormatInfo" ::: Ptr PhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr SparseImageFormatProperties2) -> IO ())
  , pVkGetPhysicalDeviceExternalBufferProperties :: FunPtr (Ptr PhysicalDevice_T -> ("pExternalBufferInfo" ::: Ptr PhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr ExternalBufferProperties) -> IO ())
  , pVkGetPhysicalDeviceExternalSemaphoreProperties :: FunPtr (Ptr PhysicalDevice_T -> ("pExternalSemaphoreInfo" ::: Ptr (SomeStruct PhysicalDeviceExternalSemaphoreInfo)) -> ("pExternalSemaphoreProperties" ::: Ptr ExternalSemaphoreProperties) -> IO ())
  , pVkGetPhysicalDeviceExternalFenceProperties :: FunPtr (Ptr PhysicalDevice_T -> ("pExternalFenceInfo" ::: Ptr PhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr ExternalFenceProperties) -> IO ())
  , pVkReleaseDisplayEXT :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> IO Result)
  , pVkAcquireXlibDisplayEXT :: FunPtr (Ptr PhysicalDevice_T -> ("dpy" ::: Ptr Display) -> DisplayKHR -> IO Result)
  , pVkGetRandROutputDisplayEXT :: FunPtr (Ptr PhysicalDevice_T -> ("dpy" ::: Ptr Display) -> RROutput -> ("pDisplay" ::: Ptr DisplayKHR) -> IO Result)
  , pVkAcquireWinrtDisplayNV :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> IO Result)
  , pVkGetWinrtDisplayNV :: FunPtr (Ptr PhysicalDevice_T -> ("deviceRelativeId" ::: Word32) -> ("pDisplay" ::: Ptr DisplayKHR) -> IO Result)
  , pVkGetPhysicalDeviceSurfaceCapabilities2EXT :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> ("pSurfaceCapabilities" ::: Ptr SurfaceCapabilities2EXT) -> IO Result)
  , pVkEnumeratePhysicalDeviceGroups :: FunPtr (Ptr Instance_T -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr PhysicalDeviceGroupProperties) -> IO Result)
  , pVkGetPhysicalDevicePresentRectanglesKHR :: FunPtr (Ptr PhysicalDevice_T -> SurfaceKHR -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr Rect2D) -> IO Result)
  , pVkCreateIOSSurfaceMVK :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr IOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkCreateMacOSSurfaceMVK :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr MacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkCreateMetalSurfaceEXT :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr MetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkGetPhysicalDeviceMultisamplePropertiesEXT :: FunPtr (Ptr PhysicalDevice_T -> ("samples" ::: SampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr MultisamplePropertiesEXT) -> IO ())
  , pVkGetPhysicalDeviceSurfaceCapabilities2KHR :: FunPtr (Ptr PhysicalDevice_T -> ("pSurfaceInfo" ::: Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR)) -> ("pSurfaceCapabilities" ::: Ptr (SomeStruct SurfaceCapabilities2KHR)) -> IO Result)
  , pVkGetPhysicalDeviceSurfaceFormats2KHR :: FunPtr (Ptr PhysicalDevice_T -> ("pSurfaceInfo" ::: Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR)) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr (SomeStruct SurfaceFormat2KHR)) -> IO Result)
  , pVkGetPhysicalDeviceDisplayProperties2KHR :: FunPtr (Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayProperties2KHR) -> IO Result)
  , pVkGetPhysicalDeviceDisplayPlaneProperties2KHR :: FunPtr (Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayPlaneProperties2KHR) -> IO Result)
  , pVkGetDisplayModeProperties2KHR :: FunPtr (Ptr PhysicalDevice_T -> DisplayKHR -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayModeProperties2KHR) -> IO Result)
  , pVkGetDisplayPlaneCapabilities2KHR :: FunPtr (Ptr PhysicalDevice_T -> ("pDisplayPlaneInfo" ::: Ptr DisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr DisplayPlaneCapabilities2KHR) -> IO Result)
  , pVkGetPhysicalDeviceCalibrateableTimeDomainsEXT :: FunPtr (Ptr PhysicalDevice_T -> ("pTimeDomainCount" ::: Ptr Word32) -> ("pTimeDomains" ::: Ptr TimeDomainEXT) -> IO Result)
  , pVkCreateDebugUtilsMessengerEXT :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr DebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pMessenger" ::: Ptr DebugUtilsMessengerEXT) -> IO Result)
  , pVkDestroyDebugUtilsMessengerEXT :: FunPtr (Ptr Instance_T -> DebugUtilsMessengerEXT -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkSubmitDebugUtilsMessageEXT :: FunPtr (Ptr Instance_T -> DebugUtilsMessageSeverityFlagBitsEXT -> ("messageTypes" ::: DebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr (SomeStruct DebugUtilsMessengerCallbackDataEXT)) -> IO ())
  , pVkGetPhysicalDeviceCooperativeMatrixPropertiesNV :: FunPtr (Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr CooperativeMatrixPropertiesNV) -> IO Result)
  , pVkGetPhysicalDeviceSurfacePresentModes2EXT :: FunPtr (Ptr PhysicalDevice_T -> ("pSurfaceInfo" ::: Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR)) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr PresentModeKHR) -> IO Result)
  , pVkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR :: FunPtr (Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> ("pCounterCount" ::: Ptr Word32) -> ("pCounters" ::: Ptr PerformanceCounterKHR) -> ("pCounterDescriptions" ::: Ptr PerformanceCounterDescriptionKHR) -> IO Result)
  , pVkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR :: FunPtr (Ptr PhysicalDevice_T -> ("pPerformanceQueryCreateInfo" ::: Ptr QueryPoolPerformanceCreateInfoKHR) -> ("pNumPasses" ::: Ptr Word32) -> IO ())
  , pVkCreateHeadlessSurfaceEXT :: FunPtr (Ptr Instance_T -> ("pCreateInfo" ::: Ptr HeadlessSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result)
  , pVkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV :: FunPtr (Ptr PhysicalDevice_T -> ("pCombinationCount" ::: Ptr Word32) -> ("pCombinations" ::: Ptr FramebufferMixedSamplesCombinationNV) -> IO Result)
  , pVkGetPhysicalDeviceToolProperties :: FunPtr (Ptr PhysicalDevice_T -> ("pToolCount" ::: Ptr Word32) -> ("pToolProperties" ::: Ptr PhysicalDeviceToolProperties) -> IO Result)
  , pVkGetPhysicalDeviceFragmentShadingRatesKHR :: FunPtr (Ptr PhysicalDevice_T -> ("pFragmentShadingRateCount" ::: Ptr Word32) -> ("pFragmentShadingRates" ::: Ptr PhysicalDeviceFragmentShadingRateKHR) -> IO Result)
  , pVkAcquireDrmDisplayEXT :: FunPtr (Ptr PhysicalDevice_T -> ("drmFd" ::: Int32) -> DisplayKHR -> IO Result)
  , pVkGetDrmDisplayEXT :: FunPtr (Ptr PhysicalDevice_T -> ("drmFd" ::: Int32) -> ("connectorId" ::: Word32) -> Ptr DisplayKHR -> IO Result)
  , pVkGetPhysicalDeviceOpticalFlowImageFormatsNV :: FunPtr (Ptr PhysicalDevice_T -> ("pOpticalFlowImageFormatInfo" ::: Ptr OpticalFlowImageFormatInfoNV) -> ("pFormatCount" ::: Ptr Word32) -> ("pImageFormatProperties" ::: Ptr OpticalFlowImageFormatPropertiesNV) -> IO Result)
  }

deriving instance Eq InstanceCmds
deriving instance Show InstanceCmds
instance Zero InstanceCmds where
  zero = InstanceCmds
    nullPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr

-- | A version of 'getInstanceProcAddr' which can be called
-- with a null pointer for the instance.
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetInstanceProcAddr" getInstanceProcAddr' :: Ptr Instance_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction

initInstanceCmds :: Ptr Instance_T -> IO InstanceCmds
initInstanceCmds handle = do
  let getFirstInstanceProcAddr = \case
        []   -> pure nullFunPtr
        x:xs -> do
          p <- getInstanceProcAddr' handle x
          if p /= nullFunPtr
            then pure p
            else getFirstInstanceProcAddr xs
  vkDestroyInstance <- getInstanceProcAddr' handle (Ptr "vkDestroyInstance"#)
  vkEnumeratePhysicalDevices <- getInstanceProcAddr' handle (Ptr "vkEnumeratePhysicalDevices"#)
  vkGetInstanceProcAddr <- getInstanceProcAddr' handle (Ptr "vkGetInstanceProcAddr"#)
  vkGetPhysicalDeviceProperties <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceProperties"#)
  vkGetPhysicalDeviceQueueFamilyProperties <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceQueueFamilyProperties"#)
  vkGetPhysicalDeviceMemoryProperties <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceMemoryProperties"#)
  vkGetPhysicalDeviceFeatures <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceFeatures"#)
  vkGetPhysicalDeviceFormatProperties <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceFormatProperties"#)
  vkGetPhysicalDeviceImageFormatProperties <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceImageFormatProperties"#)
  vkCreateDevice <- getInstanceProcAddr' handle (Ptr "vkCreateDevice"#)
  vkEnumerateDeviceLayerProperties <- getInstanceProcAddr' handle (Ptr "vkEnumerateDeviceLayerProperties"#)
  vkEnumerateDeviceExtensionProperties <- getInstanceProcAddr' handle (Ptr "vkEnumerateDeviceExtensionProperties"#)
  vkGetPhysicalDeviceSparseImageFormatProperties <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceSparseImageFormatProperties"#)
  vkCreateAndroidSurfaceKHR <- getInstanceProcAddr' handle (Ptr "vkCreateAndroidSurfaceKHR"#)
  vkGetPhysicalDeviceDisplayPropertiesKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceDisplayPropertiesKHR"#)
  vkGetPhysicalDeviceDisplayPlanePropertiesKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceDisplayPlanePropertiesKHR"#)
  vkGetDisplayPlaneSupportedDisplaysKHR <- getInstanceProcAddr' handle (Ptr "vkGetDisplayPlaneSupportedDisplaysKHR"#)
  vkGetDisplayModePropertiesKHR <- getInstanceProcAddr' handle (Ptr "vkGetDisplayModePropertiesKHR"#)
  vkCreateDisplayModeKHR <- getInstanceProcAddr' handle (Ptr "vkCreateDisplayModeKHR"#)
  vkGetDisplayPlaneCapabilitiesKHR <- getInstanceProcAddr' handle (Ptr "vkGetDisplayPlaneCapabilitiesKHR"#)
  vkCreateDisplayPlaneSurfaceKHR <- getInstanceProcAddr' handle (Ptr "vkCreateDisplayPlaneSurfaceKHR"#)
  vkDestroySurfaceKHR <- getInstanceProcAddr' handle (Ptr "vkDestroySurfaceKHR"#)
  vkGetPhysicalDeviceSurfaceSupportKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceSurfaceSupportKHR"#)
  vkGetPhysicalDeviceSurfaceCapabilitiesKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceSurfaceCapabilitiesKHR"#)
  vkGetPhysicalDeviceSurfaceFormatsKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceSurfaceFormatsKHR"#)
  vkGetPhysicalDeviceSurfacePresentModesKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceSurfacePresentModesKHR"#)
  vkCreateViSurfaceNN <- getInstanceProcAddr' handle (Ptr "vkCreateViSurfaceNN"#)
  vkCreateWaylandSurfaceKHR <- getInstanceProcAddr' handle (Ptr "vkCreateWaylandSurfaceKHR"#)
  vkGetPhysicalDeviceWaylandPresentationSupportKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceWaylandPresentationSupportKHR"#)
  vkCreateWin32SurfaceKHR <- getInstanceProcAddr' handle (Ptr "vkCreateWin32SurfaceKHR"#)
  vkGetPhysicalDeviceWin32PresentationSupportKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceWin32PresentationSupportKHR"#)
  vkCreateXlibSurfaceKHR <- getInstanceProcAddr' handle (Ptr "vkCreateXlibSurfaceKHR"#)
  vkGetPhysicalDeviceXlibPresentationSupportKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceXlibPresentationSupportKHR"#)
  vkCreateXcbSurfaceKHR <- getInstanceProcAddr' handle (Ptr "vkCreateXcbSurfaceKHR"#)
  vkGetPhysicalDeviceXcbPresentationSupportKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceXcbPresentationSupportKHR"#)
  vkCreateDirectFBSurfaceEXT <- getInstanceProcAddr' handle (Ptr "vkCreateDirectFBSurfaceEXT"#)
  vkGetPhysicalDeviceDirectFBPresentationSupportEXT <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceDirectFBPresentationSupportEXT"#)
  vkCreateImagePipeSurfaceFUCHSIA <- getInstanceProcAddr' handle (Ptr "vkCreateImagePipeSurfaceFUCHSIA"#)
  vkCreateStreamDescriptorSurfaceGGP <- getInstanceProcAddr' handle (Ptr "vkCreateStreamDescriptorSurfaceGGP"#)
  vkCreateScreenSurfaceQNX <- getInstanceProcAddr' handle (Ptr "vkCreateScreenSurfaceQNX"#)
  vkGetPhysicalDeviceScreenPresentationSupportQNX <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceScreenPresentationSupportQNX"#)
  vkCreateDebugReportCallbackEXT <- getInstanceProcAddr' handle (Ptr "vkCreateDebugReportCallbackEXT"#)
  vkDestroyDebugReportCallbackEXT <- getInstanceProcAddr' handle (Ptr "vkDestroyDebugReportCallbackEXT"#)
  vkDebugReportMessageEXT <- getInstanceProcAddr' handle (Ptr "vkDebugReportMessageEXT"#)
  vkGetPhysicalDeviceExternalImageFormatPropertiesNV <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceExternalImageFormatPropertiesNV"#)
  vkGetPhysicalDeviceFeatures2 <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceFeatures2KHR"#)
                                                           , (Ptr "vkGetPhysicalDeviceFeatures2"#) ]
  vkGetPhysicalDeviceProperties2 <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceProperties2KHR"#)
                                                             , (Ptr "vkGetPhysicalDeviceProperties2"#) ]
  vkGetPhysicalDeviceFormatProperties2 <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceFormatProperties2KHR"#)
                                                                   , (Ptr "vkGetPhysicalDeviceFormatProperties2"#) ]
  vkGetPhysicalDeviceImageFormatProperties2 <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceImageFormatProperties2KHR"#)
                                                                        , (Ptr "vkGetPhysicalDeviceImageFormatProperties2"#) ]
  vkGetPhysicalDeviceQueueFamilyProperties2 <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceQueueFamilyProperties2KHR"#)
                                                                        , (Ptr "vkGetPhysicalDeviceQueueFamilyProperties2"#) ]
  vkGetPhysicalDeviceMemoryProperties2 <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceMemoryProperties2KHR"#)
                                                                   , (Ptr "vkGetPhysicalDeviceMemoryProperties2"#) ]
  vkGetPhysicalDeviceSparseImageFormatProperties2 <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"#)
                                                                              , (Ptr "vkGetPhysicalDeviceSparseImageFormatProperties2"#) ]
  vkGetPhysicalDeviceExternalBufferProperties <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceExternalBufferPropertiesKHR"#)
                                                                          , (Ptr "vkGetPhysicalDeviceExternalBufferProperties"#) ]
  vkGetPhysicalDeviceExternalSemaphoreProperties <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceExternalSemaphorePropertiesKHR"#)
                                                                             , (Ptr "vkGetPhysicalDeviceExternalSemaphoreProperties"#) ]
  vkGetPhysicalDeviceExternalFenceProperties <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceExternalFencePropertiesKHR"#)
                                                                         , (Ptr "vkGetPhysicalDeviceExternalFenceProperties"#) ]
  vkReleaseDisplayEXT <- getInstanceProcAddr' handle (Ptr "vkReleaseDisplayEXT"#)
  vkAcquireXlibDisplayEXT <- getInstanceProcAddr' handle (Ptr "vkAcquireXlibDisplayEXT"#)
  vkGetRandROutputDisplayEXT <- getInstanceProcAddr' handle (Ptr "vkGetRandROutputDisplayEXT"#)
  vkAcquireWinrtDisplayNV <- getInstanceProcAddr' handle (Ptr "vkAcquireWinrtDisplayNV"#)
  vkGetWinrtDisplayNV <- getInstanceProcAddr' handle (Ptr "vkGetWinrtDisplayNV"#)
  vkGetPhysicalDeviceSurfaceCapabilities2EXT <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceSurfaceCapabilities2EXT"#)
  vkEnumeratePhysicalDeviceGroups <- getFirstInstanceProcAddr [ (Ptr "vkEnumeratePhysicalDeviceGroupsKHR"#)
                                                              , (Ptr "vkEnumeratePhysicalDeviceGroups"#) ]
  vkGetPhysicalDevicePresentRectanglesKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDevicePresentRectanglesKHR"#)
  vkCreateIOSSurfaceMVK <- getInstanceProcAddr' handle (Ptr "vkCreateIOSSurfaceMVK"#)
  vkCreateMacOSSurfaceMVK <- getInstanceProcAddr' handle (Ptr "vkCreateMacOSSurfaceMVK"#)
  vkCreateMetalSurfaceEXT <- getInstanceProcAddr' handle (Ptr "vkCreateMetalSurfaceEXT"#)
  vkGetPhysicalDeviceMultisamplePropertiesEXT <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceMultisamplePropertiesEXT"#)
  vkGetPhysicalDeviceSurfaceCapabilities2KHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceSurfaceCapabilities2KHR"#)
  vkGetPhysicalDeviceSurfaceFormats2KHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceSurfaceFormats2KHR"#)
  vkGetPhysicalDeviceDisplayProperties2KHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceDisplayProperties2KHR"#)
  vkGetPhysicalDeviceDisplayPlaneProperties2KHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceDisplayPlaneProperties2KHR"#)
  vkGetDisplayModeProperties2KHR <- getInstanceProcAddr' handle (Ptr "vkGetDisplayModeProperties2KHR"#)
  vkGetDisplayPlaneCapabilities2KHR <- getInstanceProcAddr' handle (Ptr "vkGetDisplayPlaneCapabilities2KHR"#)
  vkGetPhysicalDeviceCalibrateableTimeDomainsEXT <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT"#)
  vkCreateDebugUtilsMessengerEXT <- getInstanceProcAddr' handle (Ptr "vkCreateDebugUtilsMessengerEXT"#)
  vkDestroyDebugUtilsMessengerEXT <- getInstanceProcAddr' handle (Ptr "vkDestroyDebugUtilsMessengerEXT"#)
  vkSubmitDebugUtilsMessageEXT <- getInstanceProcAddr' handle (Ptr "vkSubmitDebugUtilsMessageEXT"#)
  vkGetPhysicalDeviceCooperativeMatrixPropertiesNV <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV"#)
  vkGetPhysicalDeviceSurfacePresentModes2EXT <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceSurfacePresentModes2EXT"#)
  vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR <- getInstanceProcAddr' handle (Ptr "vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR"#)
  vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR"#)
  vkCreateHeadlessSurfaceEXT <- getInstanceProcAddr' handle (Ptr "vkCreateHeadlessSurfaceEXT"#)
  vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV"#)
  vkGetPhysicalDeviceToolProperties <- getFirstInstanceProcAddr [ (Ptr "vkGetPhysicalDeviceToolPropertiesEXT"#)
                                                                , (Ptr "vkGetPhysicalDeviceToolProperties"#) ]
  vkGetPhysicalDeviceFragmentShadingRatesKHR <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceFragmentShadingRatesKHR"#)
  vkAcquireDrmDisplayEXT <- getInstanceProcAddr' handle (Ptr "vkAcquireDrmDisplayEXT"#)
  vkGetDrmDisplayEXT <- getInstanceProcAddr' handle (Ptr "vkGetDrmDisplayEXT"#)
  vkGetPhysicalDeviceOpticalFlowImageFormatsNV <- getInstanceProcAddr' handle (Ptr "vkGetPhysicalDeviceOpticalFlowImageFormatsNV"#)
  pure $ InstanceCmds handle
    (castFunPtr @_ @(Ptr Instance_T -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyInstance)
    (castFunPtr @_ @(Ptr Instance_T -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr (Ptr PhysicalDevice_T)) -> IO Result) vkEnumeratePhysicalDevices)
    (castFunPtr @_ @(Ptr Instance_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) vkGetInstanceProcAddr)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pProperties" ::: Ptr PhysicalDeviceProperties) -> IO ()) vkGetPhysicalDeviceProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr QueueFamilyProperties) -> IO ()) vkGetPhysicalDeviceQueueFamilyProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pMemoryProperties" ::: Ptr PhysicalDeviceMemoryProperties) -> IO ()) vkGetPhysicalDeviceMemoryProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pFeatures" ::: Ptr PhysicalDeviceFeatures) -> IO ()) vkGetPhysicalDeviceFeatures)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> Format -> ("pFormatProperties" ::: Ptr FormatProperties) -> IO ()) vkGetPhysicalDeviceFormatProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ("pImageFormatProperties" ::: Ptr ImageFormatProperties) -> IO Result) vkGetPhysicalDeviceImageFormatProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pCreateInfo" ::: Ptr (SomeStruct DeviceCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pDevice" ::: Ptr (Ptr Device_T)) -> IO Result) vkCreateDevice)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr LayerProperties) -> IO Result) vkEnumerateDeviceLayerProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr ExtensionProperties) -> IO Result) vkEnumerateDeviceExtensionProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> Format -> ImageType -> ("samples" ::: SampleCountFlagBits) -> ImageUsageFlags -> ImageTiling -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr SparseImageFormatProperties) -> IO ()) vkGetPhysicalDeviceSparseImageFormatProperties)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr AndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateAndroidSurfaceKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayPropertiesKHR) -> IO Result) vkGetPhysicalDeviceDisplayPropertiesKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayPlanePropertiesKHR) -> IO Result) vkGetPhysicalDeviceDisplayPlanePropertiesKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr DisplayKHR) -> IO Result) vkGetDisplayPlaneSupportedDisplaysKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> DisplayKHR -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayModePropertiesKHR) -> IO Result) vkGetDisplayModePropertiesKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> DisplayKHR -> ("pCreateInfo" ::: Ptr DisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pMode" ::: Ptr DisplayModeKHR) -> IO Result) vkCreateDisplayModeKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> DisplayModeKHR -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr DisplayPlaneCapabilitiesKHR) -> IO Result) vkGetDisplayPlaneCapabilitiesKHR)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr DisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateDisplayPlaneSurfaceKHR)
    (castFunPtr @_ @(Ptr Instance_T -> SurfaceKHR -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroySurfaceKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> SurfaceKHR -> ("pSupported" ::: Ptr Bool32) -> IO Result) vkGetPhysicalDeviceSurfaceSupportKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> SurfaceKHR -> ("pSurfaceCapabilities" ::: Ptr SurfaceCapabilitiesKHR) -> IO Result) vkGetPhysicalDeviceSurfaceCapabilitiesKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> SurfaceKHR -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr SurfaceFormatKHR) -> IO Result) vkGetPhysicalDeviceSurfaceFormatsKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> SurfaceKHR -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr PresentModeKHR) -> IO Result) vkGetPhysicalDeviceSurfacePresentModesKHR)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr ViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateViSurfaceNN)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr WaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateWaylandSurfaceKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> Ptr Wl_display -> IO Bool32) vkGetPhysicalDeviceWaylandPresentationSupportKHR)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr Win32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateWin32SurfaceKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> IO Bool32) vkGetPhysicalDeviceWin32PresentationSupportKHR)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr XlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateXlibSurfaceKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> VisualID -> IO Bool32) vkGetPhysicalDeviceXlibPresentationSupportKHR)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr XcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateXcbSurfaceKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> Ptr Xcb_connection_t -> ("visual_id" ::: Xcb_visualid_t) -> IO Bool32) vkGetPhysicalDeviceXcbPresentationSupportKHR)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr DirectFBSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateDirectFBSurfaceEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> ("dfb" ::: Ptr IDirectFB) -> IO Bool32) vkGetPhysicalDeviceDirectFBPresentationSupportEXT)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr ImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateImagePipeSurfaceFUCHSIA)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr StreamDescriptorSurfaceCreateInfoGGP) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateStreamDescriptorSurfaceGGP)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr ScreenSurfaceCreateInfoQNX) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateScreenSurfaceQNX)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> Ptr Screen_window -> IO Bool32) vkGetPhysicalDeviceScreenPresentationSupportQNX)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr DebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pCallback" ::: Ptr DebugReportCallbackEXT) -> IO Result) vkCreateDebugReportCallbackEXT)
    (castFunPtr @_ @(Ptr Instance_T -> DebugReportCallbackEXT -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyDebugReportCallbackEXT)
    (castFunPtr @_ @(Ptr Instance_T -> DebugReportFlagsEXT -> DebugReportObjectTypeEXT -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ()) vkDebugReportMessageEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> Format -> ImageType -> ImageTiling -> ImageUsageFlags -> ImageCreateFlags -> ("externalHandleType" ::: ExternalMemoryHandleTypeFlagsNV) -> ("pExternalImageFormatProperties" ::: Ptr ExternalImageFormatPropertiesNV) -> IO Result) vkGetPhysicalDeviceExternalImageFormatPropertiesNV)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pFeatures" ::: Ptr (SomeStruct PhysicalDeviceFeatures2)) -> IO ()) vkGetPhysicalDeviceFeatures2)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pProperties" ::: Ptr (SomeStruct PhysicalDeviceProperties2)) -> IO ()) vkGetPhysicalDeviceProperties2)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> Format -> ("pFormatProperties" ::: Ptr (SomeStruct FormatProperties2)) -> IO ()) vkGetPhysicalDeviceFormatProperties2)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pImageFormatInfo" ::: Ptr (SomeStruct PhysicalDeviceImageFormatInfo2)) -> ("pImageFormatProperties" ::: Ptr (SomeStruct ImageFormatProperties2)) -> IO Result) vkGetPhysicalDeviceImageFormatProperties2)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr (SomeStruct QueueFamilyProperties2)) -> IO ()) vkGetPhysicalDeviceQueueFamilyProperties2)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pMemoryProperties" ::: Ptr (SomeStruct PhysicalDeviceMemoryProperties2)) -> IO ()) vkGetPhysicalDeviceMemoryProperties2)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pFormatInfo" ::: Ptr PhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr SparseImageFormatProperties2) -> IO ()) vkGetPhysicalDeviceSparseImageFormatProperties2)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pExternalBufferInfo" ::: Ptr PhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr ExternalBufferProperties) -> IO ()) vkGetPhysicalDeviceExternalBufferProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pExternalSemaphoreInfo" ::: Ptr (SomeStruct PhysicalDeviceExternalSemaphoreInfo)) -> ("pExternalSemaphoreProperties" ::: Ptr ExternalSemaphoreProperties) -> IO ()) vkGetPhysicalDeviceExternalSemaphoreProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pExternalFenceInfo" ::: Ptr PhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr ExternalFenceProperties) -> IO ()) vkGetPhysicalDeviceExternalFenceProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> DisplayKHR -> IO Result) vkReleaseDisplayEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("dpy" ::: Ptr Display) -> DisplayKHR -> IO Result) vkAcquireXlibDisplayEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("dpy" ::: Ptr Display) -> RROutput -> ("pDisplay" ::: Ptr DisplayKHR) -> IO Result) vkGetRandROutputDisplayEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> DisplayKHR -> IO Result) vkAcquireWinrtDisplayNV)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("deviceRelativeId" ::: Word32) -> ("pDisplay" ::: Ptr DisplayKHR) -> IO Result) vkGetWinrtDisplayNV)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> SurfaceKHR -> ("pSurfaceCapabilities" ::: Ptr SurfaceCapabilities2EXT) -> IO Result) vkGetPhysicalDeviceSurfaceCapabilities2EXT)
    (castFunPtr @_ @(Ptr Instance_T -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr PhysicalDeviceGroupProperties) -> IO Result) vkEnumeratePhysicalDeviceGroups)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> SurfaceKHR -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr Rect2D) -> IO Result) vkGetPhysicalDevicePresentRectanglesKHR)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr IOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateIOSSurfaceMVK)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr MacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateMacOSSurfaceMVK)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr MetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateMetalSurfaceEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("samples" ::: SampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr MultisamplePropertiesEXT) -> IO ()) vkGetPhysicalDeviceMultisamplePropertiesEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pSurfaceInfo" ::: Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR)) -> ("pSurfaceCapabilities" ::: Ptr (SomeStruct SurfaceCapabilities2KHR)) -> IO Result) vkGetPhysicalDeviceSurfaceCapabilities2KHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pSurfaceInfo" ::: Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR)) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr (SomeStruct SurfaceFormat2KHR)) -> IO Result) vkGetPhysicalDeviceSurfaceFormats2KHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayProperties2KHR) -> IO Result) vkGetPhysicalDeviceDisplayProperties2KHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayPlaneProperties2KHR) -> IO Result) vkGetPhysicalDeviceDisplayPlaneProperties2KHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> DisplayKHR -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr DisplayModeProperties2KHR) -> IO Result) vkGetDisplayModeProperties2KHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pDisplayPlaneInfo" ::: Ptr DisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr DisplayPlaneCapabilities2KHR) -> IO Result) vkGetDisplayPlaneCapabilities2KHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pTimeDomainCount" ::: Ptr Word32) -> ("pTimeDomains" ::: Ptr TimeDomainEXT) -> IO Result) vkGetPhysicalDeviceCalibrateableTimeDomainsEXT)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr DebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pMessenger" ::: Ptr DebugUtilsMessengerEXT) -> IO Result) vkCreateDebugUtilsMessengerEXT)
    (castFunPtr @_ @(Ptr Instance_T -> DebugUtilsMessengerEXT -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyDebugUtilsMessengerEXT)
    (castFunPtr @_ @(Ptr Instance_T -> DebugUtilsMessageSeverityFlagBitsEXT -> ("messageTypes" ::: DebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr (SomeStruct DebugUtilsMessengerCallbackDataEXT)) -> IO ()) vkSubmitDebugUtilsMessageEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr CooperativeMatrixPropertiesNV) -> IO Result) vkGetPhysicalDeviceCooperativeMatrixPropertiesNV)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pSurfaceInfo" ::: Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR)) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr PresentModeKHR) -> IO Result) vkGetPhysicalDeviceSurfacePresentModes2EXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("queueFamilyIndex" ::: Word32) -> ("pCounterCount" ::: Ptr Word32) -> ("pCounters" ::: Ptr PerformanceCounterKHR) -> ("pCounterDescriptions" ::: Ptr PerformanceCounterDescriptionKHR) -> IO Result) vkEnumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pPerformanceQueryCreateInfo" ::: Ptr QueryPoolPerformanceCreateInfoKHR) -> ("pNumPasses" ::: Ptr Word32) -> IO ()) vkGetPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR)
    (castFunPtr @_ @(Ptr Instance_T -> ("pCreateInfo" ::: Ptr HeadlessSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSurface" ::: Ptr SurfaceKHR) -> IO Result) vkCreateHeadlessSurfaceEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pCombinationCount" ::: Ptr Word32) -> ("pCombinations" ::: Ptr FramebufferMixedSamplesCombinationNV) -> IO Result) vkGetPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pToolCount" ::: Ptr Word32) -> ("pToolProperties" ::: Ptr PhysicalDeviceToolProperties) -> IO Result) vkGetPhysicalDeviceToolProperties)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pFragmentShadingRateCount" ::: Ptr Word32) -> ("pFragmentShadingRates" ::: Ptr PhysicalDeviceFragmentShadingRateKHR) -> IO Result) vkGetPhysicalDeviceFragmentShadingRatesKHR)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("drmFd" ::: Int32) -> DisplayKHR -> IO Result) vkAcquireDrmDisplayEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("drmFd" ::: Int32) -> ("connectorId" ::: Word32) -> Ptr DisplayKHR -> IO Result) vkGetDrmDisplayEXT)
    (castFunPtr @_ @(Ptr PhysicalDevice_T -> ("pOpticalFlowImageFormatInfo" ::: Ptr OpticalFlowImageFormatInfoNV) -> ("pFormatCount" ::: Ptr Word32) -> ("pImageFormatProperties" ::: Ptr OpticalFlowImageFormatPropertiesNV) -> IO Result) vkGetPhysicalDeviceOpticalFlowImageFormatsNV)

data DeviceCmds = DeviceCmds
  { deviceCmdsHandle :: Ptr Device_T
  , pVkGetDeviceProcAddr :: FunPtr (Ptr Device_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
  , pVkDestroyDevice :: FunPtr (Ptr Device_T -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetDeviceQueue :: FunPtr (Ptr Device_T -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr (Ptr Queue_T)) -> IO ())
  , pVkQueueSubmit :: FunPtr (Ptr Queue_T -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr (SomeStruct SubmitInfo)) -> Fence -> IO Result)
  , pVkQueueWaitIdle :: FunPtr (Ptr Queue_T -> IO Result)
  , pVkDeviceWaitIdle :: FunPtr (Ptr Device_T -> IO Result)
  , pVkAllocateMemory :: FunPtr (Ptr Device_T -> ("pAllocateInfo" ::: Ptr (SomeStruct MemoryAllocateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pMemory" ::: Ptr DeviceMemory) -> IO Result)
  , pVkFreeMemory :: FunPtr (Ptr Device_T -> DeviceMemory -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkMapMemory :: FunPtr (Ptr Device_T -> DeviceMemory -> ("offset" ::: DeviceSize) -> DeviceSize -> MemoryMapFlags -> ("ppData" ::: Ptr (Ptr ())) -> IO Result)
  , pVkUnmapMemory :: FunPtr (Ptr Device_T -> DeviceMemory -> IO ())
  , pVkFlushMappedMemoryRanges :: FunPtr (Ptr Device_T -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr MappedMemoryRange) -> IO Result)
  , pVkInvalidateMappedMemoryRanges :: FunPtr (Ptr Device_T -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr MappedMemoryRange) -> IO Result)
  , pVkGetDeviceMemoryCommitment :: FunPtr (Ptr Device_T -> DeviceMemory -> ("pCommittedMemoryInBytes" ::: Ptr DeviceSize) -> IO ())
  , pVkGetBufferMemoryRequirements :: FunPtr (Ptr Device_T -> Buffer -> ("pMemoryRequirements" ::: Ptr MemoryRequirements) -> IO ())
  , pVkBindBufferMemory :: FunPtr (Ptr Device_T -> Buffer -> DeviceMemory -> ("memoryOffset" ::: DeviceSize) -> IO Result)
  , pVkGetImageMemoryRequirements :: FunPtr (Ptr Device_T -> Image -> ("pMemoryRequirements" ::: Ptr MemoryRequirements) -> IO ())
  , pVkBindImageMemory :: FunPtr (Ptr Device_T -> Image -> DeviceMemory -> ("memoryOffset" ::: DeviceSize) -> IO Result)
  , pVkGetImageSparseMemoryRequirements :: FunPtr (Ptr Device_T -> Image -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr SparseImageMemoryRequirements) -> IO ())
  , pVkQueueBindSparse :: FunPtr (Ptr Queue_T -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr (SomeStruct BindSparseInfo)) -> Fence -> IO Result)
  , pVkCreateFence :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct FenceCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pFence" ::: Ptr Fence) -> IO Result)
  , pVkDestroyFence :: FunPtr (Ptr Device_T -> Fence -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkResetFences :: FunPtr (Ptr Device_T -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr Fence) -> IO Result)
  , pVkGetFenceStatus :: FunPtr (Ptr Device_T -> Fence -> IO Result)
  , pVkWaitForFences :: FunPtr (Ptr Device_T -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr Fence) -> ("waitAll" ::: Bool32) -> ("timeout" ::: Word64) -> IO Result)
  , pVkCreateSemaphore :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct SemaphoreCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSemaphore" ::: Ptr Semaphore) -> IO Result)
  , pVkDestroySemaphore :: FunPtr (Ptr Device_T -> Semaphore -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCreateEvent :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct EventCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pEvent" ::: Ptr Event) -> IO Result)
  , pVkDestroyEvent :: FunPtr (Ptr Device_T -> Event -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetEventStatus :: FunPtr (Ptr Device_T -> Event -> IO Result)
  , pVkSetEvent :: FunPtr (Ptr Device_T -> Event -> IO Result)
  , pVkResetEvent :: FunPtr (Ptr Device_T -> Event -> IO Result)
  , pVkCreateQueryPool :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct QueryPoolCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pQueryPool" ::: Ptr QueryPool) -> IO Result)
  , pVkDestroyQueryPool :: FunPtr (Ptr Device_T -> QueryPool -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetQueryPoolResults :: FunPtr (Ptr Device_T -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: DeviceSize) -> QueryResultFlags -> IO Result)
  , pVkResetQueryPool :: FunPtr (Ptr Device_T -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ())
  , pVkCreateBuffer :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct BufferCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pBuffer" ::: Ptr Buffer) -> IO Result)
  , pVkDestroyBuffer :: FunPtr (Ptr Device_T -> Buffer -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCreateBufferView :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct BufferViewCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pView" ::: Ptr BufferView) -> IO Result)
  , pVkDestroyBufferView :: FunPtr (Ptr Device_T -> BufferView -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCreateImage :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct ImageCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pImage" ::: Ptr Image) -> IO Result)
  , pVkDestroyImage :: FunPtr (Ptr Device_T -> Image -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetImageSubresourceLayout :: FunPtr (Ptr Device_T -> Image -> ("pSubresource" ::: Ptr ImageSubresource) -> ("pLayout" ::: Ptr SubresourceLayout) -> IO ())
  , pVkCreateImageView :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct ImageViewCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pView" ::: Ptr ImageView) -> IO Result)
  , pVkDestroyImageView :: FunPtr (Ptr Device_T -> ImageView -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCreateShaderModule :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct ShaderModuleCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pShaderModule" ::: Ptr ShaderModule) -> IO Result)
  , pVkDestroyShaderModule :: FunPtr (Ptr Device_T -> ShaderModule -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCreatePipelineCache :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr PipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelineCache" ::: Ptr PipelineCache) -> IO Result)
  , pVkDestroyPipelineCache :: FunPtr (Ptr Device_T -> PipelineCache -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetPipelineCacheData :: FunPtr (Ptr Device_T -> PipelineCache -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO Result)
  , pVkMergePipelineCaches :: FunPtr (Ptr Device_T -> ("dstCache" ::: PipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr PipelineCache) -> IO Result)
  , pVkCreateGraphicsPipelines :: FunPtr (Ptr Device_T -> PipelineCache -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr (SomeStruct GraphicsPipelineCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelines" ::: Ptr Pipeline) -> IO Result)
  , pVkCreateComputePipelines :: FunPtr (Ptr Device_T -> PipelineCache -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr (SomeStruct ComputePipelineCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelines" ::: Ptr Pipeline) -> IO Result)
  , pVkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI :: FunPtr (Ptr Device_T -> RenderPass -> ("pMaxWorkgroupSize" ::: Ptr Extent2D) -> IO Result)
  , pVkDestroyPipeline :: FunPtr (Ptr Device_T -> Pipeline -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCreatePipelineLayout :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr PipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelineLayout" ::: Ptr PipelineLayout) -> IO Result)
  , pVkDestroyPipelineLayout :: FunPtr (Ptr Device_T -> PipelineLayout -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCreateSampler :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct SamplerCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSampler" ::: Ptr Sampler) -> IO Result)
  , pVkDestroySampler :: FunPtr (Ptr Device_T -> Sampler -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCreateDescriptorSetLayout :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct DescriptorSetLayoutCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSetLayout" ::: Ptr DescriptorSetLayout) -> IO Result)
  , pVkDestroyDescriptorSetLayout :: FunPtr (Ptr Device_T -> DescriptorSetLayout -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCreateDescriptorPool :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct DescriptorPoolCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pDescriptorPool" ::: Ptr DescriptorPool) -> IO Result)
  , pVkDestroyDescriptorPool :: FunPtr (Ptr Device_T -> DescriptorPool -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkResetDescriptorPool :: FunPtr (Ptr Device_T -> DescriptorPool -> DescriptorPoolResetFlags -> IO Result)
  , pVkAllocateDescriptorSets :: FunPtr (Ptr Device_T -> ("pAllocateInfo" ::: Ptr (SomeStruct DescriptorSetAllocateInfo)) -> ("pDescriptorSets" ::: Ptr DescriptorSet) -> IO Result)
  , pVkFreeDescriptorSets :: FunPtr (Ptr Device_T -> DescriptorPool -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr DescriptorSet) -> IO Result)
  , pVkUpdateDescriptorSets :: FunPtr (Ptr Device_T -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr (SomeStruct WriteDescriptorSet)) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr CopyDescriptorSet) -> IO ())
  , pVkCreateFramebuffer :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct FramebufferCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pFramebuffer" ::: Ptr Framebuffer) -> IO Result)
  , pVkDestroyFramebuffer :: FunPtr (Ptr Device_T -> Framebuffer -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCreateRenderPass :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct RenderPassCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pRenderPass" ::: Ptr RenderPass) -> IO Result)
  , pVkDestroyRenderPass :: FunPtr (Ptr Device_T -> RenderPass -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetRenderAreaGranularity :: FunPtr (Ptr Device_T -> RenderPass -> ("pGranularity" ::: Ptr Extent2D) -> IO ())
  , pVkCreateCommandPool :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr CommandPoolCreateInfo) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pCommandPool" ::: Ptr CommandPool) -> IO Result)
  , pVkDestroyCommandPool :: FunPtr (Ptr Device_T -> CommandPool -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkResetCommandPool :: FunPtr (Ptr Device_T -> CommandPool -> CommandPoolResetFlags -> IO Result)
  , pVkAllocateCommandBuffers :: FunPtr (Ptr Device_T -> ("pAllocateInfo" ::: Ptr CommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr (Ptr CommandBuffer_T)) -> IO Result)
  , pVkFreeCommandBuffers :: FunPtr (Ptr Device_T -> CommandPool -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr (Ptr CommandBuffer_T)) -> IO ())
  , pVkBeginCommandBuffer :: FunPtr (Ptr CommandBuffer_T -> ("pBeginInfo" ::: Ptr (SomeStruct CommandBufferBeginInfo)) -> IO Result)
  , pVkEndCommandBuffer :: FunPtr (Ptr CommandBuffer_T -> IO Result)
  , pVkResetCommandBuffer :: FunPtr (Ptr CommandBuffer_T -> CommandBufferResetFlags -> IO Result)
  , pVkCmdBindPipeline :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> IO ())
  , pVkCmdSetViewport :: FunPtr (Ptr CommandBuffer_T -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr Viewport) -> IO ())
  , pVkCmdSetScissor :: FunPtr (Ptr CommandBuffer_T -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr Rect2D) -> IO ())
  , pVkCmdSetLineWidth :: FunPtr (Ptr CommandBuffer_T -> ("lineWidth" ::: CFloat) -> IO ())
  , pVkCmdSetDepthBias :: FunPtr (Ptr CommandBuffer_T -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ())
  , pVkCmdSetBlendConstants :: FunPtr (Ptr CommandBuffer_T -> ("blendConstants" ::: Ptr (FixedArray 4 CFloat)) -> IO ())
  , pVkCmdSetDepthBounds :: FunPtr (Ptr CommandBuffer_T -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ())
  , pVkCmdSetStencilCompareMask :: FunPtr (Ptr CommandBuffer_T -> ("faceMask" ::: StencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ())
  , pVkCmdSetStencilWriteMask :: FunPtr (Ptr CommandBuffer_T -> ("faceMask" ::: StencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ())
  , pVkCmdSetStencilReference :: FunPtr (Ptr CommandBuffer_T -> ("faceMask" ::: StencilFaceFlags) -> ("reference" ::: Word32) -> IO ())
  , pVkCmdBindDescriptorSets :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr DescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ())
  , pVkCmdBindIndexBuffer :: FunPtr (Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> IndexType -> IO ())
  , pVkCmdBindVertexBuffers :: FunPtr (Ptr CommandBuffer_T -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr Buffer) -> ("pOffsets" ::: Ptr DeviceSize) -> IO ())
  , pVkCmdDraw :: FunPtr (Ptr CommandBuffer_T -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ())
  , pVkCmdDrawIndexed :: FunPtr (Ptr CommandBuffer_T -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ())
  , pVkCmdDrawMultiEXT :: FunPtr (Ptr CommandBuffer_T -> ("drawCount" ::: Word32) -> ("pVertexInfo" ::: Ptr MultiDrawInfoEXT) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDrawMultiIndexedEXT :: FunPtr (Ptr CommandBuffer_T -> ("drawCount" ::: Word32) -> ("pIndexInfo" ::: Ptr MultiDrawIndexedInfoEXT) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("stride" ::: Word32) -> ("pVertexOffset" ::: Ptr Int32) -> IO ())
  , pVkCmdDrawIndirect :: FunPtr (Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDrawIndexedIndirect :: FunPtr (Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDispatch :: FunPtr (Ptr CommandBuffer_T -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ())
  , pVkCmdDispatchIndirect :: FunPtr (Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> IO ())
  , pVkCmdSubpassShadingHUAWEI :: FunPtr (Ptr CommandBuffer_T -> IO ())
  , pVkCmdCopyBuffer :: FunPtr (Ptr CommandBuffer_T -> ("srcBuffer" ::: Buffer) -> ("dstBuffer" ::: Buffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr BufferCopy) -> IO ())
  , pVkCmdCopyImage :: FunPtr (Ptr CommandBuffer_T -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr ImageCopy) -> IO ())
  , pVkCmdBlitImage :: FunPtr (Ptr CommandBuffer_T -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr ImageBlit) -> Filter -> IO ())
  , pVkCmdCopyBufferToImage :: FunPtr (Ptr CommandBuffer_T -> ("srcBuffer" ::: Buffer) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr BufferImageCopy) -> IO ())
  , pVkCmdCopyImageToBuffer :: FunPtr (Ptr CommandBuffer_T -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstBuffer" ::: Buffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr BufferImageCopy) -> IO ())
  , pVkCmdCopyMemoryIndirectNV :: FunPtr (Ptr CommandBuffer_T -> ("copyBufferAddress" ::: DeviceAddress) -> ("copyCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdCopyMemoryToImageIndirectNV :: FunPtr (Ptr CommandBuffer_T -> ("copyBufferAddress" ::: DeviceAddress) -> ("copyCount" ::: Word32) -> ("stride" ::: Word32) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("pImageSubresources" ::: Ptr ImageSubresourceLayers) -> IO ())
  , pVkCmdUpdateBuffer :: FunPtr (Ptr CommandBuffer_T -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("dataSize" ::: DeviceSize) -> ("pData" ::: Ptr ()) -> IO ())
  , pVkCmdFillBuffer :: FunPtr (Ptr CommandBuffer_T -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> DeviceSize -> ("data" ::: Word32) -> IO ())
  , pVkCmdClearColorImage :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> ("pColor" ::: Ptr ClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr ImageSubresourceRange) -> IO ())
  , pVkCmdClearDepthStencilImage :: FunPtr (Ptr CommandBuffer_T -> Image -> ImageLayout -> ("pDepthStencil" ::: Ptr ClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr ImageSubresourceRange) -> IO ())
  , pVkCmdClearAttachments :: FunPtr (Ptr CommandBuffer_T -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr ClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr ClearRect) -> IO ())
  , pVkCmdResolveImage :: FunPtr (Ptr CommandBuffer_T -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr ImageResolve) -> IO ())
  , pVkCmdSetEvent :: FunPtr (Ptr CommandBuffer_T -> Event -> ("stageMask" ::: PipelineStageFlags) -> IO ())
  , pVkCmdResetEvent :: FunPtr (Ptr CommandBuffer_T -> Event -> ("stageMask" ::: PipelineStageFlags) -> IO ())
  , pVkCmdWaitEvents :: FunPtr (Ptr CommandBuffer_T -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr Event) -> ("srcStageMask" ::: PipelineStageFlags) -> ("dstStageMask" ::: PipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr MemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr BufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr (SomeStruct ImageMemoryBarrier)) -> IO ())
  , pVkCmdPipelineBarrier :: FunPtr (Ptr CommandBuffer_T -> ("srcStageMask" ::: PipelineStageFlags) -> ("dstStageMask" ::: PipelineStageFlags) -> DependencyFlags -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr MemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr BufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr (SomeStruct ImageMemoryBarrier)) -> IO ())
  , pVkCmdBeginQuery :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> ("query" ::: Word32) -> QueryControlFlags -> IO ())
  , pVkCmdEndQuery :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> ("query" ::: Word32) -> IO ())
  , pVkCmdBeginConditionalRenderingEXT :: FunPtr (Ptr CommandBuffer_T -> ("pConditionalRenderingBegin" ::: Ptr ConditionalRenderingBeginInfoEXT) -> IO ())
  , pVkCmdEndConditionalRenderingEXT :: FunPtr (Ptr CommandBuffer_T -> IO ())
  , pVkCmdResetQueryPool :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ())
  , pVkCmdWriteTimestamp :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlagBits -> QueryPool -> ("query" ::: Word32) -> IO ())
  , pVkCmdCopyQueryPoolResults :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("stride" ::: DeviceSize) -> QueryResultFlags -> IO ())
  , pVkCmdPushConstants :: FunPtr (Ptr CommandBuffer_T -> PipelineLayout -> ShaderStageFlags -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ())
  , pVkCmdBeginRenderPass :: FunPtr (Ptr CommandBuffer_T -> ("pRenderPassBegin" ::: Ptr (SomeStruct RenderPassBeginInfo)) -> SubpassContents -> IO ())
  , pVkCmdNextSubpass :: FunPtr (Ptr CommandBuffer_T -> SubpassContents -> IO ())
  , pVkCmdEndRenderPass :: FunPtr (Ptr CommandBuffer_T -> IO ())
  , pVkCmdExecuteCommands :: FunPtr (Ptr CommandBuffer_T -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr (Ptr CommandBuffer_T)) -> IO ())
  , pVkCreateSharedSwapchainsKHR :: FunPtr (Ptr Device_T -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr (SomeStruct SwapchainCreateInfoKHR)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSwapchains" ::: Ptr SwapchainKHR) -> IO Result)
  , pVkCreateSwapchainKHR :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct SwapchainCreateInfoKHR)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSwapchain" ::: Ptr SwapchainKHR) -> IO Result)
  , pVkDestroySwapchainKHR :: FunPtr (Ptr Device_T -> SwapchainKHR -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetSwapchainImagesKHR :: FunPtr (Ptr Device_T -> SwapchainKHR -> ("pSwapchainImageCount" ::: Ptr Word32) -> ("pSwapchainImages" ::: Ptr Image) -> IO Result)
  , pVkAcquireNextImageKHR :: FunPtr (Ptr Device_T -> SwapchainKHR -> ("timeout" ::: Word64) -> Semaphore -> Fence -> ("pImageIndex" ::: Ptr Word32) -> IO Result)
  , pVkQueuePresentKHR :: FunPtr (Ptr Queue_T -> ("pPresentInfo" ::: Ptr (SomeStruct PresentInfoKHR)) -> IO Result)
  , pVkDebugMarkerSetObjectNameEXT :: FunPtr (Ptr Device_T -> ("pNameInfo" ::: Ptr DebugMarkerObjectNameInfoEXT) -> IO Result)
  , pVkDebugMarkerSetObjectTagEXT :: FunPtr (Ptr Device_T -> ("pTagInfo" ::: Ptr DebugMarkerObjectTagInfoEXT) -> IO Result)
  , pVkCmdDebugMarkerBeginEXT :: FunPtr (Ptr CommandBuffer_T -> ("pMarkerInfo" ::: Ptr DebugMarkerMarkerInfoEXT) -> IO ())
  , pVkCmdDebugMarkerEndEXT :: FunPtr (Ptr CommandBuffer_T -> IO ())
  , pVkCmdDebugMarkerInsertEXT :: FunPtr (Ptr CommandBuffer_T -> ("pMarkerInfo" ::: Ptr DebugMarkerMarkerInfoEXT) -> IO ())
  , pVkGetMemoryWin32HandleNV :: FunPtr (Ptr Device_T -> DeviceMemory -> ExternalMemoryHandleTypeFlagsNV -> ("pHandle" ::: Ptr HANDLE) -> IO Result)
  , pVkCmdExecuteGeneratedCommandsNV :: FunPtr (Ptr CommandBuffer_T -> ("isPreprocessed" ::: Bool32) -> ("pGeneratedCommandsInfo" ::: Ptr GeneratedCommandsInfoNV) -> IO ())
  , pVkCmdPreprocessGeneratedCommandsNV :: FunPtr (Ptr CommandBuffer_T -> ("pGeneratedCommandsInfo" ::: Ptr GeneratedCommandsInfoNV) -> IO ())
  , pVkCmdBindPipelineShaderGroupNV :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> ("groupIndex" ::: Word32) -> IO ())
  , pVkGetGeneratedCommandsMemoryRequirementsNV :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr GeneratedCommandsMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ())
  , pVkCreateIndirectCommandsLayoutNV :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr IndirectCommandsLayoutCreateInfoNV) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr IndirectCommandsLayoutNV) -> IO Result)
  , pVkDestroyIndirectCommandsLayoutNV :: FunPtr (Ptr Device_T -> IndirectCommandsLayoutNV -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCmdPushDescriptorSetKHR :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr (SomeStruct WriteDescriptorSet)) -> IO ())
  , pVkTrimCommandPool :: FunPtr (Ptr Device_T -> CommandPool -> CommandPoolTrimFlags -> IO ())
  , pVkGetMemoryWin32HandleKHR :: FunPtr (Ptr Device_T -> ("pGetWin32HandleInfo" ::: Ptr MemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO Result)
  , pVkGetMemoryWin32HandlePropertiesKHR :: FunPtr (Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> HANDLE -> ("pMemoryWin32HandleProperties" ::: Ptr MemoryWin32HandlePropertiesKHR) -> IO Result)
  , pVkGetMemoryFdKHR :: FunPtr (Ptr Device_T -> ("pGetFdInfo" ::: Ptr MemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO Result)
  , pVkGetMemoryFdPropertiesKHR :: FunPtr (Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr MemoryFdPropertiesKHR) -> IO Result)
  , pVkGetMemoryZirconHandleFUCHSIA :: FunPtr (Ptr Device_T -> ("pGetZirconHandleInfo" ::: Ptr MemoryGetZirconHandleInfoFUCHSIA) -> ("pZirconHandle" ::: Ptr Zx_handle_t) -> IO Result)
  , pVkGetMemoryZirconHandlePropertiesFUCHSIA :: FunPtr (Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> ("zirconHandle" ::: Zx_handle_t) -> ("pMemoryZirconHandleProperties" ::: Ptr MemoryZirconHandlePropertiesFUCHSIA) -> IO Result)
  , pVkGetMemoryRemoteAddressNV :: FunPtr (Ptr Device_T -> ("pMemoryGetRemoteAddressInfo" ::: Ptr MemoryGetRemoteAddressInfoNV) -> ("pAddress" ::: Ptr RemoteAddressNV) -> IO Result)
  , pVkGetSemaphoreWin32HandleKHR :: FunPtr (Ptr Device_T -> ("pGetWin32HandleInfo" ::: Ptr SemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO Result)
  , pVkImportSemaphoreWin32HandleKHR :: FunPtr (Ptr Device_T -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr ImportSemaphoreWin32HandleInfoKHR) -> IO Result)
  , pVkGetSemaphoreFdKHR :: FunPtr (Ptr Device_T -> ("pGetFdInfo" ::: Ptr SemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO Result)
  , pVkImportSemaphoreFdKHR :: FunPtr (Ptr Device_T -> ("pImportSemaphoreFdInfo" ::: Ptr ImportSemaphoreFdInfoKHR) -> IO Result)
  , pVkGetSemaphoreZirconHandleFUCHSIA :: FunPtr (Ptr Device_T -> ("pGetZirconHandleInfo" ::: Ptr SemaphoreGetZirconHandleInfoFUCHSIA) -> ("pZirconHandle" ::: Ptr Zx_handle_t) -> IO Result)
  , pVkImportSemaphoreZirconHandleFUCHSIA :: FunPtr (Ptr Device_T -> ("pImportSemaphoreZirconHandleInfo" ::: Ptr ImportSemaphoreZirconHandleInfoFUCHSIA) -> IO Result)
  , pVkGetFenceWin32HandleKHR :: FunPtr (Ptr Device_T -> ("pGetWin32HandleInfo" ::: Ptr FenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO Result)
  , pVkImportFenceWin32HandleKHR :: FunPtr (Ptr Device_T -> ("pImportFenceWin32HandleInfo" ::: Ptr ImportFenceWin32HandleInfoKHR) -> IO Result)
  , pVkGetFenceFdKHR :: FunPtr (Ptr Device_T -> ("pGetFdInfo" ::: Ptr FenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO Result)
  , pVkImportFenceFdKHR :: FunPtr (Ptr Device_T -> ("pImportFenceFdInfo" ::: Ptr ImportFenceFdInfoKHR) -> IO Result)
  , pVkDisplayPowerControlEXT :: FunPtr (Ptr Device_T -> DisplayKHR -> ("pDisplayPowerInfo" ::: Ptr DisplayPowerInfoEXT) -> IO Result)
  , pVkRegisterDeviceEventEXT :: FunPtr (Ptr Device_T -> ("pDeviceEventInfo" ::: Ptr DeviceEventInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pFence" ::: Ptr Fence) -> IO Result)
  , pVkRegisterDisplayEventEXT :: FunPtr (Ptr Device_T -> DisplayKHR -> ("pDisplayEventInfo" ::: Ptr DisplayEventInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pFence" ::: Ptr Fence) -> IO Result)
  , pVkGetSwapchainCounterEXT :: FunPtr (Ptr Device_T -> SwapchainKHR -> SurfaceCounterFlagBitsEXT -> ("pCounterValue" ::: Ptr Word64) -> IO Result)
  , pVkGetDeviceGroupPeerMemoryFeatures :: FunPtr (Ptr Device_T -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr PeerMemoryFeatureFlags) -> IO ())
  , pVkBindBufferMemory2 :: FunPtr (Ptr Device_T -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr (SomeStruct BindBufferMemoryInfo)) -> IO Result)
  , pVkBindImageMemory2 :: FunPtr (Ptr Device_T -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr (SomeStruct BindImageMemoryInfo)) -> IO Result)
  , pVkCmdSetDeviceMask :: FunPtr (Ptr CommandBuffer_T -> ("deviceMask" ::: Word32) -> IO ())
  , pVkGetDeviceGroupPresentCapabilitiesKHR :: FunPtr (Ptr Device_T -> ("pDeviceGroupPresentCapabilities" ::: Ptr DeviceGroupPresentCapabilitiesKHR) -> IO Result)
  , pVkGetDeviceGroupSurfacePresentModesKHR :: FunPtr (Ptr Device_T -> SurfaceKHR -> ("pModes" ::: Ptr DeviceGroupPresentModeFlagsKHR) -> IO Result)
  , pVkAcquireNextImage2KHR :: FunPtr (Ptr Device_T -> ("pAcquireInfo" ::: Ptr AcquireNextImageInfoKHR) -> ("pImageIndex" ::: Ptr Word32) -> IO Result)
  , pVkCmdDispatchBase :: FunPtr (Ptr CommandBuffer_T -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ())
  , pVkCreateDescriptorUpdateTemplate :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr DescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr DescriptorUpdateTemplate) -> IO Result)
  , pVkDestroyDescriptorUpdateTemplate :: FunPtr (Ptr Device_T -> DescriptorUpdateTemplate -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkUpdateDescriptorSetWithTemplate :: FunPtr (Ptr Device_T -> DescriptorSet -> DescriptorUpdateTemplate -> ("pData" ::: Ptr ()) -> IO ())
  , pVkCmdPushDescriptorSetWithTemplateKHR :: FunPtr (Ptr CommandBuffer_T -> DescriptorUpdateTemplate -> PipelineLayout -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ())
  , pVkSetHdrMetadataEXT :: FunPtr (Ptr Device_T -> ("swapchainCount" ::: Word32) -> ("pSwapchains" ::: Ptr SwapchainKHR) -> ("pMetadata" ::: Ptr HdrMetadataEXT) -> IO ())
  , pVkGetSwapchainStatusKHR :: FunPtr (Ptr Device_T -> SwapchainKHR -> IO Result)
  , pVkGetRefreshCycleDurationGOOGLE :: FunPtr (Ptr Device_T -> SwapchainKHR -> ("pDisplayTimingProperties" ::: Ptr RefreshCycleDurationGOOGLE) -> IO Result)
  , pVkGetPastPresentationTimingGOOGLE :: FunPtr (Ptr Device_T -> SwapchainKHR -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr PastPresentationTimingGOOGLE) -> IO Result)
  , pVkCmdSetViewportWScalingNV :: FunPtr (Ptr CommandBuffer_T -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr ViewportWScalingNV) -> IO ())
  , pVkCmdSetDiscardRectangleEXT :: FunPtr (Ptr CommandBuffer_T -> ("firstDiscardRectangle" ::: Word32) -> ("discardRectangleCount" ::: Word32) -> ("pDiscardRectangles" ::: Ptr Rect2D) -> IO ())
  , pVkCmdSetSampleLocationsEXT :: FunPtr (Ptr CommandBuffer_T -> ("pSampleLocationsInfo" ::: Ptr SampleLocationsInfoEXT) -> IO ())
  , pVkGetBufferMemoryRequirements2 :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr BufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ())
  , pVkGetImageMemoryRequirements2 :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr (SomeStruct ImageMemoryRequirementsInfo2)) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ())
  , pVkGetImageSparseMemoryRequirements2 :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr ImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr SparseImageMemoryRequirements2) -> IO ())
  , pVkGetDeviceBufferMemoryRequirements :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr DeviceBufferMemoryRequirements) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ())
  , pVkGetDeviceImageMemoryRequirements :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr DeviceImageMemoryRequirements) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ())
  , pVkGetDeviceImageSparseMemoryRequirements :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr DeviceImageMemoryRequirements) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr SparseImageMemoryRequirements2) -> IO ())
  , pVkCreateSamplerYcbcrConversion :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct SamplerYcbcrConversionCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr SamplerYcbcrConversion) -> IO Result)
  , pVkDestroySamplerYcbcrConversion :: FunPtr (Ptr Device_T -> SamplerYcbcrConversion -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetDeviceQueue2 :: FunPtr (Ptr Device_T -> ("pQueueInfo" ::: Ptr DeviceQueueInfo2) -> ("pQueue" ::: Ptr (Ptr Queue_T)) -> IO ())
  , pVkCreateValidationCacheEXT :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr ValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pValidationCache" ::: Ptr ValidationCacheEXT) -> IO Result)
  , pVkDestroyValidationCacheEXT :: FunPtr (Ptr Device_T -> ValidationCacheEXT -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetValidationCacheDataEXT :: FunPtr (Ptr Device_T -> ValidationCacheEXT -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO Result)
  , pVkMergeValidationCachesEXT :: FunPtr (Ptr Device_T -> ("dstCache" ::: ValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr ValidationCacheEXT) -> IO Result)
  , pVkGetDescriptorSetLayoutSupport :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct DescriptorSetLayoutCreateInfo)) -> ("pSupport" ::: Ptr (SomeStruct DescriptorSetLayoutSupport)) -> IO ())
  , pVkGetShaderInfoAMD :: FunPtr (Ptr Device_T -> Pipeline -> ShaderStageFlagBits -> ShaderInfoTypeAMD -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO Result)
  , pVkSetLocalDimmingAMD :: FunPtr (Ptr Device_T -> SwapchainKHR -> ("localDimmingEnable" ::: Bool32) -> IO ())
  , pVkGetCalibratedTimestampsEXT :: FunPtr (Ptr Device_T -> ("timestampCount" ::: Word32) -> ("pTimestampInfos" ::: Ptr CalibratedTimestampInfoEXT) -> ("pTimestamps" ::: Ptr Word64) -> ("pMaxDeviation" ::: Ptr Word64) -> IO Result)
  , pVkSetDebugUtilsObjectNameEXT :: FunPtr (Ptr Device_T -> ("pNameInfo" ::: Ptr DebugUtilsObjectNameInfoEXT) -> IO Result)
  , pVkSetDebugUtilsObjectTagEXT :: FunPtr (Ptr Device_T -> ("pTagInfo" ::: Ptr DebugUtilsObjectTagInfoEXT) -> IO Result)
  , pVkQueueBeginDebugUtilsLabelEXT :: FunPtr (Ptr Queue_T -> ("pLabelInfo" ::: Ptr DebugUtilsLabelEXT) -> IO ())
  , pVkQueueEndDebugUtilsLabelEXT :: FunPtr (Ptr Queue_T -> IO ())
  , pVkQueueInsertDebugUtilsLabelEXT :: FunPtr (Ptr Queue_T -> ("pLabelInfo" ::: Ptr DebugUtilsLabelEXT) -> IO ())
  , pVkCmdBeginDebugUtilsLabelEXT :: FunPtr (Ptr CommandBuffer_T -> ("pLabelInfo" ::: Ptr DebugUtilsLabelEXT) -> IO ())
  , pVkCmdEndDebugUtilsLabelEXT :: FunPtr (Ptr CommandBuffer_T -> IO ())
  , pVkCmdInsertDebugUtilsLabelEXT :: FunPtr (Ptr CommandBuffer_T -> ("pLabelInfo" ::: Ptr DebugUtilsLabelEXT) -> IO ())
  , pVkGetMemoryHostPointerPropertiesEXT :: FunPtr (Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr MemoryHostPointerPropertiesEXT) -> IO Result)
  , pVkCmdWriteBufferMarkerAMD :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlagBits -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("marker" ::: Word32) -> IO ())
  , pVkCreateRenderPass2 :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct RenderPassCreateInfo2)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pRenderPass" ::: Ptr RenderPass) -> IO Result)
  , pVkCmdBeginRenderPass2 :: FunPtr (Ptr CommandBuffer_T -> ("pRenderPassBegin" ::: Ptr (SomeStruct RenderPassBeginInfo)) -> ("pSubpassBeginInfo" ::: Ptr SubpassBeginInfo) -> IO ())
  , pVkCmdNextSubpass2 :: FunPtr (Ptr CommandBuffer_T -> ("pSubpassBeginInfo" ::: Ptr SubpassBeginInfo) -> ("pSubpassEndInfo" ::: Ptr (SomeStruct SubpassEndInfo)) -> IO ())
  , pVkCmdEndRenderPass2 :: FunPtr (Ptr CommandBuffer_T -> ("pSubpassEndInfo" ::: Ptr (SomeStruct SubpassEndInfo)) -> IO ())
  , pVkGetSemaphoreCounterValue :: FunPtr (Ptr Device_T -> Semaphore -> ("pValue" ::: Ptr Word64) -> IO Result)
  , pVkWaitSemaphores :: FunPtr (Ptr Device_T -> ("pWaitInfo" ::: Ptr SemaphoreWaitInfo) -> ("timeout" ::: Word64) -> IO Result)
  , pVkSignalSemaphore :: FunPtr (Ptr Device_T -> ("pSignalInfo" ::: Ptr SemaphoreSignalInfo) -> IO Result)
  , pVkGetAndroidHardwareBufferPropertiesANDROID :: FunPtr (Ptr Device_T -> Ptr AHardwareBuffer -> ("pProperties" ::: Ptr (SomeStruct AndroidHardwareBufferPropertiesANDROID)) -> IO Result)
  , pVkGetMemoryAndroidHardwareBufferANDROID :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr MemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO Result)
  , pVkCmdDrawIndirectCount :: FunPtr (Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("countBuffer" ::: Buffer) -> ("countBufferOffset" ::: DeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDrawIndexedIndirectCount :: FunPtr (Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("countBuffer" ::: Buffer) -> ("countBufferOffset" ::: DeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdSetCheckpointNV :: FunPtr (Ptr CommandBuffer_T -> ("pCheckpointMarker" ::: Ptr ()) -> IO ())
  , pVkGetQueueCheckpointDataNV :: FunPtr (Ptr Queue_T -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr CheckpointDataNV) -> IO ())
  , pVkCmdBindTransformFeedbackBuffersEXT :: FunPtr (Ptr CommandBuffer_T -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr Buffer) -> ("pOffsets" ::: Ptr DeviceSize) -> ("pSizes" ::: Ptr DeviceSize) -> IO ())
  , pVkCmdBeginTransformFeedbackEXT :: FunPtr (Ptr CommandBuffer_T -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr Buffer) -> ("pCounterBufferOffsets" ::: Ptr DeviceSize) -> IO ())
  , pVkCmdEndTransformFeedbackEXT :: FunPtr (Ptr CommandBuffer_T -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr Buffer) -> ("pCounterBufferOffsets" ::: Ptr DeviceSize) -> IO ())
  , pVkCmdBeginQueryIndexedEXT :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> ("query" ::: Word32) -> QueryControlFlags -> ("index" ::: Word32) -> IO ())
  , pVkCmdEndQueryIndexedEXT :: FunPtr (Ptr CommandBuffer_T -> QueryPool -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ())
  , pVkCmdDrawIndirectByteCountEXT :: FunPtr (Ptr CommandBuffer_T -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: Buffer) -> ("counterBufferOffset" ::: DeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ())
  , pVkCmdSetExclusiveScissorNV :: FunPtr (Ptr CommandBuffer_T -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr Rect2D) -> IO ())
  , pVkCmdBindShadingRateImageNV :: FunPtr (Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ())
  , pVkCmdSetViewportShadingRatePaletteNV :: FunPtr (Ptr CommandBuffer_T -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr ShadingRatePaletteNV) -> IO ())
  , pVkCmdSetCoarseSampleOrderNV :: FunPtr (Ptr CommandBuffer_T -> CoarseSampleOrderTypeNV -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr CoarseSampleOrderCustomNV) -> IO ())
  , pVkCmdDrawMeshTasksNV :: FunPtr (Ptr CommandBuffer_T -> ("taskCount" ::: Word32) -> ("firstTask" ::: Word32) -> IO ())
  , pVkCmdDrawMeshTasksIndirectNV :: FunPtr (Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDrawMeshTasksIndirectCountNV :: FunPtr (Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("countBuffer" ::: Buffer) -> ("countBufferOffset" ::: DeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDrawMeshTasksEXT :: FunPtr (Ptr CommandBuffer_T -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ())
  , pVkCmdDrawMeshTasksIndirectEXT :: FunPtr (Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDrawMeshTasksIndirectCountEXT :: FunPtr (Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("countBuffer" ::: Buffer) -> ("countBufferOffset" ::: DeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCompileDeferredNV :: FunPtr (Ptr Device_T -> Pipeline -> ("shader" ::: Word32) -> IO Result)
  , pVkCreateAccelerationStructureNV :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct AccelerationStructureCreateInfoNV)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr AccelerationStructureNV) -> IO Result)
  , pVkCmdBindInvocationMaskHUAWEI :: FunPtr (Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ())
  , pVkDestroyAccelerationStructureKHR :: FunPtr (Ptr Device_T -> AccelerationStructureKHR -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkDestroyAccelerationStructureNV :: FunPtr (Ptr Device_T -> AccelerationStructureNV -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetAccelerationStructureMemoryRequirementsNV :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr AccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2KHR)) -> IO ())
  , pVkBindAccelerationStructureMemoryNV :: FunPtr (Ptr Device_T -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr BindAccelerationStructureMemoryInfoNV) -> IO Result)
  , pVkCmdCopyAccelerationStructureNV :: FunPtr (Ptr CommandBuffer_T -> ("dst" ::: AccelerationStructureNV) -> ("src" ::: AccelerationStructureNV) -> CopyAccelerationStructureModeKHR -> IO ())
  , pVkCmdCopyAccelerationStructureKHR :: FunPtr (Ptr CommandBuffer_T -> ("pInfo" ::: Ptr CopyAccelerationStructureInfoKHR) -> IO ())
  , pVkCopyAccelerationStructureKHR :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> ("pInfo" ::: Ptr CopyAccelerationStructureInfoKHR) -> IO Result)
  , pVkCmdCopyAccelerationStructureToMemoryKHR :: FunPtr (Ptr CommandBuffer_T -> ("pInfo" ::: Ptr CopyAccelerationStructureToMemoryInfoKHR) -> IO ())
  , pVkCopyAccelerationStructureToMemoryKHR :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> ("pInfo" ::: Ptr CopyAccelerationStructureToMemoryInfoKHR) -> IO Result)
  , pVkCmdCopyMemoryToAccelerationStructureKHR :: FunPtr (Ptr CommandBuffer_T -> ("pInfo" ::: Ptr CopyMemoryToAccelerationStructureInfoKHR) -> IO ())
  , pVkCopyMemoryToAccelerationStructureKHR :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> ("pInfo" ::: Ptr CopyMemoryToAccelerationStructureInfoKHR) -> IO Result)
  , pVkCmdWriteAccelerationStructuresPropertiesKHR :: FunPtr (Ptr CommandBuffer_T -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr AccelerationStructureKHR) -> QueryType -> QueryPool -> ("firstQuery" ::: Word32) -> IO ())
  , pVkCmdWriteAccelerationStructuresPropertiesNV :: FunPtr (Ptr CommandBuffer_T -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr AccelerationStructureNV) -> QueryType -> QueryPool -> ("firstQuery" ::: Word32) -> IO ())
  , pVkCmdBuildAccelerationStructureNV :: FunPtr (Ptr CommandBuffer_T -> ("pInfo" ::: Ptr AccelerationStructureInfoNV) -> ("instanceData" ::: Buffer) -> ("instanceOffset" ::: DeviceSize) -> ("update" ::: Bool32) -> ("dst" ::: AccelerationStructureNV) -> ("src" ::: AccelerationStructureNV) -> ("scratch" ::: Buffer) -> ("scratchOffset" ::: DeviceSize) -> IO ())
  , pVkWriteAccelerationStructuresPropertiesKHR :: FunPtr (Ptr Device_T -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr AccelerationStructureKHR) -> QueryType -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: CSize) -> IO Result)
  , pVkCmdTraceRaysKHR :: FunPtr (Ptr CommandBuffer_T -> ("pRaygenShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pMissShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pHitShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pCallableShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ())
  , pVkCmdTraceRaysNV :: FunPtr (Ptr CommandBuffer_T -> ("raygenShaderBindingTableBuffer" ::: Buffer) -> ("raygenShaderBindingOffset" ::: DeviceSize) -> ("missShaderBindingTableBuffer" ::: Buffer) -> ("missShaderBindingOffset" ::: DeviceSize) -> ("missShaderBindingStride" ::: DeviceSize) -> ("hitShaderBindingTableBuffer" ::: Buffer) -> ("hitShaderBindingOffset" ::: DeviceSize) -> ("hitShaderBindingStride" ::: DeviceSize) -> ("callableShaderBindingTableBuffer" ::: Buffer) -> ("callableShaderBindingOffset" ::: DeviceSize) -> ("callableShaderBindingStride" ::: DeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ())
  , pVkGetRayTracingShaderGroupHandlesKHR :: FunPtr (Ptr Device_T -> Pipeline -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO Result)
  , pVkGetRayTracingCaptureReplayShaderGroupHandlesKHR :: FunPtr (Ptr Device_T -> Pipeline -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO Result)
  , pVkGetAccelerationStructureHandleNV :: FunPtr (Ptr Device_T -> AccelerationStructureNV -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO Result)
  , pVkCreateRayTracingPipelinesNV :: FunPtr (Ptr Device_T -> PipelineCache -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr (SomeStruct RayTracingPipelineCreateInfoNV)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelines" ::: Ptr Pipeline) -> IO Result)
  , pVkCreateRayTracingPipelinesKHR :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> PipelineCache -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr (SomeStruct RayTracingPipelineCreateInfoKHR)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelines" ::: Ptr Pipeline) -> IO Result)
  , pVkCmdTraceRaysIndirectKHR :: FunPtr (Ptr CommandBuffer_T -> ("pRaygenShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pMissShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pHitShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pCallableShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("indirectDeviceAddress" ::: DeviceAddress) -> IO ())
  , pVkCmdTraceRaysIndirect2KHR :: FunPtr (Ptr CommandBuffer_T -> ("indirectDeviceAddress" ::: DeviceAddress) -> IO ())
  , pVkGetDeviceAccelerationStructureCompatibilityKHR :: FunPtr (Ptr Device_T -> ("pVersionInfo" ::: Ptr AccelerationStructureVersionInfoKHR) -> ("pCompatibility" ::: Ptr AccelerationStructureCompatibilityKHR) -> IO ())
  , pVkGetRayTracingShaderGroupStackSizeKHR :: FunPtr (Ptr Device_T -> Pipeline -> ("group" ::: Word32) -> ShaderGroupShaderKHR -> IO DeviceSize)
  , pVkCmdSetRayTracingPipelineStackSizeKHR :: FunPtr (Ptr CommandBuffer_T -> ("pipelineStackSize" ::: Word32) -> IO ())
  , pVkGetImageViewHandleNVX :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr ImageViewHandleInfoNVX) -> IO Word32)
  , pVkGetImageViewAddressNVX :: FunPtr (Ptr Device_T -> ImageView -> ("pProperties" ::: Ptr ImageViewAddressPropertiesNVX) -> IO Result)
  , pVkGetDeviceGroupSurfacePresentModes2EXT :: FunPtr (Ptr Device_T -> ("pSurfaceInfo" ::: Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR)) -> ("pModes" ::: Ptr DeviceGroupPresentModeFlagsKHR) -> IO Result)
  , pVkAcquireFullScreenExclusiveModeEXT :: FunPtr (Ptr Device_T -> SwapchainKHR -> IO Result)
  , pVkReleaseFullScreenExclusiveModeEXT :: FunPtr (Ptr Device_T -> SwapchainKHR -> IO Result)
  , pVkAcquireProfilingLockKHR :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr AcquireProfilingLockInfoKHR) -> IO Result)
  , pVkReleaseProfilingLockKHR :: FunPtr (Ptr Device_T -> IO ())
  , pVkGetImageDrmFormatModifierPropertiesEXT :: FunPtr (Ptr Device_T -> Image -> ("pProperties" ::: Ptr ImageDrmFormatModifierPropertiesEXT) -> IO Result)
  , pVkGetBufferOpaqueCaptureAddress :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr BufferDeviceAddressInfo) -> IO Word64)
  , pVkGetBufferDeviceAddress :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr BufferDeviceAddressInfo) -> IO DeviceAddress)
  , pVkInitializePerformanceApiINTEL :: FunPtr (Ptr Device_T -> ("pInitializeInfo" ::: Ptr InitializePerformanceApiInfoINTEL) -> IO Result)
  , pVkUninitializePerformanceApiINTEL :: FunPtr (Ptr Device_T -> IO ())
  , pVkCmdSetPerformanceMarkerINTEL :: FunPtr (Ptr CommandBuffer_T -> ("pMarkerInfo" ::: Ptr PerformanceMarkerInfoINTEL) -> IO Result)
  , pVkCmdSetPerformanceStreamMarkerINTEL :: FunPtr (Ptr CommandBuffer_T -> ("pMarkerInfo" ::: Ptr PerformanceStreamMarkerInfoINTEL) -> IO Result)
  , pVkCmdSetPerformanceOverrideINTEL :: FunPtr (Ptr CommandBuffer_T -> ("pOverrideInfo" ::: Ptr PerformanceOverrideInfoINTEL) -> IO Result)
  , pVkAcquirePerformanceConfigurationINTEL :: FunPtr (Ptr Device_T -> ("pAcquireInfo" ::: Ptr PerformanceConfigurationAcquireInfoINTEL) -> ("pConfiguration" ::: Ptr PerformanceConfigurationINTEL) -> IO Result)
  , pVkReleasePerformanceConfigurationINTEL :: FunPtr (Ptr Device_T -> PerformanceConfigurationINTEL -> IO Result)
  , pVkQueueSetPerformanceConfigurationINTEL :: FunPtr (Ptr Queue_T -> PerformanceConfigurationINTEL -> IO Result)
  , pVkGetPerformanceParameterINTEL :: FunPtr (Ptr Device_T -> PerformanceParameterTypeINTEL -> ("pValue" ::: Ptr PerformanceValueINTEL) -> IO Result)
  , pVkGetDeviceMemoryOpaqueCaptureAddress :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr DeviceMemoryOpaqueCaptureAddressInfo) -> IO Word64)
  , pVkGetPipelineExecutablePropertiesKHR :: FunPtr (Ptr Device_T -> ("pPipelineInfo" ::: Ptr PipelineInfoKHR) -> ("pExecutableCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr PipelineExecutablePropertiesKHR) -> IO Result)
  , pVkGetPipelineExecutableStatisticsKHR :: FunPtr (Ptr Device_T -> ("pExecutableInfo" ::: Ptr PipelineExecutableInfoKHR) -> ("pStatisticCount" ::: Ptr Word32) -> ("pStatistics" ::: Ptr PipelineExecutableStatisticKHR) -> IO Result)
  , pVkGetPipelineExecutableInternalRepresentationsKHR :: FunPtr (Ptr Device_T -> ("pExecutableInfo" ::: Ptr PipelineExecutableInfoKHR) -> ("pInternalRepresentationCount" ::: Ptr Word32) -> ("pInternalRepresentations" ::: Ptr PipelineExecutableInternalRepresentationKHR) -> IO Result)
  , pVkCmdSetLineStippleEXT :: FunPtr (Ptr CommandBuffer_T -> ("lineStippleFactor" ::: Word32) -> ("lineStipplePattern" ::: Word16) -> IO ())
  , pVkCreateAccelerationStructureKHR :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct AccelerationStructureCreateInfoKHR)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr AccelerationStructureKHR) -> IO Result)
  , pVkCmdBuildAccelerationStructuresKHR :: FunPtr (Ptr CommandBuffer_T -> ("infoCount" ::: Word32) -> ("pInfos" ::: Ptr AccelerationStructureBuildGeometryInfoKHR) -> ("ppBuildRangeInfos" ::: Ptr (Ptr AccelerationStructureBuildRangeInfoKHR)) -> IO ())
  , pVkCmdBuildAccelerationStructuresIndirectKHR :: FunPtr (Ptr CommandBuffer_T -> ("infoCount" ::: Word32) -> ("pInfos" ::: Ptr AccelerationStructureBuildGeometryInfoKHR) -> ("pIndirectDeviceAddresses" ::: Ptr DeviceAddress) -> ("pIndirectStrides" ::: Ptr Word32) -> ("ppMaxPrimitiveCounts" ::: Ptr (Ptr Word32)) -> IO ())
  , pVkBuildAccelerationStructuresKHR :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> ("infoCount" ::: Word32) -> ("pInfos" ::: Ptr AccelerationStructureBuildGeometryInfoKHR) -> ("ppBuildRangeInfos" ::: Ptr (Ptr AccelerationStructureBuildRangeInfoKHR)) -> IO Result)
  , pVkGetAccelerationStructureDeviceAddressKHR :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr AccelerationStructureDeviceAddressInfoKHR) -> IO DeviceAddress)
  , pVkCreateDeferredOperationKHR :: FunPtr (Ptr Device_T -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pDeferredOperation" ::: Ptr DeferredOperationKHR) -> IO Result)
  , pVkDestroyDeferredOperationKHR :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetDeferredOperationMaxConcurrencyKHR :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> IO Word32)
  , pVkGetDeferredOperationResultKHR :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> IO Result)
  , pVkDeferredOperationJoinKHR :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> IO Result)
  , pVkCmdSetCullMode :: FunPtr (Ptr CommandBuffer_T -> CullModeFlags -> IO ())
  , pVkCmdSetFrontFace :: FunPtr (Ptr CommandBuffer_T -> FrontFace -> IO ())
  , pVkCmdSetPrimitiveTopology :: FunPtr (Ptr CommandBuffer_T -> PrimitiveTopology -> IO ())
  , pVkCmdSetViewportWithCount :: FunPtr (Ptr CommandBuffer_T -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr Viewport) -> IO ())
  , pVkCmdSetScissorWithCount :: FunPtr (Ptr CommandBuffer_T -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr Rect2D) -> IO ())
  , pVkCmdBindVertexBuffers2 :: FunPtr (Ptr CommandBuffer_T -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr Buffer) -> ("pOffsets" ::: Ptr DeviceSize) -> ("pSizes" ::: Ptr DeviceSize) -> ("pStrides" ::: Ptr DeviceSize) -> IO ())
  , pVkCmdSetDepthTestEnable :: FunPtr (Ptr CommandBuffer_T -> ("depthTestEnable" ::: Bool32) -> IO ())
  , pVkCmdSetDepthWriteEnable :: FunPtr (Ptr CommandBuffer_T -> ("depthWriteEnable" ::: Bool32) -> IO ())
  , pVkCmdSetDepthCompareOp :: FunPtr (Ptr CommandBuffer_T -> ("depthCompareOp" ::: CompareOp) -> IO ())
  , pVkCmdSetDepthBoundsTestEnable :: FunPtr (Ptr CommandBuffer_T -> ("depthBoundsTestEnable" ::: Bool32) -> IO ())
  , pVkCmdSetStencilTestEnable :: FunPtr (Ptr CommandBuffer_T -> ("stencilTestEnable" ::: Bool32) -> IO ())
  , pVkCmdSetStencilOp :: FunPtr (Ptr CommandBuffer_T -> ("faceMask" ::: StencilFaceFlags) -> ("failOp" ::: StencilOp) -> ("passOp" ::: StencilOp) -> ("depthFailOp" ::: StencilOp) -> CompareOp -> IO ())
  , pVkCmdSetPatchControlPointsEXT :: FunPtr (Ptr CommandBuffer_T -> ("patchControlPoints" ::: Word32) -> IO ())
  , pVkCmdSetRasterizerDiscardEnable :: FunPtr (Ptr CommandBuffer_T -> ("rasterizerDiscardEnable" ::: Bool32) -> IO ())
  , pVkCmdSetDepthBiasEnable :: FunPtr (Ptr CommandBuffer_T -> ("depthBiasEnable" ::: Bool32) -> IO ())
  , pVkCmdSetLogicOpEXT :: FunPtr (Ptr CommandBuffer_T -> LogicOp -> IO ())
  , pVkCmdSetPrimitiveRestartEnable :: FunPtr (Ptr CommandBuffer_T -> ("primitiveRestartEnable" ::: Bool32) -> IO ())
  , pVkCmdSetTessellationDomainOriginEXT :: FunPtr (Ptr CommandBuffer_T -> TessellationDomainOrigin -> IO ())
  , pVkCmdSetDepthClampEnableEXT :: FunPtr (Ptr CommandBuffer_T -> ("depthClampEnable" ::: Bool32) -> IO ())
  , pVkCmdSetPolygonModeEXT :: FunPtr (Ptr CommandBuffer_T -> PolygonMode -> IO ())
  , pVkCmdSetRasterizationSamplesEXT :: FunPtr (Ptr CommandBuffer_T -> ("rasterizationSamples" ::: SampleCountFlagBits) -> IO ())
  , pVkCmdSetSampleMaskEXT :: FunPtr (Ptr CommandBuffer_T -> ("samples" ::: SampleCountFlagBits) -> ("pSampleMask" ::: Ptr SampleMask) -> IO ())
  , pVkCmdSetAlphaToCoverageEnableEXT :: FunPtr (Ptr CommandBuffer_T -> ("alphaToCoverageEnable" ::: Bool32) -> IO ())
  , pVkCmdSetAlphaToOneEnableEXT :: FunPtr (Ptr CommandBuffer_T -> ("alphaToOneEnable" ::: Bool32) -> IO ())
  , pVkCmdSetLogicOpEnableEXT :: FunPtr (Ptr CommandBuffer_T -> ("logicOpEnable" ::: Bool32) -> IO ())
  , pVkCmdSetColorBlendEnableEXT :: FunPtr (Ptr CommandBuffer_T -> ("firstAttachment" ::: Word32) -> ("attachmentCount" ::: Word32) -> ("pColorBlendEnables" ::: Ptr Bool32) -> IO ())
  , pVkCmdSetColorBlendEquationEXT :: FunPtr (Ptr CommandBuffer_T -> ("firstAttachment" ::: Word32) -> ("attachmentCount" ::: Word32) -> ("pColorBlendEquations" ::: Ptr ColorBlendEquationEXT) -> IO ())
  , pVkCmdSetColorWriteMaskEXT :: FunPtr (Ptr CommandBuffer_T -> ("firstAttachment" ::: Word32) -> ("attachmentCount" ::: Word32) -> ("pColorWriteMasks" ::: Ptr ColorComponentFlags) -> IO ())
  , pVkCmdSetRasterizationStreamEXT :: FunPtr (Ptr CommandBuffer_T -> ("rasterizationStream" ::: Word32) -> IO ())
  , pVkCmdSetConservativeRasterizationModeEXT :: FunPtr (Ptr CommandBuffer_T -> ConservativeRasterizationModeEXT -> IO ())
  , pVkCmdSetExtraPrimitiveOverestimationSizeEXT :: FunPtr (Ptr CommandBuffer_T -> ("extraPrimitiveOverestimationSize" ::: CFloat) -> IO ())
  , pVkCmdSetDepthClipEnableEXT :: FunPtr (Ptr CommandBuffer_T -> ("depthClipEnable" ::: Bool32) -> IO ())
  , pVkCmdSetSampleLocationsEnableEXT :: FunPtr (Ptr CommandBuffer_T -> ("sampleLocationsEnable" ::: Bool32) -> IO ())
  , pVkCmdSetColorBlendAdvancedEXT :: FunPtr (Ptr CommandBuffer_T -> ("firstAttachment" ::: Word32) -> ("attachmentCount" ::: Word32) -> ("pColorBlendAdvanced" ::: Ptr ColorBlendAdvancedEXT) -> IO ())
  , pVkCmdSetProvokingVertexModeEXT :: FunPtr (Ptr CommandBuffer_T -> ProvokingVertexModeEXT -> IO ())
  , pVkCmdSetLineRasterizationModeEXT :: FunPtr (Ptr CommandBuffer_T -> LineRasterizationModeEXT -> IO ())
  , pVkCmdSetLineStippleEnableEXT :: FunPtr (Ptr CommandBuffer_T -> ("stippledLineEnable" ::: Bool32) -> IO ())
  , pVkCmdSetDepthClipNegativeOneToOneEXT :: FunPtr (Ptr CommandBuffer_T -> ("negativeOneToOne" ::: Bool32) -> IO ())
  , pVkCmdSetViewportWScalingEnableNV :: FunPtr (Ptr CommandBuffer_T -> ("viewportWScalingEnable" ::: Bool32) -> IO ())
  , pVkCmdSetViewportSwizzleNV :: FunPtr (Ptr CommandBuffer_T -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportSwizzles" ::: Ptr ViewportSwizzleNV) -> IO ())
  , pVkCmdSetCoverageToColorEnableNV :: FunPtr (Ptr CommandBuffer_T -> ("coverageToColorEnable" ::: Bool32) -> IO ())
  , pVkCmdSetCoverageToColorLocationNV :: FunPtr (Ptr CommandBuffer_T -> ("coverageToColorLocation" ::: Word32) -> IO ())
  , pVkCmdSetCoverageModulationModeNV :: FunPtr (Ptr CommandBuffer_T -> CoverageModulationModeNV -> IO ())
  , pVkCmdSetCoverageModulationTableEnableNV :: FunPtr (Ptr CommandBuffer_T -> ("coverageModulationTableEnable" ::: Bool32) -> IO ())
  , pVkCmdSetCoverageModulationTableNV :: FunPtr (Ptr CommandBuffer_T -> ("coverageModulationTableCount" ::: Word32) -> ("pCoverageModulationTable" ::: Ptr CFloat) -> IO ())
  , pVkCmdSetShadingRateImageEnableNV :: FunPtr (Ptr CommandBuffer_T -> ("shadingRateImageEnable" ::: Bool32) -> IO ())
  , pVkCmdSetCoverageReductionModeNV :: FunPtr (Ptr CommandBuffer_T -> CoverageReductionModeNV -> IO ())
  , pVkCmdSetRepresentativeFragmentTestEnableNV :: FunPtr (Ptr CommandBuffer_T -> ("representativeFragmentTestEnable" ::: Bool32) -> IO ())
  , pVkCreatePrivateDataSlot :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr PrivateDataSlotCreateInfo) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPrivateDataSlot" ::: Ptr PrivateDataSlot) -> IO Result)
  , pVkDestroyPrivateDataSlot :: FunPtr (Ptr Device_T -> PrivateDataSlot -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkSetPrivateData :: FunPtr (Ptr Device_T -> ObjectType -> ("objectHandle" ::: Word64) -> PrivateDataSlot -> ("data" ::: Word64) -> IO Result)
  , pVkGetPrivateData :: FunPtr (Ptr Device_T -> ObjectType -> ("objectHandle" ::: Word64) -> PrivateDataSlot -> ("pData" ::: Ptr Word64) -> IO ())
  , pVkCmdCopyBuffer2 :: FunPtr (Ptr CommandBuffer_T -> ("pCopyBufferInfo" ::: Ptr CopyBufferInfo2) -> IO ())
  , pVkCmdCopyImage2 :: FunPtr (Ptr CommandBuffer_T -> ("pCopyImageInfo" ::: Ptr CopyImageInfo2) -> IO ())
  , pVkCmdBlitImage2 :: FunPtr (Ptr CommandBuffer_T -> ("pBlitImageInfo" ::: Ptr BlitImageInfo2) -> IO ())
  , pVkCmdCopyBufferToImage2 :: FunPtr (Ptr CommandBuffer_T -> ("pCopyBufferToImageInfo" ::: Ptr CopyBufferToImageInfo2) -> IO ())
  , pVkCmdCopyImageToBuffer2 :: FunPtr (Ptr CommandBuffer_T -> ("pCopyImageToBufferInfo" ::: Ptr CopyImageToBufferInfo2) -> IO ())
  , pVkCmdResolveImage2 :: FunPtr (Ptr CommandBuffer_T -> ("pResolveImageInfo" ::: Ptr ResolveImageInfo2) -> IO ())
  , pVkCmdSetFragmentShadingRateKHR :: FunPtr (Ptr CommandBuffer_T -> ("pFragmentSize" ::: Ptr Extent2D) -> ("combinerOps" ::: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)) -> IO ())
  , pVkCmdSetFragmentShadingRateEnumNV :: FunPtr (Ptr CommandBuffer_T -> FragmentShadingRateNV -> ("combinerOps" ::: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)) -> IO ())
  , pVkGetAccelerationStructureBuildSizesKHR :: FunPtr (Ptr Device_T -> AccelerationStructureBuildTypeKHR -> ("pBuildInfo" ::: Ptr AccelerationStructureBuildGeometryInfoKHR) -> ("pMaxPrimitiveCounts" ::: Ptr Word32) -> ("pSizeInfo" ::: Ptr AccelerationStructureBuildSizesInfoKHR) -> IO ())
  , pVkCmdSetVertexInputEXT :: FunPtr (Ptr CommandBuffer_T -> ("vertexBindingDescriptionCount" ::: Word32) -> ("pVertexBindingDescriptions" ::: Ptr VertexInputBindingDescription2EXT) -> ("vertexAttributeDescriptionCount" ::: Word32) -> ("pVertexAttributeDescriptions" ::: Ptr VertexInputAttributeDescription2EXT) -> IO ())
  , pVkCmdSetColorWriteEnableEXT :: FunPtr (Ptr CommandBuffer_T -> ("attachmentCount" ::: Word32) -> ("pColorWriteEnables" ::: Ptr Bool32) -> IO ())
  , pVkCmdSetEvent2 :: FunPtr (Ptr CommandBuffer_T -> Event -> ("pDependencyInfo" ::: Ptr DependencyInfo) -> IO ())
  , pVkCmdResetEvent2 :: FunPtr (Ptr CommandBuffer_T -> Event -> ("stageMask" ::: PipelineStageFlags2) -> IO ())
  , pVkCmdWaitEvents2 :: FunPtr (Ptr CommandBuffer_T -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr Event) -> ("pDependencyInfos" ::: Ptr DependencyInfo) -> IO ())
  , pVkCmdPipelineBarrier2 :: FunPtr (Ptr CommandBuffer_T -> ("pDependencyInfo" ::: Ptr DependencyInfo) -> IO ())
  , pVkQueueSubmit2 :: FunPtr (Ptr Queue_T -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr (SomeStruct SubmitInfo2)) -> Fence -> IO Result)
  , pVkCmdWriteTimestamp2 :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlags2 -> QueryPool -> ("query" ::: Word32) -> IO ())
  , pVkCmdWriteBufferMarker2AMD :: FunPtr (Ptr CommandBuffer_T -> PipelineStageFlags2 -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("marker" ::: Word32) -> IO ())
  , pVkGetQueueCheckpointData2NV :: FunPtr (Ptr Queue_T -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr CheckpointData2NV) -> IO ())
  , pVkCmdDecompressMemoryNV :: FunPtr (Ptr CommandBuffer_T -> ("decompressRegionCount" ::: Word32) -> ("pDecompressMemoryRegions" ::: Ptr DecompressMemoryRegionNV) -> IO ())
  , pVkCmdDecompressMemoryIndirectCountNV :: FunPtr (Ptr CommandBuffer_T -> ("indirectCommandsAddress" ::: DeviceAddress) -> ("indirectCommandsCountAddress" ::: DeviceAddress) -> ("stride" ::: Word32) -> IO ())
  , pVkCreateCuModuleNVX :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr CuModuleCreateInfoNVX) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pModule" ::: Ptr CuModuleNVX) -> IO Result)
  , pVkCreateCuFunctionNVX :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr CuFunctionCreateInfoNVX) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pFunction" ::: Ptr CuFunctionNVX) -> IO Result)
  , pVkDestroyCuModuleNVX :: FunPtr (Ptr Device_T -> CuModuleNVX -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkDestroyCuFunctionNVX :: FunPtr (Ptr Device_T -> CuFunctionNVX -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCmdCuLaunchKernelNVX :: FunPtr (Ptr CommandBuffer_T -> ("pLaunchInfo" ::: Ptr CuLaunchInfoNVX) -> IO ())
  , pVkGetDescriptorSetLayoutSizeEXT :: FunPtr (Ptr Device_T -> DescriptorSetLayout -> ("pLayoutSizeInBytes" ::: Ptr DeviceSize) -> IO ())
  , pVkGetDescriptorSetLayoutBindingOffsetEXT :: FunPtr (Ptr Device_T -> DescriptorSetLayout -> ("binding" ::: Word32) -> ("pOffset" ::: Ptr DeviceSize) -> IO ())
  , pVkGetDescriptorEXT :: FunPtr (Ptr Device_T -> ("pDescriptorInfo" ::: Ptr DescriptorGetInfoEXT) -> ("dataSize" ::: CSize) -> ("pDescriptor" ::: Ptr ()) -> IO ())
  , pVkCmdBindDescriptorBuffersEXT :: FunPtr (Ptr CommandBuffer_T -> ("bufferCount" ::: Word32) -> ("pBindingInfos" ::: Ptr (SomeStruct DescriptorBufferBindingInfoEXT)) -> IO ())
  , pVkCmdSetDescriptorBufferOffsetsEXT :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> ("firstSet" ::: Word32) -> ("setCount" ::: Word32) -> ("pBufferIndices" ::: Ptr Word32) -> ("pOffsets" ::: Ptr DeviceSize) -> IO ())
  , pVkCmdBindDescriptorBufferEmbeddedSamplersEXT :: FunPtr (Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> ("set" ::: Word32) -> IO ())
  , pVkGetBufferOpaqueCaptureDescriptorDataEXT :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr BufferCaptureDescriptorDataInfoEXT) -> ("pData" ::: Ptr ()) -> IO Result)
  , pVkGetImageOpaqueCaptureDescriptorDataEXT :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr ImageCaptureDescriptorDataInfoEXT) -> ("pData" ::: Ptr ()) -> IO Result)
  , pVkGetImageViewOpaqueCaptureDescriptorDataEXT :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr ImageViewCaptureDescriptorDataInfoEXT) -> ("pData" ::: Ptr ()) -> IO Result)
  , pVkGetSamplerOpaqueCaptureDescriptorDataEXT :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr SamplerCaptureDescriptorDataInfoEXT) -> ("pData" ::: Ptr ()) -> IO Result)
  , pVkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT :: FunPtr (Ptr Device_T -> ("pInfo" ::: Ptr AccelerationStructureCaptureDescriptorDataInfoEXT) -> ("pData" ::: Ptr ()) -> IO Result)
  , pVkSetDeviceMemoryPriorityEXT :: FunPtr (Ptr Device_T -> DeviceMemory -> ("priority" ::: CFloat) -> IO ())
  , pVkWaitForPresentKHR :: FunPtr (Ptr Device_T -> SwapchainKHR -> ("presentId" ::: Word64) -> ("timeout" ::: Word64) -> IO Result)
  , pVkCreateBufferCollectionFUCHSIA :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr BufferCollectionCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pCollection" ::: Ptr BufferCollectionFUCHSIA) -> IO Result)
  , pVkSetBufferCollectionBufferConstraintsFUCHSIA :: FunPtr (Ptr Device_T -> BufferCollectionFUCHSIA -> ("pBufferConstraintsInfo" ::: Ptr BufferConstraintsInfoFUCHSIA) -> IO Result)
  , pVkSetBufferCollectionImageConstraintsFUCHSIA :: FunPtr (Ptr Device_T -> BufferCollectionFUCHSIA -> ("pImageConstraintsInfo" ::: Ptr ImageConstraintsInfoFUCHSIA) -> IO Result)
  , pVkDestroyBufferCollectionFUCHSIA :: FunPtr (Ptr Device_T -> BufferCollectionFUCHSIA -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkGetBufferCollectionPropertiesFUCHSIA :: FunPtr (Ptr Device_T -> BufferCollectionFUCHSIA -> ("pProperties" ::: Ptr BufferCollectionPropertiesFUCHSIA) -> IO Result)
  , pVkCmdBeginRendering :: FunPtr (Ptr CommandBuffer_T -> ("pRenderingInfo" ::: Ptr (SomeStruct RenderingInfo)) -> IO ())
  , pVkCmdEndRendering :: FunPtr (Ptr CommandBuffer_T -> IO ())
  , pVkGetDescriptorSetLayoutHostMappingInfoVALVE :: FunPtr (Ptr Device_T -> ("pBindingReference" ::: Ptr DescriptorSetBindingReferenceVALVE) -> ("pHostMapping" ::: Ptr DescriptorSetLayoutHostMappingInfoVALVE) -> IO ())
  , pVkGetDescriptorSetHostMappingVALVE :: FunPtr (Ptr Device_T -> DescriptorSet -> ("ppData" ::: Ptr (Ptr ())) -> IO ())
  , pVkCreateMicromapEXT :: FunPtr (Ptr Device_T -> Ptr MicromapCreateInfoEXT -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pMicromap" ::: Ptr MicromapEXT) -> IO Result)
  , pVkCmdBuildMicromapsEXT :: FunPtr (Ptr CommandBuffer_T -> ("infoCount" ::: Word32) -> ("pInfos" ::: Ptr MicromapBuildInfoEXT) -> IO ())
  , pVkBuildMicromapsEXT :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> ("infoCount" ::: Word32) -> ("pInfos" ::: Ptr MicromapBuildInfoEXT) -> IO Result)
  , pVkDestroyMicromapEXT :: FunPtr (Ptr Device_T -> MicromapEXT -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkCmdCopyMicromapEXT :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyMicromapInfoEXT -> IO ())
  , pVkCopyMicromapEXT :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMicromapInfoEXT -> IO Result)
  , pVkCmdCopyMicromapToMemoryEXT :: FunPtr (Ptr CommandBuffer_T -> ("pInfo" ::: Ptr CopyMicromapToMemoryInfoEXT) -> IO ())
  , pVkCopyMicromapToMemoryEXT :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> ("pInfo" ::: Ptr CopyMicromapToMemoryInfoEXT) -> IO Result)
  , pVkCmdCopyMemoryToMicromapEXT :: FunPtr (Ptr CommandBuffer_T -> Ptr CopyMemoryToMicromapInfoEXT -> IO ())
  , pVkCopyMemoryToMicromapEXT :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMemoryToMicromapInfoEXT -> IO Result)
  , pVkCmdWriteMicromapsPropertiesEXT :: FunPtr (Ptr CommandBuffer_T -> ("micromapCount" ::: Word32) -> ("pMicromaps" ::: Ptr MicromapEXT) -> QueryType -> QueryPool -> ("firstQuery" ::: Word32) -> IO ())
  , pVkWriteMicromapsPropertiesEXT :: FunPtr (Ptr Device_T -> ("micromapCount" ::: Word32) -> ("pMicromaps" ::: Ptr MicromapEXT) -> QueryType -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: CSize) -> IO Result)
  , pVkGetDeviceMicromapCompatibilityEXT :: FunPtr (Ptr Device_T -> Ptr MicromapVersionInfoEXT -> ("pCompatibility" ::: Ptr AccelerationStructureCompatibilityKHR) -> IO ())
  , pVkGetMicromapBuildSizesEXT :: FunPtr (Ptr Device_T -> AccelerationStructureBuildTypeKHR -> Ptr MicromapBuildInfoEXT -> ("pSizeInfo" ::: Ptr MicromapBuildSizesInfoEXT) -> IO ())
  , pVkGetShaderModuleIdentifierEXT :: FunPtr (Ptr Device_T -> ShaderModule -> ("pIdentifier" ::: Ptr ShaderModuleIdentifierEXT) -> IO ())
  , pVkGetShaderModuleCreateInfoIdentifierEXT :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct ShaderModuleCreateInfo)) -> ("pIdentifier" ::: Ptr ShaderModuleIdentifierEXT) -> IO ())
  , pVkGetImageSubresourceLayout2EXT :: FunPtr (Ptr Device_T -> Image -> ("pSubresource" ::: Ptr ImageSubresource2EXT) -> ("pLayout" ::: Ptr (SomeStruct SubresourceLayout2EXT)) -> IO ())
  , pVkGetPipelinePropertiesEXT :: FunPtr (Ptr Device_T -> ("pPipelineInfo" ::: Ptr PipelineInfoEXT) -> ("pPipelineProperties" ::: Ptr BaseOutStructure) -> IO Result)
  , pVkExportMetalObjectsEXT :: FunPtr (Ptr Device_T -> ("pMetalObjectsInfo" ::: Ptr (SomeStruct ExportMetalObjectsInfoEXT)) -> IO ())
  , pVkGetFramebufferTilePropertiesQCOM :: FunPtr (Ptr Device_T -> Framebuffer -> ("pPropertiesCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr TilePropertiesQCOM) -> IO Result)
  , pVkGetDynamicRenderingTilePropertiesQCOM :: FunPtr (Ptr Device_T -> ("pRenderingInfo" ::: Ptr (SomeStruct RenderingInfo)) -> ("pProperties" ::: Ptr TilePropertiesQCOM) -> IO Result)
  , pVkCreateOpticalFlowSessionNV :: FunPtr (Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct OpticalFlowSessionCreateInfoNV)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSession" ::: Ptr OpticalFlowSessionNV) -> IO Result)
  , pVkDestroyOpticalFlowSessionNV :: FunPtr (Ptr Device_T -> OpticalFlowSessionNV -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ())
  , pVkBindOpticalFlowSessionImageNV :: FunPtr (Ptr Device_T -> OpticalFlowSessionNV -> OpticalFlowSessionBindingPointNV -> ImageView -> ImageLayout -> IO Result)
  , pVkCmdOpticalFlowExecuteNV :: FunPtr (Ptr CommandBuffer_T -> OpticalFlowSessionNV -> ("pExecuteInfo" ::: Ptr OpticalFlowExecuteInfoNV) -> IO ())
  , pVkGetDeviceFaultInfoEXT :: FunPtr (Ptr Device_T -> ("pFaultCounts" ::: Ptr DeviceFaultCountsEXT) -> ("pFaultInfo" ::: Ptr DeviceFaultInfoEXT) -> IO Result)
  }

deriving instance Eq DeviceCmds
deriving instance Show DeviceCmds
instance Zero DeviceCmds where
  zero = DeviceCmds
    nullPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr
    nullFunPtr nullFunPtr nullFunPtr nullFunPtr nullFunPtr

foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceProcAddr
  :: FunPtr (Ptr Device_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) -> Ptr Device_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction

initDeviceCmds :: InstanceCmds -> Ptr Device_T -> IO DeviceCmds
initDeviceCmds instanceCmds handle = do
  pGetDeviceProcAddr <- castFunPtr @_ @(Ptr Device_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
      <$> getInstanceProcAddr' (instanceCmdsHandle instanceCmds) (GHC.Ptr.Ptr "vkGetDeviceProcAddr"#)
  let getDeviceProcAddr' = mkVkGetDeviceProcAddr pGetDeviceProcAddr
      getFirstDeviceProcAddr = \case
        []   -> pure nullFunPtr
        x:xs -> do
          p <- getDeviceProcAddr' handle x
          if p /= nullFunPtr
            then pure p
            else getFirstDeviceProcAddr xs
  vkGetDeviceProcAddr <- getDeviceProcAddr' handle (Ptr "vkGetDeviceProcAddr"#)
  vkDestroyDevice <- getDeviceProcAddr' handle (Ptr "vkDestroyDevice"#)
  vkGetDeviceQueue <- getDeviceProcAddr' handle (Ptr "vkGetDeviceQueue"#)
  vkQueueSubmit <- getDeviceProcAddr' handle (Ptr "vkQueueSubmit"#)
  vkQueueWaitIdle <- getDeviceProcAddr' handle (Ptr "vkQueueWaitIdle"#)
  vkDeviceWaitIdle <- getDeviceProcAddr' handle (Ptr "vkDeviceWaitIdle"#)
  vkAllocateMemory <- getDeviceProcAddr' handle (Ptr "vkAllocateMemory"#)
  vkFreeMemory <- getDeviceProcAddr' handle (Ptr "vkFreeMemory"#)
  vkMapMemory <- getDeviceProcAddr' handle (Ptr "vkMapMemory"#)
  vkUnmapMemory <- getDeviceProcAddr' handle (Ptr "vkUnmapMemory"#)
  vkFlushMappedMemoryRanges <- getDeviceProcAddr' handle (Ptr "vkFlushMappedMemoryRanges"#)
  vkInvalidateMappedMemoryRanges <- getDeviceProcAddr' handle (Ptr "vkInvalidateMappedMemoryRanges"#)
  vkGetDeviceMemoryCommitment <- getDeviceProcAddr' handle (Ptr "vkGetDeviceMemoryCommitment"#)
  vkGetBufferMemoryRequirements <- getDeviceProcAddr' handle (Ptr "vkGetBufferMemoryRequirements"#)
  vkBindBufferMemory <- getDeviceProcAddr' handle (Ptr "vkBindBufferMemory"#)
  vkGetImageMemoryRequirements <- getDeviceProcAddr' handle (Ptr "vkGetImageMemoryRequirements"#)
  vkBindImageMemory <- getDeviceProcAddr' handle (Ptr "vkBindImageMemory"#)
  vkGetImageSparseMemoryRequirements <- getDeviceProcAddr' handle (Ptr "vkGetImageSparseMemoryRequirements"#)
  vkQueueBindSparse <- getDeviceProcAddr' handle (Ptr "vkQueueBindSparse"#)
  vkCreateFence <- getDeviceProcAddr' handle (Ptr "vkCreateFence"#)
  vkDestroyFence <- getDeviceProcAddr' handle (Ptr "vkDestroyFence"#)
  vkResetFences <- getDeviceProcAddr' handle (Ptr "vkResetFences"#)
  vkGetFenceStatus <- getDeviceProcAddr' handle (Ptr "vkGetFenceStatus"#)
  vkWaitForFences <- getDeviceProcAddr' handle (Ptr "vkWaitForFences"#)
  vkCreateSemaphore <- getDeviceProcAddr' handle (Ptr "vkCreateSemaphore"#)
  vkDestroySemaphore <- getDeviceProcAddr' handle (Ptr "vkDestroySemaphore"#)
  vkCreateEvent <- getDeviceProcAddr' handle (Ptr "vkCreateEvent"#)
  vkDestroyEvent <- getDeviceProcAddr' handle (Ptr "vkDestroyEvent"#)
  vkGetEventStatus <- getDeviceProcAddr' handle (Ptr "vkGetEventStatus"#)
  vkSetEvent <- getDeviceProcAddr' handle (Ptr "vkSetEvent"#)
  vkResetEvent <- getDeviceProcAddr' handle (Ptr "vkResetEvent"#)
  vkCreateQueryPool <- getDeviceProcAddr' handle (Ptr "vkCreateQueryPool"#)
  vkDestroyQueryPool <- getDeviceProcAddr' handle (Ptr "vkDestroyQueryPool"#)
  vkGetQueryPoolResults <- getDeviceProcAddr' handle (Ptr "vkGetQueryPoolResults"#)
  vkResetQueryPool <- getFirstDeviceProcAddr [ (Ptr "vkResetQueryPoolEXT"#)
                                             , (Ptr "vkResetQueryPool"#) ]
  vkCreateBuffer <- getDeviceProcAddr' handle (Ptr "vkCreateBuffer"#)
  vkDestroyBuffer <- getDeviceProcAddr' handle (Ptr "vkDestroyBuffer"#)
  vkCreateBufferView <- getDeviceProcAddr' handle (Ptr "vkCreateBufferView"#)
  vkDestroyBufferView <- getDeviceProcAddr' handle (Ptr "vkDestroyBufferView"#)
  vkCreateImage <- getDeviceProcAddr' handle (Ptr "vkCreateImage"#)
  vkDestroyImage <- getDeviceProcAddr' handle (Ptr "vkDestroyImage"#)
  vkGetImageSubresourceLayout <- getDeviceProcAddr' handle (Ptr "vkGetImageSubresourceLayout"#)
  vkCreateImageView <- getDeviceProcAddr' handle (Ptr "vkCreateImageView"#)
  vkDestroyImageView <- getDeviceProcAddr' handle (Ptr "vkDestroyImageView"#)
  vkCreateShaderModule <- getDeviceProcAddr' handle (Ptr "vkCreateShaderModule"#)
  vkDestroyShaderModule <- getDeviceProcAddr' handle (Ptr "vkDestroyShaderModule"#)
  vkCreatePipelineCache <- getDeviceProcAddr' handle (Ptr "vkCreatePipelineCache"#)
  vkDestroyPipelineCache <- getDeviceProcAddr' handle (Ptr "vkDestroyPipelineCache"#)
  vkGetPipelineCacheData <- getDeviceProcAddr' handle (Ptr "vkGetPipelineCacheData"#)
  vkMergePipelineCaches <- getDeviceProcAddr' handle (Ptr "vkMergePipelineCaches"#)
  vkCreateGraphicsPipelines <- getDeviceProcAddr' handle (Ptr "vkCreateGraphicsPipelines"#)
  vkCreateComputePipelines <- getDeviceProcAddr' handle (Ptr "vkCreateComputePipelines"#)
  vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI <- getDeviceProcAddr' handle (Ptr "vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI"#)
  vkDestroyPipeline <- getDeviceProcAddr' handle (Ptr "vkDestroyPipeline"#)
  vkCreatePipelineLayout <- getDeviceProcAddr' handle (Ptr "vkCreatePipelineLayout"#)
  vkDestroyPipelineLayout <- getDeviceProcAddr' handle (Ptr "vkDestroyPipelineLayout"#)
  vkCreateSampler <- getDeviceProcAddr' handle (Ptr "vkCreateSampler"#)
  vkDestroySampler <- getDeviceProcAddr' handle (Ptr "vkDestroySampler"#)
  vkCreateDescriptorSetLayout <- getDeviceProcAddr' handle (Ptr "vkCreateDescriptorSetLayout"#)
  vkDestroyDescriptorSetLayout <- getDeviceProcAddr' handle (Ptr "vkDestroyDescriptorSetLayout"#)
  vkCreateDescriptorPool <- getDeviceProcAddr' handle (Ptr "vkCreateDescriptorPool"#)
  vkDestroyDescriptorPool <- getDeviceProcAddr' handle (Ptr "vkDestroyDescriptorPool"#)
  vkResetDescriptorPool <- getDeviceProcAddr' handle (Ptr "vkResetDescriptorPool"#)
  vkAllocateDescriptorSets <- getDeviceProcAddr' handle (Ptr "vkAllocateDescriptorSets"#)
  vkFreeDescriptorSets <- getDeviceProcAddr' handle (Ptr "vkFreeDescriptorSets"#)
  vkUpdateDescriptorSets <- getDeviceProcAddr' handle (Ptr "vkUpdateDescriptorSets"#)
  vkCreateFramebuffer <- getDeviceProcAddr' handle (Ptr "vkCreateFramebuffer"#)
  vkDestroyFramebuffer <- getDeviceProcAddr' handle (Ptr "vkDestroyFramebuffer"#)
  vkCreateRenderPass <- getDeviceProcAddr' handle (Ptr "vkCreateRenderPass"#)
  vkDestroyRenderPass <- getDeviceProcAddr' handle (Ptr "vkDestroyRenderPass"#)
  vkGetRenderAreaGranularity <- getDeviceProcAddr' handle (Ptr "vkGetRenderAreaGranularity"#)
  vkCreateCommandPool <- getDeviceProcAddr' handle (Ptr "vkCreateCommandPool"#)
  vkDestroyCommandPool <- getDeviceProcAddr' handle (Ptr "vkDestroyCommandPool"#)
  vkResetCommandPool <- getDeviceProcAddr' handle (Ptr "vkResetCommandPool"#)
  vkAllocateCommandBuffers <- getDeviceProcAddr' handle (Ptr "vkAllocateCommandBuffers"#)
  vkFreeCommandBuffers <- getDeviceProcAddr' handle (Ptr "vkFreeCommandBuffers"#)
  vkBeginCommandBuffer <- getDeviceProcAddr' handle (Ptr "vkBeginCommandBuffer"#)
  vkEndCommandBuffer <- getDeviceProcAddr' handle (Ptr "vkEndCommandBuffer"#)
  vkResetCommandBuffer <- getDeviceProcAddr' handle (Ptr "vkResetCommandBuffer"#)
  vkCmdBindPipeline <- getDeviceProcAddr' handle (Ptr "vkCmdBindPipeline"#)
  vkCmdSetViewport <- getDeviceProcAddr' handle (Ptr "vkCmdSetViewport"#)
  vkCmdSetScissor <- getDeviceProcAddr' handle (Ptr "vkCmdSetScissor"#)
  vkCmdSetLineWidth <- getDeviceProcAddr' handle (Ptr "vkCmdSetLineWidth"#)
  vkCmdSetDepthBias <- getDeviceProcAddr' handle (Ptr "vkCmdSetDepthBias"#)
  vkCmdSetBlendConstants <- getDeviceProcAddr' handle (Ptr "vkCmdSetBlendConstants"#)
  vkCmdSetDepthBounds <- getDeviceProcAddr' handle (Ptr "vkCmdSetDepthBounds"#)
  vkCmdSetStencilCompareMask <- getDeviceProcAddr' handle (Ptr "vkCmdSetStencilCompareMask"#)
  vkCmdSetStencilWriteMask <- getDeviceProcAddr' handle (Ptr "vkCmdSetStencilWriteMask"#)
  vkCmdSetStencilReference <- getDeviceProcAddr' handle (Ptr "vkCmdSetStencilReference"#)
  vkCmdBindDescriptorSets <- getDeviceProcAddr' handle (Ptr "vkCmdBindDescriptorSets"#)
  vkCmdBindIndexBuffer <- getDeviceProcAddr' handle (Ptr "vkCmdBindIndexBuffer"#)
  vkCmdBindVertexBuffers <- getDeviceProcAddr' handle (Ptr "vkCmdBindVertexBuffers"#)
  vkCmdDraw <- getDeviceProcAddr' handle (Ptr "vkCmdDraw"#)
  vkCmdDrawIndexed <- getDeviceProcAddr' handle (Ptr "vkCmdDrawIndexed"#)
  vkCmdDrawMultiEXT <- getDeviceProcAddr' handle (Ptr "vkCmdDrawMultiEXT"#)
  vkCmdDrawMultiIndexedEXT <- getDeviceProcAddr' handle (Ptr "vkCmdDrawMultiIndexedEXT"#)
  vkCmdDrawIndirect <- getDeviceProcAddr' handle (Ptr "vkCmdDrawIndirect"#)
  vkCmdDrawIndexedIndirect <- getDeviceProcAddr' handle (Ptr "vkCmdDrawIndexedIndirect"#)
  vkCmdDispatch <- getDeviceProcAddr' handle (Ptr "vkCmdDispatch"#)
  vkCmdDispatchIndirect <- getDeviceProcAddr' handle (Ptr "vkCmdDispatchIndirect"#)
  vkCmdSubpassShadingHUAWEI <- getDeviceProcAddr' handle (Ptr "vkCmdSubpassShadingHUAWEI"#)
  vkCmdCopyBuffer <- getDeviceProcAddr' handle (Ptr "vkCmdCopyBuffer"#)
  vkCmdCopyImage <- getDeviceProcAddr' handle (Ptr "vkCmdCopyImage"#)
  vkCmdBlitImage <- getDeviceProcAddr' handle (Ptr "vkCmdBlitImage"#)
  vkCmdCopyBufferToImage <- getDeviceProcAddr' handle (Ptr "vkCmdCopyBufferToImage"#)
  vkCmdCopyImageToBuffer <- getDeviceProcAddr' handle (Ptr "vkCmdCopyImageToBuffer"#)
  vkCmdCopyMemoryIndirectNV <- getDeviceProcAddr' handle (Ptr "vkCmdCopyMemoryIndirectNV"#)
  vkCmdCopyMemoryToImageIndirectNV <- getDeviceProcAddr' handle (Ptr "vkCmdCopyMemoryToImageIndirectNV"#)
  vkCmdUpdateBuffer <- getDeviceProcAddr' handle (Ptr "vkCmdUpdateBuffer"#)
  vkCmdFillBuffer <- getDeviceProcAddr' handle (Ptr "vkCmdFillBuffer"#)
  vkCmdClearColorImage <- getDeviceProcAddr' handle (Ptr "vkCmdClearColorImage"#)
  vkCmdClearDepthStencilImage <- getDeviceProcAddr' handle (Ptr "vkCmdClearDepthStencilImage"#)
  vkCmdClearAttachments <- getDeviceProcAddr' handle (Ptr "vkCmdClearAttachments"#)
  vkCmdResolveImage <- getDeviceProcAddr' handle (Ptr "vkCmdResolveImage"#)
  vkCmdSetEvent <- getDeviceProcAddr' handle (Ptr "vkCmdSetEvent"#)
  vkCmdResetEvent <- getDeviceProcAddr' handle (Ptr "vkCmdResetEvent"#)
  vkCmdWaitEvents <- getDeviceProcAddr' handle (Ptr "vkCmdWaitEvents"#)
  vkCmdPipelineBarrier <- getDeviceProcAddr' handle (Ptr "vkCmdPipelineBarrier"#)
  vkCmdBeginQuery <- getDeviceProcAddr' handle (Ptr "vkCmdBeginQuery"#)
  vkCmdEndQuery <- getDeviceProcAddr' handle (Ptr "vkCmdEndQuery"#)
  vkCmdBeginConditionalRenderingEXT <- getDeviceProcAddr' handle (Ptr "vkCmdBeginConditionalRenderingEXT"#)
  vkCmdEndConditionalRenderingEXT <- getDeviceProcAddr' handle (Ptr "vkCmdEndConditionalRenderingEXT"#)
  vkCmdResetQueryPool <- getDeviceProcAddr' handle (Ptr "vkCmdResetQueryPool"#)
  vkCmdWriteTimestamp <- getDeviceProcAddr' handle (Ptr "vkCmdWriteTimestamp"#)
  vkCmdCopyQueryPoolResults <- getDeviceProcAddr' handle (Ptr "vkCmdCopyQueryPoolResults"#)
  vkCmdPushConstants <- getDeviceProcAddr' handle (Ptr "vkCmdPushConstants"#)
  vkCmdBeginRenderPass <- getDeviceProcAddr' handle (Ptr "vkCmdBeginRenderPass"#)
  vkCmdNextSubpass <- getDeviceProcAddr' handle (Ptr "vkCmdNextSubpass"#)
  vkCmdEndRenderPass <- getDeviceProcAddr' handle (Ptr "vkCmdEndRenderPass"#)
  vkCmdExecuteCommands <- getDeviceProcAddr' handle (Ptr "vkCmdExecuteCommands"#)
  vkCreateSharedSwapchainsKHR <- getDeviceProcAddr' handle (Ptr "vkCreateSharedSwapchainsKHR"#)
  vkCreateSwapchainKHR <- getDeviceProcAddr' handle (Ptr "vkCreateSwapchainKHR"#)
  vkDestroySwapchainKHR <- getDeviceProcAddr' handle (Ptr "vkDestroySwapchainKHR"#)
  vkGetSwapchainImagesKHR <- getDeviceProcAddr' handle (Ptr "vkGetSwapchainImagesKHR"#)
  vkAcquireNextImageKHR <- getDeviceProcAddr' handle (Ptr "vkAcquireNextImageKHR"#)
  vkQueuePresentKHR <- getDeviceProcAddr' handle (Ptr "vkQueuePresentKHR"#)
  vkDebugMarkerSetObjectNameEXT <- getDeviceProcAddr' handle (Ptr "vkDebugMarkerSetObjectNameEXT"#)
  vkDebugMarkerSetObjectTagEXT <- getDeviceProcAddr' handle (Ptr "vkDebugMarkerSetObjectTagEXT"#)
  vkCmdDebugMarkerBeginEXT <- getDeviceProcAddr' handle (Ptr "vkCmdDebugMarkerBeginEXT"#)
  vkCmdDebugMarkerEndEXT <- getDeviceProcAddr' handle (Ptr "vkCmdDebugMarkerEndEXT"#)
  vkCmdDebugMarkerInsertEXT <- getDeviceProcAddr' handle (Ptr "vkCmdDebugMarkerInsertEXT"#)
  vkGetMemoryWin32HandleNV <- getDeviceProcAddr' handle (Ptr "vkGetMemoryWin32HandleNV"#)
  vkCmdExecuteGeneratedCommandsNV <- getDeviceProcAddr' handle (Ptr "vkCmdExecuteGeneratedCommandsNV"#)
  vkCmdPreprocessGeneratedCommandsNV <- getDeviceProcAddr' handle (Ptr "vkCmdPreprocessGeneratedCommandsNV"#)
  vkCmdBindPipelineShaderGroupNV <- getDeviceProcAddr' handle (Ptr "vkCmdBindPipelineShaderGroupNV"#)
  vkGetGeneratedCommandsMemoryRequirementsNV <- getDeviceProcAddr' handle (Ptr "vkGetGeneratedCommandsMemoryRequirementsNV"#)
  vkCreateIndirectCommandsLayoutNV <- getDeviceProcAddr' handle (Ptr "vkCreateIndirectCommandsLayoutNV"#)
  vkDestroyIndirectCommandsLayoutNV <- getDeviceProcAddr' handle (Ptr "vkDestroyIndirectCommandsLayoutNV"#)
  vkCmdPushDescriptorSetKHR <- getDeviceProcAddr' handle (Ptr "vkCmdPushDescriptorSetKHR"#)
  vkTrimCommandPool <- getFirstDeviceProcAddr [ (Ptr "vkTrimCommandPoolKHR"#)
                                              , (Ptr "vkTrimCommandPool"#) ]
  vkGetMemoryWin32HandleKHR <- getDeviceProcAddr' handle (Ptr "vkGetMemoryWin32HandleKHR"#)
  vkGetMemoryWin32HandlePropertiesKHR <- getDeviceProcAddr' handle (Ptr "vkGetMemoryWin32HandlePropertiesKHR"#)
  vkGetMemoryFdKHR <- getDeviceProcAddr' handle (Ptr "vkGetMemoryFdKHR"#)
  vkGetMemoryFdPropertiesKHR <- getDeviceProcAddr' handle (Ptr "vkGetMemoryFdPropertiesKHR"#)
  vkGetMemoryZirconHandleFUCHSIA <- getDeviceProcAddr' handle (Ptr "vkGetMemoryZirconHandleFUCHSIA"#)
  vkGetMemoryZirconHandlePropertiesFUCHSIA <- getDeviceProcAddr' handle (Ptr "vkGetMemoryZirconHandlePropertiesFUCHSIA"#)
  vkGetMemoryRemoteAddressNV <- getDeviceProcAddr' handle (Ptr "vkGetMemoryRemoteAddressNV"#)
  vkGetSemaphoreWin32HandleKHR <- getDeviceProcAddr' handle (Ptr "vkGetSemaphoreWin32HandleKHR"#)
  vkImportSemaphoreWin32HandleKHR <- getDeviceProcAddr' handle (Ptr "vkImportSemaphoreWin32HandleKHR"#)
  vkGetSemaphoreFdKHR <- getDeviceProcAddr' handle (Ptr "vkGetSemaphoreFdKHR"#)
  vkImportSemaphoreFdKHR <- getDeviceProcAddr' handle (Ptr "vkImportSemaphoreFdKHR"#)
  vkGetSemaphoreZirconHandleFUCHSIA <- getDeviceProcAddr' handle (Ptr "vkGetSemaphoreZirconHandleFUCHSIA"#)
  vkImportSemaphoreZirconHandleFUCHSIA <- getDeviceProcAddr' handle (Ptr "vkImportSemaphoreZirconHandleFUCHSIA"#)
  vkGetFenceWin32HandleKHR <- getDeviceProcAddr' handle (Ptr "vkGetFenceWin32HandleKHR"#)
  vkImportFenceWin32HandleKHR <- getDeviceProcAddr' handle (Ptr "vkImportFenceWin32HandleKHR"#)
  vkGetFenceFdKHR <- getDeviceProcAddr' handle (Ptr "vkGetFenceFdKHR"#)
  vkImportFenceFdKHR <- getDeviceProcAddr' handle (Ptr "vkImportFenceFdKHR"#)
  vkDisplayPowerControlEXT <- getDeviceProcAddr' handle (Ptr "vkDisplayPowerControlEXT"#)
  vkRegisterDeviceEventEXT <- getDeviceProcAddr' handle (Ptr "vkRegisterDeviceEventEXT"#)
  vkRegisterDisplayEventEXT <- getDeviceProcAddr' handle (Ptr "vkRegisterDisplayEventEXT"#)
  vkGetSwapchainCounterEXT <- getDeviceProcAddr' handle (Ptr "vkGetSwapchainCounterEXT"#)
  vkGetDeviceGroupPeerMemoryFeatures <- getFirstDeviceProcAddr [ (Ptr "vkGetDeviceGroupPeerMemoryFeaturesKHR"#)
                                                               , (Ptr "vkGetDeviceGroupPeerMemoryFeatures"#) ]
  vkBindBufferMemory2 <- getFirstDeviceProcAddr [ (Ptr "vkBindBufferMemory2KHR"#)
                                                , (Ptr "vkBindBufferMemory2"#) ]
  vkBindImageMemory2 <- getFirstDeviceProcAddr [ (Ptr "vkBindImageMemory2KHR"#)
                                               , (Ptr "vkBindImageMemory2"#) ]
  vkCmdSetDeviceMask <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetDeviceMaskKHR"#)
                                               , (Ptr "vkCmdSetDeviceMask"#) ]
  vkGetDeviceGroupPresentCapabilitiesKHR <- getDeviceProcAddr' handle (Ptr "vkGetDeviceGroupPresentCapabilitiesKHR"#)
  vkGetDeviceGroupSurfacePresentModesKHR <- getDeviceProcAddr' handle (Ptr "vkGetDeviceGroupSurfacePresentModesKHR"#)
  vkAcquireNextImage2KHR <- getDeviceProcAddr' handle (Ptr "vkAcquireNextImage2KHR"#)
  vkCmdDispatchBase <- getFirstDeviceProcAddr [ (Ptr "vkCmdDispatchBaseKHR"#)
                                              , (Ptr "vkCmdDispatchBase"#) ]
  vkCreateDescriptorUpdateTemplate <- getFirstDeviceProcAddr [ (Ptr "vkCreateDescriptorUpdateTemplateKHR"#)
                                                             , (Ptr "vkCreateDescriptorUpdateTemplate"#) ]
  vkDestroyDescriptorUpdateTemplate <- getFirstDeviceProcAddr [ (Ptr "vkDestroyDescriptorUpdateTemplateKHR"#)
                                                              , (Ptr "vkDestroyDescriptorUpdateTemplate"#) ]
  vkUpdateDescriptorSetWithTemplate <- getFirstDeviceProcAddr [ (Ptr "vkUpdateDescriptorSetWithTemplateKHR"#)
                                                              , (Ptr "vkUpdateDescriptorSetWithTemplate"#) ]
  vkCmdPushDescriptorSetWithTemplateKHR <- getDeviceProcAddr' handle (Ptr "vkCmdPushDescriptorSetWithTemplateKHR"#)
  vkSetHdrMetadataEXT <- getDeviceProcAddr' handle (Ptr "vkSetHdrMetadataEXT"#)
  vkGetSwapchainStatusKHR <- getDeviceProcAddr' handle (Ptr "vkGetSwapchainStatusKHR"#)
  vkGetRefreshCycleDurationGOOGLE <- getDeviceProcAddr' handle (Ptr "vkGetRefreshCycleDurationGOOGLE"#)
  vkGetPastPresentationTimingGOOGLE <- getDeviceProcAddr' handle (Ptr "vkGetPastPresentationTimingGOOGLE"#)
  vkCmdSetViewportWScalingNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetViewportWScalingNV"#)
  vkCmdSetDiscardRectangleEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetDiscardRectangleEXT"#)
  vkCmdSetSampleLocationsEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetSampleLocationsEXT"#)
  vkGetBufferMemoryRequirements2 <- getFirstDeviceProcAddr [ (Ptr "vkGetBufferMemoryRequirements2KHR"#)
                                                           , (Ptr "vkGetBufferMemoryRequirements2"#) ]
  vkGetImageMemoryRequirements2 <- getFirstDeviceProcAddr [ (Ptr "vkGetImageMemoryRequirements2KHR"#)
                                                          , (Ptr "vkGetImageMemoryRequirements2"#) ]
  vkGetImageSparseMemoryRequirements2 <- getFirstDeviceProcAddr [ (Ptr "vkGetImageSparseMemoryRequirements2KHR"#)
                                                                , (Ptr "vkGetImageSparseMemoryRequirements2"#) ]
  vkGetDeviceBufferMemoryRequirements <- getFirstDeviceProcAddr [ (Ptr "vkGetDeviceBufferMemoryRequirementsKHR"#)
                                                                , (Ptr "vkGetDeviceBufferMemoryRequirements"#) ]
  vkGetDeviceImageMemoryRequirements <- getFirstDeviceProcAddr [ (Ptr "vkGetDeviceImageMemoryRequirementsKHR"#)
                                                               , (Ptr "vkGetDeviceImageMemoryRequirements"#) ]
  vkGetDeviceImageSparseMemoryRequirements <- getFirstDeviceProcAddr [ (Ptr "vkGetDeviceImageSparseMemoryRequirementsKHR"#)
                                                                     , (Ptr "vkGetDeviceImageSparseMemoryRequirements"#) ]
  vkCreateSamplerYcbcrConversion <- getFirstDeviceProcAddr [ (Ptr "vkCreateSamplerYcbcrConversionKHR"#)
                                                           , (Ptr "vkCreateSamplerYcbcrConversion"#) ]
  vkDestroySamplerYcbcrConversion <- getFirstDeviceProcAddr [ (Ptr "vkDestroySamplerYcbcrConversionKHR"#)
                                                            , (Ptr "vkDestroySamplerYcbcrConversion"#) ]
  vkGetDeviceQueue2 <- getDeviceProcAddr' handle (Ptr "vkGetDeviceQueue2"#)
  vkCreateValidationCacheEXT <- getDeviceProcAddr' handle (Ptr "vkCreateValidationCacheEXT"#)
  vkDestroyValidationCacheEXT <- getDeviceProcAddr' handle (Ptr "vkDestroyValidationCacheEXT"#)
  vkGetValidationCacheDataEXT <- getDeviceProcAddr' handle (Ptr "vkGetValidationCacheDataEXT"#)
  vkMergeValidationCachesEXT <- getDeviceProcAddr' handle (Ptr "vkMergeValidationCachesEXT"#)
  vkGetDescriptorSetLayoutSupport <- getFirstDeviceProcAddr [ (Ptr "vkGetDescriptorSetLayoutSupportKHR"#)
                                                            , (Ptr "vkGetDescriptorSetLayoutSupport"#) ]
  vkGetShaderInfoAMD <- getDeviceProcAddr' handle (Ptr "vkGetShaderInfoAMD"#)
  vkSetLocalDimmingAMD <- getDeviceProcAddr' handle (Ptr "vkSetLocalDimmingAMD"#)
  vkGetCalibratedTimestampsEXT <- getDeviceProcAddr' handle (Ptr "vkGetCalibratedTimestampsEXT"#)
  vkSetDebugUtilsObjectNameEXT <- getDeviceProcAddr' handle (Ptr "vkSetDebugUtilsObjectNameEXT"#)
  vkSetDebugUtilsObjectTagEXT <- getDeviceProcAddr' handle (Ptr "vkSetDebugUtilsObjectTagEXT"#)
  vkQueueBeginDebugUtilsLabelEXT <- getDeviceProcAddr' handle (Ptr "vkQueueBeginDebugUtilsLabelEXT"#)
  vkQueueEndDebugUtilsLabelEXT <- getDeviceProcAddr' handle (Ptr "vkQueueEndDebugUtilsLabelEXT"#)
  vkQueueInsertDebugUtilsLabelEXT <- getDeviceProcAddr' handle (Ptr "vkQueueInsertDebugUtilsLabelEXT"#)
  vkCmdBeginDebugUtilsLabelEXT <- getDeviceProcAddr' handle (Ptr "vkCmdBeginDebugUtilsLabelEXT"#)
  vkCmdEndDebugUtilsLabelEXT <- getDeviceProcAddr' handle (Ptr "vkCmdEndDebugUtilsLabelEXT"#)
  vkCmdInsertDebugUtilsLabelEXT <- getDeviceProcAddr' handle (Ptr "vkCmdInsertDebugUtilsLabelEXT"#)
  vkGetMemoryHostPointerPropertiesEXT <- getDeviceProcAddr' handle (Ptr "vkGetMemoryHostPointerPropertiesEXT"#)
  vkCmdWriteBufferMarkerAMD <- getDeviceProcAddr' handle (Ptr "vkCmdWriteBufferMarkerAMD"#)
  vkCreateRenderPass2 <- getFirstDeviceProcAddr [ (Ptr "vkCreateRenderPass2KHR"#)
                                                , (Ptr "vkCreateRenderPass2"#) ]
  vkCmdBeginRenderPass2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdBeginRenderPass2KHR"#)
                                                  , (Ptr "vkCmdBeginRenderPass2"#) ]
  vkCmdNextSubpass2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdNextSubpass2KHR"#)
                                              , (Ptr "vkCmdNextSubpass2"#) ]
  vkCmdEndRenderPass2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdEndRenderPass2KHR"#)
                                                , (Ptr "vkCmdEndRenderPass2"#) ]
  vkGetSemaphoreCounterValue <- getFirstDeviceProcAddr [ (Ptr "vkGetSemaphoreCounterValueKHR"#)
                                                       , (Ptr "vkGetSemaphoreCounterValue"#) ]
  vkWaitSemaphores <- getFirstDeviceProcAddr [ (Ptr "vkWaitSemaphoresKHR"#)
                                             , (Ptr "vkWaitSemaphores"#) ]
  vkSignalSemaphore <- getFirstDeviceProcAddr [ (Ptr "vkSignalSemaphoreKHR"#)
                                              , (Ptr "vkSignalSemaphore"#) ]
  vkGetAndroidHardwareBufferPropertiesANDROID <- getDeviceProcAddr' handle (Ptr "vkGetAndroidHardwareBufferPropertiesANDROID"#)
  vkGetMemoryAndroidHardwareBufferANDROID <- getDeviceProcAddr' handle (Ptr "vkGetMemoryAndroidHardwareBufferANDROID"#)
  vkCmdDrawIndirectCount <- getFirstDeviceProcAddr [ (Ptr "vkCmdDrawIndirectCountAMD"#)
                                                   , (Ptr "vkCmdDrawIndirectCountKHR"#)
                                                   , (Ptr "vkCmdDrawIndirectCount"#) ]
  vkCmdDrawIndexedIndirectCount <- getFirstDeviceProcAddr [ (Ptr "vkCmdDrawIndexedIndirectCountAMD"#)
                                                          , (Ptr "vkCmdDrawIndexedIndirectCountKHR"#)
                                                          , (Ptr "vkCmdDrawIndexedIndirectCount"#) ]
  vkCmdSetCheckpointNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetCheckpointNV"#)
  vkGetQueueCheckpointDataNV <- getDeviceProcAddr' handle (Ptr "vkGetQueueCheckpointDataNV"#)
  vkCmdBindTransformFeedbackBuffersEXT <- getDeviceProcAddr' handle (Ptr "vkCmdBindTransformFeedbackBuffersEXT"#)
  vkCmdBeginTransformFeedbackEXT <- getDeviceProcAddr' handle (Ptr "vkCmdBeginTransformFeedbackEXT"#)
  vkCmdEndTransformFeedbackEXT <- getDeviceProcAddr' handle (Ptr "vkCmdEndTransformFeedbackEXT"#)
  vkCmdBeginQueryIndexedEXT <- getDeviceProcAddr' handle (Ptr "vkCmdBeginQueryIndexedEXT"#)
  vkCmdEndQueryIndexedEXT <- getDeviceProcAddr' handle (Ptr "vkCmdEndQueryIndexedEXT"#)
  vkCmdDrawIndirectByteCountEXT <- getDeviceProcAddr' handle (Ptr "vkCmdDrawIndirectByteCountEXT"#)
  vkCmdSetExclusiveScissorNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetExclusiveScissorNV"#)
  vkCmdBindShadingRateImageNV <- getDeviceProcAddr' handle (Ptr "vkCmdBindShadingRateImageNV"#)
  vkCmdSetViewportShadingRatePaletteNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetViewportShadingRatePaletteNV"#)
  vkCmdSetCoarseSampleOrderNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetCoarseSampleOrderNV"#)
  vkCmdDrawMeshTasksNV <- getDeviceProcAddr' handle (Ptr "vkCmdDrawMeshTasksNV"#)
  vkCmdDrawMeshTasksIndirectNV <- getDeviceProcAddr' handle (Ptr "vkCmdDrawMeshTasksIndirectNV"#)
  vkCmdDrawMeshTasksIndirectCountNV <- getDeviceProcAddr' handle (Ptr "vkCmdDrawMeshTasksIndirectCountNV"#)
  vkCmdDrawMeshTasksEXT <- getDeviceProcAddr' handle (Ptr "vkCmdDrawMeshTasksEXT"#)
  vkCmdDrawMeshTasksIndirectEXT <- getDeviceProcAddr' handle (Ptr "vkCmdDrawMeshTasksIndirectEXT"#)
  vkCmdDrawMeshTasksIndirectCountEXT <- getDeviceProcAddr' handle (Ptr "vkCmdDrawMeshTasksIndirectCountEXT"#)
  vkCompileDeferredNV <- getDeviceProcAddr' handle (Ptr "vkCompileDeferredNV"#)
  vkCreateAccelerationStructureNV <- getDeviceProcAddr' handle (Ptr "vkCreateAccelerationStructureNV"#)
  vkCmdBindInvocationMaskHUAWEI <- getDeviceProcAddr' handle (Ptr "vkCmdBindInvocationMaskHUAWEI"#)
  vkDestroyAccelerationStructureKHR <- getDeviceProcAddr' handle (Ptr "vkDestroyAccelerationStructureKHR"#)
  vkDestroyAccelerationStructureNV <- getDeviceProcAddr' handle (Ptr "vkDestroyAccelerationStructureNV"#)
  vkGetAccelerationStructureMemoryRequirementsNV <- getDeviceProcAddr' handle (Ptr "vkGetAccelerationStructureMemoryRequirementsNV"#)
  vkBindAccelerationStructureMemoryNV <- getDeviceProcAddr' handle (Ptr "vkBindAccelerationStructureMemoryNV"#)
  vkCmdCopyAccelerationStructureNV <- getDeviceProcAddr' handle (Ptr "vkCmdCopyAccelerationStructureNV"#)
  vkCmdCopyAccelerationStructureKHR <- getDeviceProcAddr' handle (Ptr "vkCmdCopyAccelerationStructureKHR"#)
  vkCopyAccelerationStructureKHR <- getDeviceProcAddr' handle (Ptr "vkCopyAccelerationStructureKHR"#)
  vkCmdCopyAccelerationStructureToMemoryKHR <- getDeviceProcAddr' handle (Ptr "vkCmdCopyAccelerationStructureToMemoryKHR"#)
  vkCopyAccelerationStructureToMemoryKHR <- getDeviceProcAddr' handle (Ptr "vkCopyAccelerationStructureToMemoryKHR"#)
  vkCmdCopyMemoryToAccelerationStructureKHR <- getDeviceProcAddr' handle (Ptr "vkCmdCopyMemoryToAccelerationStructureKHR"#)
  vkCopyMemoryToAccelerationStructureKHR <- getDeviceProcAddr' handle (Ptr "vkCopyMemoryToAccelerationStructureKHR"#)
  vkCmdWriteAccelerationStructuresPropertiesKHR <- getDeviceProcAddr' handle (Ptr "vkCmdWriteAccelerationStructuresPropertiesKHR"#)
  vkCmdWriteAccelerationStructuresPropertiesNV <- getDeviceProcAddr' handle (Ptr "vkCmdWriteAccelerationStructuresPropertiesNV"#)
  vkCmdBuildAccelerationStructureNV <- getDeviceProcAddr' handle (Ptr "vkCmdBuildAccelerationStructureNV"#)
  vkWriteAccelerationStructuresPropertiesKHR <- getDeviceProcAddr' handle (Ptr "vkWriteAccelerationStructuresPropertiesKHR"#)
  vkCmdTraceRaysKHR <- getDeviceProcAddr' handle (Ptr "vkCmdTraceRaysKHR"#)
  vkCmdTraceRaysNV <- getDeviceProcAddr' handle (Ptr "vkCmdTraceRaysNV"#)
  vkGetRayTracingShaderGroupHandlesKHR <- getFirstDeviceProcAddr [ (Ptr "vkGetRayTracingShaderGroupHandlesNV"#)
                                                                 , (Ptr "vkGetRayTracingShaderGroupHandlesKHR"#) ]
  vkGetRayTracingCaptureReplayShaderGroupHandlesKHR <- getDeviceProcAddr' handle (Ptr "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR"#)
  vkGetAccelerationStructureHandleNV <- getDeviceProcAddr' handle (Ptr "vkGetAccelerationStructureHandleNV"#)
  vkCreateRayTracingPipelinesNV <- getDeviceProcAddr' handle (Ptr "vkCreateRayTracingPipelinesNV"#)
  vkCreateRayTracingPipelinesKHR <- getDeviceProcAddr' handle (Ptr "vkCreateRayTracingPipelinesKHR"#)
  vkCmdTraceRaysIndirectKHR <- getDeviceProcAddr' handle (Ptr "vkCmdTraceRaysIndirectKHR"#)
  vkCmdTraceRaysIndirect2KHR <- getDeviceProcAddr' handle (Ptr "vkCmdTraceRaysIndirect2KHR"#)
  vkGetDeviceAccelerationStructureCompatibilityKHR <- getDeviceProcAddr' handle (Ptr "vkGetDeviceAccelerationStructureCompatibilityKHR"#)
  vkGetRayTracingShaderGroupStackSizeKHR <- getDeviceProcAddr' handle (Ptr "vkGetRayTracingShaderGroupStackSizeKHR"#)
  vkCmdSetRayTracingPipelineStackSizeKHR <- getDeviceProcAddr' handle (Ptr "vkCmdSetRayTracingPipelineStackSizeKHR"#)
  vkGetImageViewHandleNVX <- getDeviceProcAddr' handle (Ptr "vkGetImageViewHandleNVX"#)
  vkGetImageViewAddressNVX <- getDeviceProcAddr' handle (Ptr "vkGetImageViewAddressNVX"#)
  vkGetDeviceGroupSurfacePresentModes2EXT <- getDeviceProcAddr' handle (Ptr "vkGetDeviceGroupSurfacePresentModes2EXT"#)
  vkAcquireFullScreenExclusiveModeEXT <- getDeviceProcAddr' handle (Ptr "vkAcquireFullScreenExclusiveModeEXT"#)
  vkReleaseFullScreenExclusiveModeEXT <- getDeviceProcAddr' handle (Ptr "vkReleaseFullScreenExclusiveModeEXT"#)
  vkAcquireProfilingLockKHR <- getDeviceProcAddr' handle (Ptr "vkAcquireProfilingLockKHR"#)
  vkReleaseProfilingLockKHR <- getDeviceProcAddr' handle (Ptr "vkReleaseProfilingLockKHR"#)
  vkGetImageDrmFormatModifierPropertiesEXT <- getDeviceProcAddr' handle (Ptr "vkGetImageDrmFormatModifierPropertiesEXT"#)
  vkGetBufferOpaqueCaptureAddress <- getFirstDeviceProcAddr [ (Ptr "vkGetBufferOpaqueCaptureAddressKHR"#)
                                                            , (Ptr "vkGetBufferOpaqueCaptureAddress"#) ]
  vkGetBufferDeviceAddress <- getFirstDeviceProcAddr [ (Ptr "vkGetBufferDeviceAddressEXT"#)
                                                     , (Ptr "vkGetBufferDeviceAddressKHR"#)
                                                     , (Ptr "vkGetBufferDeviceAddress"#) ]
  vkInitializePerformanceApiINTEL <- getDeviceProcAddr' handle (Ptr "vkInitializePerformanceApiINTEL"#)
  vkUninitializePerformanceApiINTEL <- getDeviceProcAddr' handle (Ptr "vkUninitializePerformanceApiINTEL"#)
  vkCmdSetPerformanceMarkerINTEL <- getDeviceProcAddr' handle (Ptr "vkCmdSetPerformanceMarkerINTEL"#)
  vkCmdSetPerformanceStreamMarkerINTEL <- getDeviceProcAddr' handle (Ptr "vkCmdSetPerformanceStreamMarkerINTEL"#)
  vkCmdSetPerformanceOverrideINTEL <- getDeviceProcAddr' handle (Ptr "vkCmdSetPerformanceOverrideINTEL"#)
  vkAcquirePerformanceConfigurationINTEL <- getDeviceProcAddr' handle (Ptr "vkAcquirePerformanceConfigurationINTEL"#)
  vkReleasePerformanceConfigurationINTEL <- getDeviceProcAddr' handle (Ptr "vkReleasePerformanceConfigurationINTEL"#)
  vkQueueSetPerformanceConfigurationINTEL <- getDeviceProcAddr' handle (Ptr "vkQueueSetPerformanceConfigurationINTEL"#)
  vkGetPerformanceParameterINTEL <- getDeviceProcAddr' handle (Ptr "vkGetPerformanceParameterINTEL"#)
  vkGetDeviceMemoryOpaqueCaptureAddress <- getFirstDeviceProcAddr [ (Ptr "vkGetDeviceMemoryOpaqueCaptureAddressKHR"#)
                                                                  , (Ptr "vkGetDeviceMemoryOpaqueCaptureAddress"#) ]
  vkGetPipelineExecutablePropertiesKHR <- getDeviceProcAddr' handle (Ptr "vkGetPipelineExecutablePropertiesKHR"#)
  vkGetPipelineExecutableStatisticsKHR <- getDeviceProcAddr' handle (Ptr "vkGetPipelineExecutableStatisticsKHR"#)
  vkGetPipelineExecutableInternalRepresentationsKHR <- getDeviceProcAddr' handle (Ptr "vkGetPipelineExecutableInternalRepresentationsKHR"#)
  vkCmdSetLineStippleEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetLineStippleEXT"#)
  vkCreateAccelerationStructureKHR <- getDeviceProcAddr' handle (Ptr "vkCreateAccelerationStructureKHR"#)
  vkCmdBuildAccelerationStructuresKHR <- getDeviceProcAddr' handle (Ptr "vkCmdBuildAccelerationStructuresKHR"#)
  vkCmdBuildAccelerationStructuresIndirectKHR <- getDeviceProcAddr' handle (Ptr "vkCmdBuildAccelerationStructuresIndirectKHR"#)
  vkBuildAccelerationStructuresKHR <- getDeviceProcAddr' handle (Ptr "vkBuildAccelerationStructuresKHR"#)
  vkGetAccelerationStructureDeviceAddressKHR <- getDeviceProcAddr' handle (Ptr "vkGetAccelerationStructureDeviceAddressKHR"#)
  vkCreateDeferredOperationKHR <- getDeviceProcAddr' handle (Ptr "vkCreateDeferredOperationKHR"#)
  vkDestroyDeferredOperationKHR <- getDeviceProcAddr' handle (Ptr "vkDestroyDeferredOperationKHR"#)
  vkGetDeferredOperationMaxConcurrencyKHR <- getDeviceProcAddr' handle (Ptr "vkGetDeferredOperationMaxConcurrencyKHR"#)
  vkGetDeferredOperationResultKHR <- getDeviceProcAddr' handle (Ptr "vkGetDeferredOperationResultKHR"#)
  vkDeferredOperationJoinKHR <- getDeviceProcAddr' handle (Ptr "vkDeferredOperationJoinKHR"#)
  vkCmdSetCullMode <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetCullModeEXT"#)
                                             , (Ptr "vkCmdSetCullMode"#) ]
  vkCmdSetFrontFace <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetFrontFaceEXT"#)
                                              , (Ptr "vkCmdSetFrontFace"#) ]
  vkCmdSetPrimitiveTopology <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetPrimitiveTopologyEXT"#)
                                                      , (Ptr "vkCmdSetPrimitiveTopology"#) ]
  vkCmdSetViewportWithCount <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetViewportWithCountEXT"#)
                                                      , (Ptr "vkCmdSetViewportWithCount"#) ]
  vkCmdSetScissorWithCount <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetScissorWithCountEXT"#)
                                                     , (Ptr "vkCmdSetScissorWithCount"#) ]
  vkCmdBindVertexBuffers2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdBindVertexBuffers2EXT"#)
                                                    , (Ptr "vkCmdBindVertexBuffers2"#) ]
  vkCmdSetDepthTestEnable <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetDepthTestEnableEXT"#)
                                                    , (Ptr "vkCmdSetDepthTestEnable"#) ]
  vkCmdSetDepthWriteEnable <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetDepthWriteEnableEXT"#)
                                                     , (Ptr "vkCmdSetDepthWriteEnable"#) ]
  vkCmdSetDepthCompareOp <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetDepthCompareOpEXT"#)
                                                   , (Ptr "vkCmdSetDepthCompareOp"#) ]
  vkCmdSetDepthBoundsTestEnable <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetDepthBoundsTestEnableEXT"#)
                                                          , (Ptr "vkCmdSetDepthBoundsTestEnable"#) ]
  vkCmdSetStencilTestEnable <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetStencilTestEnableEXT"#)
                                                      , (Ptr "vkCmdSetStencilTestEnable"#) ]
  vkCmdSetStencilOp <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetStencilOpEXT"#)
                                              , (Ptr "vkCmdSetStencilOp"#) ]
  vkCmdSetPatchControlPointsEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetPatchControlPointsEXT"#)
  vkCmdSetRasterizerDiscardEnable <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetRasterizerDiscardEnableEXT"#)
                                                            , (Ptr "vkCmdSetRasterizerDiscardEnable"#) ]
  vkCmdSetDepthBiasEnable <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetDepthBiasEnableEXT"#)
                                                    , (Ptr "vkCmdSetDepthBiasEnable"#) ]
  vkCmdSetLogicOpEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetLogicOpEXT"#)
  vkCmdSetPrimitiveRestartEnable <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetPrimitiveRestartEnableEXT"#)
                                                           , (Ptr "vkCmdSetPrimitiveRestartEnable"#) ]
  vkCmdSetTessellationDomainOriginEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetTessellationDomainOriginEXT"#)
  vkCmdSetDepthClampEnableEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetDepthClampEnableEXT"#)
  vkCmdSetPolygonModeEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetPolygonModeEXT"#)
  vkCmdSetRasterizationSamplesEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetRasterizationSamplesEXT"#)
  vkCmdSetSampleMaskEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetSampleMaskEXT"#)
  vkCmdSetAlphaToCoverageEnableEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetAlphaToCoverageEnableEXT"#)
  vkCmdSetAlphaToOneEnableEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetAlphaToOneEnableEXT"#)
  vkCmdSetLogicOpEnableEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetLogicOpEnableEXT"#)
  vkCmdSetColorBlendEnableEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetColorBlendEnableEXT"#)
  vkCmdSetColorBlendEquationEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetColorBlendEquationEXT"#)
  vkCmdSetColorWriteMaskEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetColorWriteMaskEXT"#)
  vkCmdSetRasterizationStreamEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetRasterizationStreamEXT"#)
  vkCmdSetConservativeRasterizationModeEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetConservativeRasterizationModeEXT"#)
  vkCmdSetExtraPrimitiveOverestimationSizeEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetExtraPrimitiveOverestimationSizeEXT"#)
  vkCmdSetDepthClipEnableEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetDepthClipEnableEXT"#)
  vkCmdSetSampleLocationsEnableEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetSampleLocationsEnableEXT"#)
  vkCmdSetColorBlendAdvancedEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetColorBlendAdvancedEXT"#)
  vkCmdSetProvokingVertexModeEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetProvokingVertexModeEXT"#)
  vkCmdSetLineRasterizationModeEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetLineRasterizationModeEXT"#)
  vkCmdSetLineStippleEnableEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetLineStippleEnableEXT"#)
  vkCmdSetDepthClipNegativeOneToOneEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetDepthClipNegativeOneToOneEXT"#)
  vkCmdSetViewportWScalingEnableNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetViewportWScalingEnableNV"#)
  vkCmdSetViewportSwizzleNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetViewportSwizzleNV"#)
  vkCmdSetCoverageToColorEnableNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetCoverageToColorEnableNV"#)
  vkCmdSetCoverageToColorLocationNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetCoverageToColorLocationNV"#)
  vkCmdSetCoverageModulationModeNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetCoverageModulationModeNV"#)
  vkCmdSetCoverageModulationTableEnableNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetCoverageModulationTableEnableNV"#)
  vkCmdSetCoverageModulationTableNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetCoverageModulationTableNV"#)
  vkCmdSetShadingRateImageEnableNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetShadingRateImageEnableNV"#)
  vkCmdSetCoverageReductionModeNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetCoverageReductionModeNV"#)
  vkCmdSetRepresentativeFragmentTestEnableNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetRepresentativeFragmentTestEnableNV"#)
  vkCreatePrivateDataSlot <- getFirstDeviceProcAddr [ (Ptr "vkCreatePrivateDataSlotEXT"#)
                                                    , (Ptr "vkCreatePrivateDataSlot"#) ]
  vkDestroyPrivateDataSlot <- getFirstDeviceProcAddr [ (Ptr "vkDestroyPrivateDataSlotEXT"#)
                                                     , (Ptr "vkDestroyPrivateDataSlot"#) ]
  vkSetPrivateData <- getFirstDeviceProcAddr [ (Ptr "vkSetPrivateDataEXT"#)
                                             , (Ptr "vkSetPrivateData"#) ]
  vkGetPrivateData <- getFirstDeviceProcAddr [ (Ptr "vkGetPrivateDataEXT"#)
                                             , (Ptr "vkGetPrivateData"#) ]
  vkCmdCopyBuffer2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdCopyBuffer2KHR"#)
                                             , (Ptr "vkCmdCopyBuffer2"#) ]
  vkCmdCopyImage2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdCopyImage2KHR"#)
                                            , (Ptr "vkCmdCopyImage2"#) ]
  vkCmdBlitImage2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdBlitImage2KHR"#)
                                            , (Ptr "vkCmdBlitImage2"#) ]
  vkCmdCopyBufferToImage2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdCopyBufferToImage2KHR"#)
                                                    , (Ptr "vkCmdCopyBufferToImage2"#) ]
  vkCmdCopyImageToBuffer2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdCopyImageToBuffer2KHR"#)
                                                    , (Ptr "vkCmdCopyImageToBuffer2"#) ]
  vkCmdResolveImage2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdResolveImage2KHR"#)
                                               , (Ptr "vkCmdResolveImage2"#) ]
  vkCmdSetFragmentShadingRateKHR <- getDeviceProcAddr' handle (Ptr "vkCmdSetFragmentShadingRateKHR"#)
  vkCmdSetFragmentShadingRateEnumNV <- getDeviceProcAddr' handle (Ptr "vkCmdSetFragmentShadingRateEnumNV"#)
  vkGetAccelerationStructureBuildSizesKHR <- getDeviceProcAddr' handle (Ptr "vkGetAccelerationStructureBuildSizesKHR"#)
  vkCmdSetVertexInputEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetVertexInputEXT"#)
  vkCmdSetColorWriteEnableEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetColorWriteEnableEXT"#)
  vkCmdSetEvent2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdSetEvent2KHR"#)
                                           , (Ptr "vkCmdSetEvent2"#) ]
  vkCmdResetEvent2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdResetEvent2KHR"#)
                                             , (Ptr "vkCmdResetEvent2"#) ]
  vkCmdWaitEvents2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdWaitEvents2KHR"#)
                                             , (Ptr "vkCmdWaitEvents2"#) ]
  vkCmdPipelineBarrier2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdPipelineBarrier2KHR"#)
                                                  , (Ptr "vkCmdPipelineBarrier2"#) ]
  vkQueueSubmit2 <- getFirstDeviceProcAddr [ (Ptr "vkQueueSubmit2KHR"#)
                                           , (Ptr "vkQueueSubmit2"#) ]
  vkCmdWriteTimestamp2 <- getFirstDeviceProcAddr [ (Ptr "vkCmdWriteTimestamp2KHR"#)
                                                 , (Ptr "vkCmdWriteTimestamp2"#) ]
  vkCmdWriteBufferMarker2AMD <- getDeviceProcAddr' handle (Ptr "vkCmdWriteBufferMarker2AMD"#)
  vkGetQueueCheckpointData2NV <- getDeviceProcAddr' handle (Ptr "vkGetQueueCheckpointData2NV"#)
  vkCmdDecompressMemoryNV <- getDeviceProcAddr' handle (Ptr "vkCmdDecompressMemoryNV"#)
  vkCmdDecompressMemoryIndirectCountNV <- getDeviceProcAddr' handle (Ptr "vkCmdDecompressMemoryIndirectCountNV"#)
  vkCreateCuModuleNVX <- getDeviceProcAddr' handle (Ptr "vkCreateCuModuleNVX"#)
  vkCreateCuFunctionNVX <- getDeviceProcAddr' handle (Ptr "vkCreateCuFunctionNVX"#)
  vkDestroyCuModuleNVX <- getDeviceProcAddr' handle (Ptr "vkDestroyCuModuleNVX"#)
  vkDestroyCuFunctionNVX <- getDeviceProcAddr' handle (Ptr "vkDestroyCuFunctionNVX"#)
  vkCmdCuLaunchKernelNVX <- getDeviceProcAddr' handle (Ptr "vkCmdCuLaunchKernelNVX"#)
  vkGetDescriptorSetLayoutSizeEXT <- getDeviceProcAddr' handle (Ptr "vkGetDescriptorSetLayoutSizeEXT"#)
  vkGetDescriptorSetLayoutBindingOffsetEXT <- getDeviceProcAddr' handle (Ptr "vkGetDescriptorSetLayoutBindingOffsetEXT"#)
  vkGetDescriptorEXT <- getDeviceProcAddr' handle (Ptr "vkGetDescriptorEXT"#)
  vkCmdBindDescriptorBuffersEXT <- getDeviceProcAddr' handle (Ptr "vkCmdBindDescriptorBuffersEXT"#)
  vkCmdSetDescriptorBufferOffsetsEXT <- getDeviceProcAddr' handle (Ptr "vkCmdSetDescriptorBufferOffsetsEXT"#)
  vkCmdBindDescriptorBufferEmbeddedSamplersEXT <- getDeviceProcAddr' handle (Ptr "vkCmdBindDescriptorBufferEmbeddedSamplersEXT"#)
  vkGetBufferOpaqueCaptureDescriptorDataEXT <- getDeviceProcAddr' handle (Ptr "vkGetBufferOpaqueCaptureDescriptorDataEXT"#)
  vkGetImageOpaqueCaptureDescriptorDataEXT <- getDeviceProcAddr' handle (Ptr "vkGetImageOpaqueCaptureDescriptorDataEXT"#)
  vkGetImageViewOpaqueCaptureDescriptorDataEXT <- getDeviceProcAddr' handle (Ptr "vkGetImageViewOpaqueCaptureDescriptorDataEXT"#)
  vkGetSamplerOpaqueCaptureDescriptorDataEXT <- getDeviceProcAddr' handle (Ptr "vkGetSamplerOpaqueCaptureDescriptorDataEXT"#)
  vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT <- getDeviceProcAddr' handle (Ptr "vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT"#)
  vkSetDeviceMemoryPriorityEXT <- getDeviceProcAddr' handle (Ptr "vkSetDeviceMemoryPriorityEXT"#)
  vkWaitForPresentKHR <- getDeviceProcAddr' handle (Ptr "vkWaitForPresentKHR"#)
  vkCreateBufferCollectionFUCHSIA <- getDeviceProcAddr' handle (Ptr "vkCreateBufferCollectionFUCHSIA"#)
  vkSetBufferCollectionBufferConstraintsFUCHSIA <- getDeviceProcAddr' handle (Ptr "vkSetBufferCollectionBufferConstraintsFUCHSIA"#)
  vkSetBufferCollectionImageConstraintsFUCHSIA <- getDeviceProcAddr' handle (Ptr "vkSetBufferCollectionImageConstraintsFUCHSIA"#)
  vkDestroyBufferCollectionFUCHSIA <- getDeviceProcAddr' handle (Ptr "vkDestroyBufferCollectionFUCHSIA"#)
  vkGetBufferCollectionPropertiesFUCHSIA <- getDeviceProcAddr' handle (Ptr "vkGetBufferCollectionPropertiesFUCHSIA"#)
  vkCmdBeginRendering <- getFirstDeviceProcAddr [ (Ptr "vkCmdBeginRenderingKHR"#)
                                                , (Ptr "vkCmdBeginRendering"#) ]
  vkCmdEndRendering <- getFirstDeviceProcAddr [ (Ptr "vkCmdEndRenderingKHR"#)
                                              , (Ptr "vkCmdEndRendering"#) ]
  vkGetDescriptorSetLayoutHostMappingInfoVALVE <- getDeviceProcAddr' handle (Ptr "vkGetDescriptorSetLayoutHostMappingInfoVALVE"#)
  vkGetDescriptorSetHostMappingVALVE <- getDeviceProcAddr' handle (Ptr "vkGetDescriptorSetHostMappingVALVE"#)
  vkCreateMicromapEXT <- getDeviceProcAddr' handle (Ptr "vkCreateMicromapEXT"#)
  vkCmdBuildMicromapsEXT <- getDeviceProcAddr' handle (Ptr "vkCmdBuildMicromapsEXT"#)
  vkBuildMicromapsEXT <- getDeviceProcAddr' handle (Ptr "vkBuildMicromapsEXT"#)
  vkDestroyMicromapEXT <- getDeviceProcAddr' handle (Ptr "vkDestroyMicromapEXT"#)
  vkCmdCopyMicromapEXT <- getDeviceProcAddr' handle (Ptr "vkCmdCopyMicromapEXT"#)
  vkCopyMicromapEXT <- getDeviceProcAddr' handle (Ptr "vkCopyMicromapEXT"#)
  vkCmdCopyMicromapToMemoryEXT <- getDeviceProcAddr' handle (Ptr "vkCmdCopyMicromapToMemoryEXT"#)
  vkCopyMicromapToMemoryEXT <- getDeviceProcAddr' handle (Ptr "vkCopyMicromapToMemoryEXT"#)
  vkCmdCopyMemoryToMicromapEXT <- getDeviceProcAddr' handle (Ptr "vkCmdCopyMemoryToMicromapEXT"#)
  vkCopyMemoryToMicromapEXT <- getDeviceProcAddr' handle (Ptr "vkCopyMemoryToMicromapEXT"#)
  vkCmdWriteMicromapsPropertiesEXT <- getDeviceProcAddr' handle (Ptr "vkCmdWriteMicromapsPropertiesEXT"#)
  vkWriteMicromapsPropertiesEXT <- getDeviceProcAddr' handle (Ptr "vkWriteMicromapsPropertiesEXT"#)
  vkGetDeviceMicromapCompatibilityEXT <- getDeviceProcAddr' handle (Ptr "vkGetDeviceMicromapCompatibilityEXT"#)
  vkGetMicromapBuildSizesEXT <- getDeviceProcAddr' handle (Ptr "vkGetMicromapBuildSizesEXT"#)
  vkGetShaderModuleIdentifierEXT <- getDeviceProcAddr' handle (Ptr "vkGetShaderModuleIdentifierEXT"#)
  vkGetShaderModuleCreateInfoIdentifierEXT <- getDeviceProcAddr' handle (Ptr "vkGetShaderModuleCreateInfoIdentifierEXT"#)
  vkGetImageSubresourceLayout2EXT <- getDeviceProcAddr' handle (Ptr "vkGetImageSubresourceLayout2EXT"#)
  vkGetPipelinePropertiesEXT <- getDeviceProcAddr' handle (Ptr "vkGetPipelinePropertiesEXT"#)
  vkExportMetalObjectsEXT <- getDeviceProcAddr' handle (Ptr "vkExportMetalObjectsEXT"#)
  vkGetFramebufferTilePropertiesQCOM <- getDeviceProcAddr' handle (Ptr "vkGetFramebufferTilePropertiesQCOM"#)
  vkGetDynamicRenderingTilePropertiesQCOM <- getDeviceProcAddr' handle (Ptr "vkGetDynamicRenderingTilePropertiesQCOM"#)
  vkCreateOpticalFlowSessionNV <- getDeviceProcAddr' handle (Ptr "vkCreateOpticalFlowSessionNV"#)
  vkDestroyOpticalFlowSessionNV <- getDeviceProcAddr' handle (Ptr "vkDestroyOpticalFlowSessionNV"#)
  vkBindOpticalFlowSessionImageNV <- getDeviceProcAddr' handle (Ptr "vkBindOpticalFlowSessionImageNV"#)
  vkCmdOpticalFlowExecuteNV <- getDeviceProcAddr' handle (Ptr "vkCmdOpticalFlowExecuteNV"#)
  vkGetDeviceFaultInfoEXT <- getDeviceProcAddr' handle (Ptr "vkGetDeviceFaultInfoEXT"#)
  pure $ DeviceCmds handle
    (castFunPtr @_ @(Ptr Device_T -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) vkGetDeviceProcAddr)
    (castFunPtr @_ @(Ptr Device_T -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyDevice)
    (castFunPtr @_ @(Ptr Device_T -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr (Ptr Queue_T)) -> IO ()) vkGetDeviceQueue)
    (castFunPtr @_ @(Ptr Queue_T -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr (SomeStruct SubmitInfo)) -> Fence -> IO Result) vkQueueSubmit)
    (castFunPtr @_ @(Ptr Queue_T -> IO Result) vkQueueWaitIdle)
    (castFunPtr @_ @(Ptr Device_T -> IO Result) vkDeviceWaitIdle)
    (castFunPtr @_ @(Ptr Device_T -> ("pAllocateInfo" ::: Ptr (SomeStruct MemoryAllocateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pMemory" ::: Ptr DeviceMemory) -> IO Result) vkAllocateMemory)
    (castFunPtr @_ @(Ptr Device_T -> DeviceMemory -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkFreeMemory)
    (castFunPtr @_ @(Ptr Device_T -> DeviceMemory -> ("offset" ::: DeviceSize) -> DeviceSize -> MemoryMapFlags -> ("ppData" ::: Ptr (Ptr ())) -> IO Result) vkMapMemory)
    (castFunPtr @_ @(Ptr Device_T -> DeviceMemory -> IO ()) vkUnmapMemory)
    (castFunPtr @_ @(Ptr Device_T -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr MappedMemoryRange) -> IO Result) vkFlushMappedMemoryRanges)
    (castFunPtr @_ @(Ptr Device_T -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr MappedMemoryRange) -> IO Result) vkInvalidateMappedMemoryRanges)
    (castFunPtr @_ @(Ptr Device_T -> DeviceMemory -> ("pCommittedMemoryInBytes" ::: Ptr DeviceSize) -> IO ()) vkGetDeviceMemoryCommitment)
    (castFunPtr @_ @(Ptr Device_T -> Buffer -> ("pMemoryRequirements" ::: Ptr MemoryRequirements) -> IO ()) vkGetBufferMemoryRequirements)
    (castFunPtr @_ @(Ptr Device_T -> Buffer -> DeviceMemory -> ("memoryOffset" ::: DeviceSize) -> IO Result) vkBindBufferMemory)
    (castFunPtr @_ @(Ptr Device_T -> Image -> ("pMemoryRequirements" ::: Ptr MemoryRequirements) -> IO ()) vkGetImageMemoryRequirements)
    (castFunPtr @_ @(Ptr Device_T -> Image -> DeviceMemory -> ("memoryOffset" ::: DeviceSize) -> IO Result) vkBindImageMemory)
    (castFunPtr @_ @(Ptr Device_T -> Image -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr SparseImageMemoryRequirements) -> IO ()) vkGetImageSparseMemoryRequirements)
    (castFunPtr @_ @(Ptr Queue_T -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr (SomeStruct BindSparseInfo)) -> Fence -> IO Result) vkQueueBindSparse)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct FenceCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pFence" ::: Ptr Fence) -> IO Result) vkCreateFence)
    (castFunPtr @_ @(Ptr Device_T -> Fence -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyFence)
    (castFunPtr @_ @(Ptr Device_T -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr Fence) -> IO Result) vkResetFences)
    (castFunPtr @_ @(Ptr Device_T -> Fence -> IO Result) vkGetFenceStatus)
    (castFunPtr @_ @(Ptr Device_T -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr Fence) -> ("waitAll" ::: Bool32) -> ("timeout" ::: Word64) -> IO Result) vkWaitForFences)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct SemaphoreCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSemaphore" ::: Ptr Semaphore) -> IO Result) vkCreateSemaphore)
    (castFunPtr @_ @(Ptr Device_T -> Semaphore -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroySemaphore)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct EventCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pEvent" ::: Ptr Event) -> IO Result) vkCreateEvent)
    (castFunPtr @_ @(Ptr Device_T -> Event -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyEvent)
    (castFunPtr @_ @(Ptr Device_T -> Event -> IO Result) vkGetEventStatus)
    (castFunPtr @_ @(Ptr Device_T -> Event -> IO Result) vkSetEvent)
    (castFunPtr @_ @(Ptr Device_T -> Event -> IO Result) vkResetEvent)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct QueryPoolCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pQueryPool" ::: Ptr QueryPool) -> IO Result) vkCreateQueryPool)
    (castFunPtr @_ @(Ptr Device_T -> QueryPool -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyQueryPool)
    (castFunPtr @_ @(Ptr Device_T -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: DeviceSize) -> QueryResultFlags -> IO Result) vkGetQueryPoolResults)
    (castFunPtr @_ @(Ptr Device_T -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()) vkResetQueryPool)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct BufferCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pBuffer" ::: Ptr Buffer) -> IO Result) vkCreateBuffer)
    (castFunPtr @_ @(Ptr Device_T -> Buffer -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyBuffer)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct BufferViewCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pView" ::: Ptr BufferView) -> IO Result) vkCreateBufferView)
    (castFunPtr @_ @(Ptr Device_T -> BufferView -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyBufferView)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct ImageCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pImage" ::: Ptr Image) -> IO Result) vkCreateImage)
    (castFunPtr @_ @(Ptr Device_T -> Image -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyImage)
    (castFunPtr @_ @(Ptr Device_T -> Image -> ("pSubresource" ::: Ptr ImageSubresource) -> ("pLayout" ::: Ptr SubresourceLayout) -> IO ()) vkGetImageSubresourceLayout)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct ImageViewCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pView" ::: Ptr ImageView) -> IO Result) vkCreateImageView)
    (castFunPtr @_ @(Ptr Device_T -> ImageView -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyImageView)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct ShaderModuleCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pShaderModule" ::: Ptr ShaderModule) -> IO Result) vkCreateShaderModule)
    (castFunPtr @_ @(Ptr Device_T -> ShaderModule -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyShaderModule)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr PipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelineCache" ::: Ptr PipelineCache) -> IO Result) vkCreatePipelineCache)
    (castFunPtr @_ @(Ptr Device_T -> PipelineCache -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyPipelineCache)
    (castFunPtr @_ @(Ptr Device_T -> PipelineCache -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO Result) vkGetPipelineCacheData)
    (castFunPtr @_ @(Ptr Device_T -> ("dstCache" ::: PipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr PipelineCache) -> IO Result) vkMergePipelineCaches)
    (castFunPtr @_ @(Ptr Device_T -> PipelineCache -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr (SomeStruct GraphicsPipelineCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelines" ::: Ptr Pipeline) -> IO Result) vkCreateGraphicsPipelines)
    (castFunPtr @_ @(Ptr Device_T -> PipelineCache -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr (SomeStruct ComputePipelineCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelines" ::: Ptr Pipeline) -> IO Result) vkCreateComputePipelines)
    (castFunPtr @_ @(Ptr Device_T -> RenderPass -> ("pMaxWorkgroupSize" ::: Ptr Extent2D) -> IO Result) vkGetDeviceSubpassShadingMaxWorkgroupSizeHUAWEI)
    (castFunPtr @_ @(Ptr Device_T -> Pipeline -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyPipeline)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr PipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelineLayout" ::: Ptr PipelineLayout) -> IO Result) vkCreatePipelineLayout)
    (castFunPtr @_ @(Ptr Device_T -> PipelineLayout -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyPipelineLayout)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct SamplerCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSampler" ::: Ptr Sampler) -> IO Result) vkCreateSampler)
    (castFunPtr @_ @(Ptr Device_T -> Sampler -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroySampler)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct DescriptorSetLayoutCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSetLayout" ::: Ptr DescriptorSetLayout) -> IO Result) vkCreateDescriptorSetLayout)
    (castFunPtr @_ @(Ptr Device_T -> DescriptorSetLayout -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyDescriptorSetLayout)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct DescriptorPoolCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pDescriptorPool" ::: Ptr DescriptorPool) -> IO Result) vkCreateDescriptorPool)
    (castFunPtr @_ @(Ptr Device_T -> DescriptorPool -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyDescriptorPool)
    (castFunPtr @_ @(Ptr Device_T -> DescriptorPool -> DescriptorPoolResetFlags -> IO Result) vkResetDescriptorPool)
    (castFunPtr @_ @(Ptr Device_T -> ("pAllocateInfo" ::: Ptr (SomeStruct DescriptorSetAllocateInfo)) -> ("pDescriptorSets" ::: Ptr DescriptorSet) -> IO Result) vkAllocateDescriptorSets)
    (castFunPtr @_ @(Ptr Device_T -> DescriptorPool -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr DescriptorSet) -> IO Result) vkFreeDescriptorSets)
    (castFunPtr @_ @(Ptr Device_T -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr (SomeStruct WriteDescriptorSet)) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr CopyDescriptorSet) -> IO ()) vkUpdateDescriptorSets)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct FramebufferCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pFramebuffer" ::: Ptr Framebuffer) -> IO Result) vkCreateFramebuffer)
    (castFunPtr @_ @(Ptr Device_T -> Framebuffer -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyFramebuffer)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct RenderPassCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pRenderPass" ::: Ptr RenderPass) -> IO Result) vkCreateRenderPass)
    (castFunPtr @_ @(Ptr Device_T -> RenderPass -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyRenderPass)
    (castFunPtr @_ @(Ptr Device_T -> RenderPass -> ("pGranularity" ::: Ptr Extent2D) -> IO ()) vkGetRenderAreaGranularity)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr CommandPoolCreateInfo) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pCommandPool" ::: Ptr CommandPool) -> IO Result) vkCreateCommandPool)
    (castFunPtr @_ @(Ptr Device_T -> CommandPool -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyCommandPool)
    (castFunPtr @_ @(Ptr Device_T -> CommandPool -> CommandPoolResetFlags -> IO Result) vkResetCommandPool)
    (castFunPtr @_ @(Ptr Device_T -> ("pAllocateInfo" ::: Ptr CommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr (Ptr CommandBuffer_T)) -> IO Result) vkAllocateCommandBuffers)
    (castFunPtr @_ @(Ptr Device_T -> CommandPool -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr (Ptr CommandBuffer_T)) -> IO ()) vkFreeCommandBuffers)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pBeginInfo" ::: Ptr (SomeStruct CommandBufferBeginInfo)) -> IO Result) vkBeginCommandBuffer)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> IO Result) vkEndCommandBuffer)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> CommandBufferResetFlags -> IO Result) vkResetCommandBuffer)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> IO ()) vkCmdBindPipeline)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr Viewport) -> IO ()) vkCmdSetViewport)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr Rect2D) -> IO ()) vkCmdSetScissor)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("lineWidth" ::: CFloat) -> IO ()) vkCmdSetLineWidth)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ()) vkCmdSetDepthBias)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("blendConstants" ::: Ptr (FixedArray 4 CFloat)) -> IO ()) vkCmdSetBlendConstants)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ()) vkCmdSetDepthBounds)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("faceMask" ::: StencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ()) vkCmdSetStencilCompareMask)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("faceMask" ::: StencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ()) vkCmdSetStencilWriteMask)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("faceMask" ::: StencilFaceFlags) -> ("reference" ::: Word32) -> IO ()) vkCmdSetStencilReference)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr DescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ()) vkCmdBindDescriptorSets)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> IndexType -> IO ()) vkCmdBindIndexBuffer)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr Buffer) -> ("pOffsets" ::: Ptr DeviceSize) -> IO ()) vkCmdBindVertexBuffers)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ()) vkCmdDraw)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ()) vkCmdDrawIndexed)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("drawCount" ::: Word32) -> ("pVertexInfo" ::: Ptr MultiDrawInfoEXT) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("stride" ::: Word32) -> IO ()) vkCmdDrawMultiEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("drawCount" ::: Word32) -> ("pIndexInfo" ::: Ptr MultiDrawIndexedInfoEXT) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("stride" ::: Word32) -> ("pVertexOffset" ::: Ptr Int32) -> IO ()) vkCmdDrawMultiIndexedEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) vkCmdDrawIndirect)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) vkCmdDrawIndexedIndirect)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()) vkCmdDispatch)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> IO ()) vkCmdDispatchIndirect)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> IO ()) vkCmdSubpassShadingHUAWEI)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("srcBuffer" ::: Buffer) -> ("dstBuffer" ::: Buffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr BufferCopy) -> IO ()) vkCmdCopyBuffer)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr ImageCopy) -> IO ()) vkCmdCopyImage)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr ImageBlit) -> Filter -> IO ()) vkCmdBlitImage)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("srcBuffer" ::: Buffer) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr BufferImageCopy) -> IO ()) vkCmdCopyBufferToImage)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstBuffer" ::: Buffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr BufferImageCopy) -> IO ()) vkCmdCopyImageToBuffer)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("copyBufferAddress" ::: DeviceAddress) -> ("copyCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) vkCmdCopyMemoryIndirectNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("copyBufferAddress" ::: DeviceAddress) -> ("copyCount" ::: Word32) -> ("stride" ::: Word32) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("pImageSubresources" ::: Ptr ImageSubresourceLayers) -> IO ()) vkCmdCopyMemoryToImageIndirectNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("dataSize" ::: DeviceSize) -> ("pData" ::: Ptr ()) -> IO ()) vkCmdUpdateBuffer)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> DeviceSize -> ("data" ::: Word32) -> IO ()) vkCmdFillBuffer)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Image -> ImageLayout -> ("pColor" ::: Ptr ClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr ImageSubresourceRange) -> IO ()) vkCmdClearColorImage)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Image -> ImageLayout -> ("pDepthStencil" ::: Ptr ClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr ImageSubresourceRange) -> IO ()) vkCmdClearDepthStencilImage)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr ClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr ClearRect) -> IO ()) vkCmdClearAttachments)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("srcImage" ::: Image) -> ("srcImageLayout" ::: ImageLayout) -> ("dstImage" ::: Image) -> ("dstImageLayout" ::: ImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr ImageResolve) -> IO ()) vkCmdResolveImage)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Event -> ("stageMask" ::: PipelineStageFlags) -> IO ()) vkCmdSetEvent)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Event -> ("stageMask" ::: PipelineStageFlags) -> IO ()) vkCmdResetEvent)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr Event) -> ("srcStageMask" ::: PipelineStageFlags) -> ("dstStageMask" ::: PipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr MemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr BufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr (SomeStruct ImageMemoryBarrier)) -> IO ()) vkCmdWaitEvents)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("srcStageMask" ::: PipelineStageFlags) -> ("dstStageMask" ::: PipelineStageFlags) -> DependencyFlags -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr MemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr BufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr (SomeStruct ImageMemoryBarrier)) -> IO ()) vkCmdPipelineBarrier)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> QueryPool -> ("query" ::: Word32) -> QueryControlFlags -> IO ()) vkCmdBeginQuery)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> QueryPool -> ("query" ::: Word32) -> IO ()) vkCmdEndQuery)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pConditionalRenderingBegin" ::: Ptr ConditionalRenderingBeginInfoEXT) -> IO ()) vkCmdBeginConditionalRenderingEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> IO ()) vkCmdEndConditionalRenderingEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()) vkCmdResetQueryPool)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineStageFlagBits -> QueryPool -> ("query" ::: Word32) -> IO ()) vkCmdWriteTimestamp)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> QueryPool -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("stride" ::: DeviceSize) -> QueryResultFlags -> IO ()) vkCmdCopyQueryPoolResults)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineLayout -> ShaderStageFlags -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ()) vkCmdPushConstants)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pRenderPassBegin" ::: Ptr (SomeStruct RenderPassBeginInfo)) -> SubpassContents -> IO ()) vkCmdBeginRenderPass)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> SubpassContents -> IO ()) vkCmdNextSubpass)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> IO ()) vkCmdEndRenderPass)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr (Ptr CommandBuffer_T)) -> IO ()) vkCmdExecuteCommands)
    (castFunPtr @_ @(Ptr Device_T -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr (SomeStruct SwapchainCreateInfoKHR)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSwapchains" ::: Ptr SwapchainKHR) -> IO Result) vkCreateSharedSwapchainsKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct SwapchainCreateInfoKHR)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSwapchain" ::: Ptr SwapchainKHR) -> IO Result) vkCreateSwapchainKHR)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroySwapchainKHR)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> ("pSwapchainImageCount" ::: Ptr Word32) -> ("pSwapchainImages" ::: Ptr Image) -> IO Result) vkGetSwapchainImagesKHR)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> ("timeout" ::: Word64) -> Semaphore -> Fence -> ("pImageIndex" ::: Ptr Word32) -> IO Result) vkAcquireNextImageKHR)
    (castFunPtr @_ @(Ptr Queue_T -> ("pPresentInfo" ::: Ptr (SomeStruct PresentInfoKHR)) -> IO Result) vkQueuePresentKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pNameInfo" ::: Ptr DebugMarkerObjectNameInfoEXT) -> IO Result) vkDebugMarkerSetObjectNameEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pTagInfo" ::: Ptr DebugMarkerObjectTagInfoEXT) -> IO Result) vkDebugMarkerSetObjectTagEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pMarkerInfo" ::: Ptr DebugMarkerMarkerInfoEXT) -> IO ()) vkCmdDebugMarkerBeginEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> IO ()) vkCmdDebugMarkerEndEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pMarkerInfo" ::: Ptr DebugMarkerMarkerInfoEXT) -> IO ()) vkCmdDebugMarkerInsertEXT)
    (castFunPtr @_ @(Ptr Device_T -> DeviceMemory -> ExternalMemoryHandleTypeFlagsNV -> ("pHandle" ::: Ptr HANDLE) -> IO Result) vkGetMemoryWin32HandleNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("isPreprocessed" ::: Bool32) -> ("pGeneratedCommandsInfo" ::: Ptr GeneratedCommandsInfoNV) -> IO ()) vkCmdExecuteGeneratedCommandsNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pGeneratedCommandsInfo" ::: Ptr GeneratedCommandsInfoNV) -> IO ()) vkCmdPreprocessGeneratedCommandsNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineBindPoint -> Pipeline -> ("groupIndex" ::: Word32) -> IO ()) vkCmdBindPipelineShaderGroupNV)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr GeneratedCommandsMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ()) vkGetGeneratedCommandsMemoryRequirementsNV)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr IndirectCommandsLayoutCreateInfoNV) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr IndirectCommandsLayoutNV) -> IO Result) vkCreateIndirectCommandsLayoutNV)
    (castFunPtr @_ @(Ptr Device_T -> IndirectCommandsLayoutNV -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyIndirectCommandsLayoutNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr (SomeStruct WriteDescriptorSet)) -> IO ()) vkCmdPushDescriptorSetKHR)
    (castFunPtr @_ @(Ptr Device_T -> CommandPool -> CommandPoolTrimFlags -> IO ()) vkTrimCommandPool)
    (castFunPtr @_ @(Ptr Device_T -> ("pGetWin32HandleInfo" ::: Ptr MemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO Result) vkGetMemoryWin32HandleKHR)
    (castFunPtr @_ @(Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> HANDLE -> ("pMemoryWin32HandleProperties" ::: Ptr MemoryWin32HandlePropertiesKHR) -> IO Result) vkGetMemoryWin32HandlePropertiesKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pGetFdInfo" ::: Ptr MemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO Result) vkGetMemoryFdKHR)
    (castFunPtr @_ @(Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr MemoryFdPropertiesKHR) -> IO Result) vkGetMemoryFdPropertiesKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pGetZirconHandleInfo" ::: Ptr MemoryGetZirconHandleInfoFUCHSIA) -> ("pZirconHandle" ::: Ptr Zx_handle_t) -> IO Result) vkGetMemoryZirconHandleFUCHSIA)
    (castFunPtr @_ @(Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> ("zirconHandle" ::: Zx_handle_t) -> ("pMemoryZirconHandleProperties" ::: Ptr MemoryZirconHandlePropertiesFUCHSIA) -> IO Result) vkGetMemoryZirconHandlePropertiesFUCHSIA)
    (castFunPtr @_ @(Ptr Device_T -> ("pMemoryGetRemoteAddressInfo" ::: Ptr MemoryGetRemoteAddressInfoNV) -> ("pAddress" ::: Ptr RemoteAddressNV) -> IO Result) vkGetMemoryRemoteAddressNV)
    (castFunPtr @_ @(Ptr Device_T -> ("pGetWin32HandleInfo" ::: Ptr SemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO Result) vkGetSemaphoreWin32HandleKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr ImportSemaphoreWin32HandleInfoKHR) -> IO Result) vkImportSemaphoreWin32HandleKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pGetFdInfo" ::: Ptr SemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO Result) vkGetSemaphoreFdKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pImportSemaphoreFdInfo" ::: Ptr ImportSemaphoreFdInfoKHR) -> IO Result) vkImportSemaphoreFdKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pGetZirconHandleInfo" ::: Ptr SemaphoreGetZirconHandleInfoFUCHSIA) -> ("pZirconHandle" ::: Ptr Zx_handle_t) -> IO Result) vkGetSemaphoreZirconHandleFUCHSIA)
    (castFunPtr @_ @(Ptr Device_T -> ("pImportSemaphoreZirconHandleInfo" ::: Ptr ImportSemaphoreZirconHandleInfoFUCHSIA) -> IO Result) vkImportSemaphoreZirconHandleFUCHSIA)
    (castFunPtr @_ @(Ptr Device_T -> ("pGetWin32HandleInfo" ::: Ptr FenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO Result) vkGetFenceWin32HandleKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pImportFenceWin32HandleInfo" ::: Ptr ImportFenceWin32HandleInfoKHR) -> IO Result) vkImportFenceWin32HandleKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pGetFdInfo" ::: Ptr FenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO Result) vkGetFenceFdKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pImportFenceFdInfo" ::: Ptr ImportFenceFdInfoKHR) -> IO Result) vkImportFenceFdKHR)
    (castFunPtr @_ @(Ptr Device_T -> DisplayKHR -> ("pDisplayPowerInfo" ::: Ptr DisplayPowerInfoEXT) -> IO Result) vkDisplayPowerControlEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pDeviceEventInfo" ::: Ptr DeviceEventInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pFence" ::: Ptr Fence) -> IO Result) vkRegisterDeviceEventEXT)
    (castFunPtr @_ @(Ptr Device_T -> DisplayKHR -> ("pDisplayEventInfo" ::: Ptr DisplayEventInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pFence" ::: Ptr Fence) -> IO Result) vkRegisterDisplayEventEXT)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> SurfaceCounterFlagBitsEXT -> ("pCounterValue" ::: Ptr Word64) -> IO Result) vkGetSwapchainCounterEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr PeerMemoryFeatureFlags) -> IO ()) vkGetDeviceGroupPeerMemoryFeatures)
    (castFunPtr @_ @(Ptr Device_T -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr (SomeStruct BindBufferMemoryInfo)) -> IO Result) vkBindBufferMemory2)
    (castFunPtr @_ @(Ptr Device_T -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr (SomeStruct BindImageMemoryInfo)) -> IO Result) vkBindImageMemory2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("deviceMask" ::: Word32) -> IO ()) vkCmdSetDeviceMask)
    (castFunPtr @_ @(Ptr Device_T -> ("pDeviceGroupPresentCapabilities" ::: Ptr DeviceGroupPresentCapabilitiesKHR) -> IO Result) vkGetDeviceGroupPresentCapabilitiesKHR)
    (castFunPtr @_ @(Ptr Device_T -> SurfaceKHR -> ("pModes" ::: Ptr DeviceGroupPresentModeFlagsKHR) -> IO Result) vkGetDeviceGroupSurfacePresentModesKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pAcquireInfo" ::: Ptr AcquireNextImageInfoKHR) -> ("pImageIndex" ::: Ptr Word32) -> IO Result) vkAcquireNextImage2KHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()) vkCmdDispatchBase)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr DescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr DescriptorUpdateTemplate) -> IO Result) vkCreateDescriptorUpdateTemplate)
    (castFunPtr @_ @(Ptr Device_T -> DescriptorUpdateTemplate -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyDescriptorUpdateTemplate)
    (castFunPtr @_ @(Ptr Device_T -> DescriptorSet -> DescriptorUpdateTemplate -> ("pData" ::: Ptr ()) -> IO ()) vkUpdateDescriptorSetWithTemplate)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> DescriptorUpdateTemplate -> PipelineLayout -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ()) vkCmdPushDescriptorSetWithTemplateKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("swapchainCount" ::: Word32) -> ("pSwapchains" ::: Ptr SwapchainKHR) -> ("pMetadata" ::: Ptr HdrMetadataEXT) -> IO ()) vkSetHdrMetadataEXT)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> IO Result) vkGetSwapchainStatusKHR)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> ("pDisplayTimingProperties" ::: Ptr RefreshCycleDurationGOOGLE) -> IO Result) vkGetRefreshCycleDurationGOOGLE)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr PastPresentationTimingGOOGLE) -> IO Result) vkGetPastPresentationTimingGOOGLE)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr ViewportWScalingNV) -> IO ()) vkCmdSetViewportWScalingNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstDiscardRectangle" ::: Word32) -> ("discardRectangleCount" ::: Word32) -> ("pDiscardRectangles" ::: Ptr Rect2D) -> IO ()) vkCmdSetDiscardRectangleEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pSampleLocationsInfo" ::: Ptr SampleLocationsInfoEXT) -> IO ()) vkCmdSetSampleLocationsEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr BufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ()) vkGetBufferMemoryRequirements2)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr (SomeStruct ImageMemoryRequirementsInfo2)) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ()) vkGetImageMemoryRequirements2)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr ImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr SparseImageMemoryRequirements2) -> IO ()) vkGetImageSparseMemoryRequirements2)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr DeviceBufferMemoryRequirements) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ()) vkGetDeviceBufferMemoryRequirements)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr DeviceImageMemoryRequirements) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2)) -> IO ()) vkGetDeviceImageMemoryRequirements)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr DeviceImageMemoryRequirements) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr SparseImageMemoryRequirements2) -> IO ()) vkGetDeviceImageSparseMemoryRequirements)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct SamplerYcbcrConversionCreateInfo)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr SamplerYcbcrConversion) -> IO Result) vkCreateSamplerYcbcrConversion)
    (castFunPtr @_ @(Ptr Device_T -> SamplerYcbcrConversion -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroySamplerYcbcrConversion)
    (castFunPtr @_ @(Ptr Device_T -> ("pQueueInfo" ::: Ptr DeviceQueueInfo2) -> ("pQueue" ::: Ptr (Ptr Queue_T)) -> IO ()) vkGetDeviceQueue2)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr ValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pValidationCache" ::: Ptr ValidationCacheEXT) -> IO Result) vkCreateValidationCacheEXT)
    (castFunPtr @_ @(Ptr Device_T -> ValidationCacheEXT -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyValidationCacheEXT)
    (castFunPtr @_ @(Ptr Device_T -> ValidationCacheEXT -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO Result) vkGetValidationCacheDataEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("dstCache" ::: ValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr ValidationCacheEXT) -> IO Result) vkMergeValidationCachesEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct DescriptorSetLayoutCreateInfo)) -> ("pSupport" ::: Ptr (SomeStruct DescriptorSetLayoutSupport)) -> IO ()) vkGetDescriptorSetLayoutSupport)
    (castFunPtr @_ @(Ptr Device_T -> Pipeline -> ShaderStageFlagBits -> ShaderInfoTypeAMD -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO Result) vkGetShaderInfoAMD)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> ("localDimmingEnable" ::: Bool32) -> IO ()) vkSetLocalDimmingAMD)
    (castFunPtr @_ @(Ptr Device_T -> ("timestampCount" ::: Word32) -> ("pTimestampInfos" ::: Ptr CalibratedTimestampInfoEXT) -> ("pTimestamps" ::: Ptr Word64) -> ("pMaxDeviation" ::: Ptr Word64) -> IO Result) vkGetCalibratedTimestampsEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pNameInfo" ::: Ptr DebugUtilsObjectNameInfoEXT) -> IO Result) vkSetDebugUtilsObjectNameEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pTagInfo" ::: Ptr DebugUtilsObjectTagInfoEXT) -> IO Result) vkSetDebugUtilsObjectTagEXT)
    (castFunPtr @_ @(Ptr Queue_T -> ("pLabelInfo" ::: Ptr DebugUtilsLabelEXT) -> IO ()) vkQueueBeginDebugUtilsLabelEXT)
    (castFunPtr @_ @(Ptr Queue_T -> IO ()) vkQueueEndDebugUtilsLabelEXT)
    (castFunPtr @_ @(Ptr Queue_T -> ("pLabelInfo" ::: Ptr DebugUtilsLabelEXT) -> IO ()) vkQueueInsertDebugUtilsLabelEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pLabelInfo" ::: Ptr DebugUtilsLabelEXT) -> IO ()) vkCmdBeginDebugUtilsLabelEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> IO ()) vkCmdEndDebugUtilsLabelEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pLabelInfo" ::: Ptr DebugUtilsLabelEXT) -> IO ()) vkCmdInsertDebugUtilsLabelEXT)
    (castFunPtr @_ @(Ptr Device_T -> ExternalMemoryHandleTypeFlagBits -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr MemoryHostPointerPropertiesEXT) -> IO Result) vkGetMemoryHostPointerPropertiesEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineStageFlagBits -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("marker" ::: Word32) -> IO ()) vkCmdWriteBufferMarkerAMD)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct RenderPassCreateInfo2)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pRenderPass" ::: Ptr RenderPass) -> IO Result) vkCreateRenderPass2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pRenderPassBegin" ::: Ptr (SomeStruct RenderPassBeginInfo)) -> ("pSubpassBeginInfo" ::: Ptr SubpassBeginInfo) -> IO ()) vkCmdBeginRenderPass2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pSubpassBeginInfo" ::: Ptr SubpassBeginInfo) -> ("pSubpassEndInfo" ::: Ptr (SomeStruct SubpassEndInfo)) -> IO ()) vkCmdNextSubpass2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pSubpassEndInfo" ::: Ptr (SomeStruct SubpassEndInfo)) -> IO ()) vkCmdEndRenderPass2)
    (castFunPtr @_ @(Ptr Device_T -> Semaphore -> ("pValue" ::: Ptr Word64) -> IO Result) vkGetSemaphoreCounterValue)
    (castFunPtr @_ @(Ptr Device_T -> ("pWaitInfo" ::: Ptr SemaphoreWaitInfo) -> ("timeout" ::: Word64) -> IO Result) vkWaitSemaphores)
    (castFunPtr @_ @(Ptr Device_T -> ("pSignalInfo" ::: Ptr SemaphoreSignalInfo) -> IO Result) vkSignalSemaphore)
    (castFunPtr @_ @(Ptr Device_T -> Ptr AHardwareBuffer -> ("pProperties" ::: Ptr (SomeStruct AndroidHardwareBufferPropertiesANDROID)) -> IO Result) vkGetAndroidHardwareBufferPropertiesANDROID)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr MemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO Result) vkGetMemoryAndroidHardwareBufferANDROID)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("countBuffer" ::: Buffer) -> ("countBufferOffset" ::: DeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) vkCmdDrawIndirectCount)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("countBuffer" ::: Buffer) -> ("countBufferOffset" ::: DeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) vkCmdDrawIndexedIndirectCount)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pCheckpointMarker" ::: Ptr ()) -> IO ()) vkCmdSetCheckpointNV)
    (castFunPtr @_ @(Ptr Queue_T -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr CheckpointDataNV) -> IO ()) vkGetQueueCheckpointDataNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr Buffer) -> ("pOffsets" ::: Ptr DeviceSize) -> ("pSizes" ::: Ptr DeviceSize) -> IO ()) vkCmdBindTransformFeedbackBuffersEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr Buffer) -> ("pCounterBufferOffsets" ::: Ptr DeviceSize) -> IO ()) vkCmdBeginTransformFeedbackEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr Buffer) -> ("pCounterBufferOffsets" ::: Ptr DeviceSize) -> IO ()) vkCmdEndTransformFeedbackEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> QueryPool -> ("query" ::: Word32) -> QueryControlFlags -> ("index" ::: Word32) -> IO ()) vkCmdBeginQueryIndexedEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> QueryPool -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ()) vkCmdEndQueryIndexedEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: Buffer) -> ("counterBufferOffset" ::: DeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ()) vkCmdDrawIndirectByteCountEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr Rect2D) -> IO ()) vkCmdSetExclusiveScissorNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ()) vkCmdBindShadingRateImageNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr ShadingRatePaletteNV) -> IO ()) vkCmdSetViewportShadingRatePaletteNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> CoarseSampleOrderTypeNV -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr CoarseSampleOrderCustomNV) -> IO ()) vkCmdSetCoarseSampleOrderNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("taskCount" ::: Word32) -> ("firstTask" ::: Word32) -> IO ()) vkCmdDrawMeshTasksNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) vkCmdDrawMeshTasksIndirectNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("countBuffer" ::: Buffer) -> ("countBufferOffset" ::: DeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) vkCmdDrawMeshTasksIndirectCountNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()) vkCmdDrawMeshTasksEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) vkCmdDrawMeshTasksIndirectEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Buffer -> ("offset" ::: DeviceSize) -> ("countBuffer" ::: Buffer) -> ("countBufferOffset" ::: DeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) vkCmdDrawMeshTasksIndirectCountEXT)
    (castFunPtr @_ @(Ptr Device_T -> Pipeline -> ("shader" ::: Word32) -> IO Result) vkCompileDeferredNV)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct AccelerationStructureCreateInfoNV)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr AccelerationStructureNV) -> IO Result) vkCreateAccelerationStructureNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ImageView -> ImageLayout -> IO ()) vkCmdBindInvocationMaskHUAWEI)
    (castFunPtr @_ @(Ptr Device_T -> AccelerationStructureKHR -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyAccelerationStructureKHR)
    (castFunPtr @_ @(Ptr Device_T -> AccelerationStructureNV -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyAccelerationStructureNV)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr AccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr (SomeStruct MemoryRequirements2KHR)) -> IO ()) vkGetAccelerationStructureMemoryRequirementsNV)
    (castFunPtr @_ @(Ptr Device_T -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr BindAccelerationStructureMemoryInfoNV) -> IO Result) vkBindAccelerationStructureMemoryNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("dst" ::: AccelerationStructureNV) -> ("src" ::: AccelerationStructureNV) -> CopyAccelerationStructureModeKHR -> IO ()) vkCmdCopyAccelerationStructureNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pInfo" ::: Ptr CopyAccelerationStructureInfoKHR) -> IO ()) vkCmdCopyAccelerationStructureKHR)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> ("pInfo" ::: Ptr CopyAccelerationStructureInfoKHR) -> IO Result) vkCopyAccelerationStructureKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pInfo" ::: Ptr CopyAccelerationStructureToMemoryInfoKHR) -> IO ()) vkCmdCopyAccelerationStructureToMemoryKHR)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> ("pInfo" ::: Ptr CopyAccelerationStructureToMemoryInfoKHR) -> IO Result) vkCopyAccelerationStructureToMemoryKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pInfo" ::: Ptr CopyMemoryToAccelerationStructureInfoKHR) -> IO ()) vkCmdCopyMemoryToAccelerationStructureKHR)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> ("pInfo" ::: Ptr CopyMemoryToAccelerationStructureInfoKHR) -> IO Result) vkCopyMemoryToAccelerationStructureKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr AccelerationStructureKHR) -> QueryType -> QueryPool -> ("firstQuery" ::: Word32) -> IO ()) vkCmdWriteAccelerationStructuresPropertiesKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr AccelerationStructureNV) -> QueryType -> QueryPool -> ("firstQuery" ::: Word32) -> IO ()) vkCmdWriteAccelerationStructuresPropertiesNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pInfo" ::: Ptr AccelerationStructureInfoNV) -> ("instanceData" ::: Buffer) -> ("instanceOffset" ::: DeviceSize) -> ("update" ::: Bool32) -> ("dst" ::: AccelerationStructureNV) -> ("src" ::: AccelerationStructureNV) -> ("scratch" ::: Buffer) -> ("scratchOffset" ::: DeviceSize) -> IO ()) vkCmdBuildAccelerationStructureNV)
    (castFunPtr @_ @(Ptr Device_T -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr AccelerationStructureKHR) -> QueryType -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: CSize) -> IO Result) vkWriteAccelerationStructuresPropertiesKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pRaygenShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pMissShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pHitShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pCallableShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ()) vkCmdTraceRaysKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("raygenShaderBindingTableBuffer" ::: Buffer) -> ("raygenShaderBindingOffset" ::: DeviceSize) -> ("missShaderBindingTableBuffer" ::: Buffer) -> ("missShaderBindingOffset" ::: DeviceSize) -> ("missShaderBindingStride" ::: DeviceSize) -> ("hitShaderBindingTableBuffer" ::: Buffer) -> ("hitShaderBindingOffset" ::: DeviceSize) -> ("hitShaderBindingStride" ::: DeviceSize) -> ("callableShaderBindingTableBuffer" ::: Buffer) -> ("callableShaderBindingOffset" ::: DeviceSize) -> ("callableShaderBindingStride" ::: DeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ()) vkCmdTraceRaysNV)
    (castFunPtr @_ @(Ptr Device_T -> Pipeline -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO Result) vkGetRayTracingShaderGroupHandlesKHR)
    (castFunPtr @_ @(Ptr Device_T -> Pipeline -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO Result) vkGetRayTracingCaptureReplayShaderGroupHandlesKHR)
    (castFunPtr @_ @(Ptr Device_T -> AccelerationStructureNV -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO Result) vkGetAccelerationStructureHandleNV)
    (castFunPtr @_ @(Ptr Device_T -> PipelineCache -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr (SomeStruct RayTracingPipelineCreateInfoNV)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelines" ::: Ptr Pipeline) -> IO Result) vkCreateRayTracingPipelinesNV)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> PipelineCache -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr (SomeStruct RayTracingPipelineCreateInfoKHR)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPipelines" ::: Ptr Pipeline) -> IO Result) vkCreateRayTracingPipelinesKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pRaygenShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pMissShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pHitShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("pCallableShaderBindingTable" ::: Ptr StridedDeviceAddressRegionKHR) -> ("indirectDeviceAddress" ::: DeviceAddress) -> IO ()) vkCmdTraceRaysIndirectKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("indirectDeviceAddress" ::: DeviceAddress) -> IO ()) vkCmdTraceRaysIndirect2KHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pVersionInfo" ::: Ptr AccelerationStructureVersionInfoKHR) -> ("pCompatibility" ::: Ptr AccelerationStructureCompatibilityKHR) -> IO ()) vkGetDeviceAccelerationStructureCompatibilityKHR)
    (castFunPtr @_ @(Ptr Device_T -> Pipeline -> ("group" ::: Word32) -> ShaderGroupShaderKHR -> IO DeviceSize) vkGetRayTracingShaderGroupStackSizeKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pipelineStackSize" ::: Word32) -> IO ()) vkCmdSetRayTracingPipelineStackSizeKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr ImageViewHandleInfoNVX) -> IO Word32) vkGetImageViewHandleNVX)
    (castFunPtr @_ @(Ptr Device_T -> ImageView -> ("pProperties" ::: Ptr ImageViewAddressPropertiesNVX) -> IO Result) vkGetImageViewAddressNVX)
    (castFunPtr @_ @(Ptr Device_T -> ("pSurfaceInfo" ::: Ptr (SomeStruct PhysicalDeviceSurfaceInfo2KHR)) -> ("pModes" ::: Ptr DeviceGroupPresentModeFlagsKHR) -> IO Result) vkGetDeviceGroupSurfacePresentModes2EXT)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> IO Result) vkAcquireFullScreenExclusiveModeEXT)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> IO Result) vkReleaseFullScreenExclusiveModeEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr AcquireProfilingLockInfoKHR) -> IO Result) vkAcquireProfilingLockKHR)
    (castFunPtr @_ @(Ptr Device_T -> IO ()) vkReleaseProfilingLockKHR)
    (castFunPtr @_ @(Ptr Device_T -> Image -> ("pProperties" ::: Ptr ImageDrmFormatModifierPropertiesEXT) -> IO Result) vkGetImageDrmFormatModifierPropertiesEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr BufferDeviceAddressInfo) -> IO Word64) vkGetBufferOpaqueCaptureAddress)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr BufferDeviceAddressInfo) -> IO DeviceAddress) vkGetBufferDeviceAddress)
    (castFunPtr @_ @(Ptr Device_T -> ("pInitializeInfo" ::: Ptr InitializePerformanceApiInfoINTEL) -> IO Result) vkInitializePerformanceApiINTEL)
    (castFunPtr @_ @(Ptr Device_T -> IO ()) vkUninitializePerformanceApiINTEL)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pMarkerInfo" ::: Ptr PerformanceMarkerInfoINTEL) -> IO Result) vkCmdSetPerformanceMarkerINTEL)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pMarkerInfo" ::: Ptr PerformanceStreamMarkerInfoINTEL) -> IO Result) vkCmdSetPerformanceStreamMarkerINTEL)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pOverrideInfo" ::: Ptr PerformanceOverrideInfoINTEL) -> IO Result) vkCmdSetPerformanceOverrideINTEL)
    (castFunPtr @_ @(Ptr Device_T -> ("pAcquireInfo" ::: Ptr PerformanceConfigurationAcquireInfoINTEL) -> ("pConfiguration" ::: Ptr PerformanceConfigurationINTEL) -> IO Result) vkAcquirePerformanceConfigurationINTEL)
    (castFunPtr @_ @(Ptr Device_T -> PerformanceConfigurationINTEL -> IO Result) vkReleasePerformanceConfigurationINTEL)
    (castFunPtr @_ @(Ptr Queue_T -> PerformanceConfigurationINTEL -> IO Result) vkQueueSetPerformanceConfigurationINTEL)
    (castFunPtr @_ @(Ptr Device_T -> PerformanceParameterTypeINTEL -> ("pValue" ::: Ptr PerformanceValueINTEL) -> IO Result) vkGetPerformanceParameterINTEL)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr DeviceMemoryOpaqueCaptureAddressInfo) -> IO Word64) vkGetDeviceMemoryOpaqueCaptureAddress)
    (castFunPtr @_ @(Ptr Device_T -> ("pPipelineInfo" ::: Ptr PipelineInfoKHR) -> ("pExecutableCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr PipelineExecutablePropertiesKHR) -> IO Result) vkGetPipelineExecutablePropertiesKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pExecutableInfo" ::: Ptr PipelineExecutableInfoKHR) -> ("pStatisticCount" ::: Ptr Word32) -> ("pStatistics" ::: Ptr PipelineExecutableStatisticKHR) -> IO Result) vkGetPipelineExecutableStatisticsKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pExecutableInfo" ::: Ptr PipelineExecutableInfoKHR) -> ("pInternalRepresentationCount" ::: Ptr Word32) -> ("pInternalRepresentations" ::: Ptr PipelineExecutableInternalRepresentationKHR) -> IO Result) vkGetPipelineExecutableInternalRepresentationsKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("lineStippleFactor" ::: Word32) -> ("lineStipplePattern" ::: Word16) -> IO ()) vkCmdSetLineStippleEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct AccelerationStructureCreateInfoKHR)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr AccelerationStructureKHR) -> IO Result) vkCreateAccelerationStructureKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("infoCount" ::: Word32) -> ("pInfos" ::: Ptr AccelerationStructureBuildGeometryInfoKHR) -> ("ppBuildRangeInfos" ::: Ptr (Ptr AccelerationStructureBuildRangeInfoKHR)) -> IO ()) vkCmdBuildAccelerationStructuresKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("infoCount" ::: Word32) -> ("pInfos" ::: Ptr AccelerationStructureBuildGeometryInfoKHR) -> ("pIndirectDeviceAddresses" ::: Ptr DeviceAddress) -> ("pIndirectStrides" ::: Ptr Word32) -> ("ppMaxPrimitiveCounts" ::: Ptr (Ptr Word32)) -> IO ()) vkCmdBuildAccelerationStructuresIndirectKHR)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> ("infoCount" ::: Word32) -> ("pInfos" ::: Ptr AccelerationStructureBuildGeometryInfoKHR) -> ("ppBuildRangeInfos" ::: Ptr (Ptr AccelerationStructureBuildRangeInfoKHR)) -> IO Result) vkBuildAccelerationStructuresKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr AccelerationStructureDeviceAddressInfoKHR) -> IO DeviceAddress) vkGetAccelerationStructureDeviceAddressKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pDeferredOperation" ::: Ptr DeferredOperationKHR) -> IO Result) vkCreateDeferredOperationKHR)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyDeferredOperationKHR)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> IO Word32) vkGetDeferredOperationMaxConcurrencyKHR)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> IO Result) vkGetDeferredOperationResultKHR)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> IO Result) vkDeferredOperationJoinKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> CullModeFlags -> IO ()) vkCmdSetCullMode)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> FrontFace -> IO ()) vkCmdSetFrontFace)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PrimitiveTopology -> IO ()) vkCmdSetPrimitiveTopology)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr Viewport) -> IO ()) vkCmdSetViewportWithCount)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr Rect2D) -> IO ()) vkCmdSetScissorWithCount)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr Buffer) -> ("pOffsets" ::: Ptr DeviceSize) -> ("pSizes" ::: Ptr DeviceSize) -> ("pStrides" ::: Ptr DeviceSize) -> IO ()) vkCmdBindVertexBuffers2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("depthTestEnable" ::: Bool32) -> IO ()) vkCmdSetDepthTestEnable)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("depthWriteEnable" ::: Bool32) -> IO ()) vkCmdSetDepthWriteEnable)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("depthCompareOp" ::: CompareOp) -> IO ()) vkCmdSetDepthCompareOp)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("depthBoundsTestEnable" ::: Bool32) -> IO ()) vkCmdSetDepthBoundsTestEnable)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("stencilTestEnable" ::: Bool32) -> IO ()) vkCmdSetStencilTestEnable)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("faceMask" ::: StencilFaceFlags) -> ("failOp" ::: StencilOp) -> ("passOp" ::: StencilOp) -> ("depthFailOp" ::: StencilOp) -> CompareOp -> IO ()) vkCmdSetStencilOp)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("patchControlPoints" ::: Word32) -> IO ()) vkCmdSetPatchControlPointsEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("rasterizerDiscardEnable" ::: Bool32) -> IO ()) vkCmdSetRasterizerDiscardEnable)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("depthBiasEnable" ::: Bool32) -> IO ()) vkCmdSetDepthBiasEnable)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> LogicOp -> IO ()) vkCmdSetLogicOpEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("primitiveRestartEnable" ::: Bool32) -> IO ()) vkCmdSetPrimitiveRestartEnable)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> TessellationDomainOrigin -> IO ()) vkCmdSetTessellationDomainOriginEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("depthClampEnable" ::: Bool32) -> IO ()) vkCmdSetDepthClampEnableEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PolygonMode -> IO ()) vkCmdSetPolygonModeEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("rasterizationSamples" ::: SampleCountFlagBits) -> IO ()) vkCmdSetRasterizationSamplesEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("samples" ::: SampleCountFlagBits) -> ("pSampleMask" ::: Ptr SampleMask) -> IO ()) vkCmdSetSampleMaskEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("alphaToCoverageEnable" ::: Bool32) -> IO ()) vkCmdSetAlphaToCoverageEnableEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("alphaToOneEnable" ::: Bool32) -> IO ()) vkCmdSetAlphaToOneEnableEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("logicOpEnable" ::: Bool32) -> IO ()) vkCmdSetLogicOpEnableEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstAttachment" ::: Word32) -> ("attachmentCount" ::: Word32) -> ("pColorBlendEnables" ::: Ptr Bool32) -> IO ()) vkCmdSetColorBlendEnableEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstAttachment" ::: Word32) -> ("attachmentCount" ::: Word32) -> ("pColorBlendEquations" ::: Ptr ColorBlendEquationEXT) -> IO ()) vkCmdSetColorBlendEquationEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstAttachment" ::: Word32) -> ("attachmentCount" ::: Word32) -> ("pColorWriteMasks" ::: Ptr ColorComponentFlags) -> IO ()) vkCmdSetColorWriteMaskEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("rasterizationStream" ::: Word32) -> IO ()) vkCmdSetRasterizationStreamEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ConservativeRasterizationModeEXT -> IO ()) vkCmdSetConservativeRasterizationModeEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("extraPrimitiveOverestimationSize" ::: CFloat) -> IO ()) vkCmdSetExtraPrimitiveOverestimationSizeEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("depthClipEnable" ::: Bool32) -> IO ()) vkCmdSetDepthClipEnableEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("sampleLocationsEnable" ::: Bool32) -> IO ()) vkCmdSetSampleLocationsEnableEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstAttachment" ::: Word32) -> ("attachmentCount" ::: Word32) -> ("pColorBlendAdvanced" ::: Ptr ColorBlendAdvancedEXT) -> IO ()) vkCmdSetColorBlendAdvancedEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ProvokingVertexModeEXT -> IO ()) vkCmdSetProvokingVertexModeEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> LineRasterizationModeEXT -> IO ()) vkCmdSetLineRasterizationModeEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("stippledLineEnable" ::: Bool32) -> IO ()) vkCmdSetLineStippleEnableEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("negativeOneToOne" ::: Bool32) -> IO ()) vkCmdSetDepthClipNegativeOneToOneEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("viewportWScalingEnable" ::: Bool32) -> IO ()) vkCmdSetViewportWScalingEnableNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportSwizzles" ::: Ptr ViewportSwizzleNV) -> IO ()) vkCmdSetViewportSwizzleNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("coverageToColorEnable" ::: Bool32) -> IO ()) vkCmdSetCoverageToColorEnableNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("coverageToColorLocation" ::: Word32) -> IO ()) vkCmdSetCoverageToColorLocationNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> CoverageModulationModeNV -> IO ()) vkCmdSetCoverageModulationModeNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("coverageModulationTableEnable" ::: Bool32) -> IO ()) vkCmdSetCoverageModulationTableEnableNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("coverageModulationTableCount" ::: Word32) -> ("pCoverageModulationTable" ::: Ptr CFloat) -> IO ()) vkCmdSetCoverageModulationTableNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("shadingRateImageEnable" ::: Bool32) -> IO ()) vkCmdSetShadingRateImageEnableNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> CoverageReductionModeNV -> IO ()) vkCmdSetCoverageReductionModeNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("representativeFragmentTestEnable" ::: Bool32) -> IO ()) vkCmdSetRepresentativeFragmentTestEnableNV)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr PrivateDataSlotCreateInfo) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pPrivateDataSlot" ::: Ptr PrivateDataSlot) -> IO Result) vkCreatePrivateDataSlot)
    (castFunPtr @_ @(Ptr Device_T -> PrivateDataSlot -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyPrivateDataSlot)
    (castFunPtr @_ @(Ptr Device_T -> ObjectType -> ("objectHandle" ::: Word64) -> PrivateDataSlot -> ("data" ::: Word64) -> IO Result) vkSetPrivateData)
    (castFunPtr @_ @(Ptr Device_T -> ObjectType -> ("objectHandle" ::: Word64) -> PrivateDataSlot -> ("pData" ::: Ptr Word64) -> IO ()) vkGetPrivateData)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pCopyBufferInfo" ::: Ptr CopyBufferInfo2) -> IO ()) vkCmdCopyBuffer2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pCopyImageInfo" ::: Ptr CopyImageInfo2) -> IO ()) vkCmdCopyImage2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pBlitImageInfo" ::: Ptr BlitImageInfo2) -> IO ()) vkCmdBlitImage2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pCopyBufferToImageInfo" ::: Ptr CopyBufferToImageInfo2) -> IO ()) vkCmdCopyBufferToImage2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pCopyImageToBufferInfo" ::: Ptr CopyImageToBufferInfo2) -> IO ()) vkCmdCopyImageToBuffer2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pResolveImageInfo" ::: Ptr ResolveImageInfo2) -> IO ()) vkCmdResolveImage2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pFragmentSize" ::: Ptr Extent2D) -> ("combinerOps" ::: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)) -> IO ()) vkCmdSetFragmentShadingRateKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> FragmentShadingRateNV -> ("combinerOps" ::: Ptr (FixedArray 2 FragmentShadingRateCombinerOpKHR)) -> IO ()) vkCmdSetFragmentShadingRateEnumNV)
    (castFunPtr @_ @(Ptr Device_T -> AccelerationStructureBuildTypeKHR -> ("pBuildInfo" ::: Ptr AccelerationStructureBuildGeometryInfoKHR) -> ("pMaxPrimitiveCounts" ::: Ptr Word32) -> ("pSizeInfo" ::: Ptr AccelerationStructureBuildSizesInfoKHR) -> IO ()) vkGetAccelerationStructureBuildSizesKHR)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("vertexBindingDescriptionCount" ::: Word32) -> ("pVertexBindingDescriptions" ::: Ptr VertexInputBindingDescription2EXT) -> ("vertexAttributeDescriptionCount" ::: Word32) -> ("pVertexAttributeDescriptions" ::: Ptr VertexInputAttributeDescription2EXT) -> IO ()) vkCmdSetVertexInputEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("attachmentCount" ::: Word32) -> ("pColorWriteEnables" ::: Ptr Bool32) -> IO ()) vkCmdSetColorWriteEnableEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Event -> ("pDependencyInfo" ::: Ptr DependencyInfo) -> IO ()) vkCmdSetEvent2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Event -> ("stageMask" ::: PipelineStageFlags2) -> IO ()) vkCmdResetEvent2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr Event) -> ("pDependencyInfos" ::: Ptr DependencyInfo) -> IO ()) vkCmdWaitEvents2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pDependencyInfo" ::: Ptr DependencyInfo) -> IO ()) vkCmdPipelineBarrier2)
    (castFunPtr @_ @(Ptr Queue_T -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr (SomeStruct SubmitInfo2)) -> Fence -> IO Result) vkQueueSubmit2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineStageFlags2 -> QueryPool -> ("query" ::: Word32) -> IO ()) vkCmdWriteTimestamp2)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineStageFlags2 -> ("dstBuffer" ::: Buffer) -> ("dstOffset" ::: DeviceSize) -> ("marker" ::: Word32) -> IO ()) vkCmdWriteBufferMarker2AMD)
    (castFunPtr @_ @(Ptr Queue_T -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr CheckpointData2NV) -> IO ()) vkGetQueueCheckpointData2NV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("decompressRegionCount" ::: Word32) -> ("pDecompressMemoryRegions" ::: Ptr DecompressMemoryRegionNV) -> IO ()) vkCmdDecompressMemoryNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("indirectCommandsAddress" ::: DeviceAddress) -> ("indirectCommandsCountAddress" ::: DeviceAddress) -> ("stride" ::: Word32) -> IO ()) vkCmdDecompressMemoryIndirectCountNV)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr CuModuleCreateInfoNVX) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pModule" ::: Ptr CuModuleNVX) -> IO Result) vkCreateCuModuleNVX)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr CuFunctionCreateInfoNVX) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pFunction" ::: Ptr CuFunctionNVX) -> IO Result) vkCreateCuFunctionNVX)
    (castFunPtr @_ @(Ptr Device_T -> CuModuleNVX -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyCuModuleNVX)
    (castFunPtr @_ @(Ptr Device_T -> CuFunctionNVX -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyCuFunctionNVX)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pLaunchInfo" ::: Ptr CuLaunchInfoNVX) -> IO ()) vkCmdCuLaunchKernelNVX)
    (castFunPtr @_ @(Ptr Device_T -> DescriptorSetLayout -> ("pLayoutSizeInBytes" ::: Ptr DeviceSize) -> IO ()) vkGetDescriptorSetLayoutSizeEXT)
    (castFunPtr @_ @(Ptr Device_T -> DescriptorSetLayout -> ("binding" ::: Word32) -> ("pOffset" ::: Ptr DeviceSize) -> IO ()) vkGetDescriptorSetLayoutBindingOffsetEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pDescriptorInfo" ::: Ptr DescriptorGetInfoEXT) -> ("dataSize" ::: CSize) -> ("pDescriptor" ::: Ptr ()) -> IO ()) vkGetDescriptorEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("bufferCount" ::: Word32) -> ("pBindingInfos" ::: Ptr (SomeStruct DescriptorBufferBindingInfoEXT)) -> IO ()) vkCmdBindDescriptorBuffersEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> ("firstSet" ::: Word32) -> ("setCount" ::: Word32) -> ("pBufferIndices" ::: Ptr Word32) -> ("pOffsets" ::: Ptr DeviceSize) -> IO ()) vkCmdSetDescriptorBufferOffsetsEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> PipelineBindPoint -> PipelineLayout -> ("set" ::: Word32) -> IO ()) vkCmdBindDescriptorBufferEmbeddedSamplersEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr BufferCaptureDescriptorDataInfoEXT) -> ("pData" ::: Ptr ()) -> IO Result) vkGetBufferOpaqueCaptureDescriptorDataEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr ImageCaptureDescriptorDataInfoEXT) -> ("pData" ::: Ptr ()) -> IO Result) vkGetImageOpaqueCaptureDescriptorDataEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr ImageViewCaptureDescriptorDataInfoEXT) -> ("pData" ::: Ptr ()) -> IO Result) vkGetImageViewOpaqueCaptureDescriptorDataEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr SamplerCaptureDescriptorDataInfoEXT) -> ("pData" ::: Ptr ()) -> IO Result) vkGetSamplerOpaqueCaptureDescriptorDataEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pInfo" ::: Ptr AccelerationStructureCaptureDescriptorDataInfoEXT) -> ("pData" ::: Ptr ()) -> IO Result) vkGetAccelerationStructureOpaqueCaptureDescriptorDataEXT)
    (castFunPtr @_ @(Ptr Device_T -> DeviceMemory -> ("priority" ::: CFloat) -> IO ()) vkSetDeviceMemoryPriorityEXT)
    (castFunPtr @_ @(Ptr Device_T -> SwapchainKHR -> ("presentId" ::: Word64) -> ("timeout" ::: Word64) -> IO Result) vkWaitForPresentKHR)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr BufferCollectionCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pCollection" ::: Ptr BufferCollectionFUCHSIA) -> IO Result) vkCreateBufferCollectionFUCHSIA)
    (castFunPtr @_ @(Ptr Device_T -> BufferCollectionFUCHSIA -> ("pBufferConstraintsInfo" ::: Ptr BufferConstraintsInfoFUCHSIA) -> IO Result) vkSetBufferCollectionBufferConstraintsFUCHSIA)
    (castFunPtr @_ @(Ptr Device_T -> BufferCollectionFUCHSIA -> ("pImageConstraintsInfo" ::: Ptr ImageConstraintsInfoFUCHSIA) -> IO Result) vkSetBufferCollectionImageConstraintsFUCHSIA)
    (castFunPtr @_ @(Ptr Device_T -> BufferCollectionFUCHSIA -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyBufferCollectionFUCHSIA)
    (castFunPtr @_ @(Ptr Device_T -> BufferCollectionFUCHSIA -> ("pProperties" ::: Ptr BufferCollectionPropertiesFUCHSIA) -> IO Result) vkGetBufferCollectionPropertiesFUCHSIA)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pRenderingInfo" ::: Ptr (SomeStruct RenderingInfo)) -> IO ()) vkCmdBeginRendering)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> IO ()) vkCmdEndRendering)
    (castFunPtr @_ @(Ptr Device_T -> ("pBindingReference" ::: Ptr DescriptorSetBindingReferenceVALVE) -> ("pHostMapping" ::: Ptr DescriptorSetLayoutHostMappingInfoVALVE) -> IO ()) vkGetDescriptorSetLayoutHostMappingInfoVALVE)
    (castFunPtr @_ @(Ptr Device_T -> DescriptorSet -> ("ppData" ::: Ptr (Ptr ())) -> IO ()) vkGetDescriptorSetHostMappingVALVE)
    (castFunPtr @_ @(Ptr Device_T -> Ptr MicromapCreateInfoEXT -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pMicromap" ::: Ptr MicromapEXT) -> IO Result) vkCreateMicromapEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("infoCount" ::: Word32) -> ("pInfos" ::: Ptr MicromapBuildInfoEXT) -> IO ()) vkCmdBuildMicromapsEXT)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> ("infoCount" ::: Word32) -> ("pInfos" ::: Ptr MicromapBuildInfoEXT) -> IO Result) vkBuildMicromapsEXT)
    (castFunPtr @_ @(Ptr Device_T -> MicromapEXT -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyMicromapEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Ptr CopyMicromapInfoEXT -> IO ()) vkCmdCopyMicromapEXT)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMicromapInfoEXT -> IO Result) vkCopyMicromapEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("pInfo" ::: Ptr CopyMicromapToMemoryInfoEXT) -> IO ()) vkCmdCopyMicromapToMemoryEXT)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> ("pInfo" ::: Ptr CopyMicromapToMemoryInfoEXT) -> IO Result) vkCopyMicromapToMemoryEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> Ptr CopyMemoryToMicromapInfoEXT -> IO ()) vkCmdCopyMemoryToMicromapEXT)
    (castFunPtr @_ @(Ptr Device_T -> DeferredOperationKHR -> Ptr CopyMemoryToMicromapInfoEXT -> IO Result) vkCopyMemoryToMicromapEXT)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> ("micromapCount" ::: Word32) -> ("pMicromaps" ::: Ptr MicromapEXT) -> QueryType -> QueryPool -> ("firstQuery" ::: Word32) -> IO ()) vkCmdWriteMicromapsPropertiesEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("micromapCount" ::: Word32) -> ("pMicromaps" ::: Ptr MicromapEXT) -> QueryType -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: CSize) -> IO Result) vkWriteMicromapsPropertiesEXT)
    (castFunPtr @_ @(Ptr Device_T -> Ptr MicromapVersionInfoEXT -> ("pCompatibility" ::: Ptr AccelerationStructureCompatibilityKHR) -> IO ()) vkGetDeviceMicromapCompatibilityEXT)
    (castFunPtr @_ @(Ptr Device_T -> AccelerationStructureBuildTypeKHR -> Ptr MicromapBuildInfoEXT -> ("pSizeInfo" ::: Ptr MicromapBuildSizesInfoEXT) -> IO ()) vkGetMicromapBuildSizesEXT)
    (castFunPtr @_ @(Ptr Device_T -> ShaderModule -> ("pIdentifier" ::: Ptr ShaderModuleIdentifierEXT) -> IO ()) vkGetShaderModuleIdentifierEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct ShaderModuleCreateInfo)) -> ("pIdentifier" ::: Ptr ShaderModuleIdentifierEXT) -> IO ()) vkGetShaderModuleCreateInfoIdentifierEXT)
    (castFunPtr @_ @(Ptr Device_T -> Image -> ("pSubresource" ::: Ptr ImageSubresource2EXT) -> ("pLayout" ::: Ptr (SomeStruct SubresourceLayout2EXT)) -> IO ()) vkGetImageSubresourceLayout2EXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pPipelineInfo" ::: Ptr PipelineInfoEXT) -> ("pPipelineProperties" ::: Ptr BaseOutStructure) -> IO Result) vkGetPipelinePropertiesEXT)
    (castFunPtr @_ @(Ptr Device_T -> ("pMetalObjectsInfo" ::: Ptr (SomeStruct ExportMetalObjectsInfoEXT)) -> IO ()) vkExportMetalObjectsEXT)
    (castFunPtr @_ @(Ptr Device_T -> Framebuffer -> ("pPropertiesCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr TilePropertiesQCOM) -> IO Result) vkGetFramebufferTilePropertiesQCOM)
    (castFunPtr @_ @(Ptr Device_T -> ("pRenderingInfo" ::: Ptr (SomeStruct RenderingInfo)) -> ("pProperties" ::: Ptr TilePropertiesQCOM) -> IO Result) vkGetDynamicRenderingTilePropertiesQCOM)
    (castFunPtr @_ @(Ptr Device_T -> ("pCreateInfo" ::: Ptr (SomeStruct OpticalFlowSessionCreateInfoNV)) -> ("pAllocator" ::: Ptr AllocationCallbacks) -> ("pSession" ::: Ptr OpticalFlowSessionNV) -> IO Result) vkCreateOpticalFlowSessionNV)
    (castFunPtr @_ @(Ptr Device_T -> OpticalFlowSessionNV -> ("pAllocator" ::: Ptr AllocationCallbacks) -> IO ()) vkDestroyOpticalFlowSessionNV)
    (castFunPtr @_ @(Ptr Device_T -> OpticalFlowSessionNV -> OpticalFlowSessionBindingPointNV -> ImageView -> ImageLayout -> IO Result) vkBindOpticalFlowSessionImageNV)
    (castFunPtr @_ @(Ptr CommandBuffer_T -> OpticalFlowSessionNV -> ("pExecuteInfo" ::: Ptr OpticalFlowExecuteInfoNV) -> IO ()) vkCmdOpticalFlowExecuteNV)
    (castFunPtr @_ @(Ptr Device_T -> ("pFaultCounts" ::: Ptr DeviceFaultCountsEXT) -> ("pFaultInfo" ::: Ptr DeviceFaultInfoEXT) -> IO Result) vkGetDeviceFaultInfoEXT)

